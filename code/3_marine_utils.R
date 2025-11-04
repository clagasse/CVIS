###############################################################################
# 3_marine_utils.R

# Functions to load and summarize marine data relevant

###############################################################################


# get datatable from CMIP6 NCDF file

ncdf_to_dt <- function(file_name = "ensemble-percentiles_mon_QDM+OSTIA_historical+ssp370_1950-2100_BC.nc",
                       file_path = file.path(paths$climate, "Marine_CMIP6"),
                       MAZ_obj = MAZ,
                       start_year = 1980,
                       end_year = 2020,
                       keep_all = FALSE  # clip to MAZ or keep all spatial points
) {

  nc <- nc_open(file.path(file_path, file_name))

  lon <- ncvar_get(nc, "lon")
  lat <- ncvar_get(nc, "lat")
  time <- ncvar_get(nc, "time")
  sst <- ncvar_get(nc, "sst_p50")  # Use sst_p50 for median SST
  sstp10 <- ncvar_get(nc, "sst_p10")
  sstp90 <- ncvar_get(nc, "sst_p90")

  dates <- as.Date(time, origin = "1850-01-01")

  time_index <- which(dates >= as.Date(paste0(start_year, "-01-01"))                        &
    dates <= as.Date(paste0(end_year, "-12-31")))

  # get historic time series of monthly temps
  dates_subset <- dates[time_index]
  sst <- sst[, , time_index]
  sstp10 <- sstp10[, , time_index]
  sstp90 <- sstp90[, , time_index]

  sst <- sst - 273.15 # convert to celsius
  sstp10 <- sstp10 - 273.15 # convert to celsius
  sstp90 <- sstp90 - 273.15 # convert to celsius

  # make into data.table
  grid <- expand.grid(lon = lon, lat = lat)
  n_cells <- nrow(grid)

  # Create a data.table with all time slices
  dt_list <- lapply(1:length(dates_subset), function(i) {
    data.table(
      lon = grid$lon,
      lat = grid$lat,
      year = format(dates_subset[i], "%Y"),
      month = format(dates_subset[i], "%m"),
      SST = as.vector(sst[, , i]),
      SST_p10 = as.vector(sstp10[, , i]),
      SST_p90 = as.vector(sstp90[, , i])
    )
  })

  # Combine all into one long data.table
  dt_long <- rbindlist(dt_list)

  # make one column for each month
  dt_wide <- dcast(
    dt_long,
    lon + lat + year ~ month,
    value.var = c("SST", "SST_p10", "SST_p90")
  )

  # convert to sf
  CMIP_sf <- st_as_sf(dt_wide, coords = c("lon", "lat"), crs = 4269) %>%
    st_transform(crs = "EPSG:3005")
  # join with MAZ to get MAZ assignments
  CMIP_sf   <- st_join(CMIP_sf, MAZ_obj["MAZ_Acrony"], left = keep_all)

}


# ---------Installing function point2rast from PACEA ------------------
#- code copied from https://github.com/pbs-assess/pacea/blob/main/R/interpolate.R
point2rast <- function(data, spatobj, loc = c("x", "y"), cellsize, nnmax = 4,
                       as = c("SpatRast", "SpatVect")) {

  requireNamespace("methods", quietly = TRUE)
  requireNamespace("terra", quietly = TRUE)
  requireNamespace("gstat", quietly = TRUE)
  requireNamespace("sf", quietly = TRUE)
  requireNamespace("stats", quietly = TRUE)

  stopifnot("must provide cellsize value" = exists("cellsize"))
  stopifnot("must specify valid value for 'as'" = as %in% c("SpatRast", "SpatVect"))

  if (!any(sapply(c("sf", "Spatial"), function(cl) methods::is(data, cl)))) {

    data <- as.data.frame(data)

    if (!length(dim(loc))) {

      stopifnot("loc vector must of be of length==2 " = length(loc) == 2)
      stopifnot("loc names not found in data" = loc %in% names(data))

      coords <- setNames(as.data.frame(data[, loc]), c("x", "y"))

      tdat <- as.data.frame(data[, -which(colnames(data) %in% loc), drop = FALSE])

    } else {

      stopifnot("loc data must be a matrix or dataframe of two columns" =
        length(dim(loc)) == 2 & dim(loc)[2] == 2)
      stopifnot("loc data is not of equal length to data" = nrow(data) == nrow(loc))

      coords <- setNames(as.data.frame(loc), c("x", "y"))

      tdat <- as.data.frame(data[, !colnames(data) %in% colnames(loc), drop = FALSE])

    }
  }

  if (methods::is(data, "Spatial")) {
    coords <- setNames(as.data.frame(data)[, c("coords.x1", "coords.x2")], c("x", "y"))
    tdat <- as.data.frame(data)[, names(data), drop = FALSE]
  }

  if (methods::is(data, "sf")) {
    coords <- setNames(as.data.frame(matrix(unlist(data$geometry), ncol = 2, byrow = TRUE)), c("x", "y"))
    tdat <- as.data.frame(data)[, -which(names(data) == "geometry"), drop = FALSE]
  }

  tbb <- terra::ext(spatobj)
  if (!any(coords$x >= tbb$xmin & coords$x <= tbb$xmax &
    coords$y >= tbb$ymin & coords$y <= tbb$ymax)) {
    warning("'loc' coordinates within spatobj extent = 0; check crs or extent of spatobj")
  }

  terror <- try(terra::crs(spatobj), silent = TRUE)
  if ("try-error" %in% class(terror)) {
    r <- terra::rast(terra::ext(spatobj), res = c(cellsize))
  } else {
    r <- terra::rast(terra::ext(spatobj), res = c(cellsize), crs = terra::crs(spatobj))
  }

  nn.pred <- apply(tdat, 2, FUN = nnfit, r = r, loc = loc, coords = coords, nnmax = nnmax)
  xyz <- cbind(as.data.frame(suppressWarnings(terra::crds(r))), nn.pred)

  if (as[1] == "SpatRast") {
    spat <- terra::rast(xyz, type = "xyz", crs = terra::crs(r))
  }
  if (as[1] == "SpatVect") {
    spat <- terra::vect(xyz, geom = c("x", "y"), crs = terra::crs(r))
  }

  return(spat)
}

#' nearest neighbour fit function
#' @noRd
nnfit <- function(x, r, loc, coords, nnmax) {

  requireNamespace("stats", quietly = TRUE)

  xdat <- stats::na.omit(data.frame(xvar = as.vector(x), coords))

  f <- paste0("xvar", " ~ 1")
  lf <- paste0("~", paste(loc, collapse = "+"))

  gs <- gstat::gstat(formula = xvar ~ 1, locations = ~ x + y, data = xdat, nmax = nnmax, set = list(idp = 0))
  nn <- terra::interpolate(r, gs, debug.level = 0)
  return(as.vector(nn$var1.pred))
}


## function to assign points or polygons based on intersection with MAZ
assign_points <- function(x, y, var = "MAZ_Acrony") {
  t_int <- st_intersects(x, y)

  for (i in 1:nrow(x)) {
    t_vec <- y[[var]][t_int[[i]]]
    t_vec <- str_sort(t_vec)

    x[[var]][i] <- str_flatten(t_vec, collapse = ",")
  }
  return(x)

}



# Data reshaping ----------------------------------------------------------

subset_and_mean_var <- function(data,
                                months = c(3, 4, 5),
                                period_code_0_year = 1995,
                                period_code_3_year = 2050,
                                period_code_5_year = 2090,
                                var_name = "SST",
                                MAZ_pick = "GStr",
                                include_quantiles = TRUE) {

  month_chars <- sprintf("%02d", months)
  month_cols <- paste0(var_name, "_", month_chars)

  # get number of decades for rate of T change
  period_3_decades <- (period_code_3_year - period_code_0_year) / 10
  period_5_decades <- (period_code_5_year - period_code_0_year) / 10

  if (include_quantiles == TRUE) {
    qlow_cols  <- paste0(var_name, "_", "p10", "_", month_chars)
    qhigh_cols <- paste0(var_name, "_", "p90", "_", month_chars)
  }

  missing_cols <- setdiff(month_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing columns in sf_data: ", paste(missing_cols, collapse = ", "))
  }

  summary_data <- data %>%
    filter(MAZ_Acrony == MAZ_pick) %>%
    mutate(
      monthly_mean = rowMeans(select(., any_of(month_cols)), na.rm = TRUE),
      monthly_p10  = if (include_quantiles && all(qlow_cols %in% names(.))) rowMeans(select(., all_of(qlow_cols)), na.rm = TRUE) else NA,
      monthly_p90  = if (include_quantiles && all(qhigh_cols %in% names(.))) rowMeans(select(., all_of(qhigh_cols)), na.rm = TRUE) else NA
    ) %>%
    group_by(rcp, period_code) %>%
    summarize(
      !!paste0(var_name, "proj_", "mean")     := mean(monthly_mean, na.rm = TRUE),
      !!paste0(var_name, "proj_", "qlowsp")   := quantile(monthly_mean, qlowsp, na.rm = TRUE),
      !!paste0(var_name, "proj_", "qhighsp")  := quantile(monthly_mean, qhighsp, na.rm = TRUE),
      !!paste0(var_name, "proj_", "qlowgcm")  := mean(monthly_p10, na.rm = TRUE),
      !!paste0(var_name, "proj_", "qhighgcm") := mean(monthly_p90, na.rm = TRUE),
      .groups = "drop"
    )

  # Separate baseline
  baseline <- summary_data %>%
    filter(period_code == 0) %>%
    rename_with(~ paste0(.x, "_hist"), -c(rcp, period_code))

  # Join and calculate rate of change using number of decades for period
  rate_data <- summary_data %>%
    filter(period_code != 0) %>%
    bind_cols(select(baseline, -c(period_code, rcp))) %>%
    mutate(
      decades = case_when(
        period_code == 3 ~ period_3_decades,
        period_code == 5 ~ period_5_decades,
        TRUE ~ NA_real_
      ),

      !!paste0(var_name, "rate_", "mean")     := (!!sym(paste0(var_name, "proj_", "mean")) - !!sym(paste0(var_name, "proj_", "mean_hist"))) / decades,
      !!paste0(var_name, "rate_", "qlowsp")   := (!!sym(paste0(var_name, "proj_", "qlowsp")) - !!sym(paste0(var_name, "proj_", "qlowsp_hist"))) / decades,
      !!paste0(var_name, "rate_", "qhighsp")  := (!!sym(paste0(var_name, "proj_", "qhighsp")) - !!sym(paste0(var_name, "proj_", "qhighsp_hist"))) / decades,
      !!paste0(var_name, "rate_", "qlowgcm")  := (!!sym(paste0(var_name, "proj_", "qlowgcm")) - !!sym(paste0(var_name, "proj_", "qlowgcm_hist"))) / decades,
      !!paste0(var_name, "rate_", "qhighgcm") := (!!sym(paste0(var_name, "proj_", "qhighgcm")) - !!sym(paste0(var_name, "proj_", "qhighgcm_hist"))) / decades
    ) %>%
    select(rcp, period_code, contains("rate"))

  summary_data <- summary_data %>%
    left_join(rate_data, by = c("rcp", "period_code"))

  return(summary_data)
}



# Marine nearshore timing -------------------------------------------------

# simple functions to determine start and end dates for the nearshore period when summarizing indicators

ns_timing_start <- function(oe_peak_month,
                            ns_start_offset,
                            ns_time_method) {

  if (ns_time_method == "peak_offset") ns_timing_start <- oe_peak_month - ns_start_offset
  if (ns_time_method == "static") ns_timing_start <- ns_start_static
}

ns_timing_end <- function(oe_peak_month,
                          ns_end_offset,
                          ns_time_method) {

  if (ns_time_method == "peak_offset") ns_timing_end <- oe_peak_month + ns_end_offset
  if (ns_time_method == "static") ns_timing_end <- ns_end_static
}
