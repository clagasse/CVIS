# ==============================================================================
# CVIS Freshwater Utility Functions (2_fw_utils.R)
#
# Description:
#   Utility functions for freshwater indicators, spatial subsetting, FWA path
#   reconstructions, PCIC indicators netCDF loading, and plotting/mapping.
#
# Functions:
#   - calculate_subset_means_all: Calculates mean for column subsets.
#   - greater_zero: Returns TRUE if value is > 0.
#   - wmean, wsd, wqt: Weighted mean, standard deviation, and quantiles.
#   - standardize_long_stats, stack_long_stats: Data formatting helpers.
#   - subset_intersections: Spatial intersection helper.
#   - reconstruct_migrT_rcps: Reconstructs migrT_rcps from daily data.
#   - subset_fw_models: Subsets FW stream potential datasets.
#   - loadPCIC_ind: Loads PCIC indicator NetCDF variables.
#   - downstream_path, measure_downstream: FWA network path functions.
#   - choose_CU_stream: Selects representative streams.
#   - calculate_work: Computes vertical/horizontal fish work.
#   - rainfall_plot, map.stage: Visualizations.
#
# Dependencies:
#   - sf, dplyr, purrr, tidyr, Hmisc, stars/ncdf4 (via read_ncdf)
# ==============================================================================

# ==================== 1. Summary & Formatting Helpers ====================


## simple calculation functions
calculate_subset_means_all <- function(df, index_list) {
  # Select only numeric columns
  numeric_df <- df %>% select_if(is.numeric)
  
  # For each subset, calculate means of all numeric columns
  map_df(index_list, function(indices) {
    numeric_df[indices, ] %>%
      summarise(across(everything(), \(x) mean(x, na.rm = TRUE)))
  }, .id = "subset")
}

greater_zero <- function(x) {
  if_else(x > 0, T, F)
}



#weighted mean
wmean <- function(x, length_metre, na.rm = TRUE) {
  sum(x * length_metre, na.rm = TRUE) / sum(length_metre, na.rm = TRUE)
}
#weighted standard deviation
wsd <- function(x, length_metre, na.rm = TRUE) {
  sqrt(Hmisc::wtd.var(x, weights = length_metre, na.rm = TRUE))
}
#weighted quantiles
wqt <- function(x, length_metre, prob = 0.025, na.rm = TRUE) {
  if(sum(!is.na(x)) == 0) return(NA)
  y <- Hmisc::wtd.quantile(x, weights = length_metre, probs = prob, na.rm = TRUE)
  unname(y)
}



# helper functions for standardizing format  --------------------------

# Ensure every df has the same columns and types before binding
standardize_long_stats <- function(df) {
  # Required columns in the final stack
  cols <- c("FULL_CU_IN", "gcm", "gcm_name", "dsmodel", "rcp", "period_code",
            "indicator", "stat", "value", "category")
  
  # Add any missing columns as NA
  for (nm in cols) {
    if (!nm %in% names(df)) df[[nm]] <- NA
  }
  
  # Coerce to consistent types
  df %>%
    mutate(
      FULL_CU_IN  = as.character(FULL_CU_IN),
      gcm         = as.character(gcm),
      gcm_name    = as.character(gcm_name),
      dsmodel     = as.character(dsmodel),
      rcp         = as.integer(rcp),
      period_code = as.integer(period_code),
      indicator   = as.character(indicator),
      stat        = as.character(stat),
      value       = as.numeric(value),
      category    = as.character(category)
    ) %>%
    select(all_of(cols))
}

# Bind any number of long-stat data frames safely
stack_long_stats <- function(...) {
  dots <- list(...)
  map(dots, standardize_long_stats) %>%
    bind_rows() %>%
    arrange(FULL_CU_IN, category, dsmodel, indicator, rcp, period_code, stat, gcm)
}

# ==================== 2. Spatial Analysis & Habitat Subsetting ====================

# calculate number of decades from historical to projection period. used for calculating rates of T change
decade_calc <- function(historical_pick = "0", period_pick = "3") {
  year_hist <- case_when(
    historical_pick == "0" ~ 1990,
    historical_pick == "1" ~ 2010
  )
  
  year_proj <- case_when(
    period_pick == "0" ~ 1990,
    period_pick == "1" ~ 2010,
    period_pick == "2" ~ 2030,
    period_pick == "3" ~ 2050,
    period_pick == "4" ~ 2070,
    period_pick == "5" ~ 2090
  )
  
  decades <- (year_proj - year_hist) / 10
}


# for two spatial objects, subset the data in sp_x that intersects with sp_y
subset_intersections <- function(sp_x, sp_y) {
  intersect <- st_intersects(sp_x, sp_y, sparse = FALSE)
  sp_x_sub <- sp_x[apply(intersect, 1, sum) > 0,]
  return(sp_x_sub)
}

# Reconstruct migrT_rcps from migr_daily_all if it is missing (as in newer data files)
reconstruct_migrT_rcps <- function(migr_daily_all) {
  if (is.null(migr_daily_all) || nrow(migr_daily_all) == 0) {
    return(list())
  }
  
  # Unnest daily migration temperature data, dropping period from outer select to avoid duplicate names in unnest
  df_unnested <- migr_daily_all %>%
    dplyr::filter(attr == "migrT", gcm_name != "ensemble") %>%
    dplyr::select(FULL_CU_IN, rcp, time) %>%
    tidyr::unnest(time) %>%
    dplyr::mutate(doy = as.integer(time))
  
  if (nrow(df_unnested) == 0) {
    return(list())
  }
  
  # Calculate summary stats across GCM models
  df_summary <- df_unnested %>%
    dplyr::group_by(FULL_CU_IN, rcp, period, doy) %>%
    dplyr::summarise(
      mean_val = mean(migrT, na.rm = TRUE),
      q10_val = stats::quantile(migrT, probs = 0.1, na.rm = TRUE),
      q90_val = stats::quantile(migrT, probs = 0.9, na.rm = TRUE),
      .groups = "drop"
    )
  
  rcps <- unique(df_summary$rcp)
  migrT_rcps <- list()
  
  for (rcp_val in rcps) {
    df_rcp <- df_summary %>% dplyr::filter(rcp == rcp_val)
    cus <- unique(df_rcp$FULL_CU_IN)
    migrT_rcps[[rcp_val]] <- list()
    
    for (cu_val in cus) {
      df_cu <- df_rcp %>% dplyr::filter(FULL_CU_IN == cu_val)
      periods <- sort(unique(df_cu$period))
      doys <- sort(unique(df_cu$doy))
      
      mat_mean <- matrix(NA_real_, nrow = length(doys), ncol = length(periods),
                         dimnames = list(as.character(doys), periods))
      mat_10 <- mat_mean
      mat_90 <- mat_mean
      
      for (per_val in periods) {
        df_per <- df_cu %>% dplyr::filter(period == per_val)
        idx <- match(df_per$doy, doys)
        mat_mean[idx, per_val] <- df_per$mean_val
        mat_10[idx, per_val] <- df_per$q10_val
        mat_90[idx, per_val] <- df_per$q90_val
      }
      
      migrT_rcps[[rcp_val]][[cu_val]] <- list(
        doy = list(
          mean = mat_mean,
          `0.1` = mat_10,
          `0.9` = mat_90
        ),
        gcm = NULL
      )
    }
  }
  
  return(migrT_rcps)
}

# Subset fw_models by CU and get model_rs, model_spawning, and model_rearing for the CU species
subset_fw_models <- function(fw_models, 
                             cu_i, 
                             stream_cu_picks, 
                             cu_run, 
                             spp_lookup, 
                             to_factor = FALSE,
                             filter_rs = FALSE) {
  # Get species abbreviations
  sp_pick <- cu_run$SPECIES_NAME[cu_run$FULL_CU_IN == cu_i]
  if (length(sp_pick) == 0) {
    stop("CU ", cu_i, " not found in cu_run.")
  }
  sp_pick <- sp_pick[1] # Ensure single value
  
  sp_pick_bcfp <- spp_lookup$spp_abr_bcfp[spp_lookup$SPECIES_NAME == sp_pick]
  
  # Determine column names for habitat potential models
  model_h_pick <- paste0("model_habitat_", sp_pick_bcfp)
  model_r_pick <- paste0("model_rearing_", sp_pick_bcfp)
  model_s_pick <- paste0("model_spawning_", sp_pick_bcfp)
  
  # Harrison downstream (Weaver) sockeye exception
  if (cu_i == "SEL-03-04") {
    model_h_pick <- "model_habitat_salmon"
  }
  
  # Subset stream indices for the CU
  stream_cu_sub <- stream_cu_picks[, colnames(stream_cu_picks) == cu_i]
  fw_models_cu <- fw_models[stream_cu_sub, ]
  
  # Calculate rearing/spawning habitat (model_rs)
  avail_h_cols <- intersect(model_h_pick, names(fw_models_cu))
  if (length(avail_h_cols) > 0) {
    fw_models_cu$model_rs <- rowSums(st_drop_geometry(fw_models_cu)[, avail_h_cols, drop = FALSE], na.rm = TRUE) > 0
  } else {
    fw_models_cu$model_rs <- FALSE
  }
  
  # Convert model_rs to factor for plotting if requested
  if (isTRUE(to_factor)) {
    fw_models_cu$model_rs <- factor(fw_models_cu$model_rs, levels = c(TRUE, FALSE),
                                    labels = c("1-SPAWNING/REARING", "2-ACCESSIBLE"))
  }
  
  # Calculate spawning habitat only (model_spawning)
  avail_s_cols <- intersect(model_s_pick, names(fw_models_cu))
  if (length(avail_s_cols) > 0) {
    fw_models_cu$model_spawning <- rowSums(st_drop_geometry(fw_models_cu)[, avail_s_cols, drop = FALSE], na.rm = TRUE) > 0
  } else {
    fw_models_cu$model_spawning <- FALSE
  }
  
  # Calculate rearing habitat only (model_rearing)
  # Check if species is Chinook, Coho, or Sockeye (using both abbreviations and full names)
  if (sp_pick %in% c("ck", "co", "sk", "Chinook", "Coho", "Sockeye")) {
    avail_r_cols <- intersect(model_r_pick, names(fw_models_cu))
    if (length(avail_r_cols) > 0) {
      fw_models_cu$model_rearing <- rowSums(st_drop_geometry(fw_models_cu)[, avail_r_cols, drop = FALSE], na.rm = TRUE) > 0
    } else {
      fw_models_cu$model_rearing <- FALSE
    }
  } else {
    fw_models_cu$model_rearing <- FALSE
  }
  
  if(filter_rs == TRUE) {
    fw_models_cu <- fw_models_cu %>%
      filter(model_rs == "1-SPAWNING/REARING")
  }
  
  return(fw_models_cu)
}


# ==================== 3. PCIC NetCDF Data Loading ====================

loadPCIC_ind <- function(
    variable = "POT19freq_year_aClimMean", # Which variable to load?
    model = "ensMean", # Which GCM? ensMean = ensemble Mean
    rcp = 45, # Which emissions scenario? Options 45 or 85, 
    verbose = FALSE #provide details on data set?
){
  
  # Define location of data
  root_dat <- file.path(paths$climate, "PCIC_indicators")
  
    # Two variables in VICGL output
  varNames <- data.frame(
    fileName = c("peakFlowDay_aClimMean", "POT19freq_year_aClimMean"),
    Name = c("peakQday_year", "POT19freq_year")
  )
  
  timeRanges <- c("1971-2000", "2041-2070")
  
  v <- which(varNames$fileName == variable)
  
  # Open netcdf file from first time period
  #historic <- nc_open(paste0(root_dat,"/", varNames$fileName[v], "_", model, "_VICGL-dynWat_rcp", rcp, "_", timeRanges[1], "_bccoast+fraser.nc"))
  historic <- read_ncdf(paste0(root_dat,"/", varNames$fileName[v], "_", model, "_VICGL-dynWat_rcp", rcp, "_", timeRanges[1], "_bccoast+fraser.nc"))
  
  #open netcdf file from second time period
  proj <- read_ncdf(paste0(root_dat,"/", varNames$fileName[v], "_", model, "_VICGL-dynWat_rcp", rcp, "_", timeRanges[2], "_bccoast+fraser.nc"))
  #proj <- nc_open(paste0(root_dat,"/", varNames$fileName[v], "_", model, "_VICGL-dynWat_rcp", rcp, "_", timeRanges[2], "_bccoast+fraser.nc"))
  
  if(verbose == TRUE) {
    # # Explore dataset. What are the variables?
    print(paste("File",historic$filename,"contains",historic$nvars,"variables"))
    for( i in 1:historic$nvars ) {
      t <- historic$var[[i]]
      print(paste("Here is information on variable number",i))
      print(paste("   Name: ",t$name))
      print(paste("   Units:",t$units))
      print(paste("   Missing value:",t$missval))
      print(paste("   # dimensions :",t$ndims))
      print(paste("   Variable size:",t$varsize))
    }
  }
  
  z <- cbind(historic, proj)
  
  varExtracted0     <-  ncvar_get(historic, varNames$Name[v])
  varExtracted_proj <-  ncvar_get(proj, varNames$Name[v])
  
  # Spatial variables
  lon <- ncvar_get(historic, "lon")
  lat <- ncvar_get(historic, "lat")
  
  var_all <- array(c(varExtracted0, varExtracted_proj), dim = c(length(lon), length(lat), 2))
  
  # Reduce dimensionality so that grid point[i, ] corresponds to var[i, ]
  varExtracted1 <- var_all
  dim(varExtracted1) <- c(length(lon)*length(lat), 2)
  
  dimnames(varExtracted1) <- list(
    1:nrow(varExtracted1),
    c("hist", "proj")
  )
  
  # grid_points <- data.frame(
  #   id = c(1:(length(lon)*length(lat))),
  #   lon = rep(lon, length(lat)),
  #   lat = rep(lat, each = length(lon)),
  #   lon_id = rep(c(1:length(lon)), length(lat)),
  #   lat_id = rep(c(1:length(lat)), each = length(lon)))
  # write.csv(grid_points, file = "freshwater/output/PCIC-grid-points_bccoast.csv")
  
  return(varExtracted1)
  
}





# ==================== 4. Freshwater Atlas (FWA) Path Analysis ====================

downstream_path <- function(stream_pick, stream_network, code_type = "FWA") {
  
  FWA_code <- stream_pick$localcode
  st <- str_length(FWA_code)
  if(code_type == "FWA") {
    FWA_code <- stream_pick$FWA_WATERSHED_CODE
    st <- str_locate(FWA_code, "000000")[1] - 2   #get stream code last position
  }
  
  st_level <- (st + 4) / 7
  
  for (n in 1:st_level) {
    
    #get FWA code for current FWA level of iteration
    c_cut <- (n - 1) * 7
    s_pick <- stringr::str_sub(FWA_code, 1, st - c_cut)
    
    if (code_type == "FWA") s_pick <- stringr::str_c(s_pick, "-000000")
    
    # candidates at this level
    if (code_type == "FWA") {
      ind <- stringr::str_starts(stream_network$FWA_WATERSHED_CODE, s_pick)
    } else {
      ind <- (stream_network$localcode == s_pick)
    }
    
    candidates <- stream_network[ind, ] %>%
      dplyr::filter(
        stream_order >= max(stream_pick$stream_order, na.rm = TRUE),
        stream_magnitude >= max(stream_pick$stream_magnitude, na.rm = TRUE)
      )
    
    if (nrow(candidates) == 0) next
    
    #get streams with matching FWA code that intersect with migration reaches (confluences will include tributaries)
    int_mat <- sf::st_intersects(candidates, stream_pick, sparse = FALSE)
    FWA_int <- candidates[which(rowSums(int_mat) > 0), ]
    
    if (nrow(FWA_int) == 0) next
    
    FWA_int <- FWA_int %>% dplyr::filter(!is.na(downstream_route_measure))
    
    #  pick ONE "downstream continuation" route, then only take downstream on that route
    # Heuristic: prefer the largest river (order/magnitude), then the most-downstream DRM among those
    next_reach <- FWA_int %>%
      dplyr::arrange(dplyr::desc(stream_order),
                     dplyr::desc(stream_magnitude),
                     downstream_route_measure) %>%
      dplyr::slice(1)
    
    #get downstream_route_measure from lowest intersecting reach
    next_key <- next_reach$blue_line_key
    dd <- next_reach$downstream_route_measure
    
    low_stream <- candidates %>%
      dplyr::filter(
        blue_line_key == next_key,
        downstream_route_measure <= dd   # see note below on direction
      )
    
    #take all streams with downstream_route_measure distance below intersect
    stream_pick <- dplyr::bind_rows(stream_pick, low_stream) %>%
      dplyr::distinct()  # avoid duplicates
  }
  
  stream_pick
}


#measure distance to ocean entry for each stream segment in a migration paths object
measure_downstream <- function(stream_pick, stream_network, code_type = "FWA") {
  
  #if value already exists, return it and exit function
  if(!is.na(stream_pick$downstream_distance)) return(stream_pick$downstream_distance)

  FWA_code <- stream_pick$wscode
  st <- str_length(FWA_code)
  
  if(code_type == "FWA") {
    FWA_code <- stream_pick$FWA_WATERSHED_CODE
    st <- str_locate(FWA_code, "000000")[1] - 2   #get stream code last position
  }
  
  st_level <- (st+4)/7
  
  if(code_type == "FWA") s_pick <- str_sub(FWA_code,1,st)
  
  for(k in 1:st_level){

    c_cut <- (k-1) * 7
    code <- paste0(str_sub(FWA_code, 1, st - c_cut), "-000000")
    
    #select streams with same watershed code 
    candidates <- filter(stream_network, str_starts(FWA_WATERSHED_CODE, code)) 
    
    if(k == 1) {
      # filter streams with equal or lower distance to next downstream section
      candidates <- filter(candidates, downstream_route_measure <= stream_pick$downstream_route_measure)
      temp_path <- candidates  # add to path object
    } 
    if(k > 1) {
      #get streams with lower FWA code that intersect with migration reaches
      FWA_int <- st_intersects(candidates, temp_path, sparse = FALSE)
      if(length(FWA_int) == 0) next
      FWA_int <- candidates[which(apply(FWA_int, 1, sum) > 0),]

      #filter streams with lower FWA code to select those that are downstream of the intersection point
      candidates <- filter(candidates, downstream_route_measure <= FWA_int$downstream_route_measure)
      temp_path <- bind_rows(temp_path, candidates)  #combine into path object
    }
    
    # filter out any streams with higher order or magnitude as an extra check
    temp_path <- temp_path %>%
      filter(stream_order >= max(stream_pick$stream_order, na.rm = T), stream_magnitude >= max(stream_pick$magnitude, na.rm = T))
    
  }

  #get downstream_route_measure range from lowest intersecting reach
  if(code_type == "FWA") {
    dd <- sum(candidates$length_metre, na.rm = T) - (stream_pick$length_metre / 2)
  }
  
  return(dd)
}


#function to choose stream within a CU boundary for subsequent path analysis
choose_CU_stream <- function(FWA, cu_boundary, subset_order = FALSE, min_order = 5) {
  
  cu_FWA <- st_contains(cu_boundary, FWA)
  cu_FWA <- FWA[cu_FWA[[1]],]  #%>%
  
  if(subset_order == TRUE) cu_FWA <- filter(cu_FWA, stream_order >= min_order) 
  boundary_centre <- st_centroid(cu_boundary)
  
  cu_FWA_pick <- st_nearest_feature(boundary_centre, cu_FWA)
  cu_FWA_pick <- cu_FWA[cu_FWA_pick,]
  
  return(cu_FWA_pick)
}

#mat <- st_coordinates(cu_FWA)
#mat_loc <- median(mat[,3])
#mid


#simple calculation for work to reach migration segment

calculate_work <- function(elev, dist) {
  work <- 0.0001 * elev * dist
}

# ==================== 5. Diagnostic Plotting & Mapping ====================

rainfall_plot <- function(data) {
    ggplot(data, aes(x = factor(STREAM_ORDER), y = CT_anad, fill = factor(STREAM_ORDER))) +
  # Add half-violin from {ggdist} package
  stat_halfeye(
    adjust = 0.5,
    justification = -0.2,
    .width = 0,
    point_colour = NA
  ) +
  geom_boxplot(
    width = 0.12,
    outlier.color = NA,
    alpha = 0.5
  ) +
  stat_dots(
    side = "left",
    justification = 0.1,
    binwidth = 0.25,
    size = 0.1
  ) +
  scale_fill_viridis(discrete = TRUE) +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 11)
  ) +
  ggtitle("Stream Orders within CU boundary") +
  xlab("")
}



#------------------------------------------------------------------------------
# Plot map
#------------------------------------------------------------------------------

map.stage <- function(
    cu_boundary.i,
    zoi.i,
    mig_paths.i,
    grid_polys = NA
){
  
  if(!is.na(zoi.i)){
    bounds <- cbind(st_bbox(zoi.i), st_bbox(mig_paths.i))
  bounds <- c(
    xmin = min(bounds[1, ]), 
    xmax = max(bounds[3,]), 
    ymin = min(bounds[2, ]), 
    ymax = max(bounds[4, ]))
  } else {
    bounds <- st_bbox(mig_paths.i)[c(1,3,2,4)]
  }
  
  par(bg = NA)
  
  plot(st_geometry(BC), border = NA, col = NA, axes = FALSE, las = 1, ylim = bounds[3:4], xlim = bounds[1:2], bty = "o")
  # plot(st_geometry(mig_paths.i), lwd = 3, col = cols[3], axes=TRUE, las = 1, ylim = bounds[3:4], xlim = bounds[1:2])
  # mtext(side = 3, line = 1, cus_to_keep$culabel[i])
  
  plot(grid_polys, add = TRUE, border = grey(0.8), col = NA)
  plot(BC, add = TRUE, col = NA, border = 1)
  
  
  if(!is.na(zoi.i)){
    plot(st_geometry(zoi.i), border = cols[1], col = paste0(cols[1], 50), lwd = 1.5, add = TRUE)
  }
  
  if(!is.na(cu_boundary.i)){
  plot(st_geometry(cu_boundary.i), border = 1, col = NA, lwd = 1, add = TRUE)
  }
  
  # coarser lakes and rivers
  plot(st_geometry(lakes0), border = 1, col = NA, add = TRUE)
  plot(st_geometry(rivers0), col = 1, add = TRUE)
  
  # legend("topleft", fill = c(paste0(cols[1], 50), cols[3], "white"), border = c(cols[1], cols[3], "#000000"), legend = c("Spawn ZOI", "Mig.", "CU boundary"), bg = "white")
  
}