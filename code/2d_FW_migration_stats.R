################################################################################
#
# 2e_FW_migration_stats.R
#
#  Summarize upstream migration indicators from PCIC model outputs
#
#  1) import PCIC outputs of ensemble and model averages, averaged over each time period
#  2) import CU paths with downstream distances
#  3) for each CU, use a moving migration window to subset the migration path:
#         a. Day 1 = run timing start, begin within 100km of the river mouth
#         b. spawn timing start = the date when the migration window reaches spawning sites
#         c. front migration movement rate = distance to spawning sites/
#                                         (# days between spawn timing start and run timing start)
#         d. tail migration movement rate = distance to spawning sites/
#                                    (# days between spawn timing peak and run timing end)
#         e. Final day = spawn timing peak date
#  4) Determine PCIC grid cells overlapping with migration window streams for each day of run
#  5) Calculate weighting for each path segment using proportion of spawning sites it leads to
# i.e. stream segments shared by all sites have weighting of 1,
# terminal stream segments leading to one of many spawning sites have lower weighting
#  6) take mean and quantiles of temperature and discharge for PCIC grid cells across spatial dimensions
# subsetting only segments within the moving migration timing window
#  7) For each CU, summarize temperature and discharge indicators across time, GCMs and RCPs
#       a. Time series of daily mean, q10 and q90 across GCMs for each period and RCP
#       b. Average, q10 and q90 temp or discharge across time series by period and RCP
#       c. Mean, q10 and q90 proportion of spatial extent above 19 degree or 21 degree threshold temp for each period, RCP

#################################################################################

library(here)
setwd(here())
source(file.path(here(), "code", "0_setup.R"))

historical <- "0"   # historical climatology period for temperature models
# 0 = 1981-2000,  1 = 2001-2020
# for flow models, 0 = 1981-2010

qlgcm <- 0.1    # lower quantile for statistics on GCM variation
qhgcm <- 0.9   # upper quantile for statistics
qld  <- 0.1    # lower quantile for temporal variation within migration window
qhd <- 0.9    # upper quantile for temporal variation

#--------- 1. import spatial objects ---------------------
# load CU paths
load(file.path(paths$fw, "2025-07-25_fw_upstream_paths.Rdata"))

# load PCIC daily outputs
PCIC_file_loc <- file.path(paths$climate, "PCIC_averaged", "combined")

# read PCIC outputs, with dimensions for each model, time period, and day of year
PCIC_daily <- read_mdim(file.path(PCIC_file_loc, "daily_rcp45.nc"))
# PCIC_daily_85 <- read_mdim(file.path(PCIC_file_loc, "daily_rcp85.nc"))

# convert Temperature from Kelvin to Celsius
PCIC_daily <- mutate(PCIC_daily, waterTemperature = waterTemperature - 273.15)

# get base PCIC grid - smaller object quicker for operations
PCIC_base <- PCIC_daily[, , , 1, 1, 1]

periods    <- st_get_dimension_values(PCIC_daily, "period")
gcm_models <- st_get_dimension_values(PCIC_daily, "model")

#--------------------- 2. Functions for indicators -----------------------------------

# function to produce summary statistics across rcps, gcms, periods, and days of year for a stars attribute
summarize_attribute <- function(data,
                                attr_name = "discharge",
                                type = "mean",  # mean or sum
                                qlowday = qld,   # lower and upper quantiles to extract
                                qhighday = qhd,
                                qlowgcm = qlgcm,
                                qhighgcm = qhgcm) {
  ## a. SPATIAL DIMENSION -----
  # Get array with average over x and y — keeping period, day of year, and model
  if (type == "mean") {
    spatial_avg <- st_apply(
      data[attr_name],  # variable to calc
      MARGIN = c("time", "period", "model"),  # dimensions to keep
      FUN = mean, na.rm = T
    )[[1]]
  }

  # if(type == "sum") {
  #   spatial_avg <- st_apply(
  #     data[attr_name],  # variable to calc
  #     MARGIN = c("time", "period", "model"),  # dimensions to keep
  #     FUN = sum, na.rm = T
  #   )[[1]]
  # }
  #
  dimnames(spatial_avg) <- list(st_get_dimension_values(data, "time"),
    st_get_dimension_values(data, "period"),
    st_get_dimension_values(data, "model"))

  ## b. GCM dimension ------
  ## Get average and quantiles for each day across GCM outputs and attributes
  doy_avg <- apply(spatial_avg,
    MARGIN = c(1, 2), # dimensions to keep
    FUN = mean,
    na.rm = T)

  doy_qlow <- apply(spatial_avg,
    MARGIN = c(1, 2),
    FUN = quantile,
    probs = qlowday,
    na.rm = T)

  doy_qhigh <- apply(spatial_avg,
    MARGIN = c(1, 2),
    FUN = quantile,
    probs = qhighday,
    na.rm = T)

  doy_stats <- list(doy_avg, doy_qlow, doy_qhigh)
  names(doy_stats) <- c("mean", qlowday, qhighday)


  ## c. TIME DIMENSION ------
  ## Aggregate over time - get average and quantiles across days within migration window

  # get average across days of year for time periods, models, and rcps
  gcm_doy_avg <- apply(spatial_avg,
    MARGIN = c(2, 3),
    FUN = mean,
    na.rm = T)

  # get average across gcm models
  gcm_stats <- tibble(period = st_get_dimension_values(data, "period"),
    mean    = NA,
    qlowgcm    = NA,
    qhighgcm   = NA,
    qmingcm     = NA,
    qmaxgcm     = NA)

  # take average, qlow and qhigh across gcms for each time period and rcp
  gcm_stats$mean <- apply(gcm_doy_avg, 1, mean)
  gcm_stats$qlowgcm  <- apply(gcm_doy_avg, 1, quantile, probs = qlowgcm)
  gcm_stats$qhighgcm  <- apply(gcm_doy_avg, 1, quantile, probs = qhighgcm)
  gcm_stats$qmingcm  <- apply(gcm_doy_avg, 1, min)
  gcm_stats$qmaxgcm  <- apply(gcm_doy_avg, 1, max)

  output <- list(doy = doy_stats,
    gcm = gcm_stats)

  return(output)

}

cu_migr_stats <- list() # list to store cu migration characteristics
migrT_rcps <- list()  # list to store temperature results
migrQ_rcps <- list()  # list to store discharge results
migrA19_rcps <- list()
migrA21_rcps <- list()

for (j in 1:2) {

  migrT_all <- list()  # list to store temperature results
  migrQ_all <- list()  # list to store discharge results
  migrA19_all <- list()
  migrA21_all <- list()

  if (j == 1) print("Starting migration stats for RCP 45")

  if (j == 2) {
    rm(PCIC_daily)
    gc()
    PCIC_daily <- read_mdim(file.path(PCIC_file_loc, "daily_rcp85.nc"))
    # convert Temperature from Kelvin to Celsius
    PCIC_daily <- mutate(PCIC_daily, waterTemperature = waterTemperature - 273.15)

    print("Starting migration stats for RCP 85")
  }

  for (i in 1:n.CUs) {
    cu_i <- cu_run$FULL_CU_IN[i]
    migr_cu <- migr_list[[cu_i]] %>%
      st_transform(4269)
    migr_cu_main <- filter(migr_cu, prop_paths == 1)   # select only paths leading to all NuSEDS sites

    # get cu timing and calculate movement rates for migration window
    cu_timing_i <- cu_timing_Fr[cu_timing_Fr$FULL_CU_IN == cu_i, ] %>%
      select(rt_start, rt_end, sp_start, sp_peak, sp_end) %>%
      mutate(rt_to_sp_s = sp_start - rt_start,  # days between spawn timing start and run timing start, used to calculate movement rate of front
        rt_to_sp_p = sp_peak - rt_end,     # days from spawn timing peak to run timing end, used to calculate movement of tail
        migr_s     = round(rt_start),           # start date for migration window used to calculate exposure - set to run timing start
        migr_e     = round(sp_peak),            # end date for migration window - set to peak spawn timing
        migr_dur   = round(migr_e - migr_s),
        max_dist   = max(migr_cu$downstream_distance, na.rm = T),
        main_dist  = sum(migr_cu_main$length_metre), # distance in common for all sites
        migr_wdist   = sum(migr_cu$length_metre * migr_cu$prop_paths),  # weighted average migration distance
        main_elev  = max(st_coordinates(migr_cu_main)[, 3]),  # highest point in common for all sites
        max_elev   = max(st_coordinates(migr_cu)[, 3]),      # highest point along all of migration path (ie. highest NUSEDS site)
        s_rate     = max_dist / (rt_to_sp_s),  # movement rate of front of migration window
        e_rate     = max_dist / (rt_to_sp_p))  # movement rate of tail of migration window

    if (sum(!is.na(migr_cu)) == 0 |   # skip if path is empty
      is.na(cu_timing_i$migr_s) | is.na(cu_timing_i$migr_e)) { # skip if timing info missing

      cu_migr_stats[[i]]   <- NA
      migrT_all[[i]]       <- NA
      migrQ_all[[i]]       <- NA
      migrA19_all[[i]]  <- NA
      migrA21_all[[i]]  <- NA

      next
    }

    ### GET MIGRATION WINDOW
    # Create a moving migration window over the cu path and input into stars array
    # get index of grid cells that intersect with each stream segment
    intersections <- st_intersects(PCIC_base, migr_cu)

    # create a mapping table between grid cell index and stream index that intersect, with prop_path
    intersection_df <- data.frame(
      grid_id = rep(seq_along(intersections), lengths(intersections)),
      line_id = unlist(intersections),
      prop_path = migr_cu$prop_paths[unlist(intersections)]
    )

    # create a matrix of streams to include for each day of the migration
    migr_win <- matrix(NA, nrow = nrow(migr_cu), ncol = 365)
    PCIC_ind <- matrix(NA, nrow = dim(PCIC_base)[["x"]] * dim(PCIC_base)[["y"]], ncol = 365)
    PCIC_pp <- matrix(NA, nrow = dim(PCIC_base)[["x"]] * dim(PCIC_base)[["y"]], ncol = 365)
    up_dist <- vector("numeric", 365)
    dn_dist <- vector("numeric", 365)

    # for each day of year, get index of stream segments that fall within migration window using downstream distance
    # then translate index of stream segments into index of PCIC grid cells
    for (day in 1:ncol(migr_win)) {
      # upper range of migration window = #days since rt_start x front movement rate
      up_dist[day] <- max(min((day - cu_timing_i$migr_s) * cu_timing_i$s_rate, cu_timing_i$max_dist), 0)
      dn_dist[day] <- max(min((day - cu_timing_i$migr_e) * cu_timing_i$e_rate, cu_timing_i$max_dist), 0)

      # stream segments within migration window distance each day
      migr_win[, day] <- migr_cu$downstream_distance < up_dist[day] & migr_cu$downstream_distance > dn_dist[day]
      # turn T/F into index of streams
      migr_win_index <- which(migr_win[, day] == TRUE)
      # if day is outside the migration timing window (rt_start and sp_peak), make NA
      if (day > cu_timing_i$migr_e | day < cu_timing_i$migr_s) migr_win_index <- NULL
      # use mapping table to convert stream index into PCIC grid cell index
      int_day <- intersection_df$grid_id[intersection_df$line_id %in% migr_win_index]

      # join with prop_path value
      prop_day <- bind_cols(grid_id = int_day,
        prop_path = intersection_df$prop_path[intersection_df$line_id %in% migr_win_index])

      int_day <- unique(int_day)

      # get T/F index for each PCIC grid cell each day
      PCIC_ind[, day] <- seq(1, dim(PCIC_base)[["x"]] * dim(PCIC_base)[["y"]]) %in% int_day

      ## Remainder of this for loop is for implementing prop_path weighting, not currently working
      # get average prop_path for each grid cell
      # prop_avg <- prop_day %>%
      #   group_by(grid_id) %>%
      #   summarize(prop_avg = mean(prop_path, na.rm = T)) %>%
      #   ungroup()
      #
      # #create a vector corresponding to total number of grid cells,
      # # fill it with prop_path value using index of intersecting cells
      # vec <- rep(NA, dim(PCIC_base)[["x"]] * dim(PCIC_base)[["y"]])
      # vec[prop_avg[[1]]] <- prop_avg[[2]]
      #
      # #get prop_paths value for each PCIC grid cell each day
      # PCIC_pp[,day] <- vec
    }

    daily_cells <- apply(PCIC_ind, 2, sum, na.rm = T)  # vector of number of grid cells included each day
    # reshape flattened PCIC index by grid cells into array with same dimensions as stars object
    doy_int <- array(PCIC_ind, dim = dim(PCIC_daily))

    # put the array into the stars object as a new attribute
    PCIC_daily$migration_window <- doy_int

    ### SUBSET grid to migration path and timing window -----
    PCIC_cu <- PCIC_daily[migr_cu] %>%   # subset cells that intersect with migr_cu
      filter(time >= cu_timing_i$migr_s, # filter out values outside migration window
        time <= cu_timing_i$migr_e)

    # new T or Q attribute with days outside of migration window turned to NA
    PCIC_cu$win_T <- PCIC_cu$waterTemperature
    PCIC_cu$win_T[!PCIC_cu$migration_window] <- NA

    PCIC_cu$win_Q <- PCIC_cu$discharge
    PCIC_cu$win_Q[!PCIC_cu$migration_window] <- NA

    ## Logical for whether cell temperature is above threshold on each day
    PCIC_cu$win_Tthr19 <- PCIC_cu$win_T > 19
    PCIC_cu$win_Tthr21 <- PCIC_cu$win_T > 21

    ### NOT WORKING - prop_path weighting
    # weight the grid cells based on the proportion of NUSEDS spawning sites the stream segments leads to
    # i.e. terminal segments that only lead to one site out of many for a CU have lower weighting
    # daily_prop <- apply(PCIC_pp, 2, sum, na.rm = T)  #vector of summed weight of grid cells included each day
    # doy_prop <- array(PCIC_pp, dim = dim(PCIC_cu))
    # PCIC_cu$weighted_window  <- doy_prop
    # #calculate weighted temperature and discharge, only including days within migration window
    # PCIC_cu$weighted_T <- PCIC_cu$waterTemperature * PCIC_cu$weighted_window
    #
    # daily_prop <- daily_prop[time_vals]
    #
    # ### problem step - use st_apply to take sum of weighted_T across spatial dimensions then divide by summed weighting
    # # Get weighted average over x and y — keeping period, day of year, and model
    # PCIC_avg_pp <- st_apply(
    #   PCIC_cu[c("weighted_T")],  # variable to calc
    #   MARGIN = c("period", "model", "time"),  # dimensions to keep
    #   FUN = function(x) {
    #     sum(x, na.rm = T)
    #   }
    # )
    # PCIC_avg_pp <- st_apply(
    #   PCIC_avg_pp,  # variable to calc
    #   MARGIN ="time",  # dimensions to keep
    #   FUN = function(x, time_index) {
    #     for(i in seq_along(time_index)) x / daily_prop[i]
    #   },
    #   time_index = st_get_dimension_values(PCIC_avg_pp, "time")
    # )


    ### AGGREGATE OVER DIMENSIONS to get summary statistics------

    migrT_stats <- summarize_attribute(PCIC_cu,
      attr_name = "win_T",
      type = "mean")

    migrQ_stats <- summarize_attribute(PCIC_cu,
      attr_name = "win_Q",
      type = "mean")

    migrA19_stats <- summarize_attribute(PCIC_cu,
      attr_name = "win_Tthr19",
      type = "mean")

    migrA21_stats <- summarize_attribute(PCIC_cu,
      attr_name = "win_Tthr21",
      type = "mean")

    cu_migr_stats[[i]]   <- cu_timing_i
    migrT_all[[i]]       <- migrT_stats
    migrQ_all[[i]]       <- migrQ_stats
    migrA19_all[[i]]  <- migrA19_stats
    migrA21_all[[i]]  <- migrA21_stats

    print(paste("Migration stats complete for:", cu_i))

  }

  names(cu_migr_stats)  <- cu_run$FULL_CU_IN
  names(migrT_all)      <- cu_run$FULL_CU_IN
  names(migrQ_all)      <- cu_run$FULL_CU_IN
  names(migrA19_all) <- cu_run$FULL_CU_IN
  names(migrA21_all) <- cu_run$FULL_CU_IN

  migrT_rcps[[j]]      <- migrT_all
  migrQ_rcps[[j]]      <- migrQ_all
  migrA19_rcps[[j]] <- migrA19_all
  migrA21_rcps[[j]] <- migrA21_all

}

names(migrT_rcps) <- c("45", "85")
names(migrQ_rcps) <- c("45", "85")
names(migrA19_rcps) <- c("45", "85")
names(migrA21_rcps) <- c("45", "85")


#------------- Flatten results into dataframe

# Helper function to extract gcm tibbles from a nested list
extract_gcm <- function(data_list, attr_label) {
  imap_dfr(data_list, function(rcp_list, rcp_name) {
    imap_dfr(rcp_list, function(cu_list, cu_name) {
      if (is.null(cu_list) || !is.list(cu_list)) return(NULL)
      if (!"gcm" %in% names(cu_list)) return(NULL)
      gcm_tbl <- cu_list[["gcm"]]
      if (!is.data.frame(gcm_tbl)) return(NULL)
      gcm_tbl %>%
        mutate(RCP = rcp_name,
          FULL_CU_IN = cu_name,
          attr = attr_label) %>%
        relocate(attr, RCP, FULL_CU_IN)
    })
  })
}

# Apply to each object
df_migrT     <- extract_gcm(migrT_rcps, "migrT")
df_migrQ     <- extract_gcm(migrQ_rcps, "migrQ")
df_migrA19 <- extract_gcm(migrA19_rcps, "migrA19")
df_migrA21 <- extract_gcm(migrA21_rcps, "migrA21")

# Combine all into one data frame
df_migr_combined <- bind_rows(
  df_migrT,
  df_migrQ,
  df_migrA19,
  df_migrA21
)


# Filter to only data frames or NULLs
cleaned_list <- cu_migr_stats %>%
  keep(~ is.data.frame(.x) || is.null(.x))
df_migr_cu  <- list_rbind(cleaned_list, names_to = "FULL_CU_IN")


migr_all_flat <- df_migr_combined %>%
  pivot_wider(
    id_cols = c(RCP, FULL_CU_IN, period),
    names_from = attr,
    values_from = c(mean, qlowgcm, qhighgcm, qmingcm, qmaxgcm),
    names_glue = "{attr}_{.value}"
  ) %>%
  left_join(select(cu_run, FULL_CU_IN, CU_NAME, CU_Species, FAZ),
    by = "FULL_CU_IN") %>%
  relocate(CU_NAME, CU_Species, FAZ, .after = FULL_CU_IN)

migr_all_flat <- left_join(migr_all_flat, df_migr_cu,
  join_by("FULL_CU_IN"))


save(cu_migr_stats, migrT_rcps, migrQ_rcps, migrA19_rcps, migrA21_rcps,
  migr_all_flat,
  file = file.path(paths$fw, paste0(today, "_migr_stats.Rdata")))





## ----------------- PLOTTING-----------------------------
#
# load(file.path(paths$fw, "2025-07-31_migr_stats.Rdata"))
#
# data <- filter(migr_all_flat, period %in% c("1981-2010","2041-2060"), RCP == "45")
#
# stat_suffix <- "mean"
# gcm_range_suffix <- c("q10", "q90")
# #take column names that contain prefix with model type
# cols_sub <- names(data)[str_detect(names(data), "migrA21_")]
#
# stat_col <- cols_sub[str_detect(cols_sub, paste0(stat_suffix, "$"))]  #must end with wmean
# min_gcmcol <- cols_sub[str_detect(cols_sub, paste0(gcm_range_suffix[1], collapse = "|"))]
# max_gcmcol <- cols_sub[str_detect(cols_sub, paste0(gcm_range_suffix[2], collapse = "|"))]
#
# id_col <- "CU_NAME"
# sp_col <- "CU_Species"
# per_col <- "period"
#
# # Check if mean and ID columns exist
# if (!all(c(id_col, stat_col) %in% names(data))) {
#   stop("One or more required columns are missing in the data.")
# }
#
# # Prepare data for plotting
# plot_data <- data %>%
#   select(all_of(c(per_col, id_col, stat_col, sp_col, min_gcmcol, max_gcmcol))) %>%
#   rename(mean = !!stat_col, id = !!id_col, sp = !!sp_col,
#          min = !!min_gcmcol, max = !!max_gcmcol)
#
# # Create a named color palette using unique categories
# unique_sp <- unique(plot_data$sp)
# palette_colors <- pal_npg("nrc")(length(unique_sp))
# names(palette_colors) <- unique_sp
#
# plot_data$color <- palette_colors[plot_data$sp]
# plot_data$label <- paste0("<span style='color:", plot_data$color, "'>", plot_data$sp, "</span>")
#
# # Create a unique ordering of id by sp
# ordered_ids <- plot_data %>%
#   arrange(sp, id) %>%
#   distinct(id) %>%
#   pull(id)
#
# # Apply the ordering
# plot_data$id <- factor(plot_data$id, levels = ordered_ids)
#
# ggplot(plot_data, aes(x = id, color = sp)) +
#   #scale_fill_manual(values = palette_colors) +
#   geom_segment(aes(xend = id, y = min, yend = max),
#                color = "darkgrey",
#                linewidth = 2.5) +
#   geom_point(aes(y = mean, shape = period), size = 2.5) +
#   labs(x = "CU", y = "mean", color = "Species") +
#   coord_flip() +
#   theme_minimal() +
#   theme(axis.text.y = element_text(size = 7))
#
#
# cu_i <- "CK-12"
#
# cu_boundary_i <- cu_boundary[cu_boundary$FULL_CU_IN == cu_i, ]
# nuseds_cu <- nuseds_Fr[nuseds_Fr$FULL_CU_IN == cu_i, ]
#
# migr_cu <- migr_list[[cu_i]]
#
#
# ggplot() +
#   geom_sf(data = cu_boundary_i,
#           fill = "grey",
#           alpha = 0.5) +
#   geom_sf(data = migr_cu) +
#   geom_sf(data = nuseds_cu, colour = "darkgreen")
#
#
# plot_data <- cu_timing_Fr %>%
#   mutate(rt_sd = (rt_end - rt_start) / 6) %>%
#   rowwise() %>%
#   do(data.frame(
#     id = .$FULL_CU_IN,
#     day = seq(1,365),
#     density = dnorm(seq(1,365), mean = .$rt_peak, sd = .$rt_sd)
#   )) %>%
#   mutate(day_density = day * density)
#
#   ridge_data <- plot_data %>%
#     gather(key="id", value="density") %>%
#     mutate(value = round(as.numeric(value),0))
#
# ggplot(plot_data, aes(x = day, y = density, color = factor(id))) +
#   geom_line(size = 1) +
#   labs(title = "Salmon Run Timing as Normal Distributions",
#        x = "Day of Year", y = "Density", color = "Run ID") +
#   theme_minimal()
#
#   gather(key="text", value="rt_start") %>%
#   mutate(text = gsub("\\.", " ",text)) %>%
#   mutate(value = round(as.numeric(value),0))
#
#
#     library(ggridges)
#   library(viridis)
#
#
#   ggplot( data =plot_data, aes(y=id, x=density,  fill=id)) +
#   geom_density_ridges(alpha=0.6, bandwidth=4) +
#   scale_fill_viridis(discrete=TRUE) +
#   scale_color_viridis(discrete=TRUE) +
#   theme(
#     legend.position="none",
#     panel.spacing = unit(0.1, "lines"),
#     strip.text.x = element_text(size = 8)
#   ) +
#   xlab("") +
#   ylab("Assigned Probability (%)")
#
#
# ggplot() +
#   geom()
#
#
#
# #
# # cu_boundary_i <- cu_boundary[cu_boundary$CUID == cuid_i, ]
# # nuseds_CU <- nuseds_Fr[nuseds_Fr$FULL_CU_IN == CU_IN_i,]
# #
# # #
# # ggplot(migr_cu_low) +
# #   geom_sf(aes(color = STREAM_ORD)) +
# #   geom_sf(data = cu_boundary_i, color = "black", alpha = 0.1)
# #
# # ggplot(migr_cu_high) +
# #   geom_sf(aes(color = STREAM_ORD)) +
# #   geom_sf(data = cu_boundary_i, color = "black", alpha = 0.1)
# #
# # # FWA_Fr_ord9 <- FWA_Fr_high %>%
# # #   filter(STREAM_ORD >= 8)
# # #
# # ggplot() +
# #   geom_sf(data = bc_coast) +
# #   geom_sf(data = FWA_Fr_ord9, aes(color = STREAM_ORD))
# #
# #
# #
# #
# # PCIC_daymigr_long <- pivot_longer(PCIC_daymigr_thr,
# #                                   cols = c(T_19, T_21, T_23),
# #                                   names_to = "T_threshold", values_to = "prop") %>%
# #   mutate(year = as.factor(year))
# #
# # plot_cols <- c(T_19 = "black", T_21 = "orange", T_23 = "red")
# #
# # ggplot(PCIC_daymigr_long, aes(x = as.Date(paste0(hist_ystart, "-01-01")) + day, y = prop, linetype = year, colour = T_threshold)) +
# #   geom_vline(xintercept = as.Date(paste0(hist_ystart, "-01-01")) + rt_s,
# #              linetype = "dotted", colour = "darkgrey", linewidth = 1) +
# #   geom_text(aes(x = as.Date(paste0(hist_ystart, "-01-01")) + rt_s, y = -0.02),
# #             label = "Run timing start", colour = "darkgrey",size = 3) +
# #   geom_vline(xintercept = as.Date(paste0(hist_ystart, "-01-01")) + rt_e,
# #              linetype = "dotted", colour = "darkgrey", linewidth = 1) +
# #   geom_text(aes(x = as.Date(paste0(hist_ystart, "-01-01")) + rt_e, y = -0.02),
# #             label = "Run timing end", colour = "darkgrey",size = 3) +
# #   geom_vline(xintercept = as.Date(paste0(hist_ystart, "-01-01")) + sp_timing_peak,
# #              linetype = "dashed", colour = "darkgrey", linewidth = 1) +
# #   geom_text(aes(x = as.Date(paste0(hist_ystart, "-01-01")) + sp_timing_peak, y = -0.05),
# #             label = "Spawning peak", colour = "darkgrey", size = 3) +
# #   geom_line(linewidth = 1.3) +
# #   #scale_x_date(date_breaks = "1 month", date_labels = "%b") +
# #   scale_color_manual(values = plot_cols) +
# #   scale_linetype_discrete(labels=c(paste0(hist_ystart,"-",hist_ystart+20), paste0(proj_ystart,"-", proj_ystart+20))) +
# #   labs(color = "T threshold", y = "Number of grid cells", x = "Date") +
# #   xlim( as.Date(paste0(hist_ystart, "-01-01")) +rt_s-50, as.Date(paste0(hist_ystart, "-01-01")) + sp_timing_peak + 50)
#
