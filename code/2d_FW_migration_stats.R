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

qlgcm <- 0    # lower quantile for statistics on GCM variation
qhgcm <- 1   # upper quantile for statistics
qld  <- 0.1    # lower quantile for temporal variation within migration window
qhd <- 0.9    # upper quantile for temporal variation

#--------- 1. import spatial objects ---------------------
# load CU paths
load(file.path(paths$fw, "fw_upstream_paths.Rdata"))

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
                                indicator_type = "proj",  # proj or change
                                qlowday = qld,   # lower and upper quantiles to extract
                                qhighday = qhd,
                                qlowgcm = qlgcm,
                                qhighgcm = qhgcm) {
  ## a. SPATIAL DIMENSION -----
  # Get array with average over x and y — keeping period, day of year, and model
  spatial_avg <- st_apply(
    data[attr_name],  # variable to calc
    MARGIN = c("time", "period", "model"),  # dimensions to keep
    FUN = mean, na.rm = T
  )[[1]]

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
  ## Aggregate over time - get average (or sum) and quantiles across days within migration window

  # get average (or sum) across days of year for time periods, models, and rcps
  if (type == "mean") {
    gcm_doy_avg <- apply(spatial_avg,
      MARGIN = c(2, 3),
      FUN = mean,
      na.rm = T)
  }

  if (type == "sum") {
    gcm_doy_avg <- apply(spatial_avg,
      MARGIN = c(2, 3),
      FUN = sum,
      na.rm = T)
  }

  # get average across gcm models
  gcm_stats <- tibble(period = st_get_dimension_values(data, "period"),
    mean    = NA,
    qlowgcm    = NA,
    qhighgcm   = NA)

  # take average, qlow and qhigh, and change from historical across gcms for each time period and rcp
  gcm_stats$mean     <- apply(gcm_doy_avg, 1, mean)
  gcm_stats$qlowgcm  <- apply(gcm_doy_avg, 1, quantile, probs = qlowgcm)
  gcm_stats$qhighgcm <- apply(gcm_doy_avg, 1, quantile, probs = qhighgcm)

  if (indicator_type == "change") {
    gcm_stats$hist     <- gcm_stats$mean[1]
    gcm_stats$mean     <- (gcm_stats$mean - gcm_stats$hist) / gcm_stats$hist
    gcm_stats$qlowgcm  <- (gcm_stats$qlowgcm - gcm_stats$hist) / gcm_stats$hist
    gcm_stats$qhighgcm <- (gcm_stats$qhighgcm - gcm_stats$hist) / gcm_stats$hist
  }

  output <- list(doy = doy_stats,
    gcm = gcm_stats)

  return(output)

}

# 
# # Build migration window index aligned to PCIC time dimension
# build_pcic_window_index_dynamic <- function(PCIC_base, migr_cu, migr_s, migr_e, s_rate, e_rate, max_dist, time_vals) {
#   n_time <- length(time_vals)
#   intersections <- sf::st_intersects(PCIC_base, migr_cu)
#   
#   intersection_df <- data.frame(
#     grid_id = rep(seq_along(intersections), lengths(intersections)),
#     line_id = unlist(intersections)
#   )
#   
#   n_cells <- length(intersections)
#   PCIC_ind <- matrix(FALSE, nrow = n_cells, ncol = n_time)
#   
#   for (k in seq_len(n_time)) {
#     day <- time_vals[k]
#     if (day < migr_s || day > migr_e) next
#     if (!is.finite(s_rate) || !is.finite(e_rate)) next
#     
#     head_dist <- max(min((day - migr_s) * s_rate, max_dist), 0)
#     tail_dist <- max(min((day - migr_e) * e_rate, max_dist), 0)
#     
#     migr_win_index <- which(migr_cu$downstream_distance < head_dist &
#                               migr_cu$downstream_distance > tail_dist)
#     
#     if (length(migr_win_index) == 0) next
#     int_cells <- unique(intersection_df$grid_id[intersection_df$line_id %in% migr_win_index])
#     if (length(int_cells) == 0) next
#     
#     PCIC_ind[int_cells, k] <- TRUE
#   }
#   return(PCIC_ind)
# }


#function to get an index of which streams to include for each day of the year based on upstream migration distance
get_migration_index <- function(migr_cu,
                               PCIC_base, 
                               migr_s, # start date for migration window
                               migr_e, # end date for migration window
                               s_rate,  # movement rate of head of migration window
                               e_rate,  # movement rate of tail of migration window
                               migrdist,
                               intersection_df,  #mapping table between PCIC grid cell and stream index
                               win_start_dist = 50000  #minimum upstream distance to include at start of migration window
                               )
{
  migr_win <- matrix(NA, nrow = nrow(migr_cu), ncol = 365)  #stream/day matrix
  head_dist <- vector("numeric", 365)  #daily upstream distance to use as head of migration window
  tail_dist <- vector("numeric", 365)  #daily upstream distance to use as tail of migration window
  PCIC_ind <- matrix(NA, nrow = dim(PCIC_base)[["x"]] * dim(PCIC_base)[["y"]], ncol = 365)
  #PCIC_pp <- matrix(NA, nrow = dim(PCIC_base)[["x"]] * dim(PCIC_base)[["y"]], ncol = 365)
  
  for (day in 1:ncol(migr_win)) {
    # upper range of migration window = #days since rt_start x front movement rate
    head_dist[day] <- min((day - migr_s) * s_rate, migrdist)
    head_dist[day] <- max(head_dist[day], win_start_dist)  #remove potential negative values
    tail_dist[day] <- min((day - migr_e) * e_rate, migrdist)
    tail_dist[day] <- max(tail_dist[day], 0)
    
    # stream segments within migration window distance each day
    migr_win[, day] <- migr_cu$downstream_distance <= head_dist[day] & migr_cu$downstream_distance >= tail_dist[day]
    # turn T/F into index of streams
    migr_win_index <- which(migr_win[, day] == TRUE)
    # if day is outside the migration timing window (rt_start and sp_peak), make NA
    if (day > migr_e | day < migr_s) migr_win_index <- NULL
    
    # use mapping table to convert stream index into PCIC grid cell index
    int_day <- intersection_df$grid_id[intersection_df$line_id %in% migr_win_index]
    
    # join with prop_path value
    prop_day <- bind_cols(grid_id = int_day,
                          prop_path = intersection_df$prop_path[intersection_df$line_id %in% migr_win_index])
    
    int_day <- unique(int_day)
    
    # get T/F index for each PCIC grid cell each day
    PCIC_ind[, day] <- seq(1, dim(PCIC_base)[["x"]] * dim(PCIC_base)[["y"]]) %in% int_day
    
    #       ## Remainder of this for loop is for implementing prop_path weighting, not currently working
    #       # get average prop_path for each grid cell
    #       # prop_avg <- prop_day %>%
    #       #   group_by(grid_id) %>%
    #       #   summarize(prop_avg = mean(prop_path, na.rm = T)) %>%
    #       #   ungroup()
    #       #
    #       # #create a vector corresponding to total number of grid cells,
    #       # # fill it with prop_path value using index of intersecting cells
    #       # vec <- rep(NA, dim(PCIC_base)[["x"]] * dim(PCIC_base)[["y"]])
    #       # vec[prop_avg[[1]]] <- prop_avg[[2]]
    #       #
    #       # #get prop_paths value for each PCIC grid cell each day
    #       # PCIC_pp[,day] <- vec
  }
  
  return(PCIC_ind)

}


# 3. get stats ------------------------------------------------------------

cu_migr_stats <- list() # list to store cu migration characteristics
migrT_rcps <- list()  # list to store temperature results
migrQ_rcps <- list()  # list to store discharge results
#migrA21_rcps <- list()

for (j in 1:2) {

  migrT_all <- list()  # list to store temperature results
  migrQ_all <- list()  # list to store discharge results
  #migrA21_all <- list()

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
      mutate(
        rt_to_sp_s = sp_start - rt_start,  # days between spawn timing start and run timing start, used to calculate movement rate of front
        rt_to_sp_p = sp_peak - rt_end,     # days from spawn timing peak to run timing end, used to calculate movement of tail
        migr_s     = round(rt_start),           # start date for migration window used to calculate exposure - set to run timing start
        migr_e     = round(sp_peak),            # end date for migration window - set to peak spawn timing
        migr_dur   = round(migr_e - migr_s),
        migrdist   = round(sum(migr_cu$length_metre * migr_cu$prop_paths)),  # weighted average migration distance
        s_rate     = migrdist / (rt_to_sp_s),  # movement rate of front of migration window
        e_rate     = migrdist / (rt_to_sp_p),  # movement rate of tail of migration window
        
        max_dist   = max(migr_cu$downstream_distance, na.rm = T),
        main_dist  = sum(migr_cu_main$length_metre), # distance in common for all sites
        main_elev  = max(st_coordinates(migr_cu_main)[, 3]),  # highest point in common for all sites
        max_elev   = max(st_coordinates(migr_cu)[, 3]))      # highest point along all of migration path (ie. highest NUSEDS site)


    if (sum(!is.na(migr_cu)) == 0 |   # skip if path is empty
      is.na(cu_timing_i$migr_s) | is.na(cu_timing_i$migr_e)) { # skip if timing info missing

      cu_migr_stats[[i]]   <- NA
      migrT_all[[i]]       <- NA
      migrQ_all[[i]]       <- NA
      #migrA21_all[[i]]  <- NA

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
    
    
    PCIC_ind <- get_migration_index(migr_cu,
                        PCIC_base, 
                        migr_s = cu_timing_i$migr_s, # start date for migration window
                        migr_e = cu_timing_i$migr_e, # end date for migration window
                        s_rate = cu_timing_i$s_rate,  # movement rate of head of migration window
                        e_rate = cu_timing_i$e_rate,  # movement rate of tail of migration window
                        migrdist = cu_timing_i$migrdist,
                        intersection_df)  #mapping table between PCIC grid cell and stream index
# 
#     # create a matrix of streams to include for each day of the migration
#     migr_win <- matrix(NA, nrow = nrow(migr_cu), ncol = 365)
#     PCIC_ind <- matrix(NA, nrow = dim(PCIC_base)[["x"]] * dim(PCIC_base)[["y"]], ncol = 365)
#     #PCIC_pp <- matrix(NA, nrow = dim(PCIC_base)[["x"]] * dim(PCIC_base)[["y"]], ncol = 365)
#     head_dist <- vector("numeric", 365)
#     tail_dist <- vector("numeric", 365)
# 
#     # for each day of year, get index of stream segments that fall within migration window using downstream distance
#     # then translate index of stream segments into index of PCIC grid cells
#     for (day in 1:ncol(migr_win)) {
#       # upper range of migration window = #days since rt_start x front movement rate
#       head_dist[day] <- max(min((day - cu_timing_i$migr_s) * cu_timing_i$s_rate, cu_timing_i$max_dist), 0)
#       tail_dist[day] <- max(min((day - cu_timing_i$migr_e) * cu_timing_i$e_rate, cu_timing_i$max_dist), 0)
# 
#       # stream segments within migration window distance each day
#       migr_win[, day] <- migr_cu$downstream_distance < head_dist[day] & migr_cu$downstream_distance > tail_dist[day]
#       # turn T/F into index of streams
#       migr_win_index <- which(migr_win[, day] == TRUE)
#       # if day is outside the migration timing window (rt_start and sp_peak), make NA
#       if (day > cu_timing_i$migr_e | day < cu_timing_i$migr_s) migr_win_index <- NULL
#       # use mapping table to convert stream index into PCIC grid cell index
#       int_day <- intersection_df$grid_id[intersection_df$line_id %in% migr_win_index]
# 
#       # join with prop_path value
#       prop_day <- bind_cols(grid_id = int_day,
#         prop_path = intersection_df$prop_path[intersection_df$line_id %in% migr_win_index])
# 
#       int_day <- unique(int_day)
# 
#       # get T/F index for each PCIC grid cell each day
#       PCIC_ind[, day] <- seq(1, dim(PCIC_base)[["x"]] * dim(PCIC_base)[["y"]]) %in% int_day
# 
#       ## Remainder of this for loop is for implementing prop_path weighting, not currently working
#       # get average prop_path for each grid cell
#       # prop_avg <- prop_day %>%
#       #   group_by(grid_id) %>%
#       #   summarize(prop_avg = mean(prop_path, na.rm = T)) %>%
#       #   ungroup()
#       #
#       # #create a vector corresponding to total number of grid cells,
#       # # fill it with prop_path value using index of intersecting cells
#       # vec <- rep(NA, dim(PCIC_base)[["x"]] * dim(PCIC_base)[["y"]])
#       # vec[prop_avg[[1]]] <- prop_avg[[2]]
#       #
#       # #get prop_paths value for each PCIC grid cell each day
#       # PCIC_pp[,day] <- vec
#     }

    daily_cells <- apply(PCIC_ind, 2, sum, na.rm = T)  # vector of number of grid cells included each day
    # reshape flattened PCIC index by grid cells into array with same dimensions as stars object
    doy_int <- array(PCIC_ind, dim = dim(PCIC_daily))

    # put the array into the stars object as a new attribute
    PCIC_daily$migration_window <- doy_int

    ### SUBSET grid to migration path and timing window -----
    PCIC_cu <- PCIC_daily[migr_cu] %>%   # subset cells that intersect with migr_cu
      filter(time >= cu_timing_i$migr_s, time <= cu_timing_i$migr_e) # filter out values outside migration window

    # new T or Q attribute with days outside of migration window turned to NA
    PCIC_cu$win_T <- PCIC_cu$waterTemperature
    PCIC_cu$win_T[!PCIC_cu$migration_window] <- NA

    PCIC_cu$win_Q <- PCIC_cu$discharge
    PCIC_cu$win_Q[!PCIC_cu$migration_window] <- NA

    ## Logical for whether cell temperature is above threshold on each day
    #PCIC_cu$win_Tthr21 <- PCIC_cu$win_T > 21

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
      type = "mean",
      indicator_type = "proj")

    migrQ_stats <- summarize_attribute(PCIC_cu,
      attr_name = "win_Q",
      type = "mean",
      indicator_type = "change")

    # migrA21_stats <- summarize_attribute(PCIC_cu,
    #   attr_name = "win_Tthr21",
    #   type = "mean",
    #   indicator_type = "proj")

    cu_migr_stats[[i]]   <- cu_timing_i
    migrT_all[[i]]       <- migrT_stats
    migrQ_all[[i]]       <- migrQ_stats
    #migrA21_all[[i]]  <- migrA21_stats

    print(paste("Migration stats complete for:", cu_i))

  }

  names(cu_migr_stats)  <- cu_run$FULL_CU_IN
  names(migrT_all)      <- cu_run$FULL_CU_IN
  names(migrQ_all)      <- cu_run$FULL_CU_IN
  #names(migrA21_all) <- cu_run$FULL_CU_IN

  migrT_rcps[[j]]      <- migrT_all
  migrQ_rcps[[j]]      <- migrQ_all
  #migrA21_rcps[[j]] <- migrA21_all

}

names(migrT_rcps) <- c("45", "85")
names(migrQ_rcps) <- c("45", "85")
#names(migrA21_rcps) <- c("45", "85")


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
        mutate(rcp = rcp_name,
          FULL_CU_IN = cu_name,
          attr = attr_label) %>%
        relocate(attr, FULL_CU_IN, rcp)
    })
  })
}

# Apply to each object
df_migrT     <- extract_gcm(migrT_rcps, "migrT")
df_migrQ     <- extract_gcm(migrQ_rcps, "migrQ")
#df_migrA21 <- extract_gcm(migrA21_rcps, "migrA21")

# Combine all into one data frame
df_migr_combined <- bind_rows(
  df_migrT,
  df_migrQ
  #df_migrA21
)


# Filter to only data frames or NULLs
cleaned_list <- cu_migr_stats %>%
  keep(~ is.data.frame(.x) || is.null(.x))
df_migr_cu  <- list_rbind(cleaned_list, names_to = "FULL_CU_IN")

migr_all_flat <- df_migr_combined %>%
  pivot_wider(
    id_cols = c(rcp, FULL_CU_IN, period),
    names_from = attr,
    values_from = c(mean, qlowgcm, qhighgcm),
    names_glue = "{attr}_{.value}"
  ) %>%
  left_join(select(cu_run, FULL_CU_IN, CU_NAME, SPECIES_NAME),
    by = "FULL_CU_IN") %>%
  relocate(CU_NAME, SPECIES_NAME, .after = FULL_CU_IN)

migr_all_flat <- left_join(migr_all_flat, df_migr_cu,
  join_by("FULL_CU_IN"))


save(cu_migr_stats, migrT_rcps, migrQ_rcps, #migrA21_rcps,
  migr_all_flat,
  file = file.path(paths$fw, paste0(today, "_migr_stats.Rdata")))
