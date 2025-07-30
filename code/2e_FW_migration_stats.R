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
         #i.e. stream segments shared by all sites have weighting of 1, 
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

historical <- "0"   #historical climatology period for temperature models
# 0 = 1981-2000,  1 = 2001-2020
#for flow models, 0 = 1981-2010

qlow_gcm <- 0.1    #lower quantile for statistics on GCM variation
qhigh_gcm <- 0.9   #upper quantile for statistics
qlow_day  <- 0.1    #lower quantile for temporal variation within migration window
qhigh_day <- 0.9    # upper quantile for temporal variation

T_thr <- 19   #temperature threshold for spatial extent indicator


#--------- 1. import spatial objects ---------------------
#load CU paths
load(file.path(paths$fw, "2025-07-25_fw_upstream_paths.Rdata"))

#load PCIC daily outputs
PCIC_file_loc <- file.path(paths$climate, "PCIC_averaged", "combined")

#read PCIC outputs, with dimensions for each model, time period, and day of year
PCIC_daily <- read_mdim(file.path(PCIC_file_loc, "daily_rcp45.nc"))
PCIC_daily_85 <- read_mdim(file.path(PCIC_file_loc, "daily_rcp85.nc"))

#combine with RCP scenario as a new dimension
PCIC_daily <- c(PCIC_daily, PCIC_daily_85, along = "rcp")
PCIC_daily <- st_set_dimensions(PCIC_daily, "rcp", values = c("rcp45", "rcp85"))

rm(PCIC_daily_85)

#convert Temperature from Kelvin to Celsius
PCIC_daily <- mutate(PCIC_daily, waterTemperature = waterTemperature - 273.15)

#get base PCIC grid - smaller object quicker for operations
PCIC_base <- PCIC_daily[,,,1,1,1,1]

periods    <- st_get_dimension_values(PCIC_daily, "period")
gcm_models <- st_get_dimension_values(PCIC_daily, "model")

#--------------------- 2. Functions for indicators -----------------------------------



#function to produce summary statistics across rcps, gcms, periods, and days of year for a stars attribute
summarize_attribute <- function(data,
                                attr_name = "discharge",
                                type = "mean",  #mean or sum
                                qlow_sp = qlow_day,   #lower and upper quantiles to extract
                                qhigh_sp = qhigh_day,
                                qlow = qlow_gcm,
                                qhigh = qhigh_gcm) {
  
  ## a. SPATIAL DIMENSION -----
  # Get array with average over x and y — keeping period, day of year, and model
  if(type == "mean") {
    spatial_avg <- st_apply(
      data[attr_name],  # variable to calc
      MARGIN = c("time", "period", "model", "rcp"),  # dimensions to keep
      FUN = mean, na.rm = T 
    )[[1]]
  }
  
  if(type == "sum") { 
    spatial_avg <- st_apply(
      data[attr_name],  # variable to calc
      MARGIN = c("time", "period", "model", "rcp"),  # dimensions to keep
      FUN = sum, na.rm = T 
    )[[1]]
  }
  
  dimnames(spatial_avg) <- list(st_get_dimension_values(data, "time"),
                                  st_get_dimension_values(data, "period"),
                                  st_get_dimension_values(data, "model"),
                                st_get_dimension_values(data, "rcp"))
  
  ## b. GCM dimension ------
  ## Get average and quantiles for each day across GCM outputs and attributes
  doy_avg <- apply(spatial_avg,
                            MARGIN = c(1,2,4), #dimensions to keep
                            FUN = mean, 
                            na.rm = T)
  
  doy_qlow <- apply(spatial_avg,
                    MARGIN = c(1,2,4), 
                    FUN = quantile, 
                    probs = qlow_sp,
                    na.rm = T)
  
  doy_qhigh <- apply(spatial_avg,
                    MARGIN = c(1,2,4), 
                    FUN = quantile, 
                    probs = qhigh_sp,
                    na.rm = T)
  
  doy_stats_45 <- list(doy_avg[rcp = "rcp45"], doy_qlow[rcp = "rcp45"], doy_qhigh[rcp = "rcp45"]) 
  doy_stats_85 <- list(doy_avg[rcp = "rcp85"], doy_qlow[rcp = "rcp85"], doy_qhigh[rcp = "rcp85"])
  names(doy_stats_45) <- c("mean", qlow_day, qhigh_day)
  names(doy_stats_85) <- c("mean", qlow_day, qhigh_day)
  
  ## c. TIME DIMENSION ------
  ## Aggregate over time - get average and quantiles across days within migration window
  
  #get average across days of year for time periods, models, and rcps
  gcm_doy_avg <- apply(spatial_avg,
                   MARGIN = c(2,3,4),
                   FUN = mean, 
                   na.rm = T)
  #split across rcps
  gcm_doy_avg45 <- gcm_doy_avg[rcp = "rcp45"]
  gcm_doy_avg85 <- gcm_doy_avg[rcp = "rcp85"]
  
  #get average across gcm models
  gcm_stats_45 <- tibble(period = st_get_dimension_values(data, "period"),
                           mean = NA, 
                           q10= NA, 
                           q90= NA,
                           min= NA,
                           max = NA)

  gcm_stats_85 <- gcm_stats_45
  
  #take average, qlow and qhigh across gcms for each time period and rcp
  gcm_stats_45$mean <- apply(gcm_doy_avg_45, 1, mean)
  gcm_stats_45$q10  <- apply(gcm_doy_avg_45, 1, quantile, probs = qlow)
  gcm_stats_45$q90  <- apply(gcm_doy_avg_45, 1, quantile, probs = qhigh)
  gcm_stats_45$min  <- apply(gcm_doy_avg_45, 1, min)
  gcm_stats_45$max  <- apply(gcm_doy_avg_45, 1, max)
  
  gcm_stats_85$mean <- apply(gcm_doy_avg_85, 1, mean)
  gcm_stats_85$q10  <- apply(gcm_doy_avg_85, 1, quantile, probs = qlow)
  gcm_stats_85$q90  <- apply(gcm_doy_avg_85, 1, quantile, probs = qhigh)
  gcm_stats_85$min  <- apply(gcm_doy_avg_85, 1, min)
  gcm_stats_85$max  <- apply(gcm_doy_avg_85, 1, max)
  
  #return results as a list
  output <- list(doy_45 = doy_stats_45, 
                 doy_85 = doy_stats_85, 
                 gcm_45 = gcm_stats_45, 
                 gcm_85 = gcm_stats_85)

  return(output)
  
}

migr_all <- list()  #list to store results

for(i in 1:2) {
  cu_i <- cu_run$FULL_CU_IN[i]
  migr_cu <- migr_list[[cu_i]] %>%
    st_transform(4269)
  migr_cu_main <- filter(migr_cu, prop_paths == 1)   #select only paths leading to all NuSEDS sites
  
  #get cu timing and calculate movement rates for migration window
  cu_timing_i <- cu_timing_Fr[cu_timing_Fr$FULL_CU_IN == cu_i,] %>%
    select(rt_start, rt_end, sp_start, sp_peak, sp_end) %>%
    mutate(rt_to_sp_s = sp_start - rt_start,  #days between spawn timing start and run timing start, used to calculate movement rate of front
           rt_to_sp_p = sp_peak - rt_end,     #days from spawn timing peak to run timing end, used to calculate movement of tail
           migr_s     = round(rt_start),           #start date for migration window used to calculate exposure - set to run timing start
           migr_e     = round(sp_peak),            #end date for migration window - set to peak spawn timing
           migr_dur   = round(migr_e - migr_s),
           max_dist   = max(migr_cu$downstream_distance, na.rm = T),
           main_dist  = sum(migr_cu_main$length_metre), #distance in common for all sites
           wtd_dist   = sum(migr_cu$length_metre * migr_cu$prop_paths),  #weighted average migration distance
           main_elev  = max(st_coordinates(migr_cu_main)[,3]),  #highest point in common for all sites
           max_elev   = max(st_coordinates(migr_cu)[,3]),      #highest point along all of migration path (ie. highest NUSEDS site)
           s_rate     = max_dist / (rt_to_sp_s),  #movement rate of front of migration window
           e_rate     = max_dist / (rt_to_sp_p))  #movement rate of tail of migration window
  
  if(sum(!is.na(migr_cu)) == 0 |   #skip if path is empty
     is.na(cu_timing_i$migr_s) | is.na(cu_timing_i$migr_e))  {#skip if timing info missing
    migr_all[[i]] <- NA
    next
  }
  
  PCIC_cu <- PCIC_daily  #stars object to be subsetted later
  
  ### GET MIGRATION WINDOW
  # Create a moving migration window over the cu path and input into stars array
  # get index of grid cells that intersect with each stream segment
  intersections <- st_intersects(PCIC_base, migr_cu)
  
  #create a mapping table between grid cell index and stream index that intersect, with prop_path
  intersection_df <- data.frame(
    grid_id = rep(seq_along(intersections), lengths(intersections)),
    line_id = unlist(intersections),
    prop_path = migr_cu$prop_paths[unlist(intersections)]
  )
  
  #create a matrix of streams to include for each day of the migration
  migr_win <- matrix(NA, nrow = nrow(migr_cu), ncol = 365)
  PCIC_ind <- matrix(NA, nrow = dim(PCIC_base)[["x"]] * dim(PCIC_base)[["y"]], ncol = 365)
  PCIC_pp <- matrix(NA, nrow = dim(PCIC_base)[["x"]] * dim(PCIC_base)[["y"]], ncol = 365)
  up_dist <- vector("numeric", 365)
  dn_dist <- vector("numeric", 365)
  
  #for each day of year, get index of stream segments that fall within migration window using downstream distance
  # then translate index of stream segments into index of PCIC grid cells
  for(day in 1:ncol(migr_win)) {
    #upper range of migration window = #days since rt_start x front movement rate
    up_dist[day] <- max(min((day - cu_timing_i$migr_s) * cu_timing_i$s_rate, cu_timing_i$max_dist), 0)
    dn_dist[day] <- max(min((day - cu_timing_i$migr_e) * cu_timing_i$e_rate, cu_timing_i$max_dist), 0)
    
    #stream segments within migration window distance each day
    migr_win[,day] <- migr_cu$downstream_distance < up_dist[day] & migr_cu$downstream_distance > dn_dist[day]
    #turn T/F into index of streams
    migr_win_index <- which(migr_win[,day] == TRUE)  
    #if day is outside the migration timing window (rt_start and sp_peak), make NA
    if(day > cu_timing_i$migr_e | day < cu_timing_i$migr_s) migr_win_index <- NULL
    #use mapping table to convert stream index into PCIC grid cell index
    int_day <- intersection_df$grid_id[intersection_df$line_id %in% migr_win_index]
    
    #join with prop_path value
    prop_day <- bind_cols(grid_id = int_day, 
                          prop_path = intersection_df$prop_path[intersection_df$line_id %in% migr_win_index])
    
    int_day <- unique(int_day)
    
    #get T/F index for each PCIC grid cell each day
    PCIC_ind[,day] <- seq(1, dim(PCIC_base)[["x"]] * dim(PCIC_base)[["y"]]) %in% int_day
    
    ## Remainder of this for loop is for implementing prop_path weighting, not currently working
    #get average prop_path for each grid cell
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
  
  daily_cells <- apply(PCIC_ind, 2, sum, na.rm = T)  #vector of number of grid cells included each day
  #reshape flattened PCIC index by grid cells into array with same dimensions as stars object
  doy_int <- array(PCIC_ind, dim = dim(PCIC_cu))
  #put the array into the stars object as a new attribute
  PCIC_cu$migration_window <- doy_int
  
  #new T or Q attribute with days outside of migration window turned to NA
  PCIC_cu$win_T <- PCIC_cu$waterTemperature 
  PCIC_cu$win_T[!PCIC_cu$migration_window] <- NA
  
  PCIC_cu$win_Q <- PCIC_cu$discharge 
  PCIC_cu$win_Q[!PCIC_cu$migration_window] <- NA
  
  ## Logical for whether cell temperature is above threshold on each day
  PCIC_cu$win_Tthr_19 <- PCIC_cu$win_T > 19
  PCIC_cu$win_Tthr_21 <- PCIC_cu$win_T > 21
  
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
  
  ### SUBSET grid to migration path and timing window -----
  PCIC_cu <- PCIC_cu[migr_cu] %>%   #subset cells that intersect with migr_cu
  filter(time >= cu_timing_i$migr_s, # filter out values outside migration window
         time <= cu_timing_i$migr_e)
  
  
  ### AGGREGATE OVER DIMENSIONS to get summary statistics------
  
  migrT_stats <- summarize_attribute(PCIC_cu, 
                                     attr_name = "win_T",
                                     type = "mean")
  
  migr_all[[i]] <- migrT_stats
  
  print(paste("Migration stats complete for:", cu_i))
  
  
  # 
  # ## a. SPATIAL DIMENSION -----
  # # Get average over x and y — keeping period, day of year, and model
  # PCIC_avg_sp_T <- st_apply(
  #   PCIC_cu[c("win_T")],  # variable to calc
  #   MARGIN = c("time", "period", "model"),  # dimensions to keep
  #   FUN = mean, na.rm = T 
  # )[[1]]
  # dimnames(PCIC_avg_sp_T) <- list(st_get_dimension_values(PCIC_cu, "time"),
  #                                 st_get_dimension_values(PCIC_cu, "period"),
  #                                 st_get_dimension_values(PCIC_cu, "model"))
  # 
  # PCIC_avg_sp_Q <- st_apply(
  #   PCIC_cu[c("win_Q")],  # variable to calc
  #   MARGIN = c("time", "period", "model"),  # dimensions to keep
  #   FUN = mean, na.rm = T 
  # )[[1]]
  # dimnames(PCIC_avg_sp_Q) <- list(st_get_dimension_values(PCIC_cu, "time"),
  #                                 st_get_dimension_values(PCIC_cu, "period"),
  #                                 st_get_dimension_values(PCIC_cu, "model"))
  # 
  # #sum of grid cells above threshold
  # PCIC_avg_sp_Tthr_19 <- st_apply(
  #   PCIC_cu[c("win_Tthr_19")],  # variable to calc
  #   MARGIN = c("time", "period", "model"),  # dimensions to keep
  #   FUN = sum, na.rm = T 
  # )[[1]]
  # dimnames(PCIC_avg_sp_Tthr_19) <- list(st_get_dimension_values(PCIC_cu, "time"),
  #                                 st_get_dimension_values(PCIC_cu, "period"),
  #                                 st_get_dimension_values(PCIC_cu, "model"))
  # 
  # #sum of grid cells above threshold
  # PCIC_avg_sp_Tthr_21 <- st_apply(
  #   PCIC_cu[c("win_Tthr_21")],  # variable to calc
  #   MARGIN = c("time", "period", "model"),  # dimensions to keep
  #   FUN = sum, na.rm = T 
  # )[[1]]
  # dimnames(PCIC_avg_sp_Tthr_21) <- list(st_get_dimension_values(PCIC_cu, "time"),
  #                                 st_get_dimension_values(PCIC_cu, "period"),
  #                                 st_get_dimension_values(PCIC_cu, "model"))
  # 
  # ## b. GCM dimension ------
  # ## Get average and qlow and qhigh for each day across GCM outputs and attributes
  # 
  # PCIC_avg_daily_T <- apply(PCIC_avg_sp_T,
  #                           MARGIN = c(1,2), 
  #                           FUN = mean, 
  #                           na.rm = T
  # )
  # 
  # PCIC_qlow_daily_T <- apply(PCIC_avg_sp_T,
  #                          MARGIN = c(1,2), 
  #                          FUN = quantile,
  #                          probs = qlow_gcm,
  #                          na.rm = T
  # )
  # 
  # PCIC_qhigh_daily <- st_apply(
  #   PCIC_avg_sp, 
  #   MARGIN = c("time", "period"),  
  #   FUN = quantile,
  #   probs = c(qhigh_gcm),
  #   na.rm = TRUE
  # )[[1]]   #retrieve as a matrix
  # colnames(PCIC_qhigh_daily) <- periods
  # rownames(PCIC_qhigh_daily) <- st_get_dimension_values(PCIC_cu, "time")
  # 
  # PCIC_T_daily <- list(PCIC_avg_daily, PCIC_qlow_daily, PCIC_qhigh_daily) 
  # names(PCIC_T_daily) <- c("mean", "qlow", "qhigh")
  # 
  # 
  # ## c. TIME DIMENSION ------
  # ## Aggregate over time - get average and quantiles across days within migration window
  # 
  # #get average over time dimension across time periods and models
  # PCIC_avg_gcm <- st_apply(
  #   PCIC_avg_sp, 
  #   MARGIN = c("model", "period"), # dimensions to keep
  #   FUN = mean, 
  #   na.rm = T)[[1]]
  # colnames(PCIC_avg_gcm) <- periods
  # rownames(PCIC_avg_gcm) <- gcm_models
  # 
  # PCIC_gcm_stats <- tibble(period = periods, 
  #                          mean = NA, 
  #                          q10= NA, 
  #                          q90= NA,
  #                          min= NA,
  #                          max = NA)
  # #take average, qlow and qhigh across gcms for each time period
  # PCIC_gcm_stats$mean <- apply(PCIC_avg_gcm, 2, mean)
  # PCIC_gcm_stats$q10 <- apply(PCIC_avg_gcm, 2, quantile, probs = qlow_gcm)
  # PCIC_gcm_stats$q90 <- apply(PCIC_avg_gcm, 2, quantile, probs = qhigh_gcm)
  # PCIC_gcm_stats$min <- apply(PCIC_avg_gcm, 2, min)
  # PCIC_gcm_stats$max <- apply(PCIC_avg_gcm, 2, max)
  # 
  # 
  # migr_stats_i <- list(gcm_T = PCIC_gcm_stats,
  #                      daily_T = PCIC_T_daily)
  # 

}

names(migr_all) <- cu_run$FULL_CU_IN


save(migr_all,
     file = file.path(paths$fw, paste0(today, "_migr_stats.Rdata")))



# 
# cu_boundary_i <- cu_boundary[cu_boundary$CUID == cuid_i, ]
# nuseds_CU <- nuseds_Fr[nuseds_Fr$FULL_CU_IN == CU_IN_i,]
# 
# # 
# ggplot(migr_cu_low) +
#   geom_sf(aes(color = STREAM_ORD)) +
#   geom_sf(data = cu_boundary_i, color = "black", alpha = 0.1)
# 
# ggplot(migr_cu_high) + 
#   geom_sf(aes(color = STREAM_ORD)) +
#   geom_sf(data = cu_boundary_i, color = "black", alpha = 0.1)
# 
# # FWA_Fr_ord9 <- FWA_Fr_high %>%
# #   filter(STREAM_ORD >= 8)
# # 
# ggplot() +
#   geom_sf(data = bc_coast) +
#   geom_sf(data = FWA_Fr_ord9, aes(color = STREAM_ORD))
# 
# 
# 
# 
# PCIC_daymigr_long <- pivot_longer(PCIC_daymigr_thr, 
#                                   cols = c(T_19, T_21, T_23), 
#                                   names_to = "T_threshold", values_to = "prop") %>%
#   mutate(year = as.factor(year))
# 
# plot_cols <- c(T_19 = "black", T_21 = "orange", T_23 = "red")
# 
# ggplot(PCIC_daymigr_long, aes(x = as.Date(paste0(hist_ystart, "-01-01")) + day, y = prop, linetype = year, colour = T_threshold)) +
#   geom_vline(xintercept = as.Date(paste0(hist_ystart, "-01-01")) + rt_s, 
#              linetype = "dotted", colour = "darkgrey", linewidth = 1) +
#   geom_text(aes(x = as.Date(paste0(hist_ystart, "-01-01")) + rt_s, y = -0.02), 
#             label = "Run timing start", colour = "darkgrey",size = 3) +
#   geom_vline(xintercept = as.Date(paste0(hist_ystart, "-01-01")) + rt_e, 
#              linetype = "dotted", colour = "darkgrey", linewidth = 1) +
#   geom_text(aes(x = as.Date(paste0(hist_ystart, "-01-01")) + rt_e, y = -0.02), 
#             label = "Run timing end", colour = "darkgrey",size = 3) +
#   geom_vline(xintercept = as.Date(paste0(hist_ystart, "-01-01")) + sp_timing_peak, 
#              linetype = "dashed", colour = "darkgrey", linewidth = 1) +
#   geom_text(aes(x = as.Date(paste0(hist_ystart, "-01-01")) + sp_timing_peak, y = -0.05), 
#             label = "Spawning peak", colour = "darkgrey", size = 3) +
#   geom_line(linewidth = 1.3) +
#   #scale_x_date(date_breaks = "1 month", date_labels = "%b") +
#   scale_color_manual(values = plot_cols) +
#   scale_linetype_discrete(labels=c(paste0(hist_ystart,"-",hist_ystart+20), paste0(proj_ystart,"-", proj_ystart+20))) +
#   labs(color = "T threshold", y = "Number of grid cells", x = "Date") +
#   xlim( as.Date(paste0(hist_ystart, "-01-01")) +rt_s-50, as.Date(paste0(hist_ystart, "-01-01")) + sp_timing_peak + 50)

