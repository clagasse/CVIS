################################################################################
#
# 2e_FW_migration_stats.R
#
#  Summarize upstream migration indicators from PCIC model outputs
#
#  1) import PCIC outputs of ensemble and model averages, averaged over each time period
#  2) import CU paths with downstream distances
#  3) for each CU path, use a moving migration window to subset PCIC data: 
#         a. begin within 100km of the river mouth at run timing start
#         b. use the spawn timing start as the date when the migration window reaches spawning sites
#         c. use the distance to spawning sites/ (# days between spawn timing start and run timing start)
#           as the rate to move the front of the migration window upstream from run timing start
#         d. use the distance to spawning sites. (# days between spawn timing peak and run timing end) 
#            as the rate to move the tail of the migration window upstream from run timing end
#         e. migration window reaches spawning sites and ends at spawn timing peak

#  4) for each day of year, determine which PCIC grid cells intersect with migration window
#  5) take mean and range of temperature and discharge for intersecting PCIC grid cells for each day of migration window
#  6) summarize temperature and discharge indicators across migration window, ESMs and RCPs
#
#
################################################################################# 

library(here)
setwd(here())
source(file.path(here(), "code", "0_setup.R"))

historical <- "0"   #historical climatology period for temperature models
# 0 = 1981-2000,  1 = 2001-2020
#for flow models, 0 = 1981-2010

qlow_gcm <- 0.1    #lower quantile for statistics on GCM variation
qhigh_gcm <- 0.9   #upper quantile for statistics
qlow_sp  <- 0.1    #lower quantile for temporal variation within migration window
qhigh_sp <- 0.9    # upper quantile for temporal variation


#--------- 2. load spatial objects ---------------------
#load CU paths
load(file.path(paths$fw, "2025-07-22_fw_upstream_paths.Rdata"))

#load PCIC daily outputs
PCIC_file_loc <- file.path(paths$climate, "PCIC_averaged", "combined")

#read PCIC outputs, with dimensions for each model, time period, and day of year
PCIC_daily <- read_mdim(file.path(PCIC_file_loc, "daily_rcp45.nc"))
#PCIC_daily_85 <- read_mdim(file.path(PCIC_file_loc, "daily_rcp85.nc"))

#get base PCIC grid
PCIC_base <- PCIC_daily[,,,1,1,1]


# # Convert to array
# arr <- as.array(PCIC_daily_45[[1]])
# 
# # Combine x and y into a single dimension
# dim_combined <- prod(dim(arr)[1:2])  # total number of spatial cells
# other_dims <- dim(arr)[-c(1,2)]      # other dimensions (e.g., bands)
# 
# # Flatten the array
# arr_flat <- array(arr, dim = c(dim_combined, other_dims))


#--------------------- 3. Functions for indicators -----------------------------------


# grid cell level indicators along migratory pathway using PCIC model
migr_cu_stats <- tibble(cuid) %>% 
  mutate( rt_start        = NA,
          rt_end         = NA,
          sp_peak       = NA,
          migr_dist           = NA,
          migr_elev           = NA,
          n_grid_cells        = NA,
          Tmigr_hist_mean     = NA,
          Tmigr_hist_sd       = NA,
          Tmigr_proj_mean     = NA,
          Tmigr_proj_sd       = NA,
          Tmigr_diff          = NA,
          Tthr_hist_mean      = NA,
          Tthr_proj_mean      = NA,
          Qmigr_hist_mean     = NA,
          Qmigr_hist_sd       = NA,
          Qmigr_proj_mean     = NA,
          Qmigr_proj_sd       = NA,
          Qmigr_propdiff      = NA) %>%
  left_join(select(cu_list, cuid, CU_NAME, FULL_CU_IN, Species_simple), join_by(cuid)) %>%
  relocate(CU_NAME:Species_simple)


for(i in 1:n.iter) {
  cu_i <- cu_run$FULL_CU_IN[i]
  migr_cu <- migr_list[[cu_i]] %>%
    st_transform(4269)
  migr_cu_main <- filter(migr_cu, prop_paths == 1)   #select only paths leading to all NuSEDS sites
  
  #get cu timing and calculate movement rates for migration window
  cu_timing_i <- cu_timing_Fr[cu_timing_Fr$FULL_CU_IN == cu_i,] %>%
    select(rt_start, rt_end, sp_start, sp_peak, sp_end) %>%
    mutate(rt_to_sp_s = sp_start - rt_start,  #days between spawn timing start and run timing start, used to calculate movement rate of front
           rt_to_sp_p = sp_peak - rt_end,     #days from spawn timing peak to run timing end, used to calculate movement of tail
           migr_s     = rt_start,           #start date for migration window used to calculate exposure - set to run timing start
           migr_e     = sp_peak,            #end date for migration window - set to peak spawn timing
           migr_dur   = round(migr_e - migr_s),
           max_dist   = max(migr_cu$downstream_distance, na.rm = T),
           main_dist  = sum(migr_cu_main$length_metre), #distance in common for all sites
           wtd_dist   = sum(migr_cu$length_metre * migr_cu$prop_paths),  #weighted average migration distance
           main_elev  = max(st_coordinates(migr_cu_main)[,3]),  #highest point in common for all sites
           max_elev   = max(st_coordinates(migr_cu)[,3]),      #highest point along all of migration path (ie. highest NUSEDS site)
           s_rate     = max_dist / (rt_to_sp_s),  #movement rate of front of migration window
           e_rate     = max_dist / (rt_to_sp_p))  #movement rate of tail of migration window
  
  if(sum(!is.na(migr_cu)) == 0) next   #skip if path is empty
  if(is.na(cu_timing_i$migr_s) | is.na(cu_timing_i$migr_e)) next  #skip if timing info missing
  
  #get matrix indicating which cells overlap with each stream segment
  # migr_PCIC_int <- st_intersects(PCIC_base, migr_cu, sparse = FALSE)
  # incl_migr <- which(apply(migr_PCIC_int, 1, sum) > 0)
  
  ### SUBSET grid to migration path and timing window
  
  PCIC_cu <- PCIC_daily[migr_cu] %>%   #subset cells that intersect with migr_cu
    filter(time >= cu_timing_i$migr_s, # filter out values outside migration window
           time <= cu_timing_i$migr_e)
  
  ### AGGREGATE OVER DIMENSIONS to get summary statistics
  
  ## a. SPATIAL DIMENSION
  ## Get statistics across x and y dimensions, using the stream paths to determine which grid cells to include
  # include all PCIC grid cells that intersect with migration path
  # weight the grid cells based on the proportion of NUSEDS spawning sites the stream segments leads to
  # i.e. terminal segments that only lead to one site out of many for a CU have lower weighting
  
  # determine which grid cells intersect with migration path, then get prop_paths for each cell
  intersects_sparse <- st_intersects(PCIC_cu, migr_cu, sparse = FALSE)  #get PCIC cells that intersect with path
  intersects_prop <- apply(intersects_sparse, 1, function(x) {
    vals <- x * migr_cu$prop_paths
    mean(vals[vals > 0], na.rm = T)
  })#weight path by prop_path
  
  sum_prop <- sum(intersects_prop, na.rm = T)  #get the sum of all prop_paths for weighted statistics
  
  #make into 2-D matrix
  intersects_mat <- matrix(intersects_prop, nrow = dim(PCIC_cu)[["x"]], ncol = dim(PCIC_cu)[["y"]])
  
  #create a stars object from this matrix
  intersect_stars <- st_as_stars(intersects_mat)
  names(intersect_stars) <- "path_prop"
  #confirm dimensions match
  st_dimensions(intersect_stars) <- st_dimensions(PCIC_cu)[c("x", "y")]
  #add prop_path as an attribute 
  PCIC_cu$prop_path <- intersect_stars[[1]]
  
  #calculate weighted temperature and discharge
  PCIC_cu$weighted_T <- PCIC_cu$waterTemperature * PCIC_cu$prop_path
  PCIC_cu$weighted_Q <- PCIC_cu$discharge * PCIC_cu$prop_path
  
  # Get weighted average over x and y — keeping period, day of year, and model
  PCIC_avg_sp <- st_apply(
    PCIC_cu[c("weighted_T")],  # variable to calc
    MARGIN = c("period", "model", "time"),  # dimensions to keep
    FUN = function(x) {
      sum(x, na.rm = T) / sum_prop
    }
  )
  
  #### TIME DIMENSION
  ## Aggregate over time - get average, min, max, and quantiles across days within migration window

  #get average over time dimension, preserving spatial and other dimensions
  PCIC_avg_t <- st_apply(
    PCIC_avg_sp, 
    MARGIN = c("period", "model"),
    FUN = mean, 
    na.rm = T)
  
  PCIC_qtile_t <- st_apply(
    PCIC_avg_sp, 
    MARGIN = c("period", "model"),  # dimensions to keep
    FUN = quantile,
    probs = c(qlow_sp, qhigh_sp),
    na.rm = TRUE
  )
                      

  #get index of grid cells that intersect with each stream segment
  intersections <- st_intersects(PCIC_base, migr_cu)
  
  #create a mapping table
  intersection_df <- data.frame(
    grid_id = rep(seq_along(intersections), lengths(intersections)),
    line_id = unlist(intersections)
  )


  #create a matrix of streams to include for each day of the migration
  migr_win <- matrix(NA, nrow = nrow(migr_cu), ncol = 365)
  up_dist <- vector("numeric", 365)
  dn_dist <- vector("numeric", 365)
  
  #for each day of year, subset stream segments that fall within migration window based on downstream distance
  for(day in 1:ncol(mig_win)) {
    up_dist[day] <- max(min((day - rt_s) * s_rate, max_dist), 0)
    dn_dist[day] <- max(min((day - rt_e) * e_rate, max_dist), 0)
    
    migr_win[,day] <- migr_cu$downstream_distance < up_dist[day] & migr_cu$downstream_distance > dn_dist[day]
  }
  
  i <- 200
  
  migr_win_day <- migr_win[,i]
  
  PCIC_win_day <- migr_PCIC_int[migr_win_day & !is.na(migr_win_day),]
  
  migr_cu_day <- migr_cu[migr_win_day & !is.na(migr_win_day),]
  
  PCIC_migr_day <- PCIC_base[migr_PCIC_int]
  
  #on each day, select PCIC grid cells that overlap with selected stream segments
  for(day in 1:ncol(mig_win)) {
    #the index of stream segments from migr_cu corresponds to the index of cells in migr_join
    PCIC_base[migr_join[,migr_win[,day]]]
  }
  

  migr_thr <- PCIC_migr_day_CU %>%
    mutate(year = year(time), 
           month = month(time),
           day = yday(time),
           T_19 = if_else(waterTemperature > 19, T, F),
           T_21 = if_else(waterTemperature > 21, T, F),
           T_23 = if_else(waterTemperature > 23, T, F)) %>%
    group_by(year, day) %>%
    summarize(
      n = n(),
      T_19 = sum(T_19, na.rm = T) ,
      T_21 = sum(T_21, na.rm = T) ,
      T_23 = sum(T_23, na.rm = T) )
  
  PCIC_daymigr_hist_CU <- PCIC_migr_cu %>%
    filter(yday(time) >= rt_s, yday(time) <= rt_e, year(time) == hist_ystart)
  
  # PCIC_migr_hist_CU_high <- st_crop(PCIC_day, migr_cu_high) %>%
  #   filter(yday(time) >= timing_s, yday(time) <= timing_e, year(time) == hist_ystart) %>%
  #   aggregate(by = "year", FUN = mean)
  
  # PCIC_migr_proj_CU_high <- PCIC_day[migr_cu_high] %>%
  #   filter(yday(time) >= timing_s, yday(time) <= timing_e, year(time) == proj_ystart) %>%
  #   aggregate(by = "year", FUN = mean)
  
  
  migr_cu_stats$n_grid_cells[i]       <- sum(!is.na(PCIC_migr_hist_CU$waterTemperature)) 
  migr_cu_stats$n_cells_main[i]       <- sum(!is.na(PCIC_mainmigr_cu$waterTemperature))
  
  migr_cu_stats$Tmigr_hist_mean[i]    <- mean(PCIC_migr_hist_CU$waterTemperature, na.rm = T)
  migr_cu_stats$Tmigr_hist_sd[i]      <- sd(PCIC_migr_hist_CU$waterTemperature, na.rm = T)
  migr_cu_stats$Tmigr_proj_mean[i]    <- mean(PCIC_migr_proj_CU$waterTemperature, na.rm = T)
  migr_cu_stats$Tmigr_proj_sd[i]      <- sd(PCIC_migr_proj_CU$waterTemperature, na.rm = T)
  migr_cu_stats$Tmigr_diff[i]         <- migr_cu_stats$Tmigr_proj_mean[i] - migr_cu_stats$Tmigr_hist_mean[i]
  
  migr_cu_stats$Tdd_hist[i]           <- sum(PCIC_migr_hist_CU$waterTemperature, na.rm = T)
  migr_cu_stats$Tdd_proj[i]           <- sum(PCIC_migr_proj_CU$waterTemperature, na.rm = T)
  migr_cu_stats$Tdd_diff[i]           <- migr_cu_stats$Tdd_proj[i] - migr_cu_stats$Tdd_hist[i]
  
  migr_cu_stats$Tdays_19_hist         <- sum(PCIC_daymigr_hist_CU$waterTemperature >= 19, na.rm = T) / migr_cu_stats$n_grid_cells[i]
  
  
  migr_cu_stats$Tbar19_hist[i]        <- PCIC_migr_hist_CU$waterTemperature >= 19
  
  migr_cu_stats$Qmigr_hist_mean[i]    <- mean(PCIC_migr_hist_CU$discharge, na.rm = T)
  migr_cu_stats$Qmigr_hist_sd[i]      <- sd(PCIC_migr_hist_CU$discharge, na.rm = T)
  migr_cu_stats$Qmigr_proj_mean[i]    <- mean(PCIC_migr_proj_CU$discharge, na.rm = T)
  migr_cu_stats$Qmigr_proj_sd[i]      <- sd(PCIC_migr_proj_CU$discharge, na.rm = T)
  migr_cu_stats$Qmigr_propdiff[i]     <- (migr_cu_stats$Qmigr_proj_mean[i] - migr_cu_stats$Qmigr_hist_mean[i]) / migr_cu_stats$Qmigr_hist_mean[i]
  
  migr_cu_stats$Qmigrlow_hist_mean[i]    <- mean(PCIC_migr_hist_CU_low$discharge, na.rm = T)
  migr_cu_stats$Qmigrlow_proj_mean[i]    <- mean(PCIC_migr_proj_CU_low$discharge, na.rm = T)
  migr_cu_stats$Qmigrlow_propdiff[i]     <- (migr_cu_stats$Qmigrlow_proj_mean[i] - migr_cu_stats$Qmigrlow_hist_mean[i]) / migr_cu_stats$Qmigrlow_hist_mean[i]
  
}

CVIS_migr <- migr_cu_stats %>%
  select(cuid, CU_NAME, FULL_CU_IN, Species_simple, 
         migr_dist, Tmigr_proj_mean, Qmigr_propdiff) %>%
  rename(MIG_EXP_T = Tmigr_proj_mean, MIG_EXP_Q = Qmigr_propdiff,
         MIG_SEN_len = migr_dist)

save(migr_cu_stats, CVIS_migr,
     file = here("processed_data", "freshwater", "R_data", paste0(today, "_MIGr_stats.Rdata")))


cu_boundary_i <- cu_boundary[cu_boundary$CUID == cuid_i, ]
nuseds_CU <- nuseds_Fr[nuseds_Fr$FULL_CU_IN == CU_IN_i,]

# 
ggplot(migr_cu_low) +
  geom_sf(aes(color = STREAM_ORD)) +
  geom_sf(data = cu_boundary_i, color = "black", alpha = 0.1)

ggplot(migr_cu_high) + 
  geom_sf(aes(color = STREAM_ORD)) +
  geom_sf(data = cu_boundary_i, color = "black", alpha = 0.1)

# FWA_Fr_ord9 <- FWA_Fr_high %>%
#   filter(STREAM_ORD >= 8)
# 
ggplot() +
  geom_sf(data = bc_coast) +
  geom_sf(data = FWA_Fr_ord9, aes(color = STREAM_ORD))




PCIC_daymigr_long <- pivot_longer(PCIC_daymigr_thr, 
                                  cols = c(T_19, T_21, T_23), 
                                  names_to = "T_threshold", values_to = "prop") %>%
  mutate(year = as.factor(year))

plot_cols <- c(T_19 = "black", T_21 = "orange", T_23 = "red")

ggplot(PCIC_daymigr_long, aes(x = as.Date(paste0(hist_ystart, "-01-01")) + day, y = prop, linetype = year, colour = T_threshold)) +
  geom_vline(xintercept = as.Date(paste0(hist_ystart, "-01-01")) + rt_s, 
             linetype = "dotted", colour = "darkgrey", linewidth = 1) +
  geom_text(aes(x = as.Date(paste0(hist_ystart, "-01-01")) + rt_s, y = -0.02), 
            label = "Run timing start", colour = "darkgrey",size = 3) +
  geom_vline(xintercept = as.Date(paste0(hist_ystart, "-01-01")) + rt_e, 
             linetype = "dotted", colour = "darkgrey", linewidth = 1) +
  geom_text(aes(x = as.Date(paste0(hist_ystart, "-01-01")) + rt_e, y = -0.02), 
            label = "Run timing end", colour = "darkgrey",size = 3) +
  geom_vline(xintercept = as.Date(paste0(hist_ystart, "-01-01")) + sp_timing_peak, 
             linetype = "dashed", colour = "darkgrey", linewidth = 1) +
  geom_text(aes(x = as.Date(paste0(hist_ystart, "-01-01")) + sp_timing_peak, y = -0.05), 
            label = "Spawning peak", colour = "darkgrey", size = 3) +
  geom_line(linewidth = 1.3) +
  #scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_color_manual(values = plot_cols) +
  scale_linetype_discrete(labels=c(paste0(hist_ystart,"-",hist_ystart+20), paste0(proj_ystart,"-", proj_ystart+20))) +
  labs(color = "T threshold", y = "Number of grid cells", x = "Date") +
  xlim( as.Date(paste0(hist_ystart, "-01-01")) +rt_s-50, as.Date(paste0(hist_ystart, "-01-01")) + sp_timing_peak + 50)

