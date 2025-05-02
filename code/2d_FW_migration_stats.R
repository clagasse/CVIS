## 2d_MIGration_stats.R

if(do_FAZ == TRUE) n.iter <- n.FAZ else n.iter <- n.CUs

# grid cell level indicators along migratory pathway using PCIC model
migr_CU_stats <- tibble(cuid) %>% 
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


## create a moving area for the upstream migration

# 1. begin within 100km of the river mouth at run timing start
# 2. use the spawn timing start as the date when the migration window reaches spawning sites
# 3. use the distance to spawning sites/ (# days between spawn timing start and run timing start)
#    as the rate to move the front of the migration window upstream from run timing start
# 4. use the distance to spawning sites. (# days between spawn timing peak and run timing end) 
#    as the rate to move the tail of the migration window upstream from run timing end
# 5. migration window reaches spawning sites and ends at spawn timing peak


for(i in 1:n.iter) {
  cu_i <- cu_run$FULL_CU_IN[i]
  path_CU <- path_list[[cu_i]]
  path_CU_main <- filter(path_CU, prop_paths == 1)   #select only paths leading to all NuSEDS sites
  
  if(sum(!is.na(path_CU)) == 0) next
  
  rt_s <- cu_timing_Fr$rt_start[cu_timing_Fr$FULL_CU_IN == cu_i]
  rt_e <- cu_timing_Fr$rt_end[cu_timing_Fr$FULL_CU_IN == cu_i]
  sp_timing_s    <- cu_timing_Fr$sp_start[cu_timing_Fr$FULL_CU_IN == cu_i]
  sp_timing_peak <- cu_timing_Fr$sp_peak[cu_timing_Fr$FULL_CU_IN == cu_i]
  
  rt_to_sp_s <- sp_timing_s - rt_s
  rt_to_sp_e  <- sp_timing_peak - rt_e
  mig_dur <- sp_timing_peak - rt_s
  
  max_dist <- max(path_CU$downstream_distance, na.rm = T)
  s_rate <- max_dist / (rt_to_sp_s)
  e_rate <- max_dist / (rt_to_sp_e)
  
  migr_CU_stats$rt_start[i]       <-  rt_s
  migr_CU_stats$rt_end[i]         <-  rt_e
  migr_CU_stats$sp_peak[i]        <-  sp_timing_peak
  
  migr_CU_stats$migr_dist_main[i]      <-  sum(path_CU_main$LENGTH_MET) / 1000  #distance in common for all sites
  migr_CU_stats$migr_dist_weighted[i]  <-  sum(path_CU$LENGTH_MET * path_CU$prop_paths) / 1000 
  migr_CU_stats$migr_dist_max[i]       <- max(path_CU$downstream_distance, na.rm = T) / 1000
  migr_CU_stats$migr_elev[i]           <- max(st_coordinates(path_CU_main)[,3])     #highest point in common for all sites

  if(is.na(rt_s) | is.na(sp_timing_peak)) next
  
  #create a matrix of streams to include for each day of the migration
  mig_win <- matrix(NA, nrow = nrow(path_CU), ncol = 365)
  up_dist <- vector("numeric", 365)
  dn_dist <- vector("numeric", 365)
  
  for(day in 1:ncol(mig_win)) {
    
    up_dist[day] <- max(min((day - rt_s) * s_rate, max_dist), 0)
    dn_dist[day] <- max(min((day - rt_e) * e_rate, max_dist), 0)
    
    mig_win[,day] <- path_CU$downstream_distance < up_dist[day] & path_CU$downstream_distance > dn_dist[day]
    st_as_stars(st_bbox(sf), values = NA_real_,
    ...)
  }
  
  PCIC_migr_CU <- PCIC_day[path_CU] #%>%
    #st_join(select(path_CU, downstream_distance))
  
  PCIC_slice <- slice(PCIC_migr_CU, time, 1)
  temp <- st_rasterize(select(path_CU, downstream_distance), PCIC_slice)
  temp <- st_set_dimensions(temp, c("x", "y") , names = c("lon", "lat"))
  
  temp <- as_tibble(temp)
  
  PCIC_tibble <- as_tibble(PCIC_migr_CU) %>%
    left_join(temp, by = c("lon", "lat"))

  PCIC_migr_hist_CU <- PCIC_migr_CU %>%
    filter(year(time) == hist_ystart) %>%
    aggregate(by = "year", FUN = mean)
  
  PCIC_migr_CU <- c(PCIC_migr_CU, temp)
  
  
  PCIC_mainmigr_CU <- PCIC_day[path_CU_main]
  PCIC_migr_CU_low <- PCIC_day[path_CU_low]
  
  path_CU_test <- mutate(mean_temp <- st_extract(PCIC_migr_CU, path_CU, fun = mean, na.rm = TRUE))
  

  
  PCIC_migr_hist_CU <- PCIC_migr_CU %>%
    filter(year(time) == hist_ystart) #, yday(time) >= rt_s, yday(time) <= sp_timing_peak, ) %>%
    #aggregate(by = "year", FUN = mean)
  
  PCIC_migr_proj_CU <- PCIC_migr_CU %>%
    filter(yday(time) >= rt_s, yday(time) <= sp_timing_peak, year(time) == proj_ystart) %>%
    aggregate(by = "year", FUN = mean)
  
  # PCIC_migr_hist_CU_low <- PCIC_migr_CU_low %>%
  #   filter(yday(time) >= rt_s, yday(time) <= sp_timing_peak, year(time) == hist_ystart) %>%
  #   aggregate(by = "year", FUN = mean)
  # 
  # PCIC_migr_proj_CU_low <- PCIC_migr_CU_low %>%
  #   filter(yday(time) >= rt_s, yday(time) <= sp_timing_peak, year(time) == proj_ystart) %>%
  #   aggregate(by = "year", FUN = mean)
  
  PCIC_migr_day_CU <-  as_tibble(PCIC_migr_CU) %>%
    filter(!is.na(waterTemperature)) #day >= rt_s, day <= sp_timing_peak,
  
  PCIC_daymigr_thr <- PCIC_migr_day_CU %>%
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
  
  PCIC_daymigr_hist_CU <- PCIC_migr_CU %>%
    filter(yday(time) >= rt_s, yday(time) <= rt_e, year(time) == hist_ystart)
  
  # PCIC_migr_hist_CU_high <- st_crop(PCIC_day, path_CU_high) %>%
  #   filter(yday(time) >= timing_s, yday(time) <= timing_e, year(time) == hist_ystart) %>%
  #   aggregate(by = "year", FUN = mean)
  
  # PCIC_migr_proj_CU_high <- PCIC_day[path_CU_high] %>%
  #   filter(yday(time) >= timing_s, yday(time) <= timing_e, year(time) == proj_ystart) %>%
  #   aggregate(by = "year", FUN = mean)
  
  
  migr_CU_stats$n_grid_cells[i]       <- sum(!is.na(PCIC_migr_hist_CU$waterTemperature)) 
  migr_CU_stats$n_cells_main[i]       <- sum(!is.na(PCIC_mainmigr_CU$waterTemperature))
  
  migr_CU_stats$Tmigr_hist_mean[i]    <- mean(PCIC_migr_hist_CU$waterTemperature, na.rm = T)
  migr_CU_stats$Tmigr_hist_sd[i]      <- sd(PCIC_migr_hist_CU$waterTemperature, na.rm = T)
  migr_CU_stats$Tmigr_proj_mean[i]    <- mean(PCIC_migr_proj_CU$waterTemperature, na.rm = T)
  migr_CU_stats$Tmigr_proj_sd[i]      <- sd(PCIC_migr_proj_CU$waterTemperature, na.rm = T)
  migr_CU_stats$Tmigr_diff[i]         <- migr_CU_stats$Tmigr_proj_mean[i] - migr_CU_stats$Tmigr_hist_mean[i]
  
  migr_CU_stats$Tdd_hist[i]           <- sum(PCIC_migr_hist_CU$waterTemperature, na.rm = T)
  migr_CU_stats$Tdd_proj[i]           <- sum(PCIC_migr_proj_CU$waterTemperature, na.rm = T)
  migr_CU_stats$Tdd_diff[i]           <- migr_CU_stats$Tdd_proj[i] - migr_CU_stats$Tdd_hist[i]
  
  migr_CU_stats$Tdays_19_hist         <- sum(PCIC_daymigr_hist_CU$waterTemperature >= 19, na.rm = T) / migr_CU_stats$n_grid_cells[i]
  
  
  migr_CU_stats$Tbar19_hist[i]        <- PCIC_migr_hist_CU$waterTemperature >= 19
  
  migr_CU_stats$Qmigr_hist_mean[i]    <- mean(PCIC_migr_hist_CU$discharge, na.rm = T)
  migr_CU_stats$Qmigr_hist_sd[i]      <- sd(PCIC_migr_hist_CU$discharge, na.rm = T)
  migr_CU_stats$Qmigr_proj_mean[i]    <- mean(PCIC_migr_proj_CU$discharge, na.rm = T)
  migr_CU_stats$Qmigr_proj_sd[i]      <- sd(PCIC_migr_proj_CU$discharge, na.rm = T)
  migr_CU_stats$Qmigr_propdiff[i]     <- (migr_CU_stats$Qmigr_proj_mean[i] - migr_CU_stats$Qmigr_hist_mean[i]) / migr_CU_stats$Qmigr_hist_mean[i]
  
  migr_CU_stats$Qmigrlow_hist_mean[i]    <- mean(PCIC_migr_hist_CU_low$discharge, na.rm = T)
  migr_CU_stats$Qmigrlow_proj_mean[i]    <- mean(PCIC_migr_proj_CU_low$discharge, na.rm = T)
  migr_CU_stats$Qmigrlow_propdiff[i]     <- (migr_CU_stats$Qmigrlow_proj_mean[i] - migr_CU_stats$Qmigrlow_hist_mean[i]) / migr_CU_stats$Qmigrlow_hist_mean[i]
  
}

CVIS_migr <- migr_CU_stats %>%
  select(cuid, CU_NAME, FULL_CU_IN, Species_simple, 
         migr_dist, Tmigr_proj_mean, Qmigr_propdiff) %>%
  rename(MIG_EXP_T = Tmigr_proj_mean, MIG_EXP_Q = Qmigr_propdiff,
         MIG_SEN_len = migr_dist)

save(migr_CU_stats, CVIS_migr,
     file = here("processed_data", "freshwater", "R_data", paste0(today, "_MIGr_stats.Rdata")))



# cuid_i <- 312
# 
# path_CU <- path_list[[i]]
# 
# ggplot(path_CU) +
#   geom_histogram(aes(x = STREAM_ORD))


cu_boundary_i <- cu_boundary[cu_boundary$CUID == cuid_i, ]
nuseds_CU <- nuseds_Fr[nuseds_Fr$FULL_CU_IN == CU_IN_i,]

# 
ggplot(path_CU_low) +
  geom_sf(aes(color = STREAM_ORD)) +
  geom_sf(data = cu_boundary_i, color = "black", alpha = 0.1)

ggplot(path_CU_high) + 
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

