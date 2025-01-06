
#######################################################################
## FW CU Statistics

tscapes_acc$Tthr_hist <- tscapes_acc$Tw8_0_00_0 > deg_threshold
tscapes_acc$Tthr_proj <- tscapes_acc$Tw8_9_45_3 > deg_threshold

tscapes_CU_stats <- select(as_tibble(tscapes_acc[1:n.CUs,]), Tw8_0_00_0, Tw8_9_45_3) %>%
  mutate(Tw_diff = NA,
         w_Tw8_0_00_0 = NA,
         w_Tw8_9_45_3 = NA,
         w_Tw_diff = NA,
         sd_Tw8_0_00_0 = NA,
         sd_Tw8_9_45_3 = NA,
         Tw_z_score      = NA,
         Tthr_len_hist  = NA,
         Tthr_len_proj  = NA,
         prop_TThr_hist = NA,
         prop_TThr_proj = NA,
         n_streams = NA,
         length_sum = NA,
         CUID = cuid) %>%
  left_join(select(cu_list, cuid, cuname, FULL_CU_IN, Species_simple), join_by(CUID == cuid)) %>%
  relocate(CUID:Species_simple)

PCIC_CU_stats <- select(tscapes_CU_stats, CUID, cuname, FULL_CU_IN, Species_simple) %>%
  mutate(augQ_hist_mean = NA,
         augQ_hist_sd   = NA,
         augQ_proj_mean = NA,
         augQ_proj_sd   = NA,
         augQ_diff      = NA,
         augQ_z         = NA,
         mayQ_hist_mean = NA,
         mayQ_hist_sd   = NA,
         mayQ_proj_mean = NA,
         mayQ_proj_sd   = NA,
         mayQ_diff      = NA,
         mayQ_z         = NA,
         augT_PCIC_hist_mean = NA,
         augT_PCIC_hist_sd   = NA,
         augT_PCIC_proj_mean = NA,
         augT_PCIC_proj_sd   = NA,
         augT_PCIC_diff      = NA,
         augT_PCIC_z         = NA,
         peakQday_hist      = NA,
         peakQday_proj      = NA,
         peakQday_diff      = NA,
         peakT_hist         = NA,
         peakT_proj         = NA,
         dayQ05_proj_mean   = NA,
         dayQ05_proj_sd     = NA,
         dayMAD05_hist_mean = NA,
         dayMAD05_proj_mean = NA
         )
        

migr_CU_stats <- select(tscapes_CU_stats, CUID, cuname, FULL_CU_IN, Species_simple) %>%
  mutate( timing_start        = NA,
          timing_end          = NA,
          migr_dist           = NA,
          migr_elev           = NA,
          n_grid_cells        = NA,
          Tmigr_hist_mean     = NA,
          Tmigr_hist_sd       = NA,
          Tmigr_proj_mean     = NA,
          Tmigr_proj_sd       = NA,
          Tthr_hist_mean      = NA,
          Tthr_proj_mean      = NA,
          Qmigr_hist_mean     = NA,
          Qmigr_hist_sd       = NA,
          Qmigr_proj_mean     = NA,
          Qmigr_proj_sd       = NA)

          

for(i in 1:n.CUs) {
  
  cuid_i <- cu_run$cuid[i]
  # Subset CU boundary
  cu_boundary_i <- cu_boundary[cu_boundary$CUID == cuid[i], ]
  #subset CU migration path
  #path_CU <- flatten(path_list[names(path_list) == cuid_i])
  path_CU <- path_list[[i]]
  
  #subset thermalscapes accessible streams within CU boundary
  tscapes_CU <- tscapes_acc[cu_tscapes[,i],] %>%
    filter(!is.na(Tw8_0_00_0))

  total_length = sum(tscapes_CU$Shape_Length, na.rm=T)
  
  
  tscapes_CU_stats$length_sum[i]    <- sum(tscapes_CU$Shape_Length, na.rm = T)
  tscapes_CU_stats$Tw8_0_00_0[i]    <- mean(tscapes_CU$Tw8_0_00_0, na.rm = T)
  tscapes_CU_stats$Tw8_9_45_3[i]    <- mean(tscapes_CU$Tw8_9_45_3, na.rm = T)
  tscapes_CU_stats$Tw_diff[i]       <- mean(tscapes_CU$Tw8_9_45_3 - tscapes_CU$Tw8_0_00_0, na.rm = T)
  
  tscapes_CU_stats$w_Tw8_0_00_0[i]  <- Hmisc::wtd.mean(tscapes_CU$Tw8_0_00_0, tscapes_CU$Shape_Length)
  tscapes_CU_stats$w_Tw8_9_45_3[i]  <- Hmisc::wtd.mean(tscapes_CU$Tw8_9_45_3, tscapes_CU$Shape_Length)
  tscapes_CU_stats$w_Tw_diff[i]     <- Hmisc::wtd.mean(tscapes_CU$Tw8_9_45_3 - tscapes_CU$Tw8_0_00_0, tscapes_CU$Shape_Length)
  
  tscapes_CU_stats$sd_Tw8_0_00_0[i] <- sqrt(Hmisc::wtd.var(tscapes_CU$Tw8_0_00_0, tscapes_CU$Shape_Length))
  tscapes_CU_stats$sd_Tw8_9_45_3[i] <- sqrt(Hmisc::wtd.var(tscapes_CU$Tw8_9_45_3, tscapes_CU$Shape_Length))
  tscapes_CU_stats$Tw_z_score[i]    <- (tscapes_CU_stats$w_Tw8_9_45_3[i] -  tscapes_CU_stats$w_Tw8_0_00_0[i]) / tscapes_CU_stats$sd_Tw8_0_00_0[i]
  
  tscapes_CU_stats$n_streams[i]      <- nrow(tscapes_CU)
  tscapes_CU_stats$Tthr_len_hist[i]  <- sum(tscapes_CU$Tthr_hist * tscapes_CU$Shape_Length, na.rm=T)
  tscapes_CU_stats$Tthr_len_proj[i]  <- sum(tscapes_CU$Tthr_proj * tscapes_CU$Shape_Length, na.rm=T)
  tscapes_CU_stats$prop_TThr_hist[i] <- tscapes_CU_stats$Tthr_len_hist[i] / tscapes_CU_stats$length_sum[i]
  tscapes_CU_stats$prop_TThr_proj[i] <- tscapes_CU_stats$Tthr_len_proj[i] / tscapes_CU_stats$length_sum[i]
  
  ### PCIC monthly summary stats for CU boundary
  
  #mask cells that overlap with accessible streams within CU spawning boundary
  PCIC_Aug_CU_hist <- PCIC_month[tscapes_CU] %>%   #change to CU_boundary_i for all grid cells
    filter(month(time) == 8, year(time) == 1985) 
  PCIC_Aug_CU_proj <- PCIC_month[tscapes_CU] %>%
    filter(month(time) == 8, year(time) == 2055) 
  
  PCIC_May_CU_hist <- PCIC_month[tscapes_CU] %>%
    filter(month(time) == 5, year(time) == 1985)
  PCIC_May_CU_proj <- PCIC_month[tscapes_CU] %>%
    filter(month(time) == 5, year(time) == 2055)
    
  PCIC_CU_stats$augQ_hist_mean[i] <- mean(PCIC_Aug_CU_hist$flow_month, na.rm =T)
  PCIC_CU_stats$augQ_hist_sd[i]   <- sd(PCIC_Aug_CU_hist$flow_month, na.rm =T)
  PCIC_CU_stats$augQ_proj_mean[i] <- mean(PCIC_Aug_CU_proj$flow_month, na.rm =T)
  PCIC_CU_stats$augQ_proj_sd[i]   <- sd(PCIC_Aug_CU_proj$flow_month, na.rm =T)
  PCIC_CU_stats$augQ_diff[i]      <- PCIC_CU_stats$augQ_proj_mean[i] - PCIC_CU_stats$augQ_hist_mean[i]
  PCIC_CU_stats$augQ_z[i]         <- PCIC_CU_stats$augQ_diff[i] / PCIC_CU_stats$augQ_hist_sd[i]  
  
  PCIC_CU_stats$mayQ_hist_mean[i] <- mean(PCIC_May_CU_hist$flow_month, na.rm =T)
  PCIC_CU_stats$mayQ_hist_sd[i]   <- sd(PCIC_May_CU_hist$flow_month, na.rm =T)
  PCIC_CU_stats$mayQ_proj_mean[i] <- mean(PCIC_May_CU_proj$flow_month, na.rm =T)
  PCIC_CU_stats$mayQ_proj_sd[i]   <- sd(PCIC_May_CU_proj$flow_month, na.rm =T)
  PCIC_CU_stats$mayQ_diff[i]      <- PCIC_CU_stats$mayQ_proj_mean[i] - PCIC_CU_stats$mayQ_hist_mean[i]
  PCIC_CU_stats$mayQ_z[i]         <- PCIC_CU_stats$mayQ_diff[i] / PCIC_CU_stats$mayQ_hist_sd[i]  
  
  PCIC_CU_stats$augT_PCIC_hist_mean[i] <- mean(PCIC_Aug_CU_hist$tw_month, na.rm =T)
  PCIC_CU_stats$augT_PCIC_hist_sd[i]   <- sd(PCIC_Aug_CU_hist$tw_month, na.rm =T)
  PCIC_CU_stats$augT_PCIC_proj_mean[i] <- mean(PCIC_Aug_CU_proj$tw_month, na.rm =T)
  PCIC_CU_stats$augT_PCIC_proj_sd[i]   <- sd(PCIC_Aug_CU_proj$tw_month, na.rm =T)
  PCIC_CU_stats$augT_PCIC_diff[i]      <- PCIC_CU_stats$augT_PCIC_proj_mean[i] - PCIC_CU_stats$augT_PCIC_hist_mean[i]
  PCIC_CU_stats$augT_PCIC_z[i]         <- PCIC_CU_stats$augT_PCIC_diff[i] / PCIC_CU_stats$augT_PCIC_hist_sd[i]  
  
  
  #### PCIC daily CU stats for CU boundary
  PCIC_day_CU <-  st_crop(PCIC_day, tscapes_CU) %>% 
    as_tibble()
  #get daily mean flow and temperature for all cells within CU in each time period
  PCIC_day_summary <- PCIC_day_CU %>%
    group_by(time) %>%
    dplyr::summarize(tw_day = mean(tw_day, na.rm=T),
                     flow_day = mean(flow_day, na.rm = T),
                     Q05    = quantile(flow_day, probs = 0.05, na.rm = T)) %>%
    mutate(year = year(time), 
           month = month(time),
           day = yday(time))
  
  #get 5th percentile flow for historic period
  PCIC_Q05_hist_CU <- PCIC_day_CU %>%
    filter(year(time) == 1985) %>%
    group_by(lon, lat) %>%
    dplyr::summarize(Q05_hist = quantile(flow_day, probs = 0.05, na.rm = T),
                     MAD_hist = mean(flow_day, na.rm = T),
                     MAD05_hist = 0.05 * MAD_hist,
                     day_Q05_hist = sum(flow_day < Q05_hist),
                     day_MAD05_hist = sum(flow_day < MAD05_hist))
    #apply("time", quantile, probs = 0.05, na.rm = T) 
  
  PCIC_Q05_proj_CU <- PCIC_day_CU %>%
    filter(year(time) == 2055) %>%
    left_join(PCIC_Q05_hist_CU, by = c("lon", "lat")) %>%
    group_by(lon, lat) %>%
    dplyr::summarize(day_Q05 = sum(flow_day < Q05_hist),
                     day_Q05_hist = first(day_Q05_hist),
                     day_MAD05 = sum(flow_day < MAD05_hist),
                     MAD_hist = first(MAD_hist),
                     day_MAD05_hist = first(day_MAD05_hist))
  
    #st_apply(c("time"), quantile, probs = 0.05, na.rm = T)

  PCIC_CU_stats$peakQday_hist[i] <- which.max(filter(PCIC_day_summary, year == 1985)$flow_day)
  PCIC_CU_stats$peakQday_proj[i] <- which.max(filter(PCIC_day_summary, year == 2055)$flow_day)
  PCIC_CU_stats$peakQday_diff[i] <- PCIC_CU_stats$peakQday_proj[i] - PCIC_CU_stats$peakQday_hist[i]
  PCIC_CU_stats$peakT_hist[i]    <- max((filter(PCIC_day_summary, year == 1985)$tw_day))
  PCIC_CU_stats$peakT_proj[i]    <- max((filter(PCIC_day_summary, year == 2055)$tw_day))
  PCIC_CU_stats$dayQ05_proj_mean[i]   <- mean(PCIC_Q05_proj_CU$day_Q05, na.rm = T)
  PCIC_CU_stats$dayQ05_proj_sd[i]     <- sd(PCIC_Q05_proj_CU$day_Q05, na.rm = T)
  PCIC_CU_stats$dayMAD05_hist_mean[i]   <- mean(PCIC_Q05_proj_CU$day_MAD05, na.rm = T)
  PCIC_CU_stats$dayMAD05_proj_mean[i]   <- mean(PCIC_Q05_proj_CU$day_MAD05_hist, na.rm = T)
  
  ### Upstream migration stats
  timing_s <- cu_timing$rt_start[cu_timing$cuid == cuid_i]
  timing_e <- cu_timing$rt_end[cu_timing$cuid == cuid_i]
  
  PCIC_migr_hist_CU <- st_crop(PCIC_day, path_CU) %>%
    filter(yday(time) >= timing_s, yday(time) <= timing_e, year(time) == 1985) %>%
    aggregate(by = "year", FUN = mean)
  
  PCIC_migr_proj_CU <- PCIC_day[path_CU] %>%
    filter(yday(time) >= timing_s, yday(time) <= timing_e, year(time) == 2055) %>%
    aggregate(by = "year", FUN = mean)

  migr_CU_stats$timing_start[i]       <-  timing_s
  migr_CU_stats$timing_end[i]         <-  timing_e
  migr_CU_stats$migr_dist[i]          <-  sum(path_CU$LENGTH_MET) / 1000
  migr_CU_stats$migr_elev[i]          <- max(st_coordinates(path_CU)[,3])
  migr_CU_stats$n_grid_cells[i]       <- sum(!is.na(PCIC_migr_hist_CU$tw_day))
  migr_CU_stats$Tmigr_hist_mean[i]    <- mean(PCIC_migr_hist_CU$tw_day, na.rm = T)
  migr_CU_stats$Tmigr_hist_sd[i]      <- sd(PCIC_migr_hist_CU$tw_day, na.rm = T)
  migr_CU_stats$Tmigr_proj_mean[i]    <- mean(PCIC_migr_proj_CU$tw_day, na.rm = T)
  migr_CU_stats$Tmigr_proj_sd[i]      <- sd(PCIC_migr_proj_CU$tw_day, na.rm = T)
  migr_CU_stats$Tthr_hist_mean[i]     <- sum(PCIC_migr_hist_CU$tw_day > deg_threshold, na.rm = T)
  migr_CU_stats$Tthr_proj_mean[i]     <- sum(PCIC_migr_proj_CU$tw_day > deg_threshold, na.rm = T)
  migr_CU_stats$Qmigr_hist_mean[i]    <- mean(PCIC_migr_hist_CU$flow_day, na.rm = T)
  migr_CU_stats$Qmigr_hist_sd[i]      <- sd(PCIC_migr_hist_CU$flow_day, na.rm = T)
  migr_CU_stats$Qmigr_proj_mean[i]    <- mean(PCIC_migr_proj_CU$flow_day, na.rm = T)
  migr_CU_stats$Qmigr_proj_sd[i]      <- sd(PCIC_migr_proj_CU$flow_day, na.rm = T)
  
  print(paste("CU", cuid_i, "stats done"))
}


## round values for display
PCIC_CU_stats <- PCIC_CU_stats %>%
  mutate(across(where(is.numeric), round, 2))

tscapes_CU_stats <- tscapes_CU_stats %>%
  mutate(across(where(is.numeric), round, 2))

migr_CU_stats <- migr_CU_stats %>%
  mutate(across(where(is.numeric), round, 2))

indies_CU <- select(tscapes_CU_stats, CUID, cuname, FULL_CU_IN, Species_simple, 
                       w_Tw8_0_00_0, w_Tw8_9_45_3, w_Tw_diff, prop_TThr_hist, prop_TThr_proj) %>%
  left_join(select(PCIC_CU_stats, CUID,  starts_with("peakQday"), starts_with("dayMAD05")), by = "CUID") %>%
  left_join(select(migr_CU_stats, CUID, starts_with("Tmigr"), starts_with("TThr"), starts_with("Qmigr")), by = "CUID")

all_CU_stats <- tscapes_CU_stats %>%
  left_join(select(PCIC_CU_stats, -c(cuname,FULL_CU_IN,Species_simple)), by = "CUID") %>%
  left_join(select(migr_CU_stats, -c(cuname,FULL_CU_IN,Species_simple)), by = "CUID")

## save output
save(tscapes_CU_stats, PCIC_CU_stats, migr_CU_stats, indies_CU, all_CU_stats,
     file = here("output", paste0(today, "_fw_stats_output.Rdata")))

write_csv(all_CU_stats, here("output", paste0(today, "_fw_all_stats.csv")))
