## 2d_FW_migration_stats.R

if(do_FAZ == TRUE) n.iter <- n.FAZ else n.iter <- n.CUs

# grid cell level indicators along migratory pathway using PCIC model
migr_CU_stats <- tibble(cuid) %>% 
  mutate( timing_start        = NA,
          timing_end          = NA,
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
  cuid_i <- cu_run$cuid[i]
  
  ### Upstream migration stats - for CUs, not FAZ
  if(do_FAZ == FALSE) {
    rt_s <- cu_timing_Fr$rt_start[cu_timing_Fr$cuid == cuid_i]
    rt_e <- cu_timing_Fr$rt_end[cu_timing_Fr$cuid == cuid_i]
    sp_timing_peak <- cu_timing_Fr$sp_peak[cu_timing_Fr$cuid == cuid_i]
    
    path_CU <- path_list[[i]]
    
    path_CU_high <- path_CU %>%
      filter(STREAM_ORD >= 8)
  
    PCIC_migr_hist_CU <- st_crop(PCIC_day, path_CU) %>%
      filter(yday(time) >= rt_s, yday(time) <= sp_timing_peak, year(time) == hist_ystart) %>%
      aggregate(by = "year", FUN = mean)
    
    PCIC_migr_proj_CU <- PCIC_day[path_CU] %>%
      filter(yday(time) >= rt_s, yday(time) <= sp_timing_peak, year(time) == proj_ystart) %>%
      aggregate(by = "year", FUN = mean)
    
    # PCIC_migr_hist_CU_high <- st_crop(PCIC_day, path_CU_high) %>%
    #   filter(yday(time) >= timing_s, yday(time) <= timing_e, year(time) == hist_ystart) %>%
    #   aggregate(by = "year", FUN = mean)
    # 
    # PCIC_migr_proj_CU_high <- PCIC_day[path_CU_high] %>%
    #   filter(yday(time) >= timing_s, yday(time) <= timing_e, year(time) == proj_ystart) %>%
    #   aggregate(by = "year", FUN = mean)
    
    migr_CU_stats$mig_t_start[i]       <-  rt_s
    migr_CU_stats$mig_t_end[i]         <-  sp_timing_peak
    migr_CU_stats$migr_dist[i]          <-  sum(path_CU$LENGTH_MET) / 1000
    migr_CU_stats$migr_elev[i]          <- max(st_coordinates(path_CU)[,3])
    migr_CU_stats$n_grid_cells[i]       <- sum(!is.na(PCIC_migr_hist_CU$waterTemperature))
    
    migr_CU_stats$Tmigr_hist_mean[i]    <- mean(PCIC_migr_hist_CU$waterTemperature, na.rm = T)
    migr_CU_stats$Tmigr_hist_sd[i]      <- sd(PCIC_migr_hist_CU$waterTemperature, na.rm = T)
    migr_CU_stats$Tmigr_proj_mean[i]    <- mean(PCIC_migr_proj_CU$waterTemperature, na.rm = T)
    migr_CU_stats$Tmigr_proj_sd[i]      <- sd(PCIC_migr_proj_CU$waterTemperature, na.rm = T)
    migr_CU_stats$Tmigr_diff[i]         <- migr_CU_stats$Tmigr_proj_mean[i] - migr_CU_stats$Tmigr_hist_mean[i]

    migr_CU_stats$Qmigr_hist_mean[i]    <- mean(PCIC_migr_hist_CU$discharge, na.rm = T)
    migr_CU_stats$Qmigr_hist_sd[i]      <- sd(PCIC_migr_hist_CU$discharge, na.rm = T)
    migr_CU_stats$Qmigr_proj_mean[i]    <- mean(PCIC_migr_proj_CU$discharge, na.rm = T)
    migr_CU_stats$Qmigr_proj_sd[i]      <- sd(PCIC_migr_proj_CU$discharge, na.rm = T)
    migr_CU_stats$Qmigr_propdiff[i]     <- (migr_CU_stats$Qmigr_proj_mean[i] - migr_CU_stats$Qmigr_hist_mean[i]) / migr_CU_stats$Qmigr_hist_mean[i]
    
  }
}

CVIS_migr <- migr_CU_stats %>%
  select(cuid, CU_NAME, FULL_CU_IN, Species_simple, 
         migr_dist, Tmigr_proj_mean, Qmigr_propdiff) %>%
  rename(FW_MIG_EXP_T = Tmigr_proj_mean, FW_MIG_EXP_Q = Qmigr_propdiff,
         FW_MIG_SEN_len = migr_dist)

save(migr_CU_stats, CVIS_migr,
     file = here("processed_data", "freshwater", "R_data", paste0(today, "_fw_migr_stats.Rdata")))



# cuid_i <- 312
# 
# path_CU <- path_list[[i]]
# 
# ggplot(path_CU) +
#   geom_histogram(aes(x = STREAM_ORD))
# 
# ggplot(path_CU) +
#   geom_sf(aes(color = STREAM_ORD))
# 
# FWA_Fr_ord9 <- FWA_Fr_high %>%
#   filter(STREAM_ORD >= 8)
# 
# ggplot() +
#   geom_sf(data = bc_coast) +
#   geom_sf(data = FWA_Fr_ord9, aes(color = STREAM_ORD)) 

