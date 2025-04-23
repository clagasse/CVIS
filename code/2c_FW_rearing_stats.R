################################################################################
##  2c_FW_rearing_stats.R
# This code will create summary statistics of climate models and other models
# within each CU boundary to represent changes to the freshwater environment
# for the spawning, incubation, and rearing life stages of Pacific salmon

do_FAZ <- F   #if TRUE, will use FAZ boundaries instead of CUs
do_PCIC <- T  #calculate PCIC grid cell statistics?


#--------------------- CREATE DATAFRAMES FOR CU STATS---------------------------

# stream level indicators within CU boundaries
spn_stats <- tibble(cuid) %>% #,select(as_tibble(fw_amod[1:n.CUs,]), Tw8_0_00_0, Tw8_9_45_3) %>%
  mutate(dur_spn = NA,   #duration of spawning and rearing period
         length_sum = NA,  #length of stream reaches
         avg_length = NA,  #average length of stream reaches
         avg_order = NA,  #average stream order
         n_streams = NA,   #number of stream reaches
         cu_area = NA,     #area of CU boundary
         Tw8_0_00_0 = NA,   #1981-2000 August T
         Tw8_0_00_1 = NA,   #2001-2020 August T
         sd_Tw8_0_00_1 = NA,#standard deviation historic Aug T
         sd_Tw8_9_45_3 = NA,
         # Tav_0_00_1 = NA,  #7DEC 2001-2020 ensemble mean T
         # ThiPI_0_00_1 = NA, #7DEC 2001-2020 ensemble 85% quantile T
         # Tav_9_45_3 = NA,  #7DEC 2041-2060 ensemble mean T
         # ThiPI_9_45_3 = NA, #7DEC 2041-2060 ensemble 85% quantile T
         # 
         # Risk16_len = NA, #7DEC 2041-2060 ensemble mean risk of 16C
         # Risk20_len = NA, #7DEC 2041-2060 ensemble mean risk of 18C
         # Risk24_len = NA, #7DEC 2041-2060 ensemble mean risk of 20C
         # Risk16_prop = NA,
         # Risk20_prop = NA,
         # Risk24_prop = NA,
         
         CT_anad_mean = NA, #cumulative stressor score, weighted by stream reach
         CT_anad_025 = NA,
         CT_anad_975 = NA,
         
         MAD_hist = NA,     #historic mean annual discharge
         MAD_proj = NA,
         meanQ_1_hist = NA, #mean monthly flow for each month
         meanQ_2_hist = NA,
         meanQ_3_hist = NA,
         meanQ_4_hist = NA,
         meanQ_5_hist = NA,
         meanQ_6_hist = NA,
         meanQ_7_hist = NA,
         meanQ_8_hist = NA,
         meanQ_9_hist = NA,
         meanQ_10_hist = NA,
         meanQ_11_hist = NA,
         meanQ_12_hist = NA,
         meanQ_win_hist = NA,
         meanQ_1_proj = NA,
         meanQ_2_proj = NA,
         meanQ_3_proj = NA,
         meanQ_4_proj = NA,
         meanQ_5_proj = NA,
         meanQ_6_proj = NA,
         meanQ_7_proj = NA,
         meanQ_8_proj = NA,
         meanQ_9_proj = NA,
         meanQ_10_proj = NA,
         meanQ_11_proj = NA,
         meanQ_12_proj = NA,
         meanQ_win_proj = NA,
         meanQ_8_diff = NA,
         meanQ_win_diff = NA,
         meanQ_8_pchange = NA,
         meanQ_win_pchange = NA,
         MADprop_8_hist = NA,
         MADprop_win_hist = NA,
         MADprop_8_proj = NA,
         MADprop_win_proj = NA,
         MADprop_8_diff = NA,
         MADprop_win_diff = NA,
         
         SPN_ENM_fav_hist = NA,
         SPN_ENM_fav_proj = NA,
         SPN_ENM_fav_diff = NA
         )  %>%
  left_join(select(cu_list, cuid, CU_NAME, FULL_CU_IN, Species_simple), join_by(cuid)) %>%
  relocate(CU_NAME:Species_simple)

if(do_FAZ == TRUE) {
  spn_stats <- spn_stats[1:n.FAZ,] %>%
    select(-c(CU_NAME:Species_simple)) %>%
    mutate( FAZ_Acrony = FAZ_Fr$FAZ_Acrony,
           FAZ_Name = FAZ_Fr$FAZ_Name) %>%
    relocate(FAZ_Acrony,FAZ_Name)
}

# Grid cell level indicators from PCIC model within CU boundaries
spn_PCIC_CU <- select(spn_stats, cuid, CU_NAME, FULL_CU_IN, Species_simple) %>%
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
         augT_PCIC_rate      = NA,
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



#----------------------CALCULATE CU STATISTICS --------------------------------

if(do_FAZ == TRUE) n.iter <- n.FAZ else n.iter <- n.CUs

#n.iter <- 3

for(i in 1:n.iter) {
  
  if(do_FAZ == FALSE) {
    cuid_i <- cu_run$cuid[i]
    sp_pick <- cu_run$spp[cu_run$cuid == cuid_i] #species abbr
    # Subset CU boundary
    cu_boundary_i <- cu_boundary[cu_boundary$CUID == cuid_i,]
    #subset CU migration path
    #path_CU <- flatten(path_list[names(path_list) == cuid_i])
    
    #subset thermalscapes accessible streams within CU boundary
    amod_CU <- fw_amod[stream_cu_picks[,i],] #%>%
      #filter(!is.na(Tw8_0_00_1))
    
    # get species-specific ENM reaches
    reaches_ENM_sp <- get(paste0("reaches_ENM_", sp_pick))
    
    #subset ENM outputs for CU
    pick_st <- lengths(st_intersects(reaches_ENM_sp, cu_boundary_i)) > 0
    reaches_ENM_cu <- reaches_ENM_sp[pick_st,]
    

  }  else if(do_FAZ == TRUE) {
    cuid_i <- FAZ_Fr$FAZ_Acrony[i]
    cu_boundary_i <- FAZ_Fr[FAZ_Fr$FAZ_Acrony == cuid_i,]
    amod_CU <- fw_amod[stream_FAZ_picks[,i],] %>%
      filter(!is.na(Tw8_0_00_1))
  }
  
  spn_stats$dur_spn[i] = cu_run$Peak_Spawn_To_Ocean_Entry_Days[cu_run$cuid == cuid_i]
  
  total_length = sum(amod_CU$Shape_Length, na.rm=T)
  
  spn_stats$length_sum[i]    <- sum(amod_CU$Shape_Length, na.rm = T)
  spn_stats$avg_length[i]    <- mean(amod_CU$Shape_Length, na.rm = T)
  spn_stats$avg_order[i]     <- mean(amod_CU$STREAM_ORDER, na.rm = T)
  spn_stats$n_streams[i]     <- nrow(amod_CU)
  spn_stats$cu_area[i]       <- st_area(cu_boundary_i) / 1e6
  
  # Temperautre statistics
  spn_stats$Tw8_0_00_0[i]  <- Hmisc::wtd.mean(amod_CU$Tw8_0_00_0, amod_CU$Shape_Length)
  spn_stats$Tw8_0_00_1[i]  <- Hmisc::wtd.mean(amod_CU$Tw8_0_00_1, amod_CU$Shape_Length)
  spn_stats$sd_Tw8_0_00_1[i] <- sqrt(Hmisc::wtd.var(amod_CU$Tw8_0_00_1, amod_CU$Shape_Length))
  spn_stats$sd_Tw8_9_45_3[i] <- sqrt(Hmisc::wtd.var(amod_CU$Tw8_9_45_3, amod_CU$Shape_Length))
  #spn_stats$Tw_z_score[i]    <- (spn_stats$Tw8_9_45_3[i] -  spn_stats$Tw8_0_00_1[i]) / spn_stats$sd_Tw8_0_00_1[i]
  
  spn_stats$SPN_EXP_rateT_9[i] <- Hmisc::wtd.mean(amod_CU$SPN_EXP_rateT_9, amod_CU$Shape_Length)
  spn_stats$SPN_EXP_rateT_1[i] <- Hmisc::wtd.mean(amod_CU$SPN_EXP_rateT_1, amod_CU$Shape_Length)
  spn_stats$SPN_EXP_rateT_2[i] <- Hmisc::wtd.mean(amod_CU$SPN_EXP_rateT_2, amod_CU$Shape_Length)
  spn_stats$SPN_EXP_rateT_3[i] <- Hmisc::wtd.mean(amod_CU$SPN_EXP_rateT_3, amod_CU$Shape_Length)
  spn_stats$SPN_EXP_rateT_4[i] <- Hmisc::wtd.mean(amod_CU$SPN_EXP_rateT_4, amod_CU$Shape_Length)
  spn_stats$SPN_EXP_rateT_5[i] <- Hmisc::wtd.mean(amod_CU$SPN_EXP_rateT_5, amod_CU$Shape_Length)
  spn_stats$SPN_EXP_rateT_6[i] <- Hmisc::wtd.mean(amod_CU$SPN_EXP_rateT_6, amod_CU$Shape_Length)
  
  spn_stats$SPN_EXP_projT_9[i] <- Hmisc::wtd.mean(amod_CU$SPN_EXP_projT_9, amod_CU$Shape_Length)
  spn_stats$SPN_EXP_projT_1[i] <- Hmisc::wtd.mean(amod_CU$SPN_EXP_projT_1, amod_CU$Shape_Length)
  spn_stats$SPN_EXP_projT_2[i] <- Hmisc::wtd.mean(amod_CU$SPN_EXP_projT_2, amod_CU$Shape_Length)
  spn_stats$SPN_EXP_projT_3[i] <- Hmisc::wtd.mean(amod_CU$SPN_EXP_projT_3, amod_CU$Shape_Length)
  spn_stats$SPN_EXP_projT_4[i] <- Hmisc::wtd.mean(amod_CU$SPN_EXP_projT_4, amod_CU$Shape_Length)
  spn_stats$SPN_EXP_projT_5[i] <- Hmisc::wtd.mean(amod_CU$SPN_EXP_projT_5, amod_CU$Shape_Length)
  spn_stats$SPN_EXP_projT_6[i] <- Hmisc::wtd.mean(amod_CU$SPN_EXP_projT_6, amod_CU$Shape_Length)
  
  spn_stats$SPN_EXP_projTp05_9[i] <- Hmisc::wtd.quantile(amod_CU$SPN_EXP_projT_9, amod_CU$Shape_Length, 0.05)
  spn_stats$SPN_EXP_projTp05_1[i] <- Hmisc::wtd.quantile(amod_CU$SPN_EXP_projT_1, amod_CU$Shape_Length, 0.05)
  spn_stats$SPN_EXP_projTp05_2[i] <- Hmisc::wtd.quantile(amod_CU$SPN_EXP_projT_2, amod_CU$Shape_Length, 0.05)
  spn_stats$SPN_EXP_projTp05_3[i] <- Hmisc::wtd.quantile(amod_CU$SPN_EXP_projT_3, amod_CU$Shape_Length, 0.05)
  spn_stats$SPN_EXP_projTp05_4[i] <- Hmisc::wtd.quantile(amod_CU$SPN_EXP_projT_4, amod_CU$Shape_Length, 0.05)
  spn_stats$SPN_EXP_projTp05_5[i] <- Hmisc::wtd.quantile(amod_CU$SPN_EXP_projT_5, amod_CU$Shape_Length, 0.05)
  spn_stats$SPN_EXP_projTp05_6[i] <- Hmisc::wtd.quantile(amod_CU$SPN_EXP_projT_6, amod_CU$Shape_Length, 0.05)
  
  spn_stats$SPN_EXP_projTp95_9[i] <- Hmisc::wtd.quantile(amod_CU$SPN_EXP_projT_9, amod_CU$Shape_Length, 0.95)
  spn_stats$SPN_EXP_projTp95_1[i] <- Hmisc::wtd.quantile(amod_CU$SPN_EXP_projT_1, amod_CU$Shape_Length, 0.95)
  spn_stats$SPN_EXP_projTp95_2[i] <- Hmisc::wtd.quantile(amod_CU$SPN_EXP_projT_2, amod_CU$Shape_Length, 0.95)
  spn_stats$SPN_EXP_projTp95_3[i] <- Hmisc::wtd.quantile(amod_CU$SPN_EXP_projT_3, amod_CU$Shape_Length, 0.95)
  spn_stats$SPN_EXP_projTp95_4[i] <- Hmisc::wtd.quantile(amod_CU$SPN_EXP_projT_4, amod_CU$Shape_Length, 0.95)
  spn_stats$SPN_EXP_projTp95_5[i] <- Hmisc::wtd.quantile(amod_CU$SPN_EXP_projT_5, amod_CU$Shape_Length, 0.95)
  spn_stats$SPN_EXP_projTp95_6[i] <- Hmisc::wtd.quantile(amod_CU$SPN_EXP_projT_6, amod_CU$Shape_Length, 0.95)
  
  # spn_stats$Tav_0_00_1[i]    <- Hmisc::wtd.mean(amod_CU$Tav_0_00_1, amod_CU$Shape_Length)
  # spn_stats$ThiPI_0_00_1[i]  <- Hmisc::wtd.mean(amod_CU$ThiPI_0_00_1, amod_CU$Shape_Length)
  # spn_stats$Tav_9_45_3[i]    <- Hmisc::wtd.mean(amod_CU$Tav_9_45_3, amod_CU$Shape_Length)
  # spn_stats$ThiPI_9_45_3[i]  <- Hmisc::wtd.mean(amod_CU$ThiPI_9_45_3, amod_CU$Shape_Length)
  # 
  # spn_stats$Risk16_len[i]    <- sum(amod_CU$Risk16_mod_len, na.rm = T) #7DEC 2041-2060 ensemble mean risk of 16C
  # spn_stats$Risk20_len[i]    <- sum(amod_CU$Risk20_mod_len, na.rm = T)
  # spn_stats$Risk24_len[i]    <- sum(amod_CU$Risk24_mod_len, na.rm = T)
  # 
  # spn_stats$Risk16_prop[i]    <- spn_stats$Risk16_len[i] / spn_stats$length_sum[i]
  # spn_stats$Risk20_prop[i]    <- spn_stats$Risk20_len[i] / spn_stats$length_sum[i]
  # spn_stats$Risk24_prop[i]    <- spn_stats$Risk24_len[i] / spn_stats$length_sum[i]
  
  #cumulative threat score
  spn_stats$CT_anad_mean[i]     <- Hmisc::wtd.mean(amod_CU$CT_anad, amod_CU$Shape_Length, na.rm = T)
  spn_stats$CT_anad_05[i]       <- Hmisc::wtd.quantile(amod_CU$CT_anad, amod_CU$Shape_Length, probs = 0.05)
  spn_stats$CT_anad_95[i]       <- Hmisc::wtd.quantile(amod_CU$CT_anad, amod_CU$Shape_Length, probs = 0.95)
  
  #flow statistics
  # spn_stats$MAD_proj[i]       <- Hmisc::wtd.mean(amod_CU$mean_flow_m3s_17_1, amod_CU$Shape_Length, na.rm = T)
  # spn_stats$MAD_hist[i]       <- Hmisc::wtd.mean(amod_CU$mean_17_40, amod_CU$Shape_Length, na.rm = T)
  # spn_stats$meanQ_1_hist[i]   <- Hmisc::wtd.mean(amod_CU$mean_flow_m3s_1_1, amod_CU$Shape_Length, na.rm = T)
  # spn_stats$meanQ_2_hist[i]   <- Hmisc::wtd.mean(amod_CU$mean_flow_m3s_2_1, amod_CU$Shape_Length, na.rm = T)
  # spn_stats$meanQ_3_hist[i]   <- Hmisc::wtd.mean(amod_CU$mean_flow_m3s_3_1, amod_CU$Shape_Length, na.rm = T)
  # spn_stats$meanQ_4_hist[i]   <- Hmisc::wtd.mean(amod_CU$mean_flow_m3s_4_1, amod_CU$Shape_Length, na.rm = T)
  # spn_stats$meanQ_5_hist[i]   <- Hmisc::wtd.mean(amod_CU$mean_flow_m3s_5_1, amod_CU$Shape_Length, na.rm = T)
  # spn_stats$meanQ_6_hist[i]   <- Hmisc::wtd.mean(amod_CU$mean_flow_m3s_6_1, amod_CU$Shape_Length, na.rm = T)
  # spn_stats$meanQ_7_hist[i]   <- Hmisc::wtd.mean(amod_CU$mean_flow_m3s_7_1, amod_CU$Shape_Length, na.rm = T)
  # spn_stats$meanQ_8_hist[i]   <- Hmisc::wtd.mean(amod_CU$mean_flow_m3s_8_1, amod_CU$Shape_Length, na.rm = T)
  # spn_stats$meanQ_9_hist[i]   <- Hmisc::wtd.mean(amod_CU$mean_flow_m3s_9_1, amod_CU$Shape_Length, na.rm = T)
  # spn_stats$meanQ_10_hist[i]  <- Hmisc::wtd.mean(amod_CU$mean_flow_m3s_10_1, amod_CU$Shape_Length, na.rm = T)
  # spn_stats$meanQ_11_hist[i]  <- Hmisc::wtd.mean(amod_CU$mean_flow_m3s_11_1, amod_CU$Shape_Length, na.rm = T)
  # spn_stats$meanQ_12_hist[i]  <- Hmisc::wtd.mean(amod_CU$mean_flow_m3s_12_1, amod_CU$Shape_Length, na.rm = T)
  # spn_stats$meanQ_win_hist[i] <- Hmisc::wtd.mean(amod_CU$mean_flow_m3s_win_1, amod_CU$Shape_Length, na.rm = T)
  # spn_stats$meanQ_1_proj[i]   <- Hmisc::wtd.mean(amod_CU$mean_1_40, amod_CU$Shape_Length, na.rm = T)
  # spn_stats$meanQ_2_proj[i]   <- Hmisc::wtd.mean(amod_CU$mean_2_40, amod_CU$Shape_Length, na.rm = T)
  # spn_stats$meanQ_3_proj[i]   <- Hmisc::wtd.mean(amod_CU$mean_3_40, amod_CU$Shape_Length, na.rm = T)
  # spn_stats$meanQ_4_proj[i]   <- Hmisc::wtd.mean(amod_CU$mean_4_40, amod_CU$Shape_Length, na.rm = T)
  # spn_stats$meanQ_5_proj[i]   <- Hmisc::wtd.mean(amod_CU$mean_5_40, amod_CU$Shape_Length, na.rm = T)
  # spn_stats$meanQ_6_proj[i]   <- Hmisc::wtd.mean(amod_CU$mean_6_40, amod_CU$Shape_Length, na.rm = T)
  # spn_stats$meanQ_7_proj[i]   <- Hmisc::wtd.mean(amod_CU$mean_7_40, amod_CU$Shape_Length, na.rm = T)
  # spn_stats$meanQ_8_proj[i]   <- Hmisc::wtd.mean(amod_CU$mean_8_40, amod_CU$Shape_Length, na.rm = T)
  # spn_stats$meanQ_9_proj[i]   <- Hmisc::wtd.mean(amod_CU$mean_9_40, amod_CU$Shape_Length, na.rm = T)
  # spn_stats$meanQ_10_proj[i]  <- Hmisc::wtd.mean(amod_CU$mean_10_40, amod_CU$Shape_Length, na.rm = T)
  # spn_stats$meanQ_11_proj[i]  <- Hmisc::wtd.mean(amod_CU$mean_11_40, amod_CU$Shape_Length, na.rm = T)
  # spn_stats$meanQ_12_proj[i]  <- Hmisc::wtd.mean(amod_CU$mean_12_40, amod_CU$Shape_Length, na.rm = T)
  # spn_stats$meanQ_win_proj[i] <- Hmisc::wtd.mean(amod_CU$mean_win_40, amod_CU$Shape_Length, na.rm = T)
  # 
  # spn_stats$meanQ_8_diff[i]     <- spn_stats$meanQ_8_proj[i] - spn_stats$meanQ_8_hist[i]
  # spn_stats$meanQ_win_diff[i]   <- spn_stats$meanQ_win_proj[i] - spn_stats$meanQ_win_hist[i]
  # spn_stats$meanQ_8_pchange[i]  <- spn_stats$meanQ_8_diff[i] / spn_stats$meanQ_8_hist[i]
  # spn_stats$meanQ_win_pchange[i]<- spn_stats$meanQ_win_diff[i] / spn_stats$meanQ_win_hist[i]
  # 
  # spn_stats$MADprop_8_hist[i]   <- Hmisc::wtd.quantile(amod_CU$MADprop_8_hist, amod_CU$Shape_Length, probs = 0.5)
  # spn_stats$MADprop_win_hist[i] <- Hmisc::wtd.quantile(amod_CU$MADprop_win_hist, amod_CU$Shape_Length, probs = 0.5)
  # spn_stats$MADprop_8_proj[i]   <- Hmisc::wtd.quantile(amod_CU$MADprop_8_proj, amod_CU$Shape_Length, probs = 0.5)
  # spn_stats$MADprop_win_proj[i] <- Hmisc::wtd.quantile(amod_CU$MADprop_win_proj, amod_CU$Shape_Length, probs = 0.5)
  
  spn_stats$SPN_EXP_winQ[i] <- Hmisc::wtd.mean(amod_CU$SPN_EXP_winQ, amod_CU$Shape_Length, na.rm = T)
  spn_stats$SPN_EXP_augQ[i] <- Hmisc::wtd.mean(amod_CU$SPN_EXP_augQ, amod_CU$Shape_Length, na.rm = T)
  
  #ENM model statistics
  spn_stats$SPN_ENM_fav_hist[i]     <- Hmisc::wtd.mean(reaches_ENM_cu$Fav_f.0_00_1, na.rm = T)
  spn_stats$SPN_ENM_fav_proj[i]     <- Hmisc::wtd.mean(reaches_ENM_cu$Fav_f.9_45_3, na.rm = T)
  spn_stats$SPN_ENM_fav_proj_5[i]     <- Hmisc::wtd.mean(reaches_ENM_cu$Fav_f.9_45_5, na.rm = T)
  spn_stats$SPN_ENM_fav_diff[i]     <- spn_stats$SPN_ENM_fav_proj[i] - spn_stats$SPN_ENM_fav_hist[i]
  spn_stats$SPN_ENM_fav_diff_5[i]     <- spn_stats$SPN_ENM_fav_proj_5[i] - spn_stats$SPN_ENM_fav_hist[i]
  
  #----------------------PCIC monthly stats ------------------------------------
  
  if(do_PCIC == T) {
    #mask cells that overlap with accessible streams within CU spawning boundary
    PCIC_Aug_CU_hist <- PCIC_month[amod_CU] %>%   #change to CU_boundary_i for all grid cells
      filter(month(time) == 8, year(time) == hist_ystart) 
    PCIC_Aug_CU_proj <- PCIC_month[amod_CU] %>%
      filter(month(time) == 8, year(time) == proj_ystart) 
    
    PCIC_May_CU_hist <- PCIC_month[amod_CU] %>%
      filter(month(time) == 5, year(time) == hist_ystart)
    PCIC_May_CU_proj <- PCIC_month[amod_CU] %>%
      filter(month(time) == 5, year(time) == proj_ystart)
    
    spn_PCIC_CU$augQ_hist_mean[i] <- mean(PCIC_Aug_CU_hist$discharge, na.rm =T)
    spn_PCIC_CU$augQ_hist_sd[i]   <- sd(PCIC_Aug_CU_hist$discharge, na.rm =T)
    spn_PCIC_CU$augQ_proj_mean[i] <- mean(PCIC_Aug_CU_proj$discharge, na.rm =T)
    spn_PCIC_CU$augQ_proj_sd[i]   <- sd(PCIC_Aug_CU_proj$discharge, na.rm =T)
    spn_PCIC_CU$augQ_diff[i]      <- spn_PCIC_CU$augQ_proj_mean[i] - spn_PCIC_CU$augQ_hist_mean[i]
    spn_PCIC_CU$augQ_z[i]         <- spn_PCIC_CU$augQ_diff[i] / spn_PCIC_CU$augQ_hist_sd[i]  
    
    spn_PCIC_CU$mayQ_hist_mean[i] <- mean(PCIC_May_CU_hist$discharge, na.rm =T)
    spn_PCIC_CU$mayQ_hist_sd[i]   <- sd(PCIC_May_CU_hist$discharge, na.rm =T)
    spn_PCIC_CU$mayQ_proj_mean[i] <- mean(PCIC_May_CU_proj$discharge, na.rm =T)
    spn_PCIC_CU$mayQ_proj_sd[i]   <- sd(PCIC_May_CU_proj$discharge, na.rm =T)
    spn_PCIC_CU$mayQ_diff[i]      <- spn_PCIC_CU$mayQ_proj_mean[i] - spn_PCIC_CU$mayQ_hist_mean[i]
    spn_PCIC_CU$mayQ_z[i]         <- spn_PCIC_CU$mayQ_diff[i] / spn_PCIC_CU$mayQ_hist_sd[i]  
    
    spn_PCIC_CU$augT_PCIC_hist_mean[i] <- mean(PCIC_Aug_CU_hist$waterTemperature, na.rm =T)
    spn_PCIC_CU$augT_PCIC_hist_sd[i]   <- sd(PCIC_Aug_CU_hist$waterTemperature, na.rm =T)
    spn_PCIC_CU$augT_PCIC_proj_mean[i] <- mean(PCIC_Aug_CU_proj$waterTemperature, na.rm =T)
    spn_PCIC_CU$augT_PCIC_proj_sd[i]   <- sd(PCIC_Aug_CU_proj$waterTemperature, na.rm =T)
    spn_PCIC_CU$augT_PCIC_diff[i]      <- spn_PCIC_CU$augT_PCIC_proj_mean[i] - spn_PCIC_CU$augT_PCIC_hist_mean[i]
    spn_PCIC_CU$augT_PCIC_rate[i]      <- spn_PCIC_CU$augT_PCIC_diff[i] / tspan
    spn_PCIC_CU$augT_PCIC_z[i]         <- spn_PCIC_CU$augT_PCIC_diff[i] / spn_PCIC_CU$augT_PCIC_hist_sd[i]  
    
    
    #### PCIC daily CU stats for CU boundary
    PCIC_day_CU <-  st_crop(PCIC_day, amod_CU) %>% 
      as_tibble()
    #get daily mean flow and temperature for all cells within CU in each time period
    PCIC_day_summary <- PCIC_day_CU %>%
      group_by(time) %>%
      dplyr::summarize(waterTemperature = mean(waterTemperature, na.rm=T),
                       discharge = mean(discharge, na.rm = T),
                       Q05    = quantile(discharge, probs = 0.05, na.rm = T)) %>%
      mutate(year = year(time), 
             month = month(time),
             day = yday(time))
    
    #get 5th percentile flow for historic period
    PCIC_Q05_hist_CU <- PCIC_day_CU %>%
      filter(year(time) == hist_ystart) %>%
      group_by(lon, lat) %>%
      dplyr::summarize(Q05_hist = quantile(discharge, probs = 0.05, na.rm = T),
                       MAD_hist = mean(discharge, na.rm = T),
                       MAD05_hist = 0.05 * MAD_hist,
                       day_Q05_hist = sum(discharge < Q05_hist),
                       day_MAD05_hist = sum(discharge < MAD05_hist))
    #apply("time", quantile, probs = 0.05, na.rm = T) 
    
    PCIC_Q05_proj_CU <- PCIC_day_CU %>%
      filter(year(time) == proj_ystart) %>%
      left_join(PCIC_Q05_hist_CU, by = c("lon", "lat")) %>%
      group_by(lon, lat) %>%
      dplyr::summarize(day_Q05 = sum(discharge < Q05_hist),
                       day_Q05_hist = first(day_Q05_hist),
                       day_MAD05 = sum(discharge < MAD05_hist),
                       MAD_hist = first(MAD_hist),
                       day_MAD05_hist = first(day_MAD05_hist))
    
    #st_apply(c("time"), quantile, probs = 0.05, na.rm = T)
    
    spn_PCIC_CU$peakQday_hist[i] <- which.max(filter(PCIC_day_summary, year == hist_ystart)$discharge)
    spn_PCIC_CU$peakQday_proj[i] <- which.max(filter(PCIC_day_summary, year == proj_ystart)$discharge)
    spn_PCIC_CU$peakQday_diff[i] <- spn_PCIC_CU$peakQday_proj[i] - spn_PCIC_CU$peakQday_hist[i]
    spn_PCIC_CU$peakT_hist[i]    <- max((filter(PCIC_day_summary, year == hist_ystart)$waterTemperature))
    spn_PCIC_CU$peakT_proj[i]    <- max((filter(PCIC_day_summary, year == proj_ystart)$waterTemperature))
    spn_PCIC_CU$dayQ05_proj_mean[i]   <- mean(PCIC_Q05_proj_CU$day_Q05, na.rm = T)
    spn_PCIC_CU$dayQ05_proj_sd[i]     <- sd(PCIC_Q05_proj_CU$day_Q05, na.rm = T)
    spn_PCIC_CU$dayMAD05_hist_mean[i]   <- mean(PCIC_Q05_proj_CU$day_MAD05, na.rm = T)
    spn_PCIC_CU$dayMAD05_proj_mean[i]   <- mean(PCIC_Q05_proj_CU$day_MAD05_hist, na.rm = T)
    
  }
  
  print(paste("CU", cuid_i, "stats done"))
}

#---------------------------------CREATE AND SAVE OUTPUTS ---------------------

if(do_FAZ == FALSE) {

all_spn_stats <- spn_stats %>%
  left_join(select(spn_PCIC_CU, -c(CU_NAME,FULL_CU_IN,Species_simple)), by = "cuid")

CVIS_spn <- all_spn_stats %>%
  select(cuid, CU_NAME, FULL_CU_IN, Species_simple,
         SPN_EXP_projT_9, SPN_EXP_rateT_9, SPN_EXP_augQ, SPN_EXP_winQ, 
         peakQday_diff, CT_anad_mean, dur_spn, SPN_ENM_fav_diff) %>%
  rename(SPN_EXP_peakQday = peakQday_diff, SPN_SEN_CT_anad = CT_anad_mean,
         SPN_SEN_dur = dur_spn)

# test correlation
cor_stats <- cor(CVIS_spn %>% select(-c(cuid, CU_NAME, FULL_CU_IN, Species_simple)), use = "pairwise.complete.obs")

## save output
save(spn_stats, spn_PCIC_CU, all_spn_stats, CVIS_spn, cor_stats,
     file = here("processed_data", "freshwater", "R_data", paste0(today, "_SPN_stats.Rdata")))

#write_csv(all_spn_stats, here("processed_data", "freshwater", paste0(today, "_fw_all_spnstats.csv")))
#write_csv(as_tibble(cor_stats), here("processed_data", "freshwater", paste0(today, "_fw_cor_stats.csv")))
#write_csv(CVIS_spn, here("processed_data", "freshwater", paste0(today, "_fw_CVIS_spn.csv")))

} else if(do_FAZ == TRUE) {
  spn_PCIC_FAZ <- spn_PCIC_CU %>%
    mutate(across(where(is.numeric), round, 2))
  
  spn_FAZ <- spn_stats %>%
    mutate(across(where(is.numeric), round, 2))
  
  all_spn_FAZ <- spn_FAZ %>%
    left_join(select(PCIC_FAZ_stats, -c(CU_NAME,FULL_CU_IN,Species_simple)), by = "cuid")
  
  CVIS_spn_FAZ <- all_FAZ_stats %>%
    select(FAZ_Acrony, FAZ_Name, 
           #Tw8_0_00_0,
            Tw8_9_45_3, Tw_rate, augT_PCIC_hist_mean, augT_PCIC_proj_mean, augT_PCIC_rate,
           meanQ_8_diff, meanQ_win_diff, meanQ_8_pchange, meanQ_win_pchange,   #absolute differences in flow
           MADprop_8_diff, MADprop_win_diff,  #differences in flow relative to MAD
           peakQday_diff, CT_anad_mean)  %>%
    rename(projT_spn = Tw8_9_45_3, rateT_spn = Tw_rate, augQ_spn = MADprop_8_diff,
           winQ_spn = MADprop_win_diff, peakQday_spn = peakQday_diff,
           CT_anad_spn = CT_anad_mean)

  # test correlation
  cor_FAZstats <- cor(CVIS_FAZ %>% select(-c(FAZ_Acrony, FAZ_Name)))
  
  ## save output
  save(spn_FAZ, spn_PCIC_FAZ, all_spn_FAZ, CVIS_spn_FAZ, cor_FAZstats,
       file = here("processed_data", "freshwater", "R_data", paste0(today, "_fw_FAZstats_output.Rdata")))
  
  write_csv(all_spn_FAZ, here("output", paste0(today, "_fw_all_FAZstats.csv")))
  write_csv(as_tibble(cor_stats), here("output", paste0(today, "_fw_cor_FAZstats.csv")))
  write_csv(CVIS_spn_FAZ, here("output", paste0(today, "_fw_CVIS_FAZstats.csv")))
  
  
}


#write_csv(PCIC_ts_compare, here("output", paste0(today, "_fw_all_stats.csv")))

# TS_7DECM_stats <- all_spn_stats %>%
#   select(cuid, CU_NAME, starts_with("Risk"), starts_with("Tw"), starts_with("Thi"), starts_with("Tav"))
# 
# write.csv(TS_7DECM_stats, here("output", paste0(today, "_fw_TS_7DECM_stats.csv")))

          