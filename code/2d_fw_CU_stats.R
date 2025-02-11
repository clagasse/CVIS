
#######################################################################
## FW CU Statistics

tscape_tspan <- (2050 - 1990) / 10  #number of decades between time periods
PCIC_tspan <- (2055 - 1985) / 10

do_FAZ <- F

## calculate stream level indicators

#length of stream above temperature threshold
fw_acc_indies$Tthr_hist <- (fw_acc_indies$Tw8_0_00_0 > deg_threshold) * fw_acc_indies$Shape_Length
fw_acc_indies$Tthr_proj <- (fw_acc_indies$Tw8_9_45_3 > deg_threshold) * fw_acc_indies$Shape_Length

fw_acc_indies$mean_flow_m3s_win_1 <- (fw_acc_indies$mean_flow_m3s_11_1 + fw_acc_indies$mean_flow_m3s_12_1 +
                                        fw_acc_indies$mean_flow_m3s_1_1 + fw_acc_indies$mean_flow_m3s_2_1) / 4
fw_acc_indies$mean_win_40 <- (fw_acc_indies$mean_11_40 + fw_acc_indies$mean_12_40 +
                                fw_acc_indies$mean_1_40 + fw_acc_indies$mean_2_40) / 4


#August min flow and winter max flows as %mad historic and future
fw_acc_indies$MADprop_8_hist  <- fw_acc_indies$mean_flow_m3s_8_1 / fw_acc_indies$mean_flow_m3s_17_1
fw_acc_indies$MADprop_11_hist <- fw_acc_indies$mean_flow_m3s_11_1 / fw_acc_indies$mean_flow_m3s_17_1
fw_acc_indies$MADprop_12_hist <- fw_acc_indies$mean_flow_m3s_12_1 / fw_acc_indies$mean_flow_m3s_17_1
fw_acc_indies$MADprop_1_hist  <- fw_acc_indies$mean_flow_m3s_1_1 / fw_acc_indies$mean_flow_m3s_17_1
fw_acc_indies$MADprop_2_hist  <- fw_acc_indies$mean_flow_m3s_2_1 / fw_acc_indies$mean_flow_m3s_17_1
fw_acc_indies$MADprop_win_hist <-fw_acc_indies$mean_flow_m3s_win_1 / fw_acc_indies$mean_flow_m3s_17_1

fw_acc_indies$MADprop_8_proj  <- fw_acc_indies$mean_8_40 / fw_acc_indies$mean_flow_m3s_17_1
fw_acc_indies$MADprop_11_proj <- fw_acc_indies$mean_11_40 / fw_acc_indies$mean_flow_m3s_17_1
fw_acc_indies$MADprop_12_proj <- fw_acc_indies$mean_12_40 / fw_acc_indies$mean_flow_m3s_17_1
fw_acc_indies$MADprop_1_proj  <- fw_acc_indies$mean_1_40 / fw_acc_indies$mean_flow_m3s_17_1
fw_acc_indies$MADprop_2_proj  <- fw_acc_indies$mean_2_40 / fw_acc_indies$mean_flow_m3s_17_1
fw_acc_indies$MADprop_win_proj <- fw_acc_indies$mean_win_40 / fw_acc_indies$mean_flow_m3s_17_1


ggplot(fw_acc_indies) +
  geom_point(aes(x=mean_win_40, y = mean_flow_m3s_17_1, fill = STREAM_ORDER.x)) +
  theme_minimal()


# stream level indicators within CU boundaries
fw_CU_stats <- tibble(cuid) %>% #,select(as_tibble(fw_acc_indies[1:n.CUs,]), Tw8_0_00_0, Tw8_9_45_3) %>%
  mutate(length_sum = NA,
         n_streams = NA,   #number of stream reaches
         unw_Tw8_0_00_0 = NA,
         unw_Tw8_9_45_3 = NA,
         Tw8_0_00_0 = NA,   #1981-2000 August T
         Tw8_9_45_3 = NA,   #2041-2060 ensemble mean August T
         Tw8_1_45_3 = NA,   #2041-2060 GCM mean August T
         Tw8_2_45_3 = NA,
         Tw8_3_45_3 = NA,
         Tw8_4_45_3 = NA,
         Tw8_5_45_3 = NA,
         Tw8_6_45_3 = NA,
         Tw_diff = NA,      #difference in August T 
         sd_Tw8_0_00_0 = NA,#standard deviation historic Aug T
         sd_Tw8_9_45_3 = NA,
         Tw_z_score      = NA,
         Tw_rate         = NA,  #decadal rate of change in August T
         Tw_rate_1       = NA, #decadal rate of change in August T for GCM 1
         Tw_rate_2       = NA, #decadal rate of change in August T for GCM 2
         Tw_rate_3       = NA,
         Tw_rate_4       = NA,
         Tw_rate_5       = NA,
         Tw_rate_6       = NA,
         Tthr_len_hist  = NA,   #length of stream reaches over T thresholds
         Tthr_len_proj  = NA,
         TThr_prop_hist = NA,   #proportion of stream length over T thresholds in CU
         TThr_prop_proj = NA,
         
         Tav_0_00_0 = NA,  #7DEC 1981-2000 ensemble mean T
         ThiPI_0_00_0 = NA, #7DEC 1981-2000 ensemble 85% quantile T
         Tav_9_45_3 = NA,  #7DEC 2041-2060 ensemble mean T
         ThiPI_9_45_3 = NA, #7DEC 2041-2060 ensemble 85% quantile T
         
         Risk16_len = NA, #7DEC 2041-2060 ensemble mean risk of 16C
         Risk20_len = NA, #7DEC 2041-2060 ensemble mean risk of 18C
         Risk24_len = NA, #7DEC 2041-2060 ensemble mean risk of 20C
         Risk16_prop = NA,
         Risk20_prop = NA,
         Risk24_prop = NA,
         
         
         CT_anad_mean = NA, #cumulative stressor score, weighted by stream reach
         CT_anad_025 = NA,
         CT_anad_975 = NA,
         unw_CT_anad_mean = NA,   #unweighted cum stressors score
         
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
         MADprop_win_diff = NA
         )  %>%
  left_join(select(cu_list, cuid, cuname, FULL_CU_IN, Species_simple), join_by(cuid)) %>%
  relocate(cuname:Species_simple)

if(do_FAZ == TRUE) {
  fw_CU_stats <- fw_CU_stats[1:n.FAZ,] %>%
    select(-c(cuname:Species_simple)) %>%
    mutate( FAZ_Acrony = FAZ_Fr$FAZ_Acrony,
           FAZ_Name = FAZ_Fr$FAZ_Name) %>%
    relocate(FAZ_Acrony,FAZ_Name)
}

# Grid cell level indicators from PCIC model within CU boundaries
PCIC_CU_stats <- select(fw_CU_stats, cuid, cuname, FULL_CU_IN, Species_simple) %>%
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
        
# grid cell level indicators along migratory pathway using PCIC model
migr_CU_stats <- select(fw_CU_stats, cuid, cuname, FULL_CU_IN, Species_simple) %>%
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
          Qmigr_propdiff      = NA)


if(do_FAZ == TRUE) n.iter <- n.FAZ else n.iter <- n.CUs

for(i in 1:n.iter) {
  
  if(do_FAZ == FALSE) {
  cuid_i <- cu_run$cuid[i]
  # Subset CU boundary
  cu_boundary_i <- cu_boundary[cu_boundary$CUID == cuid_i,]
  #subset CU migration path
  #path_CU <- flatten(path_list[names(path_list) == cuid_i])
  path_CU <- path_list[[i]]
  #subset thermalscapes accessible streams within CU boundary
  fw_acc_CU <- fw_acc_indies[stream_cu_picks[,i],] %>%
    filter(!is.na(Tw8_0_00_0))
  
  }  else if(do_FAZ == TRUE) {
    cuid_i <- FAZ_Fr$FAZ_Acrony[i]
    cu_boundary_i <- FAZ_Fr[FAZ_Fr$FAZ_Acrony == cuid_i,]
    fw_acc_CU <- fw_acc_indies[stream_FAZ_picks[,i],] %>%
      filter(!is.na(Tw8_0_00_0))
  }
  

  total_length = sum(fw_acc_CU$Shape_Length, na.rm=T)
  
  fw_CU_stats$length_sum[i]    <- sum(fw_acc_CU$Shape_Length, na.rm = T)
  fw_CU_stats$n_streams[i]      <- nrow(fw_acc_CU)
  
  fw_CU_stats$Tw8_0_00_0[i]  <- Hmisc::wtd.mean(fw_acc_CU$Tw8_0_00_0, fw_acc_CU$Shape_Length)
  fw_CU_stats$unw_Tw8_0_00_0[i]    <- mean(fw_acc_CU$Tw8_0_00_0, na.rm = T)   #unweighted mean
  fw_CU_stats$unw_Tw8_9_45_3[i]    <- mean(fw_acc_CU$Tw8_9_45_3, na.rm = T)
  fw_CU_stats$Tw8_9_45_3[i]  <- Hmisc::wtd.mean(fw_acc_CU$Tw8_9_45_3, fw_acc_CU$Shape_Length)
  fw_CU_stats$Tw8_1_45_3[i]  <- Hmisc::wtd.mean(fw_acc_CU$Tw8_1_45_3, fw_acc_CU$Shape_Length)
  fw_CU_stats$Tw8_2_45_3[i]  <- Hmisc::wtd.mean(fw_acc_CU$Tw8_2_45_3, fw_acc_CU$Shape_Length)
  fw_CU_stats$Tw8_3_45_3[i]  <- Hmisc::wtd.mean(fw_acc_CU$Tw8_3_45_3, fw_acc_CU$Shape_Length)
  fw_CU_stats$Tw8_4_45_3[i]  <- Hmisc::wtd.mean(fw_acc_CU$Tw8_4_45_3, fw_acc_CU$Shape_Length)
  fw_CU_stats$Tw8_5_45_3[i]  <- Hmisc::wtd.mean(fw_acc_CU$Tw8_5_45_3, fw_acc_CU$Shape_Length)
  fw_CU_stats$Tw8_6_45_3[i]  <- Hmisc::wtd.mean(fw_acc_CU$Tw8_6_45_3, fw_acc_CU$Shape_Length)
  fw_CU_stats$Tw_diff[i]     <- Hmisc::wtd.mean(fw_acc_CU$Tw8_9_45_3 - fw_acc_CU$Tw8_0_00_0, fw_acc_CU$Shape_Length)
  
  fw_CU_stats$sd_Tw8_0_00_0[i] <- sqrt(Hmisc::wtd.var(fw_acc_CU$Tw8_0_00_0, fw_acc_CU$Shape_Length))
  fw_CU_stats$sd_Tw8_9_45_3[i] <- sqrt(Hmisc::wtd.var(fw_acc_CU$Tw8_9_45_3, fw_acc_CU$Shape_Length))
  fw_CU_stats$Tw_z_score[i]    <- (fw_CU_stats$Tw8_9_45_3[i] -  fw_CU_stats$Tw8_0_00_0[i]) / fw_CU_stats$sd_Tw8_0_00_0[i]
  fw_CU_stats$Tw_rate[i]       <- fw_CU_stats$Tw_diff[i] / tscape_tspan
  fw_CU_stats$Tw_rate_1[i]     <- Hmisc::wtd.mean(fw_acc_CU$Tw8_1_45_3 - fw_acc_CU$Tw8_0_00_0, fw_acc_CU$Shape_Length) / tscape_tspan
  fw_CU_stats$Tw_rate_2[i]     <- Hmisc::wtd.mean(fw_acc_CU$Tw8_2_45_3 - fw_acc_CU$Tw8_0_00_0, fw_acc_CU$Shape_Length) / tscape_tspan
  fw_CU_stats$Tw_rate_3[i]     <- Hmisc::wtd.mean(fw_acc_CU$Tw8_3_45_3 - fw_acc_CU$Tw8_0_00_0, fw_acc_CU$Shape_Length) / tscape_tspan
  fw_CU_stats$Tw_rate_4[i]     <- Hmisc::wtd.mean(fw_acc_CU$Tw8_4_45_3 - fw_acc_CU$Tw8_0_00_0, fw_acc_CU$Shape_Length) / tscape_tspan
  fw_CU_stats$Tw_rate_5[i]     <- Hmisc::wtd.mean(fw_acc_CU$Tw8_5_45_3 - fw_acc_CU$Tw8_0_00_0, fw_acc_CU$Shape_Length) / tscape_tspan
  fw_CU_stats$Tw_rate_6[i]     <- Hmisc::wtd.mean(fw_acc_CU$Tw8_6_45_3 - fw_acc_CU$Tw8_0_00_0, fw_acc_CU$Shape_Length) / tscape_tspan
  
  fw_CU_stats$Tthr_len_hist[i]  <- sum(fw_acc_CU$Tthr_hist, na.rm=T)
  fw_CU_stats$Tthr_len_proj[i]  <- sum(fw_acc_CU$Tthr_proj, na.rm=T)
  fw_CU_stats$TThr_prop_hist[i] <- fw_CU_stats$Tthr_len_hist[i] / fw_CU_stats$length_sum[i]
  fw_CU_stats$TThr_prop_proj[i] <- fw_CU_stats$Tthr_len_proj[i] / fw_CU_stats$length_sum[i]
  
  fw_CU_stats$Tav_0_00_0[i]    <- Hmisc::wtd.mean(fw_acc_CU$Tav_0_00_0, fw_acc_CU$Shape_Length)
  fw_CU_stats$ThiPI_0_00_0[i]  <- Hmisc::wtd.mean(fw_acc_CU$ThiPI_0_00_0, fw_acc_CU$Shape_Length)
  fw_CU_stats$Tav_9_45_3[i]    <- Hmisc::wtd.mean(fw_acc_CU$Tav_9_45_3, fw_acc_CU$Shape_Length)
  fw_CU_stats$ThiPI_9_45_3[i]  <- Hmisc::wtd.mean(fw_acc_CU$ThiPI_9_45_3, fw_acc_CU$Shape_Length)
  
  fw_CU_stats$Risk16_len[i]    <- sum(fw_acc_CU$Risk16_mod_len, na.rm = T) #7DEC 2041-2060 ensemble mean risk of 16C
  fw_CU_stats$Risk20_len[i]    <- sum(fw_acc_CU$Risk20_mod_len, na.rm = T)
  fw_CU_stats$Risk24_len[i]    <- sum(fw_acc_CU$Risk24_mod_len, na.rm = T)
  
  fw_CU_stats$Risk16_prop[i]    <- fw_CU_stats$Risk16_len[i] / fw_CU_stats$length_sum[i]
  fw_CU_stats$Risk20_prop[i]    <- fw_CU_stats$Risk20_len[i] / fw_CU_stats$length_sum[i]
  fw_CU_stats$Risk24_prop[i]    <- fw_CU_stats$Risk24_len[i] / fw_CU_stats$length_sum[i]
  
  fw_CU_stats$CT_anad_mean[i]      <- Hmisc::wtd.mean(fw_acc_CU$CT_anad, fw_acc_CU$Shape_Length, na.rm = T)
  fw_CU_stats$CT_anad_025[i]       <- Hmisc::wtd.quantile(fw_acc_CU$CT_anad, fw_acc_CU$Shape_Length, probs = 0.025)
  fw_CU_stats$CT_anad_975[i]       <- Hmisc::wtd.quantile(fw_acc_CU$CT_anad, fw_acc_CU$Shape_Length, probs = 0.975)
  fw_CU_stats$unw_CT_anad_mean[i]  <- mean(fw_acc_CU$CT_anad, na.rm = T)
  
  fw_CU_stats$MAD_proj[i]       <- Hmisc::wtd.mean(fw_acc_CU$mean_flow_m3s_17_1, fw_acc_CU$Shape_Length, na.rm = T)
  fw_CU_stats$MAD_hist[i]       <- Hmisc::wtd.mean(fw_acc_CU$mean_17_40, fw_acc_CU$Shape_Length, na.rm = T)
  fw_CU_stats$meanQ_1_hist[i]   <- Hmisc::wtd.mean(fw_acc_CU$mean_flow_m3s_1_1, fw_acc_CU$Shape_Length, na.rm = T)
  fw_CU_stats$meanQ_2_hist[i]   <- Hmisc::wtd.mean(fw_acc_CU$mean_flow_m3s_2_1, fw_acc_CU$Shape_Length, na.rm = T)
  fw_CU_stats$meanQ_3_hist[i]   <- Hmisc::wtd.mean(fw_acc_CU$mean_flow_m3s_3_1, fw_acc_CU$Shape_Length, na.rm = T)
  fw_CU_stats$meanQ_4_hist[i]   <- Hmisc::wtd.mean(fw_acc_CU$mean_flow_m3s_4_1, fw_acc_CU$Shape_Length, na.rm = T)
  fw_CU_stats$meanQ_5_hist[i]   <- Hmisc::wtd.mean(fw_acc_CU$mean_flow_m3s_5_1, fw_acc_CU$Shape_Length, na.rm = T)
  fw_CU_stats$meanQ_6_hist[i]   <- Hmisc::wtd.mean(fw_acc_CU$mean_flow_m3s_6_1, fw_acc_CU$Shape_Length, na.rm = T)
  fw_CU_stats$meanQ_7_hist[i]   <- Hmisc::wtd.mean(fw_acc_CU$mean_flow_m3s_7_1, fw_acc_CU$Shape_Length, na.rm = T)
  fw_CU_stats$meanQ_8_hist[i]   <- Hmisc::wtd.mean(fw_acc_CU$mean_flow_m3s_8_1, fw_acc_CU$Shape_Length, na.rm = T)
  fw_CU_stats$meanQ_9_hist[i]   <- Hmisc::wtd.mean(fw_acc_CU$mean_flow_m3s_9_1, fw_acc_CU$Shape_Length, na.rm = T)
  fw_CU_stats$meanQ_10_hist[i]  <- Hmisc::wtd.mean(fw_acc_CU$mean_flow_m3s_10_1, fw_acc_CU$Shape_Length, na.rm = T)
  fw_CU_stats$meanQ_11_hist[i]  <- Hmisc::wtd.mean(fw_acc_CU$mean_flow_m3s_11_1, fw_acc_CU$Shape_Length, na.rm = T)
  fw_CU_stats$meanQ_12_hist[i]  <- Hmisc::wtd.mean(fw_acc_CU$mean_flow_m3s_12_1, fw_acc_CU$Shape_Length, na.rm = T)
  fw_CU_stats$meanQ_win_hist[i] <- Hmisc::wtd.mean(fw_acc_CU$mean_flow_m3s_win_1, fw_acc_CU$Shape_Length, na.rm = T)
  fw_CU_stats$meanQ_1_proj[i]   <- Hmisc::wtd.mean(fw_acc_CU$mean_1_40, fw_acc_CU$Shape_Length, na.rm = T)
  fw_CU_stats$meanQ_2_proj[i]   <- Hmisc::wtd.mean(fw_acc_CU$mean_2_40, fw_acc_CU$Shape_Length, na.rm = T)
  fw_CU_stats$meanQ_3_proj[i]   <- Hmisc::wtd.mean(fw_acc_CU$mean_3_40, fw_acc_CU$Shape_Length, na.rm = T)
  fw_CU_stats$meanQ_4_proj[i]   <- Hmisc::wtd.mean(fw_acc_CU$mean_4_40, fw_acc_CU$Shape_Length, na.rm = T)
  fw_CU_stats$meanQ_5_proj[i]   <- Hmisc::wtd.mean(fw_acc_CU$mean_5_40, fw_acc_CU$Shape_Length, na.rm = T)
  fw_CU_stats$meanQ_6_proj[i]   <- Hmisc::wtd.mean(fw_acc_CU$mean_6_40, fw_acc_CU$Shape_Length, na.rm = T)
  fw_CU_stats$meanQ_7_proj[i]   <- Hmisc::wtd.mean(fw_acc_CU$mean_7_40, fw_acc_CU$Shape_Length, na.rm = T)
  fw_CU_stats$meanQ_8_proj[i]   <- Hmisc::wtd.mean(fw_acc_CU$mean_8_40, fw_acc_CU$Shape_Length, na.rm = T)
  fw_CU_stats$meanQ_9_proj[i]   <- Hmisc::wtd.mean(fw_acc_CU$mean_9_40, fw_acc_CU$Shape_Length, na.rm = T)
  fw_CU_stats$meanQ_10_proj[i]  <- Hmisc::wtd.mean(fw_acc_CU$mean_10_40, fw_acc_CU$Shape_Length, na.rm = T)
  fw_CU_stats$meanQ_11_proj[i]  <- Hmisc::wtd.mean(fw_acc_CU$mean_11_40, fw_acc_CU$Shape_Length, na.rm = T)
  fw_CU_stats$meanQ_12_proj[i]  <- Hmisc::wtd.mean(fw_acc_CU$mean_12_40, fw_acc_CU$Shape_Length, na.rm = T)
  fw_CU_stats$meanQ_win_proj[i] <- Hmisc::wtd.mean(fw_acc_CU$mean_win_40, fw_acc_CU$Shape_Length, na.rm = T)
  
  fw_CU_stats$meanQ_8_diff[i]     <- fw_CU_stats$meanQ_8_proj[i] - fw_CU_stats$meanQ_8_hist[i]
  fw_CU_stats$meanQ_win_diff[i]   <- fw_CU_stats$meanQ_win_proj[i] - fw_CU_stats$meanQ_win_hist[i]
  fw_CU_stats$meanQ_8_pchange[i]  <- fw_CU_stats$meanQ_8_diff[i] / fw_CU_stats$meanQ_8_hist[i]
  fw_CU_stats$meanQ_win_pchange[i]<- fw_CU_stats$meanQ_win_diff[i] / fw_CU_stats$meanQ_win_hist[i]
  
  fw_CU_stats$MADprop_8_hist[i]   <- Hmisc::wtd.quantile(fw_acc_CU$MADprop_8_hist, fw_acc_CU$Shape_Length, probs = 0.5)
  fw_CU_stats$MADprop_win_hist[i] <- Hmisc::wtd.quantile(fw_acc_CU$MADprop_win_hist, fw_acc_CU$Shape_Length, probs = 0.5)
  fw_CU_stats$MADprop_8_proj[i]   <- Hmisc::wtd.quantile(fw_acc_CU$MADprop_8_proj, fw_acc_CU$Shape_Length, probs = 0.5)
  fw_CU_stats$MADprop_win_proj[i] <- Hmisc::wtd.quantile(fw_acc_CU$MADprop_win_proj, fw_acc_CU$Shape_Length, probs = 0.5)
  
  fw_CU_stats$MADprop_8_diff[i]   <- fw_CU_stats$MADprop_8_proj[i] - fw_CU_stats$MADprop_8_hist[i]
  fw_CU_stats$MADprop_win_diff[i] <- fw_CU_stats$MADprop_win_proj[i] - fw_CU_stats$MADprop_win_hist[i]
  
  ### PCIC monthly summary stats for CU boundary
  
  #mask cells that overlap with accessible streams within CU spawning boundary
  PCIC_Aug_CU_hist <- PCIC_month[fw_acc_CU] %>%   #change to CU_boundary_i for all grid cells
    filter(month(time) == 8, year(time) == 1985) 
  PCIC_Aug_CU_proj <- PCIC_month[fw_acc_CU] %>%
    filter(month(time) == 8, year(time) == 2055) 
  
  PCIC_May_CU_hist <- PCIC_month[fw_acc_CU] %>%
    filter(month(time) == 5, year(time) == 1985)
  PCIC_May_CU_proj <- PCIC_month[fw_acc_CU] %>%
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
  PCIC_CU_stats$augT_PCIC_rate[i]      <- PCIC_CU_stats$augT_PCIC_diff[i] / PCIC_tspan
  PCIC_CU_stats$augT_PCIC_z[i]         <- PCIC_CU_stats$augT_PCIC_diff[i] / PCIC_CU_stats$augT_PCIC_hist_sd[i]  
  
  
  #### PCIC daily CU stats for CU boundary
  PCIC_day_CU <-  st_crop(PCIC_day, fw_acc_CU) %>% 
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
  
  
  ### Upstream migration stats - for CUs, not FAZ
  if(do_FAZ == FALSE) {
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
    migr_CU_stats$Tmigr_diff[i]         <- migr_CU_stats$Tmigr_proj_mean[i] - migr_CU_stats$Tmigr_hist_mean[i]
    migr_CU_stats$Tthr_hist_mean[i]     <- sum(PCIC_migr_hist_CU$tw_day > deg_threshold, na.rm = T)
    migr_CU_stats$Tthr_proj_mean[i]     <- sum(PCIC_migr_proj_CU$tw_day > deg_threshold, na.rm = T)
    migr_CU_stats$Qmigr_hist_mean[i]    <- mean(PCIC_migr_hist_CU$flow_day, na.rm = T)
    migr_CU_stats$Qmigr_hist_sd[i]      <- sd(PCIC_migr_hist_CU$flow_day, na.rm = T)
    migr_CU_stats$Qmigr_proj_mean[i]    <- mean(PCIC_migr_proj_CU$flow_day, na.rm = T)
    migr_CU_stats$Qmigr_proj_sd[i]      <- sd(PCIC_migr_proj_CU$flow_day, na.rm = T)
    migr_CU_stats$Qmigr_propdiff[i]     <- (migr_CU_stats$Qmigr_proj_mean[i] - migr_CU_stats$Qmigr_hist_mean[i]) / migr_CU_stats$Qmigr_hist_mean[i] 
  }
  
  print(paste("CU", cuid_i, "stats done"))
}


#make hydrological table

monthly_Q <- pivot_longer(fw_CU_stats,
                          cols = starts_with("meanQ"),
                          names_to = "month",
                          values_to = "meanQ") %>%
  select(cuname:cuid, month, meanQ)
  #mutate(month = as.numeric(str_extract(month, "[0-9]+"))) %>%
  #arrange(month)

## round values for display
if(do_FAZ == FALSE) {
PCIC_CU_stats <- PCIC_CU_stats #%>%
  #mutate(across(where(is.numeric), round, 2))

fw_CU_stats <- fw_CU_stats #%>%
  #mutate(across(where(is.numeric), round, 2))

migr_CU_stats <- migr_CU_stats #%>%
  #mutate(across(where(is.numeric), round, 2))

all_CU_stats <- fw_CU_stats %>%
  left_join(select(PCIC_CU_stats, -c(cuname,FULL_CU_IN,Species_simple)), by = "cuid") %>%
  left_join(select(migr_CU_stats, -c(cuname,FULL_CU_IN,Species_simple)), by = "cuid")

CVIS_CU <- all_CU_stats %>%
  select(cuid, cuname, FULL_CU_IN, Species_simple, 
         #Tw8_0_00_0, 
         Tw8_9_45_3, Tw_rate, ThiPI_9_45_3,#augT_PCIC_hist_mean, augT_PCIC_proj_mean, augT_PCIC_rate,
         #meanQ_8_diff, meanQ_win_diff, meanQ_8_pchange, meanQ_win_pchange,   #absolute differences in flow
         MADprop_8_diff, MADprop_win_diff,  #differences in flow relative to MAD
         peakQday_diff, CT_anad_mean,
         #Tmigr_hist_mean, Qmigr_hist_mean,
         Tmigr_proj_mean, Qmigr_propdiff) %>%
  rename(projT_spn = ThiPI_9_45_3, rateT_spn = Tw_rate, augQ_spn = MADprop_8_diff,
         winQ_spn = MADprop_win_diff, peakQday_spn = peakQday_diff,
         anadCT_spn = CT_anad_mean, T_mig = Tmigr_proj_mean,
         Q_mig = Qmigr_propdiff)

# test correlation
cor_stats <- cor(CVIS_CU %>% select(-c(cuid, cuname, FULL_CU_IN, Species_simple)), use = "pairwise.complete.obs")

## save output
save(fw_CU_stats, PCIC_CU_stats, migr_CU_stats, all_CU_stats, CVIS_CU, cor_stats,
     file = here("output", paste0(today, "_fw_stats_output.Rdata")))

write_csv(all_CU_stats, here("output", paste0(today, "_fw_all_stats.csv")))
write_csv(as_tibble(cor_stats), here("output", paste0(today, "_fw_cor_stats.csv")))
write_csv(CVIS_CU, here("output", paste0(today, "_fw_CVIS_stats.csv")))

} else if(do_FAZ == TRUE) {
  PCIC_FAZ_stats <- PCIC_CU_stats %>%
    mutate(across(where(is.numeric), round, 2))
  
  fw_FAZ_stats <- fw_CU_stats %>%
    mutate(across(where(is.numeric), round, 2))
  
  migr_FAZ_stats <- migr_CU_stats %>%
    mutate(across(where(is.numeric), round, 2))
  
  all_FAZ_stats <- fw_FAZ_stats %>%
    left_join(select(PCIC_FAZ_stats, -c(cuname,FULL_CU_IN,Species_simple)), by = "cuid")
  
  CVIS_FAZ <- all_FAZ_stats %>%
    select(FAZ_Acrony, FAZ_Name, 
           #Tw8_0_00_0,
            Tw8_9_45_3, Tw_rate, augT_PCIC_hist_mean, augT_PCIC_proj_mean, augT_PCIC_rate,
           meanQ_8_diff, meanQ_win_diff, meanQ_8_pchange, meanQ_win_pchange,   #absolute differences in flow
           MADprop_8_diff, MADprop_win_diff,  #differences in flow relative to MAD
           peakQday_diff, CT_anad_mean)  %>%
    rename(projT_spn = Tw8_9_45_3, rateT_spn = Tw_rate, augQ_spn = MADprop_8_diff,
           winQ_spn = MADprop_win_diff, peakQday_spn = peakQday_diff,
           anadCT_spn = CT_anad_mean)

  # test correlation
  cor_FAZstats <- cor(CVIS_FAZ %>% select(-c(FAZ_Acrony, FAZ_Name)))
  
  ## save output
  save(fw_FAZ_stats, PCIC_FAZ_stats, migr_FAZ_stats, all_FAZ_stats, CVIS_FAZ, cor_FAZstats,
       file = here("output", paste0(today, "_fw_FAZstats_output.Rdata")))
  
  write_csv(all_FAZ_stats, here("output", paste0(today, "_fw_all_FAZstats.csv")))
  write_csv(as_tibble(cor_stats), here("output", paste0(today, "_fw_cor_FAZstats.csv")))
  write_csv(CVIS_FAZ, here("output", paste0(today, "_fw_CVIS_FAZstats.csv")))
  
  
}
# PCIC_ts_compare <- all_CU_stats %>%
#   select(cuid, cuname, starts_with("augT"), Tw8_0_00_0, Tw8_9_45_3, Tw8_0_00_0, Tw8_9_45_3) %>%
#   relocate(cuid:cuname) %>%
#   mutate(hist_diff = augT_PCIC_hist_mean - Tw8_0_00_0,
#          proj_diff = augT_PCIC_proj_mean - Tw8_9_45_3)


corrplot(cor_stats,  method = "number", tl.col = "black")


#write_csv(PCIC_ts_compare, here("output", paste0(today, "_fw_all_stats.csv")))

ggplot(fw_acc_indies) +
  geom_boxplot(aes(x=as.factor(STREAM_ORDER), y=MADprop_8_hist)) +
  ylim(0,1) 


TS_7DECM_stats <- all_CU_stats %>%
  select(cuid, cuname, starts_with("Risk"), starts_with("Tw"), starts_with("Thi"), starts_with("Tav"))

write.csv(TS_7DECM_stats, here("output", paste0(today, "_fw_TS_7DECM_stats.csv")))

          