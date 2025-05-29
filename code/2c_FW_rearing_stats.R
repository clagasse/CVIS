#-------------------------- 1. Overview and setup ----------------------
#
# 2c_FW_rearing_stats.R
#
# This code reads in a number of spatial datasets related to salmon distribution
# in the **Fraser region**.
#

library(here)
setwd(here())
source(file.path(here(), "code", "0_setup.R"))

library(ExPanDaR)  #for data exploration

#--------- 2. load spatial objects ---------------------

### BC FISH PASS stream accessibility and linear habitat model

#there are two main stream network objects
#bcfpc - fish accessibility and linear habitat model for all streams in the Fraser basin
#bcfpa - same for only accessible modelled and observed streams in the Fraser basin

# all streams in Fr basin
#load(file.path(paths$spatial, "BCFishpass", "BCFP_combined_Fr.Rds"))
# accessible streams only
load(file.path(paths$fw, "BCFP_combined_accessible_Fr.Rds"))

### Conservation Unit boundaries for Fraser CUs 

cu_boundary <- st_read(file.path(paths$spatial, "CU_boundaries", "fraser_cus.shp")) %>%
  st_make_valid() %>%
  st_transform(crs = 3005)  %>%  #crs 3005 is NAD83/BC Albers
  left_join(select(cu_Fr, cuid, FULL_CU_IN, spp), 
            join_by(CUID == cuid)) %>%
  filter(!is.na(FULL_CU_IN))


### Temperature, flow and cumulative threat models

#load stream network model outputs
# these are all on the same stream network as bcfpa - accessible bc fish pass stream segments
load(file.path(paths$fw, "fw_models_T_Q_CT.Rds"))
### Load ENM
load(file.path(paths$fw, "ENM_all_sp.Rds"))
# load statistical model projections of August flows for flow stations
load(file.path(paths$fw,  "Statistical_flow_projections.Rds"))

#load flow stations spatial objects
stations_stats <- read.csv(file.path(paths$climate, "Ruzzante_low_flows", "stations_performance.csv"))
#watershed hydrologic regimes
watershed_flow <- st_read(file.path(paths$climate, "Ruzzante_low_flows", "watersheds.gpkg")) %>%
  left_join(select(stations_stats, ID, regime), by = c("ID" = "ID")) %>%
  mutate(regime = as.factor(regime))

stations_flow <- st_read(file.path(paths$climate, "Ruzzante_low_flows", "stations.gpkg"))


### NUSEDS salmon spawner locations
##version from FIA. Usage column added by Michael Arbeider
nuseds_Fr <- read_csv(file.path(paths$salmon, "NuSEDS_CU_System_sites_202406.csv")) %>%
  st_as_sf(coords = c("X_LONGT", "Y_LAT"), crs = 4269) %>%
  filter(USAGE != "REMOVE")

#  field descriptions
# n = number of surveys that were not “UNKNOWN” or “NOT INSPECTED”, i.e. they were inspected but sometimes only PRESENSE was recorded and not an abundance.
# last.year = last year when the system was surveyed
# first.year = first year when the system was surveyed
# max.count = the largest count of spawners in NuSEDs
# ave.count = the mean of all non-NA counts in NuSEDs
# min.count = the minimum

#usage criteria for nuseds file
# cu.sites <- cu.sites %>% 
#   mutate(USAGE = case_when(
#     n < 5 & last.year < 2010 ~ "REMOVE",
#     n < 5 & last.year >= 2010 ~ "CAUTION",
#     n >= 5 & last.year < 1999 & SPECIES_LOOKUP != "Pink" ~ "CAUTION",
#     n >= 5 & max.count == 0 & last.year < 1999 & SPECIES_LOOKUP != "Pink" ~ "CAUTION",
#     n >= 5 & max.count != 0 & last.year < 1999 & SPECIES_LOOKUP == "Pink" ~ "KEEP",
#     n >= 5 & max.count == 0 & last.year >= 1999 ~ "CAUTION",
#     n >= 5 & max.count != 0 & last.year >= 1999 ~ "KEEP"
#   ))


#load CU stream selections

load(file.path(paths$fw, "2025-05-27_fw_streampicks.Rdata"))



#--------------------- 4. Functions for indicators -----------------------------------


#summary functions
stream_stats <- function(bcfp_cu)  {
  #calculate stream stats for spawning and rearing streams
  stream_stats <- tibble(
    total_length_acc = sum(bcfp_cu$length_metre, na.rm = T),
    total_length_rs = sum(bcfp_cu$length_metre[bcfp_cu$model_rs == TRUE], na.rm = T),
    total_length_rear = sum(bcfp_cu$length_metre[bcfp_cu$model_rearing == TRUE], na.rm = T),
    total_length_spawn = sum(bcfp_cu$length_metre[bcfp_cu$model_spawning == TRUE], na.rm = T),
    avg_order = mean(bcfp_cu$stream_order, na.rm = T),
    avg_order_rear = mean(bcfp_cu$stream_order[bcfp_cu$model_rearing == TRUE], na.rm = T),
    avg_order_spawn = mean(bcfp_cu$stream_order[bcfp_cu$model_spawning == TRUE], na.rm = T),
    n_streams = length(unique(bcfp_cu$linear_feature_id)),
    n_segments= length(unique(bcfp_cu$segmented_stream_id)),
    cu_area = st_area(cu_boundary_i) / 1e6
  )
  
  return(stream_stats)
}

#function to calculate cumulative threat statistics
sct_stats <- function(fwct_cu, 
                     cts = c("AIS", "AnadFrag", "FlowAlt", "HabDest",
                                      "LatFrag", "ResFrag", "RipDist", "Nutrient",
                                      "Pollution", "Sediment", "CT_anad"),
                     rs = TRUE) {
  
  if(rs == TRUE) fwct_cu <- fwct_cu[fwct_cu$model_rs == TRUE,]
  
  #select ct columns
  ct_grep <- paste(cts, collapse = "|")
  ct_cols <- grep(ct_grep, names(fwct_cu), value = TRUE)

  #calculate mean and other statistics from single column
  ct_stats <- map_dfc(ct_cols, function(col) {
    x <- fwct_cu[[col]]
    w <- fwct_cu$length_metre
    tibble(
      !!paste0(col, "_mean") := wmean(x, w, na.rm = TRUE),
      #!!paste0(col, "_q10") := wqt(x, w, 0.10, na.rm = TRUE),
      #!!paste0(col, "_q90") := wqt(x, w, 0.90, na.rm = TRUE)
    )
  }) %>% bind_cols()
  
  return(ct_stats)
}


#temperature summary function
sT_stats <- function(fwT_cu, 
                     RCP = c("45", "85"), 
                     period = c("0","3","4", "5"),
                     historical = "00",
                     GCMs = c(1:6),  #GCMs to include in summary
                     model = "Tw8",  #thermalscapes August temp
                     rs = TRUE
                     ) {
  
  if(rs == TRUE) fwT_cu <- fwT_cu[fwT_cu$model_rs == TRUE,]
  
  #select temperature model columns for summaries
  #temp_cols <- grep("^(Tw8_|Tav_)", names(fwT_cu), value = TRUE)
  temp_cols <- grep(paste0("^(", model,")"), names(fwT_cu), value = TRUE)
  RCP_picks <- c(historical, RCP)
  temp_cols <- temp_cols[grepl(paste(RCP_picks, collapse = "|"), temp_cols)]
  
  #subset time periods
  time_grep <- paste(paste0("_", period), collapse = "|")
  temp_cols <- temp_cols[grepl(paste0("(", time_grep, ")$"), temp_cols)]  #ends with
  
  #names of projected columns
  proj_cols  <- temp_cols[!grepl(paste0("_", historical, "_"), temp_cols)]
  #names of historical baseline
  hist_cols  <- temp_cols[grepl(paste0("_", historical, "_"), temp_cols)]

  #calculate mean and other statistics from single column
  T_stats <- map_dfc(temp_cols, function(col) {
    x <- fwT_cu[[col]]
    w <- fwT_cu$length_metre
    tibble(
      !!paste0(col, "_mean") := wmean(x, w, na.rm = TRUE),
      #!!paste0(col, "_sd") := wsd(x, w, na.rm = TRUE),
      #!!paste0(col, "_q10") := wqt(x, w, 0.10, na.rm = TRUE),
      #!!paste0(col, "_q90") := wqt(x, w, 0.90, na.rm = TRUE)
    )
  }) %>% bind_cols()
  
  
  mean_cols <- grep(paste0(paste0(model, "_"), GCMs, collapse = "|"), names(T_stats), value = TRUE)
  
  rcp_cols <- grepl(RCP, mean_cols)
  
  #calculate difference between historical and projected mean Ts
  delta_stats <- map2_dfc(proj_cols, hist_cols, function(proj, hist) {
    x <- fwT_cu[[proj]]
    h <- fwT_cu[[hist]]
    w <- fwT_cu$length_metre
    
    tibble(
      !!paste0(proj, "_delta") := wmean(x, w, na.rm = TRUE) - wmean(h, w, na.rm = TRUE)
    )
  }) %>% bind_cols()
  
  out <- bind_cols(T_stats, delta_stats)
  return(out)
}

#flow statistics function

sQ_stats <- function(fwQ_cu,
                     RCP = c("00", "45", "85"), 
                     period = c("40", "80"),
                     historical = "1",
                     rs = TRUE) {
  
  if(rs == TRUE) fwQ_cu <- fwQ_cu[fwQ_cu$model_rs == TRUE,]
  
  #columns with flow values
  flow_cols <- grep("mean", names(fwQ_cu), value = TRUE)
  
  #subset time periods
  time_picks <- c(period, historical)
  time_grep <- paste(paste0("_", time_picks), collapse = "|")
  flow_cols <- flow_cols[grepl(paste0("(", time_grep, ")$"), flow_cols)]
  
  #names of projected columns --  subset columns that end with 0
  proj_grep <- paste(paste0(period, "$"), collapse = "|")
  proj_cols  <- flow_cols[grepl(paste0("(", proj_grep, ")$"), flow_cols)]
  MAD_proj_col <- grep("17", proj_cols, value = TRUE)
  
  #names of historical baseline -- subset columns that end with
  hist_cols  <- flow_cols[grepl(paste0(historical, "$"), flow_cols)]
  MAD_hist_col <- grep("17", hist_cols, value = TRUE)
  
  hist_recycled <- rep(hist_cols, each = length(proj_cols)/length(hist_cols))
  
  
  #calculate mean and other statistics from single column
  Q_stats <- map_dfc(flow_cols, function(col) {
    x <- fwQ_cu[[col]]
    w <- fwQ_cu$length_metre
    tibble(
      !!paste0("QF", col) := wmean(x, w, na.rm = TRUE),
      #!!paste0(col, "_sd") := wsd(x, w, na.rm = TRUE),
      #!!paste0(col, "_q10") := wqt(x, w, 0.10, na.rm = TRUE),
      #!!paste0(col, "_q90") := wqt(x, w, 0.90, na.rm = TRUE)
    )
  }) %>% bind_cols()
  
  Qdelta_stats <- map2_dfc(proj_cols, hist_recycled, function(proj, hist) {
    x <- fwQ_cu[[proj]]
    h <- fwQ_cu[[hist]]
    w <- fwQ_cu$length_metre
    
    tibble(
      !!paste0("QF", proj, "_delta") := wmean(x, w, na.rm = TRUE) - wmean(h, w, na.rm = TRUE)
    )
  }) %>% bind_cols()
  
  #calculate changes in MAD
  MAD_stats <- map2_dfc(flow_cols, MAD_hist_col, function(col, hist) {
    x <- fwQ_cu[[col]]
    h <- fwQ_cu[[hist]]
    w <- fwQ_cu$length_metre
    
    tibble(
      !!paste0("QF", col, "_propMAD") := wmean(x, w, na.rm = TRUE) / wmean(h, w, na.rm = TRUE)
    )
  }) %>% bind_cols()
  
  MAD_hist_cols <- grep("flow_m3s", names(MAD_stats), value = TRUE)
  MAD_proj_cols <- grep(paste0(period, collapse = "|"), names(MAD_stats), value = TRUE)
  
  MAD_hist_recycled <- rep(MAD_hist_cols, 
                           each = length(MAD_proj_cols)/length(MAD_hist_cols))
  
  
  MADdelta_stats <- map2_dfc(MAD_proj_cols, MAD_hist_recycled, function(proj, hist) {
    x <- MAD_stats[[proj]]
    h <- MAD_stats[[hist]]
    
    tibble(
      !!paste0(proj, "delta") := x - h
    )
  }) %>% bind_cols()
  
  #calculate difference between historical and projected prop MAD
  #MAD_props <- 
  
  out <- bind_cols(Q_stats, Qdelta_stats, MAD_stats, MADdelta_stats)
  return(out)
}


#ENM statistics function

ENM_stats <- function(ENM_cu, 
                      RCP = c("00", "45", "85"), 
                      period = c("3", "5"),
                      historical = "0",
                      rs = TRUE) {
  
  #subset ENM cols based on ending with time period values
  time_picks <- c(period, historical)
  time_grep <- paste(paste0("_", time_picks), collapse = "|")
  ENM_cols <- grep(paste0("(", time_grep, ")$"), names(ENM_cu), value = TRUE)  #ends with
  
  #calculate mean and other statistics from single column
  ENM_stats <- map_dfc(ENM_cols, function(col) {
    x <- ENM_cu[[col]]
    w <- ENM_cu$Length_km
    tibble(
      !!paste0(substring(col, 4), "_mean") := wmean(x, w, na.rm = TRUE),
    )
  }) %>% bind_cols()
  
  ENM_cu_rs <- ENM_cu[ENM_cu$model_rs == TRUE,]
  
  ENM_stats_rs <- map_dfc(ENM_cols, function(col) {
    x <- ENM_cu_rs[[col]]
    w <- ENM_cu_rs$Length_km
    tibble(
      !!paste0(substring(col,4), "_mean_rs") := wmean(x, w, na.rm = TRUE)
    )
  }) %>% bind_cols()
  
  out <- bind_cols(ENM_stats, ENM_stats_rs)
  
  return(out)
}
  
  
# ----------------------- 5. Calculate stream network CU indicators-------------------

for(i in 1:n.CUs) {
  
  cu_i <- cu_run$FULL_CU_IN[i]
  sp_pick <- cu_run$spp[cu_run$FULL_CU_IN == cu_i] #species abbr
  sp_pick_bcfp <- spp_lookup$spp_abr_bcfp[spp_lookup$spp_abr == sp_pick]  #BCFP species abbr (different for Chinook)
  # Subset CU boundary
  cu_boundary_i <- cu_boundary[cu_boundary$FULL_CU_IN == cu_i,]
  
  #pick column of stream indices to subset for CU
  stream_cu_sub <- stream_cu_picks[,colnames(stream_cu_picks) == cu_i]
  reaches_ENM_sub <- reaches_ENM_all[,colnames(ENM_cu_picks) == cu_i]
  
  #subset nuseds observations
  nuseds_cu <- nuseds_Fr[nuseds_Fr$FULL_CU_IN == cu_i,]

  bcfpa_cu <- bcfpa[stream_cu_sub,] %>%
    select(segmented_stream_id:mad_m3s, 
           contains(sp_pick_bcfp)) %>%  #subset model for CU species
    rename(model_spawning = starts_with("model_spawning"),
           model_rearing  = starts_with("model_rearing")) %>%
    mutate(model_rs = if_any(starts_with("model"), ~ . == TRUE))  #get boolean for model spawning and rearing
  
  #bcfpa_cu_rs <- filter(bcfpa_cu, model_rs == TRUE)
  
  fwT_cu <- fwT[stream_cu_sub,] %>% 
    left_join(select(bcfpa_cu, segmented_stream_id, model_rs), 
              by = "segmented_stream_id") 
  
  fwT_cu_long <- fwT_cu %>%
    pivot_longer(cols = matches("^(Tw8|Tav|TlowPI|ThiPI)_"),, 
                 names_to = c(".value", "GCM", "RCP", "period"),
                 names_pattern = "^(Tw8|Tav|TlowPI|ThiPI)_(\\d)_(\\d{2})_(\\d)$")
  
  Ts_stats <- fwT_cu_long %>%
    group_by(GCM, RCP, period) %>%
    summarize(
      across(
        c(Tw8, Tav, TlowPI, ThiPI),
        list(mean = ~wmean(.x, length_metre, na.rm = TRUE),
             sd = ~wsd(.x, length_metre, na.rm = TRUE),
             min = ~min(.x, na.rm = TRUE),
             max = ~max(.x, na.rm = TRUE),
             range = ~max(.x, na.rm = TRUE) - min(.x, na.rm = TRUE)),
        .names = "{.col}_{.fn}"
      ),
      .groups = "drop"
    )
  
  Ts_proj_summary <- Ts_stats %>%
    filter(GCM %in% c(1:6)) %>%
    group_by(RCP, period) %>%
    summarize(
      across(contains("mean"), list( sd = ~sd(.x, na.rm = TRUE),
                                      min = ~min(.x, na.rm = TRUE),
                                      max = ~max(.x, na.rm = TRUE),
                                      range = ~max(.x, na.rm = TRUE) - min(.x, na.rm = TRUE),
                                      CV = ~sd(.x, na.rm = TRUE) / mean(.x, na.rm = TRUE)),
             .names = "{.col}_GCM{.fn}"),
      .groups = "drop"
    )
  
  Ts_stats <- filter(Ts_stats, GCM %in% c(0,9)) %>%
    left_join(Ts_proj_summary, 
              by = c("RCP", "period"))

  # Flow statistics
  fwQ_cu <- fwQ[stream_cu_sub,] %>%
    left_join(select(bcfpa_cu, segmented_stream_id, model_rs), 
              by = "segmented_stream_id")
  
  fwQ_cu_long <- fwQ_cu %>%
    rename_with(~ sub("_flow_m3s", "", .x), starts_with("mean_flow_m3s_")) %>%
    pivot_longer(cols = matches("mean"),
                 names_to = c(".value", "month","period"),
                 names_pattern = "^(mean)_(\\d+)_(\\d+)$") %>%
    mutate(month = as.integer(month))

  
  fwQ8_cu_long <- fwQ_cu_long %>%
    filter(month %in% c(8,17)) %>%
    pivot_wider(
      names_from = month,
      values_from = mean,
      names_prefix = "month_") %>%
    arrange(segmented_stream_id, period) %>%
    mutate(
      MAD_hist = if_else(period == "1", month_17, NA_real_),
      Q8_hist  = if_else(period == "1", month_8, NA_real_)) %>%
    fill(MAD_hist, Q8_hist, .direction = "down") %>%
    mutate(prop8 = month_8 / MAD_hist)
  
  fwQ8_cu_stats <- fwQ8_cu_long %>%
    group_by(period) %>%
    summarize(
      Q8_mean       = wmean(month_8, length_metre, na.rm = TRUE),
      Q8_sd         = wsd(month_8, length_metre, na.rm = TRUE),
      Q8_delta      = wmean(month_8 - Q8_hist, length_metre, na.rm = TRUE),
      Q8_deltaprop  = wmean((month_8 - Q8_hist) / Q8_hist, length_metre, na.rm = T),
      QMADhist_mean = wmean(MAD_hist,length_metre, na.rm = TRUE),
      QMADhist_sd   = wsd(MAD_hist, length_metre, na.rm = TRUE),
      propMAD8_mean    = wmean(prop8, length_metre, na.rm = TRUE),
      propMAD8_sd      = wsd(prop8, length_metre, na.rm = TRUE),
      propMAD8_CV      = propMAD8_sd / propMAD8_mean,
      propMAD8_min     = min(prop8, na.rm = TRUE),
      propMAD8_max     = max(prop8, na.rm = TRUE),
      propMAD8_range   = max(prop8, na.rm = TRUE) - min(prop8, na.rm = TRUE)) %>%
    ungroup()
  
  fwQ_stats <- fwQ_cu_long %>%
    group_by(month, period) %>%
    summarize(
      across(
        c(mean),
        list(mean = ~mean(.x, na.rm = TRUE),
             sd = ~sd(.x, na.rm = TRUE),
             min = ~min(.x, na.rm = TRUE),
             max = ~max(.x, na.rm = TRUE),
             range = ~max(.x, na.rm = TRUE) - min(.x, na.rm = TRUE),
             CV = ~sd(.x, na.rm = TRUE) / mean(.x, na.rm = TRUE)),
        .names = "{.col}_{.fn}"
      ),
      .groups = "drop"
    ) %>%
    arrange(period)
  
  fwct_cu<- fwct[stream_cu_sub,] %>%
    left_join(select(bcfpa_cu, segmented_stream_id, model_rs), 
              by = "segmented_stream_id")
  
  ENM_cu <- data.table(reaches_ENM_all[reaches_ENM_sub,]) %>%
    select(linear_feature_id, segmented_stream_id, Shape_Length, Length_km, 
           contains(sp_pick)) %>%
    left_join(select(bcfpa_cu, segmented_stream_id, model_rs),
              by = "segmented_stream_id")
  
  
  # run summary statistics functions
  
  #stream network summary
  ss_i <- stream_stats(bcfpa_cu)
  
  #nuseds summary
  nu_i <- data.table(nuseds_cu) %>%
    summarise(
      nuseds_sites = n(),
      nuseds_indicator = sum(IS_INDICATOR == "Y"),
      nuseds_obs = sum(n, na.rm = TRUE))
  
  #cumulative threat stats
  ct_i <- sct_stats(fwct_cu, rs = TRUE)
  
  #temperature statistics
  Ts_i <- sT_stats(fwT_cu, 
                    RCP = c("00", "45" ), 
                    period = c("0", "1", "3", "5"),
                    model = "Tw8",  #or Tav
                    GCMs = c(1:6),
                    rs = TRUE
                    )
  
  #flow statistics
  Qs_i <- sQ_stats(fwQ_cu, 
                    RCP = c("00", "45" ), 
                    period = c("40", "80"),
                    rs = TRUE
  )
  
  #ENM stats
  ENM_i <- ENM_stats(ENM_cu, 
                    RCP = c("00", "45" ), 
                    period = c("3", "5"),
                    historical = "0"
  )
  

  #combine all into one table
  all_i <- bind_cols(FULL_CU_IN = cu_i,
                        ss_i, 
                        nu_i,
                        ENM_i,
                        ct_i,
                        Ts_i, 
                        Qs_i,
                         .name_repair = "unique") 
  
  if(i == 1) {
    fwR_all <- all_i
    ss_all <- ss_i
    nu_all <- nu_i
    ct_all <- ct_i
    Ts_all <- Ts_i
    Qs_all <- Qs_i
    ENM_all <- ENM_i
      } else {
    fwR_all <- bind_rows(fwR_all, all_i)
    ss_all <- bind_rows(ss_all, ss_i)
    nu_all <- bind_rows(nu_all, nu_i)
    ct_all <- bind_rows(ct_all, ct_i)
    Ts_all <- bind_rows(Ts_all, Ts_i)
    Qs_all <- bind_rows(Qs_all, Qs_i)
    ENM_all <- bind_rows(ENM_all, ENM_i)
  }
  if(i == n.CUs) {
    print(paste(i, "CU fwR stats done"))
    rm(all_i)
    
    fwR_all <- fwR_all %>%
      left_join(select(cu_run, FULL_CU_IN, CU_NAME, CU_Species), 
                by = "FULL_CU_IN") %>%
      relocate(CU_NAME, CU_Species, .after = FULL_CU_IN)
  }
  
}


# ------------------6. Low flow statistical model at stations --------

# calculate average flows for periods
wp_pmean <- wp_mean %>%
  filter(!is.na(period)) %>%
  group_by(ID, experiment_id, period) %>%
  summarise(mean = mean(mean)) %>%
  ungroup() %>%
  mutate(period = as.factor(period)) %>%
  pivot_wider(names_from = c(experiment_id, period), values_from = mean) %>%
  mutate(Qdelta_370_3 = ssp370_3 - historical_0,
         Qdelta_370_5 = ssp370_5 - historical_0,
         Qdelta_585_3 = ssp585_3 - historical_0,
         Qdelta_585_5 = ssp585_5 - historical_0,
         Qprop_370_3 = (Qdelta_370_3 / historical_0),
         Qprop_370_5 = (Qdelta_370_5 / historical_0),
         Qprop_585_3 = (Qdelta_585_3 / historical_0),
         Qprop_585_5 = (Qdelta_585_5 / historical_0))

## join projections back to flow stations sf and CU boundaries
stations_flow <- stations_flow %>%
  left_join(wp_pmean, by = c("ID" = "ID")) %>%
  st_transform(3005)

watershed_flow <- watershed_flow %>%
  left_join(wp_pmean, by = c("ID" = "ID")) %>%
  st_transform(3005)

### Ruzzante model of low flows in August 

cu_cont <- st_contains(cu_boundary, stations_flow)
#take average of stations in each CU boundary
cu_wp_pmean <- calculate_subset_means_all(wp_pmean, cu_cont) %>%
  mutate(n_stations = unlist(lapply(cu_cont, FUN=length)),
         CU_ID = cu_boundary$FULL_CU_IN) %>%
  relocate(CU_ID, n_stations) 



#--------------------- 7. Subset statistics ----------------------

fwR_CVIS <- fwR_all %>%
  select(FULL_CU_IN:nuseds_obs, c(CT_anad_mean, 
                                  Fav_f.9_45_3_mean, Fav_change_9_45_5_mean, Fav_change_9_45_5_mean_rs,
                                  Tw8_0_00_0_mean, Tw8_9_45_3_mean, Tw8_9_45_3_delta,
                                  QFmean_flow_m3s_8_1, QFmean_8_40_delta, QFmean_8_40_propMADdelta))


#ExPanD(fwR_CVIS)

  
