#-------------------------- 1. Overview and setup ----------------------
#
# 2c_FW_rearing_stats.R
#
# This code loads stream-level climate and stressor models processed in 2a_FW_data_process.R
# and calculates summary statistics of climate indicators relevant to spawning and rearing.
# Statistics are calculated for each CU based on accessible streams with intrinsic habitat
# potential in the CU boundary, based on BC Fishpass models.
# An object with subsetted streams within each CU from 2b_FW_boundary_subset.R is required.
# Models currently included within stream stats include
# Tw8 - Thermalscape August stream temperature
# fwQ - stream network flow derived from PCIC grid model
# ct - cumulative threats model
# st8 - statistical model of August flow at hydrological stations (Ruzzante in prep)
# ENM - ecological niche model of habitat favourability

library(here)
setwd(here())
source(file.path(here(), "code", "0_setup.R"))

# library(ExPanDaR)  #for data exploration

historical <- "0"   # historical climatology period for temperature models
# 0 = 1981-2000,  1 = 2001-2020
# for flow models, 0 = 1981-2010
T_model <- "Tw8"    # temperature model Tw8 = thermalscapes August temp

qlowgcm <- 0.1    # lower quantile for statistics on GCM variation
qhighgcm <- 0.9   # upper quantile for statistics
qlowsp  <- 0.1    # lower quantile for spatial variation within CU boundary
qhighsp <- 0.9    # upper quantile for spatial variation


#--------- 2. load spatial objects ---------------------

### BC FISH PASS stream accessibility and linear habitat model

# there are two main stream network objects
# bcfpc - fish accessibility and linear habitat model for all streams in the Fraser basin
# bcfpa - same for only accessible modelled and observed streams in the Fraser basin

# all streams in Fr basin
# load(file.path(paths$spatial, "BCFishpass", "BCFP_combined_Fr.Rds"))
# accessible streams only
load(file.path(paths$fw, "BCFP_combined_accessible_Fr.Rds"))


### Temperature, flow, ENM, and cumulative threat models

# load stream network model outputs - all on the same stream network as bcfpa
# temperature and cumulative threat models
load(file.path(paths$fw, "fw_models_T_CT.Rds"))
# August historic and projected flows
load(file.path(paths$fw, "stream_flow_August_Fr_accessible.Rds"))
# historic flows - all months and annual (1981-2010)
load(file.path(paths$fw, "stream_flow_historic_Fr_accessible.Rds"))
# NovDecJan historic and projected flows
load(file.path(paths$fw, "stream_flow_NovDecJan_Fr_accessible.Rds"))


### Load ENM - these are lower resolution stream segments than bcfpa
load(file.path(paths$fw, "ENM_all_sp.Rds"))

# load statistical model projections of August flows for flow stations
load(file.path(paths$fw,  "Statistical_flow_projections.Rds"))


# load CU stream selections - for subsetting when calculating statistics
load(file.path(paths$fw, "2025-05-27_fw_streampicks.Rdata"))

# load flow stations spatial objects
stations_stats <- read.csv(file.path(paths$climate, "Ruzzante_low_flows", "stations_performance.csv"))
# watershed hydrologic regimes
watershed_flow <- st_read(file.path(paths$climate, "Ruzzante_low_flows", "watersheds.gpkg")) %>%
  left_join(select(stations_stats, ID, regime), by = c("ID" = "ID")) %>%
  mutate(regime = as.factor(regime)) %>%
  st_transform(3005)

stations_flow <- st_read(file.path(paths$climate, "Ruzzante_low_flows", "stations.gpkg")) %>%
  st_transform(3005)




#--------------------- 3. Functions for indicators -----------------------------------

# summary functions
stream_BCFP_stats <- function(bcfpa_cu) {
  # get coordinates and transform to lat/long
  coords <- bcfpa_cu %>%
    st_transform(4269) %>%
    st_coordinates()

  # calculate stream stats for spawning and rearing streams
  stream_stats <- tibble(
    total_length_acc = sum(bcfpa_cu$length_metre, na.rm = T),
    total_length_rs = sum(bcfpa_cu$length_metre[bcfpa_cu$model_rs == TRUE], na.rm = T),
    total_length_rear = sum(bcfpa_cu$length_metre[bcfpa_cu$model_rearing == TRUE], na.rm = T),
    total_length_spawn = sum(bcfpa_cu$length_metre[bcfpa_cu$model_spawning == TRUE], na.rm = T),
    proportion_rear  = total_length_rear / total_length_acc,
    proportion_spawn = total_length_spawn / total_length_acc,
    proportion_rs    = total_length_rs   / total_length_acc,
    avg_order = mean(bcfpa_cu$stream_order, na.rm = T),
    avg_order_rear = mean(bcfpa_cu$stream_order[bcfpa_cu$model_rearing == TRUE], na.rm = T),
    avg_order_spawn = mean(bcfpa_cu$stream_order[bcfpa_cu$model_spawning == TRUE], na.rm = T),
    n_streams = length(unique(bcfpa_cu$linear_feature_id)),
    n_segments = length(unique(bcfpa_cu$segmented_stream_id)),
    cu_area = st_area(cu_boundary_i) / 1e6,
    avg_elevation = mean(coords[, "Z"]),
    avg_lat    = mean(coords[, "Y"]),
    avg_lon    = mean(coords[, "X"])
  )

  return(stream_stats)
}


#------------------------ 3.1 stream temp stats function----

# calculate number of decades from historical to projection period. used for calculating rates of T change
decade_calc <- function(historical_pick = "0", period_pick = "3") {

  year_hist <- case_when(historical_pick == "0" ~ 1990,
    historical_pick == "1" ~ 2010)

  year_proj <- case_when(period_pick == "0" ~ 1990,
    period_pick == "1" ~ 2010,
    period_pick == "2" ~ 2030,
    period_pick == "3" ~ 2050,
    period_pick == "4" ~ 2070,
    period_pick == "5" ~ 2090)

  decades <- (year_proj - year_hist) / 10
}


stream_temp_stats <- function(fwT_cu,
                              RCP = c("45", "85"),
                              periods = c("0", "3", "4", "5"),
                              historical = "0",
                              GCMs = c(1:6),  # GCMs to include in summary
                              models = c("Tw8"),  # thermalscapes August temp
                              model_rs = TRUE
) {


  if (model_rs == TRUE) fwT_cu <- fwT_cu[fwT_cu$model_rs == TRUE, ]
  models_grep <- paste(models, collapse = "|")

  hist_col <- paste(models, "0", "00", historical, sep = "_")  # historical Tw8 column

  # pivot longer across selected model (e.g. Tw8)
  fwT_cu_long <- fwT_cu %>%
    mutate(histT = !!sym(hist_col)) %>%  # add historical Tw8
    pivot_longer(cols = matches(paste0("^(", models_grep, ")_")), # "^(Tw8|Tav|TlowPI|ThiPI)_"
      names_to = c(".value", "GCM", "RCP", "period"),
      names_pattern = paste0("^(", models_grep, ")_(\\d)_(\\d{2})_(\\d)$")) %>%
    select(matches(paste0("(", models_grep, ")")),
      segmented_stream_id, length_metre, model_rs, model_access_salmon,
      model_habitat_salmon, GCM, RCP, period, histT)   %>%
    filter(RCP %in% c(historical, RCP),
      period %in% periods) %>%
    mutate(across(contains(models_grep), ~ .x - histT, .names = "delta_{.col}"),
      decade_interval = decade_calc(historical, period_pick = period))

  # calculate the mean, sd, and quantiles for temperature of streams within CU boundary
  Ts_stats <- fwT_cu_long %>%
    filter(!is.na(histT)) %>%
    group_by(GCM, RCP, period) %>%
    summarize(
      n_streams = n(),
      total_length = sum(length_metre, na.rm = TRUE),
      across(
        .cols = c(all_of(models)),
        .fns = list(
          proj_mean = ~ wmean(.x, length_metre, na.rm = TRUE),
          # proj_sdsp = ~wsd(.x, length_metre, na.rm = TRUE),
          proj_qlowsp = ~ wqt(.x, length_metre, prob = qlowsp, na.rm = TRUE),
          proj_qhighsp = ~ wqt(.x, length_metre, prob = qhighsp, na.rm = TRUE),
          rate_mean = ~ wmean((.x - histT) / decade_interval, length_metre, na.rm = TRUE),
          # rate_wsdsp   = ~wsd((.x - histT)/decade_interval, length_metre, na.rm = TRUE),
          rate_qlowsp = ~ wqt((.x - histT) / decade_interval, length_metre, prob = 0.1, na.rm = TRUE),
          rate_qhighsp = ~ wqt((.x - histT) / decade_interval, length_metre, prob = 0.9, na.rm = TRUE)
        ),
        .names = "{.col}{.fn}"
      ),
      .groups = "drop"
    )

  # take the min, max, range across GCM means for the CU
  Ts_proj_summary <- Ts_stats %>%
    filter(GCM %in% GCMs) %>%
    group_by(RCP, period) %>%
    summarize(
      across(
        contains("mean"),
        list( # sdgcm = ~sd(.x, na.rm = TRUE),
          qlowgcm = ~ unlist(quantile(.x, probs = qlowgcm, na.rm = TRUE)),
          qhighgcm = ~ unlist(quantile(.x, probs = qhighgcm, na.rm = TRUE))),
        .names = "{.col}_{.fn}"
      ),
      # CVgcm = ~sd(.x, na.rm = TRUE) / mean(.x, na.rm = TRUE))),
      .groups = "drop"
    ) %>%
    rename_with(~ str_remove(.x, "_mean"), contains("_mean"))  # Remove '_mean' from column names

  Ts_stats <- filter(Ts_stats, GCM %in% c(0, 9)) %>%
    left_join(Ts_proj_summary,
      by = c("RCP", "period"))

  return(Ts_stats)
}

#----------------3.2 Stream flow statistics function--------


stream_flow_stats <- function(fwQ_cu,
                              model_rs = TRUE,
                              periods = c("0", "3", "4", "5"),
                              RCP = c("45", "85"),
                              months = c("8"),
                              GCMs = c("access1", "canesm2", "ccsm4", "cnrm", "hadgem2", "mpi")) {

  if (model_rs == TRUE) {
    fwQ_cu <- fwQ_cu[fwQ_cu$model_rs == TRUE, ]
  }

  GCM_grep <- paste(paste0(GCMs, collapse = "|"), "mean", sep = "|")

  # define month column names that will be pivoted
  Q_col <- paste0("month_", months)

  fwQ_cu_long <- fwQ_cu %>%
    select(segmented_stream_id, linear_feature_id, length_metre, contains("flow")) %>%
    pivot_longer(cols = matches("^flow"),
      names_to = c(".value", "RCP", "GCM", "period", "month"),
      names_pattern = paste0("^(flow)_(rcp\\d{2}|historical)_(", GCM_grep, ")_(\\d+)_(\\d+)$")) %>%
    mutate(RCP = substr(RCP, start = 4, stop = 5)) # remove rcp from column character

  fwQ_cu_wide <- fwQ_cu_long %>%
    pivot_wider(
      names_from = month,
      values_from = flow,
      names_prefix = "month_") %>%
    arrange(segmented_stream_id, period) %>%
    rename(month_Q = !!sym(Q_col)) %>%      # month for statistic may vary so call name dynamically
    mutate(
      MAD_hist = if_else(period == "0", month_17, NA_real_),
      Q_hist  = if_else(period == "0", month_Q, NA_real_)) %>%
    fill(MAD_hist, Q_hist, .direction = "down") %>%
    mutate(PMAD = month_Q / MAD_hist,   # August flow as proportion of historical MAD
      # PMAD8_delta = (month_8 / MAD_hist) - (Q8_hist / MAD_hist),  #change in proportional flow to MAD
      Qdelta = month_Q - Q_hist,
      Qpdelta = (month_Q - Q_hist) / Q_hist)

  fwQ_cu_stats <- fwQ_cu_wide %>%
    group_by(GCM, RCP, period) %>%
    summarize(
      n_streams = n(),
      total_length = sum(length_metre, na.rm = TRUE),
      Qproj_mean       = wmean(month_Q, length_metre, na.rm = TRUE),
      # Qproj_wsdsp         = wsd(month_Q, length_metre, na.rm = TRUE),
      QMADhist_mean     = wmean(MAD_hist, length_metre, na.rm = TRUE),
      # QMADhist_wsdsp    = wsd(MAD_hist, length_metre, na.rm = TRUE),
      Qpdelta_mean     = wmean(Qpdelta, length_metre, na.rm = T),
      # Qpdelta_wsdsp    = wsd(Qpdelta, length_metre, na.rm = TRUE),
      Qpdelta_qlowsp    = wqt(Qpdelta, length_metre, prob = qlowsp, na.rm = TRUE),
      Qpdelta_qhighsp    = wqt(Qpdelta, length_metre, prob = qhighsp, na.rm = TRUE),
      # propMAD8_mean     = wmean(PMAD8, length_metre, na.rm = TRUE),
      # propMAD8_sd      = wsd(PMAD8, length_metre, na.rm = TRUE),
      # propMAD8_CV      = propMAD8_sd / propMAD8_mean,
      # propMAD8_min     = min(PMAD8, na.rm = TRUE),
      # propMAD8_max     = max(PMAD8, na.rm = TRUE),
      # propMAD8_range   = max(PMAD8, na.rm = TRUE) - min(PMAD8, na.rm = TRUE),
      .groups = "drop")

  # take the min, max, range across GCM means for the CU
  fwQ_proj_summary <- fwQ_cu_stats %>%
    filter(GCM %in% GCMs) %>%
    group_by(RCP, period) %>%
    summarize(
      across(
        contains("mean"),
        list( # sdgcm = ~sd(.x, na.rm = TRUE),
          qlowgcm = ~ unlist(quantile(.x, probs = qlowgcm, na.rm = TRUE)),
          qhighgcm = ~ unlist(quantile(.x, probs = qhighgcm, na.rm = TRUE))),
        # CVgcm = ~sd(.x, na.rm = TRUE) / mean(.x, na.rm = TRUE)),
        .names = "{.col}_{.fn}"
      ),
      .groups = "drop"
    ) %>%
    select(-contains("QMAD")) %>%
    rename_with(~ str_remove(.x, "_mean"), contains("_mean"))

  fwQ_cu_stats <- filter(fwQ_cu_stats, GCM == "mean") %>%
    left_join(fwQ_proj_summary,
      by = c("RCP", "period"))


}



cu_highflow_month <- function(fwQ_cu,
                              model_rs = TRUE) {

  if (model_rs == TRUE) {
    fwQ_cu <- fwQ_cu[fwQ_cu$model_rs == TRUE, ]
  }


  # Step 1: Identify relevant columns - months 1 to 12 of historic flow
  hflow_cols <- names(fwQ_cu) %>%
    str_subset("mean_flow_m3s_") %>%
    keep(~ as.numeric(str_extract(., "(?<=mean_flow_m3s_)\\d+")) <= 12)

  pflow_cols <- names(fwQ_cu) %>%
    str_subset("_40") %>%
    keep(~ as.numeric(str_extract(., "(?<=mean_)\\d+")) <= 12)

  # Step 2: Add column with name of max value
  fwQ_cu <- fwQ_cu %>%
    mutate(
      max_hflow_col = hflow_cols[
        max.col(select(., all_of(hflow_cols)), ties.method = "first")
      ],
      max_pflow_col = pflow_cols[
        max.col(select(., all_of(pflow_cols)), ties.method = "first")
      ],
      max_hflow = do.call(pmax, c(select(., all_of(hflow_cols)), na.rm = TRUE)),
      max_pflow = do.call(pmax, c(select(., all_of(pflow_cols)), na.rm = TRUE))
    ) %>%
    mutate(max_hflow_month = as.numeric(str_extract(max_hflow_col, "(?<=mean_flow_m3s_)\\d+")),
      max_pflow_month = as.numeric(str_extract(max_pflow_col, "(?<=mean_)\\d+"))) %>%
    select(-c(max_hflow_col, max_pflow_col))

  fwQhigh_cu_stats <- fwQ_cu %>%
    summarize(
      n_streams = n(),
      QmonthH_mean    = wmean(max_hflow_month, length_metre, na.rm = TRUE),
      # QmonthH_wsdsp      = wsd(max_hflow_month, length_metre, na.rm = TRUE),
      QmonthH_qlowsp    = wqt(max_hflow_month, length_metre, prob = qlowsp, na.rm = TRUE),
      QmonthH_qhighsp      = wqt(max_hflow_month, length_metre, prob = qhighsp, na.rm = TRUE),
      QmonthP_mean    = wmean(max_pflow_month, length_metre, na.rm = TRUE),
      # QmonthP_wsdsp      = wsd(max_pflow_month, length_metre, na.rm = TRUE),
      QmonthP_qlowsp    = wqt(max_pflow_month, length_metre, prob = qlowsp, na.rm = TRUE),
      QmonthP_qhighsp      = wqt(max_pflow_month, length_metre, prob = qhighsp, na.rm = TRUE),
      QhighH_mean    = wmean(max_hflow, length_metre, na.rm = TRUE),
      # QhighH_wsdsp      = wsd(max_hflow, length_metre, na.rm = TRUE),
      QhighH_qlowsp    = wqt(max_hflow, length_metre, prob = qlowsp, na.rm = TRUE),
      QhighH_qhighsp      = wqt(max_hflow, length_metre, prob = qhighsp, na.rm = TRUE),
      QhighP_mean    = wmean(max_pflow, length_metre, na.rm = TRUE),
      # QhighP_wsdsp      = wsd(max_pflow, length_metre, na.rm = TRUE),
      QhighP_qlowsp    = wqt(max_pflow, length_metre, prob = qlowsp, na.rm = TRUE),
      QhighP_qhighsp      = wqt(max_pflow, length_metre, prob = qhighsp, na.rm = TRUE))


}


#----------------------3.3 Cumulative threat stats function -------------
stream_ct_stats <- function(fwct_cu,
                            cts = c("AIS", "AnadFrag", "FlowAlt", "HabDest",
                              "LatFrag", "ResFrag", "RipDist", "Nutrient",
                              "Pollution", "Sediment", "CT_anad"),
                            model_rs = TRUE) {

  if (model_rs == TRUE) fwct_cu <- fwct_cu[fwct_cu$model_rs == TRUE, ]

  # select ct columns
  ct_grep <- paste(cts, collapse = "|")
  ct_cols <- grep(ct_grep, names(fwct_cu), value = TRUE)

  fwct_cu_long <- pivot_longer(fwct_cu,
    cols = ct_cols,
    names_to = c("CT")) %>%
    filter(!is.na(value))

  ct_stats <- fwct_cu_long %>%
    filter(!is.na(value)) %>%
    group_by(CT) %>%
    summarise(
      ct_n = n(),
      ct_total_length = sum(length_metre, na.rm = TRUE),
      ct_mean = wmean(value, length_metre, na.rm = TRUE),
      # ct_wsdsp = wsd(value, length_metre, na.rm = TRUE),
      ct_qlowsp = wqt(value, length_metre, prob = qlowsp, na.rm = TRUE),
      ct_qhighsp = wqt(value, length_metre, prob = qhighsp, na.rm = TRUE),
      .groups = "drop"
    )

  return(ct_stats)
}

#------------------------- 4.4 ENM statistics function-----
ENM_stats <- function(ENM_cu,
                      RCP = c("00", "45", "85"),
                      periods = c("3", "4", "5"),
                      historical = "0",
                      model_rs = FALSE) {
  # subset ENM cols based on ending with time period values
  time_picks <- c(periods, historical)
  time_grep <- paste(paste0("_", time_picks), collapse = "|")
  ENM_cols <- grep(paste0("(", time_grep, ")$"), names(ENM_cu), value = TRUE)  # ends with

  if (model_rs == FALSE) ENM_cu <- ENM_cu[ENM_cu$model_rs == TRUE, ]

  ENM_cu_long <- ENM_cu %>%
    select(contains(ENM_cols), Length_km, segmented_stream_id) %>%
    filter(!is.na(.data[[ENM_cols[1]]])) %>%   # remove rows with missing ENM values
    pivot_longer(cols = all_of(ENM_cols),
      names_to = c(".value", "RCP", "period"),
      names_pattern = "^(.*)_(\\d{2})_(\\d)$") %>%
    arrange(segmented_stream_id, RCP, period) %>%
    mutate(Fav_hist = Fav_f.0) %>%  # historical fav
    fill(Fav_hist, .direction = "down") %>%
    mutate(Fav = coalesce(Fav_f.9, Fav_hist),
      Fav_change = Fav - Fav_hist, ) %>% # calculate change from historical
    select(-c(Fav_f.0, Fav_hist, Fav_f.9, Fav_change_9))


  # calculate mean and other statistics from single column
  ENM_stats <- ENM_cu_long %>%
    group_by(RCP, period) %>%
    summarise(
      n_streams = n(),
      total_length = sum(Length_km * 1000, na.rm = TRUE),
      across(starts_with("Fav"),
        list(
          mean = ~ wmean(.x, Length_km, na.rm = TRUE),
          # wsdsp = ~wsd(.x, Length_km, na.rm = TRUE),
          qlowsp = ~ wqt(.x, Length_km, prob = qlowsp, na.rm = TRUE),
          qhighsp = ~ wqt(.x, Length_km, prob = qhighsp, na.rm = TRUE)),
        .names = "{.col}_{.fn}"),

      .groups = "drop"
    )

  return(ENM_stats)
}

#---------------- 3.5 station low flow stats function -----

station_lowflow_stats <- function(wp_cu,
                                  historical = 0) {

  wp_cu <- wp_cu %>%
    mutate(mean_hist = ifelse(period == historical, mean, NA)) %>%
    fill(mean_hist, .direction = "down") %>%
    mutate(period = as.character(period))

  wp_i <- wp_cu %>%
    group_by(experiment_id, period) %>%
    summarise(n_stations = n(),
      st8proj_mean = mean(mean),
      # st8proj_wsdsp   = sd(mean),
      # st8proj_wsdgcm    = mean(sd),
      st8proj_qlowgcm = mean(qlow),
      st8proj_qhighgcm = mean(qhigh),
      st8pdelta_mean     = mean((mean - mean_hist) / mean_hist),
      st8pdelta_qlowsp   = unname(quantile((mean - mean_hist) / mean_hist, probs = qlowsp)),
      st8pdelta_qhighsp   = unname(quantile((mean - mean_hist) / mean_hist, probs = qhighsp)),
      # st8pdelta_sdgcm   = mean((sd - mean_hist) / mean_hist),
      st8pdelta_qlowgcm = mean((qlow - mean_hist) / mean_hist),
      st8pdelta_qhighgcm = mean((qhigh - mean_hist) / mean_hist),
      .groups = "drop")

}


# get proportion of hydrologic regime type for watersheds within the CU boundary
regime_stats <- function(watershed_flow, cu_boundary_i) {
  # Calculate intersection
  intersection_snow <- st_intersection(cu_boundary_i, filter(watershed_flow, regime == "Snowfall"))
  intersection_rain <- st_intersection(cu_boundary_i, filter(watershed_flow, regime == "Rainfall"))
  intersection_glac <- st_intersection(cu_boundary_i, filter(watershed_flow, regime == "Glacial"))
  intersection_hybr <- st_intersection(cu_boundary_i, filter(watershed_flow, regime == "Hybrid"))

  # Calculate areas
  area_poly1 <- st_area(cu_boundary_i)

  area_snow <- sum(st_area(intersection_snow)) / area_poly1
  area_rain <- sum(st_area(intersection_rain)) / area_poly1
  area_glac <- sum(st_area(intersection_glac)) / area_poly1
  area_hybr <- sum(st_area(intersection_hybr)) / area_poly1

  area_covr <- sum(area_snow, area_rain, area_glac, area_hybr)

  regime <- tribble(
    ~prop_snow, ~prop_rain, ~prop_hybrid, ~prop_glacial, ~prop_coverage,
    area_snow, area_rain, area_hybr, area_glac, area_covr)

}

# ----------------------- 4. Calculate stream network CU indicators-------------------

fwR_all <- list()  # initialize list for storing all results

for (i in 1:n.CUs) {
  #------- subset CU data -----
  cu_i <- cu_run$FULL_CU_IN[i]
  sp_pick <- cu_run$spp[cu_run$FULL_CU_IN == cu_i] # species abbr
  sp_pick_bcfp <- spp_lookup$spp_abr_bcfp[spp_lookup$spp_abr == sp_pick]  # BCFP species abbr (different for Chinook)
  # Subset CU boundary
  cu_boundary_i <- cu_boundary[cu_boundary$FULL_CU_IN == cu_i, ]

  # pick column of stream indices to subset for CU
  stream_cu_sub <- stream_cu_picks[, colnames(stream_cu_picks) == cu_i]
  reaches_ENM_sub <- ENM_cu_picks[, colnames(ENM_cu_picks) == cu_i]

  # subset nuseds observations
  nuseds_cu <- nuseds_Fr[nuseds_Fr$FULL_CU_IN == cu_i, ]

  bcfpa_cu <- bcfpa[stream_cu_sub, ] %>%
    select(segmented_stream_id:mad_m3s,
      contains(sp_pick_bcfp)) %>%  # subset model for CU species
    rename(model_spawning = starts_with("model_spawning"),
      model_rearing  = starts_with("model_rearing")) %>%
    mutate(model_rs = if_any(starts_with("model"), ~ . == TRUE))  # get boolean for model spawning and rearing

  # bcfpa_cu_rs <- filter(bcfpa_cu, model_rs == TRUE)

  # get cu FW timing info
  ## ocean entry age missing from Wilson study for some CUs, so use alternative values from Steph
  fw_timing_i <- cu_run[cu_run$FULL_CU_IN == cu_i, ] %>%
    select(Ocean_Entry_Age, Peak_Spawn_Julian, Peak_Ocean_Entry_Julian, Peak_Spawn_To_Ocean_Entry_Days)

  #-------create subsetted data tables for each model
  fwT_cu <- fwT[stream_cu_sub, ] %>%
    left_join(select(bcfpa_cu, segmented_stream_id, model_rs),
      by = "segmented_stream_id")

  fwQ8_cu <- fwQ8[stream_cu_sub, ] %>%
    left_join(select(bcfpa_cu, segmented_stream_id, model_rs),
      by = "segmented_stream_id")

  fwQNDJ_cu <- fwQNDJ[stream_cu_sub, ] %>%
    left_join(select(bcfpa_cu, segmented_stream_id, model_rs),
      by = "segmented_stream_id")

  fwct_cu <- fwct[stream_cu_sub, ] %>%
    left_join(select(bcfpa_cu, segmented_stream_id, model_rs),
      by = "segmented_stream_id")

  ENM_cu <- data.table(reaches_ENM_all[reaches_ENM_sub, ])  %>%
    select(linear_feature_id, segmented_stream_id, Shape_Length, Length_km,
      contains(sp_pick)) %>%
    left_join(select(bcfpa_cu, segmented_stream_id, model_rs),
      by = "segmented_stream_id") %>%
    rename_with(~ ifelse(grepl("Fav", .x), substr(.x, 4, nchar(.x)), .x))  # remove species prefix

  # get flow stations within each CU boundary
  cu_cont <- st_contains(cu_boundary_i, stations_flow, sparse = T)
  cu_stations <- stations_flow[unlist(cu_cont), ]

  # subset flow stations within CU boundary and calculate statistics for each GCM
  wp_cu <- wp_vm %>%
    filter(ID %in% cu_stations$ID) %>%
    mutate(mean = map_dbl(data, ~ mean(.x$mean)),
      sd   = map_dbl(data, ~ sd(.x$mean)),
      qlow = map_dbl(data, ~ quantile(.x$mean,  probs = qlowgcm)),
      qhigh = map_dbl(data, ~ quantile(.x$mean, probs = qhighgcm)))

  # calculate mean, sd, and quantiles for each station within CU boundary across all GCMs
  wp_cu_ens <- wp_cu %>%
    nest(.by = c("ID", "experiment_id", "period")) %>%
    mutate(mean = map_dbl(data, ~ mean(.x$mean)),
      sd   = map_dbl(data, ~ sd(.x$mean)),
      qlow = map_dbl(data, ~ quantile(.x$mean, probs = qlowgcm)),
      qhigh = map_dbl(data, ~ quantile(.x$mean, probs = qhighgcm))) %>%
    mutate(source_id = "ensemble", .after = experiment_id)


  #-------- run summary statistics functions ----

  # nuseds summary
  nu_i <- data.table(nuseds_cu) %>%
    summarise(
      nuseds_sites = n(),
      nuseds_indicator = sum(IS_INDICATOR == "Y"),
      nuseds_obs = sum(n, na.rm = TRUE),
      .groups = "drop")

  # watershed regime and gauge coverage
  reg_i <- regime_stats(watershed_flow, cu_boundary_i)

  # get stats using functions
  ss_i <- stream_BCFP_stats(bcfpa_cu) %>%
    bind_cols(nu_i, reg_i, fw_timing_i)


  fwT_i <- stream_temp_stats(fwT_cu,
    RCP = c("00", "45", "85"),
    periods = c("0", "1", "3", "4", "5"),
    models = "Tw8",  # or Tav
    GCMs = c(1:6),
    model_rs = TRUE
  )

  fwQlow_i <- stream_flow_stats(fwQ8_cu,
    model_rs = TRUE,
    periods = c("0", "3", "4", "5"),
    RCP = c("45", "85"),
    months = "8")

  fwQhigh_i <- stream_flow_stats(fwQNDJ_cu,
    model_rs = TRUE,
    periods = c("0", "3", "4", "5"),
    RCP = c("45", "85"),
    months = "18")   # code for NOVDECJAN months

  ct_i <- stream_ct_stats(fwct_cu,
    model_rs = TRUE)

  ENM_i <- ENM_stats(ENM_cu,
    RCP = c("00", "45"),
    period = c("3", "5"),
    historical = "0",
    model_rs = FALSE
  )

  wp_i <- station_lowflow_stats(wp_cu_ens,
    historical = 0)

  # combine all into one table
  all_i <- list(streams =  ss_i,
    ENM    =  ENM_i,
    CT     =  ct_i,
    fwT    =  fwT_i,
    fwQlow =  fwQlow_i,
    wpQlow =  wp_i,
    fwQhigh = fwQhigh_i)

  fwR_all[[i]] <- all_i

}

names(fwR_all) <- cu_run$FULL_CU_IN


#--------------------- 6. Summarize statistics across CUs ----------------------


# Define a function to pull out CU stats
CVIS_fw_pull <- function(data,
                         RCP_pick = "45",
                         SSP_pick = "ssp370",
                         period_pick = "3",
                         ct_pick = "CT_anad") {
  # Helper to safely filter and rename a tibble
  safe_process <- function(tbl,
                           filter_expr,
                           drop_cols = NULL,
                           prefix = "") {
    if (is.null(tbl)) return(tibble())

    # Save original column names before filtering
    original_cols <- names(tbl)

    # Apply filtering
    tbl_filtered <- tryCatch(
      tbl %>% filter(!!rlang::parse_expr(filter_expr)),
      error = function(e) tibble()
    )

    # If empty, return empty tibble with renamed original columns
    if (nrow(tbl_filtered) == 0) {
      empty <- tibble::as_tibble(setNames(rep(list(NA), length(original_cols)), original_cols))
      if (!is.null(drop_cols)) empty <- empty %>% select(-all_of(drop_cols))
      return(rename_with(empty, ~ paste0(prefix, .x)))
    }

    # Drop columns and rename
    if (!is.null(drop_cols)) tbl_filtered <- tbl_filtered %>% select(-all_of(drop_cols))
    return(rename_with(tbl_filtered, ~ paste0(prefix, .x)))
  }

  ss <- data$streams %>%
    mutate(RCP = RCP_pick,
      period =  period_pick,
      SSP = SSP_pick,
      .before = 1)

  ENM_pull <- safe_process(data$ENM,
    glue::glue('RCP == "{RCP_pick}" & period == "{period_pick}"'),
    drop_cols = c("period", "RCP", "n_streams"),
    prefix = "")

  ct_pull <- safe_process(data$CT,
    glue::glue('CT == "{ct_pick}"'),
    prefix = "")

  fwT <- safe_process(data$fwT,
    glue::glue('RCP == "{RCP_pick}" & period == "{period_pick}"'),
    drop_cols = c("period", "RCP", "n_streams", "total_length"),
    prefix = "")

  fwQlow <- safe_process(data$fwQlow,
    glue::glue('RCP == "{RCP_pick}" & period == "{period_pick}"'),
    drop_cols = c("period", "RCP", "n_streams", "total_length"),
    prefix = "low")

  fwQhigh <- safe_process(data$fwQhigh,
    glue::glue('RCP == "{RCP_pick}" & period == "{period_pick}"'),
    drop_cols = c("period", "RCP", "n_streams", "total_length"),
    prefix = "high")

  wpQlow <- safe_process(data$wpQlow,
    glue::glue('experiment_id == "{SSP_pick}" & period == "{period_pick}"'),
    drop_cols = "period",
    prefix = "")

  bind_cols(ss, ct_pull, ENM_pull, fwT, fwQlow, wpQlow, fwQhigh, )

}

periods <- c("3", "4", "5")  # periods to loop through
RCPs    <- c("45", "85")  # RCPs to loop through
SSPs    <- c("ssp370", "ssp585")  # SSPs to include for station low flow model

for (i in 1:length(periods)) {
  for (f in 1:length(RCPs)) {

    fwR_i <- imap_dfr(fwR_all, ~ {
      tryCatch({
        CVIS_fw_pull(.x,
          RCP_pick = RCPs[f],
          period_pick = periods[i],
          ct_pick = "CT_anad",
          SSP_pick = SSPs[f]) %>%
          mutate(FULL_CU_IN = .y, .before = 1)
      })
    }) %>%
      left_join(select(cu_run, FULL_CU_IN, CU_NAME, CU_Species, FAZ),
        by = "FULL_CU_IN") %>%
      relocate(CU_NAME, CU_Species, FAZ, .after = FULL_CU_IN)

    if (i == 1 && f == 1) {
      fwR_all_flat <- fwR_i
    } else {
      fwR_all_flat <- bind_rows(fwR_all_flat, fwR_i)
    }

  }
}


#--------------- 5. Create spatial summary object -----------------------------

### Low flow stats
## same process as CU stats
GCMs <- c("access1", "canesm2", "ccsm4", "cnrm", "hadgem2", "mpi")
GCM_grep <- paste(paste0(GCMs, collapse = "|"), "mean", sep = "|")

fwQ8_long <- fwQ8 %>%
  select(segmented_stream_id, contains("flow")) %>%
  pivot_longer(cols = matches("^flow"),
    names_to = c(".value", "RCP", "GCM", "period", "month"),
    names_pattern = paste0("^(flow)_(rcp\\d{2}|historical)_(", GCM_grep, ")_(\\d+)_(\\d+)$")) %>%
  mutate(RCP = substr(RCP, start = 4, stop = 5)) # remove rcp from column character

fwQ8_wide <- fwQ8_long %>%
  filter(GCM == "mean") %>%
  pivot_wider(
    names_from = c(month, RCP, period),
    values_from = flow,
    names_prefix = "flow_") %>%
  arrange(segmented_stream_id)

hist_col <- names(fwQ8_wide)[str_detect(names(fwQ8_wide), "flow_8_to_0")]
proj_col <- names(fwQ8_wide)[str_detect(names(fwQ8_wide), "45|85")]

fwQ8_wide <- fwQ8_wide %>%
  mutate(histQ = !!sym(hist_col),
    across(contains(proj_col), ~ (.x - histQ) / histQ, .names = "Qpdelta_{.col}"))

### High flow stats
## same process as CU stats
fwQNDJ_long <- fwQNDJ %>%
  select(segmented_stream_id, contains("flow")) %>%
  pivot_longer(cols = matches("^flow"),
    names_to = c(".value", "RCP", "GCM", "period", "month"),
    names_pattern = paste0("^(flow)_(rcp\\d{2}|historical)_(", GCM_grep, ")_(\\d+)_(\\d+)$")) %>%
  mutate(RCP = substr(RCP, start = 4, stop = 5)) # remove rcp from column character

fwQNDJ_wide <- fwQNDJ_long %>%
  filter(GCM == "mean") %>%
  pivot_wider(
    names_from = c(month, RCP, period),
    values_from = flow,
    names_prefix = "flow_") %>%
  arrange(segmented_stream_id)

hist_col <- names(fwQNDJ_wide)[str_detect(names(fwQNDJ_wide), "flow_18_to_0")]
proj_col <- names(fwQNDJ_wide)[str_detect(names(fwQNDJ_wide), "45|85")]

fwQNDJ_wide <- fwQNDJ_wide %>%
  mutate(histQ = !!sym(hist_col),
    across(contains(proj_col), ~ (.x - histQ) / histQ, .names = "Qpdelta_{.col}"))

## Historic flow stats for all months
hflow_sub <- hflow %>%
  select(segmented_stream_id, contains("flow")) %>%
  rename_with(~ str_replace_all(., "mean_flow_m3s", "flow")) %>%
  rename_with(~ str_replace_all(.,  "_1$", "_0"))


# temp stats by stream
fwT_indi <- fwT %>%
  mutate(histT = !!sym(paste(T_model, "0_00", historical, sep = "_"))) %>%  # add historical Tw8
  select(segmented_stream_id, histT,
    all_of(grep(paste0("^", T_model, "_", 9), names(fwT), value = TRUE))) %>%
  mutate(across(contains(T_model), ~ .x - histT, .names = "delta_{.col}"))


## create spatial object with all indicator variables
fwModels <- bcfpa %>%
  select(-contains(c("mapping_code", "obsrvtn", "barriers"))) %>%
  left_join(select(fwct, segmented_stream_id, CT_anad),
    join_by(segmented_stream_id)) %>%
  left_join(fwT_indi,
    join_by(segmented_stream_id)) %>%
  left_join(fwQ8_wide,
    join_by(segmented_stream_id)) %>%
  left_join(fwQNDJ_wide,
    join_by(segmented_stream_id))

#----------------- 6. Write files---------

saveRDS(fwModels, paste0(today, "_fw_stream_models.Rds"))
save(fwR_all, fwR_all_flat, file = file.path(paths$fw, paste0(today, "_fw_rearing_indicators.Rdata")))

write.csv(fwR_all_flat, file = file.path(paths$fw, paste0(today, "_fw_rearing_stats.csv")), row.names = FALSE)


# ExPanD(fwR_all_flat)
