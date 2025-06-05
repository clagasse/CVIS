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

historical <- "0"   #historical climatology period for temperature models
                     # 0 = 1981-2000,  1 = 2001-2020
T_model <- "Tw8"    #temperature model Tw8 = thermalscapes August temp
                        

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
  mutate(regime = as.factor(regime)) %>%
  st_transform(3005)

stations_flow <- st_read(file.path(paths$climate, "Ruzzante_low_flows", "stations.gpkg")) %>%
  st_transform(3005)


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
stream_BCFP_stats <- function(bcfp_cu)  {
  
  #coordinates
  coords <- st_coordinates(bcfp_cu)

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
    cu_area = st_area(cu_boundary_i) / 1e6,
    avg_elevation = mean(coords[,"Z"]),
    avg_lat    = mean(coords[,"Y"]),
    avg_lon    = mean(coords[,"X"])
  )
  
  return(stream_stats)
}


#------------------------ 4.1 stream temp stats function----

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
                     periods = c("0","3","4", "5"),
                     historical = "0",
                     GCMs = c(1:6),  #GCMs to include in summary
                     models = c("Tw8"),  #thermalscapes August temp
                     model_rs = TRUE
                     ) {
  
  
  if(model_rs == TRUE) fwT_cu <- fwT_cu[fwT_cu$model_rs == TRUE,]
  models_grep <- paste(models, collapse = "|")
  
  hist_col <- paste(models, "0", "00", historical, sep = "_")  #historical Tw8 column
  
  #pivot longer across selected model (e.g. Tw8)
  fwT_cu_long <- fwT_cu %>%
    mutate(histT = !!sym(hist_col)) %>%  #add historical Tw8
    #mutate(histT = .data[[grep(paste0("^", models, "_", "0:"), names(.), value = TRUE)]]) %>%  #add historical Tw8
    pivot_longer(cols = matches(paste0("^(",models_grep,")_")), #"^(Tw8|Tav|TlowPI|ThiPI)_"
                 names_to = c(".value", "GCM", "RCP", "period"),
                 names_pattern = paste0("^(", models_grep,")_(\\d)_(\\d{2})_(\\d)$")) %>%
    select(matches(paste0("(",models_grep,")")), 
           segmented_stream_id, length_metre, model_rs, model_access_salmon,
           model_habitat_salmon, GCM, RCP, period, histT)   %>%
    filter(RCP %in% c(historical,RCP), 
           period %in% periods) %>%
    mutate(across(contains(models_grep), ~ .x - histT, .names = "delta_{.col}"),
           decade_interval = decade_calc(historical, period_pick = period))
  
  #calculate the mean, sd, and quantiles for temperature of streams within CU boundary
  Ts_stats <- fwT_cu_long %>%
    filter(!is.na(histT)) %>% 
    group_by(GCM, RCP, period) %>%
    summarize(
      n_streams = n(),
      total_length = sum(length_metre, na.rm = TRUE),
      across(
        .cols = c(all_of(models)),
        .fns = list(
            wmean_ens = ~wmean(.x, length_metre, na.rm = TRUE),
            wsd_ens = ~wsd(.x, length_metre, na.rm = TRUE),
            wq025_ens = ~wqt(.x, length_metre, prob = 0.025, na.rm = TRUE),
            wq975_ens = ~wqt(.x, length_metre, prob = 0.975, na.rm = TRUE),
            rate_wmean_ens = ~wmean((.x - histT)/decade_interval, length_metre, na.rm = TRUE),
            rate_wsd_ens   = ~wsd((.x - histT)/decade_interval, length_metre, na.rm = TRUE),
            rate_wq025_ens = ~wqt((.x - histT)/decade_interval, length_metre, prob = 0.025, na.rm = TRUE),
            rate_wq975_ense = ~wqt((.x - histT)/decade_interval, length_metre, prob = 0.975, na.rm = TRUE)
            ),
        .names = "{.col}_{.fn}"
      ),
      .groups = "drop"
    )
  
  #take the min, max, range across GCM means for the CU
  Ts_proj_summary <- Ts_stats %>%
    filter(GCM %in% GCMs) %>%
    group_by(RCP, period) %>%
    summarize(
      across(contains("mean"), list( sd_gcm = ~sd(.x, na.rm = TRUE),
                                     min_gcm = ~min(.x, na.rm = TRUE),
                                     max_gcm = ~max(.x, na.rm = TRUE),
                                     CV_gcm = ~sd(.x, na.rm = TRUE) / mean(.x, na.rm = TRUE))),
      .groups = "drop"
    )
  
  Ts_stats <- filter(Ts_stats, GCM %in% c(0,9)) %>%
    left_join(Ts_proj_summary, 
              by = c("RCP", "period"))
  
  return(Ts_stats)
}

#----------------4.2  Flow statistics function--------



stream_lowflow_stats <- function(fwQ_cu, 
                          model_rs = TRUE, 
                          periods = c("1", "40", "80")) {
  
  if(model_rs == TRUE) {
    fwQ_cu <- fwQ_cu[fwQ_cu$model_rs == TRUE,]
  }
  
  fwQ_cu_long <- fwQ_cu %>%
    rename_with(~ sub("_flow_m3s", "", .x), starts_with("mean_flow_m3s_")) %>%
    pivot_longer(cols = matches("mean"),
                 names_to = c(".value", "month","period"),
                 names_pattern = "^(mean)_(\\d+)_(\\d+)$") %>%
    mutate(month = as.integer(month)) %>%
    filter(period %in% periods)
  
  fwQ8_cu <- fwQ_cu_long %>%
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
    mutate(PMAD8 = month_8 / MAD_hist,   #August flow as proportion of historical MAD
           #PMAD8_delta = (month_8 / MAD_hist) - (Q8_hist / MAD_hist),  #change in proportional flow to MAD
           Q8delta = month_8 - Q8_hist,
           Q8pdelta = (month_8 - Q8_hist) / Q8_hist)
  
  fwQ8_cu_stats <- fwQ8_cu %>%
    group_by(period) %>%
    summarize(
      n_streams = n(),
      total_length = sum(length_metre, na.rm = TRUE),
      Q8_wmean_ens       = wmean(month_8, length_metre, na.rm = TRUE),
      Q8_wsd_ens         = wsd(month_8, length_metre, na.rm = TRUE),
      QMADhist_wmean     = wmean(MAD_hist,length_metre, na.rm = TRUE),
      QMADhist_wsd       = wsd(MAD_hist, length_metre, na.rm = TRUE),
      Q8pdelta_wmean     = wmean(Q8pdelta, length_metre, na.rm = T),
      Q8pdelta_wsd       = wsd(Q8pdelta, length_metre, na.rm = TRUE),
      # propMAD8_mean     = wmean(PMAD8, length_metre, na.rm = TRUE),
      # propMAD8_sd      = wsd(PMAD8, length_metre, na.rm = TRUE),
      # propMAD8_CV      = propMAD8_sd / propMAD8_mean,
      # propMAD8_min     = min(PMAD8, na.rm = TRUE),
      # propMAD8_max     = max(PMAD8, na.rm = TRUE),
      # propMAD8_range   = max(PMAD8, na.rm = TRUE) - min(PMAD8, na.rm = TRUE),
      .groups = "drop")
  
}


#----------------------4.3 Cumulative threat stats function -------------
stream_ct_stats <- function(fwct_cu, 
                      cts = c("AIS", "AnadFrag", "FlowAlt", "HabDest",
                              "LatFrag", "ResFrag", "RipDist", "Nutrient",
                              "Pollution", "Sediment", "CT_anad"),
                      model_rs = TRUE) {
  
  if(model_rs == TRUE) fwct_cu <- fwct_cu[fwct_cu$model_rs == TRUE,]
  
  #select ct columns
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
      ct_wmean = wmean(value, length_metre, na.rm = TRUE),
      ct_wsd = wsd(value, length_metre, na.rm = TRUE),
      ct_wq025 = wqt(value, length_metre, prob = 0.025, na.rm = TRUE),
      ct_wq975 = wqt(value, length_metre, prob = 0.975, na.rm = TRUE),
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
  
  #subset ENM cols based on ending with time period values
  time_picks <- c(periods, historical)
  time_grep <- paste(paste0("_", time_picks), collapse = "|")
  ENM_cols <- grep(paste0("(", time_grep, ")$"), names(ENM_cu), value = TRUE)  #ends with
  
  if(model_rs == FALSE) ENM_cu <- ENM_cu[ENM_cu$model_rs == TRUE,]
  
  ENM_cu_long <- ENM_cu %>%
    select(contains(ENM_cols), Length_km, segmented_stream_id) %>%
    filter(!is.na(.data[[ENM_cols[1]]])) %>%   #remove rows with missing ENM values
    pivot_longer(cols = all_of(ENM_cols),
                 names_to = c(".value", "RCP", "period"),
                 names_pattern = "^(.*)_(\\d{2})_(\\d)$") %>%
    arrange(segmented_stream_id, RCP, period) %>%
    mutate(Fav_hist = Fav_f.0) %>%  #historical fav
    fill(Fav_hist, .direction = "down") %>%
    mutate(Fav = coalesce(Fav_f.9, Fav_hist),
           Fav_change = Fav - Fav_hist,) %>% #calculate change from historical
    select(-c(Fav_f.0, Fav_hist, Fav_f.9, Fav_change_9))


  #calculate mean and other statistics from single column
  ENM_stats <- ENM_cu_long %>%
    group_by(RCP, period) %>%
    summarise(
      n_streams = n(),
      total_length = sum(Length_km * 1000, na.rm = TRUE),
      across(starts_with("Fav"), 
             list(
                  wmean = ~wmean(.x, Length_km, na.rm = TRUE),
                  wsd = ~wsd(.x, Length_km, na.rm = TRUE),
                  wq025 = ~wqt(.x, Length_km, prob = 0.025, na.rm = TRUE),
                  wq975 = ~wqt(.x, Length_km, prob = 0.975, na.rm = TRUE)),
             .names = "{.col}_{.fn}"),

      .groups = "drop"
    ) 
  
  return(ENM_stats)
}

#---------------- 4.5 station low flow stats function -----

station_lowflow_stats <- function(wp_cu,
                                  historical = 0) {
  
  wp_cu <- wp_cu %>%
    mutate(mean_hist = ifelse(period == historical, mean, NA)) %>%
    fill(mean_hist, .direction = "down")
  
  wp_i <- wp_cu %>%
    group_by(experiment_id, period) %>%
    summarise(n_stations = n(),
              mean_st = mean(mean),
              sd_stations   = sd(mean),
              sd_GCM    = mean(sd),
              q025_GCM = mean(q025),
              q975_GCM = mean(q975),
              deltamean   = mean(mean - mean_hist, na.rm = TRUE),
              deltaq025   = mean(q025 - mean_hist, na.rm = TRUE),
              deltaq975   = mean(q975 - mean_hist, na.rm = TRUE),
              dpropmean = deltamean / mean_hist,
              dpropq025 = deltaq025 / mean_hist,
              dpropq975 = deltaq975 / mean_hist,
              .groups = "drop") 
  
}


#get proportion of hydrologic regime type for watersheds within the CU boundary
regime_stats <- function(watershed_flow, cu_boundary_i) 
{
  
  # Calculate intersection
  intersection_snow <- st_intersection(cu_boundary_i, filter(watershed_flow, regime == "Snowfall"))
  intersection_rain <- st_intersection(cu_boundary_i, filter(watershed_flow, regime == "Rainfall"))
  intersection_glac <- st_intersection(cu_boundary_i, filter(watershed_flow, regime == "Glacial"))
  intersection_hybr <- st_intersection(cu_boundary_i, filter(watershed_flow, regime == "Hybrid"))
  
  # Calculate areas
  area_poly1 <- st_area(cu_boundary_i)
  
  area_snow <- sum(st_area(intersection_snow)) / area_poly1
  area_rain <- sum(st_area(intersection_rain)) / area_poly1
  area_glac <- sum(st_area(intersection_glac))/ area_poly1
  area_hybr <- sum(st_area(intersection_hybr))/ area_poly1
  
  area_covr <- sum(area_snow, area_rain, area_glac, area_hybr)
  
  regime <- tribble(
    ~prop_snow, ~prop_rain, ~prop_hybrid, ~prop_glacial, ~prop_coverage,
    area_snow, area_rain, area_hybr, area_glac, area_covr)
  
}

# ----------------------- 5. Calculate stream network CU indicators-------------------

fwR_all <- list()  #initialize list for storing all results

for(i in 1:n.CUs) {
  
  cu_i <- cu_run$FULL_CU_IN[i]
  sp_pick <- cu_run$spp[cu_run$FULL_CU_IN == cu_i] #species abbr
  sp_pick_bcfp <- spp_lookup$spp_abr_bcfp[spp_lookup$spp_abr == sp_pick]  #BCFP species abbr (different for Chinook)
  # Subset CU boundary
  cu_boundary_i <- cu_boundary[cu_boundary$FULL_CU_IN == cu_i,]
  
  #pick column of stream indices to subset for CU
  stream_cu_sub <- stream_cu_picks[,colnames(stream_cu_picks) == cu_i]
  reaches_ENM_sub <- ENM_cu_picks[,colnames(ENM_cu_picks) == cu_i]
  
  #subset nuseds observations
  nuseds_cu <- nuseds_Fr[nuseds_Fr$FULL_CU_IN == cu_i,]

  bcfpa_cu <- bcfpa[stream_cu_sub,] %>%
    select(segmented_stream_id:mad_m3s, 
           contains(sp_pick_bcfp)) %>%  #subset model for CU species
    rename(model_spawning = starts_with("model_spawning"),
           model_rearing  = starts_with("model_rearing")) %>%
    mutate(model_rs = if_any(starts_with("model"), ~ . == TRUE))  #get boolean for model spawning and rearing
  
  #bcfpa_cu_rs <- filter(bcfpa_cu, model_rs == TRUE)
  
  #-------create subsetted data tables for each model
  fwT_cu <- fwT[stream_cu_sub,] %>% 
    left_join(select(bcfpa_cu, segmented_stream_id, model_rs), 
              by = "segmented_stream_id") 
  
  fwQ_cu <- fwQ[stream_cu_sub,] %>%
    left_join(select(bcfpa_cu, segmented_stream_id, model_rs), 
              by = "segmented_stream_id")
  
  fwct_cu<- fwct[stream_cu_sub,] %>%
    left_join(select(bcfpa_cu, segmented_stream_id, model_rs), 
              by = "segmented_stream_id")
  
  ENM_cu <- data.table(reaches_ENM_all[reaches_ENM_sub,])  %>%
    select(linear_feature_id, segmented_stream_id, Shape_Length, Length_km, 
           contains(sp_pick)) %>%
    left_join(select(bcfpa_cu, segmented_stream_id, model_rs),
              by = "segmented_stream_id") %>%
    rename_with(~ ifelse(grepl("Fav", .x), substr(.x, 4, nchar(.x)), .x))  #remove species prefix
  
  # get flow stations within each CU boundary
  cu_cont <- st_contains(cu_boundary_i, stations_flow, sparse = T) 
  cu_stations <- stations_flow[unlist(cu_cont),]
  #subset flow stations from ensemble model averages
  wp_cu <- wp_stats_ens %>%
    filter(ID %in% cu_stations$ID)
  
  #-------- run summary statistics functions
  
  #nuseds summary
  nu_i <- data.table(nuseds_cu) %>%
    summarise(
      nuseds_sites = n(),
      nuseds_indicator = sum(IS_INDICATOR == "Y"),
      nuseds_obs = sum(n, na.rm = TRUE),
      .groups = "drop")
  
  #watershed regime and gauge coverage
  reg_i <- regime_stats(watershed_flow, cu_boundary_i)
  
  #get stats using functions
  ss_i <- stream_BCFP_stats(bcfpa_cu) %>%
    bind_cols(nu_i, reg_i)
  
  fwT_i <- stream_temp_stats(fwT_cu, 
                    RCP = c("00", "45", "85"), 
                    periods = c("0", "1", "3", "4", "5"),
                    models = "Tw8",  #or Tav
                    GCMs = c(1:6),
                    model_rs = TRUE
  )

  fwQlow_i <- stream_lowflow_stats(fwQ_cu, 
                          model_rs = TRUE, 
                          periods = c("1", "40", "60", "80"))
  
  ct_i <- stream_ct_stats(fwct_cu, 
                          model_rs = TRUE)
  
  ENM_i <- ENM_stats(ENM_cu, 
                    RCP = c("00", "45" ), 
                    period = c("3", "5"),
                    historical = "0",
                    model_rs = FALSE
  )
  
  wp_i <- station_lowflow_stats(wp_cu,
                                        historical = 0)
  
  #combine all into one table
  all_i <- list(streams =  ss_i, 
                 ENM    =  ENM_i,
                 CT     =  ct_i,
                 fwT    =  fwT_i, 
                 fwQlow =  fwQlow_i,
                 wpQlow =  wp_i) 
  
  fwR_all[[i]] <- all_i
  
  # if(i == 1) {
  #   fwR_all <- all_i
  #   ss_all <- ss_i
  #   nu_all <- nu_i
  #   ct_all <- ct_i
  #   Ts_all <- Ts_i
  #   Qs_all <- Qs_i
  #   ENM_all <- ENM_i
  #     } else {
  #   fwR_all <- bind_rows(fwR_all, all_i)
  #   ss_all <- bind_rows(ss_all, ss_i)
  #   nu_all <- bind_rows(nu_all, nu_i)
  #   ct_all <- bind_rows(ct_all, ct_i)
  #   Ts_all <- bind_rows(Ts_all, Ts_i)
  #   Qs_all <- bind_rows(Qs_all, Qs_i)
  #   ENM_all <- bind_rows(ENM_all, ENM_i)
  # }
  # if(i == n.CUs) {
  #   print(paste(i, "CU fwR stats done"))
  #   rm(all_i)
  #   
  #   fwR_all <- fwR_all %>%
  #     left_join(select(cu_run, FULL_CU_IN, CU_NAME, CU_Species), 
  #               by = "FULL_CU_IN") %>%
  #     relocate(CU_NAME, CU_Species, .after = FULL_CU_IN)
  # }
  
}

names(fwR_all) <- cu_run$FULL_CU_IN


#--------------------- 6. Summarize statistics across CUs ----------------------


# Define a function to pull out CU stats
CVIS_fw_pull <- function(data,
                         RCP_pick = "45",
                         SSP_pick = "ssp370",
                         period_pick = "3",
                         ct_pick = "CT_anad") {
  
  period_pick_flow <- case_when(
    period_pick == "2" ~ "20",
    period_pick == "3" ~ "40",
    period_pick == "4" ~ "60",
    period_pick == "5" ~ "80"
  )
  
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
    return(rename_with(tbl_filtered, ~ paste0(prefix, .x )))
  }

  ss <- data$streams %>%
    mutate(RCP = RCP_pick,
           period =  period_pick,
           SSP = SSP_pick, 
           .before = 1)
  
  ENM_pull <- safe_process(data$ENM, 
                           glue::glue('RCP == "{RCP_pick}" & period == "{period_pick}"'), 
                           drop_cols = c("period", "RCP"), 
                           prefix = "ENM_")
  
  ct_pull <- safe_process(data$CT, 
                          glue::glue('CT == "{ct_pick}"'), 
                          prefix = "ct_")
  
  fwT <- safe_process(data$fwT, 
                      glue::glue('RCP == "{RCP_pick}" & period == "{period_pick}"'), 
                      drop_cols = c("period", "RCP", "n_streams", "total_length"), 
                      prefix = "")
  
  fwQlow <- safe_process(data$fwQlow,
                         glue::glue('period == "{period_pick_flow}"'), 
                         drop_cols = c("period", "n_streams", "total_length"),
                         prefix = "")
  
  wpQlow <- safe_process(data$wpQlow, 
                         glue::glue('experiment_id == "{SSP_pick}" & period == "{period_pick}"'),
                         drop_cols = "period", 
                         prefix = "wpQ_")
  
  bind_cols(ss, ct_pull, ENM_pull, fwT, fwQlow, wpQlow)
  
}

periods <- c("3", "4", "5")  #periods to loop through
RCPs    <- c("45", "85")  #RCPs to loop through

for(i in 1:length(periods)) {
  for(f in 1:length(RCPs)) {
  
  fwR_i <- imap_dfr(fwR_all, ~ {
    tryCatch({
      CVIS_fw_pull(.x, 
                   RCP_pick = RCPs[f], 
                   period_pick = periods[i], 
                   ct_pick = "CT_anad",
                   SSP_pick = "ssp370") %>%
        mutate(FULL_CU_IN = .y, .before = 1)
    })
  }) %>%
    left_join(select(cu_run, FULL_CU_IN, CU_NAME, CU_Species, FAZ), 
              by = "FULL_CU_IN") %>%
    relocate(CU_NAME, CU_Species, FAZ, .after = FULL_CU_IN)
  
  if(i == 1 && f == 1) {
    fwR_all_flat <- fwR_i
  } else {
    fwR_all_flat <- bind_rows(fwR_all_flat, fwR_i)
  }
  
  }
}


#--------------- 7. Create spatial summary object -----------------------------

# flow stats by stream
## same process as CU stats
fwQ_long <- fwQ %>%
  rename_with(~ sub("_flow_m3s", "", .x), starts_with("mean_flow_m3s_")) %>%
  pivot_longer(cols = matches("mean"),
               names_to = c(".value", "month","period"),
               names_pattern = "^(mean)_(\\d+)_(\\d+)$") %>%
  mutate(month = as.integer(month))

fwQ8 <- fwQ_long %>%
  filter(month %in% c(8,17)) %>%
  pivot_wider(
    names_from = month,
    values_from = mean,
    names_prefix = "month_") %>%
  arrange(segmented_stream_id) %>%
  mutate(
    MAD_hist = if_else(period == "1", month_17, NA_real_),
    Q8_hist  = if_else(period == "1", month_8, NA_real_)) %>%
  fill(MAD_hist, Q8_hist, .direction = "down") %>%
  mutate(PMAD8 = month_8 / MAD_hist,   #August flow as proportion of historical MAD
         #PMAD8_delta = (month_8 / MAD_hist) - (Q8_hist / MAD_hist),  #change in proportional flow to MAD
         Q8_delta = month_8 - Q8_hist,
         Q8_deltaprop = (month_8 - Q8_hist) / Q8_hist) %>%
  pivot_wider(id_cols = c(segmented_stream_id, MAD_hist, Q8_hist),
            names_from = period,
              values_from = c(PMAD8, Q8_delta, Q8_deltaprop, month_8),
              names_prefix = "p")


#temp stats by stream
fwT_indi <- fwT %>%
  mutate(histT = !!sym(paste(T_model, "0_00", historical, sep = "_"))) %>%  #add historical Tw8
  select(segmented_stream_id, histT,
         all_of(grep(paste0("^",T_model, "_", 9), names(fwT), value = TRUE))) %>%
  mutate(across(contains(T_model), ~ .x - histT, .names = "delta_{.col}"))


## create spatial object with all indicator variables
fwModels <- bcfpa %>%
  select(-contains(c("mapping_code", "obsrvtn", "barriers"))) %>%
  left_join(select(fwct, segmented_stream_id, CT_anad),
            join_by(segmented_stream_id)) %>%
  left_join(fwT_indi,
            join_by(segmented_stream_id)) %>%
  left_join(fwQ8,
            join_by(segmented_stream_id))


#----------------- 8. Write files---------

save(fwModels, fwR_all, fwR_all_flat, file = file.path(paths$fw, paste0(today, "_fw_rearing_models_indicators.Rdata")))

write.csv(fwR_all_flat, file = file.path(paths$fw, paste0(today, "_fw_rearing_stats.csv")), row.names = FALSE)

#ExPanD(fwR_all_flat)