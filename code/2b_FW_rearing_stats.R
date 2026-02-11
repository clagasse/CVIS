#---- 1. Overview and setup ----
#
# 2b_FW_rearing_stats.R
#
# This code loads stream-level climate and stressor models processed in 2a_FW_data_process.R
# and calculates summary statistics of climate indicators relevant to spawning and rearing.
# Statistics are calculated for each CU based on accessible streams with intrinsic habitat
# potential in the CU boundary, based on BC Fishpass models.
# An object with subsetted streams within each CU from 2b_FW_boundary_subset.R is required.
# Models currently included within stream stats include
# tw8 - Thermalscape August stream temperature
# fwQ - stream network flow derived from PCIC grid model
# ct - cumulative threats model
# st8 - statistical model of August flow at hydrological stations (Ruzzante in prep)
# ENM - ecological niche model of habitat favourability

library(here)
setwd(here())
source(file.path(here(), "code", "0_setup.R"))


#---- 2. load spatial objects ----

# load CU stream selections - for subsetting when calculating statistics
load(file.path(paths$fw, "fw_streampicks_tscapes.Rdata"))

#load stream network object with model values and BC fishpass values
load(file.path(paths$fw, "fw_models_tscapes.Rds"))

# load statistical model projections of August flows for flow stations
load(file.path(paths$fw,  "Statistical_flow_projections.Rds"))

# load flow stations spatial objects
stations_stats <- read.csv(file.path(paths$climate, "Ruzzante_low_flows", "stations_performance.csv"))
# watershed hydrologic regimes
watershed_flow <- st_read(file.path(paths$climate, "Ruzzante_low_flows", "watersheds.gpkg")) %>%
  left_join(select(stations_stats, ID, regime), by = c("ID" = "ID")) %>%
  mutate(regime = as.factor(regime)) %>%
  st_transform(3005)

stations_flow <- st_read(file.path(paths$climate, "Ruzzante_low_flows", "stations.gpkg")) %>%
  st_transform(3005)


# set common length column name.  bc fishpass uses length_metre for segments
if (base_network == "tscapes") {
  fw_models <- fw_models %>%
    mutate(length_metre = shape_length,
           stream_id_key = linear_feature_id) %>%
    select(-any_of(c("shape_leng", "shape_length")))
}
# set a common stream id key
if (base_network == "bcfpa") {
  fw_models <- fw_models %>%
    mutate(stream_id_key = segmented_stream_id)
}


#---- 3. Functions for indicators ----

# summary functions
stream_BCFP_stats <- function(streams) {
  # get coordinates and transform to lat/long
  coords <- streams %>%
    st_transform(4269) %>%
    st_coordinates()
  
  # calculate stream stats for spawning and rearing streams
  stream_stats <- tibble(
    total_length_acc = sum(streams$length_metre, na.rm = T),
    total_length_rs = sum(streams$length_metre[streams$model_rs == TRUE], na.rm = T),
    total_length_rear = sum(streams$length_metre[streams$model_rearing == TRUE], na.rm = T),
    total_length_spawn = sum(streams$length_metre[streams$model_spawning == TRUE], na.rm = T),
    proportion_rear  = total_length_rear / total_length_acc,
    proportion_spawn = total_length_spawn / total_length_acc,
    proportion_rs    = total_length_rs   / total_length_acc,
    avg_order = mean(streams$stream_order, na.rm = T),
    avg_order_rear = mean(streams$stream_order[streams$model_rearing == TRUE], na.rm = T),
    avg_order_spawn = mean(streams$stream_order[streams$model_spawning == TRUE], na.rm = T),
    n_streams = length(unique(streams$linear_feature_id)),
    n_segments = length(unique(streams$stream_id_key)),
    cu_area = st_area(cu_boundary_i) / 1e6,
    avg_elevation = mean(coords[, "Z"]),
    avg_lat    = mean(coords[, "Y"]),
    avg_lon    = mean(coords[, "X"])
  )
  
  return(stream_stats)
}


#---- 3.1 stream temp stats function----

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


stream_env_stats <- function(
    fwT_cu,
    RCP = rcp_vec,
    period_codes = c("0", "3", "4", "5"),
    historical = historical_code,
    indicator = c("tw8"),     # tw8, flow8, flow18, etc.
    dsmodel_name = "tscapes",     #name of downscaled model to include in output column
    model_rs = TRUE,       # filter to only include modelled rearing and spawning habitat from BC fish pass
    qlowsp = qlsp,         # lower quantile for spatial variation
    qhighsp = qhsp,        # upper quantile for spatial variation
    gcm_names = gcm_codes, # mapping from GCM id -> name (named character vector)
    include_pdelta = TRUE,   # compute proportional difference vs historical baseline
    hist_zero_tol = 0          # treat |histT| <= tol as 0 to avoid Inf; set e.g. 1e-6 if needed
) {
  
  if (model_rs == TRUE) fwT_cu <- fwT_cu[fwT_cu$model_rs == TRUE, ]
  indicator_grep <- paste(indicator, collapse = "|")
  
  # historical column name (e.g., "tw8_...*_0")
  hist_col <- names(fwT_cu)[
    str_detect(names(fwT_cu), paste0("^", indicator, ".*_0$"))
  ]

  # Pivot to long; add historical temperature and a decade interval
  fwT_cu_long <- fwT_cu %>%
    dplyr::mutate(histT = !!rlang::sym(hist_col)) %>%     # add historical model column
    tidyr::pivot_longer(
      cols = dplyr::matches(paste0("^(", indicator_grep, ")_")),
      names_to = c(".value", "gcm", "rcp", "period_code"),
      names_pattern = paste0("^(", indicator_grep, ")_(\\d+)_(\\d+)_(\\d)$")
    ) %>%
    dplyr::select(
      dplyr::matches(paste0("(", indicator_grep, ")")),
      stream_id_key, length_metre, model_rs, model_access_salmon,
      model_habitat_salmon, gcm, rcp, period_code, histT
    ) %>%
    dplyr::filter(
      rcp %in% c(historical, RCP),
      period_code %in% period_codes
    ) %>%
    dplyr::mutate(
      # delta columns (kept for potential downstream use)
      dplyr::across(dplyr::contains(indicator_grep), ~ .x - histT, .names = "delta_{.col}"),
      decade_interval = decade_calc(historical, period_pick = period_code),
      gcm = as.integer(gcm),  # ensure numeric for joining/mapping
      rcp = as.integer(rcp)
    )
  
  # CU-level, length-weighted stats BY GCM (no averaging across GCMs)
  Ts_stats <- fwT_cu_long %>%
    dplyr::filter(!is.na(histT)) %>%
    dplyr::group_by(gcm, rcp, period_code) %>%
    dplyr::summarize(
      n_streams    = dplyr::n(),
      total_length = sum(length_metre, na.rm = TRUE),
      dsmodel      = dsmodel_name,
      
      dplyr::across(
        .cols = c(dplyr::all_of(indicator)),
        .fns = c(
          list(
            proj_mean    = ~ wmean(.x, length_metre, na.rm = TRUE),
            proj_qlowsp  = ~ wqt(.x, length_metre, prob = qlowsp,  na.rm = TRUE),
            proj_qhighsp = ~ wqt(.x, length_metre, prob = qhighsp, na.rm = TRUE),
            
            rate_mean    = ~ wmean((.x - histT) / decade_interval, length_metre, na.rm = TRUE),
            rate_qlowsp  = ~ wqt((.x - histT) / decade_interval, length_metre, prob = qlowsp,  na.rm = TRUE),
            rate_qhighsp = ~ wqt((.x - histT) / decade_interval, length_metre, prob = qhighsp, na.rm = TRUE)
          ),
          
          # NEW: proportional difference vs baseline: (proj - hist) / hist
          if (isTRUE(include_pdelta)) list(
            pdelta_mean = ~ wmean(
              dplyr::if_else(!is.na(histT) & abs(histT) > hist_zero_tol,
                             (.x - histT) / histT,
                             NA_real_),
              length_metre,
              na.rm = TRUE
            ),
            pdelta_qlowsp = ~ wqt(
              dplyr::if_else(!is.na(histT) & abs(histT) > hist_zero_tol,
                             (.x - histT) / histT,
                             NA_real_),
              length_metre,
              prob = qlowsp,
              na.rm = TRUE
            ),
            pdelta_qhighsp = ~ wqt(
              dplyr::if_else(!is.na(histT) & abs(histT) > hist_zero_tol,
                             (.x - histT) / histT,
                             NA_real_),
              length_metre,
              prob = qhighsp,
              na.rm = TRUE
            )
          ) else NULL
        ),
        .names = "{.col}{.fn}"
      ),
      
      .groups = "drop"
    )
  
  # ---- Add GCM name column ----
  if (is.null(gcm_names)) {
    gcm_map <- dplyr::tibble(
      gcm = sort(unique(Ts_stats$gcm)),
      gcm_name = paste0("GCM_", sort(unique(Ts_stats$gcm)))
    )
  } else {
    if (!is.null(names(gcm_names)) && any(nzchar(names(gcm_names)))) {
      gcm_map <- dplyr::tibble(
        gcm = as.integer(names(gcm_names)),
        gcm_name = unname(gcm_names)
      )
    } else {
      gcm_ids <- sort(unique(Ts_stats$gcm))
      if (length(gcm_names) != length(gcm_ids)) {
        warning("Length of 'gcm_names' does not match number of GCM ids; auto-labelling instead.")
        gcm_map <- dplyr::tibble(
          gcm = gcm_ids,
          gcm_name = paste0("GCM_", gcm_ids)
        )
      } else {
        gcm_map <- dplyr::tibble(
          gcm = gcm_ids,
          gcm_name = gcm_names
        )
      }
    }
  }
  
  Ts_stats <- Ts_stats %>%
    dplyr::left_join(gcm_map, by = "gcm") %>%
    dplyr::relocate(gcm_name, .after = gcm)
  
  return(Ts_stats)
}



# Returns wide output with columns like <prefix>_qlowgcm, <prefix>_qhighgcm
compute_gcm_quantiles_wide <- function(df,
                                       group_keys = c("FULL_CU_IN", "rcp", "period_code", "dsmodel"),
                                       low_prob = qlgcm,
                                       high_prob = qhgcm) {
  # Identify all *_mean columns
  mean_cols <- grep("_mean$", names(df), value = TRUE)
  if (length(mean_cols) == 0) {
    stop("No columns ending with '_mean' found.")
  }
  
  df %>%
    group_by(across(all_of(group_keys))) %>%
    summarize(
      across(
        .cols = all_of(mean_cols),
        .fns  = list(
          qlowgcm  = ~ quantile(.x, probs = low_prob,  na.rm = TRUE),
          qhighgcm = ~ quantile(.x, probs = high_prob, na.rm = TRUE)
        ),
        .names = "{.col}_{.fn}"
      ),
      .groups = "drop"
    ) %>%
    # Optional: rename <prefix>_mean_qlowgcm -> <prefix>_qlowgcm (drop the _mean)
    rename_with(
      ~ sub("_mean_(qlowgcm|qhighgcm)$", "_\\1", .x),
      .cols = matches("_mean_(qlowgcm|qhighgcm)$")
    )
}


#----3.3 Cumulative threat stats function ----
stream_ct_stats <- function(fwct_cu,
                            ct_prefix = "cthr",
                            qlowsp = qlsp,
                            qhighsp = qhsp,
                            dsmodel_name = "cumulthreat",
                            model_rs = TRUE) {
  
  if (model_rs == TRUE) fwct_cu <- fwct_cu[fwct_cu$model_rs == TRUE, ]
  
  fwct_cu_long <- pivot_longer(fwct_cu,
                               cols = contains(ct_prefix),
                               names_to = c("CT_type")) %>%
    filter(!is.na(value))
  
  ct_stats <- fwct_cu_long %>%
    filter(!is.na(value)) %>%
    group_by(CT_type) %>%
    summarise(
      period_code = 0,
      gcm = 0,
      rcp = 0,
      dsmodel = dsmodel_name,
      n_streams = n(),
      total_length = sum(length_metre, na.rm = TRUE),
      cthr_mean = wmean(value, length_metre, na.rm = TRUE),
      cthr_qlowsp = wqt(value, length_metre, prob = qlowsp, na.rm = TRUE),
      cthr_qhighsp = wqt(value, length_metre, prob = qhighsp, na.rm = TRUE),
      .groups = "drop"
    )
  
  return(ct_stats)
}

#---- 3.4 ENM statistics function----
ENM_stats <- function(ENM_cu,
                      ENM_sp,
                      historical = historical_code,
                      qlowsp = qlsp,
                      qhighsp   = qhsp,
                      dsmodel_name = "ENM",
                      accessible_only = TRUE) {
  
  ENM_cols <- grep(paste0("fav_", ENM_sp), names(ENM_cu), value = TRUE)
  
  if (accessible_only == TRUE) ENM_cu <- ENM_cu[ENM_cu$model_access_salmon %in% c("INFERRED", "OBSERVED"), ]
  
  ENM_cu_long <- ENM_cu %>%
    select(ENM_cols, length_metre, stream_id_key) %>%
    filter(!is.na(.data[[ENM_cols[1]]])) %>%   # remove rows with missing ENM values
    pivot_longer(cols = all_of(ENM_cols),
                 names_to = c(".value", "rcp", "period"),
                 names_pattern = "^(.*)_(\\w{2,})_(\\d)$") %>%
    rename_with(~ gsub(paste0("_", ENM_sp), "", .x, fixed = TRUE)) %>%
    arrange(stream_id_key, rcp, period) %>%
    mutate(hist = if_else(period == 0, fav, NA)) %>%  # historical fav
    fill(hist, .direction = "up") %>%
    mutate(long_favchange = fav - hist,
           rcp = if_else(rcp == "hist", "0", rcp))  #adjust hist to 0 in rcp column for consistency
  
  # calculate mean and other statistics from single column
  ENM_stats <- ENM_cu_long %>%
    group_by(rcp, period) %>%
    summarise(
      dsmodel = dsmodel_name,
      n_streams = n(),
      total_length = sum(length_metre, na.rm = TRUE),
      favchange_mean = wmean(long_favchange, length_metre, na.rm = TRUE),
      favchange_qlowsp = wqt(long_favchange, length_metre, prob = qlowsp, na.rm = TRUE),
      favchange_qhighsp = wqt(long_favchange, length_metre, prob = qhighsp, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    rename(period_code = period)
  
  return(ENM_stats)
}

#---- 3.5 station low flow stats function ----

station_lowflow_stats <- function(wp_cu,
                                  qlowsp = qlsp,
                                  qhighsp = qhsp,
                                  dsmodel_name = "station",
                                  indicator = "flow8",
                                  historical = historical_code) {
  
  wp_cu <- wp_cu %>%
    mutate(mean_hist = ifelse(period == historical, mean, NA)) %>%
    fill(mean_hist, .direction = "down") %>%
    mutate(period = as.character(period),
           rcp = str_sub(experiment_id, -2, -1)) %>%
    mutate(rcp = if_else(rcp == "al", "0", rcp))
    
  
  wp_i <- wp_cu %>%
    group_by(rcp, period) %>%
    summarise(n_stations = n(),
              st8proj_mean = mean(mean),
              st8proj_qlowgcm = mean(qlow),
              st8proj_qhighgcm = mean(qhigh),
              st8pdelta_mean     = mean((mean - mean_hist) / mean_hist),
              st8pdelta_qlowsp   = unname(quantile((mean - mean_hist) / mean_hist, probs = qlowsp)),
              st8pdelta_qhighsp   = unname(quantile((mean - mean_hist) / mean_hist, probs = qhighsp)),
              st8pdelta_qlowgcm = mean((qlow - mean_hist) / mean_hist),
              st8pdelta_qhighgcm = mean((qhigh - mean_hist) / mean_hist),
              .groups = "drop") %>%
    rename(period_code = period) %>%
    mutate(gcm = "9",                #add gcm code for ensemble
           dsmodel = dsmodel_name,
           .after = period_code)   
  
  return(wp_i)
}

station_lowflow_stats <- function(wp_cu,
                                  qlowsp = qlsp,
                                  qhighsp = qhsp,
                                  dsmodel_name = "station",
                                  indicator = "flow8",
                                  historical = historical_code) {
  
  # small helper to build names: e.g., "flow8" + "proj_mean" => "flow8proj_mean"
  nm <- function(suffix) paste0(indicator, suffix)
  
  wp_cu <- wp_cu %>%
    mutate(mean_hist = ifelse(period == historical, mean, NA)) %>%
    tidyr::fill(mean_hist, .direction = "down") %>%
    mutate(
      period = as.character(period),
      rcp = stringr::str_sub(experiment_id, -2, -1),
      rcp = dplyr::if_else(rcp == "al", "0", rcp)
    )
  
  wp_i <- wp_cu %>%
    dplyr::group_by(rcp, period) %>%
    dplyr::summarise(
      n_stations = dplyr::n(),
      !!nm("proj_mean")        := mean(mean),
      !!nm("proj_qlowgcm")     := mean(qlow),
      !!nm("proj_qhighgcm")    := mean(qhigh),
      !!nm("pdelta_mean")      := mean((mean - mean_hist) / mean_hist),
      !!nm("pdelta_qlowsp")    := unname(stats::quantile((mean - mean_hist) / mean_hist, probs = qlowsp)),
      !!nm("pdelta_qhighsp")   := unname(stats::quantile((mean - mean_hist) / mean_hist, probs = qhighsp)),
      !!nm("pdelta_qlowgcm")   := mean((qlow - mean_hist) / mean_hist),
      !!nm("pdelta_qhighgcm")  := mean((qhigh - mean_hist) / mean_hist),
      .groups = "drop"
    ) %>%
    dplyr::rename(period_code = period) %>%
    dplyr::mutate(
      gcm = "9",              # add gcm code for ensemble
      dsmodel = dsmodel_name,
      .after = period_code
    )
  
  return(wp_i)
}


# get proportion of hydrologic regime type for watersheds within the CU boundary
regime_stats <- function(watershed_flow, cu_boundary_i) {
  
  st_agr(cu_boundary_i) <- "constant"  # suppress warnings about constant attribute assignment
  st_agr(watershed_flow) <- "constant"
  
  # Calculate intersection - shared portion of x and y
  intersection_all <- st_intersection(cu_boundary_i, watershed_flow)
  # combine into single geometry to avoid double-counting overlapping watershed areas
  union_all        <- st_union(intersection_all)
  
  # repeat for each regime type
  intersection_snow <- st_intersection(cu_boundary_i, filter(watershed_flow, regime == "Snowfall"))
  union_snow        <- st_union(intersection_snow)
  intersection_rain <- st_intersection(cu_boundary_i, filter(watershed_flow, regime == "Rainfall"))
  union_rain        <- st_union(intersection_rain)
  intersection_glac <- st_intersection(cu_boundary_i, filter(watershed_flow, regime == "Glacial"))
  union_glac        <- st_union(intersection_glac)
  intersection_hybr <- st_intersection(cu_boundary_i, filter(watershed_flow, regime == "Hybrid"))
  union_hybr        <- st_union(intersection_hybr)
  
  # Calculate areas
  cu_area     <- st_area(cu_boundary_i)    # area of cu boundary
  area_gauged <- sum(st_area(union_all))  # area within cu of gauged watersheds
  prop_cover  <- area_gauged / cu_area    # proportion of cu boundary with watersheds classified to regime
  
  area_snow <- sum(st_area(union_snow))
  area_rain <- sum(st_area(union_rain))
  area_glac <- sum(st_area(union_glac))
  area_hybr <- sum(st_area(union_hybr))
  
  # total area of regime classifications
  # this may be greater than area gauged where you have a watershed assigned to multiple regimes from different gauges
  area_regimes <- sum(area_snow, area_rain, area_glac, area_hybr)
  
  prop_snow <- area_snow / area_regimes
  prop_rain <- area_rain / area_regimes
  prop_glac <- area_glac / area_regimes
  prop_hybr <- area_hybr / area_regimes
  
  regime <- tribble(
    ~prop_snow, ~prop_rain, ~prop_hybrid, ~prop_glacial, ~prop_coverage,
    prop_snow, prop_rain, prop_hybr, prop_glac, prop_cover)
  
}

# ---- 4. Calculate stream network CU indicators----

for (i in 1:n.CUs) {
  #---- a. subset CU data ----
  cu_i <- cu_run$FULL_CU_IN[i]
  sp_pick <- cu_run$SPECIES_NAME[cu_run$FULL_CU_IN == cu_i] # species abbr
  sp_pick_bcfp <- spp_lookup$spp_abr_bcfp[spp_lookup$SPECIES_NAME == sp_pick]  # BCFP species abbr (different for Chinook)
  sp_pick_ENM  <- str_to_lower(sp_pick)
  # get model_spawning and model_rearing columns for CU species
  model_h_pick <- paste0("model_habitat_", sp_pick_bcfp)
  model_r_pick <- paste0("model_rearing_", sp_pick_bcfp)
  model_s_pick <- paste0("model_spawning_", sp_pick_bcfp)
  
  # harrison downstream (Weaver) doesn't have any modelled sockeye habitat, so use any salmon habitat instead
  if (cu_i == "SEL-03-04") model_h_pick == "model_habitat_salmon"
  
  # Subset CU boundary
  cu_boundary_i <- cu_boundary[cu_boundary$FULL_CU_IN == cu_i, ]
  
  # pick column of stream indices to subset for CU
  stream_cu_sub <- stream_cu_picks[, colnames(stream_cu_picks) == cu_i]
  
  # subset nuseds observations
  nuseds_cu <- nuseds_Fr[nuseds_Fr$FULL_CU_IN == cu_i, ]
  
  # get cu FW timing info
  fw_timing_i <- cu_timing_Fr[cu_timing_Fr$FULL_CU_IN == cu_i, ] %>%
    select(oe_age, sp_peak, oe_peak, peak_sp_to_oe)
  
  #----create subsetted data tables for each model
  fw_models_cu <- fw_models[stream_cu_sub, ] %>%
    mutate(model_rs = if_any(all_of(model_h_pick), ~ . == TRUE),
           model_spawning = if_any(all_of(model_s_pick), ~ . == TRUE)) %>%
    mutate(model_rs = if_else(is.na(model_rs), FALSE, model_rs))
  
  if (sp_pick %in% c("ck", "co", "sk")) fw_models_cu <- mutate(fw_models_cu,
                                                               model_rearing = if_any(all_of(model_r_pick), ~ . == TRUE))
  
  # get flow stations within each CU boundary
  cu_cont <- st_contains(cu_boundary_i, stations_flow, sparse = T)
  cu_stations <- stations_flow[unlist(cu_cont), ]
  
  # subset flow stations within CU boundary and calculate statistics for each GCM
  wp_cu <- wp_vm %>%
    filter(ID %in% cu_stations$ID) %>%
    mutate(mean = map_dbl(data, ~ mean(.x$mean)),
           sd   = map_dbl(data, ~ sd(.x$mean)),
           qlow = map_dbl(data, ~ quantile(.x$mean,  probs = qlgcm)),
           qhigh = map_dbl(data, ~ quantile(.x$mean, probs = qhgcm)))
  
  # calculate mean, sd, and quantiles for each station within CU boundary across all GCMs
  wp_cu_ens <- wp_cu %>%
    nest(.by = c("ID", "experiment_id", "period")) %>%
    mutate(mean = map_dbl(data, ~ mean(.x$mean)),
           sd   = map_dbl(data, ~ sd(.x$mean)),
           qlow = map_dbl(data, ~ quantile(.x$mean, probs = qlgcm)),
           qhigh = map_dbl(data, ~ quantile(.x$mean, probs = qhgcm))) %>%
    mutate(source_id = "ensemble", .after = experiment_id)
  
  
  #---- b. run summary statistics functions ----
  
  # nuseds summary
  nu_i <- data.table(nuseds_cu) %>%
    summarise(
      nuseds_sites = n(),
      nuseds_indicator = sum(IS_INDICATOR == "Y"),
      nuseds_obs = sum(n, na.rm = TRUE),
      .groups = "drop")
  
  # watershed regime and gauge coverage
  reg_i <- regime_stats(watershed_flow, cu_boundary_i)
  
  # get stats using functions - these are static (no gcm/period variation)
  ss_i <- stream_BCFP_stats(fw_models_cu) %>%
    bind_cols(nu_i, reg_i, fw_timing_i) %>%
    mutate(FULL_CU_IN = cu_i, .before = 1)
  
  # Temperature stats - returns data with gcm, rcp, period_code
  fwT_i <- stream_env_stats(st_drop_geometry(fw_models_cu),
                             RCP = c("00", "45", "85"),
                             dsmodel_name = "tscapes",   # or Tav
                             indicator = "tw8",   
                             model_rs = TRUE,
                             include_pdelta = FALSE
  ) %>%
    mutate(FULL_CU_IN = cu_i, .before = 1)
  
  #get gcm quantiles and add to table
  gcm_qtiles <- compute_gcm_quantiles_wide(fwT_i)
  fwT_i <- fwT_i %>%
    left_join(gcm_qtiles, by = c("FULL_CU_IN", "rcp", "period_code", "dsmodel"))
  
  # proportional difference in flows during low flow month (August)
  fwQlow_i <- stream_env_stats(st_drop_geometry(fw_models_cu),
                             RCP = c("00", "45", "85"),
                             dsmodel_name = "streamdyn",   # or Tav
                             indicator = "flow8",  
                             model_rs = TRUE,
                             include_pdelta = TRUE
  ) %>%
    mutate(FULL_CU_IN = cu_i, .before = 1) %>% 
    select(
      -matches("proj"),
      -matches("rate")
    )
  
  #get gcm quantiles and add to table
  gcm_qtiles <- compute_gcm_quantiles_wide(fwQlow_i)
  fwQlow_i <- fwQlow_i %>%
    left_join(gcm_qtiles, by = c("FULL_CU_IN", "rcp", "period_code", "dsmodel"))
  
  # proportional difference in flows during winter months
  fwQhigh_i <- stream_env_stats(st_drop_geometry(fw_models_cu),
                             RCP = c("00", "45", "85"),
                             dsmodel_name = "streamdyn",   # or Tav
                             indicator = "flow18", 
                             model_rs = TRUE,
                             include_pdelta = TRUE
  ) %>%
    mutate(FULL_CU_IN = cu_i, .before = 1) %>% 
    select(
      -matches("proj"),
      -matches("rate")
    )
    
  #get gcm quantiles and add to table
  gcm_qtiles <- compute_gcm_quantiles_wide(fwQhigh_i)
  fwQhigh_i <- fwQhigh_i %>%
    left_join(gcm_qtiles, by = c("FULL_CU_IN", "rcp", "period_code", "dsmodel"))

    # Cumulative threats - static (no gcm/period variation)
  cthr_i <- stream_ct_stats(st_drop_geometry(fw_models_cu),
                          model_rs = TRUE) %>%
    mutate(FULL_CU_IN = cu_i, .before = 1)
  
  # ENM stats - returns data with rcp, period_code
  ENM_i <- ENM_stats(st_drop_geometry(fw_models_cu),
                     ENM_sp = sp_pick_ENM,
                     accessible_only = TRUE
  ) %>%
    mutate(FULL_CU_IN = cu_i, 
           gcm        = 9,
           .before = 1)
  
  # Station low flow stats - returns data with rcp, period_code
  wp_i <- station_lowflow_stats(wp_cu_ens,
                                historical = 0) %>%
    mutate(FULL_CU_IN = cu_i, .before = 1)
  
  
  ## join data sets together

  # ---- Keys and indicator regex ----
  join_keys <- c("FULL_CU_IN", "rcp", "gcm", "period_code", "dsmodel")
  indicator_re <- "^(flow8pdelta|flow18pdelta|tw8rate|tw8proj|favchange|cthr)"  # only indicator columns you want
  
  # ---- Helper: make sure keys exist and are comparable (character) ----
  std_keys <- function(df) {
    df %>%
      mutate(
        FULL_CU_IN  = as.character(.data$FULL_CU_IN),
        rcp         = if ("rcp" %in% names(.)) as.character(.data$rcp) else .data$rcp,
        gcm         = if ("gcm" %in% names(.)) as.character(.data$gcm) else .data$gcm,
        period_code = if ("period_code" %in% names(.)) as.character(.data$period_code) else .data$period_code
      )
  }
  
  full_join_coalesce <- function(x, y, by, suffix = c(".x", ".y")) {
    out <- dplyr::full_join(x, y, by = by, suffix = suffix)
    
    # overlapping columns (excluding keys)
    dup_base <- intersect(
      setdiff(names(x), by),
      setdiff(names(y), by)
    )
    
    for (nm in dup_base) {
      left  <- paste0(nm, suffix[1])
      right <- paste0(nm, suffix[2])
      
      # If either side missing (e.g., one df didn’t have it after select), skip safely
      if (!left %in% names(out) || !right %in% names(out)) next
      
      out[[nm]] <- dplyr::coalesce(out[[left]], out[[right]])
      out[[left]] <- NULL
      out[[right]] <- NULL
    }
    
    out
  }
  
  pick_cols <- function(df) dplyr::select(df, dplyr::any_of(join_keys), dplyr::matches(indicator_re))
  
  # ---- Standardize all your inputs ----
  fwT_i     <- std_keys(fwT_i)
  fwQlow_i  <- std_keys(fwQlow_i)
  fwQhigh_i <- std_keys(fwQhigh_i)
  ENM_i     <- std_keys(ENM_i)
  wp_i      <- std_keys(wp_i)
  cthr_i    <- std_keys(cthr_i) %>%
    filter(CT_type == paste0("cthr_", ct_type))  #filter to only include cumulative threat score 
  # cthr_i and wp_i are not included because they don't have these indicators
  
  
  fw_all_i <- list(fwT_i, fwQlow_i, fwQhigh_i, ENM_i, cthr_i, wp_i) |>
    purrr::map(~ .x |> pick_cols()) |>
    purrr::reduce(~ full_join_coalesce(.x, .y, by = join_keys))
  
  if(i == 1) {
    fw_all <- fw_all_i 
    ss_all <- ss_i
  }
  else  {
    fw_all <- bind_rows(fw_all, fw_all_i)
    ss_all <- bind_rows(ss_all, ss_i)
  }
  
  print(paste("Done stream stats for CU", cu_i))
  
}


## reshape to longer data frame
wanted_prefix <- indicator_re
fw_long <- fw_all %>%
  # Keep only the indicator families of interest AND the desired stat suffixes
  pivot_longer(
    cols = matches(paste0(wanted_prefix, ".*_(mean|qlowsp|qhighsp|qlowgcm|qhighgcm)$")),
    names_to = c("indicator", "stat"),
    names_pattern = "^(.*)_(mean|qlowsp|qhighsp|qlowgcm|qhighgcm)$",
    values_to = "value"
  ) %>%
  filter(!is.na(value))
#names(fwR_all_list) <- cu_run$FULL_CU_IN



#---- 6. Write files----

# Save both the list format (for backwards compatibility) and the combined data frame
save(fw_long, fw_all, ss_all, file = file.path(paths$fw, paste0(today, "_fw_rearing_indicators.Rdata")))
