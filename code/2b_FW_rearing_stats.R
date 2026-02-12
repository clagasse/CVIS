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

model_rs_pick <- case_when(
  fw_habitat_selection == "rs" ~ TRUE,
  fw_habitat_selection == "acc" ~ FALSE)


#---- 2. load spatial objects ----

# load CU stream selections - for subsetting when calculating statistics
load(file.path(paths$fw, "fw_streampicks_tscapes.Rdata"))

#load stream network object with model values and BC fishpass values
load(file.path(paths$fw, "fw_models_tscapes.Rds"))

# load statistical model projections of August flows for flow stations  (wp_vm)
load(file.path(paths$fw,  "Statistical_flow_projections.Rds"))

# load flow stations spatial objects  (watershed_flow, stations_flow, stations_stats)
load(file.path(paths$fw, "flow_gauge_data.Rdata"))


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


#---- 3.1 stream stats function----

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
    indicator = "tw8proj",      # <- full indicator name: "<base><kind>" e.g., "tw8proj"
    dsmodel_name = "tscapes",
    model_rs = model_rs_pick,
    qlowsp = qlsp,
    qhighsp = qhsp,
    hist_zero_tol = 0           # tolerance for pdelta denominator
) {
  
  # ---- Parse indicator into base + kind ----
  # Expect pattern "<base><kind>" where kind in {proj, rate, pdelta}
  m <- stringr::str_match(indicator, "^(.*?)(proj|rate|pdelta)$")
  if (is.na(m[1, 1])) {
    stop("`indicator` must end with one of: 'proj', 'rate', 'pdelta'. Example: 'tw8proj', 'tw8rate'.")
  }
  base_ind <- m[1, 2]  # e.g., "tw8"
  kind     <- m[1, 3]  # one of "proj", "rate", "pdelta"
  
  # ---- Optional filter for modelled RS habitat ----
  if (isTRUE(model_rs)) {
    fwT_cu <- fwT_cu[fwT_cu$model_rs == TRUE, ]
  }
  
  # ---- Identify the historical column for the base indicator (e.g., "tw8_..._*_0") ----
  hist_col <- names(fwT_cu)[stringr::str_detect(names(fwT_cu), paste0("^", base_ind, ".*_0$"))]
  if (length(hist_col) != 1L) {
    stop("Could not uniquely identify historical column for base indicator = '", base_ind, "'. ",
         "Found: ", paste(hist_col, collapse = ", "))
  }
  
  # ---- Pivot long for ONLY the base indicator, add hist, decade interval ----
  fwT_cu_long <- fwT_cu %>%
    dplyr::mutate(histT = !!rlang::sym(hist_col)) %>%
    tidyr::pivot_longer(
      cols = dplyr::matches(paste0("^(", base_ind, ")_")),
      names_to = c(".value", "gcm", "rcp", "period_code"),
      names_pattern = paste0("^(", base_ind, ")_(\\d+)_(\\d+)_(\\d)$")
    ) %>%
    dplyr::select(
      dplyr::matches(paste0("(^", base_ind, "$)")),
      stream_id_key, length_metre, model_rs, model_access_salmon,
      model_habitat_salmon, gcm, rcp, period_code, histT
    ) %>%
    dplyr::filter(
      rcp %in% c(historical, RCP),
      period_code %in% period_codes
    ) %>%
    dplyr::mutate(
      decade_interval = decade_calc(historical, period_pick = period_code),
      gcm = as.integer(gcm),
      rcp = as.integer(rcp)
    )
  
  # ---- Define the value to summarise based on the indicator 'kind' ----
  # proj   -> value = base indicator (e.g., tw8)
  # rate   -> value = (tw8 - histT) / decade_interval
  # pdelta -> value = (tw8 - histT) / histT (guarded)
  value_expr <- switch(
    kind,
    proj   = quote(.data[[base_ind]]),
    rate   = quote((.data[[base_ind]] - histT) / decade_interval),
    pdelta = quote(dplyr::if_else(!is.na(histT) & abs(histT) > hist_zero_tol,
                                  (.data[[base_ind]] - histT) / histT,
                                  NA_real_)),
    stop("Unrecognized indicator kind: ", kind)
  )
  
  # ---- Summarize directly to LONG rows (one row per stat) ----
  Ts_long <- fwT_cu_long %>%
    dplyr::filter(!is.na(histT)) %>%
    dplyr::group_by(gcm, rcp, period_code) %>%
    dplyr::reframe(
      dsmodel   = dsmodel_name,
      indicator = indicator,   # keep the full indicator label (e.g., "tw8rate")
      
      stat = c("nsegments", "length", "mean", "qlowsp", "qhighsp"),
      value = c(
        dplyr::n(),
        sum(length_metre, na.rm = TRUE),
        wmean(!!value_expr, length_metre, na.rm = TRUE),
        wqt(  !!value_expr, length_metre, prob = qlowsp,  na.rm = TRUE),
        wqt(  !!value_expr, length_metre, prob = qhighsp, na.rm = TRUE)
      )
    ) %>%
    dplyr::ungroup()
  
  return(Ts_long)
}


#helper to get qlowgcm and qhighgcm after getting stats
compute_gcm_quantiles_long <- function(
    df,
    group_keys       = c("FULL_CU_IN", "rcp", "period_code", "dsmodel", "indicator"),
    gcm_col          = c("auto", "gcm", "gcm_name"),
    ensemble_value   = 9,          # value used to tag the ensemble rows in the chosen gcm_col (e.g., 9 or "Ensemble")
    low_prob         = qlgcm,
    high_prob        = qhgcm,
    stat_mean_regex  = "mean$"     # which stat labels to treat as 'mean' to roll up across GCMs
) {
  # --- Determine which GCM identifier column to use ---
  gcm_col <- match.arg(gcm_col)
  has_gcm      <- "gcm" %in% names(df)
  has_gcm_name <- "gcm_name" %in% names(df)
  
  if (gcm_col == "auto") {
    if (has_gcm) {
      gcm_col <- "gcm"
    } else if (has_gcm_name) {
      gcm_col <- "gcm_name"
    } else {
      stop("Input `df` must have either a 'gcm' or 'gcm_name' column.")
    }
  } else {
    if (!gcm_col %in% names(df)) {
      stop("Requested `gcm_col = '", gcm_col, "'`, but that column is not present in `df`.")
    }
  }
  
  # --- Required columns check (with flexible GCM id) ---
  required_cols <- unique(c(group_keys, gcm_col, "stat", "value"))
  missing_cols  <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop("Input `df` is missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Keep only keys that actually exist (defensive)
  keys <- intersect(group_keys, names(df))
  
  # --- Filter to "mean-like" stats and exclude ensemble rows from inputs ---
  df_means <- df %>%
    dplyr::filter(stringr::str_detect(.data$stat, stat_mean_regex)) %>%
    dplyr::filter(.data[[gcm_col]] != ensemble_value)
  
  if (nrow(df_means) == 0L) {
    stop(
      "No per-GCM rows found for stats matching `", stat_mean_regex,
      "` after excluding ensemble rows where ", gcm_col, " == ", paste(ensemble_value, collapse = ", "),
      ". Ensure your long data has per-GCM 'mean' stats and the correct ensemble_value."
    )
  }
  
  # --- Compute quantiles across GCMs for each (keys + stat) combination ---
  q_long <- df_means %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(keys, "stat")))) %>%
    dplyr::summarise(
      mean     = mean(.data$value, na.rm = TRUE),
      qlowgcm  = stats::quantile(.data$value, probs = low_prob,  na.rm = TRUE),
      qhighgcm = stats::quantile(.data$value, probs = high_prob, na.rm = TRUE),
      .groups  = "drop"
    ) %>%
    tidyr::pivot_longer(
      cols      = c(mean, qlowgcm, qhighgcm),
      names_to  = "qname",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      stat = dplyr::case_when(
        qname == "qlowgcm"  ~ sub("mean$", "qlowgcm", .data$stat),
        qname == "qhighgcm" ~ sub("mean$", "qhighgcm", .data$stat),
        TRUE ~ .data$stat
      )
    ) %>%
    dplyr::select(-qname)
  
  # --- Attach the ensemble identifier in the chosen column ---
  # We preserve whichever id system the input used for GCMs.
  if (gcm_col == "gcm") {
    # Ensure a 'gcm' column set to ensemble_value; if gcm_name exists, set to NA to avoid mislabeling.
    q_long <- q_long %>%
      dplyr::mutate(
        gcm = as.integer(ensemble_value),
        gcm_name = if ("gcm_name" %in% names(df)) NA_character_ else NULL
      ) %>%
      dplyr::select(dplyr::all_of(keys), gcm, dplyr::any_of("gcm_name"), stat, value)
  } else { # gcm_col == "gcm_name"
    q_long <- q_long %>%
      dplyr::mutate(
        gcm_name = as.character(ensemble_value),
        gcm = if ("gcm" %in% names(df)) NA_integer_ else NULL
      ) %>%
      dplyr::select(dplyr::all_of(keys), dplyr::any_of("gcm"), gcm_name, stat, value)
  }
  
  # Order output in a stable, readable way
  q_long <- q_long %>%
    dplyr::arrange(dplyr::across(dplyr::all_of(keys)), .data$stat)
  
  return(q_long)
}


#----3.2 Cumulative threat stats function ----
stream_ct_stats <- function(
    fwct_cu,
    indicator    = "cthr",           # base substring to select columns (e.g., "cthr")
    type_col     = "CT_type",
    qlowsp       = qlsp,             # lower quantile for spatial variation
    qhighsp      = qhsp,             # upper quantile for spatial variation
    model_rs     = model_rs_pick
) {
  # Optional filter: only modelled RS habitat
  if (isTRUE(model_rs)) {
    fwct_cu <- fwct_cu[fwct_cu$model_rs == TRUE, ]
  }
  
  # Pivot the selected indicator columns (e.g., all columns containing "cthr")
  fwct_cu_long <- fwct_cu %>%
    tidyr::pivot_longer(
      cols = dplyr::contains(indicator),
      names_to  = type_col,
      values_to = "value"
    ) %>%
    dplyr::filter(!is.na(.data$value))
  
  # Summarise directly to LONG rows (ensure consistent lengths in reframe)
  ct_long <- fwct_cu_long %>%
    dplyr::group_by(.data$CT_type) %>%
    dplyr::reframe(
      # one row per stat
      stat = c("nsegments", "length", "mean", "qlowsp", "qhighsp"),
      value = c(
        dplyr::n(),
        sum(.data$length_metre, na.rm = TRUE),
        wmean(.data$value, .data$length_metre, na.rm = TRUE),
        wqt(  .data$value, .data$length_metre, prob = qlowsp,  na.rm = TRUE),
        wqt(  .data$value, .data$length_metre, prob = qhighsp, na.rm = TRUE)
      ),
      # replicate annotations to match length(stat)
      period_code = rep(0L, 5),
      gcm         = rep(0L, 5),
      rcp         = rep(0L, 5),
      dsmodel     = rep(dplyr::first(.data$CT_type), 5),  # <-- use CT_type as dsmodel
      indicator   = rep(indicator, 5)
    ) %>%
    dplyr::ungroup() %>%
    # Omit CT_type from final output
    dplyr::select(gcm, dsmodel, rcp, period_code, indicator, stat, value) %>%
    dplyr::arrange(.data$dsmodel, .data$stat)
  
  return(ct_long)
}

#---- 3.4 ENM statistics function----
ENM_stats <- function(ENM_cu,
                      ENM_sp,
                      historical = historical_code,
                      qlowsp = qlsp,
                      qhighsp = qhsp,
                      dsmodel_name = "ENM",
                      indicator = "favchange",
                      accessible_only = TRUE) {
  
  # Identify the ENM columns (e.g., "fav_<sp>_<rcp>_<period>")
  ENM_cols <- grep(paste0("fav_", ENM_sp), names(ENM_cu), value = TRUE)
  
  # Optionally subset to accessible segments
  if (isTRUE(accessible_only)) {
    ENM_cu <- dplyr::filter(ENM_cu, .data$model_access_salmon %in% c("INFERRED", "OBSERVED"))
  }
  
  # Long form and compute historical + change
  ENM_cu_long <- ENM_cu %>%
    dplyr::select(dplyr::all_of(ENM_cols), length_metre, stream_id_key) %>%
    dplyr::filter(!is.na(.data[[ENM_cols[1]]])) %>%  # remove rows with missing ENM values
    tidyr::pivot_longer(
      cols = dplyr::all_of(ENM_cols),
      names_to = c(".value", "rcp", "period"),
      names_pattern = "^(.*)_(\\w{2,})_(\\d)$"
    ) %>%
    dplyr::rename_with(~ gsub(paste0("_", ENM_sp), "", .x, fixed = TRUE)) %>%
    dplyr::arrange(stream_id_key, rcp, period) %>%
    dplyr::mutate(
      hist = dplyr::if_else(period == 0, fav, NA_real_),  # historical fav
      rcp  = dplyr::if_else(rcp == "hist", "0", rcp)      # adjust hist to "0"
    ) %>%
    tidyr::fill(hist, .direction = "up") %>%
    dplyr::mutate(long_favchange = fav - hist)
  
  # Directly emit long rows: one row per stat per group
  ENM_long <- ENM_cu_long %>%
    dplyr::group_by(rcp, period) %>%
    dplyr::reframe(
      # fixed annotation columns repeated per stat row
      gcm       = "9",                 # ensemble code
      dsmodel   = dsmodel_name,
      indicator = indicator,
      # stat names and values aligned by position
      stat = c("nsegments", "length", "mean", "qlowsp", "qhighsp"),
      value = c(
        dplyr::n(),
        sum(length_metre, na.rm = TRUE),
        wmean(long_favchange, length_metre, na.rm = TRUE),
        wqt(long_favchange, length_metre, prob = qlowsp,  na.rm = TRUE),
        wqt(long_favchange, length_metre, prob = qhighsp, na.rm = TRUE)
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::rename(period_code = period) %>%
    dplyr::select(gcm, dsmodel, rcp, period_code, indicator, stat, value) %>%
    dplyr::arrange(rcp, period_code, stat)
  
  return(ENM_long)
}


#---- 3.5 station low flow stats function ----

station_lowflow_stats <- function(
    wp_cu,
    qlowsp = qlsp,
    qhighsp = qhsp,
    dsmodel_name = "station",
    indicator = "flow8proj",        # full indicator: "<base><kind>", e.g., "flow8proj" / "flow8rate" / "flow8pdelta"
    historical = historical_code,
    hist_zero_tol = 0,              # guard for pdelta denominator
    decade_calc_fun = decade_calc   # function to compute decade interval (historical, period_pick)
) {
  
  # If no input rows, return a zero-row tibble with the correct schema
  if (nrow(wp_cu) == 0L) {
    return(tibble::tibble(
      gcm_name    = character(),
      dsmodel     = character(),
      rcp         = character(),
      period_code = character(),
      indicator   = character(),
      stat        = character(),
      value       = numeric()
    ))
  }
  
  # ---- Parse indicator into base + kind ----
  # Expect: "<base><kind>" where kind ∈ {proj, rate, pdelta}
  m <- stringr::str_match(indicator, "^(.*?)(proj|rate|pdelta)$")
  if (is.na(m[1, 1])) {
    stop("`indicator` must end with one of: 'proj', 'rate', 'pdelta'. Example: 'flow8proj', 'flow8rate', 'flow8pdelta'.")
  }
  base_ind <- m[1, 2]  # e.g., "flow8"
  kind     <- m[1, 3]  # e.g., "proj"
  
  # ---- Prep: historical fill + rcp tidy ----
  wp_cu <- wp_cu %>%
    dplyr::mutate(mean_hist = ifelse(period == historical, mean, NA_real_)) %>%
    tidyr::fill(mean_hist, .direction = "down") %>%
    dplyr::mutate(
      period = as.character(period)
    )
  # ---- If 'rate' is requested, compute decade_interval ----
  if (identical(kind, "rate")) {
    if (!is.function(decade_calc_fun)) {
      stop("`kind = 'rate'` requires `decade_calc_fun`. Provide a function (e.g., `decade_calc`) or switch to 'proj'/'pdelta'.")
    }
    wp_cu <- wp_cu %>%
      dplyr::mutate(decade_interval = decade_calc_fun(historical, period_pick = period))
  }
  
  # ---- Build the value expression by kind ----
  value_expr <- switch(
    kind,
    proj = quote(mean),  # station-projected mean
    rate = quote((mean - mean_hist) / decade_interval),
    pdelta = quote(dplyr::if_else(!is.na(mean_hist) & abs(mean_hist) > hist_zero_tol,
                                  (mean - mean_hist) / mean_hist,
                                  NA_real_)),
    stop("Unrecognized kind: ", kind)
  )
  
  # ---- Labels for emitted rows ----
  stat_labels <- c("nsegments", "mean", "qlowsp", "qhighsp")
  n_stats <- length(stat_labels)
  
  # ---- Summarise directly to LONG rows (no wide→long churn) ----
  out_long <- wp_cu %>%
    dplyr::group_by(rcp, gcm_name, period) %>%
    dplyr::reframe(
      # values in the same order as stat_labels
      value = c(
        dplyr::n(),                                            # nsegments
        mean(!!value_expr, na.rm = TRUE),                      # mean (by kind)
        unname(stats::quantile(!!value_expr, probs = qlowsp,  na.rm = TRUE)),  # qlowsp
        unname(stats::quantile(!!value_expr, probs = qhighsp, na.rm = TRUE))   # qhighsp
      ),
      stat = stat_labels,
      
      # replicate annotation columns to the same length as `stat`
      period_code = rep(as.integer(dplyr::first(period)), n_stats),
      rcp         = rep(as.integer(dplyr::first(rcp)),    n_stats),
      gcm_name    = rep(dplyr::first(gcm_name),    n_stats),  # ensemble code
      dsmodel     = rep(dsmodel_name,                      n_stats),
      indicator   = rep(paste0(base_ind, kind),            n_stats)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(gcm_name, dsmodel, rcp, period_code, indicator, stat, value) %>%
    dplyr::arrange(rcp, gcm_name, dsmodel, period_code, stat)
  
  return(out_long)
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
    mutate(mean     = map_dbl(data, ~ mean(.x$mean)),
           sd       = map_dbl(data, ~ sd(.x$mean)),
           qlowvariant  = map_dbl(data, ~ quantile(.x$mean,  probs = qlgcm)),
           qhighvariant = map_dbl(data, ~ quantile(.x$mean, probs = qhgcm)))
  
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
  
  
  
  # 1) Stream env — tw8rate (per-GCM stats + ensemble quantiles)
  fwTrate_i <- stream_env_stats(
    st_drop_geometry(fw_models_cu),
    RCP = c("00", "45", "85"),
    dsmodel_name = "tscapes",
    indicator = "tw8rate",
    model_rs = TRUE
  ) %>%
    mutate(FULL_CU_IN = cu_i, .before = 1) %>%
    { d <- .; bind_rows(d, compute_gcm_quantiles_long(d)) } %>%
    dplyr::distinct(
      FULL_CU_IN, dsmodel, indicator, rcp, period_code, stat, gcm,
      .keep_all = TRUE
    )
  
  # 2) Stream env — tw8proj
  fwTproj_i <- stream_env_stats(
    st_drop_geometry(fw_models_cu),
    RCP = c("00", "45", "85"),
    dsmodel_name = "tscapes",
    indicator = "tw8proj",
    model_rs = TRUE
  ) %>%
    mutate(FULL_CU_IN = cu_i, .before = 1) %>%
    { d <- .; bind_rows(d, compute_gcm_quantiles_long(d)) } %>%
    dplyr::distinct(
      FULL_CU_IN, dsmodel, indicator, rcp, period_code, stat, gcm,
      .keep_all = TRUE
    )
  
  # proportional difference in flows during low flow month (August)
  fwQlow_i <- stream_env_stats(st_drop_geometry(fw_models_cu),
                               dsmodel_name = "streamdyn",   # or Tav
                               indicator = "flow8pdelta",  
                               model_rs = TRUE
  ) %>%
    mutate(FULL_CU_IN = cu_i, .before = 1) %>%
    bind_rows(compute_gcm_quantiles_long(.)) %>%
    dplyr::distinct(
      FULL_CU_IN, dsmodel, indicator, rcp, period_code, stat, gcm,
      .keep_all = TRUE
    )
  
  # proportional difference in flows during winter months
  fwQhigh_i <- stream_env_stats(st_drop_geometry(fw_models_cu),
                                dsmodel_name = "streamdyn",   # or Tav
                                indicator = "flow18pdelta", 
                                model_rs = TRUE
  ) %>%
    mutate(FULL_CU_IN = cu_i, .before = 1) %>%
    bind_rows(compute_gcm_quantiles_long(.)) %>%
    dplyr::distinct(
      FULL_CU_IN, dsmodel, indicator, rcp, period_code, stat, gcm,
      .keep_all = TRUE
    )
  
  # 3) ENM — favchange (per-GCM ensemble rows are computed from the long table)
  ENM_i <- ENM_stats(
    ENM_cu = st_drop_geometry(fw_models_cu),
    ENM_sp = sp_pick_ENM,
    dsmodel_name = "ENM",
    indicator = "favchange",
    accessible_only = TRUE
  ) %>%
    mutate(FULL_CU_IN = cu_i, .before = 1) 
  
  # 4) Cumulative threats — dsmodel = CT_type (no ensemble quantiles here)
  cthr_i <- stream_ct_stats(
    fwct_cu = st_drop_geometry(fw_models_cu),
    indicator = "cthr",
    model_rs = TRUE
  ) %>%
    mutate(FULL_CU_IN = cu_i, .before = 1)
  
  # Station low flow stats - returns data with rcp, period_code
  wp_i <- station_lowflow_stats(wp_cu,
                                indicator = "flow8pdelta") %>%
    mutate(FULL_CU_IN = cu_i, .before = 1) 
  if(nrow(wp_i) > 0) wp_i  <- wp_i %>%
    bind_rows(compute_gcm_quantiles_long(.,
                                         ensemble_value = "ensemble")) # %>%
    # filter(rcp %in% rcp_vec,
    #        gcm_name == "ensemble" | stat = )
  
  # 5) Stack them all
  fw_all_i <- stack_long_stats(
    fwTrate_i,
    fwTproj_i,  
    fwQlow_i,
    fwQhigh_i,
    wp_i,
    ENM_i,
    cthr_i
  )
  
  # add in gcm_names where missing
  fw_all_i <- fw_all_i %>%
    left_join(gcm_codes, by = "gcm") %>%
    mutate(gcm_name = coalesce(gcm_name.y, gcm_name.x), 
           .after = gcm) %>%
    select(-gcm_name.x, -gcm_name.y) %>%
    mutate(
      gcm = dplyr::if_else(
        str_to_lower(str_trim(gcm_name)) == "ensemble",
        9L,
        as.integer(gcm)
      )
    )
  
  if(i == 1) {
    fw_all <- fw_all_i 
    ss_all <- ss_i
  }  else  {
    fw_all <- bind_rows(fw_all, fw_all_i)
    ss_all <- bind_rows(ss_all, ss_i)
  }
  
  print(paste("Done stream stats for CU", cu_i))
  
}


# ## reshape to longer data frame
# wanted_prefix <- indicator_re
# fw_long <- fw_all %>%
#   # Keep only the indicator families of interest AND the desired stat suffixes
#   pivot_longer(
#     cols = matches(paste0(wanted_prefix, ".*_(mean|qlowsp|qhighsp|qlowgcm|qhighgcm)$")),
#     names_to = c("indicator", "stat"),
#     names_pattern = "^(.*)_(mean|qlowsp|qhighsp|qlowgcm|qhighgcm)$",
#     values_to = "value"
#   ) %>%
#   filter(!is.na(value))
# #names(fwR_all_list) <- cu_run$FULL_CU_IN



#---- 6. Write files----

# Save both the list format (for backwards compatibility) and the combined data frame
save(fw_all, ss_all, file = file.path(paths$fw, paste0(today, "_fw_rearing_indicators.Rdata")))
