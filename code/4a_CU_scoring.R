# ==============================================================================
# CVIS Core Vulnerability Scoring Engine (4a_CU_scoring.R)
#
# Description:
#   Aggregates all CVIS environmental, genetic, and demographic indicators
#   (Freshwater Spawning/Rearing, Upstream Migration, Nearshore Marine, Demographics,
#   and Genetics). Standardizes indicators to a 0-1 scale using scenario-specific
#   ranges and computes combined vulnerability scores and ranks (both regional
#   and species-specific).
#
# Workflow Steps:
#   1. Load setup environment and configure baseline ranges.
#   2. Load and prep indicators from all domains (FW, migration, marine, genetics, demographics).
#   3. Standardize indicator values using standard scaling functions.
#   4. Reconstruct static and projected indicator scenarios.
#   5. Calculate combined multi-scale scores (0-100 regional, 0-100 species) and ranks.
#   6. Save output datasets to output/.
#
# Inputs:
#   - processed_data/freshwater/fw_rearing_indicators.Rdata
#   - processed_data/freshwater/migr_stats.Rdata
#   - processed_data/marine/marine_stats.Rdata
#   - genetics and demographic tables (via 0_setup.R)
#
# Outputs:
#   - output/scoring_results.Rdata
#
# Dependencies:
#   - Requires indicators from FW, migration, and marine stages to be completed.
# ==============================================================================

# ==================== 1. Setup and Environment ====================
library(here)
setwd(here())
source(file.path(here(), "code", "0_setup.R"))

# ---- Baseline Settings for Scaling ----
# If you want to standardize scores and ranges relative to a specific baseline setup,
# set these variables. If NA, ranges will be calculated dynamically for each group.
scale_baseline_rcp <- NA # e.g. "45"
scale_baseline_period <- NA # e.g. "3"

# Threshold for station coverage overlap (exclude CUs below this for flow8pdelta)
min_station_coverage <- 0.1

#which variables are used when grouping CU indicator results for standardization?
# This determines what min-max range is applied when standardizing from 0 to 1
# default is to group separately across all scenarios and climate models
grouping_vars_pick <- c("gcm", "rcp", "period_code", "dsmodel")

# --- Load Data ---

# Freshwater
fw_file <- get_latest_file(paths$fw, "fw_rearing_indicators.Rdata")
load(fw_file) # loads fw_all, ss_all

# Exclude flow8pdelta station-model results for CUs with low station coverage
low_coverage_cus <- ss_all %>%
  filter(as.numeric(prop_coverage) < min_station_coverage) %>%
  pull(FULL_CU_IN)

if (length(low_coverage_cus) > 0) {
  cat("Excluding flow8pdelta station-model results for", length(low_coverage_cus), "CUs with station coverage <", min_station_coverage, ":\n")
  cat("  ", paste(low_coverage_cus, collapse = ", "), "\n")
  fw_all <- fw_all %>%
    filter(!(indicator == "flow8pdelta" & dsmodel == "station" & FULL_CU_IN %in% low_coverage_cus))
}


# Migration
migr_file <- get_latest_file(paths$fw, "migr_stats.Rdata")
load(migr_file) # loads migr_all, etc.

# Marine
mar_file <- get_latest_file(paths$marine, "marine_stats.Rdata") # look for .Rds
load(mar_file)

# Demographics (cu_long) is loaded via 1a_CU_import (sourced in 0_setup)
# Genetics (genetics_long) is loaded via 1f_genetics_import (sourced in 0_setup)

# Standardize columns for binding
cols_keep <- c("FULL_CU_IN", "gcm", "gcm_name", "dsmodel", "rcp", "period_code", "indicator", "stat", "value", "category")

# Helper to prep dataframes
prep_df <- function(df, model_default = "none") {
  if (!"gcm_name" %in% names(df)) df$gcm_name <- NA
  if (!"dsmodel" %in% names(df)) df$dsmodel <- model_default

  df %>%
    mutate(
      FULL_CU_IN = as.character(FULL_CU_IN),
      rcp = as.character(rcp),
      period_code = as.character(period_code), # ensure consistency
      gcm = as.character(gcm),
      value = as.numeric(value)
    ) %>%
    select(any_of(cols_keep))
}

fw_long <- prep_df(fw_all)
migr_long <- prep_df(migr_all)
mar_long <- prep_df(mar_all)
cu_long_prep <- prep_df(cu_long, model_default = "observed")
genetic_long_prep <- prep_df(genetics_long)


# Combine all data and filter according to settings
all_long <- bind_rows(cu_long_prep, genetic_long_prep, fw_long, migr_long, mar_long) %>%
  filter(!is.na(value)) %>%
  # Filter to relevant periods/rcps if needed, or keep all
  filter(period_code %in% periods_use) %>%
  # keep only selected cumulative threat type (ie. all)
  filter(!(str_detect(dsmodel, regex("cthr", ignore_case = TRUE)) & dsmodel != cthr_pick))

# Add species info
all_long <- select(cu_run, FULL_CU_IN, SPECIES_NAME, CVIS_NAME, CU_COMMON_NAME, SMU_SIMPLE) %>%
  left_join(all_long, by = "FULL_CU_IN")

calibration_input <- maz_all %>%
  filter(MAZ != "Offshore")  #remove offshore areas from the calibration data for MAZ, since it's not actually a MAZ


# ==================== 2. Calculate Standardized Scores ====================

all_std_long <- list()

# Loop through indicators in tbl_standardize
for (i in 1:nrow(tbl_standardize)) {
  # ind_abbrev = "SSTproj"
  ind_abbrev <- tbl_standardize$abbrev[i]
  std_params_i <- as.list(tbl_standardize[i, ])

  # Check if indicator exists in data
  if (nrow(filter(all_long, indicator == ind_abbrev)) == 0) {
    cat("  No data found for", ind_abbrev, "- skipping.\n")
    next
  }

  # 1. Run default standardization
  std_result_default <- standardize_long_indicator(
    data = all_long,
    calibration_data = calibration_input,
    grouping_vars = grouping_vars_pick,
    indicator_pick = ind_abbrev,
    std_fun = tbl_standardize$std_fun[i],
    std_params = std_params_i,
    calibration_gcm = "9", # Ensemble
    baseline_rcp = scale_baseline_rcp,
    baseline_period = scale_baseline_period
  )
  
  if (is.null(std_result_default)) next

  std_fun_default <- tbl_standardize$std_fun[i]

  if (std_fun_default %in% c("cat_std", "step_std", "enh_std")) {
    # Discrete/categorical: duplicate default to both options
    std_result <- bind_rows(
      std_result_default %>% mutate(std_method = "exponential"),
      std_result_default %>% mutate(std_method = "linear")
    )
  } else if (std_fun_default %in% c("exponential_std", "decay_std")) {
    # Default is exponential/decay -> alternative is linear
    std_exp <- std_result_default %>% mutate(std_method = "exponential")
    
    alt_fun <- if (std_fun_default == "exponential_std") "linear_std" else "invlinear_std"
    std_result_lin <- standardize_long_indicator(
      data = all_long,
      calibration_data = calibration_input,
      grouping_vars = grouping_vars_pick,
      indicator_pick = ind_abbrev,
      std_fun = alt_fun,
      std_params = std_params_i,
      calibration_gcm = "9",
      baseline_rcp = scale_baseline_rcp,
      baseline_period = scale_baseline_period
    )
    
    if (!is.null(std_result_lin)) {
      std_lin <- std_result_lin %>% mutate(std_method = "linear")
      std_result <- bind_rows(std_exp, std_lin)
    } else {
      std_result <- std_exp
    }
  } else if (std_fun_default %in% c("linear_std", "invlinear_std")) {
    # Default is linear/inverse-linear -> alternative is exponential
    std_lin <- std_result_default %>% mutate(std_method = "linear")
    
    alt_fun <- if (std_fun_default == "linear_std") "exponential_std" else "decay_std"
    alt_params <- std_params_i
    alt_params$lambda <- 3 # default lambda for alternative exponential risk curve
    
    std_result_exp <- standardize_long_indicator(
      data = all_long,
      calibration_data = calibration_input,
      grouping_vars = grouping_vars_pick,
      indicator_pick = ind_abbrev,
      std_fun = alt_fun,
      std_params = alt_params,
      calibration_gcm = "9",
      baseline_rcp = scale_baseline_rcp,
      baseline_period = scale_baseline_period
    )
    
    if (!is.null(std_result_exp)) {
      std_exp <- std_result_exp %>% mutate(std_method = "exponential")
      std_result <- bind_rows(std_exp, std_lin)
    } else {
      std_result <- std_lin
    }
  } else {
    # Fallback
    std_result <- bind_rows(
      std_result_default %>% mutate(std_method = "exponential"),
      std_result_default %>% mutate(std_method = "linear")
    )
  }

  all_std_long[[i]] <- std_result
}

all_std_long <- bind_rows(all_std_long)

# Define "mix" standardization method (using indicator-specific default curves)
mix_std_long <- all_std_long %>%
  left_join(tbl_indicators %>% select(indicator = abbrev, std_fun), by = "indicator") %>%
  mutate(default_method = if_else(std_fun %in% c("linear_std", "invlinear_std"), "linear", "exponential")) %>%
  filter(std_method == default_method) %>%
  mutate(std_method = "mix") %>%
  select(-std_fun, -default_method)

all_std_long <- bind_rows(all_std_long, mix_std_long)

# ==================== 3. Scoring and Ranks Across Indicators ====================

## first we need to make sure that indicators without projections (e.g. status)
# get applied when calculating scores for each rcp/gcm/scenario combination

# Keep only specific baseline models for each indicator as defined in tbl_standardize, and filter for mean stat
dat <- all_std_long %>%
  filter(stat == "mean") %>%
  left_join(tbl_standardize %>% select(abbrev, dsmodel_baseline_ind = dsmodel_baseline), by = c("indicator" = "abbrev")) %>%
  filter(dsmodel == dsmodel_baseline_ind)

# STATIC: indicators with only baseline rows (used as fallback for non projected indicators)
static_tbl <- dat %>%
  filter(gcm == 0, period_code == 0) %>%
  select(FULL_CU_IN, SPECIES_NAME, CVIS_NAME, CU_COMMON_NAME, SMU_SIMPLE, std_method, indicator, category, static_value = std_value) %>%
  distinct()

# PROJECTED: everything else (including baseline rows that also have projections, if any)
proj_tbl <- dat %>%
  filter(!(gcm == 0 & period_code == 0)) %>%
  filter(gcm %in% c("9", common_gcms)) %>% # filter individual gcm outputs and ensembles that are used across model
  select(FULL_CU_IN, SPECIES_NAME, CVIS_NAME, CU_COMMON_NAME, SMU_SIMPLE, std_method, gcm, rcp, period_code,
    indicator, category,
    proj_value = std_value
  )

# ENSEMBLE: Ensemble projections (GCM 9) used as preferred fallback for specific GCM projections
ensemble_tbl <- proj_tbl %>%
  filter(gcm == "9") %>%
  select(FULL_CU_IN, std_method, rcp, period_code, indicator, ensemble_value = proj_value)

# CU × GCM × RCP × PERIOD grid (from projections)
grid_cu_scen <- proj_tbl %>%
  distinct(FULL_CU_IN, SPECIES_NAME, CVIS_NAME, CU_COMMON_NAME, SMU_SIMPLE, std_method, gcm, rcp, period_code)

# Indicator list per CU (union of indicators seen anywhere — projected or static)
inds_per_cu <- dat %>%
  distinct(FULL_CU_IN, SPECIES_NAME, CVIS_NAME, CU_COMMON_NAME, SMU_SIMPLE, std_method, indicator, category)

# Expand grid with indicators for the same CU, excluding period_code 0
grid_expanded <- grid_cu_scen %>%
  filter(period_code != 0) %>%
  inner_join(inds_per_cu, by = c("FULL_CU_IN", "SPECIES_NAME", "CVIS_NAME", "CU_COMMON_NAME", "SMU_SIMPLE", "std_method"))

vals <- grid_expanded %>%
  left_join(proj_tbl,
    by = c(
      "FULL_CU_IN", "SPECIES_NAME", "CVIS_NAME", "CU_COMMON_NAME", "SMU_SIMPLE", "std_method",
      "gcm", "rcp", "period_code", "indicator", "category"
    )
  ) %>%
  left_join(ensemble_tbl, by = c("FULL_CU_IN", "std_method", "rcp", "period_code", "indicator")) %>%
  left_join(static_tbl,
    by = c("FULL_CU_IN", "SPECIES_NAME", "CVIS_NAME", "CU_COMMON_NAME", "SMU_SIMPLE", "std_method", "indicator", "category")
  ) %>%
  mutate(std_value = dplyr::coalesce(proj_value, ensemble_value, static_value))


# now that we have an expanded grid of values covering all gcm/scenario/period combinations
# we calculate teh aggregated scores
scores_base <- vals %>%
  group_by(FULL_CU_IN, SPECIES_NAME, CVIS_NAME, CU_COMMON_NAME, SMU_SIMPLE, std_method, gcm, rcp, period_code) %>%
  calculate_combined_scores()

# Calculate combined 0-100 scores and ranks
scores_tidy <- scale_and_rank_scores(
  scores_base,
  scale_baseline_rcp = scale_baseline_rcp,
  scale_baseline_period = scale_baseline_period,
  group_vars = c("std_method", "gcm", "rcp", "period_code", "method", "category"),
  within_species = TRUE,
  rank_descending = FALSE
) %>%
  arrange(std_method, rcp, period_code, gcm, SPECIES_NAME, FULL_CU_IN, method, category)

# ==================== 4. Averaging Scores ====================

# 1) Score averages (from scores_tidy)
# scores_tidy has at least: rcp, period_code, method, category, score
score_avgs_tidy <- scores_tidy %>%
  group_by(rcp, period_code, method, category) %>%
  summarise(mean_value = mean(score, na.rm = TRUE), .groups = "drop")

# 2) Indicator averages (from all_std_long)
# all_std_long has at least: rcp, period_code, indicator, std_value
ind_avgs_tidy <- all_std_long %>%
  group_by(rcp, period_code, indicator) %>%
  summarise(mean_value = mean(std_value, na.rm = TRUE), .groups = "drop") %>%
  # Normalize to the same tidy shape: put indicator into 'category' and tag method
  mutate(
    measure_type = "indicator",
    method = "indicator",
    category = indicator
  ) %>%
  select(rcp, period_code, method, category, mean_value)


# ==================== 5. Save Outputs ====================

# Make baseline scenario filtered version of all_std_long and scores_tidy
# Sourced from tbl_indicators for default std_method per indicator
all_std_long_baseline <- all_std_long %>%
  filter(std_method == std_method_base) %>%
  left_join(tbl_standardize %>% select(indicator = abbrev, dsmodel_baseline_ind = dsmodel_baseline), by = "indicator") %>%
  filter(
    is.na(dsmodel) | dsmodel == dsmodel_baseline_ind,
    period_code %in% c("0", sens_period_base),
    rcp %in% c("0", sens_rcp_base),
    gcm %in% c("0", sens_gcm_base)
  ) %>%
  select(-dsmodel_baseline_ind) %>%
  group_by(indicator, FULL_CU_IN) %>%
  filter(!(any(period_code != "0") & period_code == "0")) %>%
  ungroup()


scores_tidy_baseline <- scores_tidy %>%
  filter(
    std_method == std_method_base,
    period_code %in% c("0", sens_period_base),
    rcp %in% c("0", sens_rcp_base),
    gcm %in% c("0", sens_gcm_base)
  )

# Save as R objects — primary output for all downstream scripts
save(all_std_long, scores_tidy, all_std_long_baseline, scores_tidy_baseline, file = file.path(paths$output, "scoring_results.Rdata"))
