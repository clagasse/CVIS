####
#
# 4a_CU_scoring.R
#
# 1 - get indicator values for each CU (Long Format)
# 2 - standardize indicators using ensemble-based ranges
# 3 - calculate combined scores and ranks
# 4 - export long and wide format results

#----1. Setup and import----
library(here)
setwd(here())
source(file.path(here(), "code", "0_setup.R"))

# ---- Baseline Settings for Scaling ----
# If you want to standardize scores and ranges relative to a specific baseline setup,
# set these variables. If NA, ranges will be calculated dynamically for each group.
scale_baseline_rcp <- NA # e.g. "45"
scale_baseline_period <- NA # e.g. "3"

grouping_vars_pick <- c("rcp", "period_code", "dsmodel")

# Helper to find latest file by pattern
get_latest_file <- function(path, pattern) {
  files <- list.files(path, pattern = pattern, full.names = TRUE)
  if (length(files) == 0) stop("No files found matching ", pattern)
  # filter out files starting with ~ (temp files)
  files <- files[!grepl("^~", basename(files))]
  file_info <- file.info(files)
  latest_file <- rownames(file_info)[which.max(file_info$mtime)]
  cat("Loading latest file:", basename(latest_file), "\n")
  return(latest_file)
}

# --- Load Data ---

# Freshwater
fw_file <- get_latest_file(paths$fw, "fw_rearing_indicators.Rdata")
load(fw_file) # loads fw_all, ss_all

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

calibration_input <- maz_all


#---- 2. Calculate standardized scores----

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

  # Standardize
  std_result <- standardize_long_indicator(
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

  all_std_long[[i]] <- std_result
}

all_std_long <- bind_rows(all_std_long)

test_mar <- filter(
  all_std_long, indicator == "SSTproj",
  rcp == "45", period_code == "3", dsmodel == "qdm"
)


# 3.  Scoring and ranks across indicators----

## first we need to make sure that indicators without projections (e.g. status)
# get applied when calculating scores for each rcp/gcm/scenario combination

# Keep only specific baseline models for each indicator as defined in tbl_standardize
dat <- all_std_long %>%
  left_join(tbl_standardize %>% select(abbrev, dsmodel_baseline_ind = dsmodel_baseline), by = c("indicator" = "abbrev")) %>%
  filter(dsmodel == dsmodel_baseline_ind)

# STATIC: indicators with only baseline rows (used as fallback for non projected indicators)
static_tbl <- dat %>%
  filter(gcm == 0, period_code == 0) %>%
  select(FULL_CU_IN, SPECIES_NAME, CVIS_NAME, CU_COMMON_NAME, SMU_SIMPLE, indicator, category, static_value = std_value) %>%
  distinct()

# PROJECTED: everything else (including baseline rows that also have projections, if any)
proj_tbl <- dat %>%
  filter(!(gcm == 0 & period_code == 0)) %>%
  filter(gcm %in% c("9", common_gcms)) %>% # filter individual gcm outputs and ensembles that are used across model
  select(FULL_CU_IN, SPECIES_NAME, CVIS_NAME, CU_COMMON_NAME, SMU_SIMPLE, gcm, rcp, period_code,
    indicator, category,
    proj_value = std_value
  )

# ENSEMBLE: Ensemble projections (GCM 9) used as preferred fallback for specific GCM projections
ensemble_tbl <- proj_tbl %>%
  filter(gcm == "9") %>%
  select(FULL_CU_IN, rcp, period_code, indicator, ensemble_value = proj_value)

# CU × GCM × RCP × PERIOD grid (from projections)
grid_cu_scen <- proj_tbl %>%
  distinct(FULL_CU_IN, SPECIES_NAME, CVIS_NAME, CU_COMMON_NAME, SMU_SIMPLE, gcm, rcp, period_code)

# Indicator list per CU (union of indicators seen anywhere — projected or static)
inds_per_cu <- dat %>%
  distinct(FULL_CU_IN, SPECIES_NAME, CVIS_NAME, CU_COMMON_NAME, SMU_SIMPLE, indicator, category)

# Expand grid with indicators for the same CU, excluding period_code 0
grid_expanded <- grid_cu_scen %>%
  filter(period_code != 0) %>%
  inner_join(inds_per_cu, by = c("FULL_CU_IN", "SPECIES_NAME", "CVIS_NAME", "CU_COMMON_NAME", "SMU_SIMPLE"))

vals <- grid_expanded %>%
  left_join(proj_tbl,
    by = c(
      "FULL_CU_IN", "SPECIES_NAME", "CVIS_NAME", "CU_COMMON_NAME", "SMU_SIMPLE",
      "gcm", "rcp", "period_code", "indicator", "category"
    )
  ) %>%
  left_join(ensemble_tbl, by = c("FULL_CU_IN", "rcp", "period_code", "indicator")) %>%
  left_join(static_tbl,
    by = c("FULL_CU_IN", "SPECIES_NAME", "CVIS_NAME", "CU_COMMON_NAME", "SMU_SIMPLE", "indicator", "category")
  ) %>%
  mutate(std_value = dplyr::coalesce(proj_value, ensemble_value, static_value))


# now that we have an expanded grid of values covering all gcm/scenario/period combinations
# we calculate teh aggregated scores
scores_base <- vals %>%
  group_by(FULL_CU_IN, SPECIES_NAME, CVIS_NAME, CU_COMMON_NAME, SMU_SIMPLE, gcm, rcp, period_code) %>%
  calculate_combined_scores()

# Helper for 0-100 scaling with NA-safe behavior
scale_0_100 <- function(x) {
  mn <- suppressWarnings(min(x, na.rm = TRUE))
  mx <- suppressWarnings(max(x, na.rm = TRUE))
  if (!is.finite(mn) || !is.finite(mx) || mx <= mn) {
    return(rep(NA_real_, length(x))) # constant or all-NA -> no scale
  }
  (x - mn) / (mx - mn) * 100
}

# Cross-species 0-100 within
score100_cross <- scores_base

if (!is.na(scale_baseline_rcp) && !is.na(scale_baseline_period)) {
  bounds_cross <- score100_cross %>%
    filter(rcp == scale_baseline_rcp, period_code == scale_baseline_period) %>%
    group_by(gcm, method, category) %>%
    summarise(
      mn = suppressWarnings(min(score, na.rm = TRUE)),
      mx = suppressWarnings(max(score, na.rm = TRUE)),
      .groups = "drop"
    )

  score100_cross <- score100_cross %>%
    left_join(bounds_cross, by = c("gcm", "method", "category")) %>%
    mutate(score100_all = if_else(!is.finite(mn) | !is.finite(mx) | mx <= mn, NA_real_, (score - mn) / (mx - mn) * 100)) %>%
    select(FULL_CU_IN, SPECIES_NAME, CVIS_NAME, CU_COMMON_NAME, SMU_SIMPLE, gcm, rcp, period_code, method, category, score100_all)
} else {
  score100_cross <- score100_cross %>%
    group_by(rcp, period_code, gcm, method, category) %>%
    mutate(score100_all = scale_0_100(score)) %>%
    ungroup() %>%
    select(FULL_CU_IN, SPECIES_NAME, CVIS_NAME, CU_COMMON_NAME, SMU_SIMPLE, gcm, rcp, period_code, method, category, score100_all)
}

# Within-species 0-100
score100_within <- scores_base

if (!is.na(scale_baseline_rcp) && !is.na(scale_baseline_period)) {
  bounds_within <- score100_within %>%
    filter(rcp == scale_baseline_rcp, period_code == scale_baseline_period) %>%
    group_by(SPECIES_NAME, gcm, method, category) %>%
    summarise(
      mn = suppressWarnings(min(score, na.rm = TRUE)),
      mx = suppressWarnings(max(score, na.rm = TRUE)),
      .groups = "drop"
    )

  score100_within <- score100_within %>%
    left_join(bounds_within, by = c("SPECIES_NAME", "gcm", "method", "category")) %>%
    mutate(score100_species = if_else(!is.finite(mn) | !is.finite(mx) | mx <= mn, NA_real_, (score - mn) / (mx - mn) * 100)) %>%
    select(FULL_CU_IN, SPECIES_NAME, CVIS_NAME, CU_COMMON_NAME, SMU_SIMPLE, gcm, rcp, period_code, method, category, score100_species)
} else {
  score100_within <- score100_within %>%
    group_by(SPECIES_NAME, rcp, period_code, gcm, method, category) %>%
    mutate(score100_species = scale_0_100(score)) %>%
    ungroup() %>%
    select(FULL_CU_IN, SPECIES_NAME, CVIS_NAME, CU_COMMON_NAME, SMU_SIMPLE, gcm, rcp, period_code, method, category, score100_species)
}


# ---- Existing ranks (keep if you still want them) ----
ranks_cross <- scores_base %>%
  group_by(rcp, period_code, gcm, method, category) %>%
  mutate(rankall = rank(score, ties.method = "average", na.last = "keep")) %>%
  ungroup() %>%
  select(FULL_CU_IN, SPECIES_NAME, CVIS_NAME, CU_COMMON_NAME, SMU_SIMPLE, gcm, rcp, period_code, method, category, rankall)

ranks_within <- scores_base %>%
  group_by(SPECIES_NAME, rcp, period_code, gcm, method, category) %>%
  mutate(rankspecies = rank(score, ties.method = "average", na.last = "keep")) %>%
  ungroup() %>%
  select(FULL_CU_IN, SPECIES_NAME, CVIS_NAME, CU_COMMON_NAME, SMU_SIMPLE, gcm, rcp, period_code, method, category, rankspecies)

# 3) Final tidy table with new 0–100 scores alongside ranks
scores_tidy <- scores_base %>%
  left_join(score100_cross,
    by = c("FULL_CU_IN", "SPECIES_NAME", "CVIS_NAME", "CU_COMMON_NAME", "SMU_SIMPLE", "gcm", "rcp", "period_code", "method", "category")
  ) %>%
  left_join(score100_within,
    by = c("FULL_CU_IN", "SPECIES_NAME", "CVIS_NAME", "CU_COMMON_NAME", "SMU_SIMPLE", "gcm", "rcp", "period_code", "method", "category")
  ) %>%
  left_join(ranks_cross,
    by = c("FULL_CU_IN", "SPECIES_NAME", "CVIS_NAME", "CU_COMMON_NAME", "SMU_SIMPLE", "gcm", "rcp", "period_code", "method", "category")
  ) %>%
  left_join(ranks_within,
    by = c("FULL_CU_IN", "SPECIES_NAME", "CVIS_NAME", "CU_COMMON_NAME", "SMU_SIMPLE", "gcm", "rcp", "period_code", "method", "category")
  ) %>%
  arrange(rcp, period_code, gcm, SPECIES_NAME, FULL_CU_IN, method, category)

# 4. Averaging scores ----

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


# 5. Save outputs ----

# Save as R objects — primary output for all downstream scripts
save(all_std_long, scores_tidy, file = file.path(paths$output, "scoring_results.Rdata"))
