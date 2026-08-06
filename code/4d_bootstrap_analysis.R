# ==============================================================================
# CVIS Bootstrap Uncertainty Analysis (4d_bootstrap_analysis.R)
#
# Description:
#   Conducts a bootstrap uncertainty analysis by simultaneously varying multiple
#   sources of uncertainty (GCMs, RCP emissions scenario, and downscaling methods)
#   through random sampling. Focuses on Period 3 (2041-2060), keeps scoring
#   method fixed to the default (overall: catavg, category: avg), uses the default
#   indicator standardization functions defined in 0_setup, and runs 100
#   iterations to quantify combined uncertainty, rank stability, and identify
#   CUs with robust vulnerability profiles.
#
# Workflow Steps:
#   1. Load setup environment and core scoring results.
#   2. Define uncertainty sampling space (GCMs, RCPs, Downscalers).
#   3. Perform bootstrapping loop (100 iterations):
#      - Randomly sample GCM, RCP, and Downscaling assumptions.
#      - Select correct standardized values using a key-based join.
#      - Calculate overall vulnerability scores (catavg) and ranks.
#   4. Compute summary statistics (Mean, SD, 90% Uncertainty Intervals).
#   5. Categorize CUs into Robustness Profiles (Robust High, Robust Low, Uncertain).
#   6. Run multi-way ANOVA to decompose uncertainty variance.
#   7. Save outputs (Rdata, CSVs, Figures).
#
# Inputs:
#   - output/scoring_results.Rdata
#   - Configuration tables (via 0_setup.R)
#
# Outputs:
#   - output/uncertainty_analysis_results.Rdata
#   - output/CU_robustness_summary.csv
#   - output/uncertainty_variance_decomposition.csv
#   - output/figures/uncertainty_analysis/*.png
#
# Dependencies:
#   - Requires 4a_CU_scoring.R to have been executed.
# ==============================================================================

# ==================== 1. Setup and Environment ====================
library(here)
setwd(here())
source(file.path(here(), "code", "0_setup.R"))

# Load outputs from 4a
load(file.path(paths$output, "scoring_results.Rdata")) # loads all_std_long, scores_tidy

# Create outputs directories
uncertainty_fig_path <- file.path(paths$figures, "uncertainty_analysis")
dir.create(uncertainty_fig_path, showWarnings = FALSE, recursive = TRUE)

cat("Starting Bootstrap Uncertainty Analysis...\n")

# # ==================== Bootstrap Options ====================
# Toggle these on/off to include additional sources of uncertainty.
# By default, they are set to FALSE to preserve the original climate-only bootstrap behavior.
opt_perturb_spawner_abundance <- FALSE
opt_perturb_thresholds        <- F
opt_random_aggregation        <- TRUE
opt_aggregation_methods       <- c("catavg", "avgcube", "flag") # Schemes to sample if opt_random_aggregation is TRUE

# Clean and format variables in the input dataset to character to prevent join mismatches
all_std_clean <- all_std_long %>%
  mutate(
    indicator = as.character(indicator),
    dsmodel = as.character(dsmodel),
    gcm = as.character(gcm),
    rcp = as.character(rcp),
    period_code = as.character(period_code)
  )

# Define defaults for each indicator based on tbl_standardize std_fun
# linear/invlinear default to "linear", all others default to "exponential"
tbl_defaults <- tbl_standardize %>%
  mutate(std_method = if_else(std_fun %in% c("linear_std", "invlinear_std"), "linear", "exponential")) %>%
  select(indicator = abbrev, std_method)

# Pre-calculate baseline ranges for continuous indicators (required for threshold perturbation)
tbl_defaults_full <- tbl_standardize %>%
  mutate(std_method_default = if_else(std_fun %in% c("linear_std", "invlinear_std"), "linear", "exponential")) %>%
  select(indicator = abbrev, std_fun, range_type, lambda, xmin_tbl = xmin, xmax_tbl = xmax, std_method_default)

baseline_data <- all_std_long %>%
  filter(stat == "mean") %>%
  mutate(
    indicator = as.character(indicator),
    dsmodel = as.character(dsmodel),
    gcm = as.character(gcm),
    rcp = as.character(rcp),
    period_code = as.character(period_code),
    std_method = as.character(std_method)
  ) %>%
  left_join(tbl_defaults_full, by = "indicator") %>%
  filter(std_method == std_method_default) %>%
  left_join(tbl_standardize %>% select(abbrev, dsmodel_baseline), by = c("indicator" = "abbrev")) %>%
  filter(
    (indicator %in% c("cthr", "fwres", "migrdist", "CImpact", "CUstatus", "CUnmat", "hetzyg") & gcm == "0" & rcp == "0" & period_code == "0") |
    (!(indicator %in% c("cthr", "fwres", "migrdist", "CImpact", "CUstatus", "CUnmat", "hetzyg")) & gcm == "9" & rcp == "85" & period_code == "5" & dsmodel == dsmodel_baseline)
  )

calib_ranges <- baseline_data %>%
  group_by(indicator, range_type, SPECIES_NAME, xmin_tbl, xmax_tbl) %>%
  summarise(
    min_val = suppressWarnings(min(value[is.finite(value)], na.rm = TRUE)),
    max_val = suppressWarnings(max(value[is.finite(value)], na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(
    xmin = if_else(!is.na(xmin_tbl), xmin_tbl, min_val),
    xmax = if_else(!is.na(xmax_tbl), xmax_tbl, max_val)
  )

# Re-standardization helper function
standardize_perturbed <- function(x, std_fun, xmin, xmax, lambda = 3) {
  if (is.na(xmin[1]) || is.na(xmax[1]) || any(xmax <= xmin)) return(rep(0.5, length(x)))
  
  z <- pmax(xmin, pmin(xmax, x))
  z_std <- (z - xmin) / (xmax - xmin)
  
  if (std_fun == "linear_std") {
    return(z_std)
  }
  if (std_fun == "invlinear_std") {
    return(1 - z_std)
  }
  if (std_fun == "exponential_std") {
    if (xmax[1] == xmin[1]) return(rep(0.5, length(x)))
    return((exp(lambda * z_std) / exp(lambda)) - (exp(-lambda) * (1 - z_std)))
  }
  if (std_fun == "decay_std") {
    return(exp(-lambda * z_std) - exp(-lambda) * z_std)
  }
  return(x) # Fallback for step_std/cat_std
}

# ==================== 2. Define Sampling Space ====================
N_iterations <- 500
target_period <- "3" # Only period 3 (2041-2060)

# Uncertainty Sources:
# 1. Climate Models (GCMs)
gcm_choices <- c("9", common_gcms) # c("1", "4", "6") (CanESM2, HadGEM2, MPI)
# 2. Emission Scenarios (RCPs)
rcp_choices <- c("45", "85")
# 3. Spawning/Rearing Temp Downscaling
ds_temp_choices <- c("pcicgrid", "tscapes")
# 4. Spawning/Rearing Flow Downscaling
ds_flow_choices <- c("station", "streamdyn")
# 5. Nearshore Marine SST Downscaling
ds_mar_choices <- c("qdm", "bccmssc")

# Set seed for reproducibility
set.seed(42)

# List to hold Bootstrap iteration outputs
boot_results_list <- vector("list", N_iterations)

# ==================== 3. Bootstrap Loop ====================
cat("Running", N_iterations, "bootstrap iterations for Period 3...\n")

for (k in 1:N_iterations) {
  # A. Randomly sample the uncertainty sources
  g_k <- sample(gcm_choices, 1)
  rcp_k <- sample(rcp_choices, 1)
  d_temp <- sample(ds_temp_choices, 1)
  d_flow <- sample(ds_flow_choices, 1)
  d_mar <- sample(ds_mar_choices, 1)
  
  # B. Build selection key table for this iteration
  # Note: Flow 'station' downscaler only has ensemble mean '9', so fall back to '9' if 'station' is chosen.
  g_flow <- if (d_flow == "station") "9" else g_k
  
  iter_keys <- tibble::tribble(
    ~indicator, ~dsmodel, ~gcm, ~rcp, ~period_code,
    "tw8rate",      d_temp,   g_k,  rcp_k, target_period,
    "tw8proj",      d_temp,   g_k,  rcp_k, target_period,
    "flow8pdelta",  d_flow,   g_flow, rcp_k, target_period,
    "flow18pdelta", d_flow,   g_flow, rcp_k, target_period,
    "migrTproj",    "pcicgrid",  g_k, rcp_k, target_period,
    "migrQpdelta",  "pcicgrid",  g_k, rcp_k, target_period,
    "SSTproj",      d_mar,    "9",  rcp_k, target_period,
    "SSTrate",      d_mar,    "9",  rcp_k, target_period,
    "favchange",    "ENM",    "9",  rcp_k, target_period,
    "genoff",       "observed", "9",  rcp_k, target_period,
    # Static baseline indicators (GCM 0, RCP 0, Period 0)
    "cthr",         "cthr_anad", "0", "0", "0",
    "fwres",        "observed", "0", "0", "0",
    "migrdist",     "observed", "0", "0", "0",
    "CImpact",      "CImpact",  "0", "0", "0",
    "CUstatus",     "observed", "0", "0", "0",
    "CUnmat",       "observed", "0", "0", "0",
    "hetzyg",       "observed", "0", "0", "0"
  ) %>%
    left_join(tbl_defaults, by = "indicator")
  
  # C. Extract and score
  res <- all_std_clean %>%
    inner_join(iter_keys, by = c("indicator", "dsmodel", "gcm", "rcp", "period_code", "std_method")) %>%
    filter(stat == "mean")

  # D. Sample temperature threshold scenario (13 or 17 if enabled, default 15)
  temp_threshold_k <- if (opt_perturb_thresholds) sample(c(13, 17), 1) else 15

  # E. Conditionally apply spawner abundance and threshold perturbation
  if (opt_perturb_spawner_abundance || opt_perturb_thresholds) {
    res_perturbed <- list()
    for (ind in unique(res$indicator)) {
      ind_data <- res %>% filter(indicator == ind)
      
      std_info <- tbl_defaults_full %>% filter(indicator == ind)
      if (nrow(std_info) > 0 && std_info$std_fun %in% c("linear_std", "invlinear_std", "exponential_std", "decay_std")) {
        
        ind_data <- ind_data %>%
          left_join(calib_ranges %>% select(indicator, SPECIES_NAME, base_xmin = xmin, base_xmax = xmax), by = c("indicator", "SPECIES_NAME"))
        
        # Determine xmin_k and xmax_k
        if (opt_perturb_thresholds && ind %in% c("tw8proj", "migrTproj")) {
          ind_data <- ind_data %>%
            mutate(
              range_val = base_xmax - base_xmin,
              xmin_k = temp_threshold_k,
              xmax_k = base_xmax,
              xmax_k = if_else(xmax_k <= xmin_k, xmin_k + 0.1, xmax_k)
            )
        } else {
          ind_data <- ind_data %>%
            mutate(xmin_k = base_xmin, xmax_k = base_xmax)
        }
        
        # Perturb raw values for CUnmat spawner count
        if (ind == "CUnmat" && opt_perturb_spawner_abundance) {
          ind_data <- ind_data %>%
            mutate(value = value * exp(rnorm(n(), 0, 0.25)))
        }
        
        # Re-standardize
        lambda_val <- ifelse(is.na(std_info$lambda), 3, std_info$lambda)
        ind_data <- ind_data %>%
          mutate(
            std_value = standardize_perturbed(value, std_info$std_fun, xmin_k, xmax_k, lambda_val)
          ) %>%
          select(-base_xmin, -base_xmax, -xmin_k, -xmax_k)
        
        if ("range_val" %in% names(ind_data)) {
          ind_data <- ind_data %>% select(-range_val)
        }
      }
      
      res_perturbed[[ind]] <- ind_data
    }
    res <- bind_rows(res_perturbed)
  }
  
  # Calculate portfolio scores
  scores_iter_raw <- res %>%
    group_by(FULL_CU_IN, SPECIES_NAME, CVIS_LABEL, CU_COMMON_NAME, SMU_SIMPLE) %>%
    calculate_combined_scores()
  
  if (opt_random_aggregation) {
    # Sample overall and category methods
    agg_choices <- opt_aggregation_methods
    sampled_agg <- sample(agg_choices, 1)
    
    # Map overall to category method
    cat_method_map <- c("catavg" = "avg", "avgcube" = "cube", "flag" = "flag")
    sampled_cat_method <- cat_method_map[sampled_agg]
    
    scores_iter <- scores_iter_raw %>%
      filter(
        (category == "all" & method == sampled_agg) |
        (category != "all" & method == sampled_cat_method)
      ) %>%
      mutate(
        method = if_else(category == "all", "catavg", "avg")
      )
  } else {
    scores_iter <- scores_iter_raw %>%
      filter(
        (category == "all" & method == "catavg") |
        (category != "all" & method == "avg")
      )
  }
  
  # Scale to 0-100 and compute ranks across CUs
  scores_scaled <- scale_and_rank_scores(
    scores_iter,
    group_vars = "category",
    within_species = FALSE, 
    rank_descending = TRUE
  ) %>%
    rename(score100 = score100_all, rank_val = rankall)
  
  # Append metadata of assumptions
  scores_scaled <- scores_scaled %>%
    mutate(
      iteration = k,
      sampled_gcm = g_k,
      sampled_rcp = rcp_k,
      sampled_ds_temp = d_temp,
      sampled_ds_flow = d_flow,
      sampled_ds_mar = d_mar,
      sampled_aggregation = if (opt_random_aggregation) sampled_agg else "catavg",
      sampled_temp_threshold = as.character(temp_threshold_k)
    )
  
  boot_results_list[[k]] <- scores_scaled
}

mc_results <- bind_rows(boot_results_list)
cat("Bootstrap simulation completed.\n")

# ==================== 4. Statistical Summary & Robustness ====================
cat("Summarizing uncertainty results...\n")

# Filter for overall vulnerability (category == "all")
all_scores <- mc_results %>% filter(category == "all")

# Compute summary statistics of score and rank distributions for each CU
cu_summary <- all_scores %>%
  group_by(FULL_CU_IN, SPECIES_NAME, CVIS_LABEL, CU_COMMON_NAME, SMU_SIMPLE) %>%
  summarise(
    mean_score = mean(score100, na.rm = TRUE),
    sd_score = sd(score100, na.rm = TRUE),
    q5_score = quantile(score100, 0.05, na.rm = TRUE),
    q95_score = quantile(score100, 0.95, na.rm = TRUE),
    range_score = q95_score - q5_score,
    
    mean_rank = mean(rank_val, na.rm = TRUE),
    q5_rank = quantile(rank_val, 0.05, na.rm = TRUE),
    q95_rank = quantile(rank_val, 0.95, na.rm = TRUE),
    range_rank = q95_rank - q5_rank,
    .groups = "drop"
  )

# Classify CUs into Robustness Profiles:
#   - Robust High: Vulnerability score is consistently high (q5_score >= 50)
#   - Robust Low: Vulnerability score is consistently low (q95_score <= 35)
#   - Highly Uncertain: Large spread in score (range_score >= 25) or rank (range_rank >= 15)
#   - Moderate / Intermediate: All others
cu_summary <- cu_summary %>%
  mutate(
    robustness_profile = case_when(
      q5_score >= 50 ~ "Robust High",
      q95_score <= 35 ~ "Robust Low",
      range_score >= 25 | range_rank >= 15 ~ "Highly Uncertain",
      TRUE ~ "Intermediate / Moderate"
    )
  )

print(table(cu_summary$robustness_profile))

# ==================== 5. Variance Decomposition (ANOVA) ====================
cat("Performing variance decomposition using ANOVA...\n")

# Run multi-way ANOVA to partition uncertainty variance
# We control for the true spatial variation among CUs by including FULL_CU_IN
# Build ANOVA formula dynamically based on enabled options
formula_str <- "score100 ~ FULL_CU_IN + sampled_gcm + sampled_rcp + sampled_ds_temp + sampled_ds_flow + sampled_ds_mar"
unc_factors <- c("sampled_gcm", "sampled_rcp", "sampled_ds_temp", "sampled_ds_flow", "sampled_ds_mar")

if (opt_random_aggregation) {
  formula_str <- paste(formula_str, "+ sampled_aggregation")
  unc_factors <- c(unc_factors, "sampled_aggregation")
}

if (opt_perturb_thresholds) {
  formula_str <- paste(formula_str, "+ sampled_temp_threshold")
  unc_factors <- c(unc_factors, "sampled_temp_threshold")
}

lm_fit <- lm(as.formula(formula_str), data = all_scores)
anova_res <- anova(lm_fit)
anova_df <- as.data.frame(anova_res) %>%
  rownames_to_column("Source")

# Subset ANOVA table to target uncertainty factors and calculate relative contribution
anova_unc <- anova_df %>%
  filter(Source %in% unc_factors) %>%
  mutate(
    pct_uncertainty_variance = `Sum Sq` / sum(`Sum Sq`) * 100
  ) %>%
  mutate(
    Source_Label = case_when(
      Source == "sampled_gcm" ~ "Climate Model (GCM)",
      Source == "sampled_rcp" ~ "Emission Scenario (RCP)",
      Source == "sampled_ds_temp" ~ "Temperature Downscaling",
      Source == "sampled_ds_flow" ~ "Flow Downscaling",
      Source == "sampled_ds_mar" ~ "Marine Downscaling",
      Source == "sampled_aggregation" ~ "Aggregation Scheme",
      Source == "sampled_temp_threshold" ~ "Temperature Threshold (13 vs 17)",
      TRUE ~ Source
    )
  ) %>%
  arrange(desc(pct_uncertainty_variance))

print(as.data.frame(anova_unc[, c("Source_Label", "pct_uncertainty_variance")]))

# ==================== 6. Save Datasets ====================
cat("Saving data outputs...\n")

# Save R object for downstream analysis (keep variable name mc_results for backward compatibility)
save(mc_results, cu_summary, anova_unc, file = file.path(paths$output, "uncertainty_analysis_results.Rdata"))

cat("Script 4d complete. Analysis results successfully saved!\n")
