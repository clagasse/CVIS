####
#
# 4c_indicator_sensitivity.R
#
# Part 1: Indicator Redundancy (Spearman Correlation)
# Part 2: Indicator Leverage (Jackknife / Leave-one-out rescoring)
#
####

#----1. Setup and Import----
library(here)
setwd(here())
source(file.path(here(), "code", "0_setup.R"))

# Load standardized indicator data
load(file.path(paths$output, "scoring_results.Rdata")) # loads all_std_long

# Parameters for Baseline Scenario (from 0_setup.R)
rcp_base <- sens_rcp_base
period_base <- sens_period_base
gcm_base <- sens_gcm_base

# Recreate the 'filled' indicator values for the baseline scenario
# Replicates 4a logic to handle indicators that are 'static' or fall back to ensemble
dat <- all_std_long %>%
    filter(dsmodel %in% dsmodel_baseline) %>%
    mutate(
        rcp = as.character(rcp),
        period_code = as.character(period_code),
        gcm = as.character(gcm)
    )

cat(paste0("Rows in dataset after filtering for baseline models: ", nrow(dat), "\n"))

# Get baseline projections (GCM 9, RCP 45, Period 3)
proj_tbl_base <- dat %>%
    filter(rcp == rcp_base, period_code == period_base, gcm == gcm_base) %>%
    select(FULL_CU_IN, SPECIES_NAME, CVIS_NAME, indicator, category, proj_value = std_value)

cat(paste0("Rows in proj_tbl_base: ", nrow(proj_tbl_base), "\n"))

# Static values (for indicators that don't change by scenario)
static_tbl <- dat %>%
    filter(gcm == "0", period_code == "0") %>%
    select(FULL_CU_IN, SPECIES_NAME, CVIS_NAME, indicator, category, static_value = std_value) %>%
    distinct()

cat(paste0("Rows in static_tbl: ", nrow(static_tbl), "\n"))

# Unique identifiers for CUs and indicators
inds_per_cu <- dat %>%
    distinct(FULL_CU_IN, SPECIES_NAME, CVIS_NAME, indicator, category)

# Combine for the final baseline state
baseline_vals <- inds_per_cu %>%
    left_join(proj_tbl_base, by = c("FULL_CU_IN", "SPECIES_NAME", "CVIS_NAME", "indicator", "category")) %>%
    left_join(static_tbl, by = c("FULL_CU_IN", "SPECIES_NAME", "CVIS_NAME", "indicator", "category")) %>%
    mutate(std_value = coalesce(proj_value, static_value)) %>%
    # Resolve duplication: pick mean score if multiple entries exist for same (CU, indicator)
    group_by(FULL_CU_IN, SPECIES_NAME, CVIS_NAME, indicator, category) %>%
    summarise(std_value = mean(std_value, na.rm = TRUE), .groups = "drop") %>%
    mutate(std_value = ifelse(is.nan(std_value), NA_real_, std_value))

cat(paste0("Rows in baseline_vals (unique keys): ", nrow(baseline_vals), "\n"))
rows_with_data <- sum(!is.na(baseline_vals$std_value))
cat(paste0("Rows in baseline_vals with non-NA values: ", rows_with_data, "\n"))

if (rows_with_data == 0) {
    stop("Error: No indicator data found for the baseline scenario. Check rcp/period/gcm codes.")
}

#----2. Redundancy and Collinearity Analysis----
cat("\nAnalyzing Indicator Redundancy (Collinearity)...\n")

# Pivot wide for correlation
dat_wide <- baseline_vals %>%
    select(FULL_CU_IN, indicator, std_value) %>%
    pivot_wider(names_from = indicator, values_from = std_value) %>%
    # Ensure ONLY numeric columns are kept for cor()
    select(where(is.numeric))

cat(paste0("Numeric indicator columns for correlation: ", ncol(dat_wide), "\n"))

if (ncol(dat_wide) < 2) {
    stop("Error: Fewer than 2 numeric indicators found across CUs. Cannot compute correlation.")
}

# Spearman correlation matrix
cor_matrix <- cor(dat_wide, method = "spearman", use = "pairwise.complete.obs")

# Identify highly correlated pairs (|rho| > 0.8)
cor_df <- as.data.frame(as.table(cor_matrix)) %>%
    filter(Var1 != Var2) %>%
    # Use a triangular filter to avoid double-counting
    filter(as.character(Var1) < as.character(Var2)) %>%
    filter(abs(Freq) > 0.8) %>%
    arrange(desc(abs(Freq))) %>%
    rename(indicator1 = Var1, indicator2 = Var2, rho = Freq)

#----3. Jackknife Leverage Analysis----
cat("\nPerforming Jackknife (Leave-one-out) Leverage Analysis...\n")

# Hinge weight function (as used in 4a)
hinge_weight <- function(s, t0 = 0.33, t1 = 0.66) {
    ifelse(s <= t0, 0, ifelse(s >= t1, 1, (s - t0) / (t1 - t0)))
}

# Scoring function (replicates 4a for a given data subset)
# Note: Results are on the raw 0-1 scale (or count scale for flags)
score_subset <- function(subset_dat) {
    subset_dat %>%
        group_by(FULL_CU_IN, SPECIES_NAME, CVIS_NAME) %>%
        summarise(
            # Category averages
            a_fwrs = mean(std_value[category == "fwrs"], na.rm = TRUE),
            a_migr = mean(std_value[category == "migr"], na.rm = TRUE),
            a_mar = mean(std_value[category == "mar"], na.rm = TRUE),
            a_dem = mean(std_value[category == "dem"], na.rm = TRUE),
            a_gen = mean(std_value[category == "gen"], na.rm = TRUE),

            # Category cube-root of mean of cubes
            c_fwrs = mean((std_value[category == "fwrs"])^3, na.rm = TRUE)^(1 / 3),
            c_migr = mean((std_value[category == "migr"])^3, na.rm = TRUE)^(1 / 3),
            c_mar = mean((std_value[category == "mar"])^3, na.rm = TRUE)^(1 / 3),
            c_dem = mean((std_value[category == "dem"])^3, na.rm = TRUE)^(1 / 3),
            c_gen = mean((std_value[category == "gen"])^3, na.rm = TRUE)^(1 / 3),

            # Red flag counts per category (soft counts)
            sf_fwrs = sum(hinge_weight(std_value[category == "fwrs"]), na.rm = TRUE),
            sf_migr = sum(hinge_weight(std_value[category == "migr"]), na.rm = TRUE),
            sf_mar = sum(hinge_weight(std_value[category == "mar"]), na.rm = TRUE),
            sf_dem = sum(hinge_weight(std_value[category == "dem"]), na.rm = TRUE),
            sf_gen = sum(hinge_weight(std_value[category == "gen"]), na.rm = TRUE),

            # Overall metrics
            avg_all = mean(std_value, na.rm = TRUE), # avg of all indicators
            catavg = mean(c(a_fwrs, a_migr, a_mar, a_dem, a_gen), na.rm = TRUE), # catavg
            avgcube = mean(c(c_fwrs, c_migr, c_mar, c_dem, c_gen), na.rm = TRUE),
            flagall = sum(hinge_weight(std_value), na.rm = TRUE),
            .groups = "drop"
        ) %>%
        pivot_longer(
            cols = -c(FULL_CU_IN, SPECIES_NAME, CVIS_NAME),
            names_to = "metric",
            values_to = "score"
        ) %>%
        mutate(score = ifelse(is.nan(score), NA_real_, score))
}

# Calculate actual baseline scores (using all indicators)
baseline_scores <- score_subset(baseline_vals) %>%
    rename(base_score = score)

# Jackknife Loop
unique_indicators <- sort(unique(baseline_vals$indicator))
jackknife_results <- list()

for (ind in unique_indicators) {
    cat(paste0("Excluding: ", ind, "...\n"))

    # Exclude one indicator and re-score
    subset_dat <- baseline_vals %>% filter(indicator != ind)
    jk_scores <- score_subset(subset_dat) %>%
        mutate(excluded_indicator = ind)

    jackknife_results[[ind]] <- jk_scores
}

jackknife_all <- bind_rows(jackknife_results)

# Calculate deviations from full-indicator baseline
jackknife_analysis <- jackknife_all %>%
    left_join(baseline_scores, by = c("FULL_CU_IN", "SPECIES_NAME", "CVIS_NAME", "metric")) %>%
    mutate(
        raw_dev = score - base_score,
        abs_dev = abs(raw_dev)
    )

# Summarize influence by indicator and metric (Relevant metrics only)
# Overall metrics are always relevant; category metrics are only relevant for indicators in that category
ind_cat_map <- baseline_vals %>%
    distinct(indicator, category) %>%
    rename(excluded_indicator = indicator, parent_category = category)

overall_metrics <- c("avg_all", "catavg", "avgcube", "flagall")

influence_summary <- jackknife_analysis %>%
    left_join(ind_cat_map, by = "excluded_indicator") %>%
    filter(
        metric %in% overall_metrics |
            str_detect(metric, parent_category)
    ) %>%
    group_by(excluded_indicator, parent_category, metric) %>%
    summarise(
        mean_abs_dev = mean(abs_dev, na.rm = TRUE),
        max_abs_dev = max(abs_dev, na.rm = TRUE),
        mean_raw_dev = mean(raw_dev, na.rm = TRUE),
        .groups = "drop"
    )

#----4. Save Outputs----

cat("\nSaving indicator sensitivity results...\n")

indicator_sensitivity_4c <- list(
    cor_matrix = cor_matrix,
    cor_pairs_high = cor_df,
    jackknife_results = jackknife_analysis,
    influence_summary = influence_summary
)

save(indicator_sensitivity_4c, file = file.path(paths$output, "indicator_sensitivity_4c.Rdata"))

cat("Script 4c complete. Results saved to indicator_sensitivity_4c.Rdata\n")
