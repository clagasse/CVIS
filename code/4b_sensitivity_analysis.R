# ==============================================================================
# CVIS Sensitivity Analysis Engine (4b_sensitivity_analysis.R)
#
# Description:
#   Performs sensitivity analysis on the vulnerability scoring engine.
#   Quantifies deviations in vulnerability scores and rank stability (Mean Rank
#   Displacement) across GCMs, RCPs, periods, scoring methods, and downscaling variants.
#   Conducts a Jackknife (Leave-One-Out) leverage analysis to estimate the contribution
#   and influence of individual indicators and categories on overall results.
#
# Workflow Steps:
#   1. Load setup environment and core scoring results.
#   2. Compute baseline and scenario-specific indicator raw/standardized deviations.
#   3. Calculate overall vulnerability score deviations (GCM, RCP/Period, Method, and downscaler).
#   4. Conduct Jackknife loop to systematically omit indicators and categories and re-score.
#   5. Aggregate global and species-specific sensitivity metrics.
#   6. Save output datasets to output/.
#
# Inputs:
#   - output/scoring_results.Rdata
#   - Configuration tables (via 0_setup.R)
#
# Outputs:
#   - output/sensitivity_analysis.Rdata
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

# Filter scores_tidy for baseline score analysis (Sections 3 and 4)
scores_tidy_baseline <- scores_tidy
if ("std_method" %in% names(scores_tidy)) {
    scores_tidy_baseline <- scores_tidy %>% filter(std_method == "exponential")
}

# ==================== 2. Indicator-Level Metrics & Baseline ====================

# For each indicator/CU combination, calculate the mean raw and standardized value for the baseline scenario
# For environmental change indicators, also calculate the mean qlowgcm and qhighgcm for the baseline
ind_baseline <- all_std_long %>%
    filter(std_method == "exponential") %>%
    filter(rcp == sens_rcp_base, period_code == sens_period_base, gcm == sens_gcm_base) %>%
    # Use the per-indicator baseline model defined in tbl_standardize
    left_join(tbl_standardize %>% select(abbrev, dsmodel_baseline_ind = dsmodel_baseline), by = c("indicator" = "abbrev")) %>%
    filter(dsmodel == dsmodel_baseline_ind) %>%
    group_by(FULL_CU_IN, SPECIES_NAME, CVIS_NAME, category, indicator, stat) %>%
    summarise(
        base_raw = mean(value, na.rm = TRUE),
        base_std = mean(std_value, na.rm = TRUE),
        .groups = "drop"
    ) %>%
    pivot_wider(
        names_from = stat,
        values_from = c(base_raw, base_std),
        names_glue = "{.value}_{stat}"
    )

# calculate the mean and raw values for other scenarios, and the raw and absolute deviation
# compared to baseline scenario, broken down by individual sources of variation.
ind_others <- all_std_long %>%
    filter(std_method == "exponential") %>%
    filter(stat == "mean") %>%
    filter(period_code != "0") %>%
    # Filter to only the baseline model for quantifying GCM/Scenario variation
    left_join(tbl_standardize %>% select(abbrev, dsmodel_baseline_ind = dsmodel_baseline), by = c("indicator" = "abbrev")) %>%
    filter(dsmodel == dsmodel_baseline_ind) %>%
    group_by(FULL_CU_IN, SPECIES_NAME, CVIS_NAME, category, indicator, gcm, rcp, period_code) %>%
    summarise(
        raw = mean(value, na.rm = TRUE),
        std = mean(std_value, na.rm = TRUE),
        .groups = "drop"
    )

# 2.1 GCM Deviations for indicators
ind_gcm_dev <- ind_others %>%
    filter(rcp == sens_rcp_base, period_code == sens_period_base, gcm %in% sens_gcms) %>%
    left_join(ind_baseline, by = c("FULL_CU_IN", "SPECIES_NAME", "CVIS_NAME", "category", "indicator")) %>%
    mutate(
        raw_dev = raw - base_raw_mean,
        std_dev = std - base_std_mean,
        abs_raw_dev = abs(raw_dev),
        abs_std_dev = abs(std_dev),
        source = paste0("GCM", gcm)
    )

# 2.2 RCP/Period Deviations for indicators
ind_scen_dev <- map_dfr(sens_scenarios, function(s) {
    ind_others %>%
        filter(rcp == s[1], period_code == s[2], gcm == sens_gcm_base) %>%
        left_join(ind_baseline, by = c("FULL_CU_IN", "SPECIES_NAME", "CVIS_NAME", "category", "indicator")) %>%
        mutate(
            raw_dev = raw - base_raw_mean,
            std_dev = std - base_std_mean,
            abs_raw_dev = abs(raw_dev),
            abs_std_dev = abs(std_dev),
            source = paste0("RCP", s[1], "_P", s[2])
        )
})

# 2.3 dsmethod Deviations for indicators
# Only for indicators with > 1 model available in dsmodel_baseline
ind_model_dev <- all_std_long %>%
    filter(std_method == "exponential") %>%
    filter(stat == "mean") %>%
    filter(rcp == sens_rcp_base, period_code == sens_period_base, gcm == sens_gcm_base) %>%
    # Identify variations from the baseline model
    left_join(tbl_standardize %>% select(abbrev, dsmodel_baseline_ind = dsmodel_baseline), by = c("indicator" = "abbrev")) %>%
    filter(dsmodel != dsmodel_baseline_ind) %>%
    left_join(ind_baseline, by = c("FULL_CU_IN", "SPECIES_NAME", "CVIS_NAME", "category", "indicator")) %>%
    mutate(
        raw_dev = value - base_raw_mean,
        std_dev = std_value - base_std_mean,
        abs_raw_dev = abs(raw_dev),
        abs_std_dev = abs(std_dev)
    ) %>%
    group_by(FULL_CU_IN, SPECIES_NAME, CVIS_NAME, category, indicator) %>%
    summarise(
        across(c(raw_dev, std_dev, abs_raw_dev, abs_std_dev), ~ mean(., na.rm = TRUE)),
        .groups = "drop"
    ) %>%
    mutate(source = "dsmethod") %>%
    select(FULL_CU_IN, SPECIES_NAME, CVIS_NAME, category, indicator, source, raw_dev, std_dev, abs_raw_dev, abs_std_dev)

# 2.4 stdmethod Deviations for indicators (Linear vs. Exponential)
ind_stdmethod_dev <- all_std_long %>%
    filter(std_method == "linear") %>%
    filter(stat == "mean") %>%
    filter(rcp == sens_rcp_base, period_code == sens_period_base, gcm == sens_gcm_base) %>%
    left_join(tbl_standardize %>% select(abbrev, dsmodel_baseline_ind = dsmodel_baseline), by = c("indicator" = "abbrev")) %>%
    filter(dsmodel == dsmodel_baseline_ind) %>%
    left_join(ind_baseline, by = c("FULL_CU_IN", "SPECIES_NAME", "CVIS_NAME", "category", "indicator")) %>%
    mutate(
        raw_dev = value - base_raw_mean,
        std_dev = std_value - base_std_mean,
        abs_raw_dev = abs(raw_dev),
        abs_std_dev = abs(std_dev),
        source = "stdmethod"
    ) %>%
    select(FULL_CU_IN, SPECIES_NAME, CVIS_NAME, category, indicator, source, raw_dev, std_dev, abs_raw_dev, abs_std_dev)

ind_dev_long <- bind_rows(
    ind_gcm_dev %>% select(FULL_CU_IN, SPECIES_NAME, CVIS_NAME, category, indicator, source, raw_dev, std_dev, abs_raw_dev, abs_std_dev),
    ind_scen_dev %>% select(FULL_CU_IN, SPECIES_NAME, CVIS_NAME, category, indicator, source, raw_dev, std_dev, abs_raw_dev, abs_std_dev),
    ind_model_dev %>% select(FULL_CU_IN, SPECIES_NAME, CVIS_NAME, category, indicator, source, raw_dev, std_dev, abs_raw_dev, abs_std_dev),
    ind_stdmethod_dev %>% select(FULL_CU_IN, SPECIES_NAME, CVIS_NAME, category, indicator, source, raw_dev, std_dev, abs_raw_dev, abs_std_dev)
)

ind_dev_wide <- ind_dev_long %>%
    pivot_wider(
        names_from = source,
        values_from = c(raw_dev, std_dev, abs_raw_dev, abs_std_dev),
        names_glue = "{.value}_{source}"
    )
    # Note: We no longer coalesce to 0 here to allow filtering of non-applicable sources in plots

# Repeat across all CUs to get an average
# Put all results in a dataframe with a row summarizing these metrics for each indicator/CU
ind_cu_summary <- ind_dev_wide %>%
    left_join(ind_baseline, by = c("FULL_CU_IN", "SPECIES_NAME", "CVIS_NAME", "category", "indicator"))

# ...and for all CUs
ind_all_summary <- ind_dev_wide %>%
    group_by(category, indicator) %>%
    summarise(
        FULL_CU_IN = "ALL",
        SPECIES_NAME = "ALL",
        CVIS_NAME = "ALL",
        across(where(is.numeric), ~ mean(., na.rm = TRUE)),
        .groups = "drop"
    )

base_all_summary <- ind_baseline %>%
    group_by(category, indicator) %>%
    summarise(
        across(starts_with("base_"), ~ mean(., na.rm = TRUE)),
        .groups = "drop"
    )

ind_all_summary <- ind_all_summary %>%
    left_join(base_all_summary, by = c("category", "indicator"))

# Add baseline rank per indicator/category for MRD calculation in 4c
# Calculated BOTH globally and separately within each SPECIES_NAME
ind_baseline_ranks <- ind_baseline %>%
    group_by(category, indicator) %>%
    mutate(base_rank_all = rank(base_raw_mean, ties.method = "average", na.last = "keep")) %>%
    group_by(category, indicator, SPECIES_NAME) %>%
    mutate(base_rank_sp = rank(base_raw_mean, ties.method = "average", na.last = "keep")) %>%
    ungroup() %>%
    select(FULL_CU_IN, category, indicator, base_rank_all, base_rank_sp)

# Combine results into one dataframe
indicator_metrics <- bind_rows(ind_cu_summary, ind_all_summary) %>%
    left_join(ind_baseline_ranks, by = c("FULL_CU_IN", "category", "indicator"))


# ==================== 3. Calculate Overall Vulnerability Deviations ====================

# For each indicator category and overall score, calculate deviation from baseline scenario

# Define source sets (from 0_setup.R)
gcm_sources <- sens_gcms
scen_sources <- sens_scenarios
method_sources <- sens_methods

# Filter for relevant categories and tidy up
relevant_categories <- c("all", "fwrs", "migr", "mar", "gen", "dem")

# Identify indicators with multiple models to quantify model variation
# We compare alternative models against the baseline model specified in tbl_standardize
mult_model_inds_df <- all_std_long %>%
    filter(std_method == "exponential") %>%
    left_join(tbl_standardize %>% select(abbrev, dsmodel_baseline_ind = dsmodel_baseline), by = c("indicator" = "abbrev"))

# Alternative models are those that appear in the data but are NOT the baseline for their indicator
all_models <- mult_model_inds_df %>%
    filter(dsmodel != dsmodel_baseline_ind) %>%
    pull(dsmodel) %>%
    unique()

# Map each model variant to the categories it actually affects
model_cat_relevance <- mult_model_inds_df %>%
    filter(dsmodel %in% all_models) %>%
    distinct(dsmodel, category) %>%
    rename(source = dsmodel)

# Identify baseline scores and bounds for sensitivity comparison
dat <- scores_tidy_baseline %>%
    filter(category %in% relevant_categories) %>%
    mutate(
        rcp = as.character(rcp),
        period_code = as.character(period_code),
        gcm = as.character(gcm)
    )

# Calculate fixed scaling bounds based on the baseline scenario
# These bounds ensure that deviations in sensitivity iterations are on the same CU-scale as the report
sens_bounds <- dat %>%
    filter(rcp == sens_rcp_base, period_code == sens_period_base, gcm == sens_gcm_base) %>%
    group_by(category, method) %>%
    summarise(
        mn = min(score, na.rm = TRUE),
        mx = max(score, na.rm = TRUE),
        .groups = "drop"
    )

# Define Baselines per category using pre-calculated ranks from 4a
baselines <- dat %>%
    filter(rcp == sens_rcp_base, period_code == sens_period_base, gcm == sens_gcm_base) %>%
    filter((category == "all" & method == sens_method_overall_base) | (category != "all" & method == sens_method_category_base)) %>%
    select(FULL_CU_IN, SPECIES_NAME, CVIS_NAME, SMU_SIMPLE, category, 
           base_score = score100_all, 
           base_rank_all = rankall, 
           base_rank_sp = rankspecies)

if (nrow(baselines) == 0) stop("Baseline scenarios not found in data.")

# Calculate scores for EACH dsmodel variation (baseline scenario, using fixed bounds)
model_variants <- list()
for (mod in all_models) {
    mod_dat <- mult_model_inds_df %>%
        filter(rcp == sens_rcp_base, period_code == sens_period_base, gcm == sens_gcm_base) %>%
        group_by(FULL_CU_IN, SPECIES_NAME, CVIS_NAME, SMU_SIMPLE, category, indicator) %>%
        summarise(
            std_value = if (any(dsmodel == mod)) {
                std_value[dsmodel == mod][1]
            } else {
                std_value[dsmodel == dsmodel_baseline_ind][1]
            },
            .groups = "drop"
        )

    if (nrow(mod_dat) > 0) {
        mod_scores <- mod_dat %>%
            group_by(FULL_CU_IN, SPECIES_NAME, CVIS_NAME, SMU_SIMPLE) %>%
            calculate_combined_scores() %>%
            left_join(sens_bounds, by = c("category", "method")) %>%
            mutate(
                score100_all = if_else(!is.na(mx) & !is.na(mn) & mx > mn, (score - mn) / (mx - mn) * 100, score * 100)
            ) %>%
            # Calculate ranks properly grouped by method and category
            group_by(category, method) %>%
            mutate(rankall = rank(score, ties.method = "average")) %>%
            group_by(category, method, SPECIES_NAME) %>%
            mutate(rankspecies = rank(score, ties.method = "average")) %>%
            ungroup() %>%
            mutate(source = mod)

        model_variants[[mod]] <- mod_scores
    }
}

if (length(model_variants) > 0) {
    model_variants_df <- bind_rows(model_variants)
} else {
    model_variants_df <- tibble(
        FULL_CU_IN = character(), SPECIES_NAME = character(), CVIS_NAME = character(), SMU_SIMPLE = character(),
        category = character(), method = character(), score = numeric(), source = character(),
        score100_all = numeric(), rankall = numeric(), rankspecies = numeric()
    )
}

# Helper function to calculate raw and absolute deviation from baseline
# Uses ranks from the dataframe (which come from scores_tidy for GCM/RCP scenarios)
calc_devs <- function(df, baseline_df) {
    df %>%
        left_join(baseline_df, by = c("FULL_CU_IN", "SPECIES_NAME", "CVIS_NAME", "SMU_SIMPLE", "category")) %>%
        mutate(
            raw_dev = score100_all - base_score,
            abs_dev = abs(raw_dev),
            rank_diff_all = abs(base_rank_all - rankall),
            rank_diff_sp = abs(base_rank_sp - rankspecies)
        )
}

all_devs_list <- list()

for (cat in relevant_categories) {
    cat_baseline <- baselines %>% filter(category == cat)
    cat_dat <- dat %>% filter(category == cat)

    # Baseline method for this category
    b_method <- if (cat == "all") sens_method_overall_base else sens_method_category_base

    # 2.1 GCM Deviations
    gcm_dev <- cat_dat %>%
        filter(rcp == sens_rcp_base, period_code == sens_period_base, method == b_method, gcm %in% gcm_sources) %>%
        calc_devs(cat_baseline) %>%
        select(FULL_CU_IN, SPECIES_NAME, CVIS_NAME, SMU_SIMPLE, category, base_score, base_rank_all, base_rank_sp, 
               source = gcm, raw_dev, abs_dev, rank_diff_all, rank_diff_sp, rankall, rankspecies) %>%
        mutate(source = paste0("GCM", source))

    # 2.2 RCP/Period Deviations
    scen_dev <- map_dfr(scen_sources, function(s) {
        cat_dat %>%
            filter(rcp == s[1], period_code == s[2], gcm == sens_gcm_base, method == b_method) %>%
            calc_devs(cat_baseline) %>%
            select(FULL_CU_IN, SPECIES_NAME, CVIS_NAME, SMU_SIMPLE, category, base_score, base_rank_all, base_rank_sp, 
                   raw_dev, abs_dev, rank_diff_all, rank_diff_sp, rankall, rankspecies) %>%
            mutate(source = paste0("RCP", s[1], "_P", s[2]))
    })

    # 2.3 Method Deviations
    m_dev <- cat_dat %>%
        filter(rcp == sens_rcp_base, period_code == sens_period_base, gcm == sens_gcm_base, method %in% method_sources) %>%
        calc_devs(cat_baseline) %>%
        select(FULL_CU_IN, SPECIES_NAME, CVIS_NAME, SMU_SIMPLE, category, base_score, base_rank_all, base_rank_sp, 
               source = method, raw_dev, abs_dev, rank_diff_all, rank_diff_sp, rankall, rankspecies) %>%
        mutate(source = paste0("Method_", source))

    # 2.4 dsmethod Deviations
    mod_dev <- tibble()
    if (nrow(model_variants_df) > 0) {
        # Identify models relevant to this specific category
        rel_mods <- if (cat == "all") all_models else model_cat_relevance$source[model_cat_relevance$category == cat]

        mod_dev <- model_variants_df %>%
            filter(category == cat, method == b_method, source %in% rel_mods) %>%
            calc_devs(cat_baseline) %>%
            mutate(source = "dsmethod") %>%
            group_by(FULL_CU_IN, SPECIES_NAME, CVIS_NAME, SMU_SIMPLE, category, base_score, base_rank_all, base_rank_sp, source) %>%
            summarise(
                across(c(raw_dev, abs_dev, rank_diff_all, rank_diff_sp, rankall, rankspecies), ~ mean(., na.rm = TRUE)),
                .groups = "drop"
            ) %>%
            select(FULL_CU_IN, SPECIES_NAME, CVIS_NAME, SMU_SIMPLE, category, base_score, base_rank_all, base_rank_sp, 
                   source, raw_dev, abs_dev, rank_diff_all, rank_diff_sp, rankall, rankspecies)
    }

    # 2.5 stdmethod Deviations (Linear vs. Exponential)
    stdmethod_dev <- tibble()
    if ("std_method" %in% names(scores_tidy)) {
        stdmethod_dev <- scores_tidy %>%
            filter(std_method == "linear") %>%
            filter(category == cat) %>%
            filter(rcp == sens_rcp_base, period_code == sens_period_base, gcm == sens_gcm_base, method == b_method) %>%
            mutate(
                rcp = as.character(rcp),
                period_code = as.character(period_code),
                gcm = as.character(gcm)
            ) %>%
            calc_devs(cat_baseline) %>%
            select(FULL_CU_IN, SPECIES_NAME, CVIS_NAME, SMU_SIMPLE, category, base_score, base_rank_all, base_rank_sp, 
                   raw_dev, abs_dev, rank_diff_all, rank_diff_sp, rankall, rankspecies) %>%
            mutate(source = "stdmethod")
    }

    all_devs_list[[cat]] <- bind_rows(gcm_dev, scen_dev, m_dev, mod_dev, stdmethod_dev)
}

all_devs_long <- bind_rows(all_devs_list)

# Pivot wider to show columns for both raw and absolute values for each source
all_devs_wide <- all_devs_long %>%
    pivot_wider(
        names_from = source,
        values_from = c(raw_dev, abs_dev, rank_diff_all, rank_diff_sp, rankall, rankspecies),
        names_glue = "{.value}_{source}"
    ) %>%
    mutate(across(starts_with("raw_dev_") | starts_with("abs_dev_") | starts_with("rank_diff_") | starts_with("rankall_") | starts_with("rankspecies_"), ~ coalesce(., 0)))

# # Calculate proportions based on ABSOLUTE deviations
# abs_cols <- names(all_devs_wide)[startsWith(names(all_devs_wide), "abs_dev_")]
# all_devs_wide$total_abs_dev <- rowSums(all_devs_wide[, abs_cols])
#
# for (col in abs_cols) {
#     prop_name <- str_replace(col, "abs_dev_", "prop_")
#     all_devs_wide[[prop_name]] <- all_devs_wide[[col]] / all_devs_wide$total_abs_dev
# }

# Identify Top Driver using ABSOLUTE deviations
top_drivers <- all_devs_long %>%
    group_by(FULL_CU_IN, category) %>%
    mutate(rank_driver = rank(-abs_dev, ties.method = "first")) %>%
    filter(rank_driver == 1) %>%
    select(FULL_CU_IN, category, top_driver = source) %>%
    ungroup()

all_devs_wide <- all_devs_wide %>%
    left_join(top_drivers, by = c("FULL_CU_IN", "category"))

# ==================== 4. Jackknife Leverage Analysis ====================
cat("\nPerforming Jackknife (Leave-one-out) Leverage Analysis...\n")

# (hinge_weight now in 4_scoring_utils.R)

# Recreate the 'filled' indicator values for the baseline scenario using specific baseline models
jk_dat <- all_std_long %>%
    filter(std_method == "exponential") %>%
    left_join(tbl_standardize %>% select(abbrev, dsmodel_baseline_ind = dsmodel_baseline), by = c("indicator" = "abbrev")) %>%
    filter(dsmodel == dsmodel_baseline_ind) %>%
    mutate(
        rcp = as.character(rcp),
        period_code = as.character(period_code),
        gcm = as.character(gcm)
    )

jk_proj_tbl_base <- jk_dat %>%
    filter(rcp == sens_rcp_base, period_code == sens_period_base, gcm == sens_gcm_base) %>%
    select(FULL_CU_IN, SPECIES_NAME, CVIS_NAME, indicator, category, proj_value = std_value)

jk_static_tbl <- jk_dat %>%
    filter(gcm == "0", period_code == "0") %>%
    select(FULL_CU_IN, SPECIES_NAME, CVIS_NAME, indicator, category, static_value = std_value) %>%
    distinct()

jk_inds_per_cu <- jk_dat %>%
    distinct(FULL_CU_IN, SPECIES_NAME, CVIS_NAME, indicator, category)

baseline_vals <- jk_inds_per_cu %>%
    left_join(jk_proj_tbl_base, by = c("FULL_CU_IN", "SPECIES_NAME", "CVIS_NAME", "indicator", "category")) %>%
    left_join(jk_static_tbl, by = c("FULL_CU_IN", "SPECIES_NAME", "CVIS_NAME", "indicator", "category")) %>%
    mutate(std_value = coalesce(proj_value, static_value)) %>%
    group_by(FULL_CU_IN, SPECIES_NAME, CVIS_NAME, indicator, category) %>%
    summarise(std_value = mean(std_value, na.rm = TRUE), .groups = "drop") %>%
    mutate(std_value = ifelse(is.nan(std_value), NA_real_, std_value))

# Calculate actual baseline scores (0-1 scale first)
baseline_scores_raw <- baseline_vals %>%
    group_by(FULL_CU_IN, SPECIES_NAME, CVIS_NAME) %>%
    calculate_combined_scores()

# Determine fixed scaling bounds based ON THE FULL BASELINE
# This matches the logic in 4a where scores are scaled relative to the baseline distribution
jk_bounds <- baseline_scores_raw %>%
    group_by(method, category) %>%
    summarise(
        mn = suppressWarnings(min(score, na.rm = TRUE)),
        mx = suppressWarnings(max(score, na.rm = TRUE)),
        .groups = "drop"
    )

# Scale the baseline itself using these bounds
baseline_scores <- baseline_scores_raw %>%
    left_join(jk_bounds, by = c("method", "category")) %>%
    mutate(base_score_100 = if_else(mx > mn, (score - mn) / (mx - mn) * 100, score * 100)) %>%
    select(FULL_CU_IN, SPECIES_NAME, CVIS_NAME, method, category, base_score_100)

# Jackknife Loop
unique_indicators <- sort(unique(baseline_vals$indicator))
jackknife_results <- list()

# 1. Leave out individual indicators
for (ind in unique_indicators) {
    cat(paste0("Excluding Indicator: ", ind, "...\n"))
    # Exclude one indicator and re-score
    jk_scores <- baseline_vals %>%
        filter(indicator != ind) %>%
        group_by(FULL_CU_IN, SPECIES_NAME, CVIS_NAME) %>%
        calculate_combined_scores() %>%
        mutate(excluded_element = ind, excluded_type = "indicator")

    jackknife_results[[ind]] <- jk_scores
}

# 2. Leave out entire category
unique_categories <- sort(unique(baseline_vals$category))
for (cat_group in unique_categories) {
    cat(paste0("Excluding Category: ", cat_group, "...\n"))
    # Exclude all indicators in category and re-score
    jk_scores <- baseline_vals %>%
        filter(category != cat_group) %>%
        group_by(FULL_CU_IN, SPECIES_NAME, CVIS_NAME) %>%
        calculate_combined_scores() %>%
        mutate(excluded_element = cat_group, excluded_type = "category")

    jackknife_results[[paste0("CAT_", cat_group)]] <- jk_scores
}

jackknife_all <- bind_rows(jackknife_results)

# Calculate deviations using FIXED baseline bounds for scaling
jackknife_analysis <- jackknife_all %>%
    left_join(jk_bounds, by = c("method", "category")) %>%
    mutate(jk_score_100 = if_else(mx > mn, (score - mn) / (mx - mn) * 100, score * 100)) %>%
    left_join(baseline_scores, by = c("FULL_CU_IN", "SPECIES_NAME", "CVIS_NAME", "method", "category")) %>%
    mutate(
        raw_dev = jk_score_100 - base_score_100,
        abs_dev = abs(raw_dev)
    )

ind_cat_map <- bind_rows(
    baseline_vals %>% distinct(indicator, category) %>% rename(excluded_element = indicator, parent_category = category),
    tibble(excluded_element = unique_categories, parent_category = unique_categories)
)

jackknife_joined <- jackknife_analysis %>%
    left_join(ind_cat_map, by = "excluded_element") %>%
    filter(
        category == "all" | category == parent_category | excluded_type == "category"
    )

# Calculate global jackknife leverage
influence_global <- jackknife_joined %>%
    group_by(excluded_element, excluded_type, parent_category, method, category) %>%
    summarise(
        mean_raw_dev = mean(raw_dev, na.rm = TRUE),
        q10_raw_dev  = quantile(raw_dev, 0.1, na.rm = TRUE),
        q90_raw_dev  = quantile(raw_dev, 0.9, na.rm = TRUE),
        mean_abs_dev = mean(abs_dev, na.rm = TRUE),
        q10_abs_dev  = quantile(abs_dev, 0.1, na.rm = TRUE),
        q90_abs_dev  = quantile(abs_dev, 0.9, na.rm = TRUE),
        max_abs_dev  = max(abs_dev, na.rm = TRUE),
        .groups = "drop"
    ) %>%
    mutate(SPECIES_NAME = "ALL")

# Calculate species jackknife leverage
influence_species <- jackknife_joined %>%
    group_by(SPECIES_NAME, excluded_element, excluded_type, parent_category, method, category) %>%
    summarise(
        mean_raw_dev = mean(raw_dev, na.rm = TRUE),
        q10_raw_dev  = quantile(raw_dev, 0.1, na.rm = TRUE),
        q90_raw_dev  = quantile(raw_dev, 0.9, na.rm = TRUE),
        mean_abs_dev = mean(abs_dev, na.rm = TRUE),
        q10_abs_dev  = quantile(abs_dev, 0.1, na.rm = TRUE),
        q90_abs_dev  = quantile(abs_dev, 0.9, na.rm = TRUE),
        max_abs_dev  = max(abs_dev, na.rm = TRUE),
        .groups = "drop"
    )

influence_summary <- bind_rows(influence_global, influence_species)

# ==================== 5. Species-Level Summaries ====================
cat("\nCalculating species-level sensitivity summaries...\n")

# Summary of overall score sensitivity per species
species_score_summary <- all_devs_long %>%
    group_by(SPECIES_NAME, category, source) %>%
    summarise(
        n_cus = n(),
        mean_raw_dev = mean(raw_dev, na.rm = TRUE),
        mean_abs_dev = mean(abs_dev, na.rm = TRUE),
        mrd_all = mean(rank_diff_all, na.rm = TRUE),
        mrd_sp = mean(rank_diff_sp, na.rm = TRUE),
        .groups = "drop"
    )

# Summary of overall score sensitivity GLOBALLY (across all species)
global_score_summary <- all_devs_long %>%
    group_by(category, source) %>%
    summarise(
        SPECIES_NAME = "ALL",
        n_cus = n(),
        mean_raw_dev = mean(raw_dev, na.rm = TRUE),
        mean_abs_dev = mean(abs_dev, na.rm = TRUE),
        mrd_all = mean(rank_diff_all, na.rm = TRUE),
        mrd_sp = NA_real_, # Not applicable for global
        .groups = "drop"
    )

# Summary of indicator-level metrics per species
ind_species_summary <- ind_dev_long %>%
    left_join(ind_baseline, by = c("FULL_CU_IN", "SPECIES_NAME", "CVIS_NAME", "category", "indicator")) %>%
    group_by(SPECIES_NAME, category, indicator, source) %>%
    summarise(
        mean_raw_dev = mean(raw_dev, na.rm = TRUE),
        mean_std_dev = mean(std_dev, na.rm = TRUE),
        .groups = "drop"
    )

# ==================== 6. Indicator Redundancy and Correlation ====================
cat("\nAnalyzing Indicator Redundancy (Collinearity)...\n")

# Pivot wide for correlation
dat_wide <- baseline_vals %>%
    select(FULL_CU_IN, indicator, std_value) %>%
    pivot_wider(names_from = indicator, values_from = std_value) %>%
    # Ensure ONLY numeric columns are kept for cor()
    select(where(is.numeric))

cat(paste0("Numeric indicator columns for correlation: ", ncol(dat_wide), "\n"))

# Spearman correlation matrix
cor_matrix <- cor(dat_wide, method = "pearson", use = "pairwise.complete.obs")


# ==================== 7. Save Outputs ====================

cat("\nSaving sensitivity results...\n")

overall_sensitivity <- list(
    indicator_metrics = indicator_metrics,
    deviations = all_devs_wide,
    categories = relevant_categories,
    jackknife_results = jackknife_analysis,
    influence_summary = influence_summary,
    species_score_summary = species_score_summary,
    global_score_summary = global_score_summary,
    score_sensitivity_summary = bind_rows(global_score_summary, species_score_summary),
    species_indicator_summary = ind_species_summary,
    correlation_indicators = cor_matrix
)

save(overall_sensitivity, file = file.path(paths$output, "sensitivity_analysis.Rdata"))

cat("Script 4b complete. Results saved to sensitivity_analysis.Rdata\n")
