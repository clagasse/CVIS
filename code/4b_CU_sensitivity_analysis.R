####
#
# 4b_CU_sensitivity_analysis.R
#
# Quantify drivers of uncertainty across multiple dimensions and categories:
# Dimensions: GCMs (1, 4, 6), RCP/Period (45/5, 85/3, 85/5), Methods (cube, flag)
# Categories: all, fwrs, migr, mar
#
# Metrics:
# - Raw Deviation: (score - baseline) captures directionality
# - Absolute Deviation: abs(raw_dev) used for identifying main drivers
#
####

#----1. Setup and Import----
library(here)
setwd(here())
source(file.path(here(), "code", "0_setup.R"))

# Load outputs from 4a
load(file.path(paths$output, "scoring_results.Rdata")) # loads all_std_long, scores_tidy

# Filter for relevant categories and tidy up
relevant_categories <- c("all", "fwrs", "migr", "mar")

dat <- scores_tidy %>%
    filter(category %in% relevant_categories) %>%
    mutate(
        rcp = as.character(rcp),
        period_code = as.character(period_code),
        gcm = as.character(gcm)
    )

# Define Baselines per category
# Overall ('all') uses sens_method_overall_base, while lifestages use sens_method_category_base
baselines <- dat %>%
    filter(rcp == sens_rcp_base, period_code == sens_period_base, gcm == sens_gcm_base) %>%
    filter((category == "all" & method == sens_method_overall_base) | (category != "all" & method == sens_method_category_base)) %>%
    select(FULL_CU_IN, SPECIES_NAME, CVIS_NAME, category, base_score = score100_all, base_rank = rankall)

if (nrow(baselines) == 0) stop("Baseline scenarios not found in data.")

# Helper function to calculate raw and absolute deviation from baseline
calc_devs <- function(df, baseline_df) {
    df %>%
        left_join(baseline_df, by = c("FULL_CU_IN", "SPECIES_NAME", "CVIS_NAME", "category")) %>%
        mutate(
            raw_dev = score100_all - base_score,
            abs_dev = abs(raw_dev)
        )
}

#----2. Calculate Deviations for Granular Sources----

# Define source sets (from 0_setup.R)
gcm_sources <- sens_gcms
scen_sources <- sens_scenarios
method_sources <- sens_methods

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
        select(FULL_CU_IN, SPECIES_NAME, CVIS_NAME, category, source = gcm, raw_dev, abs_dev) %>%
        mutate(source = paste0("GCM", source))

    # 2.2 RCP/Period Deviations
    scen_dev <- map_dfr(scen_sources, function(s) {
        cat_dat %>%
            filter(rcp == s[1], period_code == s[2], gcm == sens_gcm_base, method == b_method) %>%
            calc_devs(cat_baseline) %>%
            select(FULL_CU_IN, SPECIES_NAME, CVIS_NAME, category, raw_dev, abs_dev) %>%
            mutate(source = paste0("RCP", s[1], "_P", s[2]))
    })

    # 2.3 Method Deviations
    m_dev <- cat_dat %>%
        filter(rcp == sens_rcp_base, period_code == sens_period_base, gcm == sens_gcm_base, method %in% method_sources) %>%
        calc_devs(cat_baseline) %>%
        select(FULL_CU_IN, SPECIES_NAME, CVIS_NAME, category, source = method, raw_dev, abs_dev)

    all_devs_list[[cat]] <- bind_rows(gcm_dev, scen_dev, m_dev)
}

all_devs_long <- bind_rows(all_devs_list)

#----3. Relative Contribution and Ranking----

# Pivot wider to show columns for both raw and absolute values for each source
uncertainty_contributions <- all_devs_long %>%
    pivot_wider(
        names_from = source,
        values_from = c(raw_dev, abs_dev),
        names_glue = "{.value}_{source}"
    ) %>%
    mutate(across(starts_with("raw_dev_") | starts_with("abs_dev_"), ~ coalesce(., 0)))

# Calculate proportions based on ABSOLUTE deviations
abs_cols <- names(uncertainty_contributions)[startsWith(names(uncertainty_contributions), "abs_dev_")]
uncertainty_contributions$total_abs_dev <- rowSums(uncertainty_contributions[, abs_cols])

for (col in abs_cols) {
    prop_name <- str_replace(col, "abs_dev_", "prop_")
    uncertainty_contributions[[prop_name]] <- uncertainty_contributions[[col]] / uncertainty_contributions$total_abs_dev
}

# Identify Top Driver using ABSOLUTE deviations
top_drivers <- all_devs_long %>%
    group_by(FULL_CU_IN, category) %>%
    mutate(rank_driver = rank(-abs_dev, ties.method = "first")) %>%
    filter(rank_driver == 1) %>%
    select(FULL_CU_IN, category, top_driver = source) %>%
    ungroup()

uncertainty_contributions <- uncertainty_contributions %>%
    left_join(top_drivers, by = c("FULL_CU_IN", "category"))

#----4. Rank Displacement & Correlation----

mrd_list <- list()
cor_list <- list()

for (cat in relevant_categories) {
    cat_baseline <- baselines %>% filter(category == cat)
    cat_dat <- dat %>% filter(category == cat)
    b_method <- if (cat == "all") sens_method_overall_base else sens_method_category_base

    # Define shift scenarios for rank comparison
    shifts <- list(baseline = cat_baseline %>% select(FULL_CU_IN, rank = base_rank))

    # GCM shifts
    for (g in gcm_sources) {
        shifts[[paste0("GCM", g)]] <- cat_dat %>%
            filter(rcp == sens_rcp_base, period_code == sens_period_base, gcm == g, method == b_method) %>%
            select(FULL_CU_IN, rank = rankall)
    }
    # Scenario shifts
    for (s in scen_sources) {
        nm <- paste0("RCP", s[1], "_P", s[2])
        shifts[[nm]] <- cat_dat %>%
            filter(rcp == s[1], period_code == s[2], gcm == sens_gcm_base, method == b_method) %>%
            select(FULL_CU_IN, rank = rankall)
    }
    # Method shifts
    for (m in method_sources) {
        shifts[[m]] <- cat_dat %>%
            filter(rcp == sens_rcp_base, period_code == sens_period_base, gcm == sens_gcm_base, method == m) %>%
            select(FULL_CU_IN, rank = rankall)
    }

    # Join into master rank df
    rank_df <- shifts$baseline %>% rename(baseline = rank)
    for (nm in names(shifts)[names(shifts) != "baseline"]) {
        rank_df <- rank_df %>% left_join(shifts[[nm]] %>% rename(!!nm := rank), by = "FULL_CU_IN")
    }

    # Correlation (Spearman)
    cor_matrix <- cor(rank_df %>% select(-FULL_CU_IN), method = "spearman", use = "pairwise.complete.obs")
    cor_list[[cat]] <- as.data.frame(cor_matrix) %>%
        rownames_to_column("scenario") %>%
        mutate(category = cat)

    # MRD
    mrd_df <- map_dfr(names(shifts)[names(shifts) != "baseline"], function(nm) {
        val <- mean(abs(rank_df[[nm]] - rank_df$baseline), na.rm = TRUE)
        tibble(source = nm, MRD = val)
    }) %>% mutate(category = cat)

    mrd_list[[cat]] <- mrd_df
}

mrd_stats <- bind_rows(mrd_list)
cor_matrix_combined <- bind_rows(cor_list)

#----5. Save Outputs----

cat("\nSaving sensitivity results...\n")

sensitivity_analysis_4b_directional <- list(
    contributions = uncertainty_contributions,
    cor_matrix = cor_list,
    mrd_stats = mrd_stats,
    categories = relevant_categories
)

save(sensitivity_analysis_4b_directional, file = file.path(paths$output, "sensitivity_analysis_4b_directional.Rdata"))

cat("Script 4b complete. Results saved to sensitivity_analysis_4b_directional.Rdata\n")
