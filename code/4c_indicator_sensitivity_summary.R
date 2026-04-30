####
#
# 4c_indicator_sensitivity_summary.R
#
# Summarizes indicator-level sensitivity metrics across all CUs.
# Specifically, it identifies the distribution (mean, q10, q90) of deviations
# from the baseline scenario across all Conservation Units for each indicator
# and each source of variation (GCMs, RCPs, periods, etc).
#
####

#----1. Setup and Import----
library(here)
setwd(here())
source(file.path(here(), "code", "0_setup.R"))

# Load outputs from 4b (Indicator metrics and sensitivity analysis)
load(file.path(paths$output, "sensitivity_analysis.Rdata")) # loads overall_sensitivity

# Extract indicator_metrics (contains deviations for each CU and indicator)
indicator_metrics <- overall_sensitivity$indicator_metrics

cat("Summarizing indicator sensitivity across CUs...\n")

#----2. Integrated Indicator Sensitivity Summary ----

cat("Summarizing indicator sensitivity across CUs (raw and absolute deviations)...\n")

# Filter out the "ALL" summary rows to calculate statistics across individual CUs
cu_metrics <- indicator_metrics %>%
  filter(FULL_CU_IN != "ALL")

# Identify variation sources from raw deviation columns (e.g., raw_dev_GCM1)
raw_dev_cols <- names(cu_metrics)[grepl("^raw_dev_", names(cu_metrics))]

# Master list for indicator summaries
ind_summary_list <- list()

for (ind in unique(cu_metrics$indicator)) {
  ind_dat <- cu_metrics %>% filter(indicator == ind)
  cat_val <- first(ind_dat$category)
  
  # Use pre-calculated Baseline Ranks from 4b
  baseline_ranks <- ind_dat$base_rank
  baseline_vals <- ind_dat$base_raw_mean # Used for scen_vals calculation
  
  # Calculate stats for each source of variation
  ind_source_results <- map_dfr(raw_dev_cols, function(col) {
    source_label <- str_remove(col, "raw_dev_")
    
    # Identify source type and clean name
    source_type <- case_when(
      str_detect(source_label, "^GCM") ~ "GCM",
      str_detect(source_label, "^RCP") ~ "Scenario",
      str_detect(source_label, "^Method") ~ "Method",
      str_detect(source_label, "^Model") ~ "Model",
      TRUE ~ "Other"
    )
    # Use full label for GCMs and Scenarios, which match sens_source_palette
    # But strip "Method_" as the palette uses the raw method names
    source_nm <- case_when(
      source_type == "Method" ~ str_remove(source_label, "^Method_"),
      TRUE ~ source_label
    )
    
    # Extract raw deviations for this source
    raw_vals <- ind_dat[[col]]
    abs_vals <- abs(raw_vals)
    
    # Calculate Ranks for displacement and correlation
    # scen_vals = baseline + raw_dev
    scen_vals <- baseline_vals + raw_vals
    scen_ranks <- rank(scen_vals, ties.method = "average", na.last = "keep")
    
    # Rank Metrics
    mrd <- mean(abs(scen_ranks - baseline_ranks), na.rm = TRUE)
    cor_val <- cor(scen_ranks, baseline_ranks, method = "spearman", use = "pairwise.complete.obs")
    
    # Return single summary row
    tibble(
      category = cat_val,
      indicator = ind,
      base_mean = mean(baseline_vals, na.rm = TRUE),
      source_type = source_type,
      source = source_nm,
      n_cus = length(na.omit(raw_vals)),
      mean_raw_dev = mean(raw_vals, na.rm = TRUE),
      q10_raw_dev = quantile(raw_vals, 0.1, na.rm = TRUE),
      q90_raw_dev = quantile(raw_vals, 0.9, na.rm = TRUE),
      mean_abs_dev = mean(abs_vals, na.rm = TRUE),
      q10_abs_dev = quantile(abs_vals, 0.1, na.rm = TRUE),
      q90_abs_dev = quantile(abs_vals, 0.9, na.rm = TRUE),
      mrd = mrd,
      spearman_corr = cor_val
    )
  })
  
  ind_summary_list[[ind]] <- ind_source_results
}

ind_sens_summary <- bind_rows(ind_summary_list) %>%
  # Filter out sources with no influence for this indicator
  filter(mean_abs_dev != 0)


#----4. Integrated Score Sensitivity Summary (Deviations + Ranks) ----

cat("Calculating integrated score sensitivity summary (deviations, ranks, and correlations)...\n")

# Master list to store results
score_summary_list <- list()
cor_matrix_list <- list()

# Extract score deviations (one row per CU and category)
score_deviations <- overall_sensitivity$deviations
raw_dev_cols <- names(score_deviations)[grepl("raw_dev_", names(score_deviations))]

for (cat in unique(score_deviations$category)) {
  # Subset to the current category
  cat_dat <- score_deviations %>% filter(category == cat)
  
  # 4.1 Categorical & Species-level Summaries
  baseline_rank <- cat_dat$base_rank
  
  # For the correlation matrix, we need a table of ranks for ALL scenarios
  rank_df <- cat_dat %>% select(FULL_CU_IN, SPECIES_NAME, baseline = base_rank)
  
  # Loop through each source of variation
  cat_source_results <- map_dfr(raw_dev_cols, function(col) {
    source_label <- str_remove(col, "raw_dev_")
    
    # Identify type and clean name
    source_type <- case_when(
      str_detect(source_label, "^GCM") ~ "GCM",
      str_detect(source_label, "^RCP") ~ "Scenario",
      str_detect(source_label, "^Method") ~ "Method",
      str_detect(source_label, "^Model") ~ "Model",
      TRUE ~ "Other"
    )
    # Use full label for GCMs and Scenarios, which match sens_source_palette
    # But strip "Method_" as the palette uses the raw method names
    source_nm <- case_when(
      source_type == "Method" ~ str_remove(source_label, "^Method_"),
      TRUE ~ source_label
    )
    
    # Values
    raw_vals <- cat_dat[[col]]
    abs_vals <- abs(raw_vals)
    scen_scores <- cat_dat$base_score + raw_vals
    scen_ranks <- rank(scen_scores, ties.method = "average", na.last = "keep")
    
    # Add to rank_df for the category heatmap
    rank_df <<- rank_df %>% mutate(!!source_label := scen_ranks)
    
    # Metrics
    # Global (Category level)
    global_row <- tibble(
      category = cat,
      SPECIES_NAME = "ALL",
      source_type = source_type,
      source = source_nm,
      n_cus = length(na.omit(raw_vals)),
      mean_raw_dev = mean(raw_vals, na.rm = TRUE),
      q10_raw_dev = quantile(raw_vals, 0.1, na.rm = TRUE),
      q90_raw_dev = quantile(raw_vals, 0.9, na.rm = TRUE),
      mean_abs_dev = mean(abs_vals, na.rm = TRUE),
      q10_abs_dev = quantile(abs_vals, 0.1, na.rm = TRUE),
      q90_abs_dev = quantile(abs_vals, 0.9, na.rm = TRUE),
      mrd = mean(abs(scen_ranks - baseline_rank), na.rm = TRUE),
      spearman_corr = cor(scen_ranks, baseline_rank, method = "spearman", use = "pairwise.complete.obs")
    )
    
    # Species-level MRD
    species_rows <- cat_dat %>%
      mutate(rank_diff = abs(scen_ranks - baseline_rank)) %>%
      group_by(SPECIES_NAME) %>%
      summarise(
        mrd = mean(rank_diff, na.rm = TRUE),
        n_cus = n(),
        .groups = "drop"
      )
    
    # Return global and species-level combined
    # Ensure both have the same columns to avoid bind_rows issues
    res <- bind_rows(
      global_row,
      species_rows %>% mutate(
        category = cat,
        source_type = source_type,
        source = source_nm,
        mean_raw_dev = NA_real_, q10_raw_dev = NA_real_, q90_raw_dev = NA_real_,
        mean_abs_dev = NA_real_, q10_abs_dev = NA_real_, q90_abs_dev = NA_real_,
        spearman_corr = NA_real_
      )
    )
    res
  })
  
  score_summary_list[[cat]] <- cat_source_results
  
  # 4.2 Rank Correlation Matrix for this category
  rank_cols <- setdiff(names(rank_df), c("FULL_CU_IN", "SPECIES_NAME"))
  cor_matrix_list[[cat]] <- cor(rank_df %>% select(all_of(rank_cols)), method = "spearman", use = "pairwise.complete.obs")
}

score_sens_summary <- bind_rows(score_summary_list)
cor_matrices <- cor_matrix_list


#----5. Export Results----

# Save as R object for downstream use
save(
  ind_sens_summary,
  score_sens_summary,
  cor_matrices,
  file = file.path(paths$output, "indicator_sensitivity_summary.Rdata")
)

# Save as CSVs for quick inspection
write_csv(ind_sens_summary, file = file.path(paths$output, "indicator_sensitivity_summary.csv"))
write_csv(score_sens_summary, file = file.path(paths$output, "score_sensitivity_summary.csv"))

cat("Script 4c complete.\n")
cat("Indicator summary: output/indicator_sensitivity_summary.csv\n")
cat("Score summary:     output/score_sensitivity_summary.csv\n")


#----6. Example Check----

# Print example requested by user
if ("flow18pdelta" %in% ind_sens_summary$indicator) {
  cat("\nExample: Deviation for 'flow18pdelta' from GCM1 across all CUs:\n")
  example <- ind_sens_summary %>%
    filter(indicator == "flow18pdelta", source == "1", source_type == "GCM") %>%
    select(indicator, source_type, source, mean_raw_dev, mean_abs_dev, mrd, spearman_corr)
  print(as.data.frame(example))
}

if ("all" %in% score_sens_summary$category) {
  cat("\nExample: Overall vulnerability score sensitivity for GCM1:\n")
  example_score <- score_sens_summary %>%
    filter(category == "all", source == "1", source_type == "GCM") %>%
    select(category, source_type, source, mean_raw_dev, mean_abs_dev, q10_abs_dev, q90_abs_dev, mrd, spearman_corr)
  print(as.data.frame(example_score))
}
