################################################################################
#
# 4c_indicator_sensitivity_summary.R
#
# Sensitivity Consolidation & Risk Driver Analysis:
# 1. Summarizes indicator-level and score-level sensitivity across all CUs.
# 2. Identifies dominant risk drivers using ANOVA-based factor importance.
# 3. Performs multivariate analysis (PCA) to characterize species risk profiles.
# 4. Analyzes indicator collinearity and grouping through hierarchical clustering.
# 5. Generates the final consolidated sensitivity RData for all plotting scripts.
#
################################################################################

#----1. Setup and Import----
library(here)
setwd(here())
source(file.path(here(), "code", "0_setup.R"))

load(file.path(paths$output, "scoring_results.Rdata")) # Ensure all_std_long and scores_tidy are fresh
# Load outputs from 4b (Indicator metrics and sensitivity analysis)
load(file.path(paths$output, "sensitivity_analysis.Rdata")) # loads overall_sensitivity

metadata_cu <- cu_run %>%
  select(FULL_CU_IN, FAZ_group, DFO_AREA, SMU_SIMPLE, SPECIES_NAME)


cat("Summarizing indicator sensitivity across CUs (raw and absolute deviations)...\n")

# Filter out the "ALL" summary rows to calculate statistics across individual CUs
cu_metrics <- overall_sensitivity$indicator_metrics %>%
  filter(FULL_CU_IN != "ALL")

# Identify variation sources from raw deviation columns (e.g., raw_dev_GCM1)
raw_dev_cols <- names(cu_metrics)[grepl("^raw_dev_", names(cu_metrics))]

# Master list for indicator summaries
ind_summary_list <- list()

# Calculate statistics for BOTH 'ALL' (Global) and individual species
species_to_process <- c("ALL", unique(cu_metrics$SPECIES_NAME))

for (sp in species_to_process) {
  sp_metrics <- if (sp == "ALL") cu_metrics else cu_metrics %>% filter(SPECIES_NAME == sp)
  
  for (ind in unique(sp_metrics$indicator)) {
    ind_dat <- sp_metrics %>% filter(indicator == ind)
    cat_val <- first(ind_dat$category)
    
    # Use pre-calculated baseline ranks from 4b (which has both _all and _sp)
    # For global summary, we use base_rank_all; for species summary, we use base_rank_sp
    baseline_ranks <- if (sp == "ALL") ind_dat$base_rank_all else ind_dat$base_rank_sp
    baseline_vals <- ind_dat$base_raw_mean
    
    # Calculate stats for each source of variation
    ind_source_results <- map_dfr(raw_dev_cols, function(col) {
      source_label <- str_remove(col, "raw_dev_")
      
      source_type <- case_when(
        str_detect(source_label, "^GCM") ~ "GCM",
        str_detect(source_label, "^RCP") ~ "Scenario",
        str_detect(source_label, "^Method") ~ "Method",
        str_detect(source_label, "^Model") ~ "dsmethod",
        TRUE ~ "Other"
      )
      source_nm <- case_when(
        source_type == "Method" ~ str_remove(source_label, "^Method_"),
        TRUE ~ source_label
      )
      
      raw_vals <- ind_dat[[col]]
      abs_vals <- abs(raw_vals)
      
      # Calculate Ranks WITHIN CURRENT SCOPE (Global or Species) for displacement
      scen_vals <- baseline_vals + raw_vals
      scen_ranks <- rank(scen_vals, ties.method = "average", na.last = "keep")
      
      mrd <- mean(abs(scen_ranks - baseline_ranks), na.rm = TRUE)
      cor_val <- cor(scen_ranks, baseline_ranks, method = "spearman", use = "pairwise.complete.obs")
      
      tibble(
        SPECIES_NAME = sp,
        category = cat_val,
        indicator = ind,
        base_mean = mean(baseline_vals, na.rm = TRUE),
        base_q10  = quantile(baseline_vals, 0.1, na.rm = TRUE),
        base_q90  = quantile(baseline_vals, 0.9, na.rm = TRUE),
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
    ind_summary_list[[paste0(sp, "_", ind)]] <- ind_source_results
  }
}

ind_sens_summary <- bind_rows(ind_summary_list) %>%
  # Filter out sources with no influence for this indicator
  filter(mean_abs_dev != 0)


#----3. Integrated Score Sensitivity Summary (Deviations + Ranks) ----

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
  
  # For the correlation matrix, we'll focus on Global Rank correlation
  rank_df <- cat_dat %>% select(FULL_CU_IN, SPECIES_NAME, baseline = base_rank_all)
  
  # Loop through each source of variation
  cat_source_results <- map_dfr(raw_dev_cols, function(col) {
    source_label <- str_remove(col, "raw_dev_")
    
    # Identify type and clean name
    source_type <- case_when(
      str_detect(source_label, "^GCM") ~ "GCM",
      str_detect(source_label, "^RCP") ~ "Scenario",
      str_detect(source_label, "^Method") ~ "Method",
      str_detect(source_label, "^Model") ~ "dsmethod",
      TRUE ~ "Other"
    )
    source_nm <- case_when(
      source_type == "Method" ~ str_remove(source_label, "^Method_"),
      TRUE ~ source_label
    )
    
    # Values
    raw_vals <- cat_dat[[col]]
    abs_vals <- abs(raw_vals)
    
    # Use existing rank_diff columns from 4b if available
    rd_all_col <- paste0("rank_diff_all_", source_label)
    rd_sp_col <- paste0("rank_diff_sp_", source_label)
    
    mrd_all_vals <- if (rd_all_col %in% names(cat_dat)) cat_dat[[rd_all_col]] else NA_real_
    mrd_sp_vals  <- if (rd_sp_col %in% names(cat_dat)) cat_dat[[rd_sp_col]] else NA_real_
    
    # Metrics
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
      mrd = mean(mrd_all_vals, na.rm = TRUE),
      spearman_corr = cor(cat_dat$base_score + raw_vals, cat_dat$base_score, method = "spearman", use = "pairwise.complete.obs")
    )
    
    # Species-level MRD
    species_rows <- cat_dat %>%
      mutate(rd_sp = !!sym(rd_sp_col)) %>%
      group_by(SPECIES_NAME) %>%
      summarise(
        mrd = mean(rd_sp, na.rm = TRUE),
        n_cus = n(),
        .groups = "drop"
      )
    
    # Calculate Ranks for the category heatmap (Global)
    scen_scores <- cat_dat$base_score + raw_vals
    scen_ranks <- rank(scen_scores, ties.method = "average", na.last = "keep")
    
    # Add to rank_df for the category heatmap
    rank_df <<- rank_df %>% mutate(!!source_label := scen_ranks)
    
    # Return global and species-level combined
    bind_rows(
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
  })
  
  score_summary_list[[cat]] <- cat_source_results
  
  # Rank Correlation Matrix for this category
  rank_cols <- setdiff(names(rank_df), c("FULL_CU_IN", "SPECIES_NAME"))
  cor_matrix_list[[cat]] <- cor(rank_df %>% select(all_of(rank_cols)), method = "spearman", use = "pairwise.complete.obs")
}

score_sens_summary <- bind_rows(score_summary_list)
cor_matrices <- cor_matrix_list


#----4. Risk Drivers and Multivariate Analysis (Consolidated from 4d) ----

cat("Analyzing multivariate risk profiles and identification of dominant drivers...\n")

# A. Prepare Baseline Risk Profile Data
dynamic_dat <- all_std_long %>%
  filter(rcp == sens_rcp_base, period_code == sens_period_base, gcm == sens_gcm_base) %>%
  left_join(tbl_standardize %>% select(abbrev, dsmodel_baseline_ind = dsmodel_baseline), by = c("indicator" = "abbrev")) %>%
  filter(dsmodel == dsmodel_baseline_ind)

static_dat <- all_std_long %>%
  filter(period_code == 0, gcm == 0) %>%
  left_join(tbl_standardize %>% select(abbrev, dsmodel_baseline_ind = dsmodel_baseline), by = c("indicator" = "abbrev")) %>%
  filter(dsmodel == dsmodel_baseline_ind) %>%
  select(FULL_CU_IN, indicator, static_value = std_value)

analysis_dat <- all_std_long %>%
  distinct(FULL_CU_IN, SPECIES_NAME, CVIS_NAME, indicator) %>%
  left_join(dynamic_dat %>% select(FULL_CU_IN, indicator, std_value, category), by = c("FULL_CU_IN", "indicator")) %>%
  left_join(static_dat, by = c("FULL_CU_IN", "indicator")) %>%
  mutate(std_value = coalesce(std_value, static_value)) %>%
  filter(!is.na(std_value)) %>%
  left_join(tbl_indicators %>% select(abbrev, name, category_long = category), by = c("indicator" = "abbrev"))

pca_input_wide <- analysis_dat %>%
  group_by(FULL_CU_IN, SPECIES_NAME, CVIS_NAME, indicator) %>%
  summarise(val = mean(std_value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = indicator, values_from = val)

pca_ready <- pca_input_wide %>%
  select(-FULL_CU_IN, -SPECIES_NAME, -CVIS_NAME) %>%
  mutate(across(everything(), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

zero_var <- sapply(pca_ready, function(x) var(x, na.rm = TRUE) == 0 | is.na(var(x, na.rm = TRUE)))
if (any(zero_var)) pca_ready <- pca_ready[, !zero_var]

# B. Indicator Grouping (Hierarchical Clustering)
cor_matrix_ind_pearson <- cor(pca_ready, method = "pearson")
dist_matrix <- as.dist(1 - abs(cor_matrix_ind_pearson))
ind_hclust <- hclust(dist_matrix, method = "ward.D2")
ind_clusters <- cutree(ind_hclust, k = 5)
ind_cluster_df <- tibble(indicator = names(ind_clusters), ind_cluster = as.factor(ind_clusters)) %>%
  left_join(tbl_indicators %>% select(indicator = abbrev, name, category), by = "indicator")

cu_cluster_scores <- analysis_dat %>%
  left_join(ind_cluster_df %>% select(indicator, ind_cluster), by = "indicator") %>%
  filter(!is.na(ind_cluster)) %>%
  group_by(FULL_CU_IN, ind_cluster) %>%
  summarise(avg_score = mean(std_value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = ind_cluster, values_from = avg_score, names_prefix = "GRP_")

# C. Factor Importance (ANOVA)
vuln_scores <- scores_tidy %>%
  filter(rcp == sens_rcp_base, period_code == sens_period_base, gcm == sens_gcm_base, method == sens_method_overall_base, category == "all") %>%
  select(FULL_CU_IN, total_vulnerability = score100_all)

analysis_combined <- vuln_scores %>%
  left_join(pca_input_wide %>% select(-SPECIES_NAME, -CVIS_NAME), by = "FULL_CU_IN") %>%
  left_join(cu_cluster_scores, by = "FULL_CU_IN") %>%
  left_join(metadata_cu, by = "FULL_CU_IN")

lm_fit <- lm(total_vulnerability ~ SPECIES_NAME + SMU_SIMPLE + FAZ_group + ., 
             data = analysis_combined %>% select(-FULL_CU_IN, -DFO_AREA))
importance_df <- as.data.frame(anova(lm_fit)) %>%
  rownames_to_column("factor") %>%
  mutate(pct_variance = `Sum Sq` / sum(`Sum Sq`) * 100) %>%
  filter(factor != "Residuals") %>%
  arrange(desc(pct_variance))

# D. Species Drivers & PCA
species_drivers <- analysis_dat %>%
  group_by(SPECIES_NAME, indicator) %>%
  summarise(mean_std = mean(std_value, na.rm = TRUE), .groups = "drop") %>%
  group_by(SPECIES_NAME) %>% slice_max(order_by = mean_std, n = 5) %>% ungroup()

pca_res <- prcomp(pca_ready, scale. = TRUE)

# E. SMU Sensitivity
smu_sens_dat <- overall_sensitivity$deviations %>%
  filter(category == "all") %>%
  select(FULL_CU_IN, starts_with("raw_dev_")) %>%
  left_join(metadata_cu %>% select(FULL_CU_IN, SMU_SIMPLE, SPECIES_NAME), by = "FULL_CU_IN") %>%
  pivot_longer(cols = starts_with("raw_dev_"), names_to = "source", values_to = "raw_dev") %>%
  mutate(source = str_remove(source, "raw_dev_")) %>%
  filter(str_detect(source, "GCM|RCP"), !is.na(SMU_SIMPLE)) %>%
  group_by(SMU_SIMPLE, SPECIES_NAME, source) %>%
  summarise(mean_raw_dev = mean(raw_dev, na.rm = TRUE), .groups = "drop")

# Final Combined Object
risk_drivers_analysis <- list(
  indicator_clusters = ind_cluster_df,
  factor_importance = importance_df,
  species_top_drivers = species_drivers,
  pca_result = pca_res,
  pca_plot_dat = pca_input_wide %>% 
                 mutate(PC1 = pca_res$x[,1], PC2 = pca_res$x[,2]) %>%
                 left_join(metadata_cu %>% select(-SPECIES_NAME, -SMU_SIMPLE), by = "FULL_CU_IN") %>%
                 left_join(vuln_scores, by = "FULL_CU_IN"),
  analysis_combined = analysis_combined, # carries SMU, Species, Vulnerability
  cluster_scores = cu_cluster_scores,
  smu_sensitivity = smu_sens_dat,
  cor_matrix_pearson = cor_matrix_ind_pearson
)


#----5. Export Results----

# Save as R object for downstream use in 5c and 5e
save(
  ind_sens_summary,
  ind_summary_list,
  score_sens_summary,
  cor_matrices,
  risk_drivers_analysis,
  file = file.path(paths$output, "indicator_sensitivity_summary.Rdata")
)

cat("\nConsolidated Indicator Sensitivity and Risk Driver Analysis Complete.\n")


#----6. Example Check----

# Print example requested by user
if ("flow18pdelta" %in% ind_sens_summary$indicator) {
  cat("\nExample: Deviation for 'flow18pdelta' from GCM1 across all CUs:\n")
  example <- ind_sens_summary %>%
    filter(indicator == "flow18pdelta", source == "GCM1", source_type == "GCM") %>%
    select(indicator, source_type, source, mean_raw_dev, mean_abs_dev, mrd, spearman_corr)
  print(as.data.frame(example))
}

if ("all" %in% score_sens_summary$category) {
  cat("\nExample: Overall vulnerability score sensitivity for GCM1:\n")
  example_score <- score_sens_summary %>%
    filter(category == "all", source == "GCM1", source_type == "GCM") %>%
    select(category, source_type, source, mean_raw_dev, mean_abs_dev, q10_abs_dev, q90_abs_dev, mrd, spearman_corr)
  print(as.data.frame(example_score))
}
