#--------------------------------------------------------------------------
# 4d_risk_drivers_analysis.R
# Analyze multivariate risk profiles and identify dominant factors across CUs
# Includes quantitative indicator grouping and variation analysis
#--------------------------------------------------------------------------

source(here::here("code", "0_setup.R"))
library(broom)

# Load outputs from 4a, 4b, and 4c
cat("Loading analysis results...\n")
load(file.path(paths$output, "scoring_results.Rdata")) # all_std_long, scores_tidy
load(file.path(paths$output, "sensitivity_analysis.Rdata")) # overall_sensitivity (contains cor_matrix)
load(file.path(paths$output, "indicator_sensitivity_summary.Rdata")) # ind_sens_summary, etc.

# Use centralized baseline settings from 0_setup.R
cat_rcp <- sens_rcp_base
cat_period <- sens_period_base
cat_gcm <- sens_gcm_base

# 1. Data Preparation ------------------------------------------------------
cat("Preparing baseline risk profile data...\n")

# Get target dynamic indicators (baseline scenario)
dynamic_dat <- all_std_long %>%
  filter(
    rcp == cat_rcp,
    period_code == cat_period,
    gcm == cat_gcm
  ) %>%
  # Join with tbl_standardize to identify the specific baseline model for each indicator
  left_join(tbl_standardize %>% select(abbrev, dsmodel_baseline_ind = dsmodel_baseline), by = c("indicator" = "abbrev")) %>%
  filter(dsmodel == dsmodel_baseline_ind)

# Get static indicators (fallback)
static_dat <- all_std_long %>%
  filter(
    period_code == 0,
    gcm == 0
  ) %>%
  left_join(tbl_standardize %>% select(abbrev, dsmodel_baseline_ind = dsmodel_baseline), by = c("indicator" = "abbrev")) %>%
  filter(dsmodel == dsmodel_baseline_ind) %>%
  select(FULL_CU_IN, indicator, static_value = std_value)

# Create short names for labels to improve readability in plots
short_name_map <- c(
  "favchange" = "ENM Fav. Change",
  "cthr" = "FW Cumul. Threats",
  "tw8rate" = "Aug Temp Rate",
  "tw8proj" = "Aug Temp Proj",
  "flow8pdelta" = "Aug Flow Change",
  "flow18pdelta" = "Winter Flow Change",
  "fwres" = "FW Residency",
  "migrTproj" = "Migr. Temp Proj",
  "migrQpdelta" = "Migr. Flow Change",
  "migrdist" = "Migr. Distance",
  "SSTproj" = "SST Ocean Entry",
  "SSTrate" = "SST Change Rate",
  "CImpact" = "Marine Impacts",
  "CUstatus" = "WSP Status",
  "CUnmat" = "Abundance",
  "hetzyg" = "Heterozygosity",
  "genoff" = "Genomic Offset"
)

# Merge dynamic with static fallback
analysis_dat <- all_std_long %>%
  distinct(FULL_CU_IN, SPECIES_NAME, CVIS_NAME, indicator) %>%
  left_join(dynamic_dat %>% select(FULL_CU_IN, indicator, std_value, category), by = c("FULL_CU_IN", "indicator")) %>%
  left_join(static_dat, by = c("FULL_CU_IN", "indicator")) %>%
  mutate(std_value = coalesce(std_value, static_value)) %>%
  filter(!is.na(std_value)) %>%
  left_join(tbl_indicators %>% select(abbrev, name, category_long = category), by = c("indicator" = "abbrev")) %>%
  mutate(name_short = coalesce(short_name_map[indicator], indicator))

# Pivot wide for multivariate analysis
pca_input_wide <- analysis_dat %>%
  group_by(FULL_CU_IN, SPECIES_NAME, CVIS_NAME, indicator) %>%
  summarise(val = mean(std_value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = indicator, values_from = val)

# Handle Missing Data with mean imputation
pca_ready <- pca_input_wide %>%
  select(-FULL_CU_IN, -SPECIES_NAME, -CVIS_NAME) %>%
  mutate(across(everything(), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

# Remove zero-variance indicators
zero_var <- sapply(pca_ready, function(x) var(x, na.rm = TRUE) == 0 | is.na(var(x, na.rm = TRUE)))
if (any(zero_var)) {
  cat("Removing zero-variance indicators:", paste(names(pca_ready)[zero_var], collapse = ", "), "\n")
  pca_ready <- pca_ready[, !zero_var]
}

# 2. Indicator Grouping (Clustering Indicators) ----------------------------
cat("Quantitatively grouping vulnerability indicators...\n")

# Re-calculate correlation to ensure it matches the cleaned pca_ready columns
cor_matrix <- cor(pca_ready, method = "pearson")

# Hierarchical clustering of indicators
dist_matrix <- as.dist(1 - abs(cor_matrix)) # Distance based on absolute correlation
ind_hclust <- hclust(dist_matrix, method = "ward.D2")

# Define Indicator Clusters (e.g., 5 groups)
n_ind_clusters <- 5
ind_clusters <- cutree(ind_hclust, k = n_ind_clusters)
ind_cluster_df <- tibble(
  indicator = names(ind_clusters),
  ind_cluster = as.factor(ind_clusters)
) %>%
  left_join(tbl_indicators %>% select(indicator = abbrev, name, category), by = "indicator")

# Auto-label clusters based on their dominant component
cluster_labels <- ind_cluster_df %>%
  group_by(ind_cluster) %>%
  summarise(
    label = paste(head(indicator, 2), collapse = "/"),
    main_cat = names(which.max(table(category))),
    .groups = "drop"
  )
ind_cluster_df <- ind_cluster_df %>%
  left_join(cluster_labels %>% select(ind_cluster, cluster_name = label, cluster_category = main_cat), by = "ind_cluster")

# Calculate mean scores per indicator cluster per CU
cu_cluster_scores <- analysis_dat %>%
  left_join(ind_cluster_df %>% select(indicator, ind_cluster, cluster_name), by = "indicator") %>%
  filter(!is.na(ind_cluster)) %>%
  group_by(FULL_CU_IN, cluster_name) %>%
  summarise(avg_score = mean(std_value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = cluster_name, values_from = avg_score, names_prefix = "GRP_")

# 3. Factors Correlated with Vulnerability ----------------------------------
cat("Identifying factors correlated with variation in vulnerability...\n")

# Join final vulnerability scores (from scores_tidy)
vuln_scores <- scores_tidy %>%
  filter(rcp == cat_rcp, period_code == cat_period, gcm == cat_gcm, method == sens_method_overall_base, category == "all") %>%
  select(FULL_CU_IN, total_vulnerability = score100_all)

# Integrate metadata
load(file.path(paths$CU, "cu_run.Rds")) # Loads cu_run
metadata_cu <- cu_run %>%
  select(FULL_CU_IN, FAZ_group, DFO_AREA, SMU_SIMPLE, SPECIES_NAME)

analysis_combined <- vuln_scores %>%
  left_join(pca_input_wide %>% select(-SPECIES_NAME, -CVIS_NAME), by = "FULL_CU_IN") %>%
  left_join(cu_cluster_scores, by = "FULL_CU_IN") %>%
  left_join(metadata_cu, by = "FULL_CU_IN")

# Quantitative Factor Importance (Linear Regression)
# How much do Species, SMU, Geography, and Indicator Groups explain total vulnerability?
lm_fit <- lm(total_vulnerability ~ SPECIES_NAME + SMU_SIMPLE + FAZ_group + .,
  data = analysis_combined %>% select(-FULL_CU_IN, -DFO_AREA)
)

# Extract relative importance (sum of squares)
anova_res <- anova(lm_fit)
importance_df <- as.data.frame(anova_res) %>%
  rownames_to_column("factor") %>%
  mutate(pct_variance = `Sum Sq` / sum(`Sum Sq`) * 100) %>%
  filter(factor != "Residuals") %>%
  arrange(desc(pct_variance))

# 4. Species-level Characterization -----------------------------------------
cat("Analyzing species-specific vulnerability drivers...\n")

species_drivers <- analysis_dat %>%
  group_by(SPECIES_NAME, indicator, name_short, category_long) %>%
  summarise(mean_std = mean(std_value, na.rm = TRUE), .groups = "drop") %>%
  group_by(SPECIES_NAME) %>%
  slice_max(order_by = mean_std, n = 5) %>%
  ungroup()

# 5. Visualizations ---------------------------------------------------------
cat("Generating streamlined visualizations...\n")

# 5a. Indicator Correlation Heatmap (Rich Aesthetics)
p_ind_cor <- {
  corrplot(cor_matrix,
    method = "color", order = "hclust",
    col = scico(200, palette = "roma"),
    tl.col = "black", tl.srt = 45, tl.cex = 0.7,
    addrect = n_ind_clusters, rect.col = "black", rect.lwd = 2,
    mar = c(0, 0, 1, 0), title = "Grouping of Vulnerability Indicators (Correlation Clusters)"
  )
}

# 5b. Factor Importance Plot
p_importance <- ggplot(importance_df, aes(x = reorder(factor, pct_variance), y = pct_variance, fill = pct_variance)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_fill_scico(palette = "batlow", direction = 1) +
  labs(
    title = "Factors Explaining Variation in Overall Vulnerability",
    subtitle = "Percentage of variance explained by Species, SMU, Geography, and Indicator Groups",
    x = NULL, y = "% Variance Explained"
  ) +
  theme_cvis()

# 5b3. SMU Vulnerability Variation (Boxplot)
p_smu_variation <- ggplot(
  analysis_combined %>% filter(!is.na(SMU_SIMPLE)),
  aes(x = reorder(SMU_SIMPLE, total_vulnerability, FUN = median), y = total_vulnerability, fill = SPECIES_NAME)
) +
  geom_boxplot(alpha = 0.8, outlier.size = 1) +
  coord_flip() +
  scale_fill_manual(values = species_palette, name = "Species") +
  labs(
    title = "Variation in Vulnerability by SMU",
    subtitle = "Distribution of scores (baseline) across CUs",
    x = NULL, y = "Total Vulnerability Score"
  ) +
  theme_cvis() +
  theme(axis.text.y = element_text(size = 8), legend.position = "none")

# 5b4. SMU-level Sensitivity to Variation Sources
cat("Analyzing SMU-level sensitivity to climate scenarios and GCMs...\n")

# Use CU-level deviations from 4b (all_devs_long should be accessible from overall_sensitivity)
load(file.path(paths$output, "sensitivity_analysis.Rdata")) # Ensure latest is loaded

# Pivot the deviations wide to join with metadata, then back to long for summarization
smu_sens_dat <- overall_sensitivity$deviations %>%
  filter(category == "all") %>%
  select(FULL_CU_IN, starts_with("raw_dev_")) %>%
  left_join(metadata_cu %>% select(FULL_CU_IN, SMU_SIMPLE, SPECIES_NAME), by = "FULL_CU_IN") %>%
  pivot_longer(cols = starts_with("raw_dev_"), names_to = "source", values_to = "raw_dev") %>%
  mutate(source = str_remove(source, "raw_dev_")) %>%
  # Filter to GCM and Scenario sources only for clarity
  filter(str_detect(source, "GCM|RCP"), !is.na(SMU_SIMPLE)) %>%
  group_by(SMU_SIMPLE, SPECIES_NAME, source) %>%
  summarise(mean_raw_dev = mean(raw_dev, na.rm = TRUE), .groups = "drop")

p_smu_sensitivity <- ggplot(smu_sens_dat, aes(x = source, y = reorder(SMU_SIMPLE, mean_raw_dev), fill = mean_raw_dev)) +
  geom_tile(color = "white") +
  scale_fill_scico(palette = "roma", midpoint = 0, name = "Mean Deviation\nfrom Baseline") +
  labs(
    title = "SMU Sensitivity to Climate Uncertainty",
    subtitle = "Change in mean vulnerability score under different GCMs/Scenarios",
    x = "Source of Variation", y = NULL
  ) +
  theme_cvis() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y = element_text(size = 7.5)
  )

# 5c. Species Risk Signatures
p_species_sig <- ggplot(species_drivers, aes(x = reorder(name_short, mean_std), y = mean_std, fill = SPECIES_NAME)) +
  geom_col() +
  facet_wrap(~SPECIES_NAME, scales = "free_y", ncol = 3) +
  coord_flip() +
  scale_fill_manual(values = species_palette) +
  labs(
    title = "Top 5 Vulnerability Drivers by Species",
    subtitle = "Indicators with highest baseline scores across CUs within each species",
    x = NULL, y = "Mean Standardized Score"
  ) +
  theme_cvis() +
  theme(
    strip.text = element_text(size = 9, margin = margin(2, 0, 2, 0)),
    axis.text.y = element_text(size = 7),
    axis.text.x = element_text(size = 7),
    plot.title = element_text(size = 12),
    plot.subtitle = element_text(size = 9)
  )

# 5d. Stand-alone Detailed PCA Biplot
pca_res <- prcomp(pca_ready, scale. = TRUE)

# Get loadings and scale them for biplot
pca_loadings <- as.data.frame(pca_res$rotation[, 1:2])
pca_loadings$indicator <- rownames(pca_loadings)
pca_loadings$name_short <- coalesce(short_name_map[pca_loadings$indicator], pca_loadings$indicator)

# Scale loadings to match PC scores range
mult <- min(
  (max(pca_res$x[, 1]) - min(pca_res$x[, 1]) / (max(pca_loadings$PC1) - min(pca_loadings$PC1))),
  (max(pca_res$x[, 2]) - min(pca_res$x[, 2]) / (max(pca_loadings$PC2) - min(pca_loadings$PC2)))
) * 0.8

pca_loadings <- pca_loadings %>%
  mutate(x_end = PC1 * mult, y_end = PC2 * mult) %>%
  filter(abs(PC1) > 0.15 | abs(PC2) > 0.15)

pca_output <- pca_input_wide %>%
  select(-SPECIES_NAME, -CVIS_NAME) %>% # Use metadata_cu as source of truth
  mutate(PC1 = pca_res$x[, 1], PC2 = pca_res$x[, 2]) %>%
  left_join(metadata_cu, by = "FULL_CU_IN") %>%
  left_join(vuln_scores, by = "FULL_CU_IN")

p_pca_detailed <- ggplot(pca_output, aes(x = PC1, y = PC2)) +
  geom_point(aes(color = total_vulnerability, shape = SPECIES_NAME), size = 3.5, alpha = 0.7) +
  geom_segment(
    data = pca_loadings, aes(x = 0, y = 0, xend = x_end, yend = y_end),
    arrow = arrow(length = unit(0.2, "cm")), color = "grey30", alpha = 0.8
  ) +
  geom_text(
    data = pca_loadings, aes(x = x_end * 1.1, y = y_end * 1.1, label = name_short),
    size = 3, fontface = "bold", color = "black"
  ) +
  scale_color_scico(palette = "lajolla", name = "Vulnerability", midpoint = 50) +
  scale_shape_manual(values = c(16, 17, 15, 18, 8), name = "Species") +
  labs(
    title = "Detailed CU Risk Profile PCA Biplot",
    subtitle = paste0(
      "PC1 (", round(pca_res$sdev[1]^2 / sum(pca_res$sdev^2) * 100, 1), "%) and PC2 (",
      round(pca_res$sdev[2]^2 / sum(pca_res$sdev^2) * 100, 1), "%) explain differences in risk profiles"
    ),
    x = "Principal Component 1", y = "Principal Component 2"
  ) +
  theme_cvis() +
  theme(
    legend.position = "right",
    plot.title = element_text(size = 14),
    legend.text = element_text(size = 8)
  )

# 5e. Cluster-level Sensitivity (Integration with 4b/4c)
cluster_sensitivity <- ind_sens_summary %>%
  left_join(ind_cluster_df %>% select(indicator, cluster_name), by = "indicator") %>%
  group_by(cluster_name, source_type) %>%
  summarise(mean_abs_dev = mean(mean_abs_dev, na.rm = TRUE), .groups = "drop")

p_sens_cluster <- ggplot(cluster_sensitivity, aes(x = cluster_name, y = mean_abs_dev, fill = source_type)) +
  geom_col(position = "dodge") +
  scale_fill_brewer(palette = "Pastel1", name = "Source of Uncertainty") +
  labs(
    title = "Uncertainty Sensitivity by Indicator Group",
    subtitle = "Sensitivity to GCM/Scenario variation",
    x = "Indicator Group", y = "Mean Absolute Deviation"
  ) +
  theme_cvis() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 7. Save Outputs and Graphics ----------------------------------------------
cat("Saving analysis results and figures...\n")

# Combine plots for the final report section
p_final_drivers <- (p_importance | p_smu_variation) / (p_sens_cluster | p_smu_sensitivity) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "CVIS Risk Drivers and Vulnerability Factors Analysis",
    theme = theme(plot.title = element_text(size = 18, face = "bold"))
  )

ggsave(file.path(paths$figures, "risk_drivers_combined_summary.png"), p_final_drivers, width = 14, height = 15)
ggsave(file.path(paths$figures, "species_risk_signatures.png"), p_species_sig, width = 10, height = 7)
ggsave(file.path(paths$figures, "pca_detailed_biplot.png"), p_pca_detailed, width = 11, height = 8)
ggsave(file.path(paths$figures, "smu_vulnerability_variation.png"), p_smu_variation, width = 10, height = 8)
ggsave(file.path(paths$figures, "smu_climate_sensitivity.png"), p_smu_sensitivity, width = 10, height = 8)

# Save R object
risk_drivers_analysis <- list(
  indicator_clusters = ind_cluster_df,
  factor_importance = importance_df,
  species_top_drivers = species_drivers,
  pca_result = pca_res,
  cluster_scores = cu_cluster_scores
)

save(risk_drivers_analysis, file = file.path(paths$output, "risk_drivers_analysis_final.Rdata"))

cat("\nAnalysis Complete.\n")
cat("Indicators grouped into", n_ind_clusters, "clusters based on baseline correlations.\n")
cat(
  "Top factor explaining vulnerability variation:", importance_df$factor[1],
  "(", round(importance_df$pct_variance[1], 1), "% variance explained).\n"
)
