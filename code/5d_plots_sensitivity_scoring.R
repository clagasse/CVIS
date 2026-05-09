################################################################################
#
# 5d_plots_sensitivity_scoring.R
#
# Score Sensitivity & Vulnerability Visualizations:
# 1. Summarizes overall and category-level vulnerability deviations (Violin plots).
# 2. Quantifies relative stability through Mean Rank Displacement (MRD) charts.
# 3. Visualizes indicator importance via Jackknife (Leave-one-out) leverage plots.
# 4. Illustrates rank consistency across scenarios with correlation heatmaps.
# 5. Performs multivariate profiling (PCA) and SMU-level sensitivity mapping.
# 6. Generates species-specific CU rank bump plots (Chinook, Sockeye, Coho).
#
################################################################################

# 1. Setup and Import ----
library(here)
setwd(here())
source(file.path(here(), "code", "0_setup.R"))
library(reshape2)
library(patchwork)
library(ggrepel)

# Create figures directory
sens_fig_path <- file.path(paths$figures, "sensitivity_analysis")
dir.create(sens_fig_path, showWarnings = FALSE, recursive = TRUE)

# Load sensitivity results
cat("Loading sensitivity results...\n")
load(file.path(paths$output, "sensitivity_analysis.Rdata")) # loads overall_sensitivity, all_devs_wide
load(file.path(paths$output, "indicator_sensitivity_summary.Rdata")) # loads score_sens_summary, ind_sens_summary

# Shared Colors
source_colors <- sens_source_palette

# 2. Overall Vulnerability Deviations (Directional) ----
cat("Plotting Overall Vulnerability Directional Impacts...\n")

# Clean and filter the deviation data
dev_raw <- overall_sensitivity$deviations %>%
    select(FULL_CU_IN, category, starts_with("raw_dev_")) %>%
    pivot_longer(cols = starts_with("raw_dev_"), names_to = "source_label", values_to = "raw_deviation") %>%
    mutate(
        source_label = str_remove(source_label, "raw_dev_"),
        source_type = case_when(
            str_detect(source_label, "^GCM") ~ "GCM",
            str_detect(source_label, "^RCP") ~ "Scenario",
            str_detect(source_label, "^Method") ~ "Method",
            str_detect(source_label, "^Model") ~ "dsmethod",
            str_detect(source_label, "^dsmethod") ~ "dsmethod",
            TRUE ~ "Other"
        ),
        source = case_when(
            source_type == "Method" ~ str_remove(source_label, "^Method_"),
            TRUE ~ source_label
        )
    )

# 2a. Overall Vulnerability Pane
p_dev_all <- dev_raw %>%
    filter(category == "all", source != "cube") %>%
    ggplot(aes(x = source, y = raw_deviation, fill = source)) +
    geom_violin(alpha = 0.8, draw_quantiles = c(0.25, 0.5, 0.75), linewidth = 1.1) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    scale_fill_manual(values = source_colors, na.value = "grey50", guide = "none") +
    labs(
        title = "Influence of Uncertainty on Overall Vulnerability",
        x = NULL,
        y = "Score Change (Raw Deviation)"
    ) +
    theme_cvis() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 2b. Category-Level Panes (subsetted)
target_cats <- c("fwrs", "migr", "gen")
p_dev_cats <- dev_raw %>%
    filter(category %in% target_cats, !source %in% c("avgall", "avgcube")) %>%
    ggplot(aes(x = source, y = raw_deviation, fill = source)) +
    geom_violin(alpha = 0.8, draw_quantiles = c(0.25, 0.5, 0.75), linewidth = 1, scale = "width") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    facet_wrap(~category, scales = "free_x", labeller = labeller(category = cat_label_map)) +
    scale_fill_manual(values = source_colors, na.value = "grey50", guide = "none") +
    labs(
        title = "Category-Level Score Sensitivity",
        x = "Variation Source",
        y = "Score Change (Raw Deviation)"
    ) +
    theme_cvis() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Combine using patchwork
p_dev_combined <- p_dev_all / p_dev_cats + plot_layout(heights = c(1, 1.2)) +
    plot_annotation(
        title = "Directional Influence of Uncertainty on Vulnerability Scores",
        subtitle = "Distribution of (Scenario Score - Baseline Score) across CUs; Bolder lines indicate median and quartiles",
        theme = theme(plot.title = element_text(face = "bold", size = 16))
    )

ggsave(file.path(sens_fig_path, "overall_vulnerability_deviations.png"), p_dev_combined, width = 12, height = 10)

# 3. Mean Rank Displacement (Relative Vulnerability Impacts) ----
cat("Plotting Mean Rank Displacement...\n")

# Score-level global summary (Overall per variation source)
score_mrd_global <- score_sens_summary %>% filter(SPECIES_NAME == "ALL")
p_mrd <- ggplot(score_mrd_global, aes(x = reorder(source, mrd), y = mrd, fill = source)) +
    geom_bar(stat = "identity") +
    facet_wrap(~category, scales = "free_x") +
    scale_fill_manual(values = source_colors, na.value = "grey50", guide = "none") +
    coord_flip() +
    labs(
        title = "Mean Rank Displacement (Vulnerability Scores)",
        subtitle = "Average shift in regional vulnerability rank compared to baseline scenario",
        x = "Variation Source",
        y = "Displacement in Ranks"
    ) +
    theme_cvis()

ggsave(file.path(sens_fig_path, "mean_rank_displacement.png"), p_mrd, width = 11, height = 8)


# 4. Jackknife Leverage Analysis (Indicator & Category Importance) ----
cat("Plotting Jackknife Influence...\n")

influence_summary <- overall_sensitivity$influence_summary # loads from 4b

# Filter for the main aggregated overall metric "avgall" (or "avg_all")
jack_global <- influence_summary %>%
    filter((method == "avgall" | method == "avg_all") & SPECIES_NAME == "ALL")

p_jack <- ggplot(jack_global, aes(x = reorder(excluded_element, mean_abs_dev), y = mean_raw_dev, color = parent_category)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_pointrange(aes(ymin = q10_raw_dev, ymax = q90_raw_dev), size = 0.6) +
    coord_flip() +
    facet_wrap(~excluded_type, scales = "free_y", ncol = 1) +
    scale_color_scico_d(palette = "roma", name = "Parent Category") +
    labs(
        title = "Jackknife Influence on Overall Vulnerability",
        subtitle = "Mean raw deviation in vulnerability score (0-100 scale) when removing an element.\nLines show 10th-90th percentile range across CUs. Negative = Risk Driver.",
        x = "Excluded Element (Indicator or Category)",
        y = "Raw Score Deviation (jk_score - base_score)"
    ) +
    theme_cvis()

ggsave(file.path(sens_fig_path, "jackknife_leverage_overall.png"), p_jack, width = 10, height = 8)



# 5. Rank Consistency (Correlation Heatmap) ----
cat("Plotting Correlation Heatmap...\n")

# Use matrix from 4c
cor_mat <- cor_matrices$all

cor_melted <- melt(cor_mat)

p_cor <- ggplot(cor_melted, aes(Var1, Var2, fill = value)) +
    geom_tile() +
    scale_fill_scico(palette = "batlow", direction = 1, limits = c(0.7, 1)) +
    geom_text(aes(label = round(value, 2)), color = "white", size = 3) +
    labs(
        title = "Rank Consistency Heatmap (Spearman Rho)",
        subtitle = "Category: Overall Vulnerability (all)",
        x = NULL, y = NULL, fill = "Rho"
    ) +
    theme_cvis() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(sens_fig_path, "rank_correlation_heat_all.png"), p_cor, width = 8, height = 7)


# 6. Vulnerability Score Sensitivity Summary Table (Manuscript) ----
cat("Generating vulnerability score sensitivity summary table...\n")

# Prepare the data for a wide table (Category x Source Metrics)
# We focus on the global results (all CUs) and key representative sources
target_v_sources <- c(
  "GCM1", "GCM4", "GCM6", 
  "RCP85_P3", "RCP45_P5", 
  "cube"
)

score_table_wide <- score_sens_summary %>%
  filter(SPECIES_NAME == "ALL") %>%
  mutate(table_source = case_when(
    source == "GCM1" ~ "GCM1",
    source == "GCM4" ~ "GCM4",
    source == "GCM6" ~ "GCM6",
    source == "RCP85_P3" ~ "RCP85",
    source == "RCP45_P5" ~ "Period5",
    source == "cube" ~ "Method",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(table_source)) %>%
  # Keep only the requested metrics
  select(category, table_source, mean_abs_dev, q10_abs_dev, q90_abs_dev, spearman_corr) %>%
  pivot_wider(
    names_from = table_source,
    values_from = c(mean_abs_dev, q10_abs_dev, q90_abs_dev, spearman_corr),
    names_glue = "{table_source}_{.value}"
  )

# Add descriptive labels and sort rows (Overall first, then categories)
score_gt_data <- score_table_wide %>%
  mutate(
    category_label = if_else(category == "all", "OVERALL VULNERABILITY", cat_label_map[category]),
    order = if_else(category == "all", 0, 1)
  ) %>%
  arrange(order, category_label) %>%
  select(category_label, everything(), -category, -order) %>%
  # Round for the manuscript
  mutate(across(where(is.numeric), ~ round(., 2)))

# Create gt table
score_sens_gt <- score_gt_data %>%
  gt() %>%
  tab_header(
    title = md("**Vulnerability Score Sensitivity Analysis Summary**"),
    subtitle = "Absolute Score Deviations and Rank Consistency (Spearman Rho) compared to Baseline"
  ) %>%
  cols_label(
    category_label = "Vulnerability Category",
    ends_with("mean_abs_dev") ~ "Mean",
    ends_with("q10_abs_dev") ~ "q10",
    ends_with("q90_abs_dev") ~ "q90",
    ends_with("spearman_corr") ~ "Rho"
  ) %>%
  # Group by source using spanners
  tab_spanner(label = "GCM 1", columns = starts_with("GCM1")) %>%
  tab_spanner(label = "GCM 4", columns = starts_with("GCM4")) %>%
  tab_spanner(label = "GCM 6", columns = starts_with("GCM6")) %>%
  tab_spanner(label = "RCP 8.5", columns = starts_with("RCP85")) %>%
  tab_spanner(label = "Period 5", columns = starts_with("Period5")) %>%
  tab_spanner(label = "Scoring (Cube)", columns = starts_with("Method")) %>%
  # Style
  tab_options(
    table.font.size = px(11),
    column_labels.font.weight = "bold",
    table.width = pct(100),
    data_row.padding = px(4)
  ) %>%
  opt_stylize(style = 1, color = "gray")

# 7. Risk Driver & Multivariate Analysis (Consolidated from 4d) ----
cat("Plotting Risk Drivers and Multivariate Analysis...\n")

# A. Setup labels and palettes
short_name_map <- c(
  "favchange" = "ENM Fav. Change", "cthr" = "FW Cumul. Threats", "tw8rate" = "Aug Temp Rate",
  "tw8proj" = "Aug Temp Proj", "flow8pdelta" = "Aug Flow Change", "flow18pdelta" = "Winter Flow Change",
  "fwres" = "FW Residency", "migrTproj" = "Migr. Temp Proj", "migrQpdelta" = "Migr. Flow Change",
  "migrdist" = "Migr. Distance", "SSTproj" = "SST Ocean Entry", "SSTrate" = "SST Change Rate",
  "CImpact" = "Marine Impacts", "CUstatus" = "WSP Status", "CUnmat" = "Abundance",
  "hetzyg" = "Heterozygosity", "genoff" = "Genomic Offset"
)

# Extract objects
rd_data <- risk_drivers_analysis


# 7a. Factor Importance Plot
p_importance <- ggplot(rd_data$factor_importance, aes(x = reorder(factor, pct_variance), y = pct_variance, fill = pct_variance)) +
  geom_col(show.legend = FALSE) + coord_flip() + scale_fill_scico(palette = "batlow", direction = 1) +
  labs(title = "Factors Explaining Variation in Overall Vulnerability", x = NULL, y = "% Variance Explained") +
  theme_cvis()

# 7b. SMU Vulnerability Variation (Boxplot)
p_smu_variation <- ggplot(rd_data$analysis_combined %>% filter(!is.na(SMU_SIMPLE)),
  aes(x = reorder(SMU_SIMPLE, total_vulnerability, FUN = median), y = total_vulnerability, fill = SPECIES_NAME)) +
  geom_boxplot(alpha = 0.8, outlier.size = 1) + coord_flip() +
  scale_fill_manual(values = species_palette, name = "Species") +
  labs(title = "Variation in Vulnerability by SMU", subtitle = "Distribution of scores (baseline) across CUs", x = NULL, y = "Total Vulnerability Score") +
  theme_cvis() + theme(axis.text.y = element_text(size = 8), legend.position = "none")

# 7c. SMU Sensitivity to Climate Uncertainty
p_smu_sensitivity <- ggplot(rd_data$smu_sensitivity, aes(x = source, y = reorder(SMU_SIMPLE, mean_raw_dev), fill = mean_raw_dev)) +
  geom_tile(color = "white") + scale_fill_scico(palette = "roma", midpoint = 0, name = "Mean Deviation") +
  labs(title = "SMU Sensitivity to Climate Uncertainty", x = "Source of Variation", y = NULL) +
  theme_cvis() + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))

# 7d. PCA Detailed Biplot
pca_res <- rd_data$pca_result
pca_loadings <- as.data.frame(pca_res$rotation[, 1:2])
pca_loadings$indicator <- rownames(pca_loadings)
pca_loadings$name_short <- coalesce(short_name_map[pca_loadings$indicator], pca_loadings$indicator)

# Scaling loadings to match PC scores range
mult <- 15 
pca_loadings <- pca_loadings %>% mutate(x_end = PC1 * mult, y_end = PC2 * mult) %>%
  filter(abs(PC1) > 0.15 | abs(PC2) > 0.15)

p_pca_detailed <- ggplot(rd_data$pca_plot_dat, aes(x = PC1, y = PC2)) +
  geom_point(aes(color = total_vulnerability, shape = SPECIES_NAME), size = 3.5, alpha = 0.7) +
  geom_segment(data = pca_loadings, aes(x = 0, y = 0, xend = x_end, yend = y_end),
               arrow = arrow(length = unit(0.2, "cm")), color = "grey30", alpha = 0.8) +
  geom_text(data = pca_loadings, aes(x = x_end * 1.1, y = y_end * 1.1, label = name_short),
            size = 3, fontface = "bold", color = "black") +
  scale_color_scico(palette = "lajolla", name = "Vulnerability", midpoint = 50) +
  scale_shape_manual(values = c(16, 17, 15, 18, 8), name = "Species") +
  labs(title = "Detailed CU Risk Profile PCA Biplot", x = "Principal Component 1", y = "Principal Component 2") +
  theme_cvis() + theme(legend.position = "right")

# Save plots
ggsave(file.path(sens_fig_path, "factor_importance_vulnerability.png"), p_importance, width = 10, height = 7)
ggsave(file.path(sens_fig_path, "smu_vulnerability_variation.png"), p_smu_variation, width = 10, height = 8)
ggsave(file.path(sens_fig_path, "smu_climate_sensitivity.png"), p_smu_sensitivity, width = 10, height = 8)
ggsave(file.path(sens_fig_path, "pca_detailed_biplot.png"), p_pca_detailed, width = 11, height = 8)


# 8. Species-Specific Analysis (Bump Plots) (Consolidated from 5f) ----
cat("Generating species-specific CU rank bump plots...\n")

spec_fig_path <- file.path(sens_fig_path, "species_focus")
dir.create(spec_fig_path, showWarnings = FALSE, recursive = TRUE)

# Prepare deviation data focusing on Chinook, Sockeye, and Coho
dev_full <- overall_sensitivity$deviations %>%
    filter(category == "all", SPECIES_NAME %in% c("Chinook", "Sockeye", "Coho")) %>%
    select(FULL_CU_IN, SPECIES_NAME, CVIS_NAME, SMU_SIMPLE, base_score, base_rank = base_rank_sp, starts_with("raw_dev_")) %>%
    pivot_longer(cols = starts_with("raw_dev_"), names_to = "source", names_prefix = "raw_dev_", values_to = "raw_dev") %>%
    filter(source %in% c("GCM1", "GCM4", "GCM6", "RCP85_P3", "RCP45_P5", "dsmethod", "Method_cube", "Method_avgcube", "Method_flag")) %>%
    # Add Baseline
    bind_rows(
        overall_sensitivity$deviations %>%
        filter(category == "all", SPECIES_NAME %in% c("Chinook", "Sockeye", "Coho")) %>%
        select(FULL_CU_IN, SPECIES_NAME, CVIS_NAME, SMU_SIMPLE, base_score, base_rank = base_rank_sp) %>%
        mutate(source = "Baseline", raw_dev = 0)
    ) %>%
    mutate(scen_score = base_score + raw_dev) %>%
    group_by(SPECIES_NAME, source) %>%
    mutate(scen_rank = rank(-scen_score, ties.method = "average", na.last = "keep")) %>%
    ungroup()

# Labels and ordering
source_order <- c("GCM1", "GCM4", "GCM6", "Baseline", "RCP45_P5", "RCP85_P3", "dsmethod", "Method_cube", "Method_avgcube", "Method_flag")
source_labels_bump <- c("CanESM2", "HadGEM2", "MPI-ESM", "Baseline", "RCP 4.5", "RCP 8.5", "DS Meth", "Cube-M", "Avg-Cube", "Flag")

dev_full <- dev_full %>%
    mutate(source = factor(source, levels = source_order, labels = source_labels_bump))

mrd_stats_sp <- overall_sensitivity$species_score_summary %>%
    filter(category == "all") %>%
    mutate(mrd_label = paste0("MRD: ", round(mrd_sp, 1))) %>%
    mutate(source = factor(source, levels = source_order, labels = source_labels_bump))

plot_species_bump <- function(sp) {
    sp_dat <- dev_full %>% filter(SPECIES_NAME == sp)
    sp_mrd <- mrd_stats_sp %>% filter(SPECIES_NAME == sp & !is.na(source) & source != "Baseline")
    n_cus <- length(unique(sp_dat$FULL_CU_IN))
    x_faces <- ifelse(source_labels_bump == "Baseline", "bold", "plain")
    
    ggplot(sp_dat, aes(x = source, y = scen_rank, group = FULL_CU_IN)) +
        geom_line(aes(color = SMU_SIMPLE), alpha = 0.5, linewidth = 1) +
        geom_label_repel(aes(label = FULL_CU_IN, color = SMU_SIMPLE), alpha = 1, size = 2, fontface = "bold", box.padding = 0.1) +
        geom_text(data = sp_mrd, aes(x = source, y = n_cus + 1, label = mrd_label, group = NULL), 
                  size = 2.5, fontface = "italic", vjust = 1, color = "grey30") +
        scale_y_reverse(breaks = 1:n_cus, expand = expansion(mult = c(0.1, 0.15))) +
        scale_color_brewer(palette = "Set1", name = "SMU") +
        labs(title = paste0(sp, ": Vulnerability Rank Stability"), subtitle = "Rank 1 = Highest Risk.", x = NULL, y = "In-Species Rank") +
        theme_minimal(base_size = 10) +
        theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_line(color = "grey90"),
              legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1, face = x_faces))
}

for (sp in c("Chinook", "Sockeye", "Coho")) {
    p <- plot_species_bump(sp)
    ggsave(file.path(spec_fig_path, paste0("rank_bump_", sp, ".png")), p, width = 12, height = 8)
}

cat("Sensitivity Analysis Plotting Complete.\n")
