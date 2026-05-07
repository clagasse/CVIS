################################################################################
#
# 5e_plots_sensitivity_analysis.R
#
# Analyzes and summarizes sensitivity of indicator and overall vulnerability
# scores based on results from 4b. Evaluates key vulnerability drivers,
# directional influence of methodological/climate choices, and rank impacts.
#
################################################################################

# 1. Setup and Import ----
library(here)
setwd(here())
source(file.path(here(), "code", "0_setup.R"))
library(reshape2)
library(patchwork)
library(corrplot)

# Create figures directory
sens_fig_path <- file.path(paths$figures, "sensitivity_analysis")
dir.create(sens_fig_path, showWarnings = FALSE, recursive = TRUE)

# Load sensitivity results
cat("Loading sensitivity results...\n")
load(file.path(paths$output, "sensitivity_analysis.Rdata")) # loads overall_sensitivity, all_devs_wide
load(file.path(paths$output, "indicator_sensitivity_summary.Rdata")) # loads score_sens_summary, ind_sens_summary

# Shared Colors
source_colors <- sens_source_palette

# 2. Indicator-Level Sensitivity (Environmental Uncertainty) ----
cat("Plotting Indicator-Level Sensitivity...\n")

# Use reformatted summary from 4c
p_ind_sens <- ggplot(ind_sens_summary, aes(x = reorder(indicator, mean_abs_dev, mean), y = mean_abs_dev, fill = source)) +
    geom_bar(stat = "identity", position = "dodge") +
    coord_flip() +
    facet_wrap(~category, scales = "free_y") +
    scale_fill_manual(values = source_colors, na.value = "grey50", name = "Source of variation") +
    labs(
        title = "Indicator Sensitivity to Climate and Model Variation",
        subtitle = "Average absolute deviation from baseline across all CUs",
        x = "Indicator",
        y = "Mean Absolute Deviation"
    ) +
    theme_cvis() +
    theme(legend.position = "bottom")

ggsave(file.path(sens_fig_path, "indicator_level_sensitivity.png"), p_ind_sens, width = 12, height = 8)

# 1b. Indicator Shift Plot (Baseline to Future)
cat("Plotting Indicator Directional Shift Violins...\n")

# A. Re-calculate absolute raw values for each CU across all relevant sources
ind_shift_cus <- overall_sensitivity$indicator_metrics %>%
    filter(FULL_CU_IN != "ALL") %>%
    # Calculations for absolute values relative to the baseline
    mutate(
        val_Baseline = base_raw_mean,
        val_GCM1 = base_raw_mean + raw_dev_GCM1,
        val_GCM4 = base_raw_mean + raw_dev_GCM4,
        val_GCM6 = base_raw_mean + raw_dev_GCM6,
        val_RCP45_P5 = base_raw_mean + raw_dev_RCP45_P5,
        val_RCP85_P3 = base_raw_mean + raw_dev_RCP85_P3,
        val_RCP85_P5 = base_raw_mean + raw_dev_RCP85_P5,
        val_Model = base_raw_mean + raw_dev_Model
    ) %>%
    select(FULL_CU_IN, indicator, category, base_raw_mean, starts_with("val_")) %>%
    # Pivot to long format for source
    pivot_longer(
        cols = starts_with("val_"),
        names_to = "source",
        names_prefix = "val_",
        values_to = "val"
    ) %>%
    # A. Remove rows where source variation does not apply (NAs preserved from 4b)
    filter(!is.na(val)) %>%
    # B. Stricter filter: Remove sources that are EXACT duplicates of Baseline for a given indicator
    # (Happens if missing scenario data was coalesced to 0 or if the indicator is static for that source)
    group_by(indicator, source) %>%
    mutate(has_variation = source == "Baseline" | any(abs(val - base_raw_mean) > 1e-10, na.rm = TRUE)) %>%
    filter(has_variation) %>%
    # C. Only keep indicators that have at least one non-baseline source left
    group_by(indicator) %>%
    filter(n_distinct(source) > 1) %>%
    ungroup() %>%
    mutate(source = factor(source, levels = rev(c("Baseline", "GCM1", "GCM4", "GCM6", "RCP45_P5", "RCP85_P3", "RCP85_P5", "Model"))))

# B. Calculate single reference mean per indicator for the dashed line
baseline_refs <- ind_shift_cus %>%
    group_by(indicator) %>%
    summarise(ref_mean = mean(base_raw_mean, na.rm = TRUE), .groups = "drop")

# C. Create label map with units for facets
ind_label_units <- tbl_indicators %>%
    mutate(facet_label = paste0(abbrev, "\n(", unit, ")")) %>%
    select(abbrev, facet_label) %>%
    tibble::deframe()

# Expanded color palette
shift_colors <- c("Baseline" = "black", source_colors)

p_shift <- ggplot(ind_shift_cus, aes(y = source, x = val, fill = source, color = source)) +
    # Add Static Baseline reference line (centered on overall indicator mean)
    geom_vline(data = baseline_refs, aes(xintercept = ref_mean), linetype = "dashed", color = "grey30", alpha = 0.6) +
    # Violins showing the distribution shift
    geom_violin(alpha = 0.7, scale = "width", draw_quantiles = c(0.25, 0.5, 0.75), linewidth = 0.6) +
    facet_wrap(~indicator, scales = "free_x", ncol = 5, labeller = labeller(indicator = ind_label_units)) +
    scale_fill_manual(values = shift_colors, na.value = "grey50", guide = "none") +
    scale_color_manual(values = shift_colors, na.value = "grey50", guide = "none") +
    labs(
        title = NULL,
        subtitle = NULL,
        x = "Raw Indicator Value (units vary)",
        y = "Source of variation"
    ) +
    theme_cvis() +
    theme(
        strip.text = element_text(face = "bold", size = 8.5),
        axis.text.y = element_text(size = 8)
    )

ggsave(file.path(sens_fig_path, "indicator_directional_shifts_violin.png"), p_shift, width = 18, height = 12)

# 3. Overall Vulnerability Deviations (Directional) ----
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
            str_detect(source_label, "^Model") ~ "Model",
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

# 4. Mean Rank Displacement (Relative Vulnerability Impacts) ----
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

# Species-level MRD
score_mrd_species <- score_sens_summary %>% filter(SPECIES_NAME != "ALL")
p_mrd_sp <- ggplot(score_mrd_species, aes(x = reorder(source, mrd), y = mrd, fill = source)) +
    geom_bar(stat = "identity") +
    facet_grid(SPECIES_NAME ~ category, scales = "free_x") +
    scale_fill_manual(values = source_colors, na.value = "grey50", guide = "none") +
    coord_flip() +
    labs(
        title = "Mean Rank Displacement by Species",
        subtitle = "Average shift in regional vulnerability rank compared to baseline scenario",
        x = "Variation Source",
        y = "Displacement in Ranks"
    ) +
    theme_cvis()

ggsave(file.path(sens_fig_path, "mean_rank_displacement_species.png"), p_mrd_sp, width = 14, height = 10)


# Also plot indicator-level MRD for comparison
p_ind_mrd <- ggplot(ind_sens_summary, aes(x = reorder(indicator, mrd, mean), y = mrd, fill = source)) +
    geom_bar(stat = "identity", position = "dodge") +
    coord_flip() +
    facet_wrap(~category, scales = "free_y") +
    scale_fill_manual(values = source_colors, na.value = "grey50", name = "Source of variation") +
    labs(
        title = "Mean Rank Displacement (Individual Indicators)",
        subtitle = "Relative spatial shift in indicator values compared to baseline",
        x = "Indicator",
        y = "Displacement in Ranks"
    ) +
    theme_cvis() +
    theme(legend.position = "bottom")

ggsave(file.path(sens_fig_path, "indicator_rank_displacement.png"), p_ind_mrd, width = 12, height = 8)

# 5. Jackknife Leverage Analysis (Indicator & Category Importance) ----
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

jack_species <- influence_summary %>%
    filter((method == "avgall" | method == "avg_all") & SPECIES_NAME != "ALL")

p_jack_sp <- ggplot(jack_species, aes(x = reorder(excluded_element, mean_abs_dev), y = mean_raw_dev, color = parent_category)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_pointrange(aes(ymin = q10_raw_dev, ymax = q90_raw_dev), size = 0.4) +
    coord_flip() +
    facet_grid(SPECIES_NAME ~ excluded_type, scales = "free_y") +
    scale_color_scico_d(palette = "roma", name = "Parent Category") +
    labs(
        title = "Jackknife Influence by Species",
        subtitle = "Mean raw deviation in vulnerability score when removing an element.",
        x = "Excluded Element",
        y = "Raw Score Deviation"
    ) +
    theme_cvis()

ggsave(file.path(sens_fig_path, "jackknife_leverage_species.png"), p_jack_sp, width = 14, height = 12)

# 6. Rank Consistency (Correlation Heatmap) ----
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

# 7. Indicator Correlations (Redundancy) ----
cat("Plotting Indicator Redundancy (Corrplot)...\n")

png(file.path(sens_fig_path, "indicator_redundancy_corrplot.png"), width = 1000, height = 1000, res = 120)

cor_matrix_ind <- overall_sensitivity$correlation_indicators

# Use corrplot for visualization
corrplot(cor_matrix_ind,
         method = "pie",
         type = "lower",
         title = "Indicator Pearson Correlation"
)

dev.off()

# 8. Vulnerability Score Sensitivity Summary Table (Manuscript) ----
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
    id_cols = category,
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

# 9. Indicator XY Sensitivity Plot (Raw vs. Standardized) ----
cat("Plotting Indicator XY Sensitivity (Raw vs. Standardized)...\n")

# Reconstruct absolute raw and std values from deviations in indicator_metrics
# Focus on individual CUs (not the 'ALL' summary row)
ind_xy_dat <- overall_sensitivity$indicator_metrics %>%
  filter(FULL_CU_IN != "ALL") %>%
  # Filter to indicators that actually have GCM variation (not static)
  # Look for non-zero mean absolute deviation for GCMs
  filter(!is.na(abs_std_dev_GCM1)) %>%
  # Calculations for absolute values relative to the baseline
  mutate(
    # Baseline
    valraw_Baseline = base_raw_mean,
    valstd_Baseline = base_std_mean,
    # GCMs
    valraw_GCM1 = base_raw_mean + raw_dev_GCM1,
    valstd_GCM1 = base_std_mean + std_dev_GCM1,
    valraw_GCM4 = base_raw_mean + raw_dev_GCM4,
    valstd_GCM4 = base_std_mean + std_dev_GCM4,
    valraw_GCM6 = base_raw_mean + raw_dev_GCM6,
    valstd_GCM6 = base_std_mean + std_dev_GCM6,
    # Model variants
    valraw_Model = base_raw_mean + raw_dev_Model,
    valstd_Model = base_std_mean + std_dev_Model
  ) %>%
  select(FULL_CU_IN, SPECIES_NAME, category, indicator, starts_with("valraw_"), starts_with("valstd_")) %>%
  # Pivot to long format for source
  pivot_longer(
    cols = starts_with("valraw_") | starts_with("valstd_"),
    names_to = c(".value", "source"),
    names_sep = "_"
  ) %>%
  rename(raw = valraw, std = valstd) %>%
  # Filter out Model rows if there was no model variation for that indicator
  filter(!is.na(raw))

# Define shapes and colors for sources
source_shapes <- c("Baseline" = 16, "GCM1" = 17, "GCM4" = 18, "GCM6" = 15, "Model" = 13)
xy_colors <- c("Baseline" = "black", source_colors)

# Plotting XY distribution
p_xy <- ggplot(ind_xy_dat, aes(x = raw, y = std, color = source, shape = source)) +
  # Individual CU points
  geom_point(alpha = 0.3, size = 1.2) +
  # Ellipses to circle the distribution for each source
  stat_ellipse(aes(group = source), level = 0.90, linetype = "dashed", linewidth = 0.4) +
  # Large points for the mean of each source
  stat_summary(fun = mean, geom = "point", size = 4, alpha = 1, stroke = 1.5) +
  # Facet by indicator since raw units and ranges vary
  facet_wrap(~indicator, scales = "free", ncol = 4) +
  scale_color_manual(values = xy_colors) +
  scale_shape_manual(values = source_shapes) +
  labs(
    title = "Indicator Sensitivity: Raw vs. Standardized Risk Response",
    subtitle = "Points show CUs across GCM & Model variations; Large points show the mean result. Ellipses span 90% of CU distribution.",
    x = "Raw Indicator Value (standardized units vary by indicator)",
    y = "Standardized Risk Score (0-1)",
    color = "Source of variation",
    shape = "Source of variation"
  ) +
  theme_cvis() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 8)
  )

ggsave(file.path(sens_fig_path, "indicator_xy_sensitivity_gcm_model.png"), p_xy, width = 16, height = 12)

cat("Sensitivity plots and summary tables saved to output/figures/sensitivity_analysis/\n")
