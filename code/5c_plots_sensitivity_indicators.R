################################################################################
#
# 5c_plots_indicator_variation.R
#
# Indicator Sensitivity Visualizations:
# 1. Quantifies indicator-level sensitivity (Mean Absolute Deviation) 
#    across all climate and downscaling variations.
# 2. Visualizes directional shifts in raw indicator values (Violin plots).
# 3. Compares raw vs. standardized risk responses (XY sensitivity).
# 4. Analyzes indicator redundancy through correlation and cluster visualizations.
# 5. Generates the comprehensive Indicator Sensitivity Summary table.
#
################################################################################

library(here)
setwd(here())
source(file.path(here(), "code", "0_setup.R"))
library(corrplot)
library(patchwork)

# Create figures directory
fig_path <- file.path(paths$figures, "indicator_uncertainty")
dir.create(fig_path, showWarnings = FALSE, recursive = TRUE)

# Load indicator data
cat("Loading indicator data...\n")
load(file.path(paths$output, "scoring_results.Rdata")) # loads all_std_long

#----------------1. Data Processing----------------
cat("Processing indicator variation data...\n")

# Use standard RCP/Period codes for baseline
# We use RCP 45, Period 0 as the reference
rcp_ref <- "45"
period_ref <- "0"

# 1a. Extract Baseline Reference Values (ensemble mean per indicator, averaged across CUs)
baseline_vals <- all_std_long %>%
    left_join(tbl_standardize %>% select(abbrev, dsmodel_baseline_ind = dsmodel_baseline), by = c("indicator" = "abbrev")) %>%
    filter(
        rcp == rcp_ref, period_code == period_ref, gcm == "9",
        dsmodel == dsmodel_baseline_ind
    ) %>%
    group_by(indicator) %>%
    summarise(baseline_mean = mean(std_value, na.rm = TRUE), .groups = "drop")

# 1b. Extract Future Variation Data
# Ensemble model (GCM 9) has std_value per rcp/period
future_dat <- all_std_long %>%
    left_join(tbl_standardize %>% select(abbrev, dsmodel_baseline_ind = dsmodel_baseline), by = c("indicator" = "abbrev")) %>%
    filter(
        gcm == "9", period_code != "0",
        dsmodel == dsmodel_baseline_ind
    )

# Aggregate future ensemble mean across all CUs
future_agg <- future_dat %>%
    group_by(indicator, category, rcp, period_code) %>%
    summarise(
        mean = mean(std_value, na.rm = TRUE),
        qlowgcm = quantile(std_value, qlgcm, na.rm = TRUE),
        qhighgcm = quantile(std_value, qhgcm, na.rm = TRUE),
        .groups = "drop"
    )

# 1c. Combine Baseline and Future
plot_dat <- future_agg %>%
    left_join(baseline_vals, by = "indicator") %>%
    mutate(
        scenario_label = paste0("RCP ", rcp, " P", period_code),
        # Categorize direction of change for coloring stems
        direction = if_else(mean > baseline_mean, "High Risk", "Low Risk")
    )

# Filter for indicators that actually have variation
plot_dat <- plot_dat %>%
    filter(!is.na(qlowgcm) | !is.na(qhighgcm))



#----------------3. Integrated Indicator Sensitivity Plots (from 5e)----------------
cat("Generating additional indicator sensitivity visualizations...\n")

# Shared Colors
source_colors <- sens_source_palette

# 3a. Indicator-Level Sensitivity (MAD Bar Chart)
p_ind_sens <- ggplot(ind_sens_summary, aes(x = reorder(indicator, mean_abs_dev, mean), y = mean_abs_dev, fill = source)) +
    geom_bar(stat = "identity", position = "dodge") +
    coord_flip() +
    facet_wrap(~category, scales = "free_y") +
    scale_fill_manual(values = source_colors, na.value = "grey50", name = "Source of variation") +
    labs(
        title = "Indicator Sensitivity to Climate and Downscaling Method Variation",
        subtitle = "Average absolute deviation from baseline across all CUs",
        x = "Indicator",
        y = "Mean Absolute Deviation"
    ) +
    theme_cvis() +
    theme(legend.position = "bottom")

ggsave(file.path(fig_path, "indicator_level_sensitivity.png"), p_ind_sens, width = 12, height = 8)

# 3b. Indicator Directional Shift Violins
cat("Plotting Indicator Directional Shift Violins...\n")

# Re-calculate absolute raw values for each CU across all relevant sources
ind_shift_cus <- overall_sensitivity$indicator_metrics %>%
    filter(FULL_CU_IN != "ALL") %>%
    mutate(
        val_Baseline = base_raw_mean,
        val_GCM1 = base_raw_mean + raw_dev_GCM1,
        val_GCM4 = base_raw_mean + raw_dev_GCM4,
        val_GCM6 = base_raw_mean + raw_dev_GCM6,
        val_RCP45_P5 = base_raw_mean + raw_dev_RCP45_P5,
        val_RCP85_P3 = base_raw_mean + raw_dev_RCP85_P3,
        val_RCP85_P5 = base_raw_mean + raw_dev_RCP85_P5,
        val_dsmethod = base_raw_mean + raw_dev_dsmethod
    ) %>%
    select(FULL_CU_IN, indicator, category, base_raw_mean, starts_with("val_")) %>%
    pivot_longer(cols = starts_with("val_"), names_to = "source", names_prefix = "val_", values_to = "val") %>%
    filter(!is.na(val)) %>%
    group_by(indicator, source) %>%
    mutate(has_variation = source == "Baseline" | any(abs(val - base_raw_mean) > 1e-10, na.rm = TRUE)) %>%
    filter(has_variation) %>%
    group_by(indicator) %>% filter(n_distinct(source) > 1) %>% ungroup() %>%
    mutate(source = factor(source, levels = rev(c("Baseline", "GCM1", "GCM4", "GCM6", "RCP45_P5", "RCP85_P3", "RCP85_P5", "dsmethod"))))

baseline_refs <- ind_shift_cus %>% group_by(indicator) %>% summarise(ref_mean = mean(base_raw_mean, na.rm = TRUE), .groups = "drop")
ind_label_units <- tbl_indicators %>% mutate(facet_label = paste0(abbrev, "\n(", unit, ")")) %>% select(abbrev, facet_label) %>% tibble::deframe()
shift_colors <- c("Baseline" = "black", source_colors)

p_shift <- ggplot(ind_shift_cus, aes(y = source, x = val, fill = source, color = source)) +
    geom_vline(data = baseline_refs, aes(xintercept = ref_mean), linetype = "dashed", color = "grey30", alpha = 0.6) +
    geom_violin(alpha = 0.7, scale = "width", draw_quantiles = c(0.25, 0.5, 0.75), linewidth = 0.6) +
    facet_wrap(~indicator, scales = "free_x", ncol = 5, labeller = labeller(indicator = ind_label_units)) +
    scale_fill_manual(values = shift_colors, na.value = "grey50", guide = "none") +
    scale_color_manual(values = shift_colors, na.value = "grey50", guide = "none") +
    labs(x = "Raw Indicator Value (units vary)", y = "Source of variation") +
    theme_cvis() + theme(strip.text = element_text(face = "bold", size = 8.5), axis.text.y = element_text(size = 8))

ggsave(file.path(fig_path, "indicator_directional_shifts_violin.png"), p_shift, width = 18, height = 12)

# 3c. Indicator XY Sensitivity Plot (Raw vs. Standardized)
cat("Plotting Indicator XY Sensitivity (Raw vs. Standardized)...\n")
ind_xy_dat <- overall_sensitivity$indicator_metrics %>%
  filter(FULL_CU_IN != "ALL") %>%
  filter(!is.na(abs_std_dev_GCM1)) %>%
  mutate(
    valraw_Baseline = base_raw_mean, valstd_Baseline = base_std_mean,
    valraw_GCM1 = base_raw_mean + raw_dev_GCM1, valstd_GCM1 = base_std_mean + std_dev_GCM1,
    valraw_GCM4 = base_raw_mean + raw_dev_GCM4, valstd_GCM4 = base_std_mean + std_dev_GCM4,
    valraw_GCM6 = base_raw_mean + raw_dev_GCM6, valstd_GCM6 = base_std_mean + std_dev_GCM6,
    valraw_dsmethod = base_raw_mean + raw_dev_dsmethod, valstd_dsmethod = base_std_mean + std_dev_dsmethod
  ) %>%
  select(FULL_CU_IN, SPECIES_NAME, category, indicator, starts_with("valraw_"), starts_with("valstd_")) %>%
  pivot_longer(cols = starts_with("valraw_") | starts_with("valstd_"), names_to = c(".value", "source"), names_sep = "_") %>%
  rename(raw = valraw, std = valstd) %>% filter(!is.na(raw))

source_shapes <- c("Baseline" = 16, "GCM1" = 17, "GCM4" = 18, "GCM6" = 15, "dsmethod" = 13)
xy_colors <- c("Baseline" = "black", source_colors)

p_xy <- ggplot(ind_xy_dat, aes(x = raw, y = std, color = source, shape = source)) +
  geom_point(alpha = 0.3, size = 1.2) +
  stat_ellipse(aes(group = source), level = 0.90, linetype = "dashed", linewidth = 0.4) +
  stat_summary(fun = mean, geom = "point", size = 4, alpha = 1, stroke = 1.5) +
  facet_wrap(~indicator, scales = "free", ncol = 4) +
  scale_color_manual(values = xy_colors) + scale_shape_manual(values = source_shapes) +
  labs(title = "Indicator Sensitivity: Raw vs. Standardized Risk Response", 
       subtitle = "Points show CUs; Large points show mean result. Ellipses span 90% of distribution.",
       x = "Raw Indicator Value", y = "Standardized Risk Score (0-1)") +
  theme_cvis() + theme(legend.position = "bottom", strip.text = element_text(size = 8))

ggsave(file.path(fig_path, "indicator_xy_sensitivity_gcm_model.png"), p_xy, width = 16, height = 12)

# 3d. Indicator Redundancy and Correlations
cat("Plotting Indicator Redundancy and Correlation Clusters...\n")

# Indicator Redundancy (Pie Chart)
png(file.path(fig_path, "indicator_redundancy_corrplot.png"), width = 1000, height = 1000, res = 120)
cor_matrix_ind <- overall_sensitivity$correlation_indicators
corrplot(cor_matrix_ind, method = "pie", type = "lower", title = "Indicator Pearson Correlation")
dev.off()

# Indicator Correlation Clusters (Heatmap)
png(file.path(fig_path, "indicator_correlation_clusters.png"), width = 1000, height = 1000, res = 120)
rd_data <- risk_drivers_analysis
corrplot(rd_data$cor_matrix_pearson,
  method = "color", order = "hclust", col = scico(200, palette = "roma"),
  tl.col = "black", tl.srt = 45, tl.cex = 0.7, addrect = 5, rect.col = "black", rect.lwd = 2,
  mar = c(0, 0, 1, 0), title = "Indicator Correlation Clusters"
)
dev.off()

#----------------3. Manuscript Summary Table----------------
cat("Generating manuscript-ready summary table of indicator sensitivity...\n")

# Load sensitivity summary data from 4c
load(file.path(paths$output, "indicator_sensitivity_summary.Rdata"))

# 3a. Prepare baseline means for all indicators (including non-climate ones)
# We use the same baseline logic as 4b/4c
baseline_means <- all_std_long %>%
  filter(rcp %in% c("0", sens_rcp_base),
         period_code %in% c("0", sens_period_base),
         gcm %in% c("0", "9", sens_gcm_base)) %>%
  left_join(tbl_standardize %>% select(abbrev, dsmodel_baseline_ind = dsmodel_baseline), by = c("indicator" = "abbrev")) %>%
  filter(dsmodel == dsmodel_baseline_ind) %>%
  filter(stat == "mean") %>%
  group_by(indicator) %>%
  summarise(Baseline_Mean = mean(as.numeric(value), na.rm = TRUE), .groups = "drop")

# 3b. Re-map source names for the table columns
# Period 5 is the shift from P3 to P5 under RCP 45
# RCP85 is the shift from RCP 45 to 85 under Period 3
ind_sens_table_long <- ind_sens_summary %>%
  mutate(table_source = case_when(
    source == "GCM1" ~ "GCM1",
    source == "GCM4" ~ "GCM4",
    source == "GCM6" ~ "GCM6",
    source == "RCP85_P3" ~ "RCP85",
    source == "RCP45_P5" ~ "Period5",
    source == "Model" ~ "dsmethod",
    source == "dsmethod" ~ "dsmethod",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(table_source))

# Pivot to wide format with raw deviation and MRD columns
ind_sens_table_wide <- ind_sens_table_long %>%
  select(indicator, table_source, mean_raw_dev, mrd) %>%
  pivot_wider(
    names_from = table_source,
    values_from = c(mean_raw_dev, mrd),
    names_glue = "{table_source}_{.value}"
  ) %>%
  rename_with(~ str_replace(., "mean_raw_dev", "raw"), contains("mean_raw_dev"))

# Calculate GCM spread (q10 and q90) across the individual GCM means for each indicator
gcm_spread <- ind_sens_summary %>%
  filter(source_type == "GCM") %>%
  group_by(indicator) %>%
  summarise(
    GCM_q10 = quantile(mean_raw_dev, 0.1, na.rm = TRUE),
    GCM_q90 = quantile(mean_raw_dev, 0.9, na.rm = TRUE),
    .groups = "drop"
  )

# 3c. Combine into final wide summary table
indicator_sensitivity_table <- tbl_indicators %>%
  select(indicator = abbrev) %>%
  left_join(baseline_means, by = "indicator") %>%
  left_join(ind_sens_table_wide, by = "indicator") %>%
  left_join(gcm_spread, by = "indicator") %>%
  select(
    indicator,
    Baseline_Mean,
    any_of(c("GCM1_raw", "GCM1_mrd", "GCM4_raw", "GCM4_mrd", "GCM6_raw", "GCM6_mrd",
             "RCP85_raw", "RCP85_mrd", "Period5_raw", "Period5_mrd", 
             "dsmethod_raw", "dsmethod_mrd", "GCM_q10", "GCM_q90"))
  )

# 3d. Format as gt table for manuscript
gt_data <- indicator_sensitivity_table %>%
  left_join(tbl_indicators %>% select(indicator = abbrev, name, category_code = category), by = "indicator") %>%
  mutate(category = cat_label_map[category_code]) %>%
  select(category, indicator, name, Baseline_Mean, everything(), -category_code) %>%
  mutate(across(where(is.numeric), ~ round(., 2)))

ind_sens_gt <- gt_data %>%
  group_by(category) %>%
  gt(rowname_col = "indicator") %>%
  tab_header(
    title = md("**Summary of Indicator Sensitivity Analysis**"),
    subtitle = "Mean Raw Deviation and Mean Rank Displacement (MRD) from Baseline"
  ) %>%
  cols_label(
    name = "Indicator Description",
    Baseline_Mean = "Baseline",
    GCM1_raw = "Raw", GCM1_mrd = "MRD",
    GCM4_raw = "Raw", GCM4_mrd = "MRD",
    GCM6_raw = "Raw", GCM6_mrd = "MRD",
    RCP85_raw = "Raw", RCP85_mrd = "MRD",
    Period5_raw = "Raw", Period5_mrd = "MRD",
    dsmethod_raw = "Raw", dsmethod_mrd = "MRD",
    GCM_q10 = "q10", GCM_q90 = "q90"
  ) %>%
  tab_spanner(label = "GCM 1", columns = starts_with("GCM1")) %>%
  tab_spanner(label = "GCM 4", columns = starts_with("GCM4")) %>%
  tab_spanner(label = "GCM 6", columns = starts_with("GCM6")) %>%
  tab_spanner(label = "RCP 8.5", columns = starts_with("RCP85")) %>%
  tab_spanner(label = "Period 5", columns = starts_with("Period5")) %>%
  tab_spanner(label = "Downscaling method", columns = starts_with("dsmethod")) %>%
  tab_spanner(label = "GCM Spread", columns = c("GCM_q10", "GCM_q90")) %>%
  fmt_missing(everything(), missing_text = "—") %>%
  tab_options(
    table.font.size = px(11),
    column_labels.font.weight = "bold",
    row_group.font.weight = "bold",
    table.width = pct(100),
    data_row.padding = px(3)
  ) %>%
  opt_stylize(style = 6, color = "gray")

# 3e. Save Outputs
cat("Saving sensitivity table to output directory...\n")
write_csv(indicator_sensitivity_table, file.path(paths$output, "Table_Indicator_Sensitivity.csv"))
gtsave(ind_sens_gt, file.path(paths$output, "Table_Indicator_Sensitivity.html"))

if (interactive()) print(ind_sens_gt)

cat("Script 5c complete. Plots and summary table generated.\n")
