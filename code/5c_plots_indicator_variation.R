################################################################################
#
# 5c_plots_indicator_variation.R
#
# Visualizes uncertainty propagation for individual indicators (raw values)
# using a LOLLIPOP style based on script 5a:
# - Baseline reference line
# - Lollipop stem (magnitude of future change)
# - Background bars (GCM uncertainty 10-90th percentile)
#
################################################################################

library(here)
setwd(here())
source(file.path(here(), "code", "0_setup.R"))

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

#----------------2. Lollipop Visualizations----------------
cat("Generating indicator uncertainty lollipop plots...\n")

categories_to_plot <- unique(plot_dat$category)

# Define aesthetic constants from 5a style
colors_lolli <- c(
    "GCM Range" = "grey85",
    "RCP 45" = "#33a02c",
    "RCP 85" = "#e31a1c"
)

for (cat in categories_to_plot) {
    cat_dat <- plot_dat %>% filter(category == cat)

    # Get descriptive category name
    cat_desc <- cat_label_map[cat]
    if (is.na(cat_desc)) cat_desc <- cat

    p <- ggplot(cat_dat, aes(y = scenario_label)) +
        # 1. Background GCM uncertainty range (wide bar)
        geom_segment(aes(x = qlowgcm, xend = qhighgcm, yend = scenario_label),
            color = colors_lolli["GCM Range"], linewidth = 6, alpha = 0.6
        ) +

        # 2. Baseline reference line
        geom_vline(aes(xintercept = baseline_mean), linetype = "dashed", color = "grey40") +

        # 3. Lollipop Stem (Change from baseline)
        geom_segment(aes(x = baseline_mean, xend = mean, yend = scenario_label, color = rcp),
            linewidth = 1.2
        ) +

        # 4. Lollipop Head (Future ensemble mean)
        geom_point(aes(x = mean, fill = rcp), shape = 21, size = 4, color = "white", stroke = 1) +
        facet_wrap(~indicator, scales = "free_x", ncol = 3) +

        # Labels and Style
        scale_color_manual(values = c("45" = colors_lolli["RCP 45"], "85" = colors_lolli["RCP 85"])) +
        scale_fill_manual(values = c("45" = colors_lolli["RCP 45"], "85" = colors_lolli["RCP 85"])) +
        labs(
            title = paste0("Indicator Uncertainty Propagation: ", cat_desc),
            subtitle = "Lollipops: Change from baseline mean; Grey bars: 10th-90th GCM Range",
            x = "Raw Indicator Value (Physical Units)",
            y = "Future Scenario",
            fill = "Scenario (RCP)", color = "Scenario (RCP)"
        ) +
        theme_cvis() +
        theme(
            strip.text = element_text(face = "bold"),
            legend.position = "bottom",
            panel.grid.major.y = element_line(color = "grey95")
        )

    # File naming
    fname <- paste0("indicator_uncertainty_lollipop_", cat, ".png")
    ggsave(file.path(fig_path, fname), p, width = 14, height = 10)
}

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
    source == "Model" ~ "Model",
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
             "Model_raw", "Model_mrd", "GCM_q10", "GCM_q90"))
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
    Model_raw = "Raw", Model_mrd = "MRD",
    GCM_q10 = "q10", GCM_q90 = "q90"
  ) %>%
  tab_spanner(label = "GCM 1", columns = starts_with("GCM1")) %>%
  tab_spanner(label = "GCM 4", columns = starts_with("GCM4")) %>%
  tab_spanner(label = "GCM 6", columns = starts_with("GCM6")) %>%
  tab_spanner(label = "RCP 8.5", columns = starts_with("RCP85")) %>%
  tab_spanner(label = "Period 5", columns = starts_with("Period5")) %>%
  tab_spanner(label = "Model", columns = starts_with("Model")) %>%
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
