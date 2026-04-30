################################################################################
#
# 5e_plots_sensitivity_analysis.R
#
# Analyzes and summarizes sensitivity of indicator and overall vulnerability
# scores based on results from 4b. Evaluates key vulnerability drivers,
# directional influence of methodological/climate choices, and rank impacts.
#
################################################################################

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

#-------------------------------------------------------------------------
# 1. Indicator-Level Sensitivity (Environmental Uncertainty)
#-------------------------------------------------------------------------
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
cat("Plotting Indicator Directional Shift Details...\n")

# Prepare data for plotting baseline vs scenario values
ind_shift_dat <- ind_sens_summary %>%
    mutate(
        scen_mean = base_mean + mean_raw_dev,
        scen_q10 = base_mean + q10_raw_dev,
        scen_q90 = base_mean + q90_raw_dev
    )

p_shift <- ggplot(ind_shift_dat, aes(y = source, color = source)) +
    # Add Baseline reference line
    geom_vline(aes(xintercept = base_mean), linetype = "dashed", color = "grey30", alpha = 0.6) +
    # Points and Error bars for Scenarios
    geom_pointrange(aes(x = scen_mean, xmin = scen_q10, xmax = scen_q90), 
                    size = 0.6, fatten = 4) +
    facet_wrap(~indicator, scales = "free_x", ncol = 3) +
    scale_color_manual(values = source_colors, na.value = "grey50", guide = "none") +
    labs(
        title = "Indicator Shifts: Baseline vs Climate/Model Scenarios",
        subtitle = "Points show mean raw value across CUs; error bars show 10th-90th percentile spread.\nDashed line represents the categorical baseline mean.",
        x = "Raw Indicator Value",
        y = "Variation Source"
    ) +
    theme_cvis() +
    theme(strip.text = element_text(face = "bold", size = 10))

ggsave(file.path(sens_fig_path, "indicator_directional_shifts.png"), p_shift, width = 15, height = 18)

#-------------------------------------------------------------------------
# 2. Overall Vulnerability Deviations (Directional)
#-------------------------------------------------------------------------
cat("Plotting Overall Vulnerability Directional Impacts...\n")

# Still use wide deviations for the boxplot to show distribution across CUs
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
            source_type == "GCM" ~ str_remove(source_label, "^GCM"),
            source_type == "Scenario" ~ str_remove(source_label, "^RCP"),
            source_type == "Method" ~ str_remove(source_label, "^Method_"),
            source_type == "Model" ~ "Model",
            TRUE ~ source_label
        )
    )

p_dev <- ggplot(dev_raw, aes(x = source, y = raw_deviation, fill = source)) +
    geom_boxplot(alpha = 0.7, outlier.size = 0.5) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    facet_wrap(~category, scales = "free_x") +
    scale_fill_manual(values = source_colors, na.value = "grey50", guide = "none") +
    labs(
        title = "Directional Influence of Uncertainty on Vulnerability Scores",
        subtitle = "Distribution of (Scenario Score - Baseline Score) across CUs",
        x = "Variation Source",
        y = "Score Change (Raw Deviation)",
        fill = "Source"
    ) +
    theme_cvis() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(sens_fig_path, "overall_vulnerability_deviations.png"), p_dev, width = 12, height = 8)

#-------------------------------------------------------------------------
# 3. Mean Rank Displacement (Relative Vulnerability Impacts)
#-------------------------------------------------------------------------
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

#-------------------------------------------------------------------------
# 4. Jackknife Leverage Analysis (Indicator & Category Importance)
#-------------------------------------------------------------------------
cat("Plotting Jackknife Influence...\n")

influence_summary <- overall_sensitivity$influence_summary # loads from 4b

# Filter for the main aggregated overall metric "avgall" (or "avg_all")
jack_global <- influence_summary %>%
    filter((method == "avgall" | method == "avg_all") & SPECIES_NAME == "ALL")

p_jack <- ggplot(jack_global, aes(x = reorder(excluded_element, mean_abs_dev), y = mean_abs_dev, fill = parent_category)) +
    geom_col() +
    coord_flip() +
    facet_wrap(~excluded_type, scales = "free_y", ncol = 1) +
    scale_fill_scico_d(palette = "roma", name = "Parent Category") +
    labs(
        title = "Leverage on Overall Vulnerability",
        subtitle = "Mean absolute deviation in overall vulnerability when removing an element (Jackknife effect)",
        x = "Excluded Element (Indicator or Category)",
        y = "Mean Absolute Score Deviation"
    ) +
    theme_cvis()

ggsave(file.path(sens_fig_path, "jackknife_leverage_overall.png"), p_jack, width = 10, height = 8)

jack_species <- influence_summary %>%
    filter((method == "avgall" | method == "avg_all") & SPECIES_NAME != "ALL")

p_jack_sp <- ggplot(jack_species, aes(x = reorder(excluded_element, mean_abs_dev), y = mean_abs_dev, fill = parent_category)) +
    geom_col() +
    coord_flip() +
    facet_grid(SPECIES_NAME ~ excluded_type, scales = "free_y") +
    scale_fill_scico_d(palette = "roma", name = "Parent Category") +
    labs(
        title = "Leverage on Overall Vulnerability by Species",
        subtitle = "Mean absolute deviation in overall vulnerability when removing an element",
        x = "Excluded Element",
        y = "Mean Absolute Score Deviation"
    ) +
    theme_cvis()

ggsave(file.path(sens_fig_path, "jackknife_leverage_species.png"), p_jack_sp, width = 14, height = 12)

#-------------------------------------------------------------------------
# 5. Rank Consistency (Correlation Heatmap)
#-------------------------------------------------------------------------
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

#----------------6. Indicator Correlations (Redundancy)----------------
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

#-------------------------------------------------------------------------
cat("Sensitivity plots saved to output/figures/sensitivity_analysis/\n")
