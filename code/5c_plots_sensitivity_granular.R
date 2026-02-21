################################################################################
#
# 5c_plots_sensitivity_granular.R
#
# Visualizes results from granular sensitivity analysis (4b):
# 1 - Relative Contribution of 9 Uncertainty Sources
#   (GCM1, GCM4, GCM6, RCPs/Periods, Methods cube/flag)
# 2 - Top Driver Frequency by Category
# 3 - Directional Influence (Raw Deviations)
# 4 - Rank Correlation and Displacement
#
################################################################################

library(here)
setwd(here())
source(file.path(here(), "code", "0_setup.R"))
library(reshape2)
library(patchwork)

# Create figures directory
sens_fig_path <- file.path(paths$figures, "sensitivity_granular")
dir.create(sens_fig_path, showWarnings = FALSE, recursive = TRUE)

# Load sensitivity results
cat("Loading sensitivity results...\n")
load(file.path(paths$output, "sensitivity_analysis_4b_directional.Rdata")) # loads sensitivity_analysis_4b_directional

# Unpack results
contributions <- sensitivity_analysis_4b_directional$contributions
mrd_stats <- sensitivity_analysis_4b_directional$mrd_stats
cor_list <- sensitivity_analysis_4b_directional$cor_matrix
categories <- sensitivity_analysis_4b_directional$categories

# Define a color palette for the 9 sources (from 0_setup.R)
source_colors <- sens_source_palette

#----------------1. Relative Contribution (Stacked Bar)----------------
cat("Plotting Relative Contributions...\n")

# Prepare long data for plotting
prop_long <- contributions %>%
    select(FULL_CU_IN, category, starts_with("prop_")) %>%
    pivot_longer(cols = starts_with("prop_"), names_to = "source", values_to = "proportion") %>%
    mutate(source = str_remove(source, "prop_"))

# Plot for each category (Average contribution across all CUs)
prop_summary <- prop_long %>%
    group_by(category, source) %>%
    summarise(mean_prop = mean(proportion, na.rm = TRUE), .groups = "drop")

p_prop <- ggplot(prop_summary, aes(x = category, y = mean_prop, fill = source)) +
    geom_bar(stat = "identity", position = "stack") +
    scale_fill_manual(values = source_colors) +
    labs(
        title = "Mean Relative Contribution of Uncertainty Sources",
        subtitle = "Average across all CUs per category",
        x = "Evaluation Category",
        y = "Proportion of Total Absolute Deviation",
        fill = "Uncertainty Source"
    ) +
    theme_cvis() +
    coord_flip()

ggsave(file.path(sens_fig_path, "mean_relative_contribution.png"), p_prop, width = 10, height = 6)

#----------------2. Top Driver Frequency----------------
cat("Plotting Top Driver Frequencies...\n")

p_top <- ggplot(contributions, aes(x = top_driver, fill = top_driver)) +
    geom_bar() +
    facet_wrap(~category, scales = "free_y") +
    scale_fill_manual(values = source_colors) +
    labs(
        title = "Frequency of Identified Top Driver",
        subtitle = "Number of CUs where source contributes the most absolute deviation",
        x = "Main Driver of Uncertainty",
        y = "Count of CUs",
        fill = "Source"
    ) +
    theme_cvis() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(sens_fig_path, "top_driver_frequency.png"), p_top, width = 12, height = 8)

#----------------3. Directional Influence (Raw Deviations)----------------
cat("Plotting Directional Influence...\n")

# Filter raw deviations for boxplots
raw_long <- contributions %>%
    select(FULL_CU_IN, category, starts_with("raw_dev_")) %>%
    pivot_longer(cols = starts_with("raw_dev_"), names_to = "source", values_to = "raw_deviation") %>%
    mutate(source = str_remove(source, "raw_dev_"))

p_direction <- ggplot(raw_long, aes(x = source, y = raw_deviation, fill = source)) +
    geom_boxplot(alpha = 0.7, outlier.size = 0.5) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    facet_wrap(~category) +
    scale_fill_manual(values = source_colors) +
    labs(
        title = "Directional Influence of Scenarios and Methods",
        subtitle = "Distribution of (Scenario Score - Baseline Score) across CUs",
        x = "Variation Source",
        y = "Score Change (Directional)",
        fill = "Source"
    ) +
    theme_cvis() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(sens_fig_path, "directional_influence_boxplots.png"), p_direction, width = 14, height = 9)

#----------------4. Correlation Heatmap (Example: 'all' category)----------------
cat("Plotting Correlation Heatmap...\n")

cor_all <- cor_list$all
# Isolate numeric correlations and set row names for melting
cor_mat_clean <- cor_all %>%
    select(-scenario, -category) %>%
    as.matrix()
rownames(cor_mat_clean) <- cor_all$scenario

cor_melted <- melt(cor_mat_clean)

p_cor <- ggplot(cor_melted, aes(Var1, Var2, fill = value)) +
    geom_tile() +
    scale_fill_scico(palette = "batlow", direction = 1, limits = c(0, 1)) +
    geom_text(aes(label = round(value, 2)), color = "white", size = 3) +
    labs(
        title = "Rank Consistency Heatmap (Spearman Rho)",
        subtitle = "Category: Overall Vulnerability (all)",
        x = NULL, y = NULL, fill = "Rho"
    ) +
    theme_cvis() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(sens_fig_path, "rank_correlation_heat_all.png"), p_cor, width = 8, height = 7)

#----------------5. Mean Rank Displacement----------------
cat("Plotting Mean Rank Displacement...\n")

p_mrd <- ggplot(mrd_stats, aes(x = reorder(source, MRD), y = MRD, fill = source)) +
    geom_bar(stat = "identity") +
    facet_wrap(~category, scales = "free_x") +
    scale_fill_manual(values = source_colors) +
    labs(
        title = "Mean Rank Displacement",
        subtitle = "Average shift in vulnerability rank compared to baseline",
        x = "Scenario Shift",
        y = "Mean Displacement (Ranks)",
        fill = "Source"
    ) +
    theme_cvis() +
    coord_flip()

ggsave(file.path(sens_fig_path, "mean_rank_displacement.png"), p_mrd, width = 12, height = 8)

cat("Visualizations complete. Plots saved in output/figures/sensitivity_granular/\n")
