################################################################################
#
# 5d_plots_indicator_sensitivity.R
#
# Visualizes results from indicator sensitivity analysis (4c):
# 1 - Indicator Redundancy (corrplot)
# 2 - Jackknife Leverage (Influence on Overall and Category Scores)
#
################################################################################

library(here)
setwd(here())
source(file.path(here(), "code", "0_setup.R"))
library(corrplot)

# Create figures directory
fig_path <- file.path(paths$figures, "indicator_sensitivity")
dir.create(fig_path, showWarnings = FALSE, recursive = TRUE)

# Load sensitivity results
cat("Loading indicator sensitivity results...\n")
load(file.path(paths$output, "indicator_sensitivity_4c.Rdata")) # loads indicator_sensitivity_4c

# Unpack results
cor_matrix <- indicator_sensitivity_4c$cor_matrix
influence_summary <- indicator_sensitivity_4c$influence_summary

#----------------1. Indicator Redundancy (corrplot)----------------
cat("Plotting Indicator Redundancy Matrix...\n")

png(file.path(fig_path, "indicator_redundancy_corrplot.png"), width = 1000, height = 1000, res = 120)

# Use corrplot for visualization
# 'hclust' ordering groups correlated indicators together
corrplot(cor_matrix,
    method = "pie",
    type = "lower",
    title = "Indicator Redundancy (Spearman rho)"
)

dev.off()

#----------------2. Jackknife Leverage Plots----------------
cat("Plotting Jackknife Leverage (Influence Analysis)...\n")

# Prepare data for Plotting
plot_dat <- influence_summary %>%
    mutate(
        Category = cat_label_map[parent_category],
        metric_label = case_when(
            metric == "catavg" ~ "Overall Index (catavg)",
            metric == "avg_all" ~ "Simple Average (avgall)",
            metric == "avgcube" ~ "Cube Root Average (avgcube)",
            metric == "flagall" ~ "Red Flag Count (flagall)",
            startsWith(metric, "a_") ~ paste0("Category Average: ", Category),
            startsWith(metric, "c_") ~ paste0("Category Cube Root: ", Category),
            startsWith(metric, "sf_") ~ paste0("Category Flags: ", Category),
            TRUE ~ metric
        )
    )

# 2a. Leverage on Overall Index (catavg)
p_overall <- plot_dat %>%
    filter(metric == "catavg") %>%
    ggplot(aes(x = reorder(excluded_indicator, mean_abs_dev), y = mean_abs_dev, fill = Category)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = indicator_palette) +
    labs(
        title = "Indicator Leverage: Influence on Overall Index",
        subtitle = "Mean Absolute Deviation across all CUs (catavg metric)",
        x = "Excluded Indicator",
        y = "Mean Absolute Score Change",
        fill = "Life Stage Category"
    ) +
    coord_flip() +
    theme_cvis()

ggsave(file.path(fig_path, "leverage_overall_index.png"), p_overall, width = 10, height = 7)

# 2b. Leverage on Parent Category (Internal Leverage)
p_category <- plot_dat %>%
    filter(startsWith(metric, "a_")) %>%
    ggplot(aes(x = reorder(excluded_indicator, mean_abs_dev), y = mean_abs_dev, fill = Category)) +
    geom_bar(stat = "identity") +
    facet_wrap(~Category, scales = "free_y") +
    scale_fill_manual(values = indicator_palette) +
    labs(
        title = "Indicator Leverage: Influence on Parent Category",
        subtitle = "Mean Absolute Deviation of the category average (a_ metric)",
        x = "Excluded Indicator",
        y = "Mean Absolute Score Change",
        fill = "Category"
    ) +
    coord_flip() +
    theme_cvis() +
    theme(legend.position = "none")

ggsave(file.path(fig_path, "leverage_internal_category.png"), p_category, width = 12, height = 9)

cat("Visualizations complete. Plots saved in output/figures/indicator_sensitivity/\n")
