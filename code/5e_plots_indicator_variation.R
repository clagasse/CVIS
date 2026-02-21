################################################################################
#
# 5e_plots_indicator_variation.R
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
    filter(
        rcp == rcp_ref, period_code == period_ref, gcm == "9",
        dsmodel %in% dsmodel_baseline
    ) %>%
    group_by(indicator) %>%
    summarise(baseline_mean = mean(std_value, na.rm = TRUE), .groups = "drop")

# 1b. Extract Future Variation Data
# Ensemble model (GCM 9) has std_value per rcp/period
future_dat <- all_std_long %>%
    filter(
        gcm == "9", period_code != "0",
        dsmodel %in% dsmodel_baseline
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

cat("Visualizations complete. Lollipop plots saved in output/figures/indicator_uncertainty/\n")
