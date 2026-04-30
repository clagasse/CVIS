################################################################################
#
# 5e_plots_baseline_bumpplot.R
#
# Visualizes overall vulnerability scores for each CU as a rank bump/text plot.
# Uses period code 3 and RCP 45 as baseline, comparing different methods.
# Then varies one parameter at a time from baseline (RCP 85 vs Per 5).
# lowest rank at bottom, CUs represented by text (FULL_CU_IN), colored by species.
#
################################################################################

library(here)
setwd(here())
source(file.path(here(), "code", "0_setup.R"))
library(ggplot2)
library(dplyr)

# Create figures directory
fig_path <- file.path(paths$figures, "vulnerability_methods")
dir.create(fig_path, showWarnings = FALSE, recursive = TRUE)

# Load scoring results
cat("Loading scoring results...\n")
load(file.path(paths$output, "scoring_results.Rdata")) # loads scores_tidy

# 1. Prepare data cuts
# Baseline: Period 3, RCP 45, GCM 9 (Ensemble)
base_catavg <- scores_tidy %>% 
    filter(category == "all", gcm == "9", rcp == "45", period_code == "3", method == "catavg") %>% 
    mutate(col_group = "Scoring method", col_name = "Category avg")
base_avgall <- scores_tidy %>% 
    filter(category == "all", gcm == "9", rcp == "45", period_code == "3", method == "avgall") %>% 
    mutate(col_group = "Scoring method", col_name = "Avg all")
base_avgcube <- scores_tidy %>% 
    filter(category == "all", gcm == "9", rcp == "45", period_code == "3", method == "avgcube") %>% 
    mutate(col_group = "Scoring method", col_name = "Cube avg")
base_redflag <- scores_tidy %>% 
  filter(category == "all", gcm == "9", rcp == "45", period_code == "3", method == "flag") %>% 
  mutate(col_group = "Scoring method", col_name = "Red flags")


# Scenarios: vary parameter from baseline, use catavg as default method
vary_rcp45 <- scores_tidy %>% 
  filter(category == "all", gcm == "9", rcp == "45", period_code == "3", method == "catavg") %>% 
  mutate(col_group = "Emissions and time", col_name = "RCP 45, mid-century")
vary_rcp85 <- scores_tidy %>% 
    filter(category == "all", gcm == "9", rcp == "85", period_code == "3", method == "catavg") %>% 
    mutate(col_group = "Emissions and time", col_name = "RCP 85, mid-century")
vary_per5 <- scores_tidy %>% 
    filter(category == "all", gcm == "9", rcp == "45", period_code == "5", method == "catavg") %>% 
    mutate(col_group = "Emissions and time", col_name = "RCP 45, late-century")


# GCMs: vary GCM from baseline (baseline GCM is 9/Ensemble)
vary_gcmens <- scores_tidy %>% 
  filter(category == "all", gcm == "9", rcp == "85", period_code == "3", method == "catavg") %>% 
  mutate(col_group = "Global climate models", col_name = "Ensemble")
vary_gcm1 <- scores_tidy %>% 
    filter(category == "all", gcm == "1", rcp == "45", period_code == "3", method == "catavg") %>% 
    mutate(col_group = "Global climate models", col_name = "CanESM")
vary_gcm4 <- scores_tidy %>% 
    filter(category == "all", gcm == "4", rcp == "45", period_code == "3", method == "catavg") %>% 
    mutate(col_group = "Global climate models", col_name = "HadGEM")
vary_gcm6 <- scores_tidy %>% 
    filter(category == "all", gcm == "6", rcp == "45", period_code == "3", method == "catavg") %>% 
    mutate(col_group = "Global climate models", col_name = "MPI")

plot_dat <- bind_rows(base_catavg, base_avgall, base_avgcube, vary_rcp45, vary_rcp85, vary_per5, vary_gcmens, vary_gcm1, vary_gcm4, vary_gcm6)

# Factor columns for ordering
plot_dat <- plot_dat %>%
    mutate(
        col_name = factor(col_name, levels = c(
            "Category avg", "Avg all", "Cube avg",
            "RCP 45, mid-century", "RCP 85, mid-century", "RCP 45, late-century", 
            "Ensemble", "CanESM", "HadGEM", "MPI"
        )),
        col_group = factor(col_group, levels = c("Scoring method", "Emissions and time", "Global climate models"))
    ) 

cat("Formatting data...\n")

if (!exists("species_palette")) {
  species_palette <- c("Chinook" = "#E69F00", "Chum" = "#56B4E9", "Coho" = "#009E73", "Pink" = "#F0E442", "Sockeye" = "#D55E00")
}

cat("Plotting...\n")

# Now plotting as a bump chart (Lines + Text at specific ranks)
# Lowest rank at the bottom (typical numeric Y-axis behavior: y=1 at bottom)
p_vuln_rank <- ggplot(plot_dat, aes(x = col_name, y = rankall, group = FULL_CU_IN, color = SPECIES_NAME)) +
  # Lines connecting the same CU across scenarios
  geom_line(alpha = 0.3, linewidth = 0.6) +
  # Text labels for the CUs Instead of points/tiles
  geom_label(aes(label = FULL_CU_IN), size = 2.2, fontface = "bold") +
  # Scales and theme
  scale_color_manual(values = species_palette, name = "Species") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_line(color = "grey80", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10, face = "bold", color = "black"),
    axis.text.y = element_text(size = 10, color = "black"),
    strip.text = element_text(face = "bold", size = 11, margin = margin(b = 5)),
    panel.spacing.x = unit(1, "lines"),
    plot.margin = margin(10, 15, 10, 10)
  ) +
  labs(
    x = NULL, 
    y = "Overall Vulnerability Rank"
  ) +
  # Keep the columns grouped visually
  facet_grid(~ col_group, scales = "free_x", space = "free_x")

plot_filename <- file.path(fig_path, "overall_vulnerability_baseline_comparison.png")
ggsave(
    filename = plot_filename, 
    plot = p_vuln_rank, 
    width = 8, 
    height = 12, 
    dpi = 300
)

cat("Plot saved to", plot_filename, "\n")
