# ==============================================================================
# CVIS Manuscript Figures Generator (6_figures_manuscript.R)
#
# Description:
#   Generates and saves the final PNG figure outputs used in the CVIS manuscript.
#   Loads spatial datasets, model outputs, indicators, and scoring results,
#   then plots maps, lollipop plots, migration paths, and indicator tile grids.
#
# Workflow Steps:
#   1. Setup settings and verify the output directory exists.
#   2. Load freshwater, migration, marine, and scoring datasets.
#   3. Subset CU and timing data for the specified case study.
#   4. Generate Figure 2 (Freshwater indicator multipanel maps).
#   5. Generate Figure 3 (Upstream migration path).
#   6. Generate Figure 4 (Indicator lollipop plot).
#   7. Generate Figure 5 (Vulnerability spatial maps).
#   8. Generate Figure 6 (Marine adaptive zones lollipop plot).
#   9. Generate Figure 7 (Vulnerability score indicator tiles).
#
# Inputs:
#   - Processed Rds/Rdata files under paths$fw, paths$marine, and paths$output
#
# Outputs:
#   - PNG figure files saved in paths$figures/manuscript/
#
# Dependencies:
#   - Requires 0_setup.R, ggplot2, sf, dplyr
# ==============================================================================

# ==================== 1. Settings & Configurations ====================


# Settings ----------------------------------------------------------------
# set-up used in every script
library(here)
setwd(here())
source(file.path(here(), "code", "0_setup.R"))

#output directory 
output_dir <- file.path(paths$figures, "manuscript")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# select case study CUs for manuscript
casestudy_1 <- "CK-12"
casestudy_2 <- "CM-02"
casestudy_3 <- "PKO-01"


# ==================== 2. Load Processed Datasets ====================

### ----- Load frequently used data sets- ------
# load marine adaptive zone spatial object
load(file.path(paths$marine, "MAZ.Rds"))
# load watershed basins R object
load(file.path(paths$fw, "basins_shp.Rds"))
# make a Fraser basin version
Fr_basin <- filter(basins, BASIN == "FRASER")

# freshwater stream subsets by CU boundary
load(file.path(paths$fw, "fw_streampicks_tscapes.Rdata"))
# migration paths
load(file.path(paths$fw, "fw_upstream_paths.Rdata"))

# spatial models
# stream model outputs for freshwater spawning and rearing indicators
load(file.path(paths$fw, "fw_models_tscapes.Rds"))
# indicator spatial outputs
load(file.path(paths$fw, "fw_stream_indicators_sp.Rds"))
# lakes_Fr - freshwater lakes for plotting
load(file.path(paths$fw, "BC_FWA_LAKES_FR.Rds"))

# Freshwater
fw_file <- get_latest_file(paths$fw, "fw_rearing_indicators.Rdata")
load(fw_file) # loads fw_all, ss_all

# Migration
migr_file <- get_latest_file(paths$fw, "migr_stats.Rdata")
load(migr_file) # loads migr_all, etc.

# Marine
mar_file <- get_latest_file(paths$marine, "marine_stats.Rdata") # look for .Rds
load(mar_file)


#scoring data
load(file.path(paths$output, "scoring_results.Rdata")) 
# Load outputs from 4b (Indicator metrics and sensitivity analysis)
load(file.path(paths$output, "sensitivity_analysis.Rdata")) # loads overall_sensitivity



# ==================== 3. Subset Case Study Data ====================

cu_i <- casestudy_1

cu_run_i <- cu_run[cu_run$FULL_CU_IN == cu_i, ]
#sp_pick <- cu_run$spp[cu_run$FULL_CU_IN == cu_i] # species abbr
#sp_pick_bcfp <- spp_lookup$spp_abr_bcfp[spp_lookup$spp_abr == sp_pick]

cu_timing_i <- cu_timing_Fr %>% filter(FULL_CU_IN == cu_i)

cu_boundary_i <- cu_boundary[cu_boundary$FULL_CU_IN == cu_i, ]

#Lakes within CU boundary (for plotting)
temp <- unlist(st_intersects(cu_boundary_i, lakes_Fr))
lakes_cu <- lakes_Fr[temp,]

# subset nuseds observations
nuseds_cu <- nuseds_Fr[nuseds_Fr$FULL_CU_IN == cu_i, ]

# subset migration path
migr_cu <- migr_list[[cu_i]]

#subset fw models
fw_models_cu <- subset_fw_models(
  fw_models = fw_models,
  cu_i = cu_i,
  stream_cu_picks = stream_cu_picks,
  cu_run = cu_run,
  spp_lookup = spp_lookup,
  to_factor = TRUE
)

fw_sp_ind_cu <- subset_fw_models(
  fw_models = fw_sp_ind,
  cu_i = cu_i,
  stream_cu_picks = stream_cu_picks,
  cu_run = cu_run,
  spp_lookup = spp_lookup,
  to_factor = TRUE,
  filter_rs = T  #get rearing/spawning streams only for species
)

cu_timing_long_i <- cu_timing_long[cu_timing_long$FULL_CU_IN == cu_i, ]
cu_timing_i <- cu_timing_Fr[cu_timing_Fr$FULL_CU_IN == cu_i, ]



# ==================== 4. Generate & Save Figures ====================

# Figure 2 - Map of values for freshwater spawning and rearing indicators within a CU boundary

f2 <- stream_indicator_multipanel_plot(fw_sp_ind_cu,
  cu_boundary_i,
  lakes_cu,
  variables = c("favchange_chinook_85_3", "cthr_anad", "tw8proj_9_45_3", "tw8rate_9_45_3", "flow8pdelta_9_45_3", "flow18pdelta_9_45_3"),
  plot_titles = c("Change in ENM Favourability", "Cumulative Threat Score", "August Mean Temperature",
                  "Rate of Temp. Change", "Change in August Flow", "Change in Nov-Jan Flow"),
  scico_palette = "roma",
  palette_directions = c(1, -1, -1, -1, 1, -1))

#save as png
ggsave(filename = file.path(output_dir, "figure_2.png"), plot = f2, width = 9, height = 9) 

# Figure 3 - Mapped vulnerability scores for freshwater spawning and rearing category.

f3 <- spatial_fw_rearing_indicators_plot(all_std_long_baseline,
                                         cu_boundary,
                                         outline = Fr_basin,
                                         species_pick = "Chinook")

ggsave(filename = file.path(output_dir, "figure_3.png"), plot = f3, width = 10, height = 9)                                


# Figure 4 - Summary of migration timing and temperatures across CUs
# Uses migration_compare_plot from 5b to show all CUs' timing and temperatures for all 365 days (months)
f4 <- migration_compare_plot(
  migr_daily_calendar,
  timing = cu_timing_Fr,
  rcp = "45",
  period_choose = c("1981-2010", "2041-2060")
)

ggsave(filename = file.path(output_dir, "figure_4.png"), plot = f4, width = 8, height = 6)

            
                   
# Figure 5 - Marine adaptive zones and associated mean indicator scores for each indicator - SSTproj, SSTrate, CImpact (Regional & Local Point-level zoom in GStr).

f5 <- combined_maz_marine_plot(maz_all, MAZ)

ggsave(filename = file.path(output_dir, "figure_5.png"), plot = f5, width = 12, height = 9)   


# Figure 6 - Violin plot of raw indicator values for all CUs across all indicators under the baseline scenario.

f6 <- plot_raw_baseline_violins(
  all_std_long_baseline = all_std_long_baseline,
  tbl_indicators = tbl_indicators
)

ggsave(filename = file.path(output_dir, "figure_6.png"), plot = f6,
       width = 11, height = 8.5, dpi = 150) 


# Figure 7 - INdicator tile plot of overall vulnerability scores and individual indicator scores.

f7 <- indicator_cu_tile_plot(all_std_long_baseline,
                       scores_tidy_baseline)


ggsave(filename = file.path(output_dir, "figure_7.png"), plot = f7,
       width = 8, height = 9) 


# Figure 8 - Violin plot of spread of indicator values for 50 CUs across different scenarios.

f8 <- plot_indicator_directional_shifts(overall_sensitivity, 
                                        tbl_indicators)

ggsave(filename = file.path(output_dir, "figure_8.png"), plot = f8, width = 9, height = 7) 

# Figure 9 - Tile plot with comparison of category scores and overall vulnerability scores by method. 

f9 <- plot_methods_compare_tile(scores_tidy_baseline)

ggsave(filename = file.path(output_dir, "figure_9.png"), plot = f9, width = 9, height = 10)


# Figure 10 - Deviations in overall vulnerability scores across sources of variation.

f10 <- plot_score_deviations(overall_sensitivity$deviations)

ggsave(filename = file.path(output_dir, "figure_10.png"), plot = f10, width = 9, height = 9)

# Figure 11 - Species-level bump plots of change in vulnerability rank for each CU across different sources of variation.

f11 <- plot_species_bump_plot(overall_sensitivity, "Chinook")
f11b <- plot_species_bump_plot(overall_sensitivity, "Sockeye")
f11c <- plot_species_bump_plot(overall_sensitivity, "Coho")

ggsave(filename = file.path(output_dir, "figure_11.png"), plot = f11, width = 8, height = 7)


# Figure 12 - Vulnerability score violins
f12 <- plot_cvis_vulnerability_violins(scores_tidy_baseline)

ggsave(filename = file.path(output_dir, "figure_12.png"), plot = f12)


# ==================== 5. Summary Table for Manuscript ====================

source(file.path(paths$code, "5d_table_summaries.R"))

gt_table <- generate_cvis_summary_table(
  all_std_long_baseline = all_std_long_baseline,
  group_by = "smu"
)

# Save the table outputs
gtsave(gt_table, filename = file.path(output_dir, "table_summary.html"))
# Save as image if webshot2 is available (wrapped in tryCatch to prevent failures)
tryCatch({
  gtsave(gt_table, filename = file.path(output_dir, "table_summary.png"))
  cat("Table saved to PNG successfully!\n")
}, error = function(e) {
  cat("gtsave as PNG failed (likely webshot2/PhantomJS not installed): ", e$message, "\n")
})



# ==================== 6. Vulnerability Summary Table by Category ====================

gt_vuln_table <- generate_cvis_vulnerability_table(
  scores_tidy = scores_tidy,
  scores_tidy_baseline = scores_tidy_baseline
)

# Save vulnerability table outputs
gtsave(gt_vuln_table, filename = file.path(output_dir, "table_vulnerability.html"))

tryCatch({
  gtsave(gt_vuln_table, filename = file.path(output_dir, "table_vulnerability.png"))
  cat("Vulnerability Table saved to PNG successfully!\n")
}, error = function(e) {
  cat("gtsave as PNG failed: ", e$message, "\n")
})




# ==================== 7. Sensitivity Analysis Summary Table ====================

# Load sensitivity analysis results data
load(file.path(paths$output, "sensitivity_analysis.Rdata"))

gt_sens_table <- generate_cvis_sensitivity_table(
  overall_sensitivity = overall_sensitivity
)

# Save sensitivity table outputs
gtsave(gt_sens_table, filename = file.path(output_dir, "table_sensitivity.html"))

tryCatch({
  gtsave(gt_sens_table, filename = file.path(output_dir, "table_sensitivity.png"))
  cat("Sensitivity Table saved to PNG successfully!\n")
}, error = function(e) {
  cat("gtsave as PNG failed: ", e$message, "\n")
})




