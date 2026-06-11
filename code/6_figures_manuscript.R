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
#   4. Generate Figure 3 (Distributions of raw baseline indicators).
#   5. Generate Figure 4 (Freshwater indicators multipanel stream map).
#   6. Generate Figure 5 (Spatial standardized freshwater rearing scores).
#   7. Generate Figure 6 (Directional shifts/climate hazard exposure).
#   8. Generate Figure 7 (Upstream migration timing comparison).
#   9. Generate Figure 8 (Marine Adaptive Zones combined SST/CImpact map).
#   10. Generate Figure 9 (Heatmap of standardized indicator scores).
#   11. Generate Figure 10 (Bootstrap vulnerability uncertainty spread).
#   12. Generate Figure 11 (Vulnerability score deviation boxplots).
#   13. Generate Figure 12 (Relative rank stability bump plots).
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
casestudy_CU <- switch(1, "CK-12", "CM-02", "PKO-01")

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

load(file.path(paths$output, "uncertainty_analysis_results.Rdata"))

# ==================== 3. Subset Case Study Data ====================

cu_i <- casestudy_CU

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

fw_sp_ind_cu <- subset_fw_models(
  fw_models = fw_sp_ind,
  cu_i = cu_i,
  stream_cu_picks = stream_acc_cu_picks,
  cu_run = cu_run,
  spp_lookup = spp_lookup,
  to_factor = TRUE,
  filter_rs = F  #get rearing/spawning streams only for species
)

cu_timing_long_i <- cu_timing_long[cu_timing_long$FULL_CU_IN == cu_i, ]
cu_timing_i <- cu_timing_Fr[cu_timing_Fr$FULL_CU_IN == cu_i, ]


# ==================== 4. Generate & Save Figures ====================


# Figure 3 - Distributions of raw (unstandardized) indicator values across all Fraser River basin Conservation Units under the baseline scenario.
# Caption: Violin plot visualizing the density, spread, and median values of raw environmental indicators across all 50 CUs under the baseline. Indicators are grouped by vulnerability category (Spawning & Rearing, Upstream Migration, Nearshore Marine, Demographic, and Genetic) to show the underlying range of historical environmental conditions and population attributes.
f3 <- plot_raw_baseline_violins(
  all_std_long_baseline = all_std_long_baseline,
  tbl_indicators = tbl_indicators
)

ggsave(filename = file.path(output_dir, "figure_3.png"), plot = f3,
       width = 11, height = 8.5, dpi = 150) 

# Figure 4 - High-resolution spatial mapping of freshwater spawning and rearing indicators within Fraser basin

f4 <- stream_indicator_multipanel_plot(fw_sp_ind,
  cu_boundary = NULL,
  lakes_Fr,
  variables = c("cthr_anad", "tw8proj_9_45_3", "tw8rate_9_45_3", "flow8pdelta_9_45_3", "flow18pdelta_9_45_3", "favchange_chinook_85_3"),
  plot_titles = c("Cumulative Threat Score", "August Mean Temperature", "Rate of Temp. Change", "Change in August Flow", "Change in Nov-Jan Flow", "Change in ENM Favourability"),
  risk_palette = cvis_risk_palette,
  palette_directions = c(-1, -1, -1, -1, 1, 1, -1, 1))

#save as png
ggsave(filename = file.path(output_dir, "figure_4.png"), plot = f4, width = 12, height = 8)

# Figure 4 - Basin-wide spatial distribution of standardized freshwater spawning and rearing vulnerability scores for Chinook salmon.
# Caption: Maps the spatial distribution of the aggregated freshwater spawning and rearing (fwrs) category score for Chinook salmon CUs across the Fraser River basin. Spawning and rearing stream networks within each CU boundary are colored based on their standardized score (the length-weighted average of the seven underlying freshwater indicators). High values (warm colors) indicate high cumulative vulnerability, while low values (cool colors) represent physical and climatic refugia.
f5 <- spatial_fw_rearing_indicators_plot(all_std_long_baseline,
                                         cu_boundary,
                                         outline = Fr_basin,
                                         species_pick = "Chinook")

ggsave(filename = file.path(output_dir, "figure_5.png"), plot = f5, width = 10, height = 9)   

# Figure 5 - Directional shifts and expansion of climate hazard exposure across CUs under future projection scenarios.
f6 <- plot_indicator_directional_shifts(overall_sensitivity, 
                                        tbl_indicators)

ggsave(filename = file.path(output_dir, "figure_6.png"), plot = f6, width = 9, height = 7) 



# Figure 6 - Upstream migration timing and thermal exposure profiles across Fraser River basin salmon Conservation Units.
f7 <- migration_compare_plot(
  migr_daily_calendar,
  timing = cu_timing_Fr,
  rcp = "45",
  period_choose = c("1981-2010", "2041-2060")
)

ggsave(filename = file.path(output_dir, "figure_7.png"), plot = f7, width = 8, height = 6)

            
                   
# Figure 7 - Marine Adaptive Zones (MAZs) and regional marine vulnerability profiles in the Salish Sea and Northeast Pacific.
f8 <- combined_maz_marine_plot(maz_all, MAZ)

ggsave(filename = file.path(output_dir, "figure_8.png"), plot = f8, width = 12, height = 9)   


# Figure 8 - Heatmap of individual standardized indicator scores, category-level scores, and overall vulnerability portfolios across all salmon Conservation Units.
f9 <- indicator_cu_tile_plot(all_std_long_baseline,
                       scores_tidy_baseline)


ggsave(filename = file.path(output_dir, "figure_9.png"), plot = f9,
       width = 8, height = 9) 


# Figure 10 - Quantitative sensitivity analysis of overall vulnerability scores across sources of modeling variation.
all_scores <- mc_results %>% filter(category == "all")
f10 <- plot_combined_uncertainty_spread(all_scores, species_palette)

ggsave(filename = file.path(output_dir, "figure_10.png"), plot = f10, width = 9, height = 9)


# Figure 11 - Quantitative sensitivity analysis of overall vulnerability scores across sources of modeling variation.
# Caption: Boxplots illustrating deviations in overall vulnerability scores for each CU resulting from four primary sources of model variation: Global Climate Model selection, emissions scenario, standardization curves, and indicator weighting schemes. The relative spread indicates which modeling choice contributes the greatest score variance.
f11 <- plot_score_deviations(overall_sensitivity$deviations)

ggsave(filename = file.path(output_dir, "figure_11.png"), plot = f11, width = 9, height = 9)


# Figure 12 - Robustness of relative vulnerability rankings for Chinook salmon CUs across sensitivity scenarios.
# Caption: Bump plot tracking changes in relative vulnerability rank for individual Chinook salmon CUs (y-axis) across different model sensitivity runs (x-axis), representing alternative weightings, climate projections, and aggregation formulas. Crossing lines identify rankings sensitive to specific modeling options.
f12 <- plot_species_bump_plot(overall_sensitivity, "Chinook")
f12b <- plot_species_bump_plot(overall_sensitivity, "Sockeye")
f12c <- plot_species_bump_plot(overall_sensitivity, "Coho")

ggsave(filename = file.path(output_dir, "figure_12.png"), plot = f12, width = 8, height = 7)


# Supplemental figures ----------------------------------------------------

png(file.path(output_dir, "sfig_corr.png"), width = 1000, height = 1000, res = 120)
plot_indicator_redundancy_corr(overall_sensitivity$correlation_indicators)
dev.off()

jack_global <- overall_sensitivity$influence_summary %>%
  filter((method == "catavg" | method == "catavg") & SPECIES_NAME == "ALL")
sfig_jack <- plot_jackknife_influence(jack_global, cat_label_map)
ggsave(filename = file.path(output_dir, "sfig_jacknife.png"), plot = sfig_jack, width = 10, height = 8)


hydrologic_reg <- fraser_hydrologic_regime_comparison_plot()
ggsave(filename = file.path(output_dir, "sfig_hydroreg.png"), plot = hydrologic_reg, width = 10, height = 5)

# Supplemental Figure for stream attributes (basin-wide)
sfig_stream_attr <- stream_indicator_multipanel_plot(
  fwModels = fw_sp_ind,
  cu_boundary = NULL,
  lakes_cu = lakes_Fr,
  variables = c("model_access_salmon", "stream_order", "gradient", "elevation"),
  plot_titles = c("Stream Accessibility", "Stream Order", "Gradient", "Elevation"),
  ncol = 2
)
ggsave(filename = file.path(output_dir, "sfig_streams.png"), plot = sfig_stream_attr, width = 10, height = 8)



test_fw <- filter(fw_models, model_access_salmon != "NATURAL_BARRIER")

# ==================== 5. Summary Table for Manuscript ====================

gt_table <- generate_cvis_summary_table(
  all_std_long_baseline = all_std_long_baseline,
  group_by = "smu"
)

# Save the table outputs
gtsave(gt_table, filename = file.path(output_dir, "table_summary.html"))


# ==================== 6. Vulnerability Summary Table by Category ====================

gt_vuln_table <- generate_cvis_vulnerability_table(
  scores_tidy = scores_tidy,
  scores_tidy_baseline = scores_tidy_baseline
)

# Save vulnerability table outputs
gtsave(gt_vuln_table, filename = file.path(output_dir, "table_vulnerability.html"))


# ==================== 7. Sensitivity Analysis Summary Table ====================

# Load sensitivity analysis results data
load(file.path(paths$output, "sensitivity_analysis.Rdata"))

gt_sens_table <- generate_cvis_sensitivity_table(
  overall_sensitivity = overall_sensitivity
)

# Save sensitivity table outputs
gtsave(gt_sens_table, filename = file.path(output_dir, "table_sensitivity.html"))


# ==================== 8. Raw Indicator Scenario & Sensitivity Summary Table ====================

gt_raw_scenario_table <- generate_cvis_scenario_raw_summary_table(
  all_std_long = all_std_long
)

# Save raw scenario table outputs
gtsave(gt_raw_scenario_table, filename = file.path(output_dir, "table_raw_scenario_summary.html"))


# ==================== 9. CVIS Indicator Description Table ====================

gt_guide_table <- generate_cvis_indicator_description_table(
  all_std_long_baseline = all_std_long_baseline
)

# Save guide table outputs
gtsave(gt_guide_table, filename = file.path(output_dir, "table_indicators_guide.html"))


# ==================== 10. Compile All Tables into a Combined HTML File ====================

cat("Compiling all tables into a single combined HTML file...\n")
html_content <- paste0(
  "<!DOCTYPE html>\n",
  "<html>\n",
  "<head>\n",
  "  <meta charset=\"utf-8\">\n",
  "  <title>CVIS Manuscript Tables</title>\n",
  "  <style>\n",
  "    body {\n",
  "      font-family: 'Inter', 'Helvetica Neue', Arial, sans-serif;\n",
  "      margin: 40px auto;\n",
  "      max-width: 1200px;\n",
  "      color: #2D3748;\n",
  "      background-color: #FAFAFA;\n",
  "      line-height: 1.5;\n",
  "    }\n",
  "    h1 {\n",
  "      text-align: center;\n",
  "      color: #1A365D;\n",
  "      font-weight: bold;\n",
  "      border-bottom: 2px double #1A365D;\n",
  "      padding-bottom: 10px;\n",
  "      margin-bottom: 40px;\n",
  "    }\n",
  "    .table-section {\n",
  "      background: white;\n",
  "      border-radius: 8px;\n",
  "      box-shadow: 0 4px 6px -1px rgba(0,0,0,0.1), 0 2px 4px -1px rgba(0,0,0,0.06);\n",
  "      padding: 30px;\n",
  "      margin-bottom: 50px;\n",
  "      border-top: 4px solid #2B6CB0;\n",
  "    }\n",
  "    .table-section-title {\n",
  "      font-size: 18px;\n",
  "      font-weight: bold;\n",
  "      color: #1A365D;\n",
  "      margin-top: 0;\n",
  "      margin-bottom: 20px;\n",
  "      border-bottom: 1px solid #E2E8F0;\n",
  "      padding-bottom: 8px;\n",
  "    }\n",
  "  </style>\n",
  "</head>\n",
  "<body>\n",
  "  <h1>CVIS Manuscript Tables</h1>\n",
  "  \n",
  "  <div class=\"table-section\">\n",
  "    <div class=\"table-section-title\">Table 1. CVIS Climate and Vulnerability Indicators Guide</div>\n",
  "    ", gt::as_raw_html(gt_guide_table), "\n",
  "  </div>\n",
  "\n",
  "  <div class=\"table-section\">\n",
  "    <div class=\"table-section-title\">Table 2. CVIS Climate and Vulnerability Indicators Summary</div>\n",
  "    ", gt::as_raw_html(gt_table), "\n",
  "  </div>\n",
  "\n",
  "  <div class=\"table-section\">\n",
  "    <div class=\"table-section-title\">Table 3. CVIS Overall Vulnerability Score Summary by Category</div>\n",
  "    ", gt::as_raw_html(gt_vuln_table), "\n",
  "  </div>\n",
  "\n",
  "  <div class=\"table-section\">\n",
  "    <div class=\"table-section-title\">Table 4. CVIS Overall Vulnerability Score Sensitivity Analysis</div>\n",
  "    ", gt::as_raw_html(gt_sens_table), "\n",
  "  </div>\n",
  "\n",
  "  <div class=\"table-section\">\n",
  "    <div class=\"table-section-title\">Table 5. CVIS Raw Indicator Value Scenario & Sensitivity Summary</div>\n",
  "    ", gt::as_raw_html(gt_raw_scenario_table), "\n",
  "  </div>\n",
  "</body>\n",
  "</html>\n"
)

writeLines(html_content, file.path(output_dir, "tables_manuscript.html"))
cat("Combined HTML tables compiled and saved successfully to tables_manuscript.html!\n")






