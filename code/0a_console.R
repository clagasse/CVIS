# ==============================================================================
# CVIS Console - Pipeline Runner (0a_console.R)
#
# Description:
#   This is the master orchestration script for the Climate Vulnerability Indicator
#   Suite (CVIS). It configures the pipeline options, loads baseline spatial datasets,
#   runs optional data processing and statistical steps, and executes scoring and
#   report generation.
#
# Workflow Steps:
#   1. Setup environment and load baseline spatial boundaries/definitions.
#   2. Optional Freshwater preprocessing & stats (1b, 1c, 1d, 2a, 2b, 2c, 2d).
#   3. Optional Marine preprocessing & stats (3a, 3b, 3c).
#   4. Scoring indicator standardization & calculation (4a).
#   5. Generating outputs (Rmd report, Shiny app).
#
# Inputs:
#   - Spatial/definition databases (basins, MAZ, stream paths, etc.) via processed_data/
#   - Scoring results and data inputs (fw_all, ss_all, migr_all, marine_stats)
#
# Outputs:
#   - Rendered reports in reports/
#   - Interactive Shiny App
#
# Dependencies:
#   - Requires R packages (sf, tidyverse, here, shiny, rmarkdown)
#   - 0_setup.R (executed at startup)
# ==============================================================================

# ==================== 1. Initialization and Setup ====================
# Clear active workspace
rm(list = ls())

# Set working directory and load libraries
library(here)
setwd(here())
source(file.path(here(), "code", "0_setup.R"))

# Execution Toggles: Toggle steps of the pipeline on (TRUE) or off (FALSE)
run_fw_prep        <- FALSE # Run raw stream networks and PCIC flow model prep (1b, 1c, 1d)
run_fw_stats       <- FALSE # Run stream intersections, rearing, and migration stats (2a, 2b, 2c, 2d)
run_marine_prep    <- FALSE # Run raw marine NetCDF and spatial GDB imports (3a)
run_marine_stats   <- FALSE # Run marine stats & grid standardization calculations (3b, 3c)
run_scoring        <- TRUE  # Run core standardization and scoring calculation engine (4a)
run_report_all     <- TRUE  # Generate the comprehensive multi-CU CVIS HTML report (6_CVIS_report.Rmd)
run_reports_indiv  <- FALSE # Generate individual CU report HTML files (Deprecated, use Shiny app)
run_shiny_explorer <- FALSE # Launch local interactive Shiny explorer app (7_CVIS_explorer_app.R)

# ==================== 2. Load Core Spatial and Definition Data ====================
cat("Loading reference spatial objects...\n")
load(file.path(paths$marine, "MAZ.Rds"))            # Marine Adaptive Zones
load(file.path(paths$fw, "basins_shp.Rds"))         # Watershed basins
Fr_basin <- filter(basins, BASIN == "FRASER")      # Filter Fraser basin for mapping/plots

load(file.path(paths$fw, "fw_streampicks_tscapes.Rdata"))  # Stream segments by CU boundary
load(file.path(paths$fw, "fw_upstream_paths.Rdata"))       # Stream migration paths by CU boundary
load(file.path(paths$fw, "fw_models_tscapes.Rds"))         # Base stream models (tscapes, Fishpass)
load(file.path(paths$fw, "fw_stream_indicators_sp.Rds"))   # Spatial indicator streams
load(file.path(paths$fw, "BC_FWA_LAKES_FR.Rds"))           # Fraser basin lakes for plotting
load(file.path(paths$fw, "flow_gauge_data.Rdata"))         # Hydrology gauge locations and watersheds
load(file.path(paths$fw, "Tw_stations.Rds"))               # Temperature station locations

load(file.path(paths$marine, "CMIP6_SST_periods.Rds"))     # SST data by period
load(file.path(paths$marine, "CImpact_points.Rds"))        # Cumulative impacts points

# ==================== 3. Optional Pipeline Executions ====================

# 3.1 Freshwater Preprocessing and Stats
if (run_fw_prep) {
  cat("\nRunning Freshwater raw data preprocessing (Steps 1b, 1c, 1d)...\n")
  source(file.path(paths$code, "1b_FW_stream_process.R"))
  source(file.path(paths$code, "1c_FW_PCIC_period_average.R"))
  source(file.path(paths$code, "1d_FW_PCIC_model_averages.R"))
}

if (run_fw_stats) {
  cat("\nCalculating Freshwater CU statistics (Steps 2a, 2b, 2c, 2d)...\n")
  source(file.path(paths$code, "2a_FW_boundary_subset.R"))
  source(file.path(paths$code, "2b_FW_rearing_stats.R"))
  source(file.path(paths$code, "2c_FW_upstream_paths.R"))
  source(file.path(paths$code, "2d_FW_migration_stats.R"))
}

# 3.2 Marine Preprocessing and Stats
if (run_marine_prep) {
  cat("\nImporting raw Marine dataset points and NetCDFs (Step 3a)...\n")
  source(file.path(paths$code, "3a_marine_data_import.R"))
}

if (run_marine_stats) {
  cat("\nCalculating Marine CU statistics (Steps 3b, 3c)...\n")
  source(file.path(paths$code, "3c_marine_stats.R"))
  source(file.path(paths$code, "3b_marine_summarize.R")) # Standardized grid output
}

# ==================== 4. Core Scoring Engine ====================
if (run_scoring) {
  cat("\nRunning scoring engine to standardize indicators and calculate vulnerability ranks...\n")
  source(file.path(paths$code, "4a_CU_scoring.R"))
}

# ==================== 5. Report & App Generation ====================

# 5.1 Main Vulnerability Comparison HTML Report
if (run_report_all) {
  cat("\nRendering comprehensive multi-CU CVIS report...\n")
  rmarkdown::render(
    file.path(paths$code, "6_CVIS_report.Rmd"),
    output_file = paste(today, "CVIS_report.html", sep = "_"),
    output_dir = file.path(paths$reports),
    output_format = "html_document"
  )
  cat("Report rendered in:", file.path(paths$reports), "\n")
}

# 5.2 Individual CU HTML Reports (Older static approach)
if (run_reports_indiv) {
  cat("\nRendering individual CU data reports...\n")
  dir.create(file.path(paths$reports, "CU_reports"), showWarnings = FALSE, recursive = TRUE)
  for (i in 1:n.CUs) {
    CU_IN_i <- cu_run$FULL_CU_IN[i]
    default_rcp <- "45"
    default_period <- 3
    
    rmarkdown::render(
      file.path(paths$code, "6a_CU_indicator_report.Rmd"),
      output_file = paste(CU_IN_i, "CVIS_Data_report.html", sep = "_"),
      output_dir = file.path(paths$reports, "CU_reports"),
      output_format = "html_document",
      params = list(
        FULL_CU_IN = CU_IN_i,
        default_rcp = default_rcp,
        default_period = default_period
      )
    )
  }
}

# 5.3 Interactive Shiny App Explorer
if (run_shiny_explorer) {
  cat("\nLaunching interactive CVIS explorer Shiny app...\n")
  shiny::runApp(file.path(paths$code, "7_CVIS_explorer_app.R"))
}
