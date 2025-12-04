### 0a_console

# this script demonstrates the workflow for the CVIS package

######################## SETUP #########################
# setup and packages

rm(list = ls()) # clear workspace

# set-up used in every script
library(here)
setwd(here())
source(file.path(here(), "code", "0_setup.R"))

### ----- Load frequently used data sets- ------
# load marine adaptive zone spatial object
load(file.path(paths$marine, "MAZ.Rds"))
# load watershed basins R object
load(file.path(paths$fw, "basins_shp.Rds"))
# make a Fraser basin version
Fr_basin <- filter(basins, BASIN == "FRASER")
# load CU boundaries
load(file.path(paths$fw, "cu_boundary.Rds"))

# freshwater stream subsets by CU boundary
load(file.path(paths$fw, "fw_streampicks_tscapes.Rdata"))
# freshwater stream indicator statistics
load(file.path(paths$fw, "fw_rearing_indicators.Rdata"))
# migration paths
load(file.path(paths$fw, "fw_upstream_paths.Rdata"))
# migration indicators
load(file.path(paths$fw, "migr_stats.Rdata"))
# marine indicators
load(file.path(paths$marine, "marine_stats.Rds"))

# spatial models
# stream model outputs for freshwater spawning and rearing indicators
load(file.path(paths$fw, "fw_models_tscapes.Rds"))
# indicator spatial outputs
load(file.path(paths$fw, "fw_stream_indicators_sp.Rds"))
# lakes_Fr - freshwater lakes for plotting
load(file.path(paths$fw, "BC_FWA_LAKES_FR.Rds"))
# load flow gauge and watershed hydrology data
load(file.path(paths$fw, "flow_gauge_data.Rdata"))
# read Temperature gauge locations
load(file.path(paths$fw, "Tw_stations.Rds"))

# PCIC ensemble model outputs by period
# PCIC_daily45 <- read_mdim(file.path(paths$climate, "PCIC_averaged", "combined",
#   "daily_rcp45_ensemble.nc"))

## Marine data
load(file = file.path(paths$marine, "CMIP6_SST_periods.Rds"))
load(file = file.path(paths$marine, "CImpact_points.Rds"))  # load CImpact_points

## Combining all indicators into a common table, and applying standardization functions
source(file.path(paths$code, "4a_CU_scoring.R"))


# 1 - Freshwater data processing ------------------------------------------

### These scripts import the spatial data files and process them into
# common formats. Data is also summarized by time periods

## importing and process of stream network data
# script should not be sourced all at once but run in chunks as it takes a very long time
# source(file.path("code", "1b_FW_stream_process.R"))  #spawning data import script

## process raw PCIC files to get period averages
# source(file.path("code", "1c_FW_PCIC_period_averages.R"))

## process PCIC Period averages to get GCM model averages
# source(file.path("code", "1d_FW_PCIC_model_averages.R"))


# load(here("processed_data", "freshwater", "R_data",  "2025-01-24_fw_FAZstats_output.Rdata"))


# 2 - Freshwater statistics -----------------------------------------------

### These scripts using the processed data from 1 above to calculate statistics
# and indicators for each CU. There are freshwater spawning/rearing indicators
# and freshwater migration indicators

# ## script to determine which streams from the main data table are within each cu boundary
# source(file.path("code", "2a_FW_boundary_subset.R"))
#
# ## script to calculate spawning statistics for each CU, using subsetted streams from 2a
# source(file.path("code", "2b_FW_rearing_stats.R"))   # rearing stats script
#
# ## script to determine migration paths for each CU from river mouth to NUSEDS sites
# # and calculate downstream distance for each stream segment to the ocean
# source(file.path("code", "2c_FW_upstream_paths.R"))  # migration paths script
#
# ## script to calculate migration statistics for each CU, using paths from 2c
# source(file.path("code", "2d_FW_migration_stats.R"))  # migration stats script


# 3 - Marine data processing and statistics -----------------------------------

### These scripts import the marine data files and process them into
# common formats.

## importing and process of marine data
# source(file.path(paths$code, "3a_marine_data_import.R"))  # marine data import script
#
# ## script to calculate marine statistics for each CU
# source(file.path(paths$code, "3c_marine_stats.R"))  # marine stats script
#
# ## script to get a standardized grid output for marine data (mostly for plotting)
# source(file.path(paths$code, "3b_marine_grid_standardize.R"))  # marine grid standardize script




# 5. Reports --------------------------------------------------------------

## comparison of all indicators across CUs
rmarkdown::render(
  file.path(here("code", "6_CVIS_report.Rmd")),
  output_file = paste(today, "CVIS_report.html", sep = "_"),
  output_dir = file.path(paths$reports),
  output_format = "html_document")


#individual CU reports iwth indicator data  SUPERCEDED BY SHINY APP
for (i in 29:n.CUs) {
  CU_IN_i <- cu_run$FULL_CU_IN[i]
  #CU_IN_i <- "SEL-05-02"
  default_rcp <- "45"
  default_period <- 3
  
  rmarkdown::render(
    file.path(here(), "code", "6a_CU_indicator_report_tabbed.Rmd"),
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


## Shiny app to explore individual CU data (replaces 6a)
shiny::runApp(file.path(here(), "code", "7_CVIS_explorer_app.R"))
