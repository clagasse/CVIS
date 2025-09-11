### 0a_console

# this script demonstrates the workflow for the CVIS package

######################## SETUP #########################
# setup and packages

# load packages and set root project directory
rm(list = ls())

## plotting packages
# library(rcartocolor) #mapping palettes
library(scico) # scientific colour palettes
# library(wesanderson); library(viridis)  #colour palettes
library(patchwork) # for multi-panel plots
# library(ggridges)  # for ridgeline plots
library(corrplot)  # correlation matrix plots
# library(ggdist)  #ggplot visualizations for distributions
library(ggspatial)

## reporting and markdown packages
# library(skimr)  # summary statistics
# library(ggdist)
library(gt)   # gg tables for markdown
# library(Hmisc)   #weighted means and sds

## spatial data packages
library(pacea)  # bc_coast shapefile
# library(bcdata)   #retrieving from BC data catalogue
# install.packages("fwatlasbc", repos = c('https://poissonconsulting.r-universe.dev', 'https://cloud.r-project.org'))
# library(fwatlasbc)


# set-up used in every script
library(here)
setwd(here())
source(file.path(here(), "code", "0_setup.R"))


# load results from other scripts

# freshwater stream subsets by CU boundary
load(file.path(paths$fw, "2025-05-27_fw_streampicks.Rdata"))
## freshwater stream network model outputs
load(file.path(paths$fw, "2025-06-12_fw_stream_models.Rdata"))
# freshwater stream indicator statistics
load(file.path(paths$fw, "2025-09-10_fw_rearing_indicators.Rdata"))
# migration paths
load(file.path(paths$fw, "2025-07-25_fw_upstream_paths.Rdata"))
# migration indicators
load(file.path(paths$fw, "2025-09-10_migr_stats.Rdata"))
# marine indicators
load(file.path(paths$marine, "2025-09-10_marine_stats.Rdata"))

# spatial models
# PCIC ensemble model outputs by period
PCIC_daily45 <- read_mdim(file.path(paths$climate, "PCIC_averaged", "combined",
  "daily_rcp45_ensemble.nc"))
# stream model outputs for freshwater spawning and rearing indicators
load(file.path(paths$fw, "2025-09-10_fw_stream_models.Rds"))
# lakes_Fr - freshwater lakes for plotting
load(file.path(paths$fw, "BC_FWA_LAKES_FR.Rds"))



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

## script to determine which streams from the main data table are within each cu boundary
source(file.path("code", "2a_FW_boundary_subset.R"))

## script to calculate spawning statistics for each CU, using subsetted streams from 2a
source(file.path("code", "2b_FW_rearing_stats.R"))   # rearing stats script

## script to determine migration paths for each CU from river mouth to NUSEDS sites
# and calculate downstream distance for each stream segment to the ocean
source(file.path("code", "2c_FW_upstream_paths.R"))  # migration paths script

## script to calculate migration statistics for each CU, using paths from 2c
source(file.path("code", "2d_FW_migration_stats.R"))  # migration stats script


# 3 - Marine data processing and statistics -----------------------------------

### These scripts import the marine data files and process them into
# common formats.

## importing and process of marine data
source(file.path(paths$code, "3a_marine_data_import.R"))  # marine data import script

## script to calculate marine statistics for each CU
source(file.path(paths$code, "3c_marine_stats.R"))  # marine stats script

## script to get a standardized grid output for marine data (mostly for plotting)
source(file.path(paths$code, "3b_marine_grid_standardize.R"))  # marine grid standardize script


# 4 - Combining and standardizing -----------------------------------------

## Combining all indicators into a common table, and applying standardization functions
source(file.path(paths$code, "4a_CU_scoring.R"))



# 5. Plotting -------------------------------------------------------------

# plots of maps and other outputs for individual CU
source(file.path(paths$code, "5a_plots_CU.R"))

# plots of indicator values across CUs
source(file.path(paths$code, "5b_plots_compare.R"))


# 6. Reports --------------------------------------------------------------

for (i in 1:n.CUs) {

  CU_IN_i <- cu_run$FULL_CU_IN[i]
  # CU_IN_i <- "CO-5"

  rmarkdown::render(
    file.path(here(), "code", "markdown", "6a_CU_indicator_report.Rmd"),
    output_file = paste(today, CU_IN_i, "CVIS_report.html", sep = "_"),
    output_dir = here("output", "CU_profiles"),
    output_format = "html_document",
    params = list(FULL_CU_IN = CU_IN_i))

  ## overview of freshwater spawning indicators
  rmarkdown::render(
    file.path(here("code", "markdown", "2_FW_spawning_report.Rmd")),
    output_file = paste(today, CU_IN_i, "fw_spawning.html", sep = "_"),
    output_dir = here("output", "CU_profiles"),
    output_format = "html_document",
    params = list(FULL_CU_IN = CU_IN_i))


}


## comparison of freshwater spawning indicators across CUs
rmarkdown::render(
  file.path(here("code", "markdown", "2_FW_spawning_compare.Rmd")),
  output_file = paste(today, "fw_spawning_compare.html", sep = "_"),
  output_dir = here("output"),
  output_format = "html_document")


## comparison of upstream migration indicators across CUs
rmarkdown::render(
  file.path(here("code", "markdown", "2_FW_migr_compare.Rmd")),
  output_file = paste(today, "fw_migr_compare.html", sep = "_"),
  output_dir = here("output"),
  output_format = "html_document")





## comparison of all indicators across CUs
rmarkdown::render(
  file.path(here("code", "markdown", "0b_CVIS_overview.Rmd")),
  output_file = paste(today, "CVIS_overview.html", sep = "_"),
  output_dir = here("output"),
  output_format = "html_document")



## Shiny app

source(file.path(here(), "code", "shiny", "FW_spawning_app.R"))

shinyApp(ui, server)


# rmarkdown::render(
#   file.path(here(),"code","0_CU_detail_report.Rmd"),
#   output_file = paste(today, "fw_CU_detail.html", sep = "_"),
#   output_dir = here("output"),
#   output_format = "html_document")
#
