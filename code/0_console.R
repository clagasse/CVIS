### 0_console  

#load packages and set root project directory
rm(list=ls())

library(here)
library(tidyverse)
library(stars)   #package for data cubes (multi-dimensional spatial arrays)
library(sf)      #spatial features
library(wesanderson); library(viridis)  #colour palettes
`%notin%` <- Negate(`%in%`)
library(gridExtra)  #for multi-panel ggplots
library(patchwork) #for multi-panel plots
library(units)   #for unit conversion
library(corrplot)  #correlation matrix plots
#library(gt)   #gg tables for markdown
#library(Hmisc)   #weighted means and sds
#library(bcdata)   #retrieving from BC data catalogue
#install.packages("fwatlasbc", repos = c('https://poissonconsulting.r-universe.dev', 'https://cloud.r-project.org'))
#library(fwatlasbc)

#setwd("C:/Users/LAGASSEC/OneDrive - DFO-MPO/0.Workspace/CVIS")
setwd(here(".."))


today <- Sys.Date()

# Set root for spatial datasets
dat_root <- file.path("..", "0.Workspace")
climate_dat <- file.path(dat_root, "0_data_climate")
spatial_dat <- file.path(dat_root, "0_data_spatial")
salmon_dat  <- file.path(dat_root, "0_data_salmon")
code_root <- file.path(here(), "code")

##### Load CU data
source(file.path(code_root, "1a_CU_import.R"))   #CU table
# Select subset of CUs to run for analysis
cu_run <- filter(cu_list, spp %in% c("ck", "cm")) #cuid != 742)  #remove widgeon (throws error)
cuid <- cu_run$cuid[order(cu_run$spp)] # Create vector of CUs to analyze, ordered CK, CM, CO, PKO, SEL, SER, SH
n.CUs <- length (cuid)

### Load freshwater spatial data
load(here("data", "freshwater", "processed-data", "2025-01-30_fw_cu_streams.Rdata"))
load(here("data", "freshwater", "processed-data", "2025-01-30_fw_upstream_paths.Rdata"))

#load(here("data", "freshwater", "processed-data", "2025-01-24_fw_FAZ_streams.Rdata"))   #FAZ selections of streams

load(here("data", "freshwater", "processed-data", "2025-01-30_fw_spatial_inputs.Rdata"))

#load summary stat results
load(here("output", "2025-01-31_fw_stats_output.Rdata"))

#load(here("output", "2025-01-24_fw_FAZstats_output.Rdata"))

######

deg_threshold <- 21    ##degree threshold

dplyr.summarise.inform <- FALSE  #remove messages when using summarise()
#set to TRUE to make pdfs for each CU
#make_CU_plots <- FALSE
# Colour palette for plotting
#col_pal <- wes_palette("Darjeeling1")

# periods to run for analysis
t_periods <- c("hist", "mid")

#vector of indicator names for analysis
# PCIC_indies_pick <- c("peakQmag_year",     #ANNUAL peak flow amount 
#                  "peakQday_year",   #ANNUAL date of peak flow during year
#                  "POT19freq_year",    #ANNUAL frequency of days above 19 degrees
#                  "POT19dur_year",      #ANNUAL spell lenght of days above 19 degrees
#                  "lowQ05_year",       #ANNUAL frequency of low flow events
#                  "highQ95_year"       #ANNUAL frequency of high flow events
#                  )

PCIC_day_files <- c("twDay",
                 "flowDay")

PCIC_month_files <- c("twMonth",
                "flowMonth")

PCIC_indies_pick <- c("peakFlowAmt",
                         "peakFlowDay",
                         "POT19freq",
                         "POT19dur",
                         "lowQ05",
                         "highQ95")


#########################################################################
# Freshwater code 
#########################################################################

#load freshwater functions
source(file.path(code_root, "2_fw_utils.R"))

### run script to import freshwater data layers
#source(here("code", "2a_fw_import.R"))

### run script to define freshwater distributions by life stage
#source(here("freshwater", "code", "2c_fw_CU_dist.R"))

#source(file.path(code_root, "2c_fw_CU_analysis.R"))

#calculate indicators and summary statistics 
#source(file.path(code_root, "2d_fw_CU_stats.R"))



##### Render Output

# rmarkdown::render(
#   file.path(here("code","0_CU_comparison_report.Rmd")),
#   output_file = paste(today, "fw_comparison.html", sep = "_"),
#   output_dir = here("output"),
#   output_format = "html_document")

rmarkdown::render(
  file.path(here(),"code","0_CU_detail_report.Rmd"),
  output_file = paste(today, "fw_CU_detail.html", sep = "_"),
  output_dir = here("output"),
  output_format = "html_document")



