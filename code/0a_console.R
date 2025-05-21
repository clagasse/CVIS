### 0_console  

######################## SETUP #########################
# setup and packages

#load packages and set root project directory
rm(list=ls())

library(here)
setwd(here(".."))
source(file.path(here(), "code", "0_setup.R"))

library(corrplot)  #correlation matrix plots
library(pacea)  #bc_coast shapefile
library(skimr)  #summary statistics
library(ggridges)  #for ridgeline plots
library(kableExtra)  # nice markdown tables
library(ggdist)
#library(gt)   #gg tables for markdown
#library(Hmisc)   #weighted means and sds
#library(bcdata)   #retrieving from BC data catalogue
#install.packages("fwatlasbc", repos = c('https://poissonconsulting.r-universe.dev', 'https://cloud.r-project.org'))
#library(fwatlasbc)


######################## SETTINGS ######################################
#choose time periods for analysis. each period covers a range of 20 years
hist_ystart <- switch(2, 1981, 2001)  #historical range start year for FW model outputs
proj_ystart <- switch(1, 2041, 2061, 2081)  #projection range start year for FW model outputs

int_starts <- c(hist_ystart, proj_ystart)  #start years for historical and projection periods

tspan <- (proj_ystart - hist_ystart) / 10  #number of decades between time periods

dplyr.summarise.inform <- FALSE  #remove messages when using summarise()

# Colour palette for plotting
#col_pal <- wes_palette("Darjeeling1")


######################## LOAD DATA AND SCRIPTS #########################

# Loading data and scripts
source(file.path(code_root, "1a_CU_import.R"))   #CU table
# Select subset of CUs to run for analysis
cu_run <- cu_Fr %>%
  filter(spp %in% c("ck", "co", "cm", "sk"), 
         FULL_CU_IN %notin% c("SER-02", "SER-03")) %>%
  arrange(spp)    #remove widgeon (throws error)
cuid    <- cu_run$cuid# Create vector of CUs to analyze, ordered CK, CM, CO, PKO, SEL, SER, SH
cu_seq  <- cu_run$FULL_CU_IN # Create vector of CUs to analyze, ordered CK, CM, CO, PKO, SEL, SER, SH

n.CUs   <- nrow(cu_run)


### Load freshwater spatial data
load(here("processed_data", "freshwater", "R_data", "2025-04-04_fw_spatial_inputs.Rdata"))

load(here("processed_data", "freshwater", "R_data",  "2025-04-22_fw_cu_streams.Rdata"))
load(here("processed_data", "freshwater", "R_data",  "2025-04-22_fw_upstream_paths.Rdata"))

#load(here("processed_data", "freshwater", "R_data", "2025-01-24_fw_FAZ_streams.Rdata"))   #FAZ selections of streams

#load summary stat results
load(here("processed_data", "freshwater", "R_data",  "2025-04-04_SPN_stats.Rdata"))
load(here("processed_data", "freshwater", "R_data",  "2025-04-04_MIGr_stats.Rdata"))

#load(here("processed_data", "freshwater", "R_data",  "2025-01-24_fw_FAZstats_output.Rdata"))



######################## FRESHWATER SCRIPTS #########################


### run script to import freshwater data layers
#source(file.path(code_root,  "2a_FW_import.R"))

### run script to subset freshwater data layers to CUs
#source(file.path(code_root,  "2b_FW_spatial_subset.R"))

### run script to calculate statistics and indicators for CU boundaries (rearing)
#source(file.path(code_root, "2c_FW_rearing_stats.R"))

### run script to calculate statistics and indicators for migration paths
#source(file.path(code_root, "2d_FW_migration_stats.R"))



######################## STANDARDIZATION AND SCORING #########################

source(file.path(code_root, "4a_CU_scoring.R"))

######################## MARKDOWN REPORTS #####################


for(i in 1:n.CUs) {

CU_IN_i <- cu_run$FULL_CU_IN[i]
CU_IN_i <- "CK-12"
  
rmarkdown::render(
  file.path(here(),"code","0a_CU_profile.Rmd"),
  output_file = paste(today, CU_IN_i, "_profile.html", sep = "_"),
  output_dir = here("output", "CU_profiles"),
  output_format = "html_document", 
  params = list(FULL_CU_IN = CU_IN_i))

}

rmarkdown::render(
  file.path(here("code","0b_CVIS_overview.Rmd")),
  output_file = paste(today, "CVIS_overview.html", sep = "_"),
  output_dir = here("output"),
  output_format = "html_document")

# rmarkdown::render(
#   file.path(here("code","0_CU_comparison_report.Rmd")),
#   output_file = paste(today, "fw_comparison.html", sep = "_"),
#   output_dir = here("output"),
#   output_format = "html_document")

# rmarkdown::render(
#   file.path(here(),"code","0_CU_detail_report.Rmd"),
#   output_file = paste(today, "fw_CU_detail.html", sep = "_"),
#   output_dir = here("output"),
#   output_format = "html_document")
# 


