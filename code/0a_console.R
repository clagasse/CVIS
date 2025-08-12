### 0_console  

######################## SETUP #########################
# setup and packages

#load packages and set root project directory
rm(list=ls())

library(here)
setwd(here())
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
#hist_ystart <- switch(2, 1981, 2001)  #historical range start year for FW model outputs
#proj_ystart <- switch(1, 2041, 2061, 2081)  #projection range start year for FW model outputs

#int_starts <- c(hist_ystart, proj_ystart)  #start years for historical and projection periods

#tspan <- (proj_ystart - hist_ystart) / 10  #number of decades between time periods

dplyr.summarise.inform <- FALSE  #remove messages when using summarise()

# Colour palette for plotting
#col_pal <- wes_palette("Darjeeling1")


######################## LOAD DATA AND SCRIPTS #########################

# Loading data and scripts
source(file.path(code_root, "1a_CU_import.R"))   #CU table


source(file.path("code", "2c_FW_rearing_stats.R"))   #rearing stats script

cu_boundary <- st_read(file.path(paths$spatial, "CU_boundaries", "fraser_cus.shp")) %>%
  st_make_valid() %>%
  st_transform(crs = 3005)  %>%  #crs 3005 is NAD83/BC Albers
  left_join(select(cu_Fr, cuid, FULL_CU_IN, spp), 
            join_by(CUID == cuid)) %>%
  filter(!is.na(FULL_CU_IN))

nuseds_Fr <- read_csv(file.path(paths$salmon, "NuSEDS_CU_System_sites_202406.csv")) %>%
  st_as_sf(coords = c("X_LONGT", "Y_LAT"), crs = 4269) %>%
  st_transform(3005) %>%
  filter(USAGE != "REMOVE")

### Load freshwater spatial data
load(file.path(paths$fw, "2025-06-05_fw_rearing_spatial_models.Rdata"))
load(file.path(paths$fw, "2025-05-27_fw_streampicks.Rdata"))
load(file.path(paths$fw, "2025-07-25_fw_upstream_paths.Rdata"))

#load(here("processed_data", "freshwater", "R_data", "2025-01-24_fw_FAZ_streams.Rdata"))   #FAZ selections of streams

#load summary stat results
load(file.path(paths$fw,  "2025-06-12_fw_rearing_models_indicators.Rdata"))
load(file.path(paths$fw, "2025-07-31_migr_stats.Rdata"))

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

source(file.path(paths$code, "4a_CU_scoring.R"))

######################## MARKDOWN REPORTS #####################

#load FW rearing indicators
fwR_all_flat <- read_csv(file.path(paths$fw, "2025-06-13_fw_rearing_stats.csv"))

fwR_one <- filter(fwR_all_flat, period == "3", RCP == "45")
ggplot() +
  geom_point(data = fwR_one, aes(x = avg_lon, y= prop_snow)) 


#load migration indicators
load(file.path(paths$fw, "2025-07-30_migr_stats.Rdata"))

#load migration paths
load(file.path(paths$fw, "2025-07-25_fw_upstream_paths.Rdata"))


for(i in 1:n.CUs) {

CU_IN_i <- cu_run$FULL_CU_IN[i]
CU_IN_i <- "CO-5"
  
rmarkdown::render(
  file.path(here(),"code", "markdown", "0a_CU_profile.Rmd"),
  output_file = paste(today, CU_IN_i, "_profile.html", sep = "_"),
  output_dir = here("output", "CU_profiles"),
  output_format = "html_document", 
  params = list(FULL_CU_IN = CU_IN_i))

## overview of freshwater spawning indicators
rmarkdown::render(
  file.path(here("code", "markdown", "2_FW_spawning_report.Rmd")),
  output_file = paste(today, CU_IN_i,"fw_spawning.html", sep = "_"),
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


