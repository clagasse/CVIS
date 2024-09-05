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

setwd(here())

today <- Sys.Date()

dplyr.summarise.inform <- FALSE  #remove messages when using summarise()

#set to TRUE to make pdfs for each CU
make_CU_plots <- FALSE

# Colour palette for plotting
col_pal <- wes_palette("Darjeeling1")

## periods to run for analysis
t_periods <- c("hist", "mid")

#vector of indicator names for analysis
PCIC_indies <- c("peakQmag_year",     #ANNUAL peak flow amount 
                 "peakQday_year",   #ANNUAL date of peak flow during year
                 "POT19freq_year",    #ANNUAL frequency of days above 19 degrees
                 "POT19dur_year",      #ANNUAL spell lenght of days above 19 degrees
                 "lowQ05_year",       #ANNUAL frequency of low flow events
                 "highQ95_year"       #ANNUAL frequency of high flow events
                 )

PCIC_file_choose <- c("peakFlowAmt",
                         "peakFlowDay",
                         "POT19freq",
                         "POT19dur",
                         "lowQ05",
                         "highQ95")
                         


###############################################################################
# Read in relevant spatial datasets
###############################################################################

#------------------------------------------------------------------------------
# Up-to-date CU list (taken from database)
#------------------------------------------------------------------------------

cu_list <- read.csv(here("data", "demographic", "all_regions_cu_du_smu_decoder.csv")) %>%
  subset(Area == "FRASER") %>%
  filter(!is.na(cuid))

# Create variable for pooled species
cu_list$species_pooled <- cu_list$Species
cu_list$species_pooled[cu_list$species_name %in% c("Pink (odd)", "Pink (even)")] <- "Pink"
cu_list$species_pooled[cu_list$species_name %in% c("Lake sockeye", "River sockeye")] <- "Sockeye"

# Remove CUs that are considered Extinct
cu_list <- cu_list[which(cu_list$COSEWIC_status != "Extinct" | is.na(cu_list$COSEWIC_status)), ]

#remove records with multiple rows for single CU (due to multiple DUs)
cu_list <- distinct(cu_list, cuid, cuname, .keep_all = TRUE)

# Create lookup for spawning and rearing fields in the geodatabase
spp_lookup <- data.frame(
  species_pooled = sort(unique(cu_list$species_pooled)),
  streams_code = c("ch", "cm", "co", "pk", "sk")
)

cu_list$spp <- spp_lookup$streams_code[match(cu_list$species_pooled, spp_lookup$species_pooled, nomatch=NA)]
  
# Select subset of CUs to run for analysis
cu_run <- cu_list #filter(cu_list, cuid %in% c(311,312, 750, 704, 718, 740, 701, 710))

#remove Widgeon (for now due to throwing errors)
cu_run <- filter(cu_list, cuname %notin% c("Widgeon", "Harrison River"))

spp_lookup_run <- spp_lookup[spp_lookup$streams_code %in% unique(cu_run$spp),]

# Create vector of CUs to analyze, ordered CK, CM, CO, PKO, SEL, SER, SH

cuid <- cu_run$cuid[order(cu_run$spp)]

n.CUs <- length (cuid)

save(PCIC_indies, col_pal, today, cu_run, spp_lookup_run, cu_list,
     file = here("data", "settings.Rdata"))


#########################################################################
# Freshwater code 
#########################################################################

#load freshwater functions
source(here("code", "2_freshwater_utils.R"))

# Set root for spatial datasets
dat_root <- here("data", "freshwater", "spatial")

# Define freshwater stages
stages <- c("spawning", "fw_rearing") #c("adult_migration", "spawning", "eggs_alevin", "fw_rearing")


### run script to import freshwater data layers

#source(here("code", "2a_fw_import.R"))

#load spatial objects
load(here("data", "freshwater", "processed-data", "2024-09-04_fw_spatial_inputs.Rdata"))

### run script to define and plot freshwater distributions by life stage

#source(here("freshwater", "code", "2c_fw_CU_dist.R"))

#load summary stat results
load(here("output", "2024-09-04_fw_stats_output.Rdata"))



##### Render Output

rmarkdown::render(
  file.path(here("code","0_CVIS_report.Rmd")),
  output_file = paste(today, "fw_report.html", sep = "_"),
  output_dir = here("output"),
  output_format = "html_document")





