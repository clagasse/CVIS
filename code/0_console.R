### 0_console  

#load packages and set root project directory
rm(list=ls())

library(here)
library(tidyverse)
library(stars)   #package for data cubes (multi-dimensional spatial arrays)
#library(ncdf4)
library(sf)      #spatial features
library(wesanderson); library(viridis)  #colour palettes
`%notin%` <- Negate(`%in%`)
library(gridExtra)  #for multi-panel ggplots

setwd(here())

today <- Sys.Date()

dplyr.summarise.inform <- FALSE  #remove messages when using summarise()

# Colour palette for plotting
col_pal <- wes_palette("Darjeeling1")

## periods to run for analysis
t_periods <- c("hist", "mid")

#vector of indicator names for analysis
PCIC_indies <- c("peakFlowAmt",     #ANNUAL peak flow amount 
                 "peakFlowDay",   #ANNUAL date of peak flow during year
                 "POT19freq",    #ANNUAL frequency of days above 19 degrees
                 "POT19dur",      #ANNUAL spell lenght of days above 19 degrees
                 "lowQ05",       #ANNUAL frequency of low flow events
                 "highQ95"       #ANNUAL frequency of high flow events
                 )


###############################################################################
# Read in relevant spatial datasets
###############################################################################

#------------------------------------------------------------------------------
# Up-to-date CU list (taken from database)
#------------------------------------------------------------------------------

cu_list <- read.csv(here("data", "all_regions_cu_du_smu_decoder.csv")) %>%
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
cu_run <- cu_list#filter(cu_list, cuid %in% c(311,312, 750, 704, 718, 740, 701, 710))

#remove Widgeon (for now due to throwing errors)
cu_run <- filter(cu_list, cuname %notin% c("Widgeon", "Harrison River"))


spp_lookup_run <- spp_lookup[spp_lookup$streams_code %in% unique(cu_run$spp),]

# Create vector of CUs to analyze, ordered CK, CM, CO, PKO, SEL, SER, SH

cuid <- cu_run$cuid[order(cu_run$spp)]

n.CUs <- length (cuid)


#########################################################################
# Freshwater code 
#########################################################################

#load freshwater functions
source(here("freshwater", "code", "2_freshwater_utils.R"))

# Set root for spatial datasets
dat_root <- here("freshwater", "data", "spatial")


# Define freshwater stages
stages <- c("spawning", "fw_rearing") #c("adult_migration", "spawning", "eggs_alevin", "fw_rearing")


### run script to import freshwater data layers

source(here("freshwater", "code", "2a_fw_import.R"))


### run script to define and plot freshwater distributions by life stage

#set to TRUE to make pdfs for each CU
make_CU_plots <- FALSE

source(here("freshwater", "code", "2c_fw_CU_dist.R"))

save.image(here("freshwater", "output", paste0(today, "_output.Rdata")))


#PCIC_incl <- readRDS(here("freshwater", "output", "PCIC_incl.rds"))


### run script to calculate indicators for freshwater life stages







