
# ------------ setup WD and load key libraries---------------------

library(here)

library(tidyverse)
#use data.table for speed, use dtplyr for dplyr syntax
library(data.table)
#library(dtplyr)

#spatial packages
library(sf)      #spatial feature
library(stars)   #package for data cubes (multi-dimensional spatial arrays)

#library(rcartocolor) #mapping palettes
library(ggsci) #colour palettes
#library(wesanderson); library(viridis)  #colour palettes
library(patchwork) #for multi-panel plots

`%notin%` <- Negate(`%in%`)

#--------------- Directory setup
#setwd("C:/Users/LAGASSEC/OneDrive - DFO-MPO/0.Workspace/CVIS")

today <- Sys.Date()

# Set file paths

paths <- list(
    climate = file.path(here(".."), "0_data_climate"),
    spatial = file.path(here(".."), "0_data_spatial"),
    salmon  = file.path(here(".."), "0_data_salmon"),
    fw      = here("processed_data", "freshwater"),
    marine  = here("processed_data", "marine"),
    figures = here("output"),
    reports = here("output", "reports"),
    code    = here("code")
  )


#load utility functions
source(here("code", "2_fw_utils.R"))
source(here("code", "3_marine_utils.R"))
source(here("code", "4_scoring_utils.R"))

#load plotting functions
source(here("code", "0b_plots.R"))

#load CU tables
source(here("code", "1a_CU_import.R"))   #CU table



# Select subset of CUs to run for analysis
cu_run <- cu_Fr %>%
  filter(spp %in% c("ck", "co", "cm", "sk"), 
         FULL_CU_IN %notin% c("SER-02", "SER-03")) %>%
  arrange(spp)    #remove widgeon (throws error)

cuid    <- cu_run$cuid# Create vector of CUs to analyze, ordered CK, CM, CO, PKO, SEL, SER, SH
cu_seq  <- cu_run$FULL_CU_IN # Create vector of CUs to analyze, ordered CK, CM, CO, PKO, SEL, SER, SH

n.CUs   <- nrow(cu_run)


