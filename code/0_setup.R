
# ------------ setup WD and load key libraries---------------------

library(here)

library(tidyverse)
#use data.table for speed, use dtplyr for dplyr syntax
library(data.table)
library(dtplyr)
library(dplyr, warn.conflicts = FALSE)

#spatial packages
library(sf)      #spatial feature
library(stars)   #package for data cubes (multi-dimensional spatial arrays)

library(rcartocolor) #mapping palettes
library(wesanderson); library(viridis)  #colour palettes
library(patchwork) #for multi-panel plots
library(units)   #for unit conversion

`%notin%` <- Negate(`%in%`)

#--------------- Directory setup
#setwd("C:/Users/LAGASSEC/OneDrive - DFO-MPO/0.Workspace/CVIS")

today <- Sys.Date()

# Set file paths

paths <- list(
    climate = file.path(here(".."), "0_data_climate"),
    spatial = file.path(here(".."), "0_data_spatial"),
    salmon  = file.path(here(".."), "0_data_salmon"),
    fw      = here("processed_data", "freshwater")
  )


#load utility functions
source(here("code", "2_fw_utils.R"))
source(here("code", "3_marine_utils.R"))

#load CU tables
source(here("code", "1a_CU_import.R"))   #CU table
