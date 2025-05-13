
# ------------ setup WD and load key libraries---------------------

library(here)
library(tidyverse)
library(sf)      #spatial feature


#setwd("C:/Users/LAGASSEC/OneDrive - DFO-MPO/0.Workspace/CVIS")
setwd(here(".."))

today <- Sys.Date()

# Set root for spatial datasets
dat_root <- file.path("..", "0.Workspace")
climate_dat <- file.path(dat_root, "0_data_climate")
spatial_dat <- file.path(dat_root, "0_data_spatial")
salmon_dat  <- file.path(dat_root, "0_data_salmon")
code_root <- file.path(here(), "code")


#load utility functions
source(file.path(code_root, "2_fw_utils.R"))
source(file.path(code_root, "3_marine_utils.R"))