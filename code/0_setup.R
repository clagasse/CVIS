#----------------- 0_setup.R -------------------------------
#load libraries, set paths, load utility functions and tables
# select CUs to run for analysis
# used in all scripts in the CVIS package 

# ------------ setup WD and load key libraries---------------------

library(here)

library(tidyverse)
#use data.table for speed, use dtplyr for dplyr syntax
library(data.table)
#library(dtplyr)

#spatial packages
library(sf)      #spatial feature
library(stars)   #package for data cubes (multi-dimensional spatial arrays)

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
    indicators = here("processed_data"),
    figures = here("output", "figures"),
    reports = here("output", "reports"),
    code    = here("code")
  )


#load utility functions
source(here("code", "2_fw_utils.R"))
source(here("code", "3_marine_utils.R"))
source(here("code", "4_scoring_utils.R"))

#load CU tables
source(here("code", "1a_CU_import.R"))   #CU table

#load plotting functions
# source(here("code", "5a_plots_CU.R"))
# source(here("code", "5b_plots_compare.R"))


# Select subset of CUs to run for analysis
cu_run <- cu_Fr %>%
  filter(spp %in% c("co"),  # c("ck", "co", "cm", "sk")
         FULL_CU_IN %notin% c("SER-02", "SER-03")) %>% #remove widgeon and Harrison river for now (throws error)
  arrange(spp)    


cuid    <- cu_run$cuid# Create vector of CUs to analyze, ordered CK, CM, CO, PKO, SEL, SER, SH
cu_seq  <- cu_run$FULL_CU_IN # Create vector of CUs to analyze, ordered CK, CM, CO, PKO, SEL, SER, SH

n.CUs   <- nrow(cu_run)


##### ------ Lookup and definition tables

#translation table between periods
## note that not all periods are exactly the same among data sets, but average year is very close
period_lookup <- tribble(
  ~period_code, ~period,
  0, "1981-2000",
  1, "2011-2020",
  2, "2021-2040",
  3, "2041-2060",
  4, "2061-2080",
  5, "2081-2099"
)

#table of indicator abbreviations and full names
tbl_indicators <- tribble(
  ~abbrev,      ~type,    ~stat, ~std_fun, ~name, 
  "Fav_change", "fwR",    "mean",   "invlinear_std",     "ENM Change in Favourability",
  "ct",         "fwR",    "mean",    "linear_std",        "Cumulative threats to freshwater habitat",
  "Tw8rate",    "fwR",    "mean",    "linear_std",        "Rate of change in August Temperature",
  "Tw8proj",    "fwR",    "mean",     "exponential_std", "Projected August Temperature",
  "highQpdelta", "fwR",   "mean",   "linear_std", "Proportional change in August flow (stream model)",
  "lowQpdelta",  "fwR",   "mean",   "invlinear_std", "Proportional change in Nov-Jan flow (stream model)",
  "st8pdelta",  "fwR",    "mean", "invlinear_std", "Proportional change in August flow (station model)",
  "fw_res",     "fwR",    "raw",  "step_std",   "Freshwater residency time",
  "migrT",      "migr",   "mean",    "linear_std",     "Projected temperature during upstream migration",
  "migrQ",      "migr",   "mean",      "linear_std",    "Projected discharge during upstream migration",
  "migrA21",    "migr",   "mean",    "exponential_std", "Average proportion of path above 21 degrees during upstream migration",
  "migr_wdist",   "migr", "raw",      "linear_std",     "Length of upstream migration",
  "SSTproj",     "mar",   "mean", "linear_std",  "Projected nearshore SST during ocean entry",
  "CI",          "mar",   "mean", "linear_std",   "Cumulative impacts to marine nearshore habitat",
  "CUstatus",    "dem",   "raw",    "cat_std", "WSP status",
  "CUnmat",      "dem",   "raw",  "decay_std", "Number of mature individuals")



