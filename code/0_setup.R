#----------------- 0_setup.R -------------------------------
# load libraries, set paths, load utility functions and tables
# select CUs to run for analysis
# used in all scripts in the CVIS package

# ------------ setup WD and load key libraries---------------------

library(here)

library(tidyverse)
# use data.table for speed, use dtplyr for dplyr syntax
library(data.table)
# library(dtplyr)

# spatial packages
library(sf)      # spatial feature
library(stars)   # package for data cubes (multi-dimensional spatial arrays)

`%notin%` <- Negate(`%in%`)

#--------------- Directory setup
# setwd("C:/Users/LAGASSEC/OneDrive - DFO-MPO/0.Workspace/CVIS")

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


# load utility functions
source(here("code", "2_fw_utils.R"))
source(here("code", "3_marine_utils.R"))
source(here("code", "4_scoring_utils.R"))

# load CU tables
source(here("code", "1a_CU_import.R"))   # CU table

# load plotting functions
# source(here("code", "5a_plots_CU.R"))
# source(here("code", "5b_plots_compare.R"))


# Select subset of CUs to run for analysis
cu_run <- cu_Fr %>%
  filter(spp %in% c("ck", "co", "cm", "sk"),  # exclude pink salmon for now
    FULL_CU_IN %notin% c("SER-02", "SER-03")) %>% # remove widgeon and Harrison river for now (throws error)
  arrange(spp)


cuid    <- cu_run$cuid # Create vector of CUs to analyze, ordered CK, CM, CO, PKO, SEL, SER, SH
cu_seq  <- cu_run$FULL_CU_IN # Create vector of CUs to analyze, ordered CK, CM, CO, PKO, SEL, SER, SH

n.CUs   <- nrow(cu_run)





##### ------ Lookup and definition tables

# translation table between periods
## note that not all periods are exactly the same among data sets, but average year is close
period_lookup <- tribble(
  ~period_code, ~period,
  0, "1981-2000",
  1, "2011-2020",
  2, "2021-2040",
  3, "2041-2060",
  4, "2061-2080",
  5, "2081-2099"
)

# table of indicator abbreviations and full names
tbl_indicators <- tribble(
  ~abbrev,      ~type,    ~stat, ~std_fun, ~name,
  "Favchange", "fwR",    "mean",     "decay_std",     "ENM Change in Favourability",
  "ct",         "fwR",    "mean",    "linear_std",        "Cumulative threats to freshwater habitat",
  "Tw8rate",    "fwR",    "mean",    "exponential_std",        "Rate of change in August Temperature",
  "Tw8proj",    "fwR",    "mean",    "exponential_std", "Projected August Temperature",
  "lowQpdelta", "fwR",   "mean",    "decay_std",       "Proportional change in August flow (stream model)",
  "st8pdelta",  "fwR",    "mean",    "decay_std",     "Proportional change in August flow (station model)",
  "highQpdelta",  "fwR",   "mean",    "exponential_std", "Proportional change in Nov-Jan flow (stream model)",
  "fwres",     "fwR",    "value",    "step_std",   "Freshwater residency time",
  "migrT",      "migr",   "mean",    "exponential_std",     "Projected temperature during upstream migration",
  "migrQ",      "migr",   "mean",      "linear_std",    "Projected discharge during upstream migration",
  "migrA21",    "migr",   "mean",    "exponential_std", "Average proportion of path above 21 degrees during upstream migration",
  "migrdist",   "migr",  "value",      "linear_std",     "Length of upstream migration",
  "SSTproj",     "mar",   "mean", "linear_std",  "Projected nearshore SST during ocean entry",
  "CI",          "mar",   "mean", "linear_std",   "Cumulative impacts to marine nearshore habitat",
  "CUstatus",    "dem",   "category",    "cat_std", "WSP status",
  "CUnmat",      "dem",   "value",  "decay_std", "Number of mature individuals")



tbl_standardize <- tribble(
  ~abbrev,      ~type,      ~std_fun,        ~lambda, ~xmin, ~xmax,
  "Favchange", "fwR",        "decay_std",        3,     NA,   0,
  "ct",         "fwR",      "linear_std",        NA,    0,   NA,
  "Tw8rate",    "fwR",      "exponential_std",    3,    NA,   NA,
  "Tw8proj",    "fwR",       "exponential_std",   3,    14,   NA,
  "lowQpdelta",  "fwR",    "decay_std",           3,    NA,   0,
  "st8pdelta",  "fwR",    "decay_std",            3,    NA,   0,
  "highQpdelta", "fwR",    "exponential_std",     3,    0,   NA,
  "fwres",     "fwR",      "step_std",            NA,   NA,   NA,
  "migrT",      "migr",    "exponential_std",     3,    13,   NA,
  "migrQ",      "migr",       "linear_std",      NA,   NA,   NA,
  "migrA21",    "migr",     "exponential_std",    3,    0,    NA,
  "migrdist",   "migr",      "linear_std",        NA,   NA,   NA,
  "SSTproj",     "mar",   "linear_std",           NA,   NA,   NA,
  "CI",          "mar",   "linear_std",           NA,   NA,   NA,
  "CUstatus",    "dem",       "cat_std",        NA,    NA,   NA,
  "CUnmat",      "dem",     "decay_std",         3,    0, 10000)



### ----- Load frequently used data sets- ------


### Conservation Unit boundaries for Fraser CUs
cu_boundary <- st_read(file.path(paths$spatial, "CU_boundaries", "fraser_cus.shp")) %>%
  st_make_valid() %>%
  st_transform(crs = 3005)  %>%  # crs 3005 is NAD83/BC Albers
  left_join(select(cu_Fr, cuid, FULL_CU_IN, spp),
    join_by(CUID == cuid)) %>%
  filter(!is.na(FULL_CU_IN))


### NUSEDS salmon spawner locations
## version from FIA. Usage column added by Michael Arbeider
nuseds_Fr <- read_csv(file.path(paths$salmon, "NuSEDS_CU_System_sites_202406.csv")) %>%
  st_as_sf(coords = c("X_LONGT", "Y_LAT"), crs = 4269) %>%
  st_transform(3005) %>%
  filter(USAGE != "REMOVE")

#  field descriptions
# n = number of surveys that were not “UNKNOWN” or “NOT INSPECTED”, i.e. they were inspected but sometimes only PRESENSE was recorded and not an abundance.
# last.year = last year when the system was surveyed
# first.year = first year when the system was surveyed
# max.count = the largest count of spawners in NuSEDs
# ave.count = the mean of all non-NA counts in NuSEDs
# min.count = the minimum

# usage criteria for nuseds file
# cu.sites <- cu.sites %>%
#   mutate(USAGE = case_when(
#     n < 5 & last.year < 2010 ~ "REMOVE",
#     n < 5 & last.year >= 2010 ~ "CAUTION",
#     n >= 5 & last.year < 1999 & SPECIES_LOOKUP != "Pink" ~ "CAUTION",
#     n >= 5 & max.count == 0 & last.year < 1999 & SPECIES_LOOKUP != "Pink" ~ "CAUTION",
#     n >= 5 & max.count != 0 & last.year < 1999 & SPECIES_LOOKUP == "Pink" ~ "KEEP",
#     n >= 5 & max.count == 0 & last.year >= 1999 ~ "CAUTION",
#     n >= 5 & max.count != 0 & last.year >= 1999 ~ "KEEP"
#   ))



# ggplot custom theme -----------------------------------------------------

theme_cvis <- function(base_size = 14) {
  theme_bw(base_size = base_size) %+replace%
    theme(
      # L'ensemble de la figure
      plot.title = element_text(size = rel(0.9), face = "bold", margin = margin(0, 0, 5, 0)),
      # Zone où se situe le graphique
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      # Les axes
      axis.title = element_text(size = rel(0.75), face = "bold"),
      axis.text = element_text(size = rel(0.60), face = "bold"),
      axis.line = element_line(color = "black"),
      # La légende
      legend.title = element_text(size = rel(0.75), face = "bold"),
      legend.text = element_text(size = rel(0.65), face = "bold"),
      legend.key = element_rect(fill = "transparent", colour = NA),
      legend.key.size = unit(1.2, "lines"),
      legend.background = element_rect(fill = "transparent", colour = NA),
      # Les étiquettes dans le cas d'un facetting
      strip.background = element_rect(fill = "black", color = "black"),
      strip.text = element_text(size = rel(0.6), face = "bold", color = "white", margin = margin(2, 0, 2, 0))
    )
}
theme_set(theme_cvis())
