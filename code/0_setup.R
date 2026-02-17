#----------------- 0_setup.R -------------------------------
# load libraries, set paths, load utility functions and tables
# select CUs to run for analysis
# used in all scripts in the CVIS package

# ------------ setup WD and load key libraries---------------------

library(here)

library(janitor)

library(tidyverse)
library(data.table) # for faster processing of some operations

# spatial packages
library(sf) # spatial feature
library(stars) # package for data cubes (multi-dimensional spatial arrays)

## plotting packages
# library(wesanderson); library(viridis)  #colour palettes
library(patchwork) # for multi-panel plots
library(corrplot) # correlation matrix plots
library(ggspatial) # enhanced ggplot maps - base map tiles
library(gt) # nice data tables
# library(ggforce)   # for custom facet sizes
library(ggtext) # for coloured text in axis labels
library(RColorBrewer)
library(scico) # scientific colour palettes
library(gridExtra) # grid-based plots, used for indicator plots
# library(ggsci)   # colour palettes - pal_futurama

formals(read_csv)$show_col_types <- F # use read_csv quietly

`%notin%` <- Negate(`%in%`) # function that is opposite of %in%

#--------------- Directory setup
# setwd("C:/Users/LAGASSEC/OneDrive - DFO-MPO/0.Workspace/CVIS")

today <- Sys.Date()

# Set file paths
paths <- list(
  climate = file.path(here(".."), "0_data_climate"),
  spatial = file.path(here(".."), "0_data_spatial"),
  salmon = file.path(here(".."), "0_data_salmon"),
  fw = here("processed_data", "freshwater"),
  marine = here("processed_data", "marine"),
  output = here("output"),
  CU = here("processed_data", "CU"),
  figures = here("output", "figures"),
  reports = here("reports"),
  code = here("code"),
  params = here("processed_data", "params")
)



# Analysis configurations -------------------------------------------------

# CUs to subset
DFO_area_include <- c("FRASER AND INTERIOR")
CU_exclude <- c(
  "CK-01", # exclude okanagan Chinook
  "CK-02", # exclude boundary bay Chinook
  "SEL-01-01", # okanagan sockeye
  "SER-03"
) # Widgeon  (throws errors)
species_include <- c("Chinook", "Coho", "Sockeye", "Chum", "Pink")
CU_type_include <- c("Current") # only include Current CUs, exclude extirpated ones

CI_type <- c("ALL") # habitat types to use for cumulative impacts (marine)
# options include:  ALL, dp, bh, eg, sr, kp
cthr_pick <- "cthr_anad" # cumulative threat type to use for indicator - default anadromous (i.e. sum of all)

periods_use <- c(0, 3, 5) # period codes to keep (see period_lookup for corresponding years)

#default models to use when combining standardized scores
dsmodel_baseline <- c("observed", "ENM", "streamdyn", "tscapes", "pcicgrid", "qdm", "CImpact", cthr_pick)
    #excluded station, bccmssc

# lower and upper quantiles for spatial variation statistics
qlsp <- 0.1
qhsp <- 0.9

qlgcm <- 0.1 # lower quantile for statistics on GCM variation
qhgcm <- 0.9 # upper quantile for statistics

T_model <- "tw8" # temperature model tw8 = thermalscapes August temp, alternative of 7DEC (not implemented)

fw_habitat_selection <- switch(1,
  "rs",
  "acc"
) # choose how to subset stream network for spawning and rearing indicators
# rs = use BCfishpass rearing and spawning habitat (from accessible habitat)
# acc = use all BCfishpass accessible habitat

historical_code <- "0" # historical climatology period for temperature models
# 0 = 1981-2000,  1 = 2001-2020.  For flow 0 = 1981-2010

# choose stream base network
base_network <- switch(1,
  "tscapes",
  "bcfpa"
) # only tscapes currently implemented, bcfpa has FWA network broken into smaller segments

# vector of RCP codes
rcp_vec <- c("0", "00", "45", "85")

# for converting CU status categories to a numeric
status_map <- c(Green = 1, Amber = 2, Red = 3)

# choose method for determining nearshore residency period for marine indicators
# static = same months used for all CUs,  peak_offset = offsets from peak ocean entry month used
ns_time_method <- switch(2,
  "static",
  "peak_offset"
)

ns_start_static <- 4 # for static method, start month
ns_end_static <- 7 # for static method, end month included
ns_start_offset <- 2 # amount of months before peak ocean entry month to include when calculating nearshore marine indicators
ns_end_offset <- 2 # amount of months after peak ocean entry month for calculating nearshore marine indicators

min_gen_red <- 500 # if generational avg spawners is below this value and RapidStatus is None, status will be adjusted to Red


# Lookup and definition tables --------------------------------------------

# translation table between periods
## note that not all periods are exactly the same among data sets, but average year is close
period_lookup <- tribble(
  ~period_code, ~period, ~start_year, ~end_year, ~dsmodel,
  0, "1981-2000", 1981, 2000, "tscapes",
  1, "2001-2020", 2001, 2020, "tscapes",
  2, "2021-2040", 2021, 2040, "tscapes",
  3, "2041-2060", 2041, 2060, "tscapes",
  4, "2061-2080", 2061, 2080, "tscapes",
  5, "2081-2099", 2081, 2100, "tscapes",
  0, "1981-2010", 1981, 2010, "streamdyn",
  2, "2021-2040", 2021, 2040, "streamdyn",
  3, "2041-2060", 2041, 2060, "streamdyn",
  4, "2061-2080", 2061, 2080, "streamdyn",
  5, "2081-2099", 2081, 2100, "streamdyn",
  0, "1981-2010", 1981, 2010, "pcicgrid",
  2, "2021-2040", 2021, 2040, "pcicgrid",
  3, "2041-2060", 2041, 2060, "pcicgrid",
  4, "2061-2080", 2061, 2080, "pcicgrid",
  5, "2081-2099", 2081, 2100, "pcicgrid",
  0, "1981-2010", 1981, 2010, "CMIP6_SST",
  3, "2041-2060", 2041, 2060, "CMIP6_SST",
  5, "2081-2099", 2081, 2100, "CMIP6_SST",
  0, "1981-2010", 1981, 2010, "BCCM",
  3, "2041-2060", 2041, 2070, "BCCM",
  0, "1981-2010", 1986, 2005, "SSC",
  3, "2041-2060", 2046, 2065, "SSC",
)

# table of indicator abbreviations and full names
tbl_indicators <- tribble(
  ~abbrev, ~category, ~long_type, ~std_fun, ~unit, ~name,
  "favchange", "fwrs", "Freshwater Spawning and Rearing", "invlinear_std", "Favourability", "ENM Change in Favourability",
  "cthr", "fwrs", "Freshwater Spawning and Rearing", "linear_std", "Threat score", "Cumulative threats to freshwater habitat",
  "tw8rate", "fwrs", "Freshwater Spawning and Rearing", "linear_std", "Temperature change per decade (°C)", "Rate of change in August Temperature",
  "tw8proj", "fwrs", "Freshwater Spawning and Rearing", "exponential_std", "Temperature (°C)", "Projected August Temperature",
  "flow8pdelta", "fwrs", "Freshwater Spawning and Rearing", "decay_std", "Proportion change from baseline", "Proportional change in August flow",
  "flow18pdelta", "fwrs", "Freshwater Spawning and Rearing", "exponential_std", "Proportion change from baseline", "Proportional change in Nov-Jan flow",
  "fwres", "fwrs", "Freshwater Spawning and Rearing", "step_std", "Number of days", "Freshwater residency time",
  "migrTproj", "migr", "Upstream Migration", "exponential_std", "Temperature (°C)", "Projected temperature during upstream migration",
  "migrQpdelta", "migr", "Upstream Migration", "decay_std", "Proportion change", "Proportional change in discharge during upstream migration",
  "migrdist", "migr", "Upstream Migration", "linear_std", "Kilometres", "Length of upstream migration",
  "SSTproj", "mar", "Nearshore Marine", "exponential_std", "Temperature (°C)", "Projected nearshore SST during ocean entry",
  "SSTrate", "mar", "Nearshore Marine", "linear_std", "Temperature change per decade (°C)", "Rate of change in nearshore SST",
  "CImpact", "mar", "Nearshore Marine", "linear_std", "Threat score", "Cumulative impacts to marine nearshore habitat",
  "CUstatus", "dem", "Demographics", "cat_std", "Status", "WSP status",
  "CUnmat", "dem", "Demographics", "decay_std", "Number of spawners", "Number of mature individuals",
  "hetzyg", "gen", "Genetics", "invlinear_std", "Heterozygosity", "Genetic heterozygosity",
  "genoff", "gen", "Genetics", "linear_std", "Genomic offset", "Genomic offset"
)


tbl_ind_report <- tbl_indicators %>%
  select(abbrev, long_type, name) %>%
  rename(
    Abbreviation = abbrev,
    Category = long_type,
    Description = name
  )

tbl_standardize <- tribble(
  ~abbrev, ~category, ~std_fun, ~range_type, ~lambda, ~xmin, ~xmax,
  "favchange", "fwrs", "invlinear_std", "all", NA, NA, 0,
  "cthr", "fwrs", "linear_std", "all", NA, 0, NA,
  "tw8rate", "fwrs", "linear_std", "all", NA, NA, NA,
  "tw8proj", "fwrs", "exponential_std", "all", 3, 15, NA,
  "flow8pdelta", "fwrs", "decay_std", "all", 3, NA, 0,
  "flow18pdelta", "fwrs", "exponential_std", "all", 3, 0, NA,
  "fwres", "fwrs", "step_std", "all", NA, NA, NA,
  "migrTproj", "migr", "exponential_std", "all", 3, 15, NA,
  "migrQpdelta", "migr", "decay_std", "all", 3, 0, 0,
  "migrdist", "migr", "linear_std", "all", NA, NA, NA,
  "SSTproj", "mar", "exponential_std", "all", 3, 8, 18,
  "SSTrate", "mar", "linear_std", "all", NA, 0.1, 0.3,
  "CImpact", "mar", "linear_std", "all", NA, NA, NA,
  "CUstatus", "dem", "cat_std", "all", NA, NA, NA,
  "CUnmat", "dem", "decay_std", "all", 3, 0, 10000,
  "hetzyg", "gen", "invlinear_std", "species", NA, NA, NA,
  "genoff", "gen", "linear_std", "species", NA, NA, NA
)


# mapping of field codes to gcm names for thermalscapes model
gcm_codes <- tribble(
  ~gcm, ~gcm_name,
  "0",  "historical",
  "1",  "canesm2",
  "2",  "csiro",
  "3",  "gfdl",
  "4",  "hadgem2",
  "5",  "miroc",
  "6",  "mpi",
  "7",  "access1", # PCIC model
  "8",  "cnrm", # PCIC model
  "9",  "ensemble",
  "20", "ccsm4" # PCIC model
)


save(tbl_indicators, tbl_standardize, tbl_ind_report, file = file.path(paths$params, "indicator_tables.Rdata"))

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



# Color palettes ----------------------------------------------------------

## color palette function
get_scico_palette <- function(data, column, palette_name = "berlin") {
  categories <- sort(unique(data[[column]]))
  setNames(scico(length(categories), palette = palette_name), categories)
}

get_brewer_palette <- function(data, column, palette_name = "Set2") {
  categories <- sort(unique(data[[column]]))

  setNames(brewer.pal(length(categories), name = palette_name), categories)
  # brewer.pal(n, palette)
}

# Define species color palette (adjust as needed)
# species_palette <- get_brewer_palette(cu_run, "SPECIES_NAME", "Set1")
species_palette <- c(
  "Chinook" = "#1b9e77",
  "Coho" = "darkblue",
  "Sockeye" = "firebrick4",
  "Pink" = "purple3",
  "Chum" = "goldenrod4"
)

# Indicator palette used for labelling indicator categories
indicator_palette <- c(
  "Demographics" = "purple",
  "Spawning & Rearing" = "turquoise",
  "Upstream Migration" = "royalblue",
  "Nearshore Marine" = "green4",
  "Genetics" = "orange3"
)


# Run utility and plot scripts --------------------------------------------

# load CU boundaries
load(file.path(paths$fw, "cu_boundary.Rds"))

# load utility functions
source(here("code", "2_fw_utils.R"))
source(here("code", "3_marine_utils.R"))
source(here("code", "4_scoring_utils.R"))

# load plotting functions
source(here("code", "5a_plots_CU.R"))
source(here("code", "5b_plots_compare.R"))

# load CU tables
source(here("code", "1a_CU_import.R")) # CU table
