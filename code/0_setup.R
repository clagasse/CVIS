#----------------- 0_setup.R -------------------------------
# load libraries, set paths, load utility functions and tables
# select CUs to run for analysis
# used in all scripts in the CVIS package

# ------------ setup WD and load key libraries---------------------

library(here)

library(janitor)

library(tidyverse)
library(data.table)  # for faster processing of some operations

# spatial packages
library(sf)      # spatial feature
library(stars)   # package for data cubes (multi-dimensional spatial arrays)

## plotting packages
# library(wesanderson); library(viridis)  #colour palettes
library(patchwork) # for multi-panel plots
library(corrplot)  # correlation matrix plots
library(ggspatial)  # enhanced ggplot maps - base map tiles
library(gt)        # nice data tables
# library(ggforce)   # for custom facet sizes
library(ggtext)  # for coloured text in axis labels
library(RColorBrewer)
library(scico)    # scientific colour palettes
library(gridExtra) # grid-based plots, used for indicator plots
# library(ggsci)   # colour palettes - pal_futurama

formals(read_csv)$show_col_types <- F  #use read_csv quietly

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
  output  = here("output"),
  CU      = here("processed_data", "CU"),
  figures = here("output", "figures"),
  reports = here("reports"),
  code    = here("code"),
  params = here("processed_data", "params")
)



# Analysis configurations -------------------------------------------------

periods_use <- c(0, 3, 5)  # period codes to keep (see period_lookup for corresponding years)

# lower and upper quantiles for spatial variation statistics
qlowsp <- 0.1
qhighsp <- 0.9

qlowgcm <- 0.1    # lower quantile for statistics on GCM variation
qhighgcm <- 0.9   # upper quantile for statistics

T_model <- "tw8"    # temperature model tw8 = thermalscapes August temp, alternative of 7DEC (not implemented)

historical <- "0"   # historical climatology period for temperature models
# 0 = 1981-2000,  1 = 2001-2020.  For flow 0 = 1981-2010

# choose stream base network
base_network <- switch(1, "tscapes", "bcfpa")  # only tscapes currently implemented, bcfpa has FWA network broken into smaller segments

CI_type <- c("ALL")   # habitat types to use for cumulative impacts (marine)
# options include:  ALL, dp, bh, eg, sr, kp

# choose method for determining nearshore residency period for marine indicators
# static = same months used for all CUs,  peak_offset = offsets from peak ocean entry month used
ns_time_method <- switch(2, "static", "peak_offset")

ns_start_static <- 4  # for static method, start month
ns_end_static   <- 7  # for static method, end month included
ns_start_offset <- 2   # amount of months before peak ocean entry month to include when calculating nearshore marine indicators
ns_end_offset   <- 2   # amount of months after peak ocean entry month for calculating nearshore marine indicators


min_gen_red <- 500  #if generational avg spawners is below this value and RapidStatus is None, status will be adjusted to Red


# Run utility and plot scripts --------------------------------------------

# load utility functions
source(here("code", "2_fw_utils.R"))
source(here("code", "3_marine_utils.R"))
source(here("code", "4_scoring_utils.R"))

# load plotting functions
source(here("code", "5a_plots_CU.R"))
source(here("code", "5b_plots_compare.R"))


# CU settings and import -------------------------------------------------------------

# load CU tables
source(here("code", "1a_CU_import.R"))   # CU table

# Select subset of CUs to run for analysis
cu_run <- cu_list %>%
  filter(DFO_AREA == "FRASER AND INTERIOR",
    CU_TYPE == "Current",
    CU_NAME != "BOUNDARY BAY_FA_0.3",
    str_detect(SMU_NAME, "OKANAGAN", negate = TRUE)) %>%
  filter(SPECIES_NAME %in% c("Chinook", "Coho", "Sockeye", "Chum", "Pink"),   # optional species filter
    FULL_CU_IN %notin% c("SER-02")) %>% # remove widgeon (throws error)
  arrange(SPECIES_NAME)

cu_seq  <- cu_run$FULL_CU_IN # Create vector of CUs to analyze, ordered CK, CM, CO, PKO, SEL, SER, SH
n.CUs   <- nrow(cu_run)

save(cu_run, file = file.path(paths$CU, "cu_run.Rds"))

# Lookup and definition tables --------------------------------------------

# translation table between periods
## note that not all periods are exactly the same among data sets, but average year is close
period_lookup <- tribble(
  ~period_code, ~period, ~start_year, ~end_year, ~model,
  0, "1981-2000", 1981, 2000, "tscapes",
  1, "2001-2020", 2001, 2020, "tscapes",
  2, "2021-2040", 2021, 2040, "tscapes",
  3, "2041-2060", 2041, 2060, "tscapes",
  4, "2061-2080", 2061, 2080, "tscapes",
  5, "2081-2099", 2081, 2100, "tscapes",
  0, "1981-2010", 1981, 2010, "stream_flow",
  2, "2021-2040", 2021, 2040, "stream_flow",
  3, "2041-2060", 2041, 2060, "stream_flow",
  4, "2061-2080", 2061, 2080, "stream_flow",
  5, "2081-2099", 2081, 2100, "stream_flow",
  0, "1981-2010", 1981, 2010, "PCIC_migr",
  3, "2041-2060", 2041, 2060, "PCIC_migr",
  5, "2081-2099", 2081, 2100, "PCIC_migr",
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
  ~abbrev,      ~type,  ~long_type,   ~stat, ~std_fun, ~unit, ~name,
  "favchange", "fwR",   "Freshwater Spawning and Rearing",  "mean",     "invlinear_std",   "Favourability",   "ENM Change in Favourability",
  "CT",         "fwR",  "Freshwater Spawning and Rearing",   "mean",    "linear_std",     "Threat score",    "Cumulative threats to freshwater habitat",
  "tw8rate",    "fwR",  "Freshwater Spawning and Rearing",   "mean",    "linear_std",    "Temperature change per decade (°C)",   "Rate of change in August Temperature",
  "tw8proj",    "fwR",  "Freshwater Spawning and Rearing",   "mean",    "exponential_std", "Temperature (°C)",  "Projected August Temperature",
  "lowQpdelta", "fwR",  "Freshwater Spawning and Rearing",  "mean",    "decay_std",  "Proportion change from baseline",     "Proportional change in August flow",
  # "st8pdelta",  "fwR",  "Freshwater Spawning and Rearing",   "mean",    "decay_std",   "Proportion change",  "Proportional change in August flow (station model)",
  "highQpdelta",  "fwR", "Freshwater Spawning and Rearing",  "mean",    "exponential_std", "Proportion change from baseline", "Proportional change in Nov-Jan flow",
  "fwres",     "fwR",    "Freshwater Spawning and Rearing", "value",    "step_std",  "Number of days",  "Freshwater residency time",
  "migrT",      "migr",  "Upstream Migration",  "mean",    "exponential_std",  "Temperature (°C)",   "Projected temperature during upstream migration",
  "migrQ",      "migr",  "Upstream Migration", "mean",      "decay_std",  "Proportion change",  "Proportional change in discharge during upstream migration",
  # "migrA21",    "migr",   "mean",    "exponential_std", "Proportion", "Average proportion of path above 21 degrees during upstream migration",
  "migrdist",   "migr",  "Upstream Migration", "value",      "linear_std",  "Kilometres",   "Length of upstream migration",
  "SSTproj",     "mar",  "Nearshore Marine",  "mean", "exponential_std", "Temperature (°C)", "Projected nearshore SST during ocean entry",
  "SSTrate",     "mar",  "Nearshore Marine", "mean", "linear_std",  "Temperature change per decade (°C)", "Rate of change in nearshore SST",
  "CImpact",      "mar", "Nearshore Marine",  "mean", "linear_std", "Threat score",  "Cumulative impacts to marine nearshore habitat",
  "CUstatus",    "dem",  "Demographics",  "category",    "cat_std", "Status", "WSP status",
  "CUnmat",      "dem",  "Demographics",  "value",  "decay_std",  "Number of spawners", "Number of mature individuals",
  "hetzyg",        "gen",  "Genetics",      "mean",    "invlinear_std", "Heterozygosity", "Genetic heterozygosity",
  "genoff",     "gen",    "Genetics",      "mean",    "linear_std",  "Genomic offset", "Genomic offset"
)



tbl_ind_report <- tbl_indicators %>%
  select(abbrev, long_type, name) %>%
  rename(Abbreviation = abbrev,
    Category = long_type,
    Description = name)

tbl_standardize <- tribble(
  ~abbrev,      ~type,      ~std_fun,          ~range_type,  ~lambda, ~xmin, ~xmax,
  "Favchange", "fwR",        "invlinear_std",  "all",      NA,     NA,   0,
  "ct",         "fwR",      "linear_std",      "all",    NA,    0,   NA,
  "Tw8rate",    "fwR",      "linear_std",      "all",    NA,    NA,   NA,
  "Tw8proj",    "fwR",       "exponential_std", "all",    3,    15,   NA,
  "lowQpdelta",  "fwR",    "decay_std",        "all",    3,    NA,   0,
  # "st8pdelta",  "fwR",    "decay_std",       "all",      3,    NA,   0,
  "highQpdelta", "fwR",    "exponential_std",  "all",    3,    0,   NA,
  "fwres",     "fwR",      "step_std",         "all",    NA,   NA,   NA,
  "migrT",      "migr",    "exponential_std",  "all",    3,    15,   NA,
  "migrQ",      "migr",       "decay_std",     "all",    3,     0,   0,
  # "migrA21",    "migr",     "exponential_std","all",     3,    0,    NA,
  "migrdist",   "migr",      "linear_std",     "all",    NA,   NA,   NA,
  "SSTproj",     "mar",     "exponential_std",   "all",    3,   8,   18,
  "SSTrate",     "mar",     "linear_std",      "all",    NA,   0.1,   0.3,
  "CImpact",     "mar",   "linear_std",        "all",    NA,   NA,   NA,
  "CUstatus",    "dem",       "cat_std",       "all",    NA,    NA,   NA,
  "CUnmat",      "dem",     "decay_std",       "all",    3,    0, 10000,
  "hetzyg",      "gen",     "invlinear_std",      "species",     NA,    NA, NA,
  "genoff",      "gen",     "linear_std",      "species",    NA,    NA, NA)


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
  "Nearshore Marine"   = "green4",
  "Genetics"  = "orange3"
)
