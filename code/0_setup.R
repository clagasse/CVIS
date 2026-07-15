# ==============================================================================
# CVIS Environment Setup & Configuration (0_setup.R)
#
# Description:
#   Sets up the global environment for the Climate Vulnerability Indicators for Salmon
#   (CVIS). Configures folder paths, sets analysis options (CUs to run, models,
#   GCM quantiles), defines indicator translation/standardization lookup tables,
#   establishes the custom CVIS ggplot theme, and sources utility and plotting scripts.
#
# Workflow Steps:
#   1. Load required spatial, plotting, and data packages.
#   2. Set directory paths and analysis parameters.
#   3. Define indicators, scaling ranges, and GCM models.
#   4. Define CVIS ggplot theme and color palettes.
#   5. Source all helper scripts and CU import databases.
#
# Inputs:
#   - None
#
# Outputs:
#   - Configured global variables and helper functions in R environment
#
# Dependencies:
#   - Executed as the first step in all CVIS scripts.
# ==============================================================================

# ==================== 1. Setup and Libraries ====================

library(here)

# Configure Pandoc path if not found (needed for rendering Rmd files on Windows)
if (Sys.getenv("RSTUDIO_PANDOC") == "") {
  # Try standard system search first
  system_pandoc <- Sys.which("pandoc")
  if (system_pandoc != "") {
    Sys.setenv(RSTUDIO_PANDOC = dirname(system_pandoc))
  } else {
    # Try common local RStudio Quarto/Pandoc installation paths as fallbacks
    fallbacks <- c(
      "C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools",
      "C:/Program Files/RStudio/bin/quarto/bin/tools",
      "C:/Program Files/RStudio/resources/app/bin/pandoc",
      "C:/Program Files/RStudio/bin/pandoc"
    )
    for (path in fallbacks) {
      if (dir.exists(path) || file.exists(path)) {
        Sys.setenv(RSTUDIO_PANDOC = path)
        break
      }
    }
  }
}

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
library(ggdist) # for half-violin / raincloud plots
library(ggrepel) # for label positioning in plots

formals(read_csv)$show_col_types <- F # use read_csv quietly

`%notin%` <- Negate(`%in%`) # opposite of %in%

# ==================== 2. Directory Setup ====================
# setwd("C:/Users/LAGASSEC/OneDrive - DFO-MPO/0.Workspace/CVIS")

today <- Sys.Date()

# Set file paths
# Default PCIC NetCDF folder root (checks for local D:/ drive first, falls back to sibling data dir)
pcic_data_root <- if (dir.exists("D:/PCIC/fraser")) {
  "D:/PCIC/fraser"
} else {
  file.path(here(".."), "0_data_climate", "PCIC")
}

paths <- list(
  climate = file.path(here(".."), "0_data_climate"),
  spatial = file.path(here(".."), "0_data_spatial"),
  salmon  = file.path(here(".."), "0_data_salmon"),
  pcic    = pcic_data_root,
  fw      = here("processed_data", "freshwater"),
  marine  = here("processed_data", "marine"),
  output  = here("output"),
  CU      = here("processed_data", "CU"),
  figures = here("output", "figures"),
  reports = here("output", "reports"),
  code    = here("code"),
  params  = here("processed_data", "params")
)



# ==================== 3. Analysis Configurations ====================

# CUs to subset
DFO_area_include <- c("FRASER AND INTERIOR")
CU_exclude <- c(
  "CK-01", # exclude okanagan Chinook
  "CK-02", # exclude boundary bay Chinook
  "SEL-01-01", # okanagan sockeye
  "SER-02" # Widgeon  (throws errors)
)
species_include <- c("Chinook", "Coho", "Sockeye", "Chum", "Pink")
CU_type_include <- c("Current") # only include Current CUs, exclude extirpated ones

CI_type <- c("ALL") # habitat types to use for cumulative impacts (marine)
# options include:  ALL, dp, bh, eg, sr, kp
cthr_pick <- "cthr_anad" # cumulative threat type to use for indicator - default anadromous (i.e. sum of all)

periods_use <- c(0, 3, 5) # period codes to keep (see period_lookup for corresponding years)

# default models to use when combining standardized scores
dsmodel_baseline <- c("observed", "ENM", "streamdyn", "tscapes", "pcicgrid", "qdm", "CImpact", cthr_pick)
# excluded station, bccmssc

common_gcms <- c("1", "4", "6") # gcms used by PCIC and tscapes models, for sensitivity analysis

# lower and upper quantiles for spatial variation statistics
qlsp <- 0.1
qhsp <- 0.9

qlgcm <- 0.1 # lower quantile for statistics on GCM variation
qhgcm <- 0.9 # upper quantile for statistics

T_model <- "tw8" # temperature model tw8 = thermalscapes August temp, alternative of 7DEC (not implemented)

fw_habitat_selection <- switch(1,
  "rs",
  "all"
) # choose how to subset stream network for spawning and rearing indicators
# rs = use BCfishpass rearing and spawning habitat (from accessible habitat)
# all = use all streams 

historical_code <- "0" # historical climatology period for temperature models
# 0 = 1981-2000,  1 = 2001-2020.  For flow 0 = 1981-2010

# choose stream base network
base_network <- switch(1,
  "tscapes",
  "bcfpa"
) # only tscapes currently implemented, bcfpa has FWA network broken into smaller segments

# vector of RCP codes
rcp_vec <- c("0", "00", "45", "85")


#the minimum inclusive stream order to use for subsetting of migration paths
# that represent the mainstem (mostly for plotting purposes)
mainstem_min_order <- 9

# for converting CU status categories to a numeric
status_map <- c(Green = 1, Amber = 2, Red = 3)

# choose method for determining nearshore residency period for marine indicators
# static = same months used for all CUs,  peak_offset = offsets from peak ocean entry month used
ns_time_method <- switch(1,
  "static",
  "peak_offset"
)

ns_start_static <- 4 # for static method, start month
ns_end_static <- 7 # for static method, end month included
ns_start_offset <- 2 # amount of months before peak ocean entry month to include when calculating nearshore marine indicators
ns_end_offset <- 2 # amount of months after peak ocean entry month for calculating nearshore marine indicators

min_gen_red <- 1000 # if generational avg spawners is below this value and RapidStatus is None, status will be adjusted to Red


# ==================== 3.5 Scoring and Standardization Configuration ====================
# Configuration settings for indicator standardization and portfolio scoring (used in 4a_CU_scoring.R)

# Baseline settings for indicator scaling
# If you want to standardize scores and ranges relative to a specific baseline setup,
# set these variables (e.g., scale_baseline_rcp <- "45"). If NA, ranges will be
# calculated dynamically for each scenario/group.
scale_baseline_rcp <- NA # e.g. "45"
scale_baseline_period <- NA # e.g. "3"

# Which variables are used when grouping CU indicator results for standardization?
# This determines the min-max range applied when standardizing from 0 to 1.
# The default is to group separately across all GCMs, RCPs, periods, and downscalers.
grouping_vars_pick <- c("gcm", "rcp", "period_code", "dsmodel")

# Threshold for station coverage overlap (exclude CUs below this for flow8pdelta)
min_station_coverage <- 0.1


# ==================== 4. Sensitivity Analysis Configuration ====================

# Baseline scenario for sensitivity comparisons
sens_rcp_base <- "45"
sens_period_base <- "3"
sens_gcm_base <- "9"

# Default scoring methods for baseline
sens_method_overall_base <- "catavg"
sens_method_category_base <- "avg"
std_method_base <- "mix" # Baseline standardization curves choice ("mix", "exponential", or "linear")

# Variation sources for granular analysis (9 total)
sens_gcms <- c("1", "4", "6")
sens_scenarios <- list(c("45", "5"), c("85", "3"), c("85", "5"))
sens_methods <- c("cube", "flag", "avgcube")

# Mapping of life stage category codes to descriptive names
cat_label_map <- c(
  "fwrs" = "Spawning & Rearing",
  "migr" = "Upstream Migration",
  "mar"  = "Nearshore Marine",
  "dem"  = "Demographics",
  "gen"  = "Genetics"
)


# ==================== 5. Lookup & Definition Tables ====================

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

tbl_indicators <- tribble(
  ~abbrev, ~category, ~long_type, ~std_fun, ~unit, ~unit_short, ~name,
  "favchange", "fwrs", "Freshwater", "invlinear_std", "Favourability", "Fav. Δ", "Change in ENM favourability",
  "cthr", "fwrs", "Freshwater", "linear_std", "Threat score", "Threat", "Standardized cumulative threats",
  "tw8rate", "fwrs", "Freshwater", "linear_std", "Temperature change per decade (°C)", "°C/dec", "Rate of change in August Temperature (°C/decade)",
  "tw8proj", "fwrs", "Freshwater", "exponential_std", "Temperature (°C)", "°C", "Projected August Temperature (°C)",
  "flow8pdelta", "fwrs", "Freshwater", "decay_std", "Proportion change from baseline", "Prop. Δ", "Proportional change in August flow",
  "flow18pdelta", "fwrs", "Freshwater", "exponential_std", "Proportion change from baseline", "Prop. Δ", "Proportional change in Nov-Jan flow",
  "fwres", "fwrs", "Freshwater", "step_std", "Number of days", "Days", "Freshwater residency time (days)",
  "migrTproj", "migr", "Upstream Migration", "exponential_std", "Temperature (°C)", "°C", "Projected temperature during upstream migration (°C)",
  "migrQpdelta", "migr", "Upstream Migration", "decay_std", "Proportion change", "Prop. Δ", "Proportional change in discharge",
  "migrdist", "migr", "Upstream Migration", "linear_std", "Metres", "m", "Length of upstream migration (km)",
  "SSTproj", "mar", "Nearshore Marine", "exponential_std", "Temperature (°C)", "°C", "Projected nearshore SST (°C)",
  "SSTrate", "mar", "Nearshore Marine", "linear_std", "Temperature change per decade (°C)", "°C/dec", "Rate of change in nearshore SST (°C/decade)",
  "CImpact", "mar", "Nearshore Marine", "linear_std", "Threat score", "Threat", "Cumulative impacts to habitat",
  "CUstatus", "dem", "Demographics", "cat_std", "Status", "Status", "Wild Salmon Policy CU status",
  "CUnmat", "dem", "Demographics", "decay_std", "Number of spawners", "log10(N)", "Number of mature individuals (spawners)",
  "hetzyg", "gen", "Genetics", "invlinear_std", "Heterozygosity", "Hz", "Genetic heterozygosity",
  "genoff", "gen", "Genetics", "linear_std", "Genomic offset", "Offset", "Genomic offset"
)


tbl_ind_report <- tbl_indicators %>%
  select(abbrev, long_type, name) %>%
  rename(
    Abbreviation = abbrev,
    Category = long_type,
    Description = name
  )



tbl_standardize <- tribble(
  ~abbrev, ~category, ~std_fun, ~range_type, ~lambda, ~xmin, ~xmax, ~dsmodel_baseline,
  "favchange", "fwrs", "invlinear_std", "all", NA, NA, 0, "ENM",
  "cthr", "fwrs", "linear_std", "all", NA, 0, NA, "cthr_anad",
  "tw8rate", "fwrs", "linear_std", "all", NA, NA, NA, "tscapes",
  "tw8proj", "fwrs", "exponential_std", "all", 3, 15, NA, "tscapes",
  "flow8pdelta", "fwrs", "decay_std", "all", 3, NA, 0, "streamdyn",
  "flow18pdelta", "fwrs", "exponential_std", "all", 3, 0, NA, "streamdyn",
  "fwres", "fwrs", "step_std", "all", NA, NA, NA, "observed",
  "migrTproj", "migr", "exponential_std", "all", 3, 15, NA, "pcicgrid",
  "migrQpdelta", "migr", "decay_std", "all", 3, 0, 0, "pcicgrid",
  "migrdist", "migr", "linear_std", "all", NA, NA, NA, "observed",
  "SSTproj", "mar", "exponential_std", "all", 3, NA, NA, "qdm",
  "SSTrate", "mar", "linear_std", "all", NA, NA, NA, "qdm",
  "CImpact", "mar", "linear_std", "all", NA, NA, NA, "CImpact",
  "CUstatus", "dem", "cat_std", "all", NA, NA, NA, "observed",
  "CUnmat", "dem", "decay_std", "all", 3, 0, 10000, "observed",
  "hetzyg", "gen", "invlinear_std", "species", NA, NA, NA, "observed",
  "genoff", "gen", "linear_std", "species", NA, NA, NA, "observed"
)


# mapping of field codes to gcm names for thermalscapes model
gcm_codes <- tribble(
  ~gcm, ~gcm_name,
  "0",  "historical",
  "1",  "canesm2", # PCIC + tscapes
  "2",  "csiro", # tscapes
  "3",  "gfdl", # tscapes
  "4",  "hadgem2", # PCIC + tscapes
  "5",  "miroc", # tscapes
  "6",  "mpi", # PCIC + tscapes
  "7",  "access1", # PCIC model
  "8",  "cnrm", # PCIC model
  "9",  "ensemble",
  "20", "ccsm4" # PCIC model
)


save(tbl_indicators, tbl_standardize, tbl_ind_report, file = file.path(paths$params, "indicator_tables.Rdata"))

# ==================== 6. ggplot Custom Theme ====================

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
      strip.background = element_blank(),
      strip.text = element_text(size = rel(0.55), face = "bold", color = "black", margin = margin(2, 0, 2, 0))
    )
}
theme_set(theme_cvis())



# ==================== 7. Color Palettes ====================

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

# Color palette for the uncertainty sources
# GCMs: Reds/Oranges, Scenarios: Greens/Blues, Methods: Purples/Browns, Models: Pinks/Golds
sens_source_palette <- c(
  "GCM1" = "#e31a1c", "GCM4" = "#ff7f00", "GCM6" = "#fdbf6f", "GCM" = "#e31a1c", "gcm" = "#e31a1c",
  "RCP45_P5" = "#33a02c", "RCP85_P3" = "#1f78b4", "RCP85_P5" = "#a6cee3", "Scenario" = "#1f78b4", "scenario" = "#1f78b4",
  "cube" = "#6a3d9a", "flag" = "#b15928", "thr-exceed" = "#b15928", "avgcube" = "#cab2d6",
  "qdm" = "#db7093", "bccmssc" = "#daa520", "streamdyn" = "#4682b4", "tscapes" = "#d2b48c",
  "pcicgrid" = "#e7298a", "station" = "#7570b3", "model" = "#8dd3c7", "dsmethod" = "#8dd3c7",
  "stdmethod" = "#8c564b", "StdMethod" = "#8c564b"
)


# Universal risk score color palette for 0 to 1 risk (Vulnerability)
cvis_risk_palette <- "Zissou1"
cvis_risk_direction <- -1

# Switch call to allow selection of palette options in the setup file
cvis_risk_palette_colors <- switch(cvis_risk_palette,
  "Zissou1" = c("#3060AF", "#78A7F5", "#EBCC5A", "#E1AF00", "#C21A1D"),
  "RdYlBu"  = RColorBrewer::brewer.pal(11, "RdYlBu"),
  "roma"    = scico::scico(100, palette = "roma"),
  # Default fallback
  c("#3060AF", "#78A7F5", "#EBCC5A", "#E1AF00", "#C21A1D")
  #c("#3B9AB2", "#78A7C5", "#EBCC5A", "#E1AF00", "#C21A1D")
)

# Helper scale functions to support Zissou1, roma and distiller palettes
scale_fill_cvis <- function(palette = cvis_risk_palette, direction = 1, limits = NULL, na.value = "grey95", oob = scales::censor, ...) {
  cols <- switch(palette,
    "Zissou1" = c("#3060AF", "#78A7F5", "#EBCC5A", "#E1AF00", "#C21A1D"),
    "RdYlBu"  = RColorBrewer::brewer.pal(11, "RdYlBu"),
    "roma"    = scico::scico(100, palette = "roma"),
    "lajolla" = scico::scico(100, palette = "lajolla"),
    "berlin"  = scico::scico(100, palette = "berlin"),
    "batlow"  = scico::scico(100, palette = "batlow"),
    NULL
  )
  
  if (!is.null(cols)) {
    # Adjust cols based on direction:
    # We want Red/dark/last colors to represent high risk (at the high end of the gradient).
    # Since Zissou1, roma, lajolla, berlin, and batlow default to this Low-to-High risk sequence:
    # - direction = -1 (requesting high risk = Red/last) should keep it as-is.
    # - direction = 1 (requesting high risk = Blue/first) should reverse it.
    # Since RdYlBu defaults to Red-to-Blue (High risk = first):
    # - direction = -1 (requesting high risk = Red) should reverse it to Blue-to-Red.
    # - direction = 1 should keep it Red-to-Blue.
    if (palette == "RdYlBu") {
      if (direction == -1) {
        cols <- rev(cols)
      }
    } else {
      if (direction == 1) {
        cols <- rev(cols)
      }
    }
    scale_fill_gradientn(colors = cols, limits = limits, na.value = na.value, oob = oob, ...)
  } else {
    scale_fill_distiller(palette = palette, direction = direction, limits = limits, na.value = na.value, oob = oob, ...)
  }
}

scale_color_cvis <- function(palette = cvis_risk_palette, direction = 1, limits = NULL, na.value = "grey95", oob = scales::censor, ...) {
  cols <- switch(palette,
    "Zissou1" = c("#3060AF", "#78A7F5", "#EBCC5A", "#E1AF00", "#C21A1D"),
    "RdYlBu"  = RColorBrewer::brewer.pal(11, "RdYlBu"),
    "roma"    = scico::scico(100, palette = "roma"),
    "lajolla" = scico::scico(100, palette = "lajolla"),
    "berlin"  = scico::scico(100, palette = "berlin"),
    "batlow"  = scico::scico(100, palette = "batlow"),
    NULL
  )
  
  if (!is.null(cols)) {
    if (palette == "RdYlBu") {
      if (direction == -1) {
        cols <- rev(cols)
      }
    } else {
      if (direction == 1) {
        cols <- rev(cols)
      }
    }
    scale_color_gradientn(colors = cols, limits = limits, na.value = na.value, oob = oob, ...)
  } else {
    scale_color_distiller(palette = palette, direction = direction, limits = limits, na.value = na.value, oob = oob, ...)
  }
}


# Indicator palette used for labelling indicator categories
indicator_palette <- c(
  "Demographics" = "#9E6B7A",
  "Spawning & Rearing" = "#8AA382",
  "Upstream Migration" = "#7D8CA3",
  "Nearshore Marine" = "#698B93",
  "Genetics" = "#D9946C"
)



# ==================== 8. Load Sourced Scripts & Data ====================

# load utility functions
source(here("code", "2_fw_utils.R"))
source(here("code", "3_marine_utils.R"))
source(here("code", "4_scoring_utils.R"))

# load plotting functions and table summaries
source(here("code", "5a_plots_CU.R"))
source(here("code", "5b_plots_compare.R"))
source(here("code", "5c_plots_sensitivity_indicators.R"))
source(here("code", "5d_table_summaries.R"))
source(here("code", "5e_plots_study_area.R"))

# load CU tables
source(here("code", "1a_CU_import.R")) # CU table
source(here("code", "1f_genetics_import.R"))

# load CU boundaries and add alternative species name columns
load(file.path(paths$fw, "cu_boundary.Rds")) 
cu_boundary <- cu_boundary %>% 
  left_join(select(spp_lookup, PSF_species, SPECIES_NAME), join_by(Species == PSF_species))

# Load watershed basins and filter to Fraser River basin for spatial maps
load(file.path(paths$fw, "basins_shp.Rds"))
Fr_basin <- filter(basins, BASIN == "FRASER")

# Load Fraser basin lakes for plotting
load(file.path(paths$fw, "BC_FWA_LAKES_FR.Rds"))

# Load BC coastline polygon from pacea package and save to processed_data/marine if it doesn't exist already
bc_coast_path <- file.path(paths$marine, "bc_coast.Rds")
if (!file.exists(bc_coast_path)) {
  library(pacea)
  if (exists("bc_coast", envir = asNamespace("pacea"))) {
    bc_coast_obj <- get("bc_coast", envir = asNamespace("pacea"))
  } else {
    data("bc_coast", package = "pacea", envir = environment())
    bc_coast_obj <- bc_coast
  }
  saveRDS(bc_coast_obj, file = bc_coast_path)
}
bc_coast <- readRDS(bc_coast_path)



# ==================== 9. Dynamic SMU Palette Definition ====================
# Created following 1a_CU_import.R to ensure cu_run is loaded
if (exists("cu_run")) {
  smu_colors <- character()
  for (sp in unique(cu_run$SPECIES_NAME)) {
    base_col <- species_palette[as.character(sp)]
    smus <- unique(cu_run$SMU_SIMPLE[cu_run$SPECIES_NAME == sp])
    # Handle possible NAs
    smus <- smus[!is.na(smus)]
    n_smus <- length(smus)

    if (n_smus == 1) {
      cols <- base_col
      names(cols) <- smus
      smu_colors <- c(smu_colors, cols)
    } else if (n_smus > 1) {
      base_rgb <- col2rgb(base_col)
      base_hsv <- rgb2hsv(base_rgb)
      h <- base_hsv[1, 1]
      s <- base_hsv[2, 1]
      v <- base_hsv[3, 1]
      
      cols <- character(n_smus)
      for (i in 1:n_smus) {
        # Spread hue slightly around the base hue
        hue_offset <- ((i - 1) / (n_smus - 1) - 0.5) * 0.12
        new_h <- (h + hue_offset) %% 1
        
        # Spread saturation slightly
        sat_offset <- ((i - 1) / (n_smus - 1) - 0.5) * 0.20
        new_s <- max(0.4, min(1, s - sat_offset))
        
        # Spread value/lightness
        val_offset <- ((i - 1) / (n_smus - 1) - 0.5) * 0.40
        new_v <- max(0.3, min(1, v - val_offset))
        
        cols[i] <- hsv(new_h, new_s, new_v)
      }
      names(cols) <- smus
      smu_colors <- c(smu_colors, cols)
    }
  }
  smu_palette <- smu_colors
} else {
  smu_palette <- character() # Fallback
}


# ==================== 10. Report Optimization Configurations ====================
# Geometry simplification tolerance (in meters for projected layers like BC Albers EPSG:3005).
# Set to 15 meters to reduce vertex count in stream networks and lake boundaries.
# Set to 0, NA, or NULL to completely disable geometry simplification.
geom_simplify_tol <- 15

