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
# library(ggsci)   # colour palettes - pal_futurama

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
  indicators = here("processed_data"),
  figures = here("output", "figures"),
  reports = here("reports"),
  code    = here("code")
)


# load utility functions
source(here("code", "2_fw_utils.R"))
source(here("code", "3_marine_utils.R"))
source(here("code", "4_scoring_utils.R"))

# load CU tables
source(here("code", "1a_CU_import.R"))   # CU table

# load plotting functions
source(here("code", "5a_plots_CU.R"))
source(here("code", "5b_plots_compare.R"))

# Select subset of CUs to run for analysis
cu_run <- cu_Fr %>%
  filter(SPECIES_NAME %in% c("Chinook", "Coho", "Sockeye", "Chum", "Pink"),   # optional species filter
    FULL_CU_IN %notin% c("SER-02")) %>% # remove widgeon (throws error)
  arrange(SPECIES_NAME)


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
  "favchange", "fwR",    "mean",     "linear_std",     "ENM Change in Favourability",
  "CT",         "fwR",    "mean",    "linear_std",        "Cumulative threats to freshwater habitat",
  "tw8rate",    "fwR",    "mean",    "linear_std",        "Rate of change in August Temperature",
  "tw8proj",    "fwR",    "mean",    "exponential_std", "Projected August Temperature",
  "lowQpdelta", "fwR",   "mean",    "decay_std",       "Proportional change in August flow (stream model)",
  "st8pdelta",  "fwR",    "mean",    "decay_std",     "Proportional change in August flow (station model)",
  "highQpdelta",  "fwR",   "mean",    "exponential_std", "Proportional change in Nov-Jan flow (stream model)",
  "fwres",     "fwR",    "value",    "step_std",   "Freshwater residency time",
  "migrT",      "migr",   "mean",    "exponential_std",     "Projected temperature during upstream migration",
  "migrQ",      "migr",   "pdelta",      "decay_std",    "Proportional change in discharge during upstream migration",
  "migrA21",    "migr",   "mean",    "exponential_std", "Average proportion of path above 21 degrees during upstream migration",
  "migrdist",   "migr",  "value",      "linear_std",     "Length of upstream migration",
  "SSTproj",     "mar",   "mean", "linear_std",  "Projected nearshore SST during ocean entry",
  "CI",          "mar",   "mean", "linear_std",   "Cumulative impacts to marine nearshore habitat",
  "CUstatus",    "dem",   "category",    "cat_std", "WSP status",
  "CUnmat",      "dem",   "value",  "decay_std", "Number of mature individuals")



tbl_standardize <- tribble(
  ~abbrev,      ~type,      ~std_fun,        ~lambda, ~xmin, ~xmax,
  "Favchange", "fwR",        "linear_std",        NA,     NA,   0,
  "ct",         "fwR",      "linear_std",        NA,    0,   NA,
  "Tw8rate",    "fwR",      "linear_std",         NA,    NA,   NA,
  "Tw8proj",    "fwR",       "exponential_std",   3,    15,   NA,
  "lowQpdelta",  "fwR",    "decay_std",           3,    NA,   0,
  "st8pdelta",  "fwR",    "decay_std",            3,    NA,   0,
  "highQpdelta", "fwR",    "exponential_std",     3,    0,   NA,
  "fwres",     "fwR",      "step_std",            NA,   NA,   NA,
  "migrT",      "migr",    "exponential_std",     3,    15,   NA,
  "migrQ",      "migr",       "decay_std",        3,   NA,   0,
  "migrA21",    "migr",     "exponential_std",    3,    0,    NA,
  "migrdist",   "migr",      "linear_std",        NA,   NA,   NA,
  "SSTproj",     "mar",   "linear_std",           NA,   NA,   NA,
  "CI",          "mar",   "linear_std",           NA,   NA,   NA,
  "CUstatus",    "dem",       "cat_std",        NA,    NA,   NA,
  "CUnmat",      "dem",     "decay_std",         3,    0, 10000)




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
