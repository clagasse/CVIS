# ==============================================================================
# CVIS Marine Data Summarization (3b_marine_summarize.R)
#
# Description:
#   Loads processed marine point files and aggregates them into monthly averages
#   over specific historical and projected periods (mid-century, end-of-century).
#   Reshapes and combines regional ocean model outputs (SSC & BCCM) into uniform datasets.
#
# Workflow Steps:
#   1. Load setup environment and configuration values.
#   2. Define period-averaging and dataset-reshaping functions.
#   3. Summarize CMIP6 climate models.
#   4. Summarize regional models (HOTSSEA, SalishSeaCast, BCCM) and cumulative impacts.
#   5. Combine SalishSeaCast (for GStr/SFj) and BCCM (for other zones) into unified matrices.
#   6. Save period-summarized RDS files to processed_data/marine/.
#
# Inputs:
#   - processed_data/marine/ points databases in Standardized_Marine_data/Points/
#
# Outputs:
#   - processed_data/marine/CMIP6_SST_periods.Rds, ROM_SST.Rds, ROM_SSS.Rds, etc.
#
# Dependencies:
#   - Requires 3a_marine_data_import.R to have been executed.
# ==============================================================================

library(here)
setwd(here())
source(file.path(here(), "code", "0_setup.R"))

# start year for historical period in HOTSSEA and CMIP6
h_start <- period_lookup$start_year[period_lookup$model == "CMIP6_SST" & period_lookup$period_code == 0]
h_end   <- period_lookup$end_year[period_lookup$model == "CMIP6_SST" & period_lookup$period_code == 0]
# start and end year of mid-century period
p3_start <- period_lookup$start_year[period_lookup$model == "CMIP6_SST" & period_lookup$period_code == 3]
p3_end <- period_lookup$end_year[period_lookup$model == "CMIP6_SST" & period_lookup$period_code == 3]
# start and end year of end-of-century period
p5_start <- period_lookup$start_year[period_lookup$model == "CMIP6_SST" & period_lookup$period_code == 5]
p5_end <- period_lookup$end_year[period_lookup$model == "CMIP6_SST" & period_lookup$period_code == 5]


# ==================== 1. Setup and Environmental Context ====================
# BCCM model can be pulled from two sources - Pacea and .shp file provided by Angelica Pena
#  Use .shp file in this script

# For Georgia Strait, use HotSsea as the historic SST climatology
# OISST from PacEA is an alternative source based on observations
# CMIP6 is the projected SST, with Salish Sea Cast as an alternative

# read OISST data from pacea
# oisst_month <- st_transform(oisst_month,crs = "EPSG:3005")



# ==================== 2. Helper Functions ====================

# function to take average SST across all years of a period
summarize_SST_timeseries <- function(CMIP_file_loc = file.path(paths$climate, "Standardized_Marine_data", "Points"),
                                     CMIP_file_prefix = "CMIP6_ssp",
                                     period_code,
                                     period_start,
                                     period_end,
                                     ssp = "245") {

  CMIP_file_name <- paste0(CMIP_file_prefix, ssp, "_SST_", period_start, "-", period_end, ".gdb")

  # for each scenario, get monthly SST values
  CMIP_SST <- st_read(file.path(
    CMIP_file_loc,
    CMIP_file_name
  ))

  # take average across years
  CMIP_SST_period <- CMIP_SST %>%
    group_by(MAZ_Acrony, SHAPE) %>%
    summarize(across(contains("SST"), ~ mean(.x, na.rm = T)),
      .groups = "drop") %>%
    mutate(period_code = period_code,
      rcp = str_sub(ssp, -2))

  return(CMIP_SST_period)

}


# function to reshape SSC and BCCM
reshape_ROM <- function(data,
                        variable = "SST") {
  output <- data %>%
    pivot_longer(
      cols = -c("SHAPE", "MAZ_Acrony"),
      names_to = c("rcp", "month"),
      names_pattern = paste0(variable, "_([A-Za-z0-9]+)_([0-9]+)"),
      values_to = "value"
    ) %>%
    filter(!is.na(month)) %>%
    pivot_wider(
      names_from = "month",
      values_from = "value",
      names_prefix = paste0(variable, "_")
    ) %>%
    mutate(period_code = case_when(
      rcp == "H" ~ 0,
      rcp == "45" ~ 3,
      rcp == "85" ~ 5))
}



# ==================== 3. Save Marine Adaptive Zones (MAZ) ====================

# save MAZ object to Rds file
save(MAZ, file = file.path(paths$marine, "MAZ.Rds"))


# ==================== 4. Summarize CMIP6 Projected SST ====================


CMIP_periods <- period_lookup[period_lookup$model == "CMIP6_SST", ]

for (i in 1:nrow(CMIP_periods))
{
  temp_45 <- summarize_SST_timeseries(period_code = CMIP_periods$period_code[i],
    period_start = CMIP_periods$start_year[i],
    period_end = CMIP_periods$end_year[i],
    ssp = "245")
  temp_85 <- summarize_SST_timeseries(period_code = CMIP_periods$period_code[i],
    period_start = CMIP_periods$start_year[i],
    period_end = CMIP_periods$end_year[i],
    ssp = "585")
  if (i == 1) {
    CMIP_45_SST <- temp_45
    CMIP_85_SST <- temp_85
  }
  if (i > 1) {
    CMIP_45_SST <- bind_rows(CMIP_45_SST, temp_45)
    CMIP_85_SST <- bind_rows(CMIP_85_SST, temp_85)
  }

}

CMIP6_SST <- CMIP_45_SST %>%
  bind_rows(CMIP_85_SST) %>%
  mutate(rcp = if_else(period_code == 0, "H", rcp))

# save object to processed data
save(CMIP6_SST, file = file.path(paths$marine, "CMIP6_SST_periods.Rds"))


# ==================== 5. Summarize HOTSSEA Historical SST ====================

# HOTSSea SST historical output (1980-2018)
hotssea_SST <- read_sf(file.path(
  paths$climate,
  "Standardized_Marine_data",
  "Points",
  "HOTSSea_SST.gdb"
))


hotssea_SST_period <- hotssea_SST %>%
  group_by(MAZ_Acrony, SHAPE) %>%
  summarize(across(contains("SST"), ~ mean(.x, na.rm = T)),
    .groups = "drop") %>%
  mutate(period_code = 0,
    RCP = "H")

save(hotssea_SST_period, file = file.path(paths$marine, "HOTSSEA_SST_periods.Rds"))



# ==================== 6. Summarize Cumulative Impacts Points ====================

CImpact_points <- read_sf(
  file.path(
    paths$climate,
    "Standardized_Marine_data",
    "Points",
    "CI_points_sub.gdb"
  )
)

save(CImpact_points, file = file.path(paths$marine, "CImpact_points.Rds"))




# ==================== 7. Summarize SalishSeaCast ROM SST and SSS ====================

# SST
SSC_SST_points <- st_read(file.path(
  paths$climate,
  "Standardized_Marine_data",
  "Points",
  "SSC_SST_sub.gdb"
))

SSC_SST <- reshape_ROM(SSC_SST_points,
  variable = "SST")

save(SSC_SST, file = file.path(paths$marine, "SSC_SST_periods.Rds"))


# repeat for Salinity

SSC_SSS_points <- st_read(file.path(
  paths$climate,
  "Standardized_Marine_data",
  "Points",
  "SSC_SSS_sub.gdb"
))

SSC_SSS <- reshape_ROM(SSC_SSS_points,
  variable = "SSS")

save(SSC_SSS, file = file.path(paths$marine, "SSC_SSS_periods.Rds"))



# ==================== 8. Summarize BCCM ROM SST, SSS, and SSPH ====================

BCCM_SST_points  <- read_sf(
  file.path(
    paths$climate,
    "Standardized_Marine_data",
    "Points",
    "BCCM_SST_sub.gdb"
  )
)

BCCM_SST <- reshape_ROM(BCCM_SST_points,
  variable = "SST")

save(BCCM_SST, file = file.path(paths$marine, "BCCM_SST_periods.Rds"))

# BCCM salinity
BCCM_SSS_points  <- read_sf(
  file.path(
    paths$climate,
    "Standardized_Marine_data",
    "Points",
    "BCCM_SSS_sub.gdb"
  )
)

BCCM_SSS <- reshape_ROM(BCCM_SSS_points,
  variable = "SSS")

save(BCCM_SSS, file = file.path(paths$marine, "BCCM_SSS_periods.Rds"))


# BCCM PH
BCCM_SSPH_points <- read_sf(
  file.path(
    paths$climate,
    "Standardized_Marine_data",
    "Points",
    "BCCM_SSPH_sub.gdb"
  )
)

BCCM_SSPH <- reshape_ROM(BCCM_SSPH_points,
  variable = "SSPH")

save(BCCM_SSPH, file = file.path(paths$marine, "BCCM_SSPH_periods.Rds"))



# ==================== 9. Combine ROM Datasets (SSC & BCCM) ====================

# use Salish Sea Cast for GStr and SFj. Use BCCM for other MAZs

ROM_SST <- SSC_SST %>%
  filter(MAZ_Acrony %in% c("GStr", "SFj")) %>%
  bind_rows(filter(BCCM_SST, MAZ_Acrony %notin% "GStr"))

save(ROM_SST, file = file.path(paths$marine, "ROM_SST.Rds"))


ROM_SSS <- SSC_SSS %>%
  filter(MAZ_Acrony %in% c("GStr", "SFj")) %>%
  bind_rows(filter(BCCM_SSS, MAZ_Acrony %notin% "GStr"))

save(ROM_SSS, file = file.path(paths$marine, "ROM_SSS.Rds"))

