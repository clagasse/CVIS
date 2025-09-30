####  3c_marine_stats
#
# Import processed marine data from 3a_marine_data_import
# and calculate summary statistics for each CU
# Point data are used to preserve native resolutions of data sets
# For plotting, standardized gridded outputs are used from 3b.


##########


library(here)
setwd(here())
source(file.path(here(), "code", "0_setup.R"))

# install_github("pbs-assess/pacea")
library(pacea)

qlowsp <- 0.1   # lowest quantile for spatial variation in indicator
qhighsp <- 0.9  # highest quantile for spatial variation in indicator

period_val <- 3   # code to assign indicator results timespan (3 = 2040-2060)

decades <- (2055 - 1995) / 10    # decades between historic and projected period

CI_habitats <- c("All")   # habitat types to include for cumulative impacts

#---------1. Import----------------------------------

# BCCM model can be pulled from two sources - Pacea and .shp file provided by Angelica Pena
#  Use .shp file in this script

# For Georgia Strait, use HotSsea as the historic SST climatology
# OISST from PacEA is an alternative source based on observations
#  Salish Sea Cast is the projected SST

# read OISST data from pacea
# oisst_month <- st_transform(oisst_month,crs = "EPSG:3005")

## read in marine adaptive zones shapefile
MAZ     <- st_read(file.path(paths$spatial, "MAZ", "MAZ_Final.shp"))

#  note: _cropped are polygon grids,  _sub are points
SSC_SSS <- st_read(file.path(
  paths$climate,
  "Standardized_Marine_data",
  "Points",
  "SSC_SSS_sub.gdb"
))
SSC_SST <- st_read(file.path(
  paths$climate,
  "Standardized_Marine_data",
  "Points",
  "SSC_SST_sub.gdb"
))

BCCM_SST  <- read_sf(
  file.path(
    paths$climate,
    "Standardized_Marine_data",
    "Points",
    "BCCM_SST_sub.gdb"
  )
)
# BCCM_SST_MAZ <- assign_points(BCCM_SST_sub, MAZ, var = "MAZ_Acrony")
BCCM_SSS  <- read_sf(
  file.path(
    paths$climate,
    "Standardized_Marine_data",
    "Points",
    "BCCM_SSS_sub.gdb"
  )
)
BCCM_SSPH <- read_sf(
  file.path(
    paths$climate,
    "Standardized_Marine_data",
    "Points",
    "BCCM_SSPH_sub.gdb"
  )
)


# To use Pacea SSTs, uncomment the next line:
# BCCM_paceaSST_sub <- bccm_surface_temperature()

# NEP36
# NEP_SST<- read_sf( file.path(paths$climate,"Standardized_Marine_data", "Points" ,"NEP_SST_sub.gdb"))
# NEP_SSS<- read_sf( file.path(paths$climate,"Standardized_Marine_data", "Points" ,"NEP_SSS_sub.gdb"))
# NEP_SSPH<- read_sf( file.path(paths$climate, "Standardized_Marine_data", "Points" ,"NEP_SSPH_sub.gdb"))

# Salish Sea Cast
SSC_SST <- read_sf(file.path(
  paths$climate,
  "Standardized_Marine_data",
  "Points",
  "SSC_SST_sub.gdb"
))
SSC_SSS <- read_sf(file.path(
  paths$climate,
  "Standardized_Marine_data",
  "Points",
  "SSC_SSS_sub.gdb"
))

# HOTSSea SST historical output (1980-2018)
hotssea_SST <- read_sf(file.path(
  paths$climate,
  "Standardized_Marine_data",
  "Points",
  "HOTSSea_SST.gdb"
))

# Cumulative Impacts
CI_points <- read_sf(
  file.path(
    paths$climate,
    "Standardized_Marine_data",
    "Points",
    "CI_points_sub.gdb"
  )
)

# get cu timing for ocean entry
cu_marine <- cu_timing_Fr %>%
  select(FULL_CU_IN, oe_age, oe_dat_qual, oe_start, oe_peak, oe_end, n_oe) %>%
  left_join(
    select(
      cu_list,
      FULL_CU_IN,
      CU_NAME,
      Species_simple,
      MAZ
    ),
    join_by("FULL_CU_IN")
  ) %>%
  relocate(CU_NAME, Species_simple, MAZ, .after = FULL_CU_IN) %>%
  mutate(
    oe_start_month = month(ymd(paste(
      "2000", "01", "01", sep = "-"
    )) + oe_start - 1),
    oe_end_month = month(ymd(paste(
      "2000", "01", "01", sep = "-"
    )) + oe_end - 1)
  ) %>%
  mutate(ns_timing_start = oe_start_month - 1, ns_timing_end = oe_end_month + 1)


#------------- 2. Reshaping and summarizing ----------------------

# BCCMpacea_SST <- st_join(BCCMpacea_SST, left = FALSE, MAZ["MAZ_Acrony"])

BCCM_SST_long <- BCCM_SST %>%
  pivot_longer(
    cols = -c("SHAPE", "MAZ_Acrony"),
    names_to = c("RCP", "month"),
    names_pattern = "SST_([A-Za-z0-9]+)_([0-9]+)",
    values_to = "value"
  ) %>%
  mutate(model = "BCCM", month = as.numeric(month))

SSC_SST_long <- SSC_SST %>%
  pivot_longer(
    cols = -c("SHAPE", "MAZ_Acrony"),
    names_to = c("RCP", "month"),
    names_pattern = "SST_([A-Za-z0-9]+)_([0-9]+)",
    values_to = "value"
  ) %>%
  mutate(model = "SSC", month = as.numeric(month))

BCCM_SSS_long <- BCCM_SSS %>%
  pivot_longer(
    cols = -c("SHAPE", "MAZ_Acrony"),
    names_to = c("RCP", "month"),
    names_pattern = "SSS_([A-Za-z0-9]+)_([0-9]+)",
    values_to = "value"
  ) %>%
  mutate(model = "BCCM", month = as.numeric(month))

SSC_SSS_long <- SSC_SSS %>%
  pivot_longer(
    cols = -c("SHAPE", "MAZ_Acrony"),
    names_to = c("RCP", "month"),
    names_pattern = "SSS_([A-Za-z0-9]+)_([0-9]+)",
    values_to = "value"
  ) %>%
  mutate(model = "SSC", month = as.numeric(month))

BCCM_SSPH_long <- BCCM_SSPH %>%
  pivot_longer(
    cols = -c("SHAPE", "MAZ_Acrony"),
    names_to = c("RCP", "month"),
    names_pattern = "SSPH_([A-Za-z0-9]+)_([0-9]+)",
    values_to = "value"
  ) %>%
  mutate(model = "BCCM", month = as.numeric(month))

CI_long <- CI_points %>%
  pivot_longer(
    cols = c(contains("Cumul_Im")),
    names_to = c("habitat"),
    values_to = "value"
  ) %>%
  mutate(
    habitat = case_when(
      habitat == "Cumul_Impact_ALL" ~ "All",
      habitat == "Cumul_Impact_sp" ~ "Shallow_pelagic",
      habitat == "Cumul_Impact_dp" ~ "Deep_pelagic",
      habitat == "Cumul_Impact_bh" ~ "Benthic",
      habitat == "Cumul_Impact_eg" ~ "Eelgrass",
      habitat == "Cumul_Impact_sr" ~ "Sponge_reef",
      habitat == "Cumul_Impact_kp" ~ "Kelp_forest",
    )
  )


# get hotssea historic average temperature

ROM_SST <- BCCM_SST_long %>%
  filter(MAZ_Acrony != "GStr") %>%
  bind_rows(filter(hotssea_SST, MAZ_Acrony == "GStr")) %>%
  bind_rows(filter(SSC_SST_long, RCP != "H" &
    MAZ_Acrony == "GStr"))

ROM_SSS <- BCCM_SSS_long %>%
  filter(MAZ_Acrony != "GStr") %>%
  bind_rows(filter(SSC_SST_long, MAZ_Acrony == "GStr"))



#-----------3. Get indicators by CU ----------------------


for (i in 1:n.CUs) {
  cu_i <- cu_run$FULL_CU_IN[i]

  cu_marine_i <- cu_marine[cu_marine$FULL_CU_IN == cu_i, ]

  months_include <- seq(
    from = cu_marine_i$ns_timing_start,
    to = cu_marine_i$ns_timing_end,
    by = 1
  )

  # match MAZ and take mean, qlow and qhigh of included months for historic and each RCP

  SST_mar_i <- summarize_marine_var(ROM_SST,
    months_include,
    MAZ_pick = cu_marine_i$MAZ,
    var_pick = "SST",
    decades)

  SSS_mar_i <- summarize_marine_var(ROM_SSS,
    months_include,
    MAZ_pick = cu_marine_i$MAZ,
    var_pick = "SSS",
    decades)

  SS_mar_i <- left_join(SST_mar_i, SSS_mar_i, by = "RCP") %>%
    mutate(period_code = case_when(
      RCP %in% c("45", "85") ~ period_val,
      RCP == "H" ~ 0))

  CI_mar_i <- CI_long %>%
    as_tibble() %>%
    filter(MAZ_Acrony == cu_marine_i$MAZ, habitat == "All") %>%
    summarize(
      CI_mean = mean(value, na.rm = T),
      CI_qlowsp = quantile(value, qlowsp, na.rm = T),
      CI_qhighsp = quantile(value, qhighsp, na.rm = T)
    )


  if (i == 1) {
    # cu_marine_all <- cu_marine_i
    # # SST_mar <- SST_mar_i
    # # SSS_mar <- SSS_mar_i
    # # CI_mar    <- CI_mar_i
    # #
    mar_all_flat <- bind_cols(cu_marine_i, SS_mar_i,  CI_mar_i)
  }
  if (i > 1) {
    temp <- bind_cols(cu_marine_i, SS_mar_i, CI_mar_i)

    mar_all_flat <- bind_rows(mar_all_flat, temp)

    # cu_marine_all <- bind_rows(cu_marine_all, cu_marine_i)
    # SST_mar <- bind_rows(SST_mar, SST_mar_i)
    # SSS_mar <- bind_rows(SSS_mar, SSS_mar_i)
    # CI_mar <- bind_rows(CI_mar, CI_mar_i)
  }

}


save(mar_all_flat,
  file = file.path(paths$marine, paste0(today, "_marine_stats.Rds")))

# write.csv(mar_all_flat,
#   file = file.path(paths$marine, paste0(today, "_marine_stats.csv")),
#   row.names = FALSE)
