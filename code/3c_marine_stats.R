####  3c_marine_stats
#
# Import marine data summarized over time periods from 3b_marine_summarize
# and calculate summary statistics for each CU
# Point data are used to preserve native resolutions of data sets

# Inputs: CMIP6 SST, HOTSSEA SST, SSC SST, BCCM SST data sets
#  cu_timing - ocean entry
# Optional inputs:  SSC Salinity, BCCM salinity, pH

##########
library(here)
setwd(here())
source(file.path(here(), "code", "0_setup.R"))

# To use Pacea SSTs, uncomment the next line:
# BCCM_paceaSST_sub <- bccm_surface_temperature()

# get cu timing for ocean entry
cu_marine <- cu_timing_Fr %>%
  select(FULL_CU_IN, CVIS_NAME, SPECIES_NAME, oe_age, oe_dat_qual, oe_start, oe_peak, oe_end, n_oe) %>%
  mutate(
    oe_peak_month = month(ymd(paste(
      "2000", "01", "01", sep = "-"
    )) + oe_peak - 1),
    oe_start_month = month(ymd(paste(
      "2000", "01", "01", sep = "-"
    )) + oe_start - 1),
    oe_end_month = month(ymd(paste(
      "2000", "01", "01", sep = "-"
    )) + oe_end - 1)
  ) %>%
  mutate(ns_timing_start = ns_timing_start(oe_peak_month, ns_start_offset, ns_time_method),
    ns_timing_end   = ns_timing_end(oe_peak_month, ns_end_offset, ns_time_method)) %>%
  mutate(MAZ = "GStr") %>%
  select(-any_of(c("oe_age", "oe_peak")))  # remove these columns since they are already in fwR data frame

# load marine model files
load(file = file.path(paths$marine, "CMIP6_SST_periods.Rds"))
load(file = file.path(paths$marine, "CImpact_points.Rds"))  # load CImpact_points

load(file = file.path(paths$marine, "ROM_SST.Rds"))   # SSC and BCCM outputs of SST
load(file = file.path(paths$marine, "ROM_SSS.Rds"))   # SSC and BCCM outputs of SSS
# load(file = file.path(paths$marine, "SSC_SST_periods.Rds"))
# load(file = file.path(paths$marine, "BCCM_SST_periods.Rds"))



#------------- Take monthly means by MAZ and scenario ----------------------

CMIP_SST_summary <- CMIP6_SST %>%
  st_drop_geometry() %>%
  group_by(MAZ_Acrony, rcp, period_code) %>%
  summarize(across(contains("SST"), ~ mean(.x, na.rm = T)),
    .groups = "drop")

# filter to select the Cumulative impact habitat types then summarize by MAZ
CImpact_summary <- CImpact_points %>%
  st_drop_geometry() %>%
  select(-which(grepl("Cumul_Impact", names(.)) & names(.) != paste0("Cumul_Impact_", CI_type))) %>%
  group_by(MAZ_Acrony) %>%
  summarize(
    across(
      .cols = contains("Cumul_Impact"),
      .fns = list(
        mean       = ~ wmean(.x, Shape_Area, na.rm = T),
        qlowsp     = ~ wqt(.x, Shape_Area, prob = qlowsp, na.rm = T),
        qhighsp    = ~ wqt(.x, Shape_Area, prob = qhighsp, na.rm = T)
      ),
      .names = paste0("CImpact", "_", "{.fn}")
    ),
    .groups = "drop"
  )


#-----------3. Get indicators by CU ----------------------

for (i in 1:n.CUs) {
  cu_i <- cu_run$FULL_CU_IN[i]

  cu_marine_i <- cu_marine[cu_marine$FULL_CU_IN == cu_i, ]

  months_include <- seq(
    from = cu_marine_i$ns_timing_start,
    to = cu_marine_i$ns_timing_end,
    by = 1
  )

  SST_i <- subset_and_mean_var(st_drop_geometry(CMIP6_SST),
    months = months_include,
    MAZ_pick = cu_marine_i$MAZ,
    include_quantiles = TRUE)

  CImpact_i <- CImpact_summary %>%
    filter(MAZ_Acrony == cu_marine_i$MAZ)

  SST_ROM_i <- subset_and_mean_var(st_drop_geometry(ROM_SST),
    months = months_include,
    MAZ_pick = cu_marine_i$MAZ,
    include_quantiles = FALSE) %>%
    rename_with(
      ~ gsub("(SST)(proj|rate)", "\\1ddown\\2", .x),
      .cols = contains("SST")
    )

  SSS_ROM_i <- subset_and_mean_var(st_drop_geometry(ROM_SSS),
    months = months_include,
    MAZ_pick = cu_marine_i$MAZ,
    var_name = "SSS",
    include_quantiles = FALSE) %>%
    rename_with(
      ~ gsub("(SSS)(proj|rate)", "\\1ddown\\2", .x),
      .cols = contains("SSS")
    )

  SS_ROM_i <- left_join(SST_ROM_i, SSS_ROM_i, join_by(rcp, period_code))

  if (i == 1) {
    # cu_marine_all <- cu_marine_i
    # # SST_mar <- SST_mar_i
    # # SSS_mar <- SSS_mar_i
    # # CI_mar    <- CI_mar_i
    # #
    mar_all_flat <- bind_cols(cu_marine_i, SST_i, CImpact_i) %>%
      left_join(SS_ROM_i, join_by(rcp, period_code))
  }
  if (i > 1) {
    temp <- bind_cols(cu_marine_i, SST_i, CImpact_i) %>%
      left_join(SS_ROM_i, join_by(rcp, period_code))

    mar_all_flat <- bind_rows(mar_all_flat, temp)
  }

}


save(mar_all_flat,
  file = file.path(paths$marine, paste0(today, "_marine_stats.Rds")))
