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

# load marine model files
load(file = file.path(paths$marine, "CMIP6_SST_periods.Rds"))
load(file = file.path(paths$marine, "CImpact_points.Rds")) # load CImpact_points

load(file = file.path(paths$marine, "ROM_SST.Rds")) # SSC and BCCM outputs of SST
load(file = file.path(paths$marine, "ROM_SSS.Rds")) # SSC and BCCM outputs of SSS
# load(file = file.path(paths$marine, "SSC_SST_periods.Rds"))
# load(file = file.path(paths$marine, "BCCM_SST_periods.Rds"))

# load marine adaptive zone spatial object
load(file.path(paths$marine, "MAZ.Rds"))


#------------- Timing data processing

# get cu timing for ocean entry
cu_marine <- cu_timing_Fr %>%
  select(
    FULL_CU_IN, CVIS_NAME, SPECIES_NAME, oe_age, oe_dat_qual,
    oe_start, oe_peak, oe_end, n_oe, ns_start_month, ns_end_month
  ) %>%
  mutate(MAZ = "GStr") %>%
  select(-any_of(c("oe_age", "oe_peak"))) # remove these columns since they are already in fwR data frame


#make a table for MAZs with timing to get values
maz_marine <- st_drop_geometry(MAZ) %>%
  mutate(
  ns_start_month = ns_start_static,
  ns_end_month = ns_end_static
)



#------------- Take monthly means by MAZ and scenario ----------------------

CMIP_SST_summary <- CMIP6_SST %>%
  st_drop_geometry() %>%
  group_by(MAZ_Acrony, rcp, period_code) %>%
  summarize(across(contains("SST"), ~ mean(.x, na.rm = T)),
    .groups = "drop"
  )

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
        qlowsp     = ~ wqt(.x, Shape_Area, prob = qlsp, na.rm = T),
        qhighsp    = ~ wqt(.x, Shape_Area, prob = qhsp, na.rm = T)
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
    from = cu_marine_i$ns_start_month,
    to = cu_marine_i$ns_end_month,
    by = 1
  )

  SST_i <- subset_and_mean_var(st_drop_geometry(CMIP6_SST),
    months = months_include,
    MAZ_pick = cu_marine_i$MAZ,
    include_quantiles = TRUE
  ) %>%
    mutate(
      dsmodel = "qdm",
      FULL_CU_IN = cu_i
    ) %>%
    pivot_longer(
      cols = matches("SST"),
      names_to = c("indicator", "stat"),
      # REGEX: capture indicator root, optional underscore + suffix
      #  ^(.*?)         -> indicator prefix (lazy)
      #  (?:_(.*))?     -> optional group: underscore and then suffix (stat)
      names_pattern = "^(.*?)(?:_(.*))?$",
      values_to = "value"
    )

  CImpact_i <- CImpact_summary %>%
    filter(MAZ_Acrony == cu_marine_i$MAZ) %>%
    select(-MAZ_Acrony) %>%
    mutate(
      dsmodel = "CImpact",
      FULL_CU_IN = cu_i
    ) %>%
    pivot_longer(
      cols = matches("CImpact"),
      names_to = c("indicator", "stat"),
      # REGEX: capture indicator root, optional underscore + suffix
      #  ^(.*?)         -> indicator prefix (lazy)
      #  (?:_(.*))?     -> optional group: underscore and then suffix (stat)
      names_pattern = "^(.*?)(?:_(.*))?$",
      values_to = "value"
    )

  SST_ROM_i <- subset_and_mean_var(st_drop_geometry(ROM_SST),
    months = months_include,
    MAZ_pick = cu_marine_i$MAZ,
    include_quantiles = FALSE
  ) %>%
    mutate(
      dsmodel = "bccmssc",
      FULL_CU_IN = cu_i
    ) %>%
    pivot_longer(
      cols = matches("SST"),
      names_to = c("indicator", "stat"),
      # REGEX: capture indicator root, optional underscore + suffix
      #  ^(.*?)         -> indicator prefix (lazy)
      #  (?:_(.*))?     -> optional group: underscore and then suffix (stat)
      names_pattern = "^(.*?)(?:_(.*))?$",
      values_to = "value"
    )

  SSS_ROM_i <- subset_and_mean_var(st_drop_geometry(ROM_SSS),
    months = months_include,
    MAZ_pick = cu_marine_i$MAZ,
    var_name = "SSS",
    include_quantiles = FALSE
  ) %>%
    mutate(
      dsmodel = "bccmssc",
      FULL_CU_IN = cu_i
    ) %>%
    pivot_longer(
      cols = matches("SSS"),
      names_to = c("indicator", "stat"),
      # REGEX: capture indicator root, optional underscore + suffix
      #  ^(.*?)         -> indicator prefix (lazy)
      #  (?:_(.*))?     -> optional group: underscore and then suffix (stat)
      names_pattern = "^(.*?)(?:_(.*))?$",
      values_to = "value"
    )


  # join to one table
  mar_all_i <- bind_rows(SST_i, SST_ROM_i, SSS_ROM_i, CImpact_i)


  if (i == 1) {
    mar_all <- mar_all_i
  }
  if (i > 1) {
    mar_all <- bind_rows(mar_all, mar_all_i)
  }
}


# clean up values
mar_all <- mar_all %>%
  mutate(
    rcp = if_else(rcp == "H" | is.na(rcp), "0", rcp),
    period_code = if_else(is.na(period_code), 0, period_code),
    gcm = if_else(period_code == 0, 0, 9),
    category = "mar"
  ) %>%
  relocate(FULL_CU_IN)



# Get indicators by MAZ ---------------------------------------------------



for (i in 1:nrow(maz_marine)) {
  maz_i <- maz_marine[i,]
  
  months_include <- seq(
    from = maz_i$ns_start_month,
    to = maz_i$ns_end_month,
    by = 1
  )
  
  SST_i <- subset_and_mean_var(st_drop_geometry(CMIP6_SST),
                               months = months_include,
                               MAZ_pick = maz_i$MAZ_Acrony,
                               include_quantiles = TRUE
  ) %>%
    mutate(
      MAZ = maz_i$MAZ_Acrony,
      dsmodel = "qdm"
    ) %>%
    pivot_longer(
      cols = matches("SST"),
      names_to = c("indicator", "stat"),
      # REGEX: capture indicator root, optional underscore + suffix
      #  ^(.*?)         -> indicator prefix (lazy)
      #  (?:_(.*))?     -> optional group: underscore and then suffix (stat)
      names_pattern = "^(.*?)(?:_(.*))?$",
      values_to = "value"
    )
  
  CImpact_i <- CImpact_summary %>%
    filter(MAZ_Acrony == maz_i$MAZ_Acrony) %>%
    select(-MAZ_Acrony) %>%
    mutate(
      MAZ = maz_i$MAZ_Acrony,
      dsmodel = "CImpact"
    ) %>%
    pivot_longer(
      cols = matches("CImpact"),
      names_to = c("indicator", "stat"),
      # REGEX: capture indicator root, optional underscore + suffix
      #  ^(.*?)         -> indicator prefix (lazy)
      #  (?:_(.*))?     -> optional group: underscore and then suffix (stat)
      names_pattern = "^(.*?)(?:_(.*))?$",
      values_to = "value"
    )
  
  SST_ROM_i <- subset_and_mean_var(st_drop_geometry(ROM_SST),
                                   months = months_include,
                                   MAZ_pick = maz_i$MAZ_Acrony,
                                   include_quantiles = FALSE
  ) %>%
    mutate(
      MAZ = maz_i$MAZ_Acrony,
      dsmodel = "bccmssc"
    ) %>%
    pivot_longer(
      cols = matches("SST"),
      names_to = c("indicator", "stat"),
      # REGEX: capture indicator root, optional underscore + suffix
      #  ^(.*?)         -> indicator prefix (lazy)
      #  (?:_(.*))?     -> optional group: underscore and then suffix (stat)
      names_pattern = "^(.*?)(?:_(.*))?$",
      values_to = "value"
    )
  
  SSS_ROM_i <- subset_and_mean_var(st_drop_geometry(ROM_SSS),
                                   months = months_include,
                                   MAZ_pick = maz_i$MAZ_Acrony,
                                   var_name = "SSS",
                                   include_quantiles = FALSE
  ) %>%
    mutate(
      MAZ = maz_i$MAZ_Acrony,
      dsmodel = "bccmssc"
    ) %>%
    pivot_longer(
      cols = matches("SSS"),
      names_to = c("indicator", "stat"),
      # REGEX: capture indicator root, optional underscore + suffix
      #  ^(.*?)         -> indicator prefix (lazy)
      #  (?:_(.*))?     -> optional group: underscore and then suffix (stat)
      names_pattern = "^(.*?)(?:_(.*))?$",
      values_to = "value"
    )
  
  
  # join to one table
  maz_all_i <- bind_rows(SST_i, SST_ROM_i, SSS_ROM_i, CImpact_i)
  
  
  if (i == 1) {
    maz_all <- maz_all_i
  }
  if (i > 1) {
    maz_all <- bind_rows(maz_all, maz_all_i)
  }
}

#clean up values
maz_all <- maz_all %>%
  mutate(
    rcp = if_else(rcp == "H" | is.na(rcp), "0", rcp),
    period_code = if_else(is.na(period_code), 0, period_code),
    gcm = if_else(period_code == 0, 0, 9),
    category = "mar"
  )




# Save output -------------------------------------------------------------

save(mar_all, maz_all,
  file = file.path(paths$marine, paste0(today, "_marine_stats.Rdata"))
)
