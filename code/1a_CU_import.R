# ==============================================================================
# CVIS CU Metadata & Status Data Import (1a_CU_import.R)
#
# Description:
#   Imports, processes, and merges Conservation Unit (CU) level data including 
#   crosswalks, decoders, WSP assessments status, spawner locations, and life 
#   history timing metrics. Generates standard subsets and formats the outputs.
#
# Workflow Steps:
#   1. Define cyclic CUs and auxiliary helper functions (infilling, CU formatting).
#   2. Load and merge the CU-SMU crosswalk and decoders, and map cuid.
#   3. Import CCVA CU lists with Freshwater Adaptive Zones (FAZs).
#   4. Load WSP rapid status data and compute generational geometric averages.
#   5. Import spawner locations from NuSEDS.
#   6. Import and process spawner timing data compiled by PSF, computing freshwater residency and nearshore marine entry months.
#   7. Subset target CUs to run for current analysis configurations.
#   8. Compile long format CU lists for scoring.
#   9. Save processed datasets to processed_data/CU/.
#
# Inputs:
#   - Various population, timing, and assessment CSV datasets in paths$salmon
#
# Outputs:
#   - Processed metadata tables (.Rds, .Rdata) in paths$CU
#
# Dependencies:
#   - Requires 0_setup.R.
# ==============================================================================

#define cyclic CUs (spawner abundance calculated differently)
cyclic_CUs <- c(
  "SEL-06-14", # Takla-Trembleur-Estu
  "SEL-09-02", # Shuswap-ES
  "SEL-06-13", # Takla-Trembleur-Stuart-S
  "SEL-06-10", # Quesnel-S
  "SEL-03-01", # Chilliwack-ES
  "SEL-09-03" # Shuswap-L
)

#species look up table
spp_lookup <- tibble(
  spp_abr = c("ck", "cm", "co", "pk", "pk", "sk", "sk"),
  spp_abrC = c("CK", "CM", "CO", "PKE", "PKO", "SEL", "SER"),
  spp_abr_bcfp = c("ch", "cm", "co", "pk", "pk", "sk", "sk"),
  Species = c("Chinook", "Chum", "Coho", "Pink-Even", "Pink-Odd", "Sockeye (Lake Type)", "Sockeye (River Type)"),
  SPECIES_NAME = c("Chinook", "Chum", "Coho", "Pink", "Pink", "Sockeye", "Sockeye"),
  PSF_species = c("Chinook", "Chum", "Coho", "Pink-Even", "Pink-Odd", "Sockeye-Lake", "Sockeye-River")
)



# ==================== 1. Define Helper Functions ====================
# simple infilling function for NA values, used for peak spawn timing
infill_average <- function(df, col1, col2, target_col) {
  # Replace NA values in the target column with the average of col1 and col2
  df[[target_col]] <- ifelse(
    is.na(df[[target_col]]),
    rowMeans(df[, c(col1, col2)], na.rm = TRUE),
    df[[target_col]]
  )
  return(df)
}

# adjust format of CU abbreviations to include leading zeroes (eg. CK-9 becomes CK-09)
adjust_CU_IN <- function(CU_IN_vector) {
  str_replace_all(CU_IN_vector, "-(\\d)(?!\\d)", "-0\\1")
}

# ==================== 2. CU-SMU Crosswalk & Decoder ====================

crosswalk<- read_csv(file.path(paths$salmon, "CrossWalkData_2026-06-10.csv")) %>%
  clean_names(case = "all_caps") %>%
    rename(FULL_CU_IN = CU_FULL_INDEX)
  
# Import timing data
# note oe_age added for some CUs from original file
cu_timing <- read.csv(file.path(
  paths$salmon, "Timing data",
  "CU_timing_published_CL.csv"
))

# get decoder from PSF package
decoder <- read.csv(file.path(paths$salmon, "all_regions_cu_du_smu_decoder.csv"))
# make digits double 00
decoder$FULL_CU_IN <- adjust_CU_IN(decoder$FULL_CU_IN)
decoder <- decoder %>%
  left_join(select(cu_timing, cuid, culabel), join_by(cuid)) %>%
  group_by(cuid) %>%
  slice(1) %>%
  ungroup()


# join cuid from decoder to crosswalk
crosswalk <- crosswalk %>%
  left_join(select(decoder, culabel, cuid, gen_length, FULL_CU_IN), join_by(FULL_CU_IN))
# missing <- filter(crosswalk, is.na(cuid))

cu_list <- crosswalk %>%
  select(-starts_with("DU")) %>%
  mutate(
    CVIS_LABEL = str_remove_all(culabel, regex(" River|\\(cyclic\\)|\\(de novo\\)", ignore_case = TRUE)) %>%
      str_trim(),
    # clean up SMU names by trimming salmon and "- "and making normal case
    SMU_SIMPLE = str_remove_all(SMU_NAME, regex("SALMON", ignore_case = TRUE)) %>%
      str_remove_all(" - ") %>%
      str_trim() %>%
      str_to_title()
      
  ) %>% 
  relocate(CVIS_LABEL, contains("CU"))


# ==================== 3. Custom CU List & FAZ Mapping ====================

cvis_cu_list <- read.csv(file.path(paths$salmon, "CCVA_CU_List.csv"), skip = 1) %>%
  mutate(
    cuid = as.integer(cuid),
    FULL_CU_IN = adjust_CU_IN(FULL_CU_IN)
  ) %>%
  filter(CU_Type == "Current") %>%
  arrange(FULL_CU_IN)

# add FAZ info to cu_list
cu_list <- cu_list %>%
  left_join(select(cvis_cu_list, FULL_CU_IN, FAZ), by = "FULL_CU_IN")

# Create FAZ groups for better visualization (Fraser vs Thompson vs Okanagan)
cu_list <- cu_list %>%
  mutate(FAZ_group = case_when(
    # Wide-range ones with many sub-groups
    str_count(FAZ, ",") >= 3 ~ "Fraser (Widespread)",
    # Thompson basin (STh, NTh, LTh)
    str_detect(FAZ, "NTh|LTh|STh") ~ "Thompson",
    # Okanagan
    str_detect(FAZ, "OK") ~ "Okanagan",
    # Lower Fraser Basins
    str_detect(FAZ, "LFR|LILL|BB") ~ "Lower Fraser",
    # Upper and Mid Fraser
    str_detect(FAZ, "MFR|UFR") ~ "Upper Fraser",
    # Upper and Mid Fraser
    str_detect(FAZ, "FRCany") ~ "Fraser Canyon",
    # Handle missing/empty
    is.na(FAZ) | FAZ == "" ~ "Unknown",
    TRUE ~ "Fraser"
  ))

#

# ==================== 4. Import WSP Rapid Status Data ====================

# get status file names
status_files <- list.files(file.path(paths$salmon, "FIA", "Status data"))
cultus_file <- status_files[str_detect(status_files, "Cultus Lake Sockeye - 2025 10 28.csv")] # get Cultus file
status_files <- status_files[str_detect(status_files, "Retro_Synoptic")]

for (i in 1:length(status_files))
{
  temp_data <- read_csv(file.path(paths$salmon, "FIA", "Status data", status_files[i]))

  if (i == 1) status_data <- temp_data
  if (i > 1) status_data <- bind_rows(status_data, temp_data)
}

cultus_data <- read_csv(file.path(paths$salmon, "FIA", "Status data", cultus_file)) %>%
  mutate(
    SpnForAbd_Wild = eff_fem + males,
    SpnForTrend_Wild = eff_fem,
    FULL_CU_IN = "SEL-03-02",
    RapidStatus = "Red"
  ) %>%
  select(any_of(c("FULL_CU_IN", "Year", "SpnForAbd_Wild", "SpnForTrend_Wild", "RapidStatus"))) %>%
  filter(Year >= 1995)

# split Chilko and add Cultus, add metadata from cu_list
# do some infilling of missing status and gen values
status_data <- status_data %>%
  rename(
    FULL_CU_IN = CU_ID,
    GenAvgUsed = any_of("GenAvgUsed.x")
  ) %>%
  bind_rows(cultus_data) %>%
  left_join(select(cu_list, FULL_CU_IN, CU_COMMON_NAME, CVIS_LABEL, SPECIES_NAME, gen_length), join_by(FULL_CU_IN)) %>%
  relocate(CVIS_LABEL, .after = FULL_CU_IN) %>%
  select(-Stock) %>%
  mutate(FULL_CU_IN = if_else(FULL_CU_IN == "SEL-06-03/SEL-06-02", "SEL-06-03", FULL_CU_IN)) %>% # change Chilko ES-S to Chilko S
  add_row(FULL_CU_IN = "SEL-06-02", RapidStatus = "None", Species = "Sockeye", Year = 2023) %>% # add data deficient recent entry for Chilko ES
  # calculate geometric average across generations, to fill in for NA values
  # Group by CU to avoid contamination across boundary years of different CUs
  group_by(FULL_CU_IN, Species) %>%
  mutate(roll_avg_gen = sapply(seq_along(SpnForAbd_Wild), function(i) {
    k <- gen_length[i] # window size from gen_length column
    if (i < k | is.na(k)) {
      NA
    } else {
      exp(mean(log(SpnForAbd_Wild[(i - k + 1):i]), na.rm = TRUE))
    }
  })) %>%
  mutate(roll_avg_gen = if_else(is.na(SpnForAbd_Wild), roll_avg_gen, SpnForAbd_Wild)) %>%
  mutate(CVIS_RapidStatus = if_else(RapidStatus == "None" & roll_avg_gen < min_gen_red, "Red", RapidStatus)) %>%
  ungroup() %>%
  relocate(roll_avg_gen, .after = GenAvgUsed)



## get CUnmat - number of mature individuals
# use geometric average, unless CU is cyclic. If cyclic use dominant year (max of 4 year rolling window)
status_data <- status_data %>%
  arrange(FULL_CU_IN, Species, Year) %>%
  group_by(FULL_CU_IN, Species) %>%
  mutate(
    Max4yr_SpnForAbd_Wild = sapply(
      seq_along(SpnForAbd_Wild),
      function(i) {
        start <- max(1, i - 3) # 4-year window: current year and previous 3
        max(SpnForAbd_Wild[start:i], na.rm = TRUE)
      }
    )
  ) %>%
  ungroup() %>%
  mutate(CyclicCU = if_else(FULL_CU_IN %in% cyclic_CUs, TRUE, FALSE)) %>%
  mutate(CUnmat_mean = if_else(CyclicCU == TRUE, round(Max4yr_SpnForAbd_Wild), round(roll_avg_gen)))

status_data <- status_data %>%
  mutate(
    CUstatus_mean = unname(status_map[CVIS_RapidStatus]),
    status_year = Year
  ) # NA for unmapped


# get recent status
# only take last 4 years of data. Use most recent year of status, removing any values with no status
recent_status <- status_data %>%
  filter(Year >= max(Year) - 4) %>%
  # filter(RapidStatus != "None") %>%
  group_by(FULL_CU_IN) %>%
  slice_max(order_by = Year, n = 1) %>%
  ungroup()

# join to CU list
cu_list <- cu_list %>%
  left_join(select(recent_status, FULL_CU_IN, CUstatus_mean, CUnmat_mean, status_year), join_by(FULL_CU_IN))

# ==================== 5. Import NuSEDS Spawner Locations ====================

### NUSEDS salmon spawner locations
## version from FIA. Usage column added by Michael Arbeider
nuseds_Fr <- read_csv(file.path(paths$salmon, "NuSEDS_CU_System_sites_202406.csv")) %>%
  st_as_sf(coords = c("X_LONGT", "Y_LAT"), crs = 4269) %>%
  st_transform(3005) %>%
  filter(USAGE != "REMOVE")

nuseds_Fr$FULL_CU_IN <- adjust_CU_IN(nuseds_Fr$FULL_CU_IN)

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


# ==================== 6. Process PSF Timing Data ====================

cu_timing <- cu_timing %>%
  rename(sp_dat_qual = dat_qual) %>%
  mutate(oe_age = as.numeric(str_sub(oe_age, start = 1, end = 1))) %>%
  left_join(select(filter(cu_list, !is.na(cuid)), cuid, CVIS_LABEL, FULL_CU_IN, CU_NAME, SPECIES_NAME), join_by(cuid), multiple = "first") %>%
  relocate(FULL_CU_IN, CVIS_LABEL, CU_NAME, SPECIES_NAME)

cu_timing <- infill_average(cu_timing,
  col1 = "sp_start", col2 = "sp_end",
  target_col = "sp_peak"
)

cu_timing <- cu_timing %>%
  filter(!is.na(FULL_CU_IN)) %>%
  # calculate additional timing parameters for CVIS indicators
  mutate(fwres_mean = (365 - sp_peak) + oe_peak + (oe_age * 365)) %>% # peak spawn to ocean entry (fwres)
  # ocean entry months
  mutate(
    oe_peak_month = month(ymd(paste(
      "2000", "01", "01",
      sep = "-"
    )) + oe_peak - 1),
    oe_start_month = month(ymd(paste(
      "2000", "01", "01",
      sep = "-"
    )) + oe_start - 1),
    oe_end_month = month(ymd(paste(
      "2000", "01", "01",
      sep = "-"
    )) + oe_end - 1)
  ) %>%
  # nearshore start and end timing for marine indicators (see 0_setup for values used)
  mutate(
    ns_start_month = ns_timing_start(oe_peak_month, ns_start_offset, ns_time_method),
    ns_end_month = ns_timing_end(oe_peak_month, ns_end_offset, ns_time_method)
  ) %>%
  arrange(species, oe_age)

cu_timing_Fr <- filter(cu_timing, region == "fraser", !is.na(cuid))

cu_timing_long <- cu_timing_Fr %>%
  pivot_longer(
    cols = c(
      fm_start, fm_peak, fm_end, fm_dat_qual, oe_start, oe_peak, oe_end, oe_dat_qual,
      rt_start, rt_peak, rt_end, rt_dat_qual, sp_start, sp_peak, sp_end, sp_dat_qual
    ),
    names_to = "timing_event",
    values_to = "date"
  ) %>%
  select(-c(faz:oe_source_faz)) %>%
  mutate(
    life_stage = str_sub(timing_event, start = 1, end = 2),
    timing = str_sub(timing_event, start = 4)
  ) %>%
  select(-timing_event) %>%
  pivot_wider(names_from = timing, values_from = date) %>%
  mutate(life_stage = if_else(life_stage == "fm", "freshwater_migration",
    if_else(life_stage == "oe", "ocean_entry",
      if_else(life_stage == "rt", "run_timing",
        if_else(life_stage == "sp", "spawning",
          if_else(life_stage == "ar", "arrival", NA)
        )
      )
    )
  )) %>%
  arrange(species, oe_age)

# add freshwater residence timing indicators to cu_list
cu_list <- cu_list %>%
  left_join(select(cu_timing, FULL_CU_IN, fwres_mean), join_by(FULL_CU_IN))


# ==================== 7. Subset CUs to Run ====================

# Select subset of CUs to run for analysis
cu_run <- cu_list %>%
  filter(
    DFO_AREA %in% DFO_area_include,
    FULL_CU_IN %notin% CU_exclude,
    CU_TYPE %in% CU_type_include,
    SPECIES_NAME %in% species_include
  ) %>%
  arrange(SPECIES_NAME)

cu_seq <- cu_run$FULL_CU_IN # Create vector of CUs to analyze, ordered CK, CM, CO, PKO, SEL, SER, SH
n.CUs <- nrow(cu_run)


# ==================== 8. Compile Long Format CU Lists ====================

cu_long <- cu_run %>%
  select(FULL_CU_IN, matches(tbl_indicators$abbrev)) %>%
  pivot_longer(
    cols = matches(tbl_indicators$abbrev),
    names_to = c("indicator", "stat"),
    # REGEX: capture indicator root, optional underscore + suffix
    #  ^(.*?)         -> indicator prefix (lazy)
    #  (?:_(.*))?     -> optional group: underscore and then suffix (stat)
    names_pattern = "^(.*?)(?:_(.*))?$",
    values_to = "value"
  ) %>%
  mutate(
    gcm         = 0L,
    rcp         = 0L,
    period_code = 0L
  ) %>%
  left_join(select(tbl_indicators, abbrev, category), join_by(indicator == abbrev))


# ==================== 9. Save Processed Datasets ====================

save(cu_list, file = file.path(paths$CU, "cu_list.Rds"))
save(status_data, file = file.path(paths$CU, "cu_status_data.Rds"))
save(cu_timing_Fr, cu_timing_long, file = file.path(paths$CU, "cu_timing_data.Rdata"))
save(cu_run, file = file.path(paths$CU, "cu_run.Rds"))
