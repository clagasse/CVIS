### 1a_CU_import.R

## import and process CU-level information

# -----  Functions -----------------
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

adjust_CU_IN <- function(CU_IN_vector) {
  str_replace_all(CU_IN_vector, "-(\\d)(?!\\d)", "-0\\1")
}

#----------------- CU-SMU crosswalk-------------------------------------------

crosswalk <- read.csv(file.path(paths$salmon, "CrossWalkData_2025-10-10.csv")) %>%
  clean_names(case = "all_caps") %>%
  rename(FULL_CU_IN = CU_FULL_INDEX)
# crosswalk$FULL_CU_IN <- str_replace_all(crosswalk$FULL_CU_IN, "-0(\\d)(?!\\d)", "-\\1")

# get decoder from PSF package
decoder <- read.csv(file.path(paths$salmon, "all_regions_cu_du_smu_decoder.csv"))
# make digits double 00
decoder$FULL_CU_IN <- adjust_CU_IN(decoder$FULL_CU_IN)
decoder <- decoder %>%
  group_by(cuid) %>%
  slice(1) %>%
  ungroup()

# join cuid from decoder to crosswalk
crosswalk <- crosswalk %>%
  left_join(select(decoder, cuid, FULL_CU_IN), join_by(FULL_CU_IN))
# missing <- filter(crosswalk, is.na(cuid))

cu_Fr <- crosswalk %>%
  filter(DFO_AREA == "FRASER AND INTERIOR",
    CU_TYPE == "Current",
    CU_NAME != "BOUNDARY BAY_FA_0.3",
    str_detect(SMU_NAME, "OKANAGAN", negate = TRUE)) %>%
  select(-starts_with("DU")) %>%
  mutate(
    CVIS_NAME = str_remove_all(CU_NAME, regex("TIMING", ignore_case = TRUE)) %>%
      str_trim()) %>% # remove extra spaces if any
  mutate(CVIS_NAME = paste0(FULL_CU_IN, "_", CVIS_NAME)) %>%
  relocate(CVIS_NAME, contains("CU"))

# add record for Chilko ES/S combined


spp_lookup <- tibble(
  spp_abr = c("ck", "cm", "co", "pk", "pk", "sk", "sk"),
  spp_abrC = c("CK", "CM", "CO", "PKE", "PKO", "SEL", "SER"),
  spp_abr_bcfp = c("ch", "cm", "co", "pk", "pk", "sk", "sk"),
  Species = c("Chinook", "Chum", "Coho", "Pink-Even", "Pink-Odd", "Sockeye (Lake Type)", "Sockeye (River Type)"),
  Species_simple = c("Chinook", "Chum", "Coho", "Pink", "Pink", "Sockeye", "Sockeye"),
  PSF_species = c("Chinook", "Chum", "Coho", "Pink", "Pink", "Sockeye-Lake", "Sockeye-River"))


#--------------------- Up-to-date CU list--------------------------------------

# cu_list <- read.csv(file.path(paths$salmon,  "CCVA_CU_List.csv"), skip = 1) %>%
#   mutate(cuid = as.integer(cuid)) %>%
#   filter(CU_Type == "Current") %>%
#   arrange(FULL_CU_IN)
#
# cu_Fr <- cu_list %>%
#   filter(CU_Area == "FRASER INTERIOR",
#     str_detect(CU_NAME, "OKANAGAN", negate = TRUE),
#     str_detect(CU_NAME, "BOUNDARY BAY", negate = TRUE)) %>%
#   mutate(
#     CVIS_NAME = str_remove_all(CU_NAME, regex("TIMING", ignore_case = TRUE)) %>%
#       str_trim()) %>%  # remove extra spaces if any
#   mutate(CVIS_NAME = paste0(FULL_CU_IN, "_", CVIS_NAME)) %>%
#   relocate(CVIS_NAME, .after = CU_NAME)
#

#-------------------- Import WSP Rapid Status Data------------------------------

# get status file names
status_files <- list.files(file.path(paths$salmon, "FIA", "Status data"))
status_files <- status_files[str_detect(status_files, "Retro_Synoptic")]

for (i in 1:length(status_files))
{
  temp_data <- read_csv(file.path(paths$salmon, "FIA", "Status data", status_files[i]))

  if (i == 1) status_data <- temp_data
  if (i > 1) status_data <- bind_rows(status_data, temp_data)
}
status_data <- status_data %>%
  rename(FULL_CU_IN = CU_ID) %>%
  left_join(select(cu_Fr, FULL_CU_IN, CVIS_NAME), join_by(FULL_CU_IN))

status_data$CUstatus <- status_data$RapidStatus
status_data$CUnmat   <- status_data$SpnForAbd_Wild
status_data$status_year <- status_data$Year

recent_status <- status_data %>%
  group_by(FULL_CU_IN) %>%
  slice_max(order_by = Year, n = 1) %>%
  ungroup()


# join to CU list
cu_Fr <- cu_Fr %>%
  left_join(select(recent_status, FULL_CU_IN, CUstatus, CUnmat, status_year), join_by(FULL_CU_IN))


# ====================Import timing data compiled by PSF========================

# note oe_age added for some CUs from original file
cu_timing <- read.csv(file.path(paths$salmon, "Timing data",
  "CU_timing_published_CL.csv")) %>%
  rename(sp_dat_qual = dat_qual) %>%
  mutate(oe_age = as.numeric(str_sub(oe_age, start = 1, end = 1)))

cu_timing <- infill_average(cu_timing, col1 = "sp_start", col2 = "sp_end",
  target_col = "sp_peak")
cu_timing <- cu_timing %>%
  mutate(peak_sp_to_oe = (365 - sp_peak) + oe_peak + (oe_age * 365))

cu_timing_Fr <- filter(cu_timing, region == "fraser", !is.na(cuid)) %>%
  left_join(select(crosswalk, cuid, FULL_CU_IN), join_by(cuid), multiple = "first") %>%
  relocate(FULL_CU_IN) %>%
  filter(!is.na(FULL_CU_IN)) %>%
  arrange(species, oe_age)

cu_timing_long <- cu_timing_Fr %>%
  pivot_longer(cols = c(fm_start, fm_peak, fm_end, fm_dat_qual, oe_start, oe_peak, oe_end, oe_dat_qual,
    rt_start, rt_peak, rt_end, rt_dat_qual, sp_start, sp_peak, sp_end, sp_dat_qual),
  names_to = "timing_event",
  values_to = "date") %>%
  select(-c(faz:oe_source_faz)) %>%
  mutate(life_stage = str_sub(timing_event, start = 1, end = 2),
    timing = str_sub(timing_event, start = 4)) %>%
  select(-timing_event) %>%
  pivot_wider(names_from = timing, values_from = date) %>%
  mutate(life_stage = if_else(life_stage == "fm", "freshwater_migration",
    if_else(life_stage == "oe", "ocean_entry",
      if_else(life_stage == "rt", "run_timing",
        if_else(life_stage == "sp", "spawning",
          if_else(life_stage == "ar", "arrival", NA)))))) %>%
  arrange(species, oe_age)


### ----- Load frequently used data sets- ------


### Conservation Unit boundaries for Fraser CUs
cu_boundary <- st_read(file.path(paths$spatial, "CU_boundaries", "fraser_cus.shp")) %>%
  st_make_valid() %>%
  st_transform(crs = 3005)  %>%  # crs 3005 is NAD83/BC Albers
  left_join(select(cu_Fr, cuid, FULL_CU_IN, SPECIES_NAME),
    join_by(CUID == cuid)) %>%
  filter(!is.na(FULL_CU_IN))


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
