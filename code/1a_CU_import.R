### 1a_CU_import.R

## import and process CU-level information


#----------------- CU-SMU crosswalk-------------------------------------------

cu_smu <- read.csv(file.path(paths$salmon, "CrossWalkData_2025-02-14.csv")) %>%
  filter(Conservation.Unit.Area == "FRASER INTERIOR",
         Conservation.Unit.Type == "Current",
         str_detect(Stock.Management.Unit.Name, "OKANAGAN", negate = TRUE)) %>%
  select(Stock.Management.Unit.Name, Stock.Management.Unit.Id, 
         Conservation.Unit.Name, Conservation.Unit.Area, Full.Conservation.Unit.Index,
         Conservation.Unit.Species, Designatable.Unit.Number)

spp_lookup <- tibble(
  spp_abr = c("ck", "cm", "co", "pk", "pk", "sk", "sk"),
  spp_abrC = c("CK", "CM", "CO", "PKE", "PKO", "SEL", "SER"),
  spp_abr_bcfp = c("ch", "cm", "co", "pk", "pk", "sk", "sk"),
  Species = c("Chinook", "Chum", "Coho", "Pink-Even", "Pink-Odd", "Sockeye (Lake Type)", "Sockeye (River Type)"),
  Species_simple = c("Chinook", "Chum", "Coho", "Pink", "Pink", "Sockeye", "Sockeye"),
  PSF_species = c("Chinook", "Chum", "Coho", "Pink", "Pink", "Sockeye-Lake", "Sockeye-River"))
  
  


#--------------------- Up-to-date CU list--------------------------------------

cu_list <- read.csv(file.path(paths$salmon,  "CCVA_CU_List.csv"), skip = 1) %>%
  mutate(cuid = as.integer(cuid)) %>%
  filter(CU_Type == "Current") %>%
  arrange(FULL_CU_IN)

cu_Fr <- cu_list %>%
  filter(CU_Area == "FRASER INTERIOR",
         str_detect(CU_NAME, "OKANAGAN", negate = TRUE),
         str_detect(CU_NAME, "BOUNDARY BAY", negate = TRUE))


#--------------------- CU Decoder ---------------------------------------------

cu_decoder <- read.csv(file.path(paths$salmon, "all_regions_cu_du_smu_decoder.csv")) %>%
  distinct(cuid, .keep_all = TRUE) %>%
  left_join(select(spp_lookup, spp_abrC, PSF_species), join_by(spp == PSF_species), multiple = "first") %>%
  relocate(spp_abrC)


#====================Import timing data compiled by PSF========================

# cu_timing_old <- read.csv(file.path(salmon_dat, "Timing data", 
#                                 "Life Cycle Timing by CU - CCVA old", "3Life_cycle_timing_by_CU_CL.csv")) %>%
#   filter(region == "fraser", !is.na(cuid)) %>%
#   left_join(select(cu_list, cuid, FULL_CU_IN), join_by(cuid))

cu_timing <- read.csv(file.path(paths$salmon, "Timing data", 
                              "CU_timing_published_CL.csv")) %>%
  rename(sp_dat_qual = dat_qual)

cu_timing_Fr <- filter(cu_timing, region == "fraser", !is.na(cuid)) %>%
  left_join(select(cu_list, cuid, FULL_CU_IN), join_by(cuid), multiple = "first") %>%
  relocate(FULL_CU_IN) %>%
  filter(!is.na(FULL_CU_IN))

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
  arrange(species)


#--------------------- Create CVIS table of demographic factors-----------------

CVIS_dem <- cu_Fr %>%
  select(cuid, CU_NAME, FULL_CU_IN, Species_simple,
         WSP_population_status, Most_Recent_Generational_Average,
         SEP_avg_annual_releases_actual, SEP_primary_prod_objective) %>%
  rename(DEM_stat = WSP_population_status,
         DEM_nmat  = Most_Recent_Generational_Average,
         DEM_enhann = SEP_avg_annual_releases_actual,
         DEM_enhobj = SEP_primary_prod_objective) %>%
  mutate(DEM_stat = if_else(DEM_stat == "", NA, DEM_stat),
         DEM_nmat = if_else(DEM_nmat == "", NA, DEM_nmat),
         DEM_enhann = if_else(DEM_enhann == "", NA, DEM_enhann),
         DEM_enhobj = if_else(DEM_enhobj == "", NA, DEM_enhobj))




