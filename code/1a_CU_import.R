### 1a_CU_import.R

## import and process CU-level information


#------------------------------------------------------------------------------
# Up-to-date CU list (taken from database)
#------------------------------------------------------------------------------

cu_list <- read.csv(file.path(salmon_dat, "CCVA_CU_List.csv"), skip = 1) %>%
  mutate(cuid = as.integer(cuid)) %>%
  subset(Area_Region == "FRASER") %>%
  filter(COSEWIC_status != "Extinct" | is.na(COSEWIC_status), !is.na(FULL_CU_IN)) %>%
  filter(!is.na(DU_number)) %>%
  arrange(FULL_CU_IN)


# # Create lookup for spawning and rearing fields in the geodatabase
# spp_lookup <- data.frame(
#   species_pooled = sort(unique(cu_list$Species_simple)),
#   streams_code = c("ch", "cm", "co", "pk", "sk")
# )

#cu_list$spp <- spp_lookup$streams_code[match(cu_list$Species_simple, spp_lookup$Species_simple, nomatch=NA)]


#remove Widgeon (for now due to throwing errors)
#cu_run <- filter(cu_list, cuname %notin% c("Widgeon", "Harrison River"))

#spp_lookup_run <- spp_lookup[spp_lookup$streams_code %in% unique(cu_run$spp),]


#########################################################################
## Import timing data compiled by PSF
#########################################################################

cu_timing <- read.csv(file.path(salmon_dat, "Timing data", 
                                "Life Cycle Timing by CU - CCVA old", "3Life_cycle_timing_by_CU_CL.csv")) %>%
  filter(region == "fraser", !is.na(cuid)) %>%
  left_join(select(cu_list, cuid, cu_acronym), join_by(cuid))

cu_runtime_long <- cu_timing %>%
  pivot_longer(cols = c(rt_start, rt_end),
               names_to = "rt",
               values_to = "rt_date") %>%
  arrange(species)

#ggplot(data =cu_runtime_long) +
#  geom_violin(aes(x= culabel, y = rt_date, fill = species))




