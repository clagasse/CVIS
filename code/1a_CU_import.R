### 1a_CU_import.R

## import and process CU-level information


#------------------------------------------------------------------------------
# CU-SMU crosswalk
#------------------------------------------------------------------------------

cu_smu <- read.csv(file.path(salmon_dat, "CrossWalkData_2025-02-14.csv")) %>%
  filter(Conservation.Unit.Area == "FRASER INTERIOR",
         Conservation.Unit.Type == "Current",
         str_detect(Stock.Management.Unit.Name, "OKANAGAN", negate = TRUE)) %>%
  select(Stock.Management.Unit.Name, Stock.Management.Unit.Id, 
         Conservation.Unit.Name, Conservation.Unit.Area, Full.Conservation.Unit.Index,
         Conservation.Unit.Species, Designatable.Unit.Number)


#------------------------------------------------------------------------------
# Up-to-date CU list
#------------------------------------------------------------------------------

cu_list <- read.csv(file.path(salmon_dat, "CCVA_CU_List.csv"), skip = 1) %>%
  mutate(cuid = as.integer(cuid)) %>%
  filter(CU_Type == "Current") %>%
  arrange(FULL_CU_IN)

cu_Fr <- cu_list %>%
  filter(CU_Area == "FRASER INTERIOR",
         str_detect(CU_NAME, "OKANAGAN", negate = TRUE))




#########################################################################
## Import timing data compiled by PSF
#########################################################################

cu_timing <- read.csv(file.path(salmon_dat, "Timing data", 
                                "Life Cycle Timing by CU - CCVA old", "3Life_cycle_timing_by_CU_CL.csv")) %>%
  filter(region == "fraser", !is.na(cuid)) %>%
  left_join(select(cu_list, cuid, FULL_CU_IN), join_by(cuid))

cu_runtime_long <- cu_timing %>%
  pivot_longer(cols = c(rt_start, rt_end),
               names_to = "rt",
               values_to = "rt_date") %>%
  arrange(species)

#ggplot(data =cu_runtime_long) +
#  geom_violin(aes(x= culabel, y = rt_date, fill = species))




