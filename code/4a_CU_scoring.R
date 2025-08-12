################################################################################
#
# 4a_CU_scoring.R
#
# 1 - get indicator values for each CU
# 2 - apply lower thresholds and standardization function
# 3 - calculate standardized indicator values
# 4 - summarize and compare indicator values across CUs
# 5 - estimate vulnerability indices in different categories
#
###############################################################################
#----------------1. Setup and import----------------
library(here)
setwd(here())
source(file.path(here(), "code", "0_setup.R"))


#table of indicator abbreviations and full names
tbl_indicators <- tribble(
  ~abbrev, ~type, ~std_fun, ~name,
  "Fav_change", "fwR",  "invlinear_std", "ENM Change in Favourability",
  "ct",         "fwR",     "linear_std",     "Cumulative threats",
  "Tw8rate",   "fwR",       "linear_std",  "Rate of change in August Temperature",
  "Tw8proj",   "fwR", "exponential_std", "Projected August Temperature",
  "highQpdelta", "fwR",   "linear_std", "Proportional change in August flow (stream model)",
  "lowQpdelta",  "fwR",   "invlinear_std", "Proportional change in Nov-Jan flow (stream model)",
  "st8pdelta",  "fwR", "invlinear_std", "Proportional change in August flow (station model)",
  "fw_res",     "fwR",   "step_std",   "Freshwater residency time",
  "migrT",      "migr",    "linear_std",     "Projected temperature during upstream migration",
  "migrQ",      "migr",      "linear_std",    "Projected discharge during upstream migration",
  "migrTthr19",  "migr",    "exponential_std", "Average proportion of path above 19 degrees during upstream migration",
  "migr_len",    "migr",     "linear_std",     "Length of upstream migration",
  
  "stat",      "dem", "cat_std", "WSP status",
  "nmat",      "dem", "decay_std", "Number of mature individuals",
  "enh_rat",   "dem", "enh_std", "Ratio of releases to recent generational abundance of spawners")



#load FW rearing indicators
fwR_all_flat <- read_csv(file.path(paths$fw, "2025-06-12_fw_rearing_stats.csv")) %>%
  rename(period_code = period) %>%
  mutate(RCP = as.character(RCP))

#load FW migration indicators
load(file.path(paths$fw, "2025-07-31_migr_stats.Rdata"))

#load marine indicators
mar_all_flat <- read_csv(file.path(paths$marine, "2025-08-11_marine_stats.csv"))


#combine indicators into common table




#translation table between periods
period_lookup <- tribble(
  ~period_code, ~period,
  0, "1981-2000",
  0, "1981-2010",
  1, "2011-2020",
  2, "2021-2040",
  3, "2041-2060",
  4, "2061-2080",
  5, "2081-2099"
)

#add period code to migration table
migr_all_flat <- migr_all_flat %>%
  left_join(period_lookup, by = "period") %>%
  relocate(period_code, .after = period) %>%
  select(-period)

#combine indicators into single table
all_inds <- fwR_all_flat %>%
  left_join(migr_all_flat, by = c("FULL_CU_IN",  "CU_NAME", "CU_Species", "FAZ", "period_code", "RCP")) %>%
  left_join(CVIS_dem, by = c("FULL_CU_IN", "CU_NAME")) %>%
  select(FULL_CU_IN, CU_NAME, CU_Species, FAZ, period_code, RCP, SSP, contains(tbl_indicators$abbrev))      






#--------------2. Functions for standardization ------
standardize_spawning <- function(data, 
                                 indicator_pick,
                                 period_pick = "3",
                                 RCPs = c("45", "85"),
                                 gcm_range_suffix = c("qlow_gcm", "qhigh_gcm"),
                                 use_gcm_range = TRUE) 
{
  stat_suffix <- "wmean"
  id_col <- "FULL_CU_IN"
  
  data <- filter(data, period == period_pick)
  
  #take column names that contain prefix with model type
  cols_sub <- names(data)[str_detect(names(data), indicator_pick$abbrev)]
  
  #take names with prefix that also contain stat suffix
  stat_col <- cols_sub[str_detect(cols_sub, paste0(stat_suffix, "$"))]  #must end with wmean
  min_gcmcol <- cols_sub[str_detect(cols_sub, paste0(gcm_range_suffix[1], collapse = "|"))]
  max_gcmcol <- cols_sub[str_detect(cols_sub, paste0(gcm_range_suffix[2], collapse = "|"))]
  
  if (length(min_gcmcol) == 1 && length(max_gcmcol) == 1 && use_gcm_range == TRUE) {
    #select indicator columns and give one column each indicator
    wide_data <- pivot_wider(select(data, c(FULL_CU_IN, stat_col, min_gcmcol, max_gcmcol, RCP)),
                             names_from = RCP,
                             values_from = all_of(c(stat_col, min_gcmcol, max_gcmcol)))
  } else {
    wide_data <- pivot_wider(select(data, c(FULL_CU_IN, stat_col, RCP)),
                             names_from = RCP,
                             names_prefix = paste0(indicator_pick$abbrev, "_", stat_suffix, "_"),
                             values_from = all_of(c(stat_col)))
  }
    
  
  #pivot into a single row for calculating standardized values
  long_data <- pivot_longer(wide_data,
                            cols = starts_with(indicator_pick$abbrev),
                            names_prefix = paste0(indicator_pick$abbrev, "_"))
  
  
  #calculate indicator using function.  choose function using name of std_fun
  long_data$std <- get(indicator_pick$std_fun)(long_data$value)
  
  #put back into wide data frame to join original
  wide_std <- pivot_wider(select(long_data, -value),
                          id_cols = c(FULL_CU_IN),
                          names_from = name,
                          values_from = std)
  
  RCPs_grep <- paste(RCPs, collapse = "|")
  
  #pivot across RCP to match original data structure
  data_std <- pivot_longer(wide_std,
                           cols = -FULL_CU_IN,
                           names_to = "name",
                           values_to = "value"
  ) %>%
    mutate(
      RCP = as.numeric(str_extract(name, RCPs_grep)),
      period = as.numeric(period_pick),
      variable = str_remove(name, paste0("_(", RCPs_grep, ")$"))
    ) %>%
    select(-name) %>%
    pivot_wider(
      names_from = variable,
      values_from = value,
      names_prefix = paste0("std", "_" , indicator_pick$abbrev, "_")
    )
  
  return(data_std)
  
}

#------------- 3. Calculate standardized scores-------------------------------


periods <- c("3","4","5")

fwR_all_std <- fwR_all_flat %>%
  mutate(RCP = as.character(RCP),
         period = as.character(period))

for(i in 1:nrow(indicators)) {
  
  for(f in 1:length(periods)) {
    
    temp  <- standardize_spawning(fwR_all_flat, 
                                  indicator_pick = fwR_indicators[i,],
                                  period_pick = periods[f],
                                  use_gcm_range = T) %>%
      mutate(RCP = as.character(RCP),
             period = as.character(period))
    
    if(f == 1) {
      std_periods <- temp
    } else {
      std_periods <- bind_rows(std_periods, temp)
    }
    
  }
  fwR_all_std <- fwR_all_std %>%
    left_join(std_periods, join_by("FULL_CU_IN", "RCP", "period"))
  
}

## calculate an average standardized vulnerability score across indicators
fwR_all_std <- fwR_all_std %>%
  mutate(std_avg_wmean = rowMeans(across(starts_with("std") & ends_with("wmean")), na.rm =T),
         std_avg_wmean_qlow_gcm = rowMeans(across(starts_with("std") & ends_with("min_gcm")), na.rm =T),
         std_avg_wmean_qhigh_gcm = rowMeans(across(starts_with("std") & ends_with("max_gcm")), na.rm =T))


write.csv(fwR_all_std, file = file.path(paths$fw, paste0(today, "_fw_rearing_standardized.csv")), row.names = FALSE)




plot_std <- filter(fwR_all_std, period == "3", RCP =="45")

ggplot() +
  geom_point(data = plot_std, aes(x = Tw8proj_wmean, y = std_Tw8proj_wmean, color = CU_Species))






#species_choose <- c("Chinook")
#CU_exclude <- c("CK-7")

CVIS_spn <- CVIS_spn %>%
  filter(Species_simple %in% species_choose) %>%
  filter(!FULL_CU_IN %in% CU_exclude)

CVIS_dem <- CVIS_dem %>%
  filter(Species_simple %in% species_choose) %>%
  filter(!FULL_CU_IN %in% CU_exclude)

CVIS_migr <- CVIS_migr %>%
  filter(Species_simple %in% species_choose) %>%
  filter(!FULL_CU_IN %in% CU_exclude)

#--------------------- Summarize raw indicator values-------------------------


CVIS_all <- CVIS_spn %>%
  filter(Species_simple %in% species_choose) %>%
  filter(!FULL_CU_IN %in% CU_exclude) %>%
  left_join(select(CVIS_migr, cuid, MIG_SEN_len, MIG_EXP_T, MIG_EXP_Q), by = "cuid") %>%
  left_join(select(CVIS_dem, cuid, DEM_stat, DEM_nmat),  by = "cuid") %>%
  mutate(SPN_EXP_AVG = NA,
         SPN_SEN_AVG = NA,
         SPN_EXPxSEN = NA,
         MIG_EXP_AVG = NA,
         MIG_SEN_AVG = NA,
         MIG_EXPxSEN = NA)


#summary of indicators for a single CU
# 
# STD_temp <- STD_spn_long %>%
#   filter(cuid %in% c(310:313)) %>%
#   arrange(CU_NAME, value)
# 
# ggplot(STD_temp) +
#   geom_segment( aes(x=indicator, xend=indicator, y=0, yend=value), color="grey") +
#   geom_point( aes(x=indicator, y=value, color = CU_NAME), size=3) +
#   coord_flip()+
#   theme_minimal() + 
#   theme(
#     legend.position = "none",
#     panel.border = element_blank(),
#     panel.spacing = unit(0.1, "lines"),
#     strip.text.x = element_text(size = 8)
#   ) +
#   facet_wrap(~CU_NAME, ncol=1) +
#   xlab("") + 
#   ylab("")


#---------------------- Standardize indicator values---------------------------
# positive functions:  linear_std, logarithmic_std, exponential_std
# negative functions: decay_std, invlinear_std
# categorical functions: cat_std, step_std, enh_std

STD_spn <- CVIS_spn %>%
  filter(Species_simple %in% species_choose) %>%
  mutate(SPN_EXP_projT_9 = exponential_std(SPN_EXP_projT_9, lambda = 2, xmin = projT_min),    #low threshold score of 16 C
         SPN_EXP_rateT_9 = linear_std(SPN_EXP_rateT_9),     #low threshold score of minimum value
         SPN_EXP_augQ = invlinear_std(SPN_EXP_augQ, xmax = 0),    #low threshold score of minimum value
         SPN_EXP_winQ = linear_std(SPN_EXP_winQ, xmin = 0),
         SPN_EXP_peakQday = invlinear_std(SPN_EXP_peakQday, xmax = 0),
         SPN_SEN_CT_anad = linear_std(SPN_SEN_CT_anad, xmin = 0),
         SPN_SEN_dur = step_std(SPN_SEN_dur, x1 = 200, x2 = 300),
         SPN_ENM_fav_diff = invlinear_std(SPN_ENM_fav_diff, xmax = 0)) %>%
  mutate(SPN_EXP_AVG = rowMeans(select(.,contains("SPN_EXP"))),
         SPN_SEN_AVG = rowMeans(select(., contains("SPN_SEN"))))%>%
  mutate(SPN_EXPxSEN = SPN_EXP_AVG * SPN_SEN_AVG)

STD_migr <- CVIS_migr %>%
  filter(Species_simple %in% species_choose) %>%
  mutate(MIG_SEN_len = linear_std(MIG_SEN_len, xmin = 0),
         MIG_EXP_T = exponential_std(MIG_EXP_T, xmin = 16),    #low threshold score of 16 C
         MIG_EXP_Q = invlinear_std(MIG_EXP_Q, xmax = 0)) %>%
  mutate(MIG_EXP_AVG = rowMeans(select(.,contains("MIG_EXP"))),
         MIG_SEN_AVG = rowMeans(select(., contains("MIG_SEN"))))%>%
  mutate(MIG_EXPxSEN = MIG_EXP_AVG * MIG_SEN_AVG)

STD_dem <- CVIS_dem %>%
  #filter(Species_simple %in% species_choose) %>%
  mutate(DEM_stat = cat_std(DEM_stat),
         DEM_nmat = decay_std(DEM_nmat, lambda = 3, xmax = 10000),
         DEM_enhinf = enh_std(DEM_enhann, DEM_enhobj))

#combine standardized indicators
STD_all <- STD_spn %>%
  left_join(select(STD_migr, -c(CU_NAME, FULL_CU_IN, Species_simple)), by = "cuid") %>%
  left_join(select(STD_dem, c(cuid, DEM_stat, DEM_nmat)),  by = "cuid") %>%
  rename_with(~str_c("STD_", . )) %>%
  rename(cuid = STD_cuid)

#------------------create simulated indicators across range for plotting--------------------

SIM_all <- apply(select(CVIS_all,where(is.numeric)), 2, simulate_range) %>%
  as_tibble()

SIM_STD_all <- SIM_all %>%
  mutate(SPN_EXP_projT_9 = exponential_std(SPN_EXP_projT_9, lambda = 2, xmin = projT_min),    #low threshold score of 16 C
         SPN_EXP_rateT_9 = linear_std(SPN_EXP_rateT_9),    #low threshold score of minimum value
         SPN_EXP_augQ = invlinear_std(SPN_EXP_augQ, xmax = 0),    #low threshold score of minimum value
         SPN_EXP_winQ = linear_std(SPN_EXP_winQ, xmin = 0),
         SPN_EXP_peakQday = invlinear_std(SPN_EXP_peakQday, xmax = 0),
         SPN_SEN_CT_anad = linear_std(SPN_SEN_CT_anad, xmin = 0),
         SPN_SEN_dur = step_std(SPN_SEN_dur, x1 = 200, x2 = 300),
         SPN_ENM_fav_diff = invlinear_std(SPN_ENM_fav_diff, xmax = 0)) %>%
  mutate(SPN_EXP_AVG = rowMeans(select(.,contains("SPN_EXP"))),
         SPN_SEN_AVG = rowMeans(select(., contains("SPN_SEN"))))%>%
  mutate(SPN_EXPxSEN = SPN_EXP_AVG * SPN_SEN_AVG) %>%
  mutate(MIG_SEN_len = linear_std(MIG_SEN_len, xmin = 0),
         MIG_EXP_T = exponential_std(MIG_EXP_T, xmin = 16),    #low threshold score of 16 C
         MIG_EXP_Q = invlinear_std(MIG_EXP_Q, xmax = 0)) %>%
  mutate(MIG_EXP_AVG = rowMeans(select(.,contains("MIG_EXP"))),
         MIG_SEN_AVG = rowMeans(select(., contains("MIG_SEN"))))%>%
  mutate(MIG_EXPxSEN = MIG_EXP_AVG * MIG_SEN_AVG) %>%
  mutate(DEM_stat = NA,
         DEM_nmat = decay_std(DEM_nmat, lambda = 3, xmax = 10000),
         DEM_enhinf = NA) %>%
  rename_with(~str_c("STD_", . ))


SIM_STD_all <- bind_cols(SIM_all, SIM_STD_all)

#---------------------- Compare raw and standardized indicator values---------------------------

CVIS_STD_all <- CVIS_all %>%
  bind_cols(select(STD_all, -c(cuid, STD_CU_NAME, STD_Species_simple))) %>%
  left_join(select(cu_run, cuid, FAZ), join_by(cuid))

ggplot() +
  geom_point(data = CVIS_STD_all, aes(x = SPN_EXP_rateT_9, y = STD_SPN_EXP_rateT_9, color = Species_simple))

ggplot() +
  geom_point(data = CVIS_STD_all, aes(x = MIG_SEN_len, y = STD_MIG_SEN_len, color = Species_simple))

ggplot() +
  geom_point(data = CVIS_STD_all, aes(x = DEM_nmat, y = STD_DEM_nmat, color = Species_simple))




#----------------------- Summarize indicators across CUs------------------------

STD_spn_long <- pivot_longer(STD_spn, cols = c(SPN_EXP_projT_9, SPN_EXP_rateT_9, SPN_EXP_augQ, SPN_EXP_winQ,
                                                SPN_EXP_peakQday, SPN_SEN_CT_anad, SPN_SEN_dur),
             names_to = "indicator", values_to = "value") %>%
  group_by(CU_NAME)

STD_mig_long <- pivot_longer(STD_migr, cols = c(MIG_SEN_len, MIG_EXP_T, MIG_EXP_Q),
             names_to = "indicator", values_to = "value") %>%
  group_by(CU_NAME)

STD_dem_long <- pivot_longer(STD_dem, cols = c(DEM_stat, DEM_nmat),
             names_to = "indicator", values_to = "value")


# # summary of values for a single indicator

# Create a unique ordering of id by sp
# Create a unique ordering of id by sp
plot_data <- CVIS_dem

ordered_ids <- plot_data %>%
  arrange(Species_simple, CU_NAME) %>%
  distinct(CU_NAME) %>%
  pull(CU_NAME)

# Apply the ordering
plot_data$id <- factor(plot_data$CU_NAME, levels = ordered_ids)

plot_data <- filter(plot_data, Species_simple != "Pink")

ggplot(plot_data) +
  #geom_segment( aes(x=CU_NAME, xend=CU_NAME, y=0, yend=SPN_EXP_rateT_9), color="grey") +
  geom_point( aes(x=id, y=DEM_nmat, color = Species_simple), size=3 ) +
  coord_flip()+
  theme(
    panel.border = element_blank(),
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8),
  ) +
  xlab("") +
  ylab("Number of mature individuals")








# 
# 
# ggplot(CVIS_STD_all) +
#   geom_point(aes(x=SPN_EXP_rateT_9, y=STD_SPN_EXP_rateT_9), colour = "blue") +
#   geom_line(data = SIM_STD_all, aes(x=SPN_EXP_rateT_9, y=STD_SPN_EXP_rateT_9), linetype = 2) +
#   ylim(0,1) +
#   labs(x = "Rate of T increase (deg C per decade)", y = "Standardized rate of T increase") 
# 
# ggplot(CVIS_STD_all) +
#   geom_point(aes(x=SPN_EXP_augQ, y=STD_SPN_EXP_augQ), colour = "blue") +
#   geom_line(data = SIM_STD_all, aes(x=SPN_EXP_augQ, y=STD_SPN_EXP_augQ), linetype = 2) +
#   ylim(0,1) +
#   labs(x = "Change in summer flow", y = "Standardized") 
# 
# ggplot(CVIS_STD_all) +
#   geom_point(aes(x=SPN_SEN_CT_anad, y=STD_SPN_SEN_CT_anad), colour = "blue") +
#   geom_line(data = SIM_STD_all, aes(x=SPN_SEN_CT_anad, y=STD_SPN_SEN_CT_anad), linetype = 2) +
#   ylim(0,1) +
#   labs(x = "Cumulative threat score", y = "Standardized") 
