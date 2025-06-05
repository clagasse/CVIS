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
library(here)
setwd(here())
source(file.path(here(), "code", "0_setup.R"))

#scoring standardization functions
source(file.path(here(), "code", "4_scoring_utils.R"))

projT_min = 14

species_choose <- c("Chinook")
CU_exclude <- c("CK-7")

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
  filter(Species_simple %in% species_choose) %>%
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


# summary of values for a single indicator
ggplot(STD_spn) +
  geom_segment( aes(x=CU_NAME, xend=CU_NAME, y=0, yend=SPN_EXP_rateT_9), color="grey") +
  geom_point( aes(x=CU_NAME, y=SPN_EXP_rateT_9), size=3, color="#69b3a2" ) +
  coord_flip()+
  theme(
    legend.position = "none",
    panel.border = element_blank(),
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
  xlab("") +
  ylab("Rate of T increase") 



ggplot(CVIS_STD_all) +
  geom_point(aes(x=SPN_EXP_rateT_9, y=STD_SPN_EXP_rateT_9), colour = "blue") +
  geom_line(data = SIM_STD_all, aes(x=SPN_EXP_rateT_9, y=STD_SPN_EXP_rateT_9), linetype = 2) +
  ylim(0,1) +
  labs(x = "Rate of T increase (deg C per decade)", y = "Standardized rate of T increase") 

ggplot(CVIS_STD_all) +
  geom_point(aes(x=SPN_EXP_augQ, y=STD_SPN_EXP_augQ), colour = "blue") +
  geom_line(data = SIM_STD_all, aes(x=SPN_EXP_augQ, y=STD_SPN_EXP_augQ), linetype = 2) +
  ylim(0,1) +
  labs(x = "Change in summer flow", y = "Standardized") 

ggplot(CVIS_STD_all) +
  geom_point(aes(x=SPN_SEN_CT_anad, y=STD_SPN_SEN_CT_anad), colour = "blue") +
  geom_line(data = SIM_STD_all, aes(x=SPN_SEN_CT_anad, y=STD_SPN_SEN_CT_anad), linetype = 2) +
  ylim(0,1) +
  labs(x = "Cumulative threat score", y = "Standardized") 
