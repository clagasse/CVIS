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

#scoring standardization functions
source(file.path(code_root, "4_scoring_utils.R"))


#--------------------- Summarize raw indicator values-------------------------

CVIS_all <- CVIS_spn %>%
  left_join(select(CVIS_migr, cuid, FW_MIG_SEN_len, FW_MIG_EXP_T, FW_MIG_EXP_Q), by = "cuid") %>%
  left_join(select(CVIS_dem, cuid, DEM_stat, DEM_nmat),  by = "cuid") %>%
  mutate(FW_SPN_EXP_AVG = NA,
         FW_SPN_SEN_AVG = NA,
         FW_SPN_EXPxSEN = NA,
         FW_MIG_EXP_AVG = NA,
         FW_MIG_SEN_AVG = NA,
         FW_MIG_EXPxSEN = NA)


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
  mutate(FW_SPN_EXP_projT_9 = exponential_std(FW_SPN_EXP_projT_9, lambda = 3, xmin = 16),    #low threshold score of 16 C
         FW_SPN_EXP_rateT_9 = linear_std(FW_SPN_EXP_rateT_9),     #low threshold score of minimum value
         FW_SPN_EXP_augQ = invlinear_std(FW_SPN_EXP_augQ),    #low threshold score of minimum value
         FW_SPN_EXP_winQ = linear_std(FW_SPN_EXP_winQ, xmin = 0),
         FW_SPN_EXP_peakQday = invlinear_std(FW_SPN_EXP_peakQday, xmax = 0),
         FW_SPN_SEN_CT_anad = linear_std(FW_SPN_SEN_CT_anad, xmin = 0),
         FW_SPN_SEN_dur = step_std(FW_SPN_SEN_dur, x1 = 200, x2 = 300),
         FW_SPN_ENM_fav_diff = invlinear_std(FW_SPN_ENM_fav_diff, xmax = 0)) %>%
  mutate(FW_SPN_EXP_AVG = rowMeans(select(.,contains("SPN_EXP"))),
         FW_SPN_SEN_AVG = rowMeans(select(., contains("SPN_SEN"))))%>%
  mutate(FW_SPN_EXPxSEN = FW_SPN_EXP_AVG * FW_SPN_SEN_AVG)

STD_migr <- CVIS_migr %>%
  mutate(FW_MIG_SEN_len = linear_std(FW_MIG_SEN_len, xmin = 0),
         FW_MIG_EXP_T = exponential_std(FW_MIG_EXP_T, xmin = 16),    #low threshold score of 16 C
         FW_MIG_EXP_Q = invlinear_std(FW_MIG_EXP_Q, xmax = 0)) %>%
  mutate(FW_MIG_EXP_AVG = rowMeans(select(.,contains("MIG_EXP"))),
         FW_MIG_SEN_AVG = rowMeans(select(., contains("MIG_SEN"))))%>%
  mutate(FW_MIG_EXPxSEN = FW_MIG_EXP_AVG * FW_MIG_SEN_AVG)

STD_dem <- CVIS_dem %>%
  mutate(DEM_stat = cat_std(DEM_stat),
         DEM_nmat = decay_std(DEM_nmat, lambda = 3, xmax = 10000),
         DEM_enhinf = enh_std(DEM_enhann, DEM_enhobj))

#combine standardized indicators
STD_all <- STD_spn %>%
  left_join(select(STD_migr, -c(CU_NAME, FULL_CU_IN, Species_simple)), by = "cuid") %>%
  left_join(select(STD_dem, c(cuid, DEM_stat, DEM_nmat)),  by = "cuid") %>%
  rename_with(~str_c("STD_", . )) %>%
  rename(cuid = STD_cuid)



#---------------------- Compare raw and standardized indicator values---------------------------

CVIS_STD_all <- CVIS_all %>%
  bind_cols(select(STD_all, -c(cuid, STD_CU_NAME, STD_Species_simple))) 

ggplot() +
  geom_point(data = CVIS_STD_all, aes(x = FW_SPN_EXP_rateT_9, y = STD_FW_SPN_EXP_rateT_9, color = Species_simple))

ggplot() +
  geom_point(data = CVIS_STD_all, aes(x = FW_MIG_SEN_len, y = STD_FW_MIG_SEN_len, color = Species_simple))

ggplot() +
  geom_point(data = CVIS_STD_all, aes(x = DEM_nmat, y = STD_DEM_nmat, color = Species_simple))



#----------------------- Summarize indicators across CUs------------------------

STD_spn_long <- pivot_longer(STD_spn, cols = c(FW_SPN_EXP_projT_9, FW_SPN_EXP_rateT_9, FW_SPN_EXP_augQ, FW_SPN_EXP_winQ,
                                                FW_SPN_EXP_peakQday, FW_SPN_SEN_CT_anad, FW_SPN_SEN_dur),
             names_to = "indicator", values_to = "value") %>%
  group_by(CU_NAME)


# summary of values for a single indicator
ggplot(STD_spn) +
  geom_segment( aes(x=CU_NAME, xend=CU_NAME, y=0, yend=FW_SPN_EXP_rateT_9), color="grey") +
  geom_point( aes(x=CU_NAME, y=FW_SPN_EXP_rateT_9), size=3, color="#69b3a2" ) +
  coord_flip()+
  theme(
    legend.position = "none",
    panel.border = element_blank(),
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
  xlab("") +
  ylab("Rate of T increase") 

#summary of indicators for a single CU

STD_temp <- STD_spn_long %>%
  filter(cuid %in% c(310:313)) %>%
  arrange(CU_NAME, value)

ggplot(STD_temp) +
  geom_segment( aes(x=indicator, xend=indicator, y=0, yend=value), color="grey") +
  geom_point( aes(x=indicator, y=value, color = CU_NAME), size=3) +
  coord_flip()+
  theme_minimal() + 
  theme(
    legend.position = "none",
    panel.border = element_blank(),
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
  facet_wrap(~CU_NAME, ncol=1) +
  xlab("") + 
  ylab("")
