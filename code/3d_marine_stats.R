##### Marine statistics and plots

library(here)
setwd(here())
source(file.path(here(), "code", "0_setup.R"))

library(ggdist)
library(pacea)


#load historical BCCM SSTs from Pacea
BCCMpacea_SST <- bccm_surface_temperature()

#get hotssea SST
hotssea_SST <- hotssea_surface_temperature_mean() %>%
  pivot_longer(cols = c(`1980_1`:`2018_12`),
               names_to = c("year", "month"),
               names_sep = "_",
               values_to = "sst")


marine_files <- list.files(file.path(paths$climate, "Standardized_Marine_data"))

## read in processed shp files
MAZ     <- st_read(file.path(spatial_dat, "MAZ", "MAZ_Final.shp"))
#  note: _cropped are polygon grids,  _sub are points
SSC_SSS <- st_read(file.path(paths$climate, "Standardized_Marine_data", "SSC_SSS_cropped.shp")) 
SSC_SST <- st_read(file.path(paths$climate, "Standardized_Marine_data", "SSC_SST_cropped.shp"))

# BCCM_SSS <- st_read(file.path(paths$climate, "Standardized_Marine_data", "BCCM_SSS_cropped.shp"))
# BCCM_SST <- st_read(file.path(paths$climate, "Standardized_Marine_data", "BCCM_SST_cropped.shp"))

BCCM_SST_sub<-read_sf(file.path(paths$climate, "Standardized_Marine_data/BCCM_SST_sub.shp"))
BCCM_SSS_sub<-read_sf(file.path(paths$climate, "Standardized_Marine_data/BCCM_SSS_sub.shp"))
BCCM_SSPH_sub<-read_sf(file.path(paths$climate, "Standardized_Marine_data/BCCM_SSPH_sub.shp"))
# NEP_SST_sub<- read_sf( file.path(paths$climate, "Standardized_Marine_data/NEP_SST_sub.shp"))
# NEP_SSS_sub<- read_sf( file.path(paths$climate, "Standardized_Marine_data/NEP_SSS_sub.shp"))
# NEP_SSPH_sub<- read_sf( file.path(paths$climate, "Standardized_Marine_data/NEP_SSPH_sub.shp"))
SSC_SST_sub<- read_sf( file.path(paths$climate, "Standardized_Marine_data/SSC_SST_sub.shp"))
SSC_SSS_sub<- read_sf( file.path(paths$climate, "Standardized_Marine_data/SSC_SSS_sub.shp"))
CI_points_sub<-read_sf(file.path(paths$climate, "Standardized_Marine_data/CI_points_sub.shp"))


CMIP5_SST_sub<- read_sf( file.path(paths$climate, "Standardized_Marine_data/CMIP5_SST_sub.shp"))


# assign MAZ to each row in model output
# BCCM_SST_MAZ <- assign_points(BCCM_SST_sub, MAZ, var = "MAZ_Acrony")
# SSC_SST_MAZ <- assign_points(SSC_SST_sub, MAZ)

# BCCM_SSS_MAZ <- assign_points(BCCM_SSS_sub, MAZ)
# SSC_SSS_MAZ <- assign_points(SSC_SSS_sub, MAZ)
# 
# BCCM_SSPH_MAZ <- assign_points(BCCM_SSPH_sub, MAZ)

BCCMpacea_SST <- assign_points(BCCMpacea_SST, MAZ, var = "MAZ_Acrony")

BCCM_SST_long <- BCCM_SST_sub %>%
  pivot_longer(
    cols = c(contains("H"), contains("45"),contains("85")),
    names_to = c("scenario", "month"),
    names_pattern = 'SST_([A-Za-z0-9]+)_([0-9]+)',
    values_to = "value"
  ) %>%
  mutate(model = "BCCM",
         month = as.numeric(month))

BCCMpacea_SST_long <- BCCMpacea_SST_MAZ %>%
  pivot_longer(
    cols = -c("geometry", "MAZ_Acrony"),
    names_to = c("year","month"),
    names_pattern = '([A-Za-z0-9]+)_([0-9]+)',
    values_to = "value"
  ) %>%
  mutate(model = "BCCM_pacea",
         month = as.numeric(month))

SSC_SST_long <- SSC_SST_sub %>%
  pivot_longer(
    cols = c(contains("H"), contains("45"),contains("85")),
    names_to = c("scenario", "month"),
    names_pattern = 'SST_([A-Za-z0-9]+)_([0-9]+)',
    values_to = "value"
  ) %>%
  mutate(model = "SSC",
         month = as.numeric(month))

BCCM_SSS_long <- BCCM_SSS_sub %>%
  pivot_longer(
    cols = c(contains("H"), contains("45"),contains("85")),
    names_to = c("scenario", "month"),
    names_pattern = 'SSS_([A-Za-z0-9]+)_([0-9]+)',
    values_to = "value"
  ) %>%
  mutate(model = "BCCM",
         month = as.numeric(month))

SSC_SSS_long <- SSC_SSS_sub %>%
  pivot_longer(
    cols = c(contains("H"), contains("45"),contains("85")),
    names_to = c("scenario", "month"),
    names_pattern = 'SSS_([A-Za-z0-9]+)_([0-9]+)',
    values_to = "value"
  ) %>%
  mutate(model = "SSC",
         month = as.numeric(month))

BCCM_SSPH_long <- BCCM_SSPH_sub %>%
  pivot_longer(
    cols = c(contains("H"), contains("45"),contains("85")),
    names_to = c("scenario", "month"),
    names_pattern = 'SSPH_([A-Za-z0-9]+)_([0-9]+)',
    values_to = "value"
  ) %>%
  mutate(model = "BCCM",
         month = as.numeric(month))

CI_long <- CI_points_sub %>%
  pivot_longer(
    cols = c(contains("Cumul_Im")),
    names_to = c("habitat"),
    values_to = "value"
  ) %>%
  mutate(habitat = case_when(
    habitat == "Cumul_Impa" ~ "All",
    habitat == "Cumul_Im_1" ~ "Shallow_pelagic",
    habitat == "Cumul_Im_2" ~ "Deep_pelagic",
    habitat == "Cumul_Im_3" ~ "Benthic",
    habitat == "Cumul_Im_4" ~ "Eelgrass",
    habitat == "Cumul_Im_5" ~ "Sponge_reef",
    habitat == "Cumul_Im_6" ~ "Kelp_forest",
  ))


ROM_SST <- bind_rows(BCCM_SST_long, SSC_SST_long) %>%
  filter(MAZ_Acrony != "SFj")

ROM_SSS <- bind_rows(BCCM_SSS_long, SSC_SSS_long) %>%
  filter(MAZ_Acrony != "SFj")

ROM_SST_stars <- ROM_SST %>%
  st_as_stars()


### Summarize results by MAZ and scenario

ROM_SST_summary <- ROM_SST %>%
  as_tibble() %>%
  group_by(scenario, MAZ_Acrony, month) %>%
  summarize(SST_mean = mean(value, na.rm = T),
            SST_05 = quantile(value, 0.05, na.rm = T),
            SST_95 = quantile(value, 0.95, na.rm = T))

ROM_SSS_summary <- ROM_SSS %>%
  as_tibble() %>%
  group_by(scenario, MAZ_Acrony, month) %>%
  summarize(SSS_mean = mean(value, na.rm = T),
            SSS_05 = quantile(value, 0.05, na.rm = T),
            SSS_95 = quantile(value, 0.95, na.rm = T))

BCCM_SSPH_summary <- BCCM_SSPH_long %>%
  as_tibble() %>%
  group_by(scenario, MAZ_Acrony, month) %>%
  summarize(SSPH_mean = mean(value, na.rm = T),
            SSPH_05 = quantile(value, 0.05, na.rm = T),
            SSPH_95 = quantile(value, 0.95, na.rm = T))

CI_summary <- CI_long %>%
  as_tibble() %>%
  group_by(MAZ_Acrony, habitat) %>%
  summarize(CI_mean = mean(value, na.rm = T)) %>%
  pivot_wider(
    names_from = habitat,
    values_from = c(CI_mean)
  )

kable(CI_summary, format = "html", digits = 2) %>%
  kable_styling("striped", full_width = F) %>%
  save_kable(file = "CI_summary.html")

##### PLOTS


ggplot(ROM_SST_summary, aes(x=month, y = SST_mean, ymin = SST_05, ymax = SST_95,
                             group = scenario, color = scenario)) +
  geom_vline(xintercept = 4, linetype = "dashed") +
  geom_vline(xintercept = 7, linetype = "dashed") +
  geom_vline(xintercept = 10, linetype = "dashed") +
  geom_vline(xintercept = 1, linetype = "dashed") +
  geom_ribbon(alpha = 0.2) +
  geom_line(linewidth = 1.4) +
  facet_wrap(~MAZ_Acrony) +
  theme_minimal()

ggplot(ROM_SSS_summary, aes(x=month, y = SSS_mean, ymin = SSS_05, ymax = SSS_95,
                            group = scenario, color = scenario)) +
  geom_ribbon(alpha = 0.2) +
  geom_line(linewidth = 1.4) +
  geom_vline(xintercept = 4, linetype = "dashed") +
  geom_vline(xintercept = 7, linetype = "dashed") +
  geom_vline(xintercept = 10, linetype = "dashed") +
  geom_vline(xintercept = 1, linetype = "dashed") +
  facet_wrap(~MAZ_Acrony, scales = "free_y") +
  theme_minimal()

ggplot(BCCM_SSPH_summary, aes(x=month, y = SSPH_mean, ymin = SSPH_05, ymax = SSPH_95,
                            group = scenario, color = scenario)) +
  geom_ribbon(alpha = 0.2) +
  geom_line(linewidth = 1.4) +
  geom_vline(xintercept = 4, linetype = "dashed") +
  geom_vline(xintercept = 7, linetype = "dashed") +
  geom_vline(xintercept = 10, linetype = "dashed") +
  geom_vline(xintercept = 1, linetype = "dashed") +
  facet_wrap(~MAZ_Acrony, scales = "free_y") +
  theme_minimal()


ggplot(CI_points_sub, aes(x=MAZ_Acrony, y = Cumul_Impa, fill = MAZ_Acrony)) +
  geom_violin(draw_quantiles = c(0.5), adjust = 5) +
  theme_minimal()
  
ggplot(CI_long, aes(x=MAZ_Acrony, y = value, fill = MAZ_Acrony)) +
  geom_boxplot() +
  theme_minimal() +
  facet_wrap(~habitat)





#-------------------- Example exposure for a CU------------------

cu_marine <- cu_timing %>%
  filter(culabel %in% c("West Vancouver Island-Nootka and Kyuquot (Fall x-1)", 
                     "Lower Fraser River (Fall 4-1)",
                     "Fraser River (odd)")) %>%
  select(species, cuid, culabel, contains("oe")) %>%
  mutate(oe_start_m = month(as.Date(oe_start)),
         oe_end_m = month(as.Date(oe_end)),
         oe_peak_m = month(as.Date(oe_peak))) %>%
  mutate(MAZ = c("GStr", "GStr", "WVI"))

cu_marine <- cu_timing_Fr %>%
  select(species, cuid, culabel, contains("oe")) %>%
  mutate(oe_start_m = month(as.Date(oe_start)),
         oe_end_m = month(as.Date(oe_end)),
         oe_peak_m = month(as.Date(oe_peak))) %>%
  mutate(MAZ = c("GStr"))


ns_stats <- tibble(select(cu_marine, cuid, culabel, species)) %>% #,select(as_tibble(fw_amod[1:n.CUs,]), Tw8_0_00_0, Tw8_9_45_3) %>%
  mutate(oe_start_m = NA,   #
         SST_H = NA,  
         SST_45 = NA,  
         SST_85 = NA,  
         delta_SST_45 = NA,
         delta_SST_85 = NA,
         SSS_H = NA,  
         SSS_45 = NA,  
         SSS_85 = NA,
         delta_SSS_45 = NA,
         delta_SSS_85 = NA)


for(i in 1:nrow(cu_marine)) {
  
  ns_stats$oe_start_m[i] <- cu_marine[i, "oe_start_m"]
  
  cu_SST_summary <- ROM_SST_summary %>%
    filter(MAZ_Acrony == cu_marine[i, "MAZ"],
           month >= ns_stats$oe_start_m[i] & month < ns_stats$oe_start_m[i] + 4)
  
  cu_SSS_summary <- ROM_SSS_summary %>%
    filter(MAZ_Acrony == cu_marine[i, "MAZ"],
           month >= ns_stats$oe_start_m[i] & month < ns_stats$oe_start_m[i] + 4)
  
  cu_SSPH_summary <- BCCM_SSPH_summary %>%
    filter(MAZ_Acrony == cu_marine[i, "MAZ"],
           month >= ns_stats$oe_start_m[i] & month < ns_stats$oe_start_m[i] + 4)
  
  # cu_SST <- ROM_SST %>%
  #   filter(MAZ_Acrony == cu_marine[i, "MAZ"],
  #          month >= ns_stats$oe_start_m[i] & month < ns_stats$oe_start_m[i] + 4) %>%
  #   group_by(scenario, MAZ_Acrony, geometry) %>%
  #   summarize(value = mean(value))
  # 
  # cu_SSS <- ROM_SSS %>%
  #   filter(MAZ_Acrony == cu_marine[i, "MAZ"],
  #          month >= ns_stats$oe_start_m[i] & month < ns_stats$oe_start_m[i] + 4) %>%
  #   group_by(scenario, MAZ_Acrony, geometry) %>%
  #   summarize(value = mean(value))

  
  ns_stats$SST_H[i] <- mean(cu_SST_summary$SST_mean[cu_SST_summary$scenario == "H"], na.rm = T)
  ns_stats$SST_45[i] <- mean(cu_SST_summary$SST_mean[cu_SST_summary$scenario == "45"], na.rm = T)
  ns_stats$SST_85[i] <- mean(cu_SST_summary$SST_mean[cu_SST_summary$scenario == "85"], na.rm = T)
  
  ns_stats$delta_SST_45[i] <- ns_stats$SST_45[i] - ns_stats$SST_H[i]
  ns_stats$delta_SST_85[i] <- ns_stats$SST_85[i] - ns_stats$SST_H[i]
  
  ns_stats$z_SST_45[i] <- (mean(cu_SST$value[cu_SST$scenario == "45"], na.rm = T) - mean(cu_SST$value[cu_SST$scenario == "H"], na.rm = T)) /
                             sd(cu_SST$value[cu_SST$scenario == "H"], na.rm = T)
  ns_stats$z_SST_85[i] <- (mean(cu_SST$value[cu_SST$scenario == "85"], na.rm = T) - mean(cu_SST$value[cu_SST$scenario == "H"], na.rm = T)) /
    sd(cu_SST$value[cu_SST$scenario == "H"], na.rm = T)
  
  ns_stats$SSS_H[i] <- mean(cu_SSS_summary$SSS_mean[cu_SSS_summary$scenario == "H"], na.rm = T)
  ns_stats$SSS_45[i] <- mean(cu_SSS_summary$SSS_mean[cu_SSS_summary$scenario == "45"], na.rm = T)
  ns_stats$SSS_85[i] <- mean(cu_SSS_summary$SSS_mean[cu_SSS_summary$scenario == "85"], na.rm = T)
  
  ns_stats$delta_SSS_45[i] <- ns_stats$SSS_45[i] - ns_stats$SSS_H[i]
  ns_stats$delta_SSS_85[i] <- ns_stats$SSS_85[i] - ns_stats$SSS_H[i]
  
  ns_stats$z_SSS_45[i] <- (mean(cu_SSS$value[cu_SSS$scenario == "45"], na.rm = T) - mean(cu_SSS$value[cu_SSS$scenario == "H"], na.rm = T)) /
    sd(cu_SSS$value[cu_SSS$scenario == "H"], na.rm = T)
  ns_stats$z_SSS_85[i] <- (mean(cu_SSS$value[cu_SSS$scenario == "85"], na.rm = T) - mean(cu_SSS$value[cu_SSS$scenario == "H"], na.rm = T)) /
    sd(cu_SSS$value[cu_SSS$scenario == "H"], na.rm = T)
  
}

cu_SSS_diff <- pivot_wider(cu_SSS,
  names_from = scenario,
  values_from = value) %>%
  mutate(
    delta_SSS_45 = `45` - H,
    delta_SSS_85 = `85` - H
  )

cu_SST_diff <- pivot_wider(cu_SST,
  names_from = scenario,
  values_from = value) %>%
  mutate(
    delta_SST_45 = `45` - H,
    delta_SST_85 = `85` - H
  )

ggplot(cu_SST_diff) +
  geom_sf(aes(colour = delta_SST_45)) +
  scale_color_viridis_c() +
  theme_minimal()

ggplot(cu_SSS_diff) +
  geom_sf(aes(colour = delta_SSS_45)) +
  scale_color_viridis_c() +
  theme_minimal()



