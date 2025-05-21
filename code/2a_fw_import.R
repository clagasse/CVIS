###############################################################################
#
# 2a_fw_import.R
#
# This code reads in a number of spatial datasets related to salmon distribution
# in the **Fraser region**.
#
###############################################################################

# If encounter the error: 
# Error in wk_handle.wk_wkb(wkb, s2_geography_writer(oriented = oriented,  : 
# Loop 0 is not valid: Edge 2607 has duplicate vertex with edge 2625
# Then run:
#sf_use_s2(FALSE)

library(here)
setwd(here())
source(file.path(here(), "code", "0_setup.R"))


#--------------Load BC FISH PASS stream accessibility and linear habitat model------------------

#there are two main stream network objects
#bcfpc - fish accessibility and linear habitat model for all streams in the Fraser basin
#bcfpa - same for only accessible modelled and observed streams in the Fraser basin

# all streams in Fr basin
#load(file.path(paths$spatial, "BCFishpass", "BCFP_combined_Fr.Rds"))
# accessible streams only
load(file.path(paths$fw, "BCFP_combined_accessible_Fr.Rds"))


#-------------------Conservation Unit boundaries for Fraser CUs ----------------

cu_boundary <- st_read(file.path(paths$spatial, "CU_boundaries", "fraser_cus.shp")) %>%
  st_make_valid() %>%
  st_transform(crs = 3005)  %>%  #crs 3005 is NAD83/BC Albers
  left_join(select(cu_Fr, cuid, FULL_CU_IN, spp), 
            join_by(CUID == cuid))


#------------------------Load temperature, flow and cumulative threat models----

#load stream network model outputs
# these are all on the same stream network as bcfpa - accessible bc fish pass stream segments

load(file.path(paths$fw, "fw_models_T_Q_CT.Rds"))

# load statistical model projections of August flows for flow stations
load(file.path(paths$fw,  "Statistical_flow_projections.Rds"))

#load flow stations spatial objects
stations_stats <- read.csv(file.path(paths$climate, "Ruzzante_low_flows", "stations_performance.csv"))
#watershed hydrologic regimes
watershed_flow <- st_read(file.path(paths$climate, "Ruzzante_low_flows", "watersheds.gpkg")) %>%
  left_join(select(stations_stats, ID, regime), by = c("ID" = "ID")) %>%
  mutate(regime = as.factor(regime))

stations_flow <- st_read(file.path(paths$climate, "Ruzzante_low_flows", "stations.gpkg"))


## Load ENM

load(file.path(paths$fw, "ENM_all_sp.Rds"))




#---------------------------Load NUSEDS salmon spawner locations----------------

##version from FIA. Usage column added by Michael Arbeider
nuseds_Fr <- read_csv(file.path(salmon_dat, "NuSEDS_CU_System_sites_202406.csv")) %>%
  st_as_sf(coords = c("X_LONGT", "Y_LAT"), crs = 4269) %>%
  filter(USAGE != "REMOVE")

#  field descriptions
# n = number of surveys that were not “UNKNOWN” or “NOT INSPECTED”, i.e. they were inspected but sometimes only PRESENSE was recorded and not an abundance.
# last.year = last year when the system was surveyed
# first.year = first year when the system was surveyed
# max.count = the largest count of spawners in NuSEDs
# ave.count = the mean of all non-NA counts in NuSEDs
# min.count = the minimum

#usage criteria for nuseds file
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



#--------------------- Calculate indicators -----------------------------------

#Aug temperature rate of change
fw_amod$SPN_EXP_rateT_9 <- (fw_amod$Tw8_9_45_3 - fw_amod$Tw8_0_00_1) / tspan
fw_amod$SPN_EXP_rateT_1 <- (fw_amod$Tw8_1_45_3 - fw_amod$Tw8_0_00_1) / tspan
fw_amod$SPN_EXP_rateT_2 <- (fw_amod$Tw8_2_45_3 - fw_amod$Tw8_0_00_1) / tspan
fw_amod$SPN_EXP_rateT_3 <- (fw_amod$Tw8_3_45_3 - fw_amod$Tw8_0_00_1) / tspan
fw_amod$SPN_EXP_rateT_4 <- (fw_amod$Tw8_4_45_3 - fw_amod$Tw8_0_00_1) / tspan
fw_amod$SPN_EXP_rateT_5 <- (fw_amod$Tw8_5_45_3 - fw_amod$Tw8_0_00_1) / tspan
fw_amod$SPN_EXP_rateT_6 <- (fw_amod$Tw8_6_45_3 - fw_amod$Tw8_0_00_1) / tspan

#Projected temperature
fw_amod$SPN_EXP_projT_9 <- fw_amod$Tw8_9_45_3
fw_amod$SPN_EXP_projT_1 <- fw_amod$Tw8_1_45_3
fw_amod$SPN_EXP_projT_2 <- fw_amod$Tw8_2_45_3
fw_amod$SPN_EXP_projT_3 <- fw_amod$Tw8_3_45_3
fw_amod$SPN_EXP_projT_4 <- fw_amod$Tw8_4_45_3
fw_amod$SPN_EXP_projT_5 <- fw_amod$Tw8_5_45_3
fw_amod$SPN_EXP_projT_6 <- fw_amod$Tw8_6_45_3

#mean historic winter flow
fw_amod$mean_flow_m3s_win_1 <- (fw_amod$mean_flow_m3s_11_1 + fw_amod$mean_flow_m3s_12_1 +
                                  fw_amod$mean_flow_m3s_1_1 + fw_amod$mean_flow_m3s_2_1) / 4
#mean projected winter flow
fw_amod$mean_win_40 <- (fw_amod$mean_11_40 + fw_amod$mean_12_40 +
                          fw_amod$mean_1_40 + fw_amod$mean_2_40) / 4

#August min flow and winter max flows as %mad historic and future
fw_amod$MADprop_8_hist  <- fw_amod$mean_flow_m3s_8_1 / fw_amod$mean_flow_m3s_17_1
fw_amod$MADprop_11_hist <- fw_amod$mean_flow_m3s_11_1 / fw_amod$mean_flow_m3s_17_1
fw_amod$MADprop_12_hist <- fw_amod$mean_flow_m3s_12_1 / fw_amod$mean_flow_m3s_17_1
fw_amod$MADprop_1_hist  <- fw_amod$mean_flow_m3s_1_1 / fw_amod$mean_flow_m3s_17_1
fw_amod$MADprop_2_hist  <- fw_amod$mean_flow_m3s_2_1 / fw_amod$mean_flow_m3s_17_1
fw_amod$MADprop_win_hist <-fw_amod$mean_flow_m3s_win_1 / fw_amod$mean_flow_m3s_17_1

fw_amod$MADprop_8_proj  <- fw_amod$mean_8_40 / fw_amod$mean_flow_m3s_17_1
fw_amod$MADprop_11_proj <- fw_amod$mean_11_40 / fw_amod$mean_flow_m3s_17_1
fw_amod$MADprop_12_proj <- fw_amod$mean_12_40 / fw_amod$mean_flow_m3s_17_1
fw_amod$MADprop_1_proj  <- fw_amod$mean_1_40 / fw_amod$mean_flow_m3s_17_1
fw_amod$MADprop_2_proj  <- fw_amod$mean_2_40 / fw_amod$mean_flow_m3s_17_1
fw_amod$MADprop_win_proj <- fw_amod$mean_win_40 / fw_amod$mean_flow_m3s_17_1

fw_amod$SPN_EXP_winQ <- fw_amod$MADprop_win_proj - fw_amod$MADprop_win_hist
fw_amod$SPN_EXP_augQ <- fw_amod$MADprop_8_proj - fw_amod$MADprop_8_hist

nuseds_matches <- select(as_tibble(fw_amod), LINEAR_FEATURE_ID, FWA_WATERSHED_CODE) %>%
  left_join(select(as_tibble(nuseds_Fr), FWA_WATERSHED_CDE, SPECIES_LOOKUP),
            join_by(FWA_WATERSHED_CODE == FWA_WATERSHED_CDE), multiple = "all") %>%
  distinct() %>%
  group_by(FWA_WATERSHED_CODE) %>%
  summarise(nuseds_sp = toString(unique(SPECIES_LOOKUP))) %>%
  mutate(nuseds_sp = map_chr(str_split(nuseds_sp, ", "), ~str_c(str_sort(.x), collapse = ","))) %>%
  mutate(nuseds_sp = if_else(str_detect(nuseds_sp, "NA"), NA, nuseds_sp))

fw_amod <- fw_amod %>%
  left_join(nuseds_matches, join_by(FWA_WATERSHED_CODE))



  
  

  

  

#----------------------PCIC grid points and polygon----------------------------
# Read in PCIC grid
grid_points <- read.csv(here("processed_data", "freshwater", "PCIC-grid-points_bccoast.csv")) 

# Convert grid points to spatial object 
grid_points <- st_as_sf(grid_points, coords = c("lon", "lat"), crs = 4269) %>%
  st_transform(4269)

#import polygon grid - see 2x_fw_create_inputs_grid.R file
grid_polys <- readRDS(file = here("processed_data", "freshwater", "grid_polys_fw.rds")) %>%
  st_transform(4269)

#subset Fraser basin
pick_Fr <- lengths(st_intersects(grid_polys, Fr_basin)) > 0
grid_polys <- grid_polys[pick_Fr,]

pick_Fr <- lengths(st_intersects(grid_points, grid_polys)) > 0 
grid_points <- grid_points[pick_Fr,]


#--------------------- Load PCIC model projections------------------------------

# see 2x_FW_PCIC_process.R for creation of daily and monthly netcdf files
#currently using RCP 4.5 ensemble models
PCIC_day <- read_mdim(file.path(paths$climate, "PCIC_processed", "PCIC_daily.nc"))
PCIC_month <- read_mdim(file.path(paths$climate, "PCIC_processed", "PCIC_monthly.nc"))

# #get PCIC file names in directory
# PCIC_files <- list.files(file.path(paths$climate, "PCIC_indicators"))
# PCIC_files <- PCIC_files[grep(".aux.xml", PCIC_files, invert=TRUE)]
# 
# #load selected indicators for available time periods and combine into a star object
# PCIC_file_choose <- PCIC_indies_pick
# for(i in 1:length(PCIC_indies_pick)){
#   pick_files <- PCIC_files[grepl(PCIC_file_choose[i], PCIC_files)]
# 
#   for(p in 1:length(t_periods)) {
#     temp_star <- read_ncdf(file.path(paths$climate, "PCIC_indicators", pick_files[p]))
# 
#     if(p == 1)  ind_star <- temp_star
#     else if(p > 1)  ind_star <- c(ind_star, temp_star, nms = t_periods)
#     rm(temp_star)
#   }
# 
#   if(i == 1) PCIC_indies <- ind_star
#   else if(i > 1) PCIC_indies <- c(PCIC_indies, ind_star)
# }
# 
# PCIC_file_choose <- PCIC_day_files
# for(i in 1:length(PCIC_file_choose)){
#   pick_files <- PCIC_files[grepl(PCIC_file_choose[i], PCIC_files)]
# 
#   for(p in 1:length(t_periods)) {
#     temp_star <- read_ncdf(file.path(paths$climate, "PCIC_indicators", pick_files[p]))
# 
#     if(p == 1)  ind_star <- temp_star
#     else if(p > 1)  ind_star <- c(ind_star, temp_star, nms = t_periods)
#     rm(temp_star)
#   }
# 
#   if(i == 1) PCIC_day <- ind_star
#   else if(i > 1) PCIC_day <- c(PCIC_day, ind_star)
# }
# 
# PCIC_month_files <- PCIC_files[grep("Month", PCIC_files)]
# PCIC_file_choose <- PCIC_month_files
# for(i in 1:length(PCIC_file_choose)){
#   pick_files <- PCIC_files[grepl(PCIC_file_choose[i], PCIC_files)]
# 
#   for(p in 1:2) {
#     temp_star <- read_ncdf(file.path(paths$climate, "PCIC_indicators", PCIC_month_files[p]))
# 
#     if(p == 1)  ind_star <- temp_star
#     else if(p > 1)  ind_star <- c(ind_star, temp_star, nms = t_periods)
#     rm(temp_star)
#   }
# 
#   if(i == 1) PCIC_month <- ind_star
#   else if(i > 1) PCIC_month <- c(PCIC_month, ind_star)
# }
# 
# st_crs(PCIC_indies) <- 4269 #change CRS to NAD83/Albers from default of WGS84
st_crs(PCIC_day) <- 4269 #change CRS to NAD83/Albers from default of WGS84
st_crs(PCIC_month) <- 4269 #change CRS to NAD83/Albers from default of WGS84

#crop to Fraser basin
#PCIC_indies <- st_crop(PCIC_indies, grid_polys)
#PCIC_day <- st_crop(PCIC_day, grid_polys)
#PCIC_month <- st_crop(PCIC_month, grid_polys)


#------------------------- Save to Rdata objects-------------------------   

save(PCIC_month, PCIC_day, grid_points, grid_polys, 
     Fr_basin, shoreline, cu_boundary, FAZ, FAZ_Fr,
     FWA_Fr_high, bcfp, bcfp_Fr, bcfp_Fr_high, 
     tscapes, T7DECM, flow_fwa, flow_hist_fwa, fw_stress,
     reaches_ENM_ck, reaches_ENM_co, reaches_ENM_sk, reaches_ENM_pk, reaches_ENM_cm,
     nuseds_Fr,
     fw_models, fw_amod,
     file = here("processed_data", "freshwater", "R_data", paste0(today, "_fw_spatial_inputs.Rdata")))

