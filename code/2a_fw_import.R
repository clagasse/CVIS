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

#-------------------Conservation Unit boundaries for Fraser CUs ----------------

cu_boundary <- st_read(file.path(spatial_dat, "CU_boundaries", "fraser_cus.shp")) %>%
  st_make_valid() %>%
  st_transform(crs = 4269)  %>%  #crs 3005 is NAD83/BC Albers
  left_join(select(cu_list, cuid, FULL_CU_IN, spp), 
            join_by(CUID == cuid))


# Are all CUs in cu_run in cu_boundary?
#cu_list$cuid %in% cu_boundary$CUID # Yes
#cu_boundary$CUID %in% cu_list$cuid

#remove CUs that are not in the cu_boundary list
#cu_run <- cu_run[cu_run$cuid %in% cu_boundary$CUID,]

#remove CU shapes that are not being run
# cu_boundary <- cu_boundary[cu_boundary$CUID %in% cu_run$cuid,] %>%
#   left_join(select(cu_run, Species_simple, Species_ecotype, spp, cuid), join_by(CUID == cuid))


#--------------Load freshwater adaptive zone layer----------------------------

# FAZ <- st_read(file.path(spatial_dat, "FAZ", "FreshwaterAdaptiveZones.shp")) %>%
#   st_transform(crs = 4269)  #NAD83

FAZ <- st_read(file.path(spatial_dat, "FAZ", "FreshwaterAdaptiveZones.shp")) %>%
  st_make_valid() %>%
  st_transform(crs = 4269)  #NAD83

FAZ_Fr <- filter(FAZ, FAZ_Code <= 10 & FAZ_Code >1)


# determine FAZ where cu boundaries intersect and add to cu_boundary object
#FAZ_over <- st_overlaps(FAZ, cu_boundary)
#FAZ_cont <- st_contains(FAZ, cu_boundary)
FAZ_int <- st_intersects(cu_boundary, FAZ)
cu_boundary$FAZ <- as.character(NA)

for (i in 1:nrow(cu_boundary)) {
  vec_basins <- FAZ$FAZ_Acrony[FAZ_int[[i]]]
  vec_basins <- str_sort(vec_basins)
  cu_boundary$FAZ[i] <- str_flatten(vec_basins, collapse = ",")
}

#--------------Load watershed basin polygons (for clipping primarily)--------------------

basins <- st_read(file.path(spatial_dat, "BC_Basins", "BC_Basins_GoogleMapPL.shp")) %>%
  st_cast("POLYGON")
st_crs(basins) <- 4269 

Fr_basin <- filter(basins, BASIN == "FRASER")   #fraser basin only

# # determine basin where cu boundaries intersect and add to cu_boundary object
# basin_int <- st_intersects(basins, cu_boundary)
# cu_boundary$basins <- as.character(NA)
# 
# for (i in 1:nrow(cu_boundary)) {
#   vec_basins <- basins$BASIN[which(basin_int[-4,i] == TRUE)]   #remove 4th column to exclude Fraser
#   vec_basins <- str_sort(vec_basins)
#   cu_boundary$basins[i] <- str_flatten(vec_basins, collapse = ",")
# }



#--------------Load BC Freshwater Atlas stream network------------------

#load full FWA to get stream lengths for joining to BC fishpass
FWA_Fr_high <- st_read(file.path(spatial_dat, "FWA_Fraser", "FWA_Fraser.shp")) %>% 
  filter(STREAM_ORD > 2)  %>%
  st_transform(crs = 4269) 

#--------------Load BC FISH PASS stream accessibility model------------------
# bcfishpass: subsetted to accessible streams
# There are two versions, one created for PSF and one available online
# PSF version is downloaded here: https://www.hillcrestgeo.ca/outgoing/forPSF/
# Dated 2023-Dec-08 09:11 

# BC Gov weekly distribution here: https://bcgov.github.io/bc_freshwater_fish_habitat_accessibility_model/04_data_distribution.html

## Import BC Gov version and subset to accessible streams only
#bcfp <- st_read(file.path(spatial_dat, "freshwater_fish_habitat_accessibility_MODEL", "freshwater_fish_habitat_accessibility_MODEL.gpkg"), layer = "model_access") #%>%
#   st_transform(crs = 4269)
# bcfp_a <- filter(bcfp, model_access_salmon == "OBSERVED")
# bcfp_b <- filter(bcfp, model_access_salmon == "INFERRED")
# bcfp_a <- bind_rows(bcfp_a, bcfp_b)
# st_write(bcfp_a, file.path(spatial_dat, "fw_salmon_accessible.gpkg"), driver = "GPKG")

# import BC Gov version with accessibly streams only
bcfp <- st_read(file.path(spatial_dat, "freshwater_fish_habitat_accessibility_MODEL", "fw_salmon_accessible.gpkg")) %>%
select(-c(barriers_ch_cm_co_pk_sk_dnstr:remediated_dnstr_ind))  %>%
  st_transform(crs = 4269)

#create subsetted versions of bcfp for Fraser basin
bcfp_Fr <- st_contains(Fr_basin, bcfp)
bcfp_Fr <- bcfp[bcfp_Fr[[1]],]

#subsetted version with higher order streams
bcfp_Fr_high <- filter(bcfp_Fr, stream_order > 2)

#PSF alternate version of bcfp containing catchment area (upstream_area_ha) and modelled habitat by species
#bcfp_PSF <- st_read(file.path(spatial_dat, "bcfishpass_Fraser.gdb"))

#Determine which streams are contained within each FAZ
# bcfp$FAZ <- NA
# for(f in 1:length(FAZ_Fr$FAZ_Name)) {
#   FAZ_pick <- FAZ_Fr[f,]
#   pick_tscapes <- lengths(st_intersects(bcfp,FAZ_pick)) > 0
#   bcfp$FAZ[pick_tscapes] <- FAZ_pick$FAZ_Acrony
# }


#---------------------------Load NUSEDS salmon spawner locations----------------

##version from FIA. Usage column added by Michael Arbeider
nuseds_Fr <- read_csv(file.path(salmon_dat, "NuSEDS_CU_System_sites_202406.csv")) %>%
  st_as_sf(coords = c("X_LONGT", "Y_LAT"), crs = 4269) %>%
  filter(USAGE != "REMOVE")

# chin_sp <- st_read(file.path(salmon_dat, "FIA", "2023 Chinook Master.gpx"), layer = "waypoints") %>%
#   st_transform(crs = 4269)
# 
# coho_sp <- st_read(file.path(salmon_dat, "FIA", "2021 Coho Master.gpx"), layer = "waypoints") %>%
#                 st_transform(crs = 4269)
# 
# sox_layers <- st_layers(file.path(salmon_dat, "FIA", "Fraser Sockeye Spawning", "doc.kml"))
# 
# for(i in 1:length(sox_layers$name)) {
#   
#   temp <- st_read(file.path(salmon_dat, "FIA", "Fraser Sockeye Spawning", "doc.kml"), layer = sox_layers$name[i])
#   
#   if(i == 1) sockeye_sp <- temp
#   if(i > 1) sockeye_sp <- bind_rows(sockeye_sp, temp)
# }


#----------------------Load shoreline data (for mapping only)-------------------

# shoreline <- st_read(file.path(spatial_dat, "shoreline", "GSHHS_i_L1.shp")) %>%
#   st_make_valid() %>%
#   st_crop(xmin = -127.5, xmax = -121.5, ymin = 48.5, ymax = 54.5)

# Use PacEA in more recent versions
#bc_coast


#------------------------Load stream network models--------------------

## Thermalscapes august stream temperature
# Accessed from:  https://datadryad.org/dataset/doi:10.5061/dryad.bzkh189fk#readme

tscapes <- st_read(file.path(climate_dat, "bc_stream_thermalscapes.gdb"), layer = "thermalscape_fraser") %>%
  st_transform(crs = 4269)
  
tscapes_nest <- tscapes %>%
  mutate(Tw8_45_3 = pmap(select(., ends_with("45_3")), c),
         Tw8_45_5 = pmap(select(., ends_with("45_5")), c),
         Tw8_85_3 = pmap(select(., ends_with("85_3")), c),
         Tw8_85_5 = pmap(select(., ends_with("85_5")), c)) 
  #select(-c(contains("45"), contains("85"), contains("26")))

  #join to bcfp PSF version to get upstream area
  # left_join(select(as_tibble(bcfp_PSF), linear_feature_id, upstream_area_ha), 
  #           join_by(LINEAR_FEATURE_ID == linear_feature_id),
  #           multiple = "first") %>% #join to bcfp PSF version to get upstream area
  #filter out tscapes with catchment area > 30km2 for more comparable estimate to PCIC grid cells
  #tscapes_bigc <- filter(tscapes, upstream_area_ha > 3000)

## 7 Day Equivalent Model (7DECM) stream temperature
# not currently available online
T7DECM <- read_csv(file.path(climate_dat, "ThreshRisk_7DEC_Fraser.csv"))

# Foundry Spatial flow model derived from PCIC grid model
flow_fwa <- st_read(file.path(climate_dat, "Fraserflow", "fraser_ensemble_means_rcp45_2020_2100.gdb"))
flow_hist_fwa <- st_read(file.path(climate_dat, "Fraserflow", "Historic_Flow_Data.gdb"))

# Cumulative threat score for Fraser streams
fw_stress <- st_read(file.path(spatial_dat, "CumulativeThreatScore", "CumulativeThreat_FRB.shp"))


#---------------------Join stream network model outputs--------------------------

#join thermalscapes to bcfp, Foundry flow stream model, cumulative threat score, and 7DECM model
fw_models <- tscapes %>%
  left_join(select(as_tibble(bcfp), linear_feature_id, model_access_salmon), 
            join_by(LINEAR_FEATURE_ID == linear_feature_id),
            multiple = "first") %>%
  left_join(select(as_tibble(flow_fwa), LINEAR_FEATURE_ID,  
                   mean_17_40, #mean annual flows - 
                   mean_1_40, mean_2_40, mean_3_40, mean_4_40, mean_5_40, mean_6_40, 
                   mean_7_40, mean_8_40, mean_9_40, mean_10_40, mean_11_40, mean_12_40, #monthly flows
                   min_8_40,  max_8_40), #min and max august flows - min is the lowest monthly value across  the year range
            by = c("LINEAR_FEATURE_ID"), 
            multiple = "first") %>%
  left_join(select(as_tibble(flow_hist_fwa), LINEAR_FEATURE_ID, #may flows
                   mean_flow_m3s_17_1,
                   mean_flow_m3s_1_1, mean_flow_m3s_2_1, mean_flow_m3s_3_1, mean_flow_m3s_4_1, mean_flow_m3s_5_1, mean_flow_m3s_6_1, 
                   mean_flow_m3s_7_1, mean_flow_m3s_8_1, mean_flow_m3s_9_1, mean_flow_m3s_10_1, mean_flow_m3s_11_1, mean_flow_m3s_12_1), 
            by = c("LINEAR_FEATURE_ID"), 
            multiple = "first") %>%
  left_join(as_tibble(select(fw_stress, LINEAR_FEA, CT_anad)), 
            by = c("LINEAR_FEATURE_ID" = "LINEAR_FEA"), 
            multiple = "first") %>%
  left_join(select(T7DECM, - c(STREAM_ORDER, region)), by = c("LINEAR_FEATURE_ID" = "LINEAR_FEATURE_ID"), multiple = "first") %>%
  mutate(Risk20_9_45_3 = ifelse(Tav_9_45_3 < 20 & ThiPI_9_45_3 < 20, "Low", ifelse(Tav_9_45_3 < 20 & ThiPI_9_45_3 > 20, "Moderate",
                                                                                   ifelse(Tav_9_45_3 > 20 & TlowPI_9_45_3 < 20, "High", "Severe"))),
         Risk24_9_45_3 = ifelse(Tav_9_45_3 < 24 & ThiPI_9_45_3 < 24, "Low", ifelse(Tav_9_45_3 < 24 & ThiPI_9_45_3 > 24, "Moderate",
                                                                                   ifelse(Tav_9_45_3 > 24 & TlowPI_9_45_3 < 24, "High", "Severe"))),
         Risk16_mod_len = Risk16_9_45_3 %in% c("Moderate", "High", "Very High") * Shape_Length,
         Risk20_mod_len = Risk20_9_45_3 %in% c("Moderate", "High", "Very High") * Shape_Length,
         Risk24_mod_len = Risk24_9_45_3 %in% c("Moderate", "High", "Very High") * Shape_Length)

#subset to accessible streams only
fw_amod <- fw_models %>%
  filter(model_access_salmon %in% c("OBSERVED", "INFERRED"))



#----------------------Load ENM stream favourability model----------------------
# Accessible at: https://github.com/freshwater-spatial-ecology/Salmon-ENMs-2023
reaches_ENM <- st_read(file.path(climate_dat, "Salmon-ENMs-2023-master","BC_network_variables_climateproj.gdb"), layer = "BC_all_reaches_time_steps")
#midpoints_ENM <- st_read(file.path(climate_dat, "Salmon-ENMs-2023-master","BC_network_variables_climateproj.gdb"), layer = "BC_all_midpoints_time_steps")

#ENMs_base_ck <- st_read(file.path(climate_dat, "Salmon-ENMs-2023-master", "Mapped_model_predictions.gdb"), layer = "Coho_GBMFav_streams_acc")
#ENMs_45_co <- st_read(file.path(climate_dat, "Salmon-ENMs-2023-master", "Mapped_model_predictions.gdb"), layer = "COHO_BC_change_9_45_5")

ENMs_GBM_ck <- read_csv(file.path(climate_dat, "Salmon-ENMs-2023-master", "Chinook_Proj_all_FavChange_GBM_072023.csv"))
ENMs_GBM_co <- read_csv(file.path(climate_dat, "Salmon-ENMs-2023-master", "Coho_Proj_all_FavChange_GBM_102023.csv")) %>%
  rename(Fav_f.0_00_0 = `0_00_01`, Fav_f.0_00_1 = `0_00_12`, Fav_f.9_45_3 = `9_45_34`, Fav_f.9_45_5 = `9_45_56`,
         Prob_f.0_00_1 = `0_00_1`, Prob_f.9_45_3 = `9_45_3`, Prob_f.9_45_5 = `9_45_5`)
ENMs_GBM_sk <- read_csv(file.path(climate_dat, "Salmon-ENMs-2023-master", "Sockeye_Proj_all_FavChange_GBM_072023.csv"))
ENMs_GBM_pk <- read_csv(file.path(climate_dat, "Salmon-ENMs-2023-master", "Pink_Proj_all_FavChange_GBM_072023.csv"))
ENMs_GBM_cm <- read_csv(file.path(climate_dat, "Salmon-ENMs-2023-master", "Chum_Proj_all_FavChange_GBM_072023.csv"))

reaches_ENM_ck <- reaches_ENM %>%
  left_join(ENMs_GBM_ck, by = c("SDM_ID_all")) %>%
  select(SDM_ID_all, presence, x, y, Fav_f.0_00_0, Fav_f.0_00_1, Fav_f.9_45_3, Fav_f.9_45_5, 
         Prob_f.0_00_1, Prob_f.9_45_3, Prob_f.9_45_5) 
reaches_ENM_co <- reaches_ENM %>%
  left_join(ENMs_GBM_co, by = c("SDM_ID_all")) %>%
  select(SDM_ID_all, presence, x, y, Fav_f.0_00_0, Fav_f.0_00_1, Fav_f.9_45_3, Fav_f.9_45_5, 
         Prob_f.0_00_1, Prob_f.9_45_3, Prob_f.9_45_5)
reaches_ENM_sk <- reaches_ENM %>%
  left_join(ENMs_GBM_sk, by = c("SDM_ID_all")) %>%
  select(SDM_ID_all, presence, x, y, Fav_f.0_00_0, Fav_f.0_00_1, Fav_f.9_45_3, Fav_f.9_45_5, 
         Prob_f.0_00_1, Prob_f.9_45_3, Prob_f.9_45_5)
reaches_ENM_pk <- reaches_ENM %>%
  left_join(ENMs_GBM_pk, by = c("SDM_ID_all")) %>%
  select(SDM_ID_all, presence, x, y, Fav_f.0_00_0, Fav_f.0_00_1, Fav_f.9_45_3, Fav_f.9_45_5, 
         Prob_f.0_00_1, Prob_f.9_45_3, Prob_f.9_45_5)
reaches_ENM_cm <- reaches_ENM %>%
  left_join(ENMs_GBM_cm, by = c("SDM_ID_all")) %>%
  select(SDM_ID_all, presence, x, y, Fav_f.0_00_0, Fav_f.0_00_1, Fav_f.9_45_3, Fav_f.9_45_5, 
         Prob_f.0_00_1, Prob_f.9_45_3, Prob_f.9_45_5)

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


#----------------------Load low flow projections ------------------------------

stations_stats <- read.csv(file.path(climate_dat, "Ruzzante_low_flows", "stations_performance.csv"))

watershed_flow <- st_read(file.path(climate_dat, "Ruzzante_low_flows", "watersheds.gpkg")) %>%
  st_transform(crs = 4269) %>%
  left_join(select(stations_stats, ID, regime), by = c("ID" = "ID")) %>%
  mutate(regime = as.factor(regime))

stations_flow <- st_read(file.path(climate_dat, "Ruzzante_low_flows", "stations.gpkg")) %>%
  st_transform(crs = 4269)

flow_in_cu <- lengths(st_contains(cu_boundary, stations_flow)) > 0 
cu_boundary$has_flow <- flow_in_cu

projections_list <- list.files(file.path(climate_dat, "Ruzzante_low_flows", "regressionProjections"), pattern = ".csv")

for(i in 1:length(projections_list)) {
  projections_csv <- read.csv(file.path(climate_dat, "Ruzzante_low_flows", "regressionProjections", projections_list[i])) %>%
    mutate(ID = str_sub(projections_list[i],1,-5))%>%
    nest(.by = c("ID", "source_id",  "experiment_id", "Year")) #%>%
    #nest(.by = c("ID", "source_id", "experiment_id"))

  if(i == 1) watershed_proj <- projections_csv
  else if(i > 1) watershed_proj <- bind_rows(watershed_proj, projections_csv)
}

wp_sub <- filter(watershed_proj, experiment_id %in% c("historical", "ssp370"))

wp_vm <- wp_sub %>% 
  mutate(mean = map_dbl(data, ~mean(.x$predMean.m3s_8))) %>%
  nest(.by = c("ID", "experiment_id", "Year"))

wp_mean <- wp_vm %>%
  mutate(mean = map_dbl(data, ~mean(.x$mean))) %>%
  select(-data) %>%
  mutate(period = if_else(Year >= 1981 & Year <= 2000, 0,
                          if_else(Year >= 2001 & Year <= 2020, 1,
                                  if_else(Year >= 2021 & Year <= 2040, 2,
                                          if_else(Year >= 2041 & Year <= 2060, 3,
                                                  if_else(Year >= 2061 & Year <= 2080, 4,
                                                          if_else(Year >= 2081 & Year <= 2100, 5, NA)))))))

wp_pmean <- wp_mean %>%
  filter(!is.na(period)) %>%
  group_by(ID, experiment_id, period) %>%
  summarise(mean = mean(mean)) %>%
  ungroup() %>%
  mutate(period = as.factor(period)) %>%
  pivot_wider(names_from = c(experiment_id, period), values_from = mean) %>%
  mutate(Qdelta_3 = ssp370_3 - historical_0,
         Qdelta_5 = ssp370_5 - historical_0,
         Qprop_3 = (Qdelta_3 / historical_0),
         Qprop_5 = (Qdelta_5 / historical_0))

stations_flow <- stations_flow %>%
  left_join(wp_pmean, by = c("ID" = "ID")) 

watershed_flow <- watershed_flow %>%
  left_join(wp_pmean, by = c("ID" = "ID")) 

cu_cont <- st_contains(cu_boundary, stations_flow)

#take average of stations in each CU boundary

calculate_subset_means_all <- function(df, index_list) {
  # Select only numeric columns
  numeric_df <- df %>% select_if(is.numeric)
  
  # For each subset, calculate means of all numeric columns
  map_df(index_list, function(indices) {
    numeric_df[indices, ] %>%
      summarise(across(everything(), \(x) mean(x, na.rm = TRUE)))
  }, .id = "subset")
}

greater_zero <- function(x) {
  if_else(x > 0, T, F)
}

  
cu_wp_pmean <- calculate_subset_means_all(wp_pmean, cu_cont)

cu_boundary_flow <- bind_cols(cu_boundary, cu_wp_pmean)

cu_PCIC_flow <- cu_boundary_flow %>%
  left_join(CVIS_spn, by = c("CUID" = "cuid"))


station_cont <- t(st_contains(cu_boundary, stations_flow, sparse = F)) %>%
  apply(1, sum) %>%
  greater_zero() %>%
  as_tibble()

stations_flow <- bind_cols(stations_flow, station_cont) %>%
  rename(in_CU = value)


  
  

#get average historic and projected summer flows for each watershed

ggplot() + 
  geom_sf(data = filter(cu_boundary_flow, Species == "Chinook"), aes(fill = Qprop_3)) +
  scale_fill_viridis(direction = -1) +
  geom_sf(data = watershed_flow, colour = "darkred", alpha = 0.2) +
  geom_sf(data = stations_flow) 


ggplot() +
  geom_sf(data = filter(cu_PCIC_flow, Species == "Chinook"), aes(fill = SPN_EXP_augQ)) + 
  geom_sf(data = stations_flow) + 
  scale_fill_viridis(direction = -1)


ggplot(cu_PCIC_flow, x = Qprop_3, y = SPN_EXP_augQ) +
  geom_point(aes(x = Qprop_3, y = SPN_EXP_augQ, color = Species)) +
  xlim(-1,0) +
  ylim(-1,0)

ggplot() +
  

  

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
PCIC_day <- read_mdim(file.path(climate_dat, "PCIC_processed", "PCIC_daily.nc"))
PCIC_month <- read_mdim(file.path(climate_dat, "PCIC_processed", "PCIC_monthly.nc"))

# #get PCIC file names in directory
# PCIC_files <- list.files(file.path(climate_dat, "PCIC_indicators"))
# PCIC_files <- PCIC_files[grep(".aux.xml", PCIC_files, invert=TRUE)]
# 
# #load selected indicators for available time periods and combine into a star object
# PCIC_file_choose <- PCIC_indies_pick
# for(i in 1:length(PCIC_indies_pick)){
#   pick_files <- PCIC_files[grepl(PCIC_file_choose[i], PCIC_files)]
# 
#   for(p in 1:length(t_periods)) {
#     temp_star <- read_ncdf(file.path(climate_dat, "PCIC_indicators", pick_files[p]))
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
#     temp_star <- read_ncdf(file.path(climate_dat, "PCIC_indicators", pick_files[p]))
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
#     temp_star <- read_ncdf(file.path(climate_dat, "PCIC_indicators", PCIC_month_files[p]))
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

