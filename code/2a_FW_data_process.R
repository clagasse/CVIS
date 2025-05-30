###############################################################################
#
# 2a_FW_data_process.R
#
# Read in raw spatial data files and process them into R data frames and sf objects
#
# This script takes a while to run and should only be needed when new input files need to be generated
#
###############################################################################

library(here)
setwd(here())
source(file.path(here(), "code", "0_setup.R"))

library(future.apply) # parallel processing

# If encounter the error: 
# Error in wk_handle.wk_wkb(wkb, s2_geography_writer(oriented = oriented,  : 
# Loop 0 is not valid: Edge 2607 has duplicate vertex with edge 2625
# Then run:
#sf_use_s2(FALSE)

#--------------Load watershed basin polygons (for clipping primarily)--------------------
# BC Basins shapefile:  https://www.arcgis.com/home/item.html?id=7fe30c4a1cc34d14b560d868429bda35

basins <- st_read(file.path(paths$spatial, "BC_Basins", "BC_Basins_GoogleMapPL.shp")) %>%
  st_cast("POLYGON")
st_crs(basins) <- 4269 
basins <- st_transform(basins, crs = 3005)
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


#--------------Load and subset BC FISH PASS stream accessibility and linear habitat model------------------
# Load BC Fishpass models and subset to Fraser basin and accessible streams

# Info at: https://smnorris.github.io/bcfishpass/index.html
#BCFishpass has two different models
# accessibility of streams to anadromous fish
# linear spawning/rearing habitat model

# PSF version is downloaded here: https://www.hillcrestgeo.ca/outgoing/forPSF/
# Dated 2023-Dec-08 09:11

# BC Gov weekly distribution here: https://bcgov.github.io/bc_freshwater_fish_habitat_accessibility_model/04_data_distribution.html

## Habitat model - this takes a long time to read

unzip(file.path(paths$spatial, "BCFishpass", "bcfishpass_streams_habitat", "bcfishpass_streams_2024-12-09.zip"))
bcfph <- st_read(file.path(paths$spatial, "BCFishpass", "bcfishpass_streams_habitat", "bcfishpass_streams_2024-12-09.gpkg")) #%>%

bcfph_Fr <- st_contains(Fr_basin, st_zm(bcfph))  #subset Fraser basin
bcfph_Fr <- bcfph[bcfph_Fr[[1]],]

# st_write(bcfph_Fr, file.path(paths$spatial, "BCFishpass", "bcfishpass_streams_habitat", 
#                              "bcfishpass_Fraser_streams_2024-12-09.gpkg"), driver = "GPKG")

## Accessibility model
bcfp <- st_read(file.path(paths$spatial, "BCFishpass", "freshwater_fish_habitat_accessibility_MODEL", 
                          "freshwater_fish_habitat_accessibility_MODEL.gpkg"), layer = "model_access")

bcfp_Fr <- st_contains(Fr_basin, st_zm(bcfp))  #subset Fraser basin
bcfp_Fr <- bcfp[bcfp_Fr[[1]],]

# st_write(bcfp_Fr, file.path(paths$spatial, "BCFishpass", "freshwater_fish_habitat_accessibility_MODEL", 
#                             "Fraser_fish_habitat_accessibility_MODEL.gpkg"), driver = "GPKG")

#combine accessibility and habitat models within Fraser basin
bcfpc <- bcfph_Fr %>%
  left_join(select(data.table(bcfp_Fr), segmented_stream_id, linear_feature_id, model_access_salmon, model_access_steelhead),
            join_by(segmented_stream_id, linear_feature_id),
            multiple = "first")


#--------------- FRESHWATER ATLAS QUERY AND PROCESS------------------------------
#Freshwater Atlas is already used by BC Fishpass, but we need the original FWA Watershed Codes for 
# subsetting operations in some scripts

#load full FWA to get stream lengths for joining to BC fishpass
#FWA_Fr <- st_read(file.path(paths$spatial, "FWA_Fraser", "FWA_Fraser.shp"))

#FWA_watersheds <- st_layers(file.path(paths$spatial, "FWA", "FWA_STREAM_NETWORKS_SP.gdb"))

#get Fraser watershed codes for loading layers
Fr_codes <- unique(bcfpc$watershed_group_code)

#load watershed group codes within Fraser from full FWA stream network
FWA_Fr <- future_lapply(Fr_codes, function(layer) {
  st_read(file.path(paths$spatial, "FWA", "FWA_STREAM_NETWORKS_SP.gdb"), layer = layer)
})

#flatten list into single sf
FWA_Fr <- do.call(rbind, FWA_Fr)



#join watershed code
bcfpc <- bcfpc %>%
  left_join(data.table(FWA_Fr) %>%
              select(LINEAR_FEATURE_ID, FWA_WATERSHED_CODE),
            by = c("linear_feature_id" = "LINEAR_FEATURE_ID")) %>%
  relocate(FWA_WATERSHED_CODE, .after = linear_feature_id)

#check matches successful
sum(is.na(bcfpc$FWA_WATERSHED_CODE))

#create salmon rearing and spawning modelled habitat column
bcfpc <- bcfpc %>%
  mutate(model_habitat_salmon = if_else(model_spawning_ch == TRUE | model_spawning_cm == TRUE | 
                                          model_spawning_co == TRUE | model_spawning_pk == TRUE |
                                          model_spawning_sk == TRUE |
                                          model_rearing_ch == TRUE | model_rearing_co == TRUE |
                                          model_rearing_sk == TRUE, TRUE, FALSE))

#save as new .R object
# st_write(bcfpc, file.path(paths$spatial, "BCFishpass", "freshwater_fish_habitat_accessibility_MODEL", 
#                             "Fraser_fish_habitat_accessibility_MODEL.gpkg"), driver = "GPKG")
save(bcfpc, file = file.path(paths$fw, "BCFP_combined_Fr.Rds"))


#subset accessible streams only
bcfpa <- filter(bcfpc, model_access_salmon %in% c("OBSERVED", "INFERRED"))
#remove stream order 1
bcfpl <- filter(bcfpc, stream_order >= 2)

save(bcfpa, file = file.path(paths$fw, "BCFP_combined_accessible_Fr.Rds"))

#save a version with first order streams excluded
# first order streams represent ~2/3 of the streams but have less than 1% modelled rearing and spawning reaches
save(bcfpl, file = file.path(paths$spatial, "BCFishpass", "BCFP_combined_order2_Fr.Rds"))


### this code will query from the FWA database using an API, but is limited to 10,000 records
# library(fwapgr)  #package for accessing BC FWA
# collection_id <- "whse_basemapping.fwa_stream_networks_sp"
# filter_watersheds <- setNames(as.list(FR_codes), rep("watershed_group_code", length(FR_codes)))
# 
# FWA_query <- fwa_query_collection(collection_id, filter = filter_watersheds)


#-------------------- Load climate model outputs--------------------------------
## Thermalscapes august stream temperature
# Accessed from:  https://datadryad.org/dataset/doi:10.5061/dryad.bzkh189fk#readme

tscapes <- st_read(file.path(paths$climate, "bc_stream_thermalscapes.gdb"), layer = "thermalscape_fraser") %>%
  as.data.table()

# tscapes_nest <- data.table(tscapes)%>%
#   select(-c("Shape")) %>%
#   mutate(Tw_00_0 = pmap(select(., ends_with("00_0")), c),
#          Tw_00_1 = pmap(select(., ends_with("00_1")), c),
#          Tw_45_3 = pmap(select(., ends_with("45_3")), c),
#          Tw_45_5 = pmap(select(., ends_with("45_5")), c),
#          Tw_85_3 = pmap(select(., ends_with("85_3")), c),
#          Tw_85_5 = pmap(select(., ends_with("85_5")), c)) %>%
#   select(-c(contains("Tw8")))

## 7 Day Equivalent Model (7DECM) stream temperature
# not currently available online
T7DEC <- read_csv(file.path(paths$climate, "7DEC", "ThreshRisk_7DEC_Fraser.csv")) %>%
  as.data.table() %>%
  mutate(Risk20_9_45_3 = ifelse(Tav_9_45_3 < 20 & ThiPI_9_45_3 < 20, "Low", ifelse(Tav_9_45_3 < 20 & ThiPI_9_45_3 > 20, "Moderate",
                                                                                   ifelse(Tav_9_45_3 > 20 & TlowPI_9_45_3 < 20, "High",
                                                                                          "Severe"))),
         Risk24_9_45_3 = ifelse(Tav_9_45_3 < 24 & ThiPI_9_45_3 < 24, "Low", ifelse(Tav_9_45_3 < 24 & ThiPI_9_45_3 > 24, "Moderate",
                                                                                   ifelse(Tav_9_45_3 > 24 & TlowPI_9_45_3 < 24, "High",
                                                                                          "Severe"))))

         #Risk16_mod_len = Risk16_9_45_3 %in% c("Moderate", "High", "Very High") * Shape_Length,
         #Risk20_mod_len = Risk20_9_45_3 %in% c("Moderate", "High", "Very High") * Shape_Length,
         #Risk24_mod_len = Risk24_9_45_3 %in% c("Moderate", "High", "Very High") * Shape_Length)


# Foundry Spatial flow model derived from PCIC grid model
hflow <- st_read(file.path(paths$climate, "Fraserflow", "Historic_Flow_Data.gdb")) %>%
  as.data.table()
pflow <- st_read(file.path(paths$climate, "Fraserflow", "fraser_ensemble_means_rcp45_2020_2100.gdb")) %>%
  as.data.table()

#individual model projections for historical flow
target_file <- "data/fwa_rollup_access1_rcp45_r1i1p1_adj_m3s.csv"
access45 <- unzip(file.path(paths$climate, "Fraserflow", "modelled_flow.zip"), files = target_file)

access85 <- read_csv(file.path(paths$climate, "Fraserflow", "modelled_flow", "fwa_rollup_access_rcp85_r1i1p1_adj_m3s.csv")) %>%
  as.data.table()


pflow <- st_layers(file.path(paths$climate, "Fraserflow", "modelled_flow_rcp45.gdb")) %>%
  as.data.table()



# Cumulative threat score for Fraser streams
fwct <- st_read(file.path(paths$spatial, "CumulativeThreatScore", "CumulativeThreat_FRB.shp")) %>%
  as.data.table()


#---------------------Join stream network model outputs--------------------------

#load(file.path(paths$fw, "BCFP_combined_Fr.Rds")) #load bcfp stream network

#create stream network data table
bcfpmod <- as.data.table(bcfpa) %>%
  select(segmented_stream_id, linear_feature_id, FWA_WATERSHED_CODE, channel_width, length_metre,
         mad_m3s, upstream_area_ha, gradient, gnis_name, model_access_salmon, 
         model_habitat_salmon)


# bcfpcmod <- as.data.table(bcfpc) %>%
#   select(segmented_stream_id, linear_feature_id, FWA_WATERSHED_CODE, channel_width, length_metre,
#          mad_m3s, upstream_area_ha, gradient, gnis_name, model_access_salmon, 
#          model_habitat_salmon)

#join temperature models to bcfp
fwT <- bcfpmod %>%
  left_join(tscapes, 
            join_by(linear_feature_id == LINEAR_FEATURE_ID),
            relationship = "many-to-one") %>%
  left_join(select(T7DEC, - c(STREAM_ORDER, region)), 
            join_by(linear_feature_id == LINEAR_FEATURE_ID), 
            multiple = "first") 
  
tscapes_bcfp <- tscapes %>%
  left_join(bcfpcmod, 
            join_by(LINEAR_FEATURE_ID == linear_feature_id)) %>%
  
  
#join flow models to bcfp
fwQ <- bcfpmod %>%
  left_join(select(hflow, LINEAR_FEATURE_ID, Shape_Length, contains("mean")),
            join_by(linear_feature_id == LINEAR_FEATURE_ID),
            multiple = "first") %>%
  left_join(select(pflow, LINEAR_FEATURE_ID, Shape_Length, contains("mean")),
            join_by(linear_feature_id == LINEAR_FEATURE_ID),
            multiple = "first")

#join cumulative threats model to bcfp
fwct <- bcfpmod %>%
  left_join(select(fwct, -c(WATERSHED_, WATERSHED1, watershe_1)),
            join_by(linear_feature_id == LINEAR_FEA), 
            multiple = "first")


save(fwT, fwQ, fwct, file = file.path("processed_data", "Freshwater", "fw_models_T_Q_CT.Rds"))

#----------------------Ruzzante statistical low flow projections ------------------------------

stations_stats <- read_csv(file.path(paths$climate, "Ruzzante_low_flows", "stations_performance.csv"))

#watershed hydrologic regimes
watershed_flow <- st_read(file.path(paths$climate, "Ruzzante_low_flows", "watersheds.gpkg")) %>%
  st_transform(crs = 3005) %>%
  left_join(select(stations_stats, ID, regime), by = c("ID" = "ID")) %>%
  mutate(regime = as.factor(regime))

stations_flow <- st_read(file.path(paths$climate, "Ruzzante_low_flows", "stations.gpkg")) %>%
  st_transform(crs = 3005)

flow_in_cu <- lengths(st_contains(cu_boundary, stations_flow)) > 0 
cu_boundary$has_flow <- flow_in_cu

#list of stations/watersheds with projections
projections_list <- list.files(file.path(paths$climate, "Ruzzante_low_flows", "regressionProjections"), pattern = ".csv")

#import csv for each water station
#assign into 20 year periods and nest by period
for(i in 1:length(projections_list)) {
  projections_csv <- read_csv(file.path(paths$climate, "Ruzzante_low_flows", "regressionProjections", projections_list[i])) %>%
    mutate(ID = str_sub(projections_list[i],1,-5),
           period = if_else(Year >= 1981 & Year <= 2000, 0,
                                   if_else(Year >= 2001 & Year <= 2020, 1,
                                           if_else(Year >= 2021 & Year <= 2040, 2,
                                                   if_else(Year >= 2041 & Year <= 2060, 3,
                                                           if_else(Year >= 2061 & Year <= 2080, 4,
                                                                   if_else(Year >= 2081 & Year <= 2100, 5, NA))))))) %>%
    nest(.by = c("ID", "source_id",  "experiment_id", "variant_label", "period")) %>%
    filter((period < 2 & experiment_id == "historical") | 
             (period >= 2 & experiment_id != "historical"))
  
  if(i == 1) watershed_proj <- projections_csv
  else if(i > 1) watershed_proj <- bind_rows(watershed_proj, projections_csv)
}

#get average across model variants for each period and scenario
wp_vm <- watershed_proj %>% 
  mutate(mean = map_dbl(data, ~mean(.x$predMean.m3s_8))) %>%
  nest(.by = c("ID", "experiment_id", "source_id", "period"))

#get average across all GCMs for each year
wp_stats <- wp_vm %>%
  mutate(mean = map_dbl(data, ~mean(.x$mean)),
         sd   = map_dbl(data, ~sd(.x$mean)),
         q025 = map_dbl(data, ~quantile(.x$mean, probs = 0.025)),
         q975 = map_dbl(data, ~quantile(.x$mean, probs = 0.975))) 

wp_stats_ens <- wp_stats %>%
  nest(.by = c("ID", "experiment_id", "period")) %>%
  mutate(mean = map_dbl(data, ~mean(.x$mean)),
         sd   = map_dbl(data, ~sd(.x$mean)),
         q025 = map_dbl(data, ~quantile(.x$mean, probs = 0.025)),
         q975 = map_dbl(data, ~quantile(.x$mean, probs = 0.975))) %>%
  mutate(source_id = "ensemble", .after = experiment_id)


#save averaged flow projections 
save(wp_stats, wp_stats_ens, file = file.path(paths$fw, "Statistical_flow_projections.Rds"))




### comparisons between PCIC flows and Ruzzante model

# cu_cont <- st_contains(cu_boundary, stations_flow)
#take average of stations in each CU boundary
# cu_wp_pmean <- calculate_subset_means_all(wp_pmean, cu_cont)
# 
# cu_boundary_flow <- bind_cols(cu_boundary, cu_wp_pmean)
# 
# # cu_PCIC_flow <- cu_boundary_flow %>%
# #   left_join(CVIS_spn, by = c("CUID" = "cuid"))
# 
# station_cont <- t(st_contains(cu_boundary, stations_flow, sparse = F)) %>%
#   apply(1, sum) %>%
#   greater_zero() %>%
#   as_tibble()
# 
# stations_flow <- bind_cols(stations_flow, station_cont) %>%
#   rename(in_CU = value)
# 

#plot flows within CU boundaries
# ggplot() + 
#   geom_sf(data = filter(cu_boundary_flow, Species == "Chinook"), aes(fill = Qprop_3)) +
#   scale_fill_viridis(direction = -1) +
#   geom_sf(data = watershed_flow, colour = "darkred", alpha = 0.2) +
#   geom_sf(data = stations_flow) 
# ggplot() +
#   geom_sf(data = filter(cu_PCIC_flow, Species == "Chinook"), aes(fill = SPN_EXP_augQ)) + 
#   geom_sf(data = stations_flow) + 
#   scale_fill_viridis(direction = -1)
# ggplot(cu_PCIC_flow, x = Qprop_3, y = SPN_EXP_augQ) +
#   geom_point(aes(x = Qprop_3, y = SPN_EXP_augQ, color = Species)) +
#   xlim(-1,0) +
#   ylim(-1,0)



#---------------- Ecological Niche Models --------------------------------------

# Accessible at: https://github.com/freshwater-spatial-ecology/Salmon-ENMs-2023

#reaches used for ENM in BC
reaches_ENM <- st_read(file.path(paths$climate, "Salmon-ENMs-2023-master","BC_network_variables_climateproj.gdb"), 
                       layer = "BC_all_reaches_time_steps") %>%
  st_transform(crs = 3005)
#midpoint from reaches used in model fitting
midpoints_ENM <- st_read(file.path(paths$climate, 
                                   "Salmon-ENMs-2023-master","BC_network_variables_climateproj.gdb"), 
                         layer = "BC_all_midpoints_time_steps") %>%
  st_transform(crs = 3005)

ENMs_base_co <- st_read(file.path(paths$climate, "Salmon-ENMs-2023-master", "Mapped_model_predictions.gdb"), layer = "Coho_GBMFav_streams_acc")
#ENMs_45_co <- st_read(file.path(paths$climate, "Salmon-ENMs-2023-master", "Mapped_model_predictions.gdb"), layer = "COHO_BC_change_9_45_5")

st_layers(file.path(paths$climate, "Salmon-ENMs-2023-master", "Mapped_model_predictions.gdb"))

ENMs_GBM_ck <- read_csv(file.path(paths$climate, "Salmon-ENMs-2023-master", "Chinook_Proj_all_FavChange_GBM_072023.csv")) %>%
  select(SDM_ID_all, Fav_f.0_00_0, Fav_f.0_00_1, 
         Fav_f.9_45_3, Fav_f.9_45_4, Fav_f.9_45_5, 
         Fav_f.9_85_3, Fav_f.9_85_4, Fav_f.9_85_5,
        Fav_change_9_45_5, Fav_change_9_85_5) %>%
  rename_with(
    .fn = ~ if_else(str_detect(., "Fav"), paste0("ck_", .), .),
    .cols = everything()
  )
# Coho file is missing favourability outputs   
ENMs_GBM_co <- read_csv(file.path(paths$climate, "Salmon-ENMs-2023-master", "Coho_Proj_all_FavChange_GBM_102023.csv")) %>%
  rename(Fav_f.0_00_0 = `0_00_01`, Fav_f.0_00_1 = `0_00_12`, 
         Fav_f.9_45_3 = `9_45_34`, Fav_f.9_45_4 = `9_45_45`, Fav_f.9_45_5 = `9_45_56`,
         Fav_f.9_85_3 = `9_85_34`, Fav_f.9_85_4 = `9_85_45`, Fav_f.9_85_5 = `9_85_56`,
         Prob_f.0_00_0 = `0_00_0`, Prob_f.0_00_1 = `0_00_1`, 
         Prob_f.9_45_3 = `9_45_3`, Prob_f.9_45_4 = `9_45_4`, Prob_f.9_45_5 = `9_45_5`,
         Prob_f.9_85_3 = `9_85_3`, Prob_f.9_85_4 = `9_85_4`, Prob_f.9_85_5 = `9_85_5`) %>%
  select(SDM_ID_all, Prob_f.0_00_0, Prob_f.0_00_1, 
         Prob_f.9_45_3, Prob_f.9_45_4, Prob_f.9_45_5, 
         Prob_f.9_85_3, Prob_f.9_85_4, Prob_f.9_85_5,
         Fav_change_9_45_5, Fav_change_9_85_5) %>%
  rename_with(
    .fn = ~ if_else(str_detect(., "Fav"), paste0("co_", .), .),
    .cols = everything()
  )
ENMs_GBM_sk <- read_csv(file.path(paths$climate, "Salmon-ENMs-2023-master", "Sockeye_Proj_all_FavChange_GBM_072023.csv")) %>%
  select(SDM_ID_all, Fav_f.0_00_0, Fav_f.0_00_1, 
         Fav_f.9_45_3, Fav_f.9_45_4, Fav_f.9_45_5, 
         Fav_f.9_85_3, Fav_f.9_85_4, Fav_f.9_85_5,
         Fav_change_9_45_5, Fav_change_9_85_5) %>%
  rename_with(
    .fn = ~ if_else(str_detect(., "Fav"), paste0("sk_", .), .),
    .cols = everything()
  )
ENMs_GBM_pk <- read_csv(file.path(paths$climate, "Salmon-ENMs-2023-master", "Pink_Proj_all_FavChange_GBM_072023.csv")) %>%
  select(SDM_ID_all, Fav_f.0_00_0, Fav_f.0_00_1, 
         Fav_f.9_45_3, Fav_f.9_45_4, Fav_f.9_45_5, 
         Fav_f.9_85_3, Fav_f.9_85_4, Fav_f.9_85_5,
         Fav_change_9_45_5, Fav_change_9_85_5) %>%
  rename_with(
    .fn = ~ if_else(str_detect(., "Fav"), paste0("pk_", .), .),
    .cols = everything()
  )
ENMs_GBM_cm <- read_csv(file.path(paths$climate, "Salmon-ENMs-2023-master", "Chum_Proj_all_FavChange_GBM_072023.csv")) %>%
  select(SDM_ID_all, Fav_f.0_00_0, Fav_f.0_00_1, 
         Fav_f.9_45_3, Fav_f.9_45_4, Fav_f.9_45_5, 
         Fav_f.9_85_3, Fav_f.9_85_4, Fav_f.9_85_5,
         Fav_change_9_45_5, Fav_change_9_85_5) %>%
  rename_with(
    .fn = ~ if_else(str_detect(., "Fav"), paste0("cm_", .), .),
    .cols = everything()
  )

ENM_all_sp <- ENMs_GBM_ck %>%
  left_join(ENMs_GBM_co, 
            join_by(SDM_ID_all)) %>%
  left_join(ENMs_GBM_sk, 
            join_by(SDM_ID_all)) %>%
  left_join(ENMs_GBM_pk, 
            join_by(SDM_ID_all)) %>%
  left_join(ENMs_GBM_cm, 
            join_by(SDM_ID_all))

#join mid-point to bcfp stream network to determine linear_feature_id
midpoints_bcfp <- st_join(st_zm(midpoints_ENM), st_zm(bcfpc), join = st_nearest_feature, left = FALSE)

reaches_ENM_all <- reaches_ENM %>%
  left_join(ENM_all_sp, 
            join_by(SDM_ID_all)) %>%
  select(-c(contains("PPT"), contains("Tw8"))) %>%
  left_join(select(as.data.table(midpoints_bcfp), linear_feature_id, segmented_stream_id, SDM_ID_all,
                   model_access_salmon, model_habitat_salmon),
            join_by(SDM_ID_all)) %>%
  relocate(linear_feature_id, segmented_stream_id) %>%
  mutate(length_metre = Length_km /1000) %>%
  filter(!is.na(ck_Fav_change_9_45_5))


save(reaches_ENM_all, midpoints_bcfp, file = file.path("processed_data", "freshwater", "ENM_all_sp.Rds")) 

# bcfp_ENM <- bcfpc %>%
#   left_join(select(data.table(midpoints_bcfp), linear_feature_id, segmented_stream_id, SDM_ID_all),
#             join_by(linear_feature_id, segmented_stream_id)) 

#join model outputs to stream network
# bcfp_ENM <- bcfp_ENM %>%
#   left_join(ENM_all_sp,
#             join_by(SDM_ID_all))
# 
# bcfp_ENM_sub <- filter(bcfp_ENM, !is.na(SDM_ID_all))

# plotting checks of data
# cu_bsub <- cu_boundary[1,] 
# 
# midpoints_sub <- st_contains(cu_bsub, midpoints_ENM)
# midpoints_sub <- midpoints_ENM[midpoints_sub[[1]],]
# 
# bcfp_sub <- st_contains(cu_bsub, st_zm(bcfpc))
# bcfp_sub <- bcfpc[bcfp_sub[[1]],]
# 
# ENM_sub <- st_contains(cu_bsub, st_zm(bcfp_ENM_sub))
# ENM_sub <- bcfp_ENM_sub[ENM_sub[[1]],]
# 
# ggplot() +
#   geom_sf(data = bcfp_sub) +
#   geom_sf(data = midpoints_sub)
# 
# ggplot() +
#   geom_sf(data = bcfp_ENM_sub, aes(colour = cm_Fav_change_9_45_5))
# 
# ggplot() +
#   geom_sf(data = bcfp_sub, alpha = 0.5, colour = "grey") + 
#   geom_sf(data = ENM_sub, aes(colour = cm_Fav_change_9_45_5)) +
#   geom_sf(data = midpoints_sub, size = 0.5)


#--------------Load freshwater adaptive zone layer----------------------------
# 
# FAZ <- st_read(file.path(paths$spatial, "FAZ", "FreshwaterAdaptiveZones.shp")) %>%
#   st_make_valid() %>%
#   st_transform(crs = 4269)  #NAD83
# 
# FAZ_Fr <- filter(FAZ, FAZ_Code <= 10 & FAZ_Code >1)
# 
# 
# # determine FAZ where cu boundaries intersect and add to cu_boundary object
# #FAZ_over <- st_overlaps(FAZ, cu_boundary)
# #FAZ_cont <- st_contains(FAZ, cu_boundary)
# FAZ_int <- st_intersects(cu_boundary, FAZ)
# cu_boundary$FAZ <- as.character(NA)
# 
# for (i in 1:nrow(cu_boundary)) {
#   vec_basins <- FAZ$FAZ_Acrony[FAZ_int[[i]]]
#   vec_basins <- str_sort(vec_basins)
#   cu_boundary$FAZ[i] <- str_flatten(vec_basins, collapse = ",")
# }

#Determine which streams are contained within each FAZ
# bcfp$FAZ <- NA
# for(f in 1:length(FAZ_Fr$FAZ_Name)) {
#   FAZ_pick <- FAZ_Fr[f,]
#   pick_tscapes <- lengths(st_intersects(bcfp,FAZ_pick)) > 0
#   bcfp$FAZ[pick_tscapes] <- FAZ_pick$FAZ_Acrony
# }


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


save(PCIC_month, PCIC_day, grid_points, grid_polys, 
     file = here("processed_data", "freshwater", "R_data", paste0(today, "_fw_spatial_inputs.Rdata")))
