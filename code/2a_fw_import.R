###############################################################################
#
# 2a_fw_import.R
#
# This code reads in a number of spatial datasets related to salmon distribution
# in the **Fraser region** and determines, for each Conservation Unit (CU),  
# which PCIC grid cells should be used in assessments of climate change exposure  
# for freshwater life stages.
#
###############################################################################

#------------------------------------------------------------------------------
# Conservation Unit boundaries for Fraser CUs (all species)
#------------------------------------------------------------------------------

cu_boundary <- st_read(file.path(spatial_dat, "CU_boundaries", "fraser_cus.shp")) %>%
  st_make_valid() %>%
  st_transform(crs = 4269)   #crs 3005 is NAD83/BC Albers

# Are all CUs in cu_run in cu_boundary?
#cu_list$cuid %in% cu_boundary$CUID # Yes
#cu_boundary$CUID %in% cu_list$cuid

#remove CUs that are not in the cu_boundary list
cu_run <- cu_run[cu_run$cuid %in% cu_boundary$CUID,]

#remove CU shapes that are not being run
# cu_boundary <- cu_boundary[cu_boundary$CUID %in% cu_run$cuid,] %>%
#   left_join(select(cu_run, Species_simple, Species_ecotype, spp, cuid), join_by(CUID == cuid))


#------------------------------------------------------------------------------
# Load freshwater adaptive zone layer
#------------------------------------------------------------------------------

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

#------------------------------------------------------------------------------
# Load river basin data (for clipping primarily)
#------------------------------------------------------------------------------

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


#------------------------------------------------------------------------------
# bcfishpass: subsetted to accessible streams
#
# Updated to use version of database from https://www.hillcrestgeo.ca/outgoing/forPSF/
# Dated 2023-Dec-08 09:11 
#------------------------------------------------------------------------------

# bcfp <- st_read(file.path(spatial_dat, "freshwater_fish_habitat_accessibility_MODEL", "freshwater_fish_habitat_accessibility_MODEL.gpkg"), layer = "model_access") %>%
#   st_transform(crs = 4269)
# bcfp_a <- filter(bcfp, model_access_salmon == "OBSERVED")
# bcfp_b <- filter(bcfp, model_access_salmon == "INFERRED")
# bcfp_a <- bind_rows(bcfp_a, bcfp_b)
# 
# st_write(bcfp_a, file.path(spatial_dat, "fw_salmon_accessible.gpkg"), driver = "GPKG")

#load full FWA to get stream lengths for joining to BC fishpass
FWA_Fr_high <- st_read(file.path(spatial_dat, "FWA_Fraser", "FWA_Fraser.shp")) %>% 
  filter(STREAM_ORD > 3)  %>%
  st_transform(crs = 4269) 

bcfp <- st_read(file.path(spatial_dat, "freshwater_fish_habitat_accessibility_MODEL", "fw_salmon_accessible.gpkg")) %>%
  select(-c(barriers_ch_cm_co_pk_sk_dnstr:remediated_dnstr_ind))  %>%
  st_transform(crs = 4269) 

#-------------------------
# Load Thermalscapes stream network data
tscapes <- st_read(file.path(climate_dat, "bc_stream_thermalscapes.gdb"), layer = "thermalscape_fraser") %>%
  st_transform(crs = 4269) %>%
  left_join(select(as_tibble(bcfp), linear_feature_id, model_access_salmon), 
            join_by(LINEAR_FEATURE_ID == linear_feature_id),
            multiple = "first")

## join column showing which streams are accessible

tscapes_acc <- filter(tscapes, model_access_salmon %in% c("OBSERVED", "INFERRED"))

##join to bcfp and subset tscapes RCP 45 scenario, ensemble model (=9), and historic and mid-century
# bcfp <- bcfp %>%
#   left_join(select(as_tibble(tscapes), LINEAR_FEATURE_ID, b1, mic_0_00_1, Tw8_0_00_0, Tw8_9_45_3, Tw8_9_45_5), join_by(linear_feature_id == LINEAR_FEATURE_ID))

#Determine which streams are contained within each FAZ
# bcfp$FAZ <- NA
# for(f in 1:length(FAZ_Fr$FAZ_Name)) {
#   FAZ_pick <- FAZ_Fr[f,]
#   pick_tscapes <- lengths(st_intersects(bcfp,FAZ_pick)) > 0
#   bcfp$FAZ[pick_tscapes] <- FAZ_pick$FAZ_Acrony
# }


#create subsetted versions of bcfp for Fraser basin
bcfp_Fr <- st_contains(Fr_basin, bcfp)
bcfp_Fr <- bcfp[bcfp_Fr[[1]],]

#subsetted version with higher order streams
bcfp_Fr_high <- filter(bcfp_Fr, stream_order > 2)

# If encounter the error: 
# Error in wk_handle.wk_wkb(wkb, s2_geography_writer(oriented = oriented,  : 
# Loop 0 is not valid: Edge 2607 has duplicate vertex with edge 2625
# Then run:
#sf_use_s2(FALSE)

#------------------------------------------------------------------------------
# Load shoreline data (for mapping only)
#------------------------------------------------------------------------------

shoreline <- st_read(file.path(spatial_dat, "shoreline", "GSHHS_i_L1.shp")) %>%
  st_transform(crs = 4269)


#------------------------------------------------------------------------------
# PCIC grid points and polygon
#------------------------------------------------------------------------------

# Read in PCIC grid
grid_points <- read.csv(here("data", "freshwater", "processed-data", "PCIC-grid-points_bccoast.csv")) 
#read.csv("freshwater/data/processed-data/PCIC-grid-points_fraser.csv") 

# Convert grid points to spatial object 
grid_points <- st_as_sf(grid_points, coords = c("lon", "lat"), crs = 4269) %>%
  st_transform(4269)

#import polygon grid - see 2x_fw_create_inputs_grid.R file
grid_polys <- readRDS(file = here("data", "freshwater", "processed-data", "grid_polys_fw.rds")) %>%
  st_transform(4269)

#subset Fraser basin
pick_Fr <- lengths(st_intersects(grid_polys, Fr_basin)) > 0
grid_polys <- grid_polys[pick_Fr,]

pick_Fr <- lengths(st_intersects(grid_points, grid_polys)) > 0 
grid_points <- grid_points[pick_Fr,]


#-----------------------------------------------------------------------------
# Load PCIC indicator data
#-----------------------------------------------------------------------------

#get PCIC file names in directory
PCIC_files <- list.files(file.path(climate_dat, "PCIC_indicators"))
PCIC_files <- PCIC_files[grep(".aux.xml", PCIC_files, invert=TRUE)]

#load selected indicators for available time periods and combine into a star object
PCIC_file_choose <- PCIC_indies_pick
for(i in 1:length(PCIC_indies_pick)){
  pick_files <- PCIC_files[grepl(PCIC_file_choose[i], PCIC_files)]
  
  for(p in 1:length(t_periods)) {
    temp_star <- read_ncdf(file.path(climate_dat, "PCIC_indicators", pick_files[p]))
    
    if(p == 1)  ind_star <- temp_star
    else if(p > 1)  ind_star <- c(ind_star, temp_star, nms = t_periods)
    rm(temp_star)
  }
  
  if(i == 1) PCIC_indies <- ind_star
  else if(i > 1) PCIC_indies <- c(PCIC_indies, ind_star)
}

PCIC_file_choose <- PCIC_day_files
for(i in 1:length(PCIC_file_choose)){
  pick_files <- PCIC_files[grepl(PCIC_file_choose[i], PCIC_files)]
  
  for(p in 1:length(t_periods)) {
    temp_star <- read_ncdf(file.path(climate_dat, "PCIC_indicators", pick_files[p]))
    
    if(p == 1)  ind_star <- temp_star
    else if(p > 1)  ind_star <- c(ind_star, temp_star, nms = t_periods)
    rm(temp_star)
  }
  
  if(i == 1) PCIC_day <- ind_star
  else if(i > 1) PCIC_day <- c(PCIC_day, ind_star)
}

PCIC_file_choose <- PCIC_month_files
for(i in 1:length(PCIC_file_choose)){
  pick_files <- PCIC_files[grepl(PCIC_file_choose[i], PCIC_files)]
  
  for(p in 1:length(t_periods)) {
    temp_star <- read_ncdf(file.path(climate_dat, "PCIC_indicators", pick_files[p]))
    
    if(p == 1)  ind_star <- temp_star
    else if(p > 1)  ind_star <- c(ind_star, temp_star, nms = t_periods)
    rm(temp_star)
  }
  
  if(i == 1) PCIC_month <- ind_star
  else if(i > 1) PCIC_month <- c(PCIC_month, ind_star)
}

st_crs(PCIC_indies) <- 4269 #change CRS to NAD83/Albers from default of WGS84
st_crs(PCIC_day) <- 4269 #change CRS to NAD83/Albers from default of WGS84
st_crs(PCIC_month) <- 4269 #change CRS to NAD83/Albers from default of WGS84

#crop to Fraser basin
#PCIC_indies <- st_crop(PCIC_indies, grid_polys)
#PCIC_day <- st_crop(PCIC_day, grid_polys)
#PCIC_month <- st_crop(PCIC_month, grid_polys)



########################################################
## Load PCIC flow network model
#######################################################

#flow_fwa <- st_read(file.path(climate_dat, "Fraserflow", "fraser_ensemble_means_rcp45_2020_2100.gdb"))
#st_layers(file.path(climate_dat, "Fraserflow", "fraser_ensemble_means_rcp45_2020_2100.gdb"))


#join to BC fishpass model
#bcfp_flow <- bcfp %>%
#  left_join(select(as_tibble(flow_fwa), LINEAR_FEATURE_ID, min

#-----------------------------------------------------------------------------
# Load ENM prediction data
#-----------------------------------------------------------------------------

# ENMs_base_co <- st_read(here(dat_root, "Salmon-ENMs-2023-master", "Mapped_model_predictions.gdb"), layer = "Coho_GBMFav_streams_acc")
# ENMs_45_co <- st_read(here(dat_root, "Salmon-ENMs-2023-master", "Mapped_model_predictions.gdb"), layer = "COHO_BC_change_9_45_5")
# 
# st_layers(here(dat_root, "Salmon-ENMs-2023-master", "Mapped_model_predictions.gdb"))
# st_layers(here(dat_root, "Salmon-ENMs-2023-master", "FullExtent_accessible_bySpecies.gdb")) 
# st_layers(here(dat_root, "Salmon-ENMs-2023-master", "BC_network_variables_climateproj.gdb"))
# 
# reaches_ENM <- st_read(here(dat_root, "Salmon-ENMs-2023-master","BC_network_variables_climateproj.gdb"), layer = "BC_all_reaches_time_steps")
# midpoints_ENM <- st_read(here(dat_root, "Salmon-ENMs-2023-master","BC_network_variables_climateproj.gdb"), layer = "BC_all_midpoints_time_steps")
# 
# extent <- st_read(here(dat_root, "FullExtent_accessible_bySpecies.gdb"), layer = "BC_all_reaches_time_steps")
#           


#########################################################
# Load salmon spawner locations
#########################################################

chin_sp <- st_read(file.path(salmon_dat, "FIA", "2023 Chinook Master.gpx"), layer = "waypoints") %>%
  st_transform(crs = 4269)

coho_sp <- st_read(file.path(salmon_dat, "FIA", "2021 Coho Master.gpx"), layer = "waypoints") %>%
                st_transform(crs = 4269)

sox_layers <- st_layers(file.path(salmon_dat, "FIA", "Fraser Sockeye Spawning", "doc.kml"))

for(i in 1:length(sox_layers$name)) {
  
  temp <- st_read(file.path(salmon_dat, "FIA", "Fraser Sockeye Spawning", "doc.kml"), layer = sox_layers$name[i])
  
  if(i == 1) sockeye_sp <- temp
  if(i > 1) sockeye_sp <- bind_rows(sockeye_sp, temp)
}

#### Save files to Rdata objects         

save(PCIC_indies, PCIC_month, PCIC_day, grid_points, grid_polys, 
     Fr_basin, shoreline, cu_boundary, FAZ, FAZ_Fr,
     FWA_Fr_high, bcfp, bcfp_Fr, tscapes, tscapes_acc,
     chin_sp, coho_sp, sockeye_sp,
     file = here("data", "freshwater", "processed-data", paste0(today, "_fw_spatial_inputs.Rdata")))

#save(access, 
#     file = here("data", "freshwater", "processed-data", paste0(today, "BCfishpass_access.Rdata")))
