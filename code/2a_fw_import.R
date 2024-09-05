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

cu_boundary <- st_read(here(dat_root, "CU_boundaries", "fraser_cus.shp")) %>%
  st_transform(crs = 4269)

# Are all CUs in cu_run in cu_boundary?
cu_run$cuid %in% cu_boundary$CUID # Yes

#remove CUs that are not in the cu_boundary list
cu_run <- cu_run[cu_run$cuid %in% cu_boundary$CUID,]

#add species pooled column
cu_boundary$species_pooled <- cu_boundary$Species
cu_boundary$species_pooled[cu_boundary$Species %in% c("Pink-Odd", "Pink-Even")] <- "Pink"
cu_boundary$species_pooled[cu_boundary$Species %in% c("Sockeye-Lake", "Sockeye-River")] <- "Sockeye"


#------------------------------------------------------------------------------
# bcfishpass: spawning and rearing stream distributions for each species
# Nov 23, 2023: NOTE: these distributions are *usable* habitat and do not account
# for habitats that salmon may transit through.
#
# Updated to use version of database from https://www.hillcrestgeo.ca/outgoing/forPSF/
# Dated 2023-Dec-08 09:11 
#------------------------------------------------------------------------------

streams <- st_read(here(dat_root, "bcfishpass.gdb"), layer = "streams") %>%
  st_transform(crs = 4269)


# If encounter the error: 
# Error in wk_handle.wk_wkb(wkb, s2_geography_writer(oriented = oriented,  : 
# Loop 0 is not valid: Edge 2607 has duplicate vertex with edge 2625
# Then run:
sf_use_s2(FALSE)



#------------------------------------------------------------------------------
# Migration paths from spawning habitat to ocean entry point
# SKIP FOR NOW
#------------------------------------------------------------------------------
# mig_paths <- st_read(dsn = paste0(dat_root, "fw-migration/spawn_timing_migration_paths_wCU_fraser.shp")) %>% 
#   st_transform(crs = 4269)
# 
# cu_run$cuid %in% mig_paths$cuid # Doesn't include steelhead
# 
# mig_paths_start <- data.frame(
#   csv_id = mig_paths$csv_id,
#   lat = mig_paths$lat_snap,
#   lon = mig_paths$lon_snap) %>%
#   st_as_sf(coords = c("lon", "lat"), crs = 4269)

#------------------------------------------------------------------------------
# Load shoreline data (for mapping only)
#------------------------------------------------------------------------------

shoreline <- st_read(here(dat_root, "layers", "shoreline", "GSHHS_i_L1.shp")) %>%
  st_transform(crs = 4269)

#shoreline_Albers <- st_transform(shoreline, crs = 3005)

#------------------------------------------------------------------------------
# Load river basin data (for clipping primarily)
#------------------------------------------------------------------------------

basins <- st_read(here(dat_root, "layers", "BC_Basins", "BC_Basins_GoogleMapPL.shp")) %>%
  st_cast("POLYGON")
st_crs(basins) <- 4269

Fr_basin <- filter(basins, BASIN == "FRASER")   #fraser basin only


# determine basin where cu boundaries intersect and add to cu_boundary object

basin_int <- st_intersects(basins, cu_boundary, sparse = FALSE)
cu_boundary$basins <- as.character(NA)

for (i in 1:nrow(cu_boundary)) {
  vec_basins <- basins$BASIN[which(basin_int[-4,i] == TRUE)]
  vec_basins <- str_sort(vec_basins)
  cu_boundary$basins[i] <- str_flatten(vec_basins, collapse = ",")
  
}


# plot(filter(basins, BASIN == "FRASER")) 
# plot(shoreline, add = TRUE)
# plot(basins)
# plot(shoreline_Albers, add = TRUE)

#------------------------------------------------------------------------------
# PCIC grid points and polygon
#------------------------------------------------------------------------------

# Read in PCIC grid
grid_points <- read.csv(here("data", "freshwater", "processed-data", "PCIC-grid-points_bccoast.csv")) 
#read.csv("freshwater/data/processed-data/PCIC-grid-points_fraser.csv") 

# Convert grid points to spatial object 
grid_points <- st_as_sf(grid_points, coords = c("lon", "lat"), crs = 4269)

#import polygon grid - see 2x_fw_create_inputs_grid.R file
grid_polys <- readRDS(file = here("data", "freshwater", "processed-data", "grid_polys_fw.rds"))

#subset Fraser basin
pick_Fr <- lengths(st_intersects(grid_polys, Fr_basin)) > 0
grid_polys <- grid_polys[pick_Fr,]

pick_Fr <- lengths(st_intersects(grid_points, grid_polys)) > 0 
grid_points <- grid_points[pick_Fr,]

#plot(st_geometry(basins))
#plot(grid_points, add = TRUE)

#-----------------------------------------------------------------------------
# Load PCIC indicator data
#-----------------------------------------------------------------------------

#get PCIC file names in directory
PCIC_files <- list.files(here(dat_root, "PCIC_indicators"))

#load selected indicators for available time periods and combine into a star object
for(i in 1:length(PCIC_indies)){
  pick_files <- PCIC_files[grepl(PCIC_file_choose[i], PCIC_files)]
  
  for(p in 1:length(t_periods)) {
    temp_star <- read_ncdf(here(dat_root, "PCIC_indicators", pick_files[p]))
    
    if(p == 1)  ind_star <- temp_star
    else if(p > 1)  ind_star <- c(ind_star, temp_star, nms = t_periods)
    rm(temp_star)
  }
  
  if(i == 1) PCIC <- ind_star
  else if(i > 1) PCIC <- c(PCIC, ind_star)
}

st_crs(PCIC) <- 4269 #change CRS to NAD83 from default of WGS84

#crop to Fraser basin
PCIC <- st_crop(PCIC, grid_polys)

#plot(PCIC_star)

# peakFlow_star <- read_ncdf(here(dat_root, "PCIC_indicators", peakFlow_files[1]))
# 
# PCIC_indies <- read_stars(here(dat_root, "PCIC_indicators", PCIC_files))
# #hist <- read_ncdf(paste0(here(dat_root, "PCIC_indicators"), varNames$fileName[v], "_", model, "_VICGL-dynWat_rcp", rcp, "_", timeRanges[1], "_bccoast+fraser.nc"))
# 
# peakFlow <- loadPCIC_ind(
#   variable = "peakFlowDay_aClimMean", # Which variable to load? 
#   model = "ensMean",
#   rcp = 45 # Which GCM?
# )
# 
# freq19year <- loadPCIC_ind(
#   variable = "POT19freq_year_aClimMean", # Which variable to load? 
#   model = "ensMean",
#   rcp = 45 # Which GCM?
# )

save(PCIC, grid_points, grid_polys, Fr_basin, shoreline, streams, cu_boundary,
     file = here("data", "freshwater", "processed-data", paste0(today, "fw_spatial_inputs.Rdata")))
