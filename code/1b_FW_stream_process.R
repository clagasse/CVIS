# ==============================================================================
# CVIS Stream Process and Spatial Data Preparation (1b_FW_stream_process.R)
#
# Description:
#   Reads in raw spatial data files, extracts Fraser basin subsets, and processes 
#   them into standard R data frames and sf spatial objects. This script takes 
#   a while to run and should only be needed when new input files need to be 
#   generated.
#
# Workflow Steps:
#   1. Load setup environment and load spatial layers (basins, CU boundaries, FWA lakes, BCFishpass).
#   2. Process ecological niche models (ENM) favourability layers and stream flow GCMs.
#   3. Join stream network models on a common base stream network.
#   4. Calculate indicator variables (flow, temperature, ENM) for the combined spatial object.
#   5. Process Ruzzante statistical low flow projections.
#   6. Process PCIC grid data and model projections.
#
# Inputs:
#   - Spatial shapefiles and geopackages in paths$spatial
#   - ENM and flow projections in paths$climate
#
# Outputs:
#   - Processed R data objects (.Rds, .Rdata) in paths$fw and processed_data/freshwater/
#
# Dependencies:
#   - Requires 0_setup.R.
# ==============================================================================

library(here)
setwd(here())
source(file.path(here(), "code", "0_setup.R"))

library(janitor)

# ==================== 1. Stream Network & Spatial Layer Loading ====================

#--------------Basins polygons (for clipping primarily)--------------------

# BC Basins shapefile:  https://www.arcgis.com/home/item.html?id=7fe30c4a1cc34d14b560d868429bda35

basins <- st_read(file.path(paths$spatial, "BC_Basins", "BC_Basins_GoogleMapPL.shp"), quiet = TRUE) %>%
  st_cast("POLYGON")
st_crs(basins) <- 4269
basins <- st_transform(basins, crs = 3005)
Fr_basin <- filter(basins, BASIN == "FRASER")   # fraser basin only


save(basins, file = file.path(paths$fw, "basins_shp.Rds"))

cu_boundary <- st_read(file.path(paths$spatial, "CU_boundaries", "fraser_cus.shp")) %>%
  st_make_valid() %>%
  st_transform(crs = 3005)  %>%  # crs 3005 is NAD83/BC Albers
  left_join(select(cu_Fr, cuid, FULL_CU_IN, spp),
    join_by(CUID == cuid)) %>%
  filter(!is.na(FULL_CU_IN))

save(cu_boundary, file = file.path(paths$fw, "cu_boundary.Rds"))


# # determine basin where cu boundaries intersect and add to cu_boundary object
# basin_int <- st_intersects(basins, cu_boundary)
# cu_boundary$basins <- as.character(NA)
#
# for (i in 1:nrow(cu_boundary)) {
#   vec_basins <- basins$BASIN[which(basin_int[-4,i] == TRUE)]   #remove 4th column to exclude Fraser
#   vec_basins <- str_sort(vec_basins)
#   cu_boundary$basins[i] <- str_flatten(vec_basins, collapse = ",")
# }


#  FWA lakes layer (for plotting) ------------------------------------

lakes_fwa <- st_read(file.path(paths$spatial, "BC_FWA_LAKES", "FWA_LAKES_POLY.gpkg"))

lakes_Fr <- st_contains(Fr_basin, st_zm(lakes_fwa))
lakes_Fr <- lakes_fwa[lakes_Fr[[1]], ]

# intersections
# lakes_int <- st_intersects(lakes_Fr, cu_boundary)
# lakes_int$FULL_CU_IN <- as.character(NA)
#
# # for (i in 1:nrow(cu_boundary)) {
# #   vec_basins <- basins$BASIN[which(basin_int[-4,i] == TRUE)]   #remove 4th column to exclude Fraser
# #   vec_basins <- str_sort(vec_basins)
# #   cu_boundary$basins[i] <- str_flatten(vec_basins, collapse = ",")
# # }

save(lakes_Fr, file = file.path(paths$fw, "BC_FWA_LAKES_FR.Rds"))


# BC FISHPASS stream accessibility and linear habitat model------------------

# Load BC Fishpass models and subset to Fraser basin and accessible streams

# Info at: https://smnorris.github.io/bcfishpass/index.html
# BCFishpass has two different models
# accessibility of streams to anadromous fish
# linear spawning/rearing habitat model

# PSF version is downloaded here: https://www.hillcrestgeo.ca/outgoing/forPSF/
# Dated 2023-Dec-08 09:11

# BC Gov weekly distribution here: https://bcgov.github.io/bc_freshwater_fish_habitat_accessibility_model/04_data_distribution.html

## Habitat model - this takes a long time to read

unzip(file.path(paths$spatial, "BCFishpass", "bcfishpass_streams_habitat", "bcfishpass_streams_2024-12-09.zip"))
bcfph <- st_read(file.path(paths$spatial, "BCFishpass", "bcfishpass_streams_habitat", "bcfishpass_streams_2024-12-09.gpkg")) # %>%

bcfph_Fr <- st_contains(Fr_basin, st_zm(bcfph))  # subset Fraser basin
bcfph_Fr <- bcfph[bcfph_Fr[[1]], ]

# st_write(bcfph_Fr, file.path(paths$spatial, "BCFishpass", "bcfishpass_streams_habitat",
#                              "bcfishpass_Fraser_streams_2024-12-09.gpkg"), driver = "GPKG")

## Accessibility model
bcfp <- st_read(file.path(paths$spatial, "BCFishpass", "freshwater_fish_habitat_accessibility_MODEL",
  "freshwater_fish_habitat_accessibility_MODEL.gpkg"), layer = "model_access")

bcfp_Fr <- st_contains(Fr_basin, st_zm(bcfp))  # subset Fraser basin
bcfp_Fr <- bcfp[bcfp_Fr[[1]], ]

# st_write(bcfp_Fr, file.path(paths$spatial, "BCFishpass", "freshwater_fish_habitat_accessibility_MODEL",
#                             "Fraser_fish_habitat_accessibility_MODEL.gpkg"), driver = "GPKG")

# combine accessibility and habitat models within Fraser basin
bcfpc <- bcfph_Fr %>%
  left_join(select(data.table(bcfp_Fr), segmented_stream_id, linear_feature_id, model_access_salmon, model_access_steelhead),
    join_by(segmented_stream_id, linear_feature_id),
    multiple = "first")


#----- FWA Query and Process------------------------------------

# Freshwater Atlas is already used by BC Fishpass, but we need the original FWA Watershed Codes for
# subsetting operations in some scripts

# load full FWA to get stream lengths for joining to BC fishpass
# FWA_Fr <- st_read(file.path(paths$spatial, "FWA_Fraser", "FWA_Fraser.shp"))

# FWA_watersheds <- st_layers(file.path(paths$spatial, "FWA", "FWA_STREAM_NETWORKS_SP.gdb"))

# get Fraser watershed codes for loading layers
Fr_codes <- unique(bcfpc$watershed_group_code)

# load watershed group codes within Fraser from full FWA stream network
FWA_Fr <- future_lapply(Fr_codes, function(layer) {
  st_read(file.path(paths$spatial, "FWA", "FWA_STREAM_NETWORKS_SP.gdb"), layer = layer)
})

# flatten list into single sf
FWA_Fr <- do.call(rbind, FWA_Fr)

# join watershed code
bcfpc <- bcfpc %>%
  left_join(data.table(FWA_Fr) %>%
    select(LINEAR_FEATURE_ID, FWA_WATERSHED_CODE),
  by = c("linear_feature_id" = "LINEAR_FEATURE_ID")) %>%
  relocate(FWA_WATERSHED_CODE, .after = linear_feature_id)

# check matches successful
sum(is.na(bcfpc$FWA_WATERSHED_CODE))

# create salmon rearing and spawning modelled habitat column
bcfpc <- bcfpc %>%
  mutate(model_habitat_salmon = if_else(model_spawning_ch == TRUE | model_spawning_cm == TRUE |
    model_spawning_co == TRUE | model_spawning_pk == TRUE |
    model_spawning_sk == TRUE |
    model_rearing_ch == TRUE | model_rearing_co == TRUE |
    model_rearing_sk == TRUE, TRUE, FALSE)) %>%
  mutate(model_habitat_ch = if_else(model_spawning_ch == TRUE | model_rearing_ch == TRUE, TRUE, FALSE),
    model_habitat_cm = if_else(model_spawning_cm == TRUE, TRUE, FALSE),
    model_habitat_co = if_else(model_spawning_co == TRUE | model_rearing_co == TRUE, TRUE, FALSE),
    model_habitat_pk = if_else(model_spawning_pk == TRUE, TRUE, FALSE),
    model_habitat_sk = if_else(model_spawning_sk == TRUE | model_rearing_sk == TRUE, TRUE, FALSE),
  )

# save as new .R object
# st_write(bcfpc, file.path(paths$spatial, "BCFishpass", "freshwater_fish_habitat_accessibility_MODEL",
#                             "Fraser_fish_habitat_accessibility_MODEL.gpkg"), driver = "GPKG")
save(bcfpc, file = file.path(paths$fw, "BCFP_combined_Fr.Rds"))

# subset accessible streams only
bcfpa <- filter(bcfpc, model_access_salmon %in% c("OBSERVED", "INFERRED"))
# remove stream order 1
bcfpl <- filter(bcfpc, stream_order >= 2)

save(bcfpa, file = file.path(paths$fw, "BCFP_combined_accessible_Fr.Rds"))
# st_write(bcfpa, file.path(paths$spatial, "BCFP_accessible_Fr.gdb"), driver = "OpenFileGDB" )

# save a version with first order streams excluded
# first order streams represent ~2/3 of the streams but have less than 1% modelled rearing and spawning reaches
# save(bcfpl, file = file.path(paths$fw, "BCFP_combined_order2_Fr.Rds"))


# create stream network data table
bcfpmod <- as.data.table(bcfpa) %>%
  select(segmented_stream_id, linear_feature_id, FWA_WATERSHED_CODE, channel_width, length_metre,
    mad_m3s, upstream_area_ha, gradient, gnis_name, model_access_salmon,
    model_habitat_salmon,
    model_habitat_ch, model_habitat_cm, model_habitat_co, model_habitat_pk, model_habitat_sk)



# ==================== 2. Model Processing into R Spatial Object ====================

#---------------- Ecological Niche Models --------------------------------------

# load(file.path(paths$fw, "BCFP_combined_Fr.Rds")) #load bcfp stream network
# load(file.path(paths$fw, "BCFP_combined_accessible_Fr.Rds"))

### importing ENM favourability model layers
# Provided by Josie Iacarella, Sep 2025

# historic layers
hist_layers <- st_layers(file.path(paths$climate, "Salmon_ENMs", "Baseline_accessible streams.gdb"))$name
ENM_hist_list <- lapply(hist_layers, function(layer_name) {
  st_read(file.path(paths$climate, "Salmon_ENMs", "Baseline_accessible streams.gdb"), layer = layer_name)
})
names(ENM_hist_list) <-  sub("_.*", "", hist_layers)
# RCP 45
RCP45_mc_layers <- st_layers(file.path(paths$climate, "Salmon_ENMs", "Future_RCP45_2041to2060_allstreams.gdb"))$name
ENM_45_mc_list <- lapply(RCP45_mc_layers, function(layer_name) {
  st_read(file.path(paths$climate, "Salmon_ENMs", "Future_RCP45_2041to2060_allstreams.gdb"), layer = layer_name)
})
names(ENM_45_mc_list) <-  sub("_.*", "", RCP45_mc_layers)
RCP45_ec_layers <- st_layers(file.path(paths$climate, "Salmon_ENMs", "Future_RCP45_2081to2100_allstreams.gdb"))$name
ENM_45_ec_list <- lapply(RCP45_ec_layers, function(layer_name) {
  st_read(file.path(paths$climate, "Salmon_ENMs", "Future_RCP45_2081to2100_allstreams.gdb"), layer = layer_name)
})
names(ENM_45_ec_list) <-  sub("_.*", "", RCP45_ec_layers)
# RCP 85
RCP85_mc_layers <- st_layers(file.path(paths$climate, "Salmon_ENMs", "Future_RCP85_2041to2060_allstreams.gdb"))$name
ENM_85_mc_list <- lapply(RCP85_mc_layers, function(layer_name) {
  st_read(file.path(paths$climate, "Salmon_ENMs", "Future_RCP85_2041to2060_allstreams.gdb"), layer = layer_name)
})
names(ENM_85_mc_list) <-  sub("_.*", "", RCP85_mc_layers)
RCP85_ec_layers <- st_layers(file.path(paths$climate, "Salmon_ENMs", "Future_RCP85_2081to2100_allstreams.gdb"))$name
ENM_85_ec_list <- lapply(RCP85_ec_layers, function(layer_name) {
  st_read(file.path(paths$climate, "Salmon_ENMs", "Future_RCP85_2081to2100_allstreams.gdb"), layer = layer_name)
})
names(ENM_85_ec_list) <-  sub("_.*", "", RCP85_ec_layers)



# Get the common species names
species <- intersect(names(ENM_hist_list), intersect(names(ENM_45_ec_list), names(ENM_85_ec_list)))

fix_column_name <- function(df) {
  if ("SDM_accID_" %in% names(df)) {
    names(df)[names(df) == "SDM_accID_"] <- "SDM_accID_v3"
  }
  names(df) <- gsub("RASTERVALU", "Fav", names(df))

  return(df)
}

# Apply to each list
ENM_hist_list <- lapply(ENM_hist_list, fix_column_name)
ENM_45_mc_list   <- lapply(ENM_45_mc_list, fix_column_name)
ENM_45_ec_list   <- lapply(ENM_45_ec_list, fix_column_name)
ENM_85_mc_list   <- lapply(ENM_85_mc_list, fix_column_name)
ENM_85_ec_list   <- lapply(ENM_85_ec_list, fix_column_name)

### Join ENM 45 and 85 to each other using SDM ID all

# Create a list of joined results for each species
ENM_joined_list <- map(species, function(sp) {
  rcp45_mc <- ENM_45_mc_list[[sp]]

  rcp85_mc <- ENM_85_mc_list[[sp]]
  rcp45_ec <- ENM_45_ec_list[[sp]]
  rcp85_ec <- ENM_85_ec_list[[sp]]
  rcp85_mc_df <- st_drop_geometry(rcp85_mc)
  rcp85_ec_df <- st_drop_geometry(rcp85_ec)
  rcp45_ec_df <- st_drop_geometry(rcp45_ec)
  # hist_df <- st_drop_geometry(hist)

  # join_rcp <- st_join(rcp45, rcp85, left = FALSE, suffix = c("_45", "_85"))
  join_rcp <- left_join(rcp45_mc, rcp85_mc_df, join_by(SDM_ID_all), suffix = c("_45_3", "_85_3"))
  join_rcp <- left_join(join_rcp, rcp45_ec_df, join_by(SDM_ID_all))
  join_rcp <- left_join(join_rcp, rcp85_ec_df, join_by(SDM_ID_all), suffix = c("_45_5", "_85_5"))
  # left_join(rcp45_ec_df, join_by(SDM_ID_all), suffix = c("", "_45_5")) %>%
  # left_join(rcp85_ec_df, join_by(SDM_ID_all), suffix = c("", "_85_5"))

  # join_all <- left_join(join_rcp, hist_df, by = join_by(SDM_ID_all == SDM_accID_v3), suffix = c("", "_hist"))
  # join_all <- st_join(join_rcp, hist, left = FALSE, suffix = c("", "_hist"))

  # Rename Fav columns to include species name
  names(join_rcp) <- gsub("Fav", paste0("Fav_", sp), names(join_rcp))

  return(join_rcp)
})
names(ENM_joined_list) <- species

ENM_df <- ENM_joined_list[[1]]
for (i in 2:length(ENM_joined_list)) {
  temp <- st_drop_geometry(ENM_joined_list[[i]]) %>%
    select(SDM_ID_all, contains("Fav"))
  ENM_df <- left_join(ENM_df, temp,
    by = c("SDM_ID_all"))
}

ENM_df <- st_transform(ENM_df, 3005)
ENM_df <- ENM_df %>%
  select(-c(contains("Shape_Length")))
ENM_df$Shape_Length <- ENM_df$Shape_Leng
ENM_df$Shape_Leng <- NULL

### Join historic values separately, as each species has a different subset of streams

# Rename Fav columns to include species name and hist
ENM_hist_list <- map(species, function(sp) {
  hist <- ENM_hist_list[[sp]]
  names(hist) <- gsub("Fav", paste0("Fav_", sp, "_hist_0"), names(hist))
  return(hist)
})
names(ENM_hist_list) <-  sub("_.*", "", hist_layers)

# join all tables into one big table using SDM_accID_v3 key join
ENM_hist_joined <- ENM_hist_list[[1]]
for (i in 2:length(ENM_hist_list)) {
  temp <- st_drop_geometry(ENM_hist_list[[i]]) %>%
    select(SDM_accID_v3, contains("Fav"))
  ENM_hist_joined <- left_join(ENM_hist_joined, temp,
    by = c("SDM_accID_v3"))
}
ENM_hist_joined <- st_transform(ENM_hist_joined, 3005)

# spatial join historic and projected ENM objects
ENM_df <- st_join(ENM_df, ENM_hist_joined)

save(ENM_df, file = file.path(paths$fw, "ENM_all_species.Rds"))
rm(ENM_hist_list, ENM_joined_list, ENM_45_list, ENM_85_list)

# ENM_df$shape_diff <- ENM_df$Shape_Length.x - ENM_df$Shape_Length.y

# cu_boundary_i <- cu_boundary[cu_boundary$FULL_CU_IN == "SEL-05-02", ]
# # cu_pick <- cu_boundary[cu_boundary$FULL_CU_IN == cu_i], ]
# pick_st <- lengths(st_intersects(ENM_df_keyjoin, cu_boundary_i)) > 0
# ENM_df_pick <- ENM_df_keyjoin[pick_st, ]
#
# pick_hist <- lengths(st_intersects(ENM_hist_joined, cu_boundary_i)) > 0
# ENM_hist_pick <- ENM_hist_joined[pick_hist, ]
#
# pick_joined <- lengths(st_intersects(ENM_df_joined, cu_boundary_i)) > 0
# ENM_df_pick <- ENM_df_joined[pick_st, ]




#----------------- Stream level flow data -----------------------------------

# pflow <- st_read(file.path(paths$climate, "Fraserflow", "fraser_ensemble_means_rcp45_2020_2100.gdb")) %>%
#   as.data.table()

# individual model projections for historical flow
# these are very large files and need to be unzipped first

pflow_names <- list.files(file.path(paths$climate, "Fraserflow", "modelled_flow.zip", "data"))

pflow_GCMs <- list()  # initialize list to hold GCM data

for (i in 1:length(pflow_names)) {
  # read in each GCM file
  pflow_GCMs[[i]] <- fread(file.path(paths$climate, "Fraserflow", "modelled_flow", "data", pflow_names[i]),
    select = c("linear_feature_id", "time_id", "description_id", "mean_runoff_m3s"))
}

names(pflow_GCMs) <- pflow_names

save(pflow_GCMs, file = file.path(paths$fw, "stream_flow_GCMs_Fr.Rds"))



#-----   Extract August or Nov-Jan flow projections and merge with historic values

# choose months to extract
## use either August (8) or Nov-Jan (1,11,12)
month_pick <- c(8)

# choose stream base network to subset using linear id
base_network <- switch(2, "bcfpa", "tscapes")

if (base_network == "bcfpa") {
  load(file.path(paths$fw, "BCFP_combined_accessible_Fr.Rds"))
  # get index of linear_feature_ids that are accessible
  lf_id_access <- bcfpa$linear_feature_id[!is.na(bcfpa$linear_feature_id)]
  rm(bcfpa)
}

# or use Thermalscapes stream network as base
if (base_network == "tscapes") {
  tscapes <- st_read(file.path(paths$climate, "bc_stream_thermalscapes.gdb"),
    layer = "thermalscape_fraser")
  lf_id_access <- tscapes$linear_feature_id[!is.na(tscapes$linear_feature_id)]
  rm(tscapes)
}


# load GCM flow data
load(file.path(paths$fw, "stream_flow_GCMs_Fr.Rds"))

# import historical flow data object
hflow <- st_read(file.path(paths$climate, "Fraserflow", "Historic_Flow_Data.gdb")) %>%
  as.data.table() %>%
  select(-contains("min_flow"), -contains("max_flow")) # %>%#remove min and max year values from periods




pflow_names <- names(pflow_GCMs)

# code key for time period
flow_desc_id <- read_csv(file.path(paths$climate, "Fraserflow", "description_id.csv"))

desc_id_lookup <- tribble(
  ~description_id, ~period,
  grep("2020", flow_desc_id$description), "2",
  grep("2040", flow_desc_id$description), "3",
  grep("2060", flow_desc_id$description), "4",
  grep("2080", flow_desc_id$description), "5",
  grep("1981", flow_desc_id$description), "0",
)

# subset accessible streams only using Linear Feature IDs
pflow_GCMs <- lapply(pflow_GCMs, function(dt) {
  # setindex(dt, linear_feature_id) # Create index (does not sort)
  dt[linear_feature_id %in% lf_id_access & time_id %in% month_pick]
})

gc()

# dt <- pflow_GCMs[[1]]

pflow_GCMs <- lapply(pflow_GCMs, function(dt) {
  if (length(month_pick) > 1) {
    # Step 1: Reshape to wide format
    dt <- dcast(dt, linear_feature_id + description_id ~ time_id,
      value.var = "mean_runoff_m3s", fun.aggregate = mean)
    # Step 2: Compute row-wise average across time columns
    dt <- dt[, mean_runoff_m3s := rowMeans(.SD, na.rm = TRUE), .SDcols = as.character(month_pick)]
    dt[, (as.character(month_pick)) := NULL]
    dt$time_id <- 18
  }

  dt[order(linear_feature_id, description_id, time_id)]  # Sort by linear_feature_id and time_id

  # dt[, avg_AB := rowMeans(.SD), .SDcols = c("A", "B")]
})



# get mean for each scenario and combine into data.table
for (i in 1:2) {

  if (i == 1) rcp_pick <- "45"
  if (i == 2) rcp_pick <- "85"

  rcp_names <- grep(rcp_pick, pflow_names, value = TRUE)

  # Extract the value from each table
  flow_values <- lapply(pflow_GCMs[rcp_names], function(dt) dt[["mean_runoff_m3s"]])

  # Combine into a matrix: each column is from one table, rows align
  flow_matrix <- as.data.table(flow_values)

  # Extract the part between the second and third underscores
  model_names <- sapply(strsplit(names(flow_matrix), "_"), function(x) x[3])
  model_names[1] <- "access1"  # fix different name between files

  names(flow_matrix) <- model_names

  # Compute row-wise stats using vectorized functions
  flow_table <- flow_matrix[, .(
    linear_feature_id = pflow_GCMs[[1]]$linear_feature_id,  # Assuming all have the same linear_feature_id
    time_id = pflow_GCMs[[1]]$time_id,  # Assuming all have the same time_id
    description_id = pflow_GCMs[[1]]$description_id,
    scenario = rcp_pick,
    mean = rowMeans(.SD, na.rm = TRUE)
    # min = do.call(pmin, .SD),
    # max = do.call(pmax, .SD)
  )]

  flow_table <- cbind(flow_table, flow_matrix)

  if (i == 1) {
    flow_summary <- flow_table
  } else {
    flow_summary <- rbind(flow_summary, flow_table, use.names = FALSE)
  }

}

rm(flow_table, flow_matrix, flow_values)

# get period using description id
flow_summary <- flow_summary %>%
  mutate(period = case_when(description_id %in% unlist(desc_id_lookup[[1]][1]) ~ unlist(desc_id_lookup[[2]][1]),
    description_id %in% unlist(desc_id_lookup[[1]][2]) ~ unlist(desc_id_lookup[[2]][2]),
    description_id %in% unlist(desc_id_lookup[[1]][3]) ~ unlist(desc_id_lookup[[2]][3]),
    description_id %in% unlist(desc_id_lookup[[1]][4]) ~ unlist(desc_id_lookup[[2]][4]),
    description_id %in% unlist(desc_id_lookup[[1]][5]) ~ unlist(desc_id_lookup[[2]][5]))) %>%
  select(-description_id)

flow_summary_long <- flow_summary %>%
  pivot_longer(cols = c(mean, all_of(model_names)),
    names_to = "model",
    values_to = "value")

hflow_long <- hflow %>%
  mutate(scenario = "historical",
    period = "0") %>%
  filter(linear_feature_id %in% lf_id_access) %>%
  rename_with(~ str_sub(.x, end = -3), starts_with("mean_flow")) %>% # remove _1 from col names
  pivot_longer(
    cols = starts_with("mean_flow_m3s_"),
    names_to = c("time_id"),
    names_prefix = "mean_flow_m3s_",
    values_to = "value"
  ) %>%
  mutate(time_id = as.integer(time_id),
    model = "mean") %>%
  filter(time_id %in% c(month_pick, 17)) %>%
  select(linear_feature_id, time_id, scenario, period, model, value) %>%
  as.data.table()


all_flow <- bind_rows(flow_summary_long, hflow_long)

# Step 2: Pivot wider using period, time_id, and scenario as names
all_flow_wide <- all_flow %>%
  pivot_wider(
    names_from = c(scenario, model, period, time_id),
    values_from = value,
    names_sep = "_",
    names_prefix = "flow_",
    values_fn = mean   # some linear features have repeats, so this takes the average of values
  )

# save(all_flow_wide, file = file.path(paths$fw, "stream_flow_processed.Rds"))

if (length(month_pick) == 1) {
  if (base_network == "bcfpa") {
    fwQ8 <- bcfpmod %>%
      left_join(all_flow_wide,
        join_by(linear_feature_id),
        multiple = "first")
    save(fwQ8, file = file.path(paths$fw, "stream_flow_August_Fr_accessible.Rds"))
  }
  if (base_network == "tscapes") {
    fwQ8 <- all_flow_wide
    save(fwQ8, file = file.path(paths$fw, "stream_flow_August_Fr_tscapes.Rds"))
  }
}

if (length(month_pick) > 1) {

  fwQNDJ <- all_flow_wide %>%
    mutate(flow_historical_mean_0_18 =
      rowMeans(select(., all_of(paste0("flow_historical_mean_0_", month_pick)))))

  if (base_network == "bcfpa") {
    fwQNDJ <- bcfpmod %>%
      left_join(fwQNDJ,
        join_by(linear_feature_id),
        multiple = "first")

    save(fwQNDJ, file = file.path(paths$fw, "stream_flow_NovDecJan_Fr_accessible.Rds"))
  }
  if (base_network == "tscapes") save(fwQNDJ, file = file.path(paths$fw, "stream_flow_NovDecJan_Fr_tscapes.Rds"))
}

# save historic flow object - NOT NEEDED
# hflow <- bcfpmod %>%
#   left_join(hflow,
#     join_by(linear_feature_id == LINEAR_FEATURE_ID),
#     multiple = "first")
#
# if (base_network == "bcfpa") save(hflow, file = file.path(paths$fw, "stream_flow_historic_Fr_accessible.Rds"))
# if (base_network == "tscapes") save(hflow, file = file.path(paths$fw, "stream_flow_historic_Fr_tscapes.Rds"))




# Temperature -------------------------------------------------------------

# get temperature stations and save to R object
# note that some gauge locations have been adjusted to preserve privacy
Tw_stations <- st_read(file.path(paths$climate, "Tw_stations.gdb"), quiet = TRUE)

save(Tw_stations, file = file.path(paths$fw, "Tw_stations.Rds"))

## 7 Day Equivalent Model (7DECM) stream temperature
# not currently available online
# T7DEC <- read_csv(file.path(paths$climate, "7DEC", "ThreshRisk_7DEC_Fraser.csv")) %>%
#   as.data.table() %>%
#   mutate(Risk20_9_45_3 = ifelse(Tav_9_45_3 < 20 & ThiPI_9_45_3 < 20, "Low", ifelse(Tav_9_45_3 < 20 & ThiPI_9_45_3 > 20, "Moderate",
#     ifelse(Tav_9_45_3 > 20 & TlowPI_9_45_3 < 20, "High",
#       "Severe"))),
#   Risk24_9_45_3 = ifelse(Tav_9_45_3 < 24 & ThiPI_9_45_3 < 24, "Low", ifelse(Tav_9_45_3 < 24 & ThiPI_9_45_3 > 24, "Moderate",
#     ifelse(Tav_9_45_3 > 24 & TlowPI_9_45_3 < 24, "High",
#       "Severe"))))

# Risk16_mod_len = Risk16_9_45_3 %in% c("Moderate", "High", "Very High") * Shape_Length,
# Risk20_mod_len = Risk20_9_45_3 %in% c("Moderate", "High", "Very High") * Shape_Length,
# Risk24_mod_len = Risk24_9_45_3 %in% c("Moderate", "High", "Very High") * Shape_Length)

# join temperature models to bcfp
# fwT <- bcfpmod %>%
#   left_join(tscapes,
#     join_by(linear_feature_id == LINEAR_FEATURE_ID),
#     relationship = "many-to-one")
# %>%
#   left_join(select(T7DEC, -c(STREAM_ORDER, region)),
#     join_by(linear_feature_id == LINEAR_FEATURE_ID),
#     multiple = "first")




#' Flow-only structural renaming
#'
#' Transforms flow_* columns from patterns like:
#'   - flow_rcp85_mpi_4_8      -> flow8_mpi_85_4
#'   - flow_historical_mean_0_8 -> flow8_mean_0_0
rename_flow_columns_step1 <- function(df) {
  stopifnot(!is.null(names(df)))
  old <- names(df)
  new <- old
  
  # 1) Replace 'historical' with '0' only for flow_ columns
  new <- sub("^(flow)_historical_", "\\1_0_", new, perl = TRUE)
  
  # 2) Remove literal 'rcp' but keep the numeric code (rcp45 -> 45), only for flow_...
  new <- sub("^(flow)_rcp(\\d{1,2})_", "\\1_\\2_", new, perl = TRUE)
  
  # 3) Rebuild:
  #    A) flow_<rcp>_<model>_<g>_<last>  -> flow<last>_<model>_<rcp>_<g>
  new <- sub("^(flow)_(\\d{1,2})_([^_]+)_(\\d+)_(\\d+)$", "\\1\\5_\\3_\\2_\\4", new, perl = TRUE)
  
  #    B) flow_0_<model>_<g>_<last>      -> flow<last>_<model>_0_<g>
  new <- sub("^(flow)_0_([^_]+)_(\\d+)_(\\d+)$", "\\1\\4_\\2_0_\\3", new, perl = TRUE)
  
  # Apply back
  names(df) <- new
  df
}

#' Remap model token in flow columns to PCIC numeric codes
#'
#' Expects flow columns already in the form: flow<last>_<model>_<rcp>_<g>
#' Replaces <model> with its PCIC code using the provided mapping.
#' Unknown models are left as-is (e.g., "mean").

map_gcm_models_to_code_step2 <- function(df, gcm_codes) {
  stopifnot(!is.null(names(df)))
  if (is.null(names(gcm_codes)) || !all(nzchar(names(gcm_codes)))) {
    stop("gcm_codes must be a named character vector: codes -> model names.")
  }
  
  # Invert mapping to model -> code
  model_to_code <- stats::setNames(names(gcm_codes), gcm_codes)
  
  remap_one <- function(nm) {
    # Expecting: flow<last>_<model>_<rcp>_<g>
    m <- stringr::str_match(nm, "^(flow\\d+)_([^_]+)_(\\d{1,2})_(\\d+)$")
    if (is.na(m[1])) return(nm)  # no match -> unchanged
    
    prefix <- m[2]  # "flow<last>"
    model  <- m[3]
    rcp    <- m[4]
    g      <- m[5]
    
    # Replace model if mapped; otherwise keep as-is (e.g., "mean")
    model_code <- if (model %in% names(model_to_code)) model_to_code[[model]] else model
    paste0(prefix, "_", model_code, "_", rcp, "_", g)
  }
  
  names(df) <- vapply(names(df), remap_one, character(1))
  df
}

# ==================== 3. Join Stream Network Models on Common Base Network ====================

# choose stream base network
base_network <- switch(2, "bcfpa", "tscapes")

## load R objects/ spatial data layers

## BC fishpass
load(file.path(paths$fw, "BCFP_combined_Fr.Rds")) # load bcfp stream network

## Thermalscapes august stream temperature
# Accessed from:  https://datadryad.org/dataset/doi:10.5061/dryad.bzkh189fk#readme
tscapes <- st_read(file.path(paths$climate, "bc_stream_thermalscapes.gdb"), layer = "thermalscape_fraser") %>%
  clean_names()
st_geometry(tscapes) <- "shape"

# Cumulative threat score for Fraser streams
fwct <- st_read(file.path(paths$spatial, "CumulativeThreatScore", "CumulativeThreat_FRB.shp")) %>%
  as.data.table() %>%
  clean_names()  %>%
  rename(pollution_z = pollution,
         cthr_anad = ct_anad) %>%    #standardize names of cumulative threat columns with cthr prefix
  dplyr::rename_with(
    ~ paste0("cthr_", sub("_z$", "", .x)),
    .cols = dplyr::ends_with("_z")
  )


# ENM
load(file.path(paths$fw, "ENM_all_species.Rds")) # %>%
ENM_df <- clean_names(ENM_df)

# August and Nov-Jan flow  (fwQ8 and fwQNDJ)
if (base_network == "tscapes") {
  load(file.path(paths$fw, "stream_flow_August_Fr_tscapes.Rds")) %>%
    as.data.table()
  load(file.path(paths$fw, "stream_flow_NovDecJan_Fr_tscapes.Rds")) %>%
    as.data.table()
}
if (base_network == "bcfpa") {
  load(file.path(paths$fw, "stream_flow_August_Fr_accessible.Rds")) %>%
    clean_names()
  load(file.path(paths$fw, "stream_flow_NovDecJan_Fr_accessible.Rds")) %>%
    clean_names()
}


#rename flow columns for consistency with temperature column names

# Step 1: structural renaming (flow-only)
fwQ8 <- rename_flow_columns_step1(fwQ8)
# Step 2: map model names to PCIC numeric codes
fwQ8 <- map_gcm_models_to_code_step2(fwQ8, gcm_codes)

# Step 1: structural renaming (flow-only)
fwQNDJ <- rename_flow_columns_step1(fwQNDJ)
# Step 2: map model names to PCIC numeric codes
fwQNDJ <- map_gcm_models_to_code_step2(fwQNDJ, gcm_codes)

# combine on common base network
if (base_network == "bcfpa") {
  # subset accessible streams only
  bcfpa <- filter(bcfpc, model_access_salmon %in% c("OBSERVED", "INFERRED"))

  fw_models <- bcfpa %>%
    select(segmented_stream_id, linear_feature_id, FWA_WATERSHED_CODE, channel_width, length_metre,
      mad_m3s, upstream_area_ha, gradient, gnis_name, model_access_salmon,
      model_rearing_ch, model_rearing_co, model_rearing_sk,
      model_spawning_ch, model_spawning_co, model_spawning_cm, model_spawning_pk, model_spawning_sk,
      model_habitat_salmon,
      model_habitat_ch, model_habitat_cm, model_habitat_co, model_habitat_pk, model_habitat_sk)

  # get rid of multiple matches by taking first observation only
  fw_models <- bcfpa %>%
    left_join(st_drop_geometry(tscapes),
      by = join_by(linear_feature_id),
      multiple = "first")

  # join cumulative threats model to bcfp
  fwct <- bcfpmod %>%
    left_join(select(fwct, -c(WATERSHED_, WATERSHED1, watershe_1)),
      join_by(linear_feature_id == LINEAR_FEA),
      multiple = "first")

}

# sub4 <- fw_models[fw_models$linear_feature_id == 700732747,]
# sub2 <- fw_models_cu[fw_models_cu$linear_feature_id == 700732747,]

if (base_network == "tscapes") {
  bcfpmod <- as.data.table(bcfpc) %>%
    select(segmented_stream_id, linear_feature_id, channel_width, length_metre, downstream_route_measure,
      mad_m3s, upstream_area_ha, gradient, gnis_name, model_access_salmon,
      model_rearing_ch, model_rearing_co, model_rearing_sk,
      model_spawning_ch, model_spawning_co, model_spawning_cm, model_spawning_pk, model_spawning_sk,
      model_habitat_salmon,
      model_habitat_ch, model_habitat_cm, model_habitat_co, model_habitat_pk, model_habitat_sk)

  fw_models <- tscapes %>%
    left_join(bcfpmod,
      by = join_by(linear_feature_id)) %>%
    group_by(linear_feature_id) %>%
    arrange(desc(model_spawning_sk), desc(model_habitat_salmon), downstream_route_measure) %>%
    slice(1) %>%
    ungroup() %>%
    relocate(channel_width, length_metre, segmented_stream_id, downstream_route_measure,
      mad_m3s, upstream_area_ha, gradient, gnis_name, model_access_salmon,
      model_rearing_ch, model_rearing_co, model_rearing_sk,
      model_spawning_ch, model_spawning_co, model_spawning_cm, model_spawning_pk, model_spawning_sk,
      model_habitat_salmon,
      model_habitat_ch, model_habitat_cm, model_habitat_co, model_habitat_pk, model_habitat_sk, .after = fwa_watershed_code)
  # join cumulative threats
  fw_models <- fw_models %>%
    left_join(select(fwct, -any_of(c("watershed_", "watershed1", "watershe_1", "geometry", "shape_leng", "length_km"))),
      join_by(linear_feature_id == linear_fea),
      multiple = "first")
  # join Aug flow
  fw_models <- fw_models %>%
    left_join(fwQ8,
      join_by(linear_feature_id),
      multiple = "first")
  # join Nov-Jan flow
  fw_models <- fw_models %>%
    left_join(select(fwQNDJ, -any_of("flow17_9_0_0")),
      join_by(linear_feature_id),
      multiple = "first")

  # spatial join with ENM
  fw_models <- fw_models %>%
    st_join(select(ENM_df, -any_of("shape_length")),
      left = TRUE) %>%
    group_by(linear_feature_id) %>%
    arrange(fav_sockeye_hist_0, fav_pink_hist_0, fav_chinook_hist_0) %>%
    slice(1) %>%
    ungroup()

  save(fw_models, file = file.path(paths$fw, "fw_models_tscapes.Rds"))

}

# ==================== 4. Calculate Indicators for Combined Model Object ====================

load(file.path(paths$fw, "fw_models_tscapes.Rds"))
fw_models_df <- st_drop_geometry(fw_models)


### High flow stats

flow_long <- fw_models_df %>%
  select(linear_feature_id, starts_with("flow")) %>%
  pivot_longer(
    cols = starts_with("flow"),
    names_to = c(".value", "month", "gcm", "rcp", "period"),
    # 1) (flow) literal, captured for .value
    # 2) (\\d+) month: one or more digits (handles 8, 17, 18)
    # 3) ([^_]+) gcm: letters/digits (e.g., mpi, mean, 6, 1, 20)
    # 4) (\\d{1,2}) rcp: 0, 45, 85 (1–2 digits)
    # 5) (\\d+) period: one or more digits
    names_pattern = "^\\s*(flow)(\\d+)_([^_]+)_(\\d{1,2})_(\\d+)\\s*$",
    values_to = "flow"
  ) %>%
  mutate(
    month  = as.integer(month),
    gcm    = as.integer(gcm),
    rcp    = as.integer(rcp),
    period = as.integer(period)
  )

flow_wide <- flow_long %>%
  filter(gcm == 9) %>%
  pivot_wider(
    names_from = c(month, gcm, rcp, period),
    values_from = flow,
    names_prefix = "flow") %>%
  arrange(linear_feature_id)

hist_col_18 <- names(flow_wide)[str_detect(names(flow_wide), "flow18_9_0_0")]
proj_col_18 <- names(flow_wide)[str_detect(names(flow_wide), "45|85")]
proj_col_18 <- proj_col_18[str_detect(proj_col_18, "flow18")]

fwQNDJ_wide <- flow_wide %>%
  mutate(histq = !!sym(hist_col_18),
    across(contains(proj_col_18), ~ (.x - histq) / histq, .names = "delta{.col}")) %>%
  select(linear_feature_id, contains("deltaflow"))

hist_col_8 <- names(flow_wide)[str_detect(names(flow_wide), "flow8_9_0_0")]
proj_col_8 <- names(flow_wide)[str_detect(names(flow_wide), "45|85")]
proj_col_8 <- proj_col_8[str_detect(proj_col_8, "flow8")]

fwQ8_wide <- flow_wide %>%
  mutate(histq = !!sym(hist_col_8),
    across(contains(proj_col_8), ~ (.x - histq) / histq, .names = "delta{.col}")) %>%
  select(linear_feature_id, contains("deltaflow"))

## Historic flow stats for all months
# hflow_sub <- hflow %>%
#   select(segmented_stream_id, contains("flow")) %>%
#   rename_with(~ str_replace_all(., "mean_flow_m3s", "flow")) %>%
#   rename_with(~ str_replace_all(.,  "_1$", "_0"))

# temp stats by stream
fwT_indi <- fw_models_df %>%
  mutate(histT = !!sym(paste(T_model, "0_00", historical_code, sep = "_"))) %>%  # add historical Tw8
  select(linear_feature_id, histT,
    all_of(grep(paste0("^", T_model, "_", 9), names(fw_models), value = TRUE))) %>%
  mutate(across(contains(T_model), ~ .x - histT, .names = "delta{.col}"))


# ENM stats by stream
fw_ENM <- fw_models_df %>%
  mutate(favchange_coho_45_3 = fav_coho_45_3 - fav_coho_hist_0,
    favchange_coho_85_3 = fav_coho_85_3 - fav_coho_hist_0,
    favchange_chinook_45_3 = fav_chinook_45_3 - fav_chinook_hist_0,
    favchange_chinook_85_3 = fav_chinook_85_3 - fav_chinook_hist_0,
    favchange_chum_45_3 = fav_chum_45_3 - fav_chum_hist_0,
    favchange_chum_85_3 = fav_chum_85_3 - fav_chum_hist_0,
    favchange_sockeye_45_3 = fav_sockeye_45_3 - fav_sockeye_hist_0,
    favchange_sockeye_85_3 = fav_sockeye_85_3 - fav_sockeye_hist_0,
    favchange_pink_45_3 = fav_pink_45_3 - fav_pink_hist_0,
    favchange_pink_85_3 = fav_pink_85_3 - fav_pink_hist_0,
    favchange_coho_45_5 = fav_coho_45_5 - fav_coho_hist_0,
    favchange_coho_85_5 = fav_coho_85_5 - fav_coho_hist_0,
    favchange_chinook_45_5 = fav_chinook_45_5 - fav_chinook_hist_0,
    favchange_chinook_85_5 = fav_chinook_85_5 - fav_chinook_hist_0,
    favchange_chum_45_5 = fav_chum_45_5 - fav_chum_hist_0,
    favchange_chum_85_5 = fav_chum_85_5 - fav_chum_hist_0,
    favchange_sockeye_45_5 = fav_sockeye_45_5 - fav_sockeye_hist_0,
    favchange_sockeye_85_5 = fav_sockeye_85_5 - fav_sockeye_hist_0,
    favchange_pink_45_5 = fav_pink_45_5 - fav_pink_hist_0,
    favchange_pink_85_5 = fav_pink_85_5 - fav_pink_hist_0) %>%
  select(linear_feature_id, contains("favchange"))



## create spatial object with all indicator variables
fw_sp_ind <- fw_models %>%
  select(linear_feature_id, fwa_watershed_code, channel_width, length_metre,
    mad_m3s, upstream_area_ha, gradient, gnis_name, model_access_salmon,
    model_habitat_salmon,
    model_habitat_ch, model_habitat_cm, model_habitat_co, model_habitat_pk, model_habitat_sk,
    contains("cthr"),
    contains("fav")) %>%
  left_join(fwT_indi,
    join_by(linear_feature_id)) %>%
  left_join(fwQ8_wide,
    join_by(linear_feature_id)) %>%
  left_join(fwQNDJ_wide,
    join_by(linear_feature_id)) %>%
  left_join(fw_ENM,
    join_by(linear_feature_id))

save(fw_sp_ind, file = file.path(paths$fw, "fw_stream_indicators_sp.Rds"))




# ==================== 5. Ruzzante Statistical Low Flow Projections ====================

stations_stats <- read_csv(file.path(paths$climate, "Ruzzante_low_flows", "stations_performance.csv"))

# watershed hydrologic regimes
watershed_flow <- st_read(file.path(paths$climate, "Ruzzante_low_flows", "watersheds.gpkg")) %>%
  st_transform(crs = 3005) %>%
  left_join(select(stations_stats, ID, regime), by = c("ID" = "ID")) %>%
  mutate(regime = as.factor(regime))

stations_flow <- st_read(file.path(paths$climate, "Ruzzante_low_flows", "stations.gpkg")) %>%
  st_transform(crs = 3005)

flow_in_cu <- lengths(st_contains(cu_boundary, stations_flow)) > 0
cu_boundary$has_flow <- flow_in_cu

# list of stations/watersheds with projections
projections_list <- list.files(file.path(paths$climate, "Ruzzante_low_flows", "regressionProjections"), pattern = ".csv")

# import csv for each water station
# assign into 20 year periods and nest by period
for (i in 1:length(projections_list)) {
  projections_csv <- read_csv(file.path(paths$climate, "Ruzzante_low_flows", "regressionProjections",
                                        projections_list[i])) %>%
    rename(gcm_name = source_id,
           rcp = experiment_id) %>%
    mutate(
      ID = as.character(ID),
      rcp = stringr::str_sub(rcp, -2, -1),
      rcp = dplyr::if_else(rcp == "al", "0", rcp),
      period = if_else(Year >= 1981 & Year <= 2010, "0",
        # if_else(Year >= 2001 & Year <= 2020, 1,
        if_else(Year >= 2021 & Year <= 2040, "2",
          if_else(Year >= 2041 & Year <= 2060, "3",
            if_else(Year >= 2061 & Year <= 2080, "4",
              if_else(Year >= 2081 & Year <= 2100, "5", NA))))))  %>%
    nest(.by = c("ID", "gcm_name",  "rcp", "variant_label", "period")) %>%
    filter((period < 2 & rcp == "0") |
      (period >= 2 & rcp != "0"))

  if (i == 1) watershed_proj <- projections_csv
  else if (i > 1) watershed_proj <- bind_rows(watershed_proj, projections_csv)
}

# get average across model variants for each period and scenario
wp_vm <- watershed_proj %>%
  mutate(mean = map_dbl(data, ~ mean(.x$predMean.m3s_8))) %>%
  nest(.by = c("ID", "rcp", "gcm_name", "period")) 

# save averaged flow projections
save(wp_vm, file = file.path(paths$fw, "Statistical_flow_projections.Rds"))

# save spatial objects to R file
save(watershed_flow, stations_flow, stations_stats, file = file.path(paths$fw, "flow_gauge_data.Rdata"))

# #get average across all GCMs for each year
# wp_stats <- wp_vm %>%
#   mutate(mean = map_dbl(data, ~mean(.x$mean)),
#          sd   = map_dbl(data, ~sd(.x$mean)),
#          qlow = map_dbl(data, ~quantile(.x$mean,  probs = 0.1)),
#          qhigh = map_dbl(data, ~quantile(.x$mean, probs = 0.9)))
#
# wp_stats_ens <- wp_stats %>%
#   nest(.by = c("ID", "experiment_id", "period")) %>%
#   mutate(mean = map_dbl(data, ~mean(.x$mean)),
#          sd   = map_dbl(data, ~sd(.x$mean)),
#          qlow = map_dbl(data, ~quantile(.x$mean, probs = 0.1)),
#          qhigh = map_dbl(data, ~quantile(.x$mean, probs = 0.9))) %>%
#   mutate(source_id = "ensemble", .after = experiment_id)



### comparisons between PCIC flows and Ruzzante model

# cu_cont <- st_contains(cu_boundary, stations_flow)
# take average of stations in each CU boundary
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

# Determine which streams are contained within each FAZ
# bcfp$FAZ <- NA
# for(f in 1:length(FAZ_Fr$FAZ_Name)) {
#   FAZ_pick <- FAZ_Fr[f,]
#   pick_tscapes <- lengths(st_intersects(bcfp,FAZ_pick)) > 0
#   bcfp$FAZ[pick_tscapes] <- FAZ_pick$FAZ_Acrony
# }


# ==================== 6. PCIC Grid & Model Projections Processing ====================
# Read in PCIC grid
grid_points <- read.csv(file.path(paths$fw, "PCIC-grid-points_bccoast.csv"))

# Convert grid points to spatial object
grid_points <- st_as_sf(grid_points, coords = c("lon", "lat"), crs = 4269) %>%
  st_transform(3005)

# import polygon grid - see 2x_fw_create_inputs_grid.R file
grid_polys <- readRDS(file = file.path(paths$fw, "grid_polys_fw.rds")) %>%
  st_transform(3005)

# subset Fraser basin
pick_Fr <- lengths(st_intersects(grid_polys, Fr_basin)) > 0
grid_polys <- grid_polys[pick_Fr, ]

pick_Fr <- lengths(st_intersects(grid_points, grid_polys)) > 0
grid_points <- grid_points[pick_Fr, ]


#--------------------- Load PCIC model projections------------------------------

# see 2x_FW_PCIC_process.R for creation of daily and monthly netcdf files
# currently using RCP 4.5 ensemble models
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
st_crs(PCIC_day) <- 4269 # change CRS to NAD83/Albers from default of WGS84
st_crs(PCIC_month) <- 4269 # change CRS to NAD83/Albers from default of WGS84

# crop to Fraser basin
# PCIC_indies <- st_crop(PCIC_indies, grid_polys)
# PCIC_day <- st_crop(PCIC_day, grid_polys)
# PCIC_month <- st_crop(PCIC_month, grid_polys)


save(PCIC_month, PCIC_day, grid_points, grid_polys,
  file = file.path(paths$fw, "R_data", paste0(today, "_fw_spatial_inputs.RData")))
save(PCIC_month, PCIC_day, grid_points, grid_polys,
  file = file.path(paths$fw, "R_data", "fw_spatial_inputs.RData"))
