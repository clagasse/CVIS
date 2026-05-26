# ==============================================================================
# CVIS Freshwater Atlas Query Tool (1z_FWA_query.R)
#
# Description:
#   Uses the fwapgr package to query spatial watershed and stream network data 
#   from the BC Freshwater Atlas. Prepares subsets (e.g. for Vancouver Island 
#   Chinook CK-29) and performs diagnostic area overlays.
#
# Workflow Steps:
#   1. Define watershed code zero-padding utility functions.
#   2. Query FWA basins, groups, and streams using the pgfeatureserv API.
#   3. Load and intersect spatial files (lakes, drainage units, BCFishpass).
#   4. Subset streams, boundaries, and NuSEDS systems for CK-29.
#   5. Sub-divide CK-29 streams into individual watershed groups.
#   6. Render diagnostic boundary maps and compute drainage areas.
#
# Inputs:
#   - Spatial shapefiles and geopackages in paths$spatial
#
# Outputs:
#   - Intersected sub-selections loaded into R environment
#
# Dependencies:
#   - Requires 0_setup.R, pgfeatureserv, and fwapgr.
# ==============================================================================

# ==================== 1. Setup & Load Libraries ====================
library(fwapgr)  # package for accessing BC FWA
library(here)

setwd(here())
source(file.path(here(), "code", "0_setup.R"))


# ==================== 2. Define Padding Functions ====================
# Define the target segment lengths
target_lengths <- c(3, 6, 5, 5, 4, 4, 3, 3, 3, 3, 3, 3)

# Function to pad segments to match target format
pad_to_watershed_format <- function(code) {
  segments <- str_split(code, "-", simplify = TRUE)
  n_segments <- length(segments)

  # Pad existing segments to correct length
  padded_segments <- mapply(function(seg, len) {
    str_pad(seg, width = len, side = "left", pad = "0")
  }, segments, target_lengths[1:n_segments])

  # Add missing segments as zero-padded strings
  if (n_segments < length(target_lengths)) {
    extra_segments <- str_pad("0", width = target_lengths[(n_segments + 1):length(target_lengths)], side = "left", pad = "0")
    padded_segments <- c(padded_segments, extra_segments)
  }

  paste(padded_segments, collapse = "-")
}


### padding for local watershed code
# slightly different format, longer and more segments
target_segments <- 20

# Function to pad a code to the target number of segments
pad_localcode <- function(code) {
  segments <- str_split(code, "-", simplify = TRUE)
  n_segments <- length(segments)
  padding_needed <- target_segments - n_segments
  padding <- rep("00000", padding_needed)
  full_code <- paste(c(segments, padding), collapse = "-")
  return(full_code)
}


# ==================== 3. FWA Collection Queries & Vancouver Island Intersections ====================
# get information about the collections or a collection’s properties:
fwa_cols <- fwa_collections()
fwa_collection_properties("whse_basemapping.fwa_basins_poly")

bc_basins <- fwa_query_collection("whse_basemapping.fwa_basins_poly")

wshed_groups <- fwa_query_collection("whse_basemapping.fwa_watershed_groups_poly")
wshed_groups <- st_transform(wshed_groups, crs = 3005)

wsheds <- fwa_query_collection("whse_basemapping.fwa_watersheds_poly", filter)
wsheds <- st_transform(wsheds, crs = 3005)

collection_id <- "whse_basemapping.fwa_stream_networks_sp"
filter_watersheds <- setNames(as.list(FR_codes), rep("watershed_group_code", length(FR_codes)))

FWA_query <- fwa_query_collection(collection_id, filter = filter_watersheds)

# Test for Campbell River watershed
# campbell <- wshed_groups[wshed_groups$watershed_group_name == "Campbell River",]
# ggplot() +
#   geom_sf(data = campbell)

# lakes polygons
lakes_fwa <- st_read(file.path(paths$spatial, "BC_FWA_LAKES", "FWA_LAKES_POLY.gpkg"))

# load Vancouver Island watershed

# read ecological drainage units
EAU <- st_read(file.path(paths$spatial, "EAUBC_ECO_DRAINAGE_UNITS", "EAUBC_ECO_DRAINAGE_UNITS_SP.gpkg"))
EAU_VI <- EAU[EAU$ECO_DRAINAGE_UNIT == "Vancouver Island", ]

wshed_VI <- subset_intersections(wshed_groups, EAU_VI)
# wshed_VI <- st_intersects(wshed_groups, EAU_VI, sparse = FALSE)
# wshed_VI <- wshed_groups[apply(wshed_VI, 1, sum) > 0,]

wshed_VI_2 <- subset_intersections(wsheds, EAU_VI)

## create Vancouver Island BC Fishpass object
bcfph <- st_read(file.path(paths$spatial, "BCFishpass", "bcfishpass_streams_habitat", "bcfishpass_streams_2024-12-09.gpkg")) # %>%
bcfph_VI <- subset_intersections(st_zm(bcfph), EAU_VI)

lakes_VI <- subset_intersections(lakes_fwa, EAU_VI)

# save(bcfph_VI, file = file.path(paths$fw, "BCFP_VI.Rds"))

# get VI watershed codes for loading layers
VI_codes <- unique(bcfph_VI$watershed_group_code)

# get VI watersheds
collection_id <- "whse_basemapping.fwa_watersheds_poly"
filter_watersheds <- setNames(as.list(VI_codes), rep("watershed_group_code", length(VI_codes)))

VI_wsheds <- fwa_query_collection(collection_id, filter = list(watershed_group_code = "NIMP"))


# ==================== 4. Subset for Upper SoG Chinook (CK-29) ====================

load(file = file.path(paths$fw, "BCFP_VI.Rds"))

cu_boundary <- st_read(file.path(paths$spatial, "CU_boundaries", "pse_conservation_units.gdb"))
CK29_boundary <- cu_boundary[cu_boundary$FULL_CU_IN == "CK-29", ]

# get BC Fishpass streams within the CU boundary
bcfph_CK29 <- subset_intersections(st_zm(bcfph_VI), CK29_boundary)

# make watershed and local codes consistent with original FWA format
bcfph_CK29 <- bcfph_CK29 %>%
  separate(mapping_code_ch, into = c("ch_code_1", "ch_code_2", "ch_code_3"), sep = ";", fill = "right", remove = FALSE) %>%
  mutate(wscode_fwa = str_replace_all(wscode, "\\.", "-"),
    localcode_fwa = str_replace_all(localcode, "\\.", "-")) %>%
  mutate(wscode_fwa_padded = sapply(wscode_fwa, pad_to_watershed_format),
    localcode_fwa_padded = sapply(localcode_fwa, pad_localcode))

## subset only accessible streams
acc_CK29 <- bcfph_CK29 %>%
  filter(ch_code_1 %in% c("SPAWN", "ACCESS", "REAR"))


# get watershed groups within the CU boundary
wsheds_CK29 <- subset_intersections(wshed_groups, CK29_boundary)
lakes_CK29  <- subset_intersections(lakes_VI, CK29_boundary)

# load NuSEDS
nuseds <- read_csv(file.path(paths$salmon, "All Areas NuSEDS.csv"))
nuseds_sites <- read_csv(file.path(paths$salmon, "Conservation Unit Census Sites_20250528.csv")) %>%
  filter(!is.na(X_LONGT)) %>%
  st_as_sf(coords = c("X_LONGT", "Y_LAT"),
    crs = 4269) %>%
  st_transform(crs = 3005)
nuseds_sites_CK29 <- nuseds_sites[nuseds_sites$FULL_CU_IN == "CK-29", ]

# join nuseds to stream segments using watershed code to get site locations
# NOT WORKING
# nuseds_sp <- nuseds %>%
#   left_join(bcfph_CK29, join_by("WATERSHED_CDE" == "wscode_fwa_padded")) %>%
#   filter(!is.na(segmented_stream_id))


# join watersheds to accessible streams
acc_wsheds_NIMP <- VI_wsheds %>%
  left_join(st_drop_geometry(acc_CK29), join_by("watershed_key")) %>%
  filter(!is.na(segmented_stream_id)) %>%
  group_by(fwa_watershed_code) %>%
  slice(1) %>%
  ungroup()


# ==================== 5. Subset for Individual Watersheds ====================

# get polygon for watershed group of interest
wshed_NIMP <- filter(wshed_groups, watershed_group_code == "NIMP")
wshed_CAMB <- filter(wshed_groups, watershed_group_code == "CAMB")
wshed_SALM <- filter(wshed_groups, watershed_group_code == "SALM")

# subset streams
acc_NIMP <- subset_intersections(st_zm(acc_CK29), wshed_NIMP)
acc_CAMB <- subset_intersections(st_zm(acc_CK29), wshed_CAMB)
acc_SALM <- subset_intersections(st_zm(acc_CK29), wshed_SALM)

lakes_CAMB <- subset_intersections(st_zm(lakes_VI), wshed_CAMB)
lakes_SALM <- subset_intersections(st_zm(lakes_VI), wshed_SALM)
lakes_NIMP <- subset_intersections(st_zm(lakes_VI), wshed_NIMP)

bcfph_CAMB <- subset_intersections(st_zm(bcfph_CK29), wshed_CAMB)
bcfph_SALM <- subset_intersections(st_zm(bcfph_CK29), wshed_SALM)
bcfph_NIMP <- subset_intersections(st_zm(bcfph_CK29), wshed_NIMP)


# ==================== 6. Diagnostic Plots & Metrics ====================

if (interactive()) {
  # CU Boundary plot
  ggplot() +
    geom_sf(data = CK29_boundary, fill = "grey", alpha = 0.3) +
    # annotation_map_tile(type = "cartolight") +
    # geom_sf(data = wsheds_CK29, aes(fill = watershed_group_code), alpha = 0.5) +
    # geom_sf(data = bcfph_CK29_sub, aes(colour = ch_code_1), linewidth = 1.0) +
    geom_sf(data = acc_CK29, aes(colour = ch_code_1), linewidth = 1.0) +
    geom_sf(data = lakes_CK29, fill = "darkblue", alpha = 0.7) +
    geom_sf(data = nuseds_sites_CK29)


  # Nimpkish or other watershed plot
  ggplot() +
    geom_sf(data = VI_wsheds, fill = "darkred", alpha = 0.5) +
    geom_sf(data = acc_wsheds_CK29, colour = "blue") +
    geom_sf(data = acc_NIMP, aes(colour = ch_code_1))


  # Nimpkish or other watershed plot
  ggplot() +
    annotation_map_tile(type = "cartolight") +
    geom_sf(data = wshed_CAMB, fill = "grey", alpha = 0.3) +
    # geom_sf(data = acc_wsheds_CK29, colour = "blue") +
    geom_sf(data = bcfph_CAMB, aes(colour = ch_code_1), linewidth = 1.0) +
    geom_sf(data = lakes_CAMB, fill = "darkblue", alpha = 0.7)

  # Nimpkish or other watershed plot
  ggplot() +
    annotation_map_tile(type = "cartolight") +
    geom_sf(data = wshed_SALM, fill = "grey", alpha = 0.3) +
    # geom_sf(data = acc_wsheds_CK29, colour = "blue") +
    geom_sf(data = bcfph_SALM, aes(colour = ch_code_1), linewidth = 1.0) +
    geom_sf(data = lakes_SALM, fill = "darkblue", alpha = 0.7)

  ggplot() +
    annotation_map_tile(type = "cartolight") +
    geom_sf(data = wshed_NIMP, fill = "grey", alpha = 0.3) +
    # geom_sf(data = acc_wsheds_CK29, colour = "blue") +
    geom_sf(data = bcfph_NIMP, aes(colour = ch_code_1), linewidth = 1.0) +
    geom_sf(data = lakes_NIMP, fill = "darkblue", alpha = 0.7)


  # get watershed area

  # accessible areas
  sum(acc_wsheds_CK29$area_ha)

  # total area
  sum(VI_wsheds$area_ha)
}
