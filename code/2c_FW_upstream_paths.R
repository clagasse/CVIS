# ==============================================================================
# CVIS Upstream Migration Paths (2c_FW_upstream_paths.R)
#
# Description:
#   Traces the spatial paths from ocean entry (river mouth) to NuSEDS spawning
#   sites for each Conservation Unit (CU). Computes upstream migration routes,
#   identifies shared stream segments (with proportional weighting), and calculates
#   segment downstream distances and work (elevation × distance) metrics.
#
# Workflow Steps:
#   1. Load setup environment and configuration parameters.
#   2. Load downstream distance stream database (bcfph).
#   3. Loop through CUs, identify nearest stream features to NuSEDS spawner sites.
#   4. Trace paths downstream from each site to find overlap/proportional usage.
#   5. Calculate physical metrics (accumulated upstream work).
#   6. Save resulting path list (migr_list) to processed_data/freshwater/.
#
# Inputs:
#   - processed_data/freshwater/2025-07-22_fw_bcfp_downstreamdist.Rdata
#   - NuSEDS spawner site locations (via 1a_CU_import / 0_setup)
#
# Outputs:
#   - processed_data/freshwater/[date]_fw_upstream_paths.Rdata
#
# Dependencies:
#   - Requires 0_setup.R and 2_fw_utils.R.
# ==============================================================================

# ==================== 1. Setup and Environment ====================
library(here)
setwd(here())
source(file.path(here(), "code", "0_setup.R"))

# subset stream network for more manageable size for analysis
# FWA_Fr_ord5 <- filter(FWA_Fr_high, STREAM_ORD > 4)

# load accessible streams from BC FishPass and subset to higher stream order
# load(file.path(paths$fw, "BCFP_combined_accessible_Fr.Rds"))

# bcfph <- bcfpa %>%
#   filter(stream_order > 4) %>%
#   st_zm() %>%
#   mutate(downstream_distance = NA) %>%
#   select(segmented_stream_id:mad_m3s, model_access_salmon)

load(file.path(paths$fw, "2025-07-22_fw_bcfp_downstreamdist.Rdata"))


# ==================== 2. Load Downstream Distance Matrix ====================

# get downstream distance for each stream using function from 2_fw_utils
# this takes a very long time to process but only needs to be done once
# for(i in 1:nrow(bcfph)) {
#   bcfph$downstream_distance[i] <- measure_downstream(bcfph[i,], bcfph)
#   if(bcfph$downstream_distance[i] < 0) bcfph$downstream_distance[i] <- 0
# }

# save(bcfph, file = file.path(paths$fw, paste0(today, "_fw_bcfp_downstreamdist.Rdata")))

# ==================== 3. Trace Downstream Paths for each CU ====================
for (i in 1:n.CUs) {

  cu_i <- cu_run$FULL_CU_IN[i]
  nuseds_cu <- filter(nuseds_Fr, FULL_CU_IN == cu_i)

  if (nrow(nuseds_cu) == 0) {
    migr_cu <- NA
    if (i > 1) migr_list <- c(migr_list, list(migr_cu))
    next
  }

  # method 1 - nearest feature
  nearest_lines <- st_nearest_feature(nuseds_cu, bcfph)
  stream_candidates <- bcfph[nearest_lines, ]  # get one stream candidate for each nuseds site
  
  t1 <- filter(stream_candidates, gnis_name == "Fraser River")

  # method 2 - FWA code
  # stream_candidates_2 <- filter(FWA_Fr_ord5, FWA_WATERS %in% nuseds_cu$FWA_WATERSHED_CDE)

  # method 3 -intersection
  # stream_int <- st_intersects(FWA_cu, nuseds_cu, sparse = FALSE)
  # stream_candidates <- FWA_Fr_high[which(apply(stream_int, 1, sum) > 0),]

  # method 4 - CU boundary centroid
  # stream_pick <- choose_CU_stream(FWA_Fr_high, cu_boundary[cu_boundary$CUID == cuid[i],], subset_order = FALSE)

  # get downstream path from candidate point or stream
  for (j in 1:nrow(stream_candidates)) {
    stream_pick <- stream_candidates[j, ]
    #stream_pick <- t1[3,]

    # get downstream path from candidate point or stream
    migr_temp <- downstream_path(stream_pick, bcfph, code_type = "FWA")

    # if path is empty, try next candidate
    if (nrow(migr_temp) == 0) next

    if (j == 1) {
      migr_cu <- migr_temp
    }
    if (j > 1) {
      migr_cu <- bind_rows(migr_cu, migr_temp)  # end up with a spatial data frame with a full downstream path for each nuseds site
    }  # %>%
    # distinct()
  }

  dupes <- as_tibble(migr_cu) %>%
    group_by(segmented_stream_id) %>%
    summarize(num_paths = n()) %>%
    mutate(prop_paths = num_paths / max(num_paths)) # nrow(stream_candidates))

  #join migraiton segments with multiple paths
  migr_cu <- distinct(migr_cu) %>%
    left_join(dupes, by = "segmented_stream_id", multiple = "first") %>%
    mutate(work_upstream = calculate_work(max(st_coordinates(.)[, 3]), downstream_distance))  #calculate work (elev x dist)
  
  if (i == 1) migr_list <- list(migr_cu)
  if (i > 1) migr_list <- c(migr_list, list(migr_cu))

  print(paste("CU", cu_i, "migration path done"))

}

# ==================== 4. Save Outputs ====================
names(migr_list) <- cu_seq

save(migr_list,
  file = file.path(paths$fw, paste0(today, "_fw_upstream_paths.Rdata")))
save(migr_list,
  file = file.path(paths$fw, "fw_upstream_paths.Rdata"))

#############################################################################
## FAZ Boundary analysis

# n.FAZ <- nrow(FAZ_Fr)
#
# stream_FAZ_picks <- matrix(ncol = n.FAZ, nrow = nrow(fw_amod))
#
# for(i in 1:n.FAZ) {
#   FAZ_pick <- FAZ_Fr[i,]
#   pick_st <- lengths(st_intersects(fw_amod, FAZ_pick)) > 0
#   stream_FAZ_picks[,i] <- pick_st
#
#   print(paste("FAZ", FAZ_Fr$FAZ_Name[i], "boundary stream selection done"))
# }
#
# colnames(stream_FAZ_picks) <- FAZ_Fr$FAZ_Acrony
#
# save(stream_FAZ_picks,
#      file = here("processed_data", "freshwater",  paste0(today, "_fw_FAZ_streams.Rdata")))
