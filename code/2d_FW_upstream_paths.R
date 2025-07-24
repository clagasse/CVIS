################################################################################
#
# 2d_FW_upstream_paths.R
#
#  Get spatial objects of upstream paths from ocean entry to NUSEDS spawning sites for each CU
#
#  For each CU:
#    1) select streams closest to NUSEDS sites
#    2) subset stream network of downstream path to ocean from each site
#    3) determine proportion of NUSEDS sites travelling long each path
#    4) calculate downstream distance for each stream segment
#    5) Save a list of spatial objects of upstream paths for each CU as path_list
#
#  This script uses functions from the 2_fw_utils.R file
#
###############################################################################
## Path analysis - upstream migration route

library(here)
setwd(here())
source(file.path(here(), "code", "0_setup.R"))

#load(file.path(paths$fw, "2025-04-22_fw_upstream_paths.Rdata"))

#subset stream network for more manageable size for analysis
#FWA_Fr_ord5 <- filter(FWA_Fr_high, STREAM_ORD > 4)

# load accessible streams from BC FishPass and subset to higher stream order
load(file.path(paths$fw, "BCFP_combined_accessible_Fr.Rds"))

bcfph <- bcfpa %>%
  filter(stream_order > 4) %>%
  st_zm() %>%
  mutate(downstream_distance = NA) %>%
  select(segmented_stream_id:mad_m3s, model_access_salmon)
  
load(file.path(paths$fw, "2025-07-22_fw_bcfp_downstreamdist.Rdata"))

### NUSEDS salmon spawner locations
##version from FIA. Usage column added by Michael Arbeider
nuseds_Fr <- read_csv(file.path(paths$salmon, "NuSEDS_CU_System_sites_202406.csv")) %>%
  st_as_sf(coords = c("X_LONGT", "Y_LAT"), crs = 4269) %>%
  st_transform(3005) %>%
  filter(USAGE != "REMOVE")


#--------------------- Downstream distance for stream network -------------------

# get downstream distance for each stream using function from 2_fw_utils
# this takes a very long time to process but only needs to be done once
# for(i in 1:nrow(bcfph)) {
#   bcfph$downstream_distance[i] <- measure_downstream(bcfph[i,], bcfph)
#   if(bcfph$downstream_distance[i] < 0) bcfph$downstream_distance[i] <- 0
# }

#save(bcfph, file = file.path(paths$fw, paste0(today, "_fw_bcfp_downstreamdist.Rdata")))

for(i in 1:n.CUs) {
  nuseds_cu <- filter(nuseds_Fr, FULL_CU_IN == cu_run$FULL_CU_IN[i])
  
  if(nrow(nuseds_cu) == 0) {
    migr_cu <- NA
    if(i >1) migr_list <- c(migr_list, list(migr_cu))
    next
  }
  
  #method 1 - nearest feature
  nearest_lines <- st_nearest_feature(nuseds_cu, bcfph)
  stream_candidates <- bcfph[nearest_lines,]
  
  #method 2 - FWA code
  #stream_candidates_2 <- filter(FWA_Fr_ord5, FWA_WATERS %in% nuseds_cu$FWA_WATERSHED_CDE)
  
  #method 3 -intersection
  # stream_int <- st_intersects(FWA_cu, nuseds_cu, sparse = FALSE)
  # stream_candidates <- FWA_Fr_high[which(apply(stream_int, 1, sum) > 0),]

  #method 4 - CU boundary centroid
  #stream_pick <- choose_CU_stream(FWA_Fr_high, cu_boundary[cu_boundary$CUID == cuid[i],], subset_order = FALSE)
  
  #get downstream path from candidate point or stream
  for(j in 1:nrow(stream_candidates)) {
    stream_pick <- stream_candidates[j,]

    #get downstream path from candidate point or stream
    migr_temp <- downstream_path(stream_pick, bcfph, code_type = "FWA")
    
    #if path is empty, try next candidate
    if(nrow(migr_temp) == 0) next
    
    if(j == 1) {
      migr_cu <- migr_temp
    }
    if(j > 1) {
    migr_cu <- bind_rows(migr_cu, migr_temp)
    }  #%>%
      #distinct()
  }

  dupes <- as_tibble(migr_cu) %>%
    group_by(segmented_stream_id) %>% 
    summarize(num_paths = n(),
              prop_paths = num_paths / nrow(nuseds_cu))

  migr_cu <- distinct(migr_cu) %>%
    left_join(dupes, by = "segmented_stream_id", multiple = "first") 
  
  # migr_cu$downstream_distance <- apply(migr_cu, 1, function(row) {
  #   measure_downstream(row, migr_cu)
  #})
  
  if(i == 1) migr_list <- list(migr_cu)
  if(i >1) migr_list <- c(migr_list, list(migr_cu))
  
  print(paste("CU", cuid[i], "migration path done"))

}

names(migr_list) <- cu_seq[1:2]

save(migr_list,
     file = file.path(paths$fw, paste0(today, "_fw_upstream_paths.Rdata")))



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


