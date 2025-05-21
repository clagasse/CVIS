################################################################################
#
# 2b_FW_spatial_subset.R
#
#  Stream network analysis of upstream migration routes and CU boundaries
#
#  For each CU:
#    1) subset stream network of downstream path to ocean
#    2) summarize route by distance and elevation gain
#    3) select PCIC grid cells overlapping with path
#    4) select appropriate dates from PCIC model outputs based on upstream timing
#    5) summarize PCIC temperature and flow during upstream timing
#
#
###############################################################################

###############################################################################
## Path analysis - upstream migration route

#subset stream network for more manageable size for analysis
FWA_Fr_ord5 <- filter(FWA_Fr_high, STREAM_ORD > 4)

for(i in 1:n.CUs) {
  nuseds_CU <- filter(nuseds_Fr, FULL_CU_IN == cu_run$FULL_CU_IN[i])
  
  if(nrow(nuseds_CU) == 0) {
    path_CU <- NA
    if(i >1) path_list <- c(path_list, list(path_CU))
    next
  }
  
  #method 1 - nearest feature
  nearest_lines <- st_nearest_feature(nuseds_CU, FWA_Fr_ord5)
  stream_candidates <- FWA_Fr_ord5[nearest_lines,]
  
  #method 2 - FWA code
  #stream_candidates_2 <- filter(FWA_Fr_ord5, FWA_WATERS %in% nuseds_CU$FWA_WATERSHED_CDE)
  
  #method 3 -intersection
  # stream_int <- st_intersects(FWA_cu, nuseds_CU, sparse = FALSE)
  # stream_candidates <- FWA_Fr_high[which(apply(stream_int, 1, sum) > 0),]

  #method 4 - CU boundary centroid
  #stream_pick <- choose_CU_stream(FWA_Fr_high, cu_boundary[cu_boundary$CUID == cuid[i],], subset_order = FALSE)
  
  #get downstream path from candidate point or stream
  for(j in 1:nrow(stream_candidates)) {
    stream_pick <- stream_candidates[j,]

    #get downstream path from candidate point or stream
    path_temp <- downstream_path(FWA_Fr_ord5, stream_pick, code_type = "FWA")
    
    #if path is empty, try next candidate
    if(nrow(path_temp) == 0) next
    
    dd <- measure_downstream(path_temp)
    
    path_temp <- path_temp %>%
      mutate(downstream_distance = dd)
    
    if(j == 1) {
      path_CU <- path_temp
    }
    if(j > 1) {
    path_CU <- bind_rows(path_CU, path_temp)
    }  #%>%
      #distinct()
  }

  dupes <- as_tibble(path_CU) %>%
    group_by(LINEAR_FEA) %>% 
    summarize(num_paths = n(),
              prop_paths = num_paths / nrow(nuseds_CU))

  path_CU <- distinct(path_CU) %>%
    left_join(dupes, by = "LINEAR_FEA", multiple = "first") 
  
  path_CU <- path_CU %>%
    mutate(downstream_distance = measure_downstream(path_CU))
  
  # ggplot() +
  #   geom_sf(data = cu_boundary_pick, color = "black", alpha = 0.1) +
  #   geom_sf(data = st_zm(path_CU), color = "blue", alpha = 0.6) +
  #   geom_sf(data = nuseds_CU, color = "red", alpha = 0.5)

  if(i == 1) path_list <- list(path_CU)
  if(i >1) path_list <- c(path_list, list(path_CU))
  
  print(paste("CU", cuid[i], "migration path done"))

}

names(path_list) <- cu_seq

save(path_list,
     file = here("processed_data", "freshwater", "R_data", paste0(today, "_fw_upstream_paths.Rdata")))



###############################################################################
## CU Boundary analysis 
##determine which streams are contained within each CU boundary

stream_cu_picks <- matrix(ncol = n.CUs, nrow = nrow(bcfpa))
colnames(stream_cu_picks) <- cu_seq

ENM_cu_picks <- matrix(ncol = n.CUs, nrow = nrow(reaches_ENM_all))

for(i in 1:2) {
  cu_pick <- cu_boundary[cu_boundary$FULL_CU_IN == cu_seq[i],]
  pick_st <- lengths(st_intersects(st_zm(bcfpa), cu_pick)) > 0
  stream_cu_picks[,i] <- pick_st
  
  pick_st <- lengths(st_intersects(st_zm(reaches_ENM_all), cu_pick)) > 0
  ENM_cu_picks[,i] <- pick_st
  
  print(paste(cu_seq[i], "boundary stream selection done"))
}

save(stream_cu_picks, ENM_cu_picks,
     file = here("processed_data", "freshwater", paste0(today, "_fw_cu_streams.Rdata")))


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


