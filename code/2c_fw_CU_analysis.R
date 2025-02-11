################################################################################
#
# 2c_fw_CU_analysis.R
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

for(i in 1:n.CUs) {
  
  stream_pick <- choose_CU_stream(FWA_Fr_high, cu_boundary[cu_boundary$CUID == cuid[i],], subset_order = FALSE)

  path_temp <- downstream_path(FWA_Fr_high, stream_pick, code_type = "FWA")

  if(i == 1) path_list <- list(path_temp)
  if(i >1) path_list <- c(path_list, list(path_temp))
  
  print(paste("CU", cuid[i], "migration path done"))

}

names(path_list) <- cuid

save(path_list,
     file = here("data", "freshwater", "processed-data", paste0(today, "_fw_upstream_paths.Rdata")))


###############################################################################
## CU Boundary analysis 
##determine which streams are contained within each CU boundary

## This takes a long time to run!
stream_cu_picks <- matrix(ncol = n.CUs, nrow = nrow(fw_acc_indies))

for(i in 1:n.CUs) {
  cu_pick <- cu_boundary[cu_boundary$CUID == cuid[i],]
  pick_st <- lengths(st_intersects(fw_acc_indies, cu_pick)) > 0
  stream_cu_picks[,i] <- pick_st
  
  print(paste("CU", cuid[i], "boundary stream selection done"))
}

colnames(stream_cu_picks) <- cuid

save(stream_cu_picks, 
     file = here("data", "freshwater", "processed-data", paste0(today, "_fw_cu_streams.Rdata")))


#############################################################################
## FAZ Boundary analysis

# n.FAZ <- nrow(FAZ_Fr)
# 
# stream_FAZ_picks <- matrix(ncol = n.FAZ, nrow = nrow(fw_acc_indies))
#                            
# for(i in 1:n.FAZ) {
#   FAZ_pick <- FAZ_Fr[i,]
#   pick_st <- lengths(st_intersects(fw_acc_indies, FAZ_pick)) > 0
#   stream_FAZ_picks[,i] <- pick_st
#   
#   print(paste("FAZ", FAZ_Fr$FAZ_Name[i], "boundary stream selection done"))
# }
# 
# colnames(stream_FAZ_picks) <- FAZ_Fr$FAZ_Acrony
# 
# save(stream_FAZ_picks, 
#      file = here("data", "freshwater", "processed-data", paste0(today, "_fw_FAZ_streams.Rdata")))

