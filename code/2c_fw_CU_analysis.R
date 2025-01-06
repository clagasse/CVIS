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
##determine which thermalscape streams are contained within each CU boundary

## This takes a long time to run!
cu_tscapes <- matrix(ncol = n.CUs, nrow = nrow(tscapes_acc))

for(i in 1:n.CUs) {
  cu_pick <- cu_boundary[cu_boundary$CUID == cuid[i],]
  pick_tscapes <- lengths(st_intersects(tscapes_acc, cu_pick)) > 0
  cu_tscapes[,i] <- pick_tscapes
  
  print(paste("CU", cuid[i], "boundary stream selection done"))
}

colnames(cu_tscapes) <- cuid

save(cu_tscapes, 
     file = here("data", "freshwater", "processed-data", paste0(today, "_fw_cu_streams.Rdata")))

