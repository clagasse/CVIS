
#---------------------2b. CU boundary stream subsetting ---------------

# Simple script to create matrices of selections of streams within CU boundaries
# Matrix selections are used for subsetting and statistical summaries in other scripts

# create matrix choosing streams are contained within each CU boundary

stream_cu_picks <- matrix(ncol = n.CUs, nrow = nrow(bcfpa))
colnames(stream_cu_picks) <- cu_seq

ENM_cu_picks <- matrix(ncol = n.CUs, nrow = nrow(reaches_ENM_all))
colnames(ENM_cu_picks) <- cu_seq

for(i in 1:n.CUs) {
  cu_pick <- cu_boundary[cu_boundary$FULL_CU_IN == cu_seq[i],]
  pick_st <- lengths(st_intersects(st_zm(bcfpa), cu_pick)) > 0
  stream_cu_picks[,i] <- pick_st
  
  pick_st <- lengths(st_intersects(st_zm(reaches_ENM_all), cu_pick)) > 0
  ENM_cu_picks[,i] <- pick_st
  
  print(paste(cu_seq[i], "boundary stream selection done"))
}

save(stream_cu_picks, ENM_cu_picks,
     file = here("processed_data", "freshwater", paste0(today, "_fw_streampicks.Rdata")))
