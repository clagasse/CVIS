###############################################################################
#
# 2c_fw_spawning_indicators.R
#
###############################################################################

#------------------------------------------------------------------------------
# Setup
#------------------------------------------------------------------------------

fw_output <- array(NA,
                   dim = c(n.CUs, length(stages), length(ind_names), length(period_names)),
                   dimnames = list(
                     cuid, 
                     stages,
                     ind_names,
                     period_names ))

# Create list to store full spatial output for plotting maps
# Later will calculate median across all GCMs
fw_spat <- list(); length(fw_spat) <- n.CUs*length(stages)
dim(fw_spat) <- c(n.CUs, length(stages))
dimnames(fw_spat) <- list(cuid, stages)

#################################################


grid.ref <- as.numeric(dimnames(peakFlow)[[1]])

for(i in 1:length(cuid)){
  #------------------------------------------------------------------------------
  # Loop through each life stage
  #------------------------------------------------------------------------------
  
  for(j in 1:length(stages)){
    
    #   if(cu_run$species_pooled[i] %in% c("Pink", "Chum") & stages[j] == "fw_rearing"){
    #     # No freshwater rearing for pink and chum = zero exposure during this stage
    #     fw_output[i, j, , ] <- 0
    #     fw_spat[[i,j]][ , , ] <- 0
    #     
    #   } 
    # else {
    #-----------------------------------------------------------------------------
    # Identify grid cells for given life stage
    
    incl <- PCIC_incl[[i,j]]
    
    # Dimensions
    n.grid <- length(incl)
    
    #-----------------------------------------------------------------------------
    # Subset indicators relevant to CU and stage
    
    peakFlow.ij <- array(NA,
                         dim = c(n.grid, 2),
                         dimnames = list(incl, c("hist", "mid")))
    
    
    peakFlow.ij <- peakFlow[match(incl, grid.ref),]
    
    
    
    
    freq19year.ij <- array(NA,
                       dim = c(n.grid, 2),
                       dimnames = list(incl, c("hist", "mid")))
    
    freq19year.ij <- freq19year[match(incl, grid.ref),]
    
    
    peakFlow_perc.ij <- peakFlow.ij[,2] / peakFlow.ij[,1]
    peakFlow_diff.ij <- peakFlow.ij[,1] - peakFlow.ij[,2]
    
    #calculate difference between projected and historic
    #peakFlow_diff <- calc_mean_diff(peakFlow.ij)
    #freq19_diff <- calc_mean_diff(freq19year.ij)
    
    #peakFlow_diff.ij <- peakFlow.ij[p]
    
    # Store full spatial output
    fw_spat[[i,j]][ "peakFlow", , ] <- peakFlow.ij[,,2] - peakFlow.ij[,,1]
    fw_spat[[i,,]][ "freq19", ,   ] <- freq19.ij[,,2] - freq19ij[,,1]
    
    # store summary output
    for(p in 1:2){ # for each period
      # Store output in end array
      fw_output[i, j, "peakFlow", p] <- c(
        
        calc_mean_diff(peakFlow.ij)
        #median(Ts.perc[p, ]),
        #range(Ts.perc[p, ]))
      )
    }
    
  } # end life stage j
  
  print(paste0("Done ", cuid[i]))
  
} # end CU i





# dates <- dimnames(Ts.weeklyMax)[[2]]
# var <- list(
#   Ts = Ts.weeklyMax[, which(dates == "1970-01-01"):which(dates == "2099-12-31")],
#   Qs = Qs.weeklyMean[, which(dates == "1970-01-01"):which(dates == "2099-12-31")]
# )
