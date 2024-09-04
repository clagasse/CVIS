###############################################################################
#
# 2b_fw_CU_dist.R
#
#  For each CU and life stage, get indicator stats and make simple plots
#
###############################################################################


#-----------------------------------------------------------------------------
# Loop through to extract grid cell ids
#-----------------------------------------------------------------------------

PCIC_incl <- vector(mode = 'list', 
                    length = length(cuid)*length(stages))
dim(PCIC_incl) <- c(length(cuid), length(stages))
dimnames(PCIC_incl) <- list(cuid, stages)


I <- 0 # Overall cu # counter

# For each species
for(s in 1:length(unique(cu_run$spp))){
  
  # Subset spawning and rearing streams for that species (this is a bit slow)
  ind.spawn <- streams %>% 
    as.data.frame %>% 
    select(paste0("model_spawning_", spp_lookup_run$streams_code[s]))
  spawning.s <- streams[which(ind.spawn == 1), ]
  
  obs.spawn <- streams %>%
    as.data.frame %>%
    select(paste0("obsrvd_spawning_", spp_lookup_run$streams_code[s]))
  obs.spawning.s <- streams[which(obs.spawn == 1), ]
  
  if(spp_lookup_run$species_pooled[s] %in% c("Chum", "Pink") == FALSE) { # No rearing distributions for pink and chum
  ind.rear <- streams %>%
  	as.data.frame %>%
  	select(paste0("model_rearing_", spp_lookup_run$streams_code[s]))
  rearing.s <- streams[which(ind.rear == 1), ]
  }
  
  # Plot all CUs for that species
  # plot(st_geometry(spawning.s), col = col_pal[2])
  # plot(st_geometry(obs.spawning.s), add = TRUE, lwd = 1.2, col  = col_pal[3])
  # plot(st_geometry(shoreline), add = TRUE)
  # plot(st_geometry(rearing.s), col = col_pal[1], lwd = 0.5)
  # plot(st_geometry(shoreline), add = TRUE)

  
  # Extract cuids for the selected species
  cuid.s <- cu_run$cuid[which(cu_run$species_pooled == spp_lookup_run$species_pooled[s])]
  # cu_run$cu_name_pse[which(cu_run$species_pooled == spp_lookup_run$species_pooled[s])]
  
  # For each CU of that species
  for(i in 1:length(cuid.s)){
    # Subset CU boundary
    cu_boundary.i <- cu_boundary[cu_boundary$CUID == cuid.s[i], ]
    
    # SPAWNING: Find grid_id that intersect freshwater *spawning*
    dum.spawn <- st_intersects(spawning.s, cu_boundary.i, sparse = FALSE)
    spawning.i <- spawning.s[which(dum.spawn == TRUE),]
    
    intrscts_spawn <- st_intersects(spawning.i, grid_polys, sparse = FALSE)
    incl_spawn <- which(apply(intrscts_spawn, 2, sum) > 0)  #spawning grid cells to include for CU
    
    # REARING: Find grid_id that intersect freshwater *rearing* (includes migration)
    if(spp_lookup_run$species_pooled[s] %in% c("Chum", "Pink") == FALSE) { # No rearing distributions for pink and chum

      # EXCEPTION: Harrison upstream-migrating late. This CU boundary includes spawning habitat but not the rearing lake. Use Harrison downstream migrating CU boudnary to determine rearing habitat.
      if(cu_run$cuname[which(cu_run$species_pooled == spp_lookup_run$species_pooled[s])[i]] == "Harrison (U/S)-L population"){

        dum.rear <- st_intersects(rearing.s, cu_boundary[cu_boundary$cuname == "Harrison (D/S)-L population", ], sparse = FALSE)

      } else { # If any other CU, use CU boubndary

        dum.rear <- st_intersects(rearing.s, cu_boundary.i, sparse = FALSE)
      }

      rearing.i <- rearing.s[which(dum.rear == TRUE),]

      intrscts_rear <- st_intersects(rearing.i, grid_polys, sparse = FALSE)
      incl_rear <- which(apply(intrscts_rear, 2, sum) > 0)  #rearing grid cells ot include for CU
    }

    # MIGRATION: Find grid_id that intersect freshwater *migration*
    # if(s <= 5){ # for salmon
    #   
    #   mig_paths.i <- mig_paths[which(mig_paths$cuid == cuid.s[i]), ]
    #   intrscts_mig <- st_intersects(mig_paths.i, grid_polys, sparse = FALSE)
    #   incl_mig <- which(apply(intrscts_mig, 2, sum) > 0)
    #   
    # } else if(s == 6){ # for steelhead (no mig_paths)
    # 
    # # within <- st_intersection(mig_paths_start, cu_boundary.i)$csv_id
    # # plot(st_geometry(mig_paths_start[which(mig_paths_start$csv_id %in% within), ]), add = TRUE)
    # # 
    # # Or intersect with grid polys?
    # simpleLoc <- st_union(grid_polys[unique(c(incl_spawn, incl_rear)),])
    # # plot(st_geometry(simpleLoc), add = TRUE, col = NA)
    # within_simpleLoc <- st_intersection(mig_paths_start, simpleLoc)$csv_id
    # 
    # #Check that they're within the CU boundary (for some small CUs the grid cells
    # # capture points outside of watershed/CU boundaries)
    # 
    # within <- st_intersection(mig_paths_start[mig_paths_start$csv_id %in% within_simpleLoc, ], cu_boundary.i)$csv_id
    # 
    # 
    #  mig_paths.i <- mig_paths[which(mig_paths$csv_id %in% within), ]
    # intrscts_mig <- st_intersects(mig_paths.i, grid_polys, sparse = FALSE)
    # incl_mig <- which(apply(intrscts_mig, 2, sum) > 0)
    # 
    # 
    
    # } # end if steelhead
    
    # Assign grid cells to each life stage
    I <- I + 1
    #PCIC_incl[[I, "adult_migration"]] <- unique(c(incl_mig, incl_spawn))
    PCIC_incl[[I, "spawning"]] <- incl_spawn
    #PCIC_incl[[I, "eggs_alevin"]] <- incl_spawn
    if(spp_lookup_run$species_pooled[s] %in% c("Chum", "Pink") == FALSE) {
      #PCIC_incl[[I, "fw_rearing"]] <- unique(c(incl_mig, incl_rear))
      PCIC_incl[[I, "fw_rearing"]] <- unique(c(incl_rear))
    } else {
      PCIC_incl[[I, "fw_rearing"]] <- NULL
    }
    
    if(I == 1){
      PCIC_incl_overall <- unique(c(incl_spawn, incl_rear))
      #PCIC_incl_overall <- unique(c(incl_mig, incl_spawn, incl_rear))
    } else {
      PCIC_incl_overall <- unique(c(PCIC_incl_overall, incl_spawn, incl_rear))
      #PCIC_incl_overall <- unique(c(PCIC_incl_overall, incl_mig, incl_spawn, incl_rear))
    }
    
    # Plot
    pdf(file = paste0("freshwater/output/freshwater_distribution_fraser_", spp_lookup_run[s,1], "_", cuid.s[i], ".pdf"), width = 8, height = 10)
    #png(paste0("freshwater/output/freshwater_distribution_fraser_", spp_lookup_run[s,1], "_", cuid.s[i], ".pdf"))
    
    plot(st_geometry(grid_polys[unique(c(incl_spawn, incl_rear)),]), col = NA, border = NA, axes = TRUE)
    #plot(st_geometry(grid_polys[unique(c(incl_mig, incl_spawn, incl_rear)),]), col = NA, border = NA, axes = TRUE) 
    plot(st_geometry(cu_boundary.i), col = grey(0.8), border = NA, add = TRUE)
    
    #plot(st_geometry(grid_polys[incl_mig,]), col = paste0(col_pal[3], 50), border = col_pal[3], lwd = 0.5, add = TRUE) 
    plot(st_geometry(grid_polys[incl_spawn,]), col = paste0(col_pal[2], 50), border = col_pal[2], lwd = 0.5, add = TRUE)
    
    # Rearing (not for pink and chum)
    if(spp_lookup_run$species_pooled[s] %in% c("Chum", "Pink") == FALSE) {
      plot(st_geometry(grid_polys[PCIC_incl[[I, "fw_rearing"]],]), col = paste0(col_pal[1], 50), border = col_pal[1], lwd = 0.5, add = TRUE)
      plot(st_geometry(rearing.i), col = col_pal[1], add = TRUE, lwd = 1)
    } 
    
    plot(st_geometry(shoreline), add = TRUE)
    plot(st_geometry(spawning.i), col = col_pal[2], add = TRUE, lwd = 2)
    #plot(st_geometry(mig_paths.i), col = col_pal[3], add = TRUE, lwd = 0.8)
    
    legend("topright", col = c(grey(0.8), col_pal), lwd = c(10, rep(3, 3)), c("CU boundary", "Rearing", "Spawning"), bty = "n")
    #legend("topright", col = c(grey(0.8), col_pal), lwd = c(10, rep(3, 3)), c("CU boundary", "Rearing", "Spawning", "Migration"), bty = "n")
    mtext(side = 3, adj = 0, paste0(cu_run$cuname[cu_run$cuid == cuid.s[i]], " ", cu_run$species_pooled[cu_run$cuid == cuid.s[i]]), font = 2)
    dev.off()
    
    print(".")
  } # end CU i
  print(paste0("End ", spp_lookup_run$species_pooled[s]))
} # end species s



saveRDS(PCIC_incl, file = "freshwater/output/PCIC_incl.rds")
write.csv(PCIC_incl_overall, file = "freshwater/output/PCIC_incl_overall.csv", row.names = FALSE)
