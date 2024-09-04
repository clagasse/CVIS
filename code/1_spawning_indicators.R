### 1_spawning_indicators.R



#-----------------------------------------------------------------------------
# Loop through to extract grid cell ids
#-----------------------------------------------------------------------------

PCIC_incl <- vector(mode = 'list', 
                    length = length(cuid)*length(stages))
dim(PCIC_incl) <- c(length(cuid), length(stages))
dimnames(PCIC_incl) <- list(cuid, stages)


I <- 0 # Overall cu # counter

# For each species
for(s in 1:2){
  
  # Subset spawning and rearing streams for that species (this is a bit slow)
  ind.spawn <- streams %>% 
    as.data.frame %>% 
    select(paste0("model_spawning_", spp_lookup$streams_code[s]))
  spawning.s <- streams[which(ind.spawn == 1), ]
  
  # if(spp_lookup$species_pooled[s] %in% c("Chum", "Pink") == FALSE) { # No rearing distributions for pink and chum
  # ind.rear <- streams %>% 
  # 	as.data.frame %>% 
  # 	select(paste0("model_rearing_", spp_lookup$streams_code[s]))
  # rearing.s <- streams[which(ind.rear == 1), ]
  # }
  
  # # Plot all CUs for that species
  #plot(st_geometry(spawning.s), col = cols[2])
  # plot(st_geometry(obs.spawning.s), add = TRUE, lwd = 1.2, col  = cols[3])
  # plot(st_geometry(shoreline), add = TRUE)
  # plot(st_geometry(rearing.s), col = cols[1], lwd = 0.5)
  # plot(st_geometry(shoreline), add = TRUE)
  # 
  
  # Extract cuids for the selected species
  cuid.s <- cu_list$cuid[which(cu_list$species_pooled == spp_lookup$species_pooled[s])]
  # cu_list$cu_name_pse[which(cu_list$species_pooled == spp_lookup$species_pooled[s])]
  
  # For each CU of that species
  #for(i in 1:length(cuid.s)){
  for(i in 1:2){
    # Subset CU boundary
    cu_boundary.i <- cu_boundary[cu_boundary$CUID == cuid.s[i], ]
    
    # SPAWNING: Find grid_id that intersect freshwater *spawning*
    dum.spawn <- st_intersects(spawning.s, cu_boundary.i, sparse = FALSE)
    spawning.i <- spawning.s[which(dum.spawn == TRUE),]
    
    intrscts_spawn <- st_intersects(spawning.i, grid_polys, sparse = FALSE)
    incl_spawn <- which(apply(intrscts_spawn, 2, sum) > 0)
    
    # # REARING: Find grid_id that intersect freshwater *rearing* (includes migration)
    # if(spp_lookup$species_pooled[s] %in% c("Chum", "Pink") == FALSE) { # No rearing distributions for pink and chum
    #   
    #   # EXCEPTION: Harrison upstream-migrating late. This CU boundary includes spawning habitat but not the rearing lake. Use Harrison downstream migrating CU boudnary to determine rearing habitat.
    #   if(cu_list$cu_name_pse[which(cu_list$species_pooled == spp_lookup$species_pooled[s])[i]] == "Harrison-Upstream Migrating-Late"){
    #     
    #     dum.rear <- st_intersects(rearing.s, cu_boundary[cu_boundary$cuname == "Harrison-Downstream Migrating-Late", ], sparse = FALSE)
    #     
    #   } else { # If any other CU, use CU boubndary
    #     
    #     dum.rear <- st_intersects(rearing.s, cu_boundary.i, sparse = FALSE)
    #   }
    #   
    #   rearing.i <- rearing.s[which(dum.rear == TRUE),]
    #   
    #   intrscts_rear <- st_intersects(rearing.i, grid_polys, sparse = FALSE)
    #   incl_rear <- which(apply(intrscts_rear, 2, sum) > 0)
    # }
    # 
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
    # if(spp_lookup$species_pooled[s] %in% c("Chum", "Pink") == FALSE) {
    #   PCIC_incl[[I, "fw_rearing"]] <- unique(c(incl_mig, incl_rear))
    # } else {
    #   PCIC_incl[[I, "fw_rearing"]] <- NULL
    # }
    
    if(I == 1){
      PCIC_incl_overall <- unique(c(incl_spawn))
    } else {
      PCIC_incl_overall <- unique(c(PCIC_incl_overall, incl_spawn))
    }
    
    # Plot
    pdf(file = paste0("freshwater/output/freshwater_distribution_fraser_", spp_lookup[s,1], "_", cuid.s[i], ".pdf"), width = 8, height = 10)
    #png(paste0("freshwater/output/freshwater_distribution_fraser_", spp_lookup[s,1], "_", cuid.s[i], ".pdf"))
    
    plot(st_geometry(grid_polys[unique(c(incl_spawn)),]), col = NA, border = NA, axes = TRUE) 
    plot(st_geometry(cu_boundary.i), col = grey(0.8), border = NA, add = TRUE)
    
    #plot(st_geometry(grid_polys[incl_mig,]), col = paste0(col_pal[3], 50), border = col_pal[3], lwd = 0.5, add = TRUE) 
    plot(st_geometry(grid_polys[incl_spawn,]), col = paste0(col_pal[2], 50), border = col_pal[2], lwd = 0.5, add = TRUE)
    
    # # Rearing (not for pink and chum)
    # if(spp_lookup$species_pooled[s] %in% c("Chum", "Pink") == FALSE) {
    #   plot(st_geometry(grid_polys[PCIC_incl[[I, "fw_rearing"]],]), col = paste0(cols[1], 50), border = cols[1], lwd = 0.5, add = TRUE)
    #   plot(st_geometry(rearing.i), col = cols[1], add = TRUE, lwd = 1)
    # } 
    
    plot(st_geometry(shoreline), add = TRUE)
    plot(st_geometry(spawning.i), col = col_pal[2], add = TRUE, lwd = 2)
    #plot(st_geometry(mig_paths.i), col = col+pal[3], add = TRUE, lwd = 0.8)
    
    legend("topright", col = c(grey(0.8), col_pal), lwd = c(10, rep(3, 3)), c("CU boundary", "Rearing", "Spawning", "Migration"), bty = "n")
    mtext(side = 3, adj = 0, paste0(cu_list$cu_name_pse[cu_list$cuid == cuid.s[i]], " ", cu_list$species_pooled[cu_list$cuid == cuid.s[i]]), font = 2)
    dev.off()
    
    print(cuid.s[i])
  } # end CU i
  print(paste0("End ", spp_lookup$species_pooled[s]))
} # end species s



saveRDS(PCIC_incl, file = here("freshwater", "output" , "PCIC_incl.rds"))
write.csv(PCIC_incl_overall, file = "freshwater/output/PCIC_incl_overall.csv", row.names = FALSE)





for(i in 1:n.CUs){
  # i <- 47 #Bowron
  #------------------------------------------------------------------------------
  # Loop through each life stage
  #------------------------------------------------------------------------------
  
  for(j in 1:length(stages)){
    
    # If there are timing data
    if(length(which(timing$cuid == cuid[i] & timing$stage == stages[j])) == 0){
      stop(paste0("No timing data for cuid ", cu_list$cuid[i], " ", stages[j]))
      
    } else {
      
      i    f(cu_list$species_pooled[i] %in% c("Pink", "Chum") & stages[j] == "fw_rearing"){
        # No freshwater rearing for pink and chum = zero exposure during this stage
        fw_output[m, i, j, , , ] <- 0
        fw_spat[[i,j]][m, , , ] <- 0
        
      } else {
        # Extract timing and covert to day-of-year
        timing.ij <- timing[which(timing$cuid == cuid[i] & timing$stage == stages[j])[1], c("start", "n.days")]
        
        #-----------------------------------------------------------------------------
        # Identify grid cells for given life stage
        #-----------------------------------------------------------------------------
        
        # See freshwater-grid.R for code that identifies which grid cells should be used
        # for each life stage.
        
        incl <- incl.stages[[i,j]]
        
        # Dimensions
        n.grid <- length(incl)
        
        #-----------------------------------------------------------------------------
        # Subset stream temp relevant to CU and stage
        #-----------------------------------------------------------------------------
        Ts.ij <- Qs.ij <- array(NA,
                                dim = c(n.grid, 4, timing.ij$n.days, 30),
                                dimnames = list(incl, c("hist", "early", "mid", "late"), NULL, NULL))
        # Note: for stages that span >365 days, some temps may be counted twice, because essentially two cohorts occupy that space at the same time.
        
        for(p in 1:4){ # for each period
          for(y in 1:30){ # for each year in the period
            start.ind <- which(period == c("hist", "early", "mid", "late")[p] & years == period.years[y, p] & DOY == timing.ij$start)
            
            # For the last year in late century, can't extend if start + n.days > 365
            if((start.ind + timing.ij$n.days - 1) > dim(Ts.weeklyMax)[2]){
              
              n.available <- length(start.ind:(dim(Ts.weeklyMax)[2]))
              
              Ts.ij[, p, 1:n.available, y] <- Ts.weeklyMax[match(incl, grid.ref), start.ind:(dim(Ts.weeklyMax)[2])]
              Qs.ij[, p, 1:n.available, y] <- Qs.weeklyMean[match(incl, grid.ref), start.ind:(dim(Ts.weeklyMax)[2])]
              
            } else {
              
              Ts.ij[, p, , y] <- Ts.weeklyMax[match(incl, grid.ref), start.ind:(start.ind + timing.ij$n.days - 1)]
              Qs.ij[, p, , y] <- Qs.weeklyMean[match(incl, grid.ref), start.ind:(start.ind + timing.ij$n.days - 1)]
            }
            
          }}
        
          if(3 == 2){
          # Plot flow
          xDate <- as.Date(paste("1998", 1:365, sep = "-"), format = "%Y-%j")
          
          h <- which(period == c("hist"))
          e <- which(period == c("early"))
          m <- which(period == c("mid"))
          l <- which(period == c("late"))
          xx <- 10
          # xx <- 198
          quartz(width = 8, height = 5, pointsize = 12)
          plot(xDate, tapply(Qs.weeklyMean[match(incl, grid.ref)[xx], h], DOY[h], median)[1:365], "l", col = cols[5], lwd = 2, log = "y")
          abline(h = 0.2*mean(Qs.weeklyMean[match(incl, grid.ref)[xx], which(period == "hist")], na.rm = TRUE), lty = 2)
          
          polygon(x = c(xDate, rev(xDate)),
                  y = c(tapply(Qs.weeklyMean[match(incl, grid.ref)[xx], h], DOY[h], quantile, 0.25)[1:365],
                        rev(tapply(Qs.weeklyMean[match(incl, grid.ref)[xx], h], DOY[h], quantile, 0.75)[1:365])),
                  col = paste0(cols[5], 30),
                  border = NA)
          
          lines(xDate, tapply(Qs.weeklyMean[match(incl, grid.ref)[xx], m], DOY[m], median)[1:365], lwd = 2, col = cols[3])
          
          polygon(x = c(xDate, rev(xDate)),
                  y = c(tapply(Qs.weeklyMean[match(incl, grid.ref)[xx], m], DOY[m], quantile, 0.25)[1:365],
                        rev(tapply(Qs.weeklyMean[match(incl, grid.ref)[xx], m], DOY[m], quantile, 0.75)[1:365])),
                  col = paste0(cols[3], 30),
                  border = NA)
          
          # Stream temp
          plot(xDate, tapply(Ts.weeklyMax[match(incl, grid.ref)[xx], h], DOY[h], median)[1:365], "l", col = cols[5], lwd = 2, ylim = c(0, 25))
          
          polygon(x = c(xDate, rev(xDate)),
                  y = c(tapply(Ts.weeklyMax[match(incl, grid.ref)[xx], h], DOY[h], quantile, 0.25)[1:365],
                        rev(tapply(Ts.weeklyMax[match(incl, grid.ref)[xx], h], DOY[h], quantile, 0.75)[1:365])),
                  col = paste0(cols[5], 30),
                  border = NA)
          
          lines(xDate, tapply(Ts.weeklyMax[match(incl, grid.ref)[xx], m], DOY[m], median)[1:365], lwd = 2, col = cols[3])
          
          polygon(x = c(xDate, rev(xDate)),
                  y = c(tapply(Ts.weeklyMax[match(incl, grid.ref)[xx], m], DOY[m], quantile, 0.25)[1:365],
                        rev(tapply(Ts.weeklyMax[match(incl, grid.ref)[xx], m], DOY[m], quantile, 0.75)[1:365])),
                  col = paste0(cols[3], 30),
                  border = NA)
          
          # Plot propotion of days
          quartz(width = 12, height = 3, pointsize = 12)
          par(mar = c(4,4,2,1))
          plot(date[seq(1, 47482, 14)], Ts.weeklyMax[match(incl, grid.ref)[xx], seq(1, 47482, 14)], "l", las = 1, ylab = "Stream temperature", xlab = "Date", bty = "l")
          polygon(x = date[c(range(h), rev(range(h)))], y = c(-1, -1, 40, 40), border = NA, col = paste0(cols[5], 20))
          polygon(x = date[c(range(e), rev(range(e)))], y = c(-1, -1, 40, 40), border = NA, col = paste0(cols[3], 20))
          polygon(x = date[c(range(m), rev(range(m)))], y = c(-1, -1, 40, 40), border = NA, col = paste0(cols[4], 20))
          polygon(x = date[c(range(l), rev(range(l)))], y = c(-1, -1, 40, 40), border = NA, col = paste0(cols[1], 20))
          
          # Plot days over threshold
          plot(1970:1999, apply(Ts.ij[xx, 1, , ] > tmax[i,j], 2, sum), xlim = c(1950, 2100), col = paste0(cols[5], 40), ylim = c(0, timing.ij$n.days), ylab = "Number of days over threshold", xlab = "", pch = 19, bty = "l", las = 1)
          segments(x0 = 1970, x1 = 1999, y0 = sum(Ts.ij[xx, 1, , ] > tmax[i,j])/30, y1 = sum(Ts.ij[xx, 1, , ] > tmax[i,j])/30, col = cols[5], lwd = 3)
          
          for(p in 2:4){
            points(period.years[, p], apply(Ts.ij[xx, p, , ] > tmax[i,j], 2, sum), col = paste0(cols[c(3,4,1)[p-1]], 40), pch = 19)
            segments(x0 = period.years[1, p], x1 = period.years[30, p], y0 = sum(Ts.ij[xx, p, , ] > tmax[i,j])/30, y1 = sum(Ts.ij[xx, p, , ] > tmax[i,j])/30, col = cols[c(3,4,1)[p-1]], lwd = 3)
          }
          abline(v = c(1969.5, 1999.5, 2009.5, 2039.5, 2069.5), col = grey(0.8))
          
          # Proportion of stage over threshold
          plot(1970:1999, apply(Ts.ij[xx, 1, , ] > tmax[i,j], 2, sum)/timing.ij$n.days, xlim = c(1950, 2100), col = paste0(cols[5], 40), ylim = c(0, 1), ylab = "Proportion of time over threshold", xlab = "", pch = 19, bty = "l", las = 1)
          segments(x0 = 1970, x1 = 1999, y0 = sum(Ts.ij[xx, 1, , ] > tmax[i,j])/30/timing.ij$n.days, y1 = sum(Ts.ij[xx, 1, , ] > tmax[i,j])/30/timing.ij$n.days, col = cols[5], lwd = 3)
          
          for(p in 2:4){
            points(period.years[, p], apply(Ts.ij[xx, p, , ] > tmax[i,j], 2, sum)/timing.ij$n.days, col = paste0(cols[c(3,4,1)[p-1]], 40), pch = 19)
            segments(x0 = period.years[1, p], x1 = period.years[30, p], y0 = sum(Ts.ij[xx, p, , ] > tmax[i,j])/30/timing.ij$n.days, y1 = sum(Ts.ij[xx, p, , ] > tmax[i,j])/30/timing.ij$n.days, col = cols[c(3,4,1)[p-1]], lwd = 3)
          }
          abline(v = c(1969.5, 1999.5, 2009.5, 2039.5, 2069.5), col = grey(0.8))
          
        }
        
        # Plot temperature dots over threshold
        if(3 == 2){
          xx <- 1 # which grid cell
          start.ind <- which(period == c("hist", "early", "mid", "late")[p] & years == period.years[y, p] & DOY == timing.ij$start)
          
          # Plot 5 years
          par(mar = c(4,4,4,1), bg = "white")
          plot(date[years %in% period.years[1:5, p]], Ts.weeklyMax[match(incl[xx], grid.ref), which(years %in% period.years[1:5, p])], "l", xlab = "Year", ylab = "Weekly max. temp (˚C)", las = 1, bty = "l", xaxs ="i", col = grey(0.5))
          
          for(y in 1:5){
            start.ind <- which(years %in% period.years[y, p] & DOY == timing.ij$start)
            points(date[start.ind:(start.ind + timing.ij$n.days - 1)], Ts.ij[xx, p, , y], pch = 19, cex =0.6)
            segments(x0 = date[start.ind],
                     y0 = 31 + (y-1),
                     x1 = date[(start.ind + timing.ij$n.days - 1)],
                     y1 = 31 + (y-1),
                     col = paste0(cols[y], 60), 
                     lwd = 10,
                     xpd = NA
            )
          }
          mtext(side = 3, paste(cu_list$Conservation.Unit[i], cu_list$Species[i], "-", stages[j]), line = 3)
          abline(h = pmax(quantile(Ts.ij[xx, 1, , ], c(0.9, 0.975), na.rm = TRUE), c(24, 26)), lty = c(2, 3)) 
          
        } # end if 3 == 2 plot
        
        # Collapse Ts.ij years and days for easy
        dim(Ts.ij) <- c(n.grid, 4, 30*timing.ij$n.days) 
        dim(Qs.ij) <- c(n.grid, 4, 30*timing.ij$n.days) 
        
        #-----------------------------------------------------------------------------
        # Stream temperature 
        #-----------------------------------------------------------------------------
        
        #----------------------------------------------------------------------
        # Calculate days above threshold (DAT) and % DAT
        #----------------------------------------------------------------------
        Ts.dat <- Ts.perc <- array(NA, 
                                   dim = c(4, n.grid),
                                   dimnames = list(c("hist", "early", "mid", 'late'), NULL)
        )
        
        for(p in 1:4){ # For each period
          Ts.dat[p, ] <- apply(Ts.ij[, p, ] > matrix(rep(tmax[i, j], 30*timing.ij$n.days), n.grid, 30*timing.ij$n.days), 1, sum, na.rm = TRUE)
          
          # Percentage of days;
          # Need to account for late-century period not being able to extend into next year for 2099 if timing.ij$start + timing.ij$n.days > 365
          Ts.perc[p, ] <- Ts.dat[p, ] / sum(!is.na(Ts.ij[1, p, ]))
        } # end p
        
        
        # Store full spatial output
        fw_spat[[i,j]][m, "temp", , ] <- Ts.perc
        
        # store summary output
        for(p in 1:4){ # for each period
          # Store output in end array
          fw_output[m, i, j, "temp", p, ] <- c(
            median(Ts.perc[p, ]),
            range(Ts.perc[p, ]))
          
        } # end p
        
        #-----------------------------------------------------------------------------
        # Flow
        #-----------------------------------------------------------------------------
        
        # Calculate MAD over historical period for each included grid cell
        # (Use all DOY, not Qs.ij for life stage)
        if(n.grid > 1){
          mad <- apply(Qs.weeklyMean[match(incl, grid.ref), which(period == "hist")], 1, mean, na.rm = TRUE)
        } else { # if just one grid cell
          mad <- mean(Qs.weeklyMean[match(incl, grid.ref), which(period == "hist")], na.rm = TRUE)
        }
        
        # Calculate acute (critical) and chronic (optimal) temperature thresholds
        thresh_mad <- mad %*% matrix(c(0.2, 0.1), nrow = 1)
        colnames(thresh_mad) <- c("opt", "crit")
        
        # Calculate days below % mad threshold
        Qs.dbt <- Qs.perc <- array(NA, 
                                   dim = c(4, n.grid),
                                   dimnames = list(c("hist", "early", "mid", 'late'), 
                                                   NULL)
        )
        
        k <- 1 # Use 20% MAD threshold for now
        
        for(p in 1:4){
          Qs.dbt[p, ] <- apply(Qs.ij[, p, ] < matrix(rep(thresh_mad[, k], 30*timing.ij$n.days), n.grid, 30*timing.ij$n.days), 1, sum, na.rm = TRUE)
          
          # Percentage of days;
          # Need to account for late-century period not being able to extend into next year for 2099 if timing.ij$start + timing.ij$n.days > 365
          Qs.perc[p, ] <- Qs.dbt[p, ] / sum(!is.na(Qs.ij[1, p, ]))
          
        } # end p
        
        
        # Store full spatial output
        fw_spat[[i,j]][m, "flow", , ] <- Qs.perc
        
        for(p in 1:4){ # for each period
          # Store output in end array
          fw_output[m, i, j, "flow", p, ] <- c(
            median(Qs.perc[p, ]),
            range(Qs.perc[p, ]))
        }
        
      } # if if not pink/chum fw_rearing
    } # end if there are timing data
    
  } # end life stage j
  
  print(paste0("Done ", cuid[i]))
  
} # end CU i






