###############################################################################
#
# 2b_fw_species_dist.R
#
#  For each species and life stage, get indicator stats for overlapping grid cells
#
###############################################################################

spp_run <- spp_lookup$species_pooled

output_loc <- here("freshwater", "output", "species")

# For each species
for(s in 1:length(spp_run))  {
  
  #-------------------------------------------------------------
  # Subset grids by species
  
  # Subset spawning and rearing streams for that species (this is a bit slow)
  spawning.s <- streams %>%
    filter(.data[[paste0("model_spawning_", spp_lookup$streams_code[s])]] == 1)
  
  obs.spawning.s <- streams %>%
    filter(.data[[paste0("obsrvd_spawning_", spp_lookup$streams_code[s])]] == 1)
  #obs.spawning.s <- streams[which(obs.spawn == 1), ]
  
  # if(spp_lookup_run$species_pooled[s] %in% c("Chum", "Pink") == FALSE) { # No rearing distributions for pink and chum
  #   rearing.s <- streams %>%
  #     filter(.data[[paste0("model_rearing_", spp_lookup$streams_code[s])]] == 1)
  # }
  
  # Subset boundaries for all CUs of that species
  cu_boundary.s <- cu_boundary[cu_boundary$species_pooled == spp_run[s], ]
  
  # SPAWNING: Find grid_id that intersect freshwater *spawning*
  #dum.spawn <- st_intersects(spawning.s, cu_boundary.i, sparse = FALSE)
  #spawning.i <- spawning.s[which(dum.spawn == TRUE),]
  
  #intrscts_spawn <- st_intersects(spawning.s, grid_polys, sparse = FALSE) 
  #incl_spawn <- which(apply(intrscts_spawn, 2, sum) > 0)  #spawning grid cells to include for species
  
  
  #mask cells that overlap with CU spawning boundaries for species
  PCIC.species <- PCIC[cu_boundary.s]
  
  #mask cells that overlap with spawning streams for species
  PCIC.spawn <- PCIC[spawning.s]
  
  PCIC.obs.spawn <- PCIC[obs.spawning.s]
  
  
  #---------------------------------------------------------------------------
  ## STATISTICS
  
  #turn STAR object into a dataframe for statistics
  PCIC.species_data <- PCIC.species %>% 
    as.data.frame() %>%
    mutate(spp = spp_run[s]) %>%
    relocate(spp)
  
  #calculate mean for each indicator and time period
  temp_mean <- PCIC.species_data %>%
    group_by(spp, time) %>%
    summarize(across(5:last_col(), \(x) mean(x, na.rm = TRUE)))
  
  #calculate median absolute deviation for each indicator and time period
  temp_MAD <- PCIC.species_data %>%
    group_by(spp, time) %>%
    summarize(across(5:last_col(), \(x) mad(x, na.rm = TRUE)))
  
  #calculate difference in means between time periods
  PCIC.species_diff <- temp_mean[2,-1] - temp_mean[1,-1] 
  
  if(s == 1) {
    PCIC.species_mean <- temp_mean
    PCIC.species_MAD  <- temp_MAD
  }  else {
    PCIC.species_mean <- rbind(PCIC.species_mean, temp_mean)
    PCIC.species_MAD <- rbind(PCIC.species_MAD, temp_MAD)
  }
  #------------------------------------------------------------------------
  # PLOTS
  
  pdf(file = here(output_loc, paste0(spp_run[s], "_streams", ".pdf")), width = 8, height = 10)
  
  ##sf plot of spawning streams for the species
  plot(st_geometry(Fr_basin, border = "black"))
  plot(st_geometry(spawning.s), col = col_pal[2], add = TRUE)
  plot(st_geometry(obs.spawning.s), add = TRUE, lwd = 1.2, col  = col_pal[1])
  plot(st_geometry(shoreline), add = TRUE)
  plot(st_geometry(cu_boundary.s), add = TRUE, border = grey(0.8))
  
  
  legend("topright", col = c(grey(0.8), col_pal[1], col_pal[2]), lwd = c(10, rep(3, 3)), c("CU boundary", "Spawning (observed)", "Spawning (predicted)"), bty = "n")
  legend("topright", col = c(grey(0.8), col_pal[1], col_pal[2]), lwd = c(10, rep(3, 3)), c("CU boundary", "Spawning (observed)", "Spawning (predicted)"), bty = "n")
  
  dev.off()
  
  
  plot(PCIC.species[2])
  plot(cu_boundary.s, border = grey(0.8))
  plot(st_geometry(shoreline), add = TRUE)
  
  # for(g in 1:length(PCIC.species)){
  #   
  #   a1  <- ggplot() + 
      # ggplot() +
      # geom_stars(data = PCIC.species) +
      # facet_wrap(~time) +
      # theme_void() +
      # scale_fill_viridis() +
      # geom_sf(data = cu_boundary.s, fill = NA, color = "black")
  #   
  #   
  #   if(g == 1) show_star <- a1
  #   else show_star <- c(show_star, list(a1))
  #   
  # }
  # 
  # grid.arrange(show_star, grobs = show_star, nrow = length(show_star))
  # 
}

write.csv(PCIC.species_mean, here(output_loc, "PCIC_mean_ind.csv") )
write.csv(PCIC.species_MAD, here(output_loc, "PCIC_mad_ind.csv") )
  