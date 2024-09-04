###############################################################################
#
# 2c_fw_CU_dist.R
#
#  For each CU and life stage, get indicator stats and make simple plots
#
###############################################################################

spp_run <- spp_lookup$species_pooled

output_loc <- here("freshwater", "output", "CU")

if(exists("PCIC_stats")) rm(PCIC_stats)

#----------------------------------------------------
# Loop by SPECIES
for(s in 1:length(unique(cu_run$spp)))  {
  
  # Subset spawning and rearing streams for that species (this is a bit slow)
  spawning.s <- streams %>%
    filter(.data[[paste0("model_spawning_", spp_lookup$streams_code[s])]] == 1)
  
  obs.spawning.s <- streams %>%
    filter(.data[[paste0("obsrvd_spawning_", spp_lookup$streams_code[s])]] == 1)
  
  # if(spp_lookup_run$species_pooled[s] %in% c("Chum", "Pink") == FALSE) { # No rearing distributions for pink and chum
  #   rearing.s <- streams %>%
  #     filter(.data[[paste0("model_rearing_", spp_lookup$streams_code[s])]] == 1)
  # }
  
  # Extract cuids for the selected species
  cuid.s <- cu_run$cuid[which(cu_run$species_pooled == spp_lookup_run$species_pooled[s])]
  cu_run.s <- cu_run[which(cu_run$species_pooled == spp_lookup_run$species_pooled[s]),]
  # cu_run$cu_name_pse[which(cu_run$species_pooled == spp_lookup_run$species_pooled[s])]
  
  # For each CU of that species
  for(i in 1:length(cuid.s)){
    # Subset CU boundary
    cu_boundary.i <- cu_boundary[cu_boundary$CUID == cuid.s[i], ]
    
    #mask cells that overlap with CU spawning boundaries for species
    PCIC.CU <- PCIC[cu_boundary.i]
    
    #mask cells that overlap with spawning streams for species
    #PCIC.CU.spawn <- PCIC.CU[spawning.s]
    #PCIC.obs.spawn <- PCIC.CU[obs.spawning.s]
    
    #---------------------------------------------------------------------------
    ## STATISTICS by CU
    
    #turn STAR object into a dataframe for statistics
    PCIC.CU_data <- PCIC.CU %>% 
      split("time") %>%
      as.data.frame() %>%
      mutate(spp = cu_run.s$Species[i], cu = cu_run.s$cuname[i], cuid = cu_run.s$cuid[i]) %>%
      relocate(spp, cu, cuid)
    
    #calculate mean for each indicator and time period
    temp_stats <- PCIC.CU_data %>%
      group_by(spp, cu, cuid) %>%
      summarize(across(contains(names(PCIC.CU)), \(x) mean(x, na.rm = TRUE))) %>%
      rename_with(~str_c(., ".mean"), .cols = -c(1:3))
    
    #get the difference in means between projected and historic time periods
    source_vars <- seq(from = 5, to = ncol(temp_stats), by = 2)
    use_vars <- seq(from = 4, to = ncol(temp_stats), by = 2)
    
    temp_diff <- PCIC.CU_data %>%
      group_by(spp, cu, cuid) %>%
      summarize() %>%
      cbind(purrr::map2_dfr( temp_stats[source_vars], temp_stats[use_vars], \(x,y) x-y)) %>%
      rename_with(~str_replace(., '2055.07.02', "diff")) %>%
      cbind(purrr::map2_dfr( temp_stats[source_vars], temp_stats[use_vars], \(x,y) x/y)) %>%
      rename_with(~str_replace(., '2055.07.02', "prct_change")) 
      
    #calculate median absolute deviation for each indicator and time period
    temp_MAD <- PCIC.CU_data %>%
      group_by(spp, cu, cuid) %>%
      summarize(across(contains(names(PCIC.CU)), \(x) mad(x, na.rm = TRUE))) %>%
      rename_with(~str_c(., ".MAD"), .cols = -c(1:3))
    
    # temp_MAD_ave <- PCIC.CU_data %>%
    #   group_by(spp, cu) %>%
    #   summarize() %>%
    #   cbind(purrr::map2_dfr( temp_MAD[source_vars], temp_MAD[use_vars], mean())) %>%
    #   rename_with(~str_replace(., '.1985.07.02', ".meanMAD"))
    
    
    #bind stats into one object
    temp_stats <- cbind(temp_stats, temp_diff[,-c(1:3)], temp_MAD[,-c(1:3)])
    
    if(!exists("PCIC_stats")) {
      PCIC_stats <- temp_stats
    }  else {
      PCIC_stats <- rbind(PCIC_stats, temp_stats)
    }
    #------------------------------------------------------------------------
    # PLOTS
    
    if(make_CU_plots == TRUE) {
      pdf(file = here(output_loc, paste0(spp_run[s], "_", cu_run.s$cuname[i], "_streams", ".pdf")), width = 8, height = 10)
      
      ##sf plot of spawning streams for the species
      plot(st_geometry(cu_boundary.i), border = grey(0.8))
      plot(st_geometry(spawning.s), col = col_pal[1], add = TRUE)
      plot(st_geometry(obs.spawning.s), add = TRUE, lwd = 1.2, col  = col_pal[2])
      plot(st_geometry(shoreline), add = TRUE)
      
      legend("topright", col = c(grey(0.8), col_pal[1], col_pal[2]), lwd = c(10, rep(3, 3)), c("CU boundary", "Spawning (predicted)", "Spawning (observed)"), bty = "n")
      legend("topright", col = c(grey(0.8), col_pal[1], col_pal[2]), lwd = c(10, rep(3, 3)), c("CU boundary", "Spawning (predicted)", "Spawning (observed)"), bty = "n")
      
      dev.off()
      
      pdf(file = here(output_loc, paste0(spp_run[s], "_", cu_run.s$cuname[i], "_PCIC_indicators", ".pdf")))
      
      for(g in 1:length(PCIC.CU)){
        
        ggplot() +
          geom_stars(data = PCIC.CU[g]) +
          coord_equal() +
          facet_wrap(~time) +
          theme_void() +
          scale_fill_viridis()
        
        # if(g == 1) show_star <- a1
        # else show_star <- c(show_star, list(a1))
        
      }
      
      dev.off()
      # grid.arrange(show_star, grobs = show_star, nrow = length(show_star))
      # 
    }
    
    rm(temp_diff, temp_MAD, temp_stats, PCIC.CU, PCIC.CU_data, cu_boundary.i)
  }  # end CU loop
  
  rm(obs.spawning.s, spawning.s)
  
} # end species loop

write.csv(PCIC_stats, here(output_loc, "PCIC_stats.csv") )


