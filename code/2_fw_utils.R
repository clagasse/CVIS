###############################################################################
# Functions to load and summarize data relevant to exposure indicators

###############################################################################

## simple calculation functions
calculate_subset_means_all <- function(df, index_list) {
  # Select only numeric columns
  numeric_df <- df %>% select_if(is.numeric)
  
  # For each subset, calculate means of all numeric columns
  map_df(index_list, function(indices) {
    numeric_df[indices, ] %>%
      summarise(across(everything(), \(x) mean(x, na.rm = TRUE)))
  }, .id = "subset")
}

greater_zero <- function(x) {
  if_else(x > 0, T, F)
}



#weighted mean
wmean <- function(x, length_metre, na.rm = TRUE) {
  sum(x * length_metre, na.rm = TRUE) / sum(length_metre, na.rm = TRUE)
}
#weighted standard deviation
wsd <- function(x, length_metre, na.rm = TRUE) {
  sqrt(Hmisc::wtd.var(x, weights = length_metre, na.rm = TRUE))
}
#weighted quantiles
wqt <- function(x, length_metre, prob = 0.025, na.rm = TRUE) {
  if(!sum(is.na(x)) == 0) return(NA)
  y <- Hmisc::wtd.quantile(x, weights = length_metre, probs = prob, na.rm = TRUE)
  unname(y)
}



###############################################################################
# Function to load PCIC model output for given model and variable
###############################################################################

loadPCIC_ind <- function(
    variable = "POT19freq_year_aClimMean", # Which variable to load?
    model = "ensMean", # Which GCM? ensMean = ensemble Mean
    rcp = 45, # Which emissions scenario? Options 45 or 85, 
    verbose = FALSE #provide details on data set?
){
  
  # Define location of data
  root_dat <- here("data", "PCIC_indicators")
  
    # Two variables in VICGL output
  varNames <- data.frame(
    fileName = c("peakFlowDay_aClimMean", "POT19freq_year_aClimMean"),
    Name = c("peakQday_year", "POT19freq_year")
  )
  
  timeRanges <- c("1971-2000", "2041-2070")
  
  v <- which(varNames$fileName == variable)
  
  # Open netcdf file from first time period
  #historic <- nc_open(paste0(root_dat,"/", varNames$fileName[v], "_", model, "_VICGL-dynWat_rcp", rcp, "_", timeRanges[1], "_bccoast+fraser.nc"))
  historic <- read_ncdf(paste0(root_dat,"/", varNames$fileName[v], "_", model, "_VICGL-dynWat_rcp", rcp, "_", timeRanges[1], "_bccoast+fraser.nc"))
  
  #open netcdf file from second time period
  proj <- read_ncdf(paste0(root_dat,"/", varNames$fileName[v], "_", model, "_VICGL-dynWat_rcp", rcp, "_", timeRanges[2], "_bccoast+fraser.nc"))
  #proj <- nc_open(paste0(root_dat,"/", varNames$fileName[v], "_", model, "_VICGL-dynWat_rcp", rcp, "_", timeRanges[2], "_bccoast+fraser.nc"))
  
  if(verbose == TRUE) {
    # # Explore dataset. What are the variables?
    print(paste("File",historic$filename,"contains",historic$nvars,"variables"))
    for( i in 1:historic$nvars ) {
      t <- historic$var[[i]]
      print(paste("Here is information on variable number",i))
      print(paste("   Name: ",t$name))
      print(paste("   Units:",t$units))
      print(paste("   Missing value:",t$missval))
      print(paste("   # dimensions :",t$ndims))
      print(paste("   Variable size:",t$varsize))
    }
  }
  
  z <- cbind(historic, proj)
  
  varExtracted0     <-  ncvar_get(historic, varNames$Name[v])
  varExtracted_proj <-  ncvar_get(proj, varNames$Name[v])
  
  # Spatial variables
  lon <- ncvar_get(historic, "lon")
  lat <- ncvar_get(historic, "lat")
  
  var_all <- array(c(varExtracted0, varExtracted_proj), dim = c(length(lon), length(lat), 2))
  
  # Reduce dimensionality so that grid point[i, ] corresponds to var[i, ]
  varExtracted1 <- var_all
  dim(varExtracted1) <- c(length(lon)*length(lat), 2)
  
  dimnames(varExtracted1) <- list(
    1:nrow(varExtracted1),
    c("hist", "proj")
  )
  
  # grid_points <- data.frame(
  #   id = c(1:(length(lon)*length(lat))),
  #   lon = rep(lon, length(lat)),
  #   lat = rep(lat, each = length(lon)),
  #   lon_id = rep(c(1:length(lon)), length(lat)),
  #   lat_id = rep(c(1:length(lat)), each = length(lon)))
  # write.csv(grid_points, file = "freshwater/output/PCIC-grid-points_bccoast.csv")
  
  return(varExtracted1)
  
}





##########################################################################
### FWA functions
###################################################################


#function to subset downstream paths from a stream
downstream_path <- function(FWA, stream_pick, code_type = "FWA") {
  
  FWA_code <- stream_pick$localcode
  st <- str_length(FWA_code)
  if(code_type == "FWA") {
    FWA_code <- stream_pick$FWA_WATERS
    st <- str_locate(FWA_code, "000000")[1] - 2   #get stream code last position
  }

  st_level <- (st+4)/7
  
  for(n in 1:st_level) {
    
    #get FWA code for current FWA level of iteration
    c_cut <- (n-1) * 7
    s_pick <- str_sub(FWA_code, 1, st - c_cut)
    if(code_type == "FWA") s_pick <- str_c(s_pick, "-000000")
    
    #get candidate streams with lower FWA code
    ind <- str_equal(FWA$localcode, s_pick)
    if(code_type == "FWA") ind <- str_starts(FWA$FWA_WATERS, s_pick)
    candidates <- FWA[ind,] %>%
      filter(STREAM_ORD >= max(stream_pick$STREAM_ORD), STREAM_MAG >= max(stream_pick$STREAM_MAG))
    
    #get streams with lower FWA code that intersect with migration reaches
    FWA_int <- st_intersects(candidates, stream_pick, sparse = FALSE)
    FWA_int <- candidates[which(apply(FWA_int, 1, sum) > 0),]
    
    #get downstream_route_measure range from lowest intersecting reach
    if(code_type == "bcfp") {
      dd <- min(FWA_int$downstream_route_measure)
      #take candidate streams with lower downstream_route_measure range
      low_stream <- filter(candidates, DOWNSTREAM <= dd)
    }
    
    if(code_type == "FWA") {
      dd <- min(FWA_int$DOWNSTREAM)
      low_stream <- filter(candidates, DOWNSTREAM <= dd)
}
    
    #take all streams with downstream_route_measure distance below intersect
    stream_pick <- bind_rows(stream_pick, low_stream)
  }
  return(stream_pick)
}

#measure distance to ocean entry for each stream segment in a migration paths object
measure_downstream <- function(paths, code_type = "FWA") {
 
  dvec <- vector(length = nrow(paths))
  paths <- st_zm(paths)
  
  for(j in 1:nrow(paths)) {
    stream_pick <- paths[j,]
    
    FWA_code <- stream_pick$localcode
    st <- str_length(FWA_code)
    
    if(code_type == "FWA") {
      FWA_code <- stream_pick$FWA_WATERS
      st <- str_locate(FWA_code, "000000")[1] - 2   #get stream code last position
    }
    
    st_level <- (st+4)/7
    
    if(code_type == "FWA") s_pick <- str_sub(FWA_code,1,st)
    
    for(k in 1:st_level){
      # if(k == 1) {
      #   code <- paste0(str_sub(s_pick,1,3), "-000000")
      #   candidates <- filter(paths,  str_starts(FWA_WATERS, code))
      # }
      #if(k > 1)  {
      c_cut <- (k-1) * 7
      code <- paste0(str_sub(FWA_code, 1, st - c_cut), "-000000")
      #if(k == 1) code <- paste0(str_sub(s_pick,1,3), "-000000")
      temp_path <- filter(paths, str_starts(FWA_WATERS, code))
      
      if(k == 1) {
        candidates <- temp_path
        candidates <- filter(candidates, DOWNSTREAM <= stream_pick$DOWNSTREAM)
        
        
        #FWA_int <- st_intersects(candidates, stream_pick, sparse = FALSE)
        #path_int <- candidates[which(apply(FWA_int, 1, sum) > 0),]
        #candidates <- filter(candidates, DOWNSTREAM < max(path_int$DOWNSTREAM))
      } 
      if(k > 1) {
        #get streams with lower FWA code that intersect with migration reaches
        FWA_int <- st_intersects(temp_path, candidates, sparse = FALSE)
        FWA_int <- temp_path[which(apply(FWA_int, 1, sum) > 0),]
        if(nrow(FWA_int) == 0) next
        temp_path <- filter(temp_path, DOWNSTREAM <= FWA_int$DOWNSTREAM)
        candidates <- bind_rows(candidates, temp_path)
      }
      
      #if(k == st_level) tempcan <- filter(paths, FWA_WATERS == FWA_code &  DOWNSTREAM < stream_pick$DOWNSTREAM)
      
      candidates <- candidates %>%
        filter(STREAM_ORD >= max(stream_pick$STREAM_ORD, na.rm = T), STREAM_MAG >= max(stream_pick$STREAM_MAG, na.rm = T))
      
      
    }

    
    

    # if(code_type == "FWA") {
    #   candidates_1 <- filter(paths, FWA_WATERS == FWA_code)
    #   candidates_2 <- filter(paths, str_starts(FWA_WATERS == paste0(s_pick, "-000000"))) 
    #   
    #   str_starts(paths$FWA_WATERS, paste0(s_pick, "-000000"))
    #   candidates <- candidates$
    #   ind <- paths$FWA_WATERS %in% code
    #   
    # candidates <- paths[str_detect(paths, paste(codes, collapse="|")),] %>%
    #  filter((FWA_WATERS == FWA_code & DOWNSTREAM < stream_pick$DOWNSTREAM) | FWA_WATERS != FWA_code)
    #  
    #  remove <- filter(candidates, FWA_WATERS == FWA_code & DOWNSTREAM < stream_pick$DOWNSTREAM)
    
    # #get FWA code for stream
    # c_cut <- (st_level-1) * 7
    # s_pick <- str_sub(FWA_code, 1, st - c_cut)
    # if(code_type == "FWA") s_pick <- str_c(s_pick, "-000000")
    # if(code_type == "FWA") s_pick <- str_sub(FWA_code,1,st)
    # 
    # #get streams with lower FWA code
    # ind <- str_equal(paths$localcode, s_pick)
    # if(code_type == "FWA") ind <- str_starts(paths$FWA_WATERS, s_pick)
    # candidates <- paths[ind,] %>%
    #   filter(STREAM_ORD >= max(stream_pick$STREAM_ORD), STREAM_MAG >= max(stream_pick$STREAM_MAG))
    # 
    #get downstream_route_measure range from lowest intersecting reach
    if(code_type == "FWA") {
      dd <- sum(candidates$LENGTH_MET, na.rm = T) - (stream_pick$LENGTH_MET / 2)
    }
    
    dvec[j] <- dd
  }
  return(dvec)
}

#function to choose stream within a CU boundary for subsequent path analysis
choose_CU_stream <- function(FWA, cu_boundary, subset_order = FALSE, min_order = 5) {
  
  cu_FWA <- st_contains(cu_boundary, FWA)
  cu_FWA <- FWA[cu_FWA[[1]],]  #%>%
  
  if(subset_order == TRUE) cu_FWA <- filter(cu_FWA, stream_order >= min_order) 
  boundary_centre <- st_centroid(cu_boundary)
  
  cu_FWA_pick <- st_nearest_feature(boundary_centre, cu_FWA)
  cu_FWA_pick <- cu_FWA[cu_FWA_pick,]
  
  return(cu_FWA_pick)
}

#mat <- st_coordinates(cu_FWA)
#mat_loc <- median(mat[,3])
#mid

#----------------------rainfall plot-----------------------------------------

rainfall_plot <- function(data) {
    ggplot(data, aes(x = factor(STREAM_ORDER), y = CT_anad, fill = factor(STREAM_ORDER))) +
  # Add half-violin from {ggdist} package
  stat_halfeye(
    adjust = 0.5,
    justification = -0.2,
    .width = 0,
    point_colour = NA
  ) +
  geom_boxplot(
    width = 0.12,
    outlier.color = NA,
    alpha = 0.5
  ) +
  stat_dots(
    side = "left",
    justification = 0.1,
    binwidth = 0.25,
    size = 0.1
  ) +
  scale_fill_viridis(discrete = TRUE) +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 11)
  ) +
  ggtitle("Stream Orders within CU boundary") +
  xlab("")
}



#------------------------------------------------------------------------------
# Plot map
#------------------------------------------------------------------------------

map.stage <- function(
    cu_boundary.i,
    zoi.i,
    mig_paths.i,
    grid_polys = NA
){
  
  if(!is.na(zoi.i)){
    bounds <- cbind(st_bbox(zoi.i), st_bbox(mig_paths.i))
  bounds <- c(
    xmin = min(bounds[1, ]), 
    xmax = max(bounds[3,]), 
    ymin = min(bounds[2, ]), 
    ymax = max(bounds[4, ]))
  } else {
    bounds <- st_bbox(mig_paths.i)[c(1,3,2,4)]
  }
  
  par(bg = NA)
  
  plot(st_geometry(BC), border = NA, col = NA, axes = FALSE, las = 1, ylim = bounds[3:4], xlim = bounds[1:2], bty = "o")
  # plot(st_geometry(mig_paths.i), lwd = 3, col = cols[3], axes=TRUE, las = 1, ylim = bounds[3:4], xlim = bounds[1:2])
  # mtext(side = 3, line = 1, cus_to_keep$culabel[i])
  
  plot(grid_polys, add = TRUE, border = grey(0.8), col = NA)
  plot(BC, add = TRUE, col = NA, border = 1)
  
  
  if(!is.na(zoi.i)){
    plot(st_geometry(zoi.i), border = cols[1], col = paste0(cols[1], 50), lwd = 1.5, add = TRUE)
  }
  
  if(!is.na(cu_boundary.i)){
  plot(st_geometry(cu_boundary.i), border = 1, col = NA, lwd = 1, add = TRUE)
  }
  
  # coarser lakes and rivers
  plot(st_geometry(lakes0), border = 1, col = NA, add = TRUE)
  plot(st_geometry(rivers0), col = 1, add = TRUE)
  
  # legend("topleft", fill = c(paste0(cols[1], 50), cols[3], "white"), border = c(cols[1], cols[3], "#000000"), legend = c("Spawn ZOI", "Mig.", "CU boundary"), bg = "white")
  
}