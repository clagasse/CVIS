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
  if(sum(!is.na(x)) == 0) return(NA)
  y <- Hmisc::wtd.quantile(x, weights = length_metre, probs = prob, na.rm = TRUE)
  unname(y)
}


###############################################################################
# Spatial analysis utility functions
###############################################################################

# for two spatial objects, subset the data in sp_x that intersects with sp_y
subset_intersections <- function(sp_x, sp_y) {
  intersect <- st_intersects(sp_x, sp_y, sparse = FALSE)
  sp_x_sub <- sp_x[apply(intersect, 1, sum) > 0,]
  return(sp_x_sub)
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
## ----  FWA functions-----
###################################################################

downstream_path <- function(stream_pick, stream_network, code_type = "FWA") {
  
  FWA_code <- stream_pick$localcode
  st <- str_length(FWA_code)
  if(code_type == "FWA") {
    FWA_code <- stream_pick$FWA_WATERSHED_CODE
    st <- str_locate(FWA_code, "000000")[1] - 2   #get stream code last position
  }
  
  st_level <- (st + 4) / 7
  
  for (n in 1:st_level) {
    
    #get FWA code for current FWA level of iteration
    c_cut <- (n - 1) * 7
    s_pick <- stringr::str_sub(FWA_code, 1, st - c_cut)
    
    if (code_type == "FWA") s_pick <- stringr::str_c(s_pick, "-000000")
    
    # candidates at this level
    if (code_type == "FWA") {
      ind <- stringr::str_starts(stream_network$FWA_WATERSHED_CODE, s_pick)
    } else {
      ind <- (stream_network$localcode == s_pick)
    }
    
    candidates <- stream_network[ind, ] %>%
      dplyr::filter(
        stream_order >= max(stream_pick$stream_order, na.rm = TRUE),
        stream_magnitude >= max(stream_pick$stream_magnitude, na.rm = TRUE)
      )
    
    if (nrow(candidates) == 0) next
    
    #get streams with matching FWA code that intersect with migration reaches (confluences will include tributaries)
    int_mat <- sf::st_intersects(candidates, stream_pick, sparse = FALSE)
    FWA_int <- candidates[which(rowSums(int_mat) > 0), ]
    
    if (nrow(FWA_int) == 0) next
    
    FWA_int <- FWA_int %>% dplyr::filter(!is.na(downstream_route_measure))
    
    #  pick ONE "downstream continuation" route, then only take downstream on that route
    # Heuristic: prefer the largest river (order/magnitude), then the most-downstream DRM among those
    next_reach <- FWA_int %>%
      dplyr::arrange(dplyr::desc(stream_order),
                     dplyr::desc(stream_magnitude),
                     downstream_route_measure) %>%
      dplyr::slice(1)
    
    #get downstream_route_measure from lowest intersecting reach
    next_key <- next_reach$blue_line_key
    dd <- next_reach$downstream_route_measure
    
    low_stream <- candidates %>%
      dplyr::filter(
        blue_line_key == next_key,
        downstream_route_measure <= dd   # see note below on direction
      )
    
    #take all streams with downstream_route_measure distance below intersect
    stream_pick <- dplyr::bind_rows(stream_pick, low_stream) %>%
      dplyr::distinct()  # avoid duplicates
  }
  
  stream_pick
}


#measure distance to ocean entry for each stream segment in a migration paths object
measure_downstream <- function(stream_pick, stream_network, code_type = "FWA") {
  
  #if value already exists, return it and exit function
  if(!is.na(stream_pick$downstream_distance)) return(stream_pick$downstream_distance)

  FWA_code <- stream_pick$wscode
  st <- str_length(FWA_code)
  
  if(code_type == "FWA") {
    FWA_code <- stream_pick$FWA_WATERSHED_CODE
    st <- str_locate(FWA_code, "000000")[1] - 2   #get stream code last position
  }
  
  st_level <- (st+4)/7
  
  if(code_type == "FWA") s_pick <- str_sub(FWA_code,1,st)
  
  for(k in 1:st_level){

    c_cut <- (k-1) * 7
    code <- paste0(str_sub(FWA_code, 1, st - c_cut), "-000000")
    
    #select streams with same watershed code 
    candidates <- filter(stream_network, str_starts(FWA_WATERSHED_CODE, code)) 
    
    if(k == 1) {
      # filter streams with equal or lower distance to next downstream section
      candidates <- filter(candidates, downstream_route_measure <= stream_pick$downstream_route_measure)
      temp_path <- candidates  # add to path object
    } 
    if(k > 1) {
      #get streams with lower FWA code that intersect with migration reaches
      FWA_int <- st_intersects(candidates, temp_path, sparse = FALSE)
      if(length(FWA_int) == 0) next
      FWA_int <- candidates[which(apply(FWA_int, 1, sum) > 0),]

      #filter streams with lower FWA code to select those that are downstream of the intersection point
      candidates <- filter(candidates, downstream_route_measure <= FWA_int$downstream_route_measure)
      temp_path <- bind_rows(temp_path, candidates)  #combine into path object
    }
    
    # filter out any streams with higher order or magnitude as an extra check
    temp_path <- temp_path %>%
      filter(stream_order >= max(stream_pick$stream_order, na.rm = T), stream_magnitude >= max(stream_pick$magnitude, na.rm = T))
    
  }

  #get downstream_route_measure range from lowest intersecting reach
  if(code_type == "FWA") {
    dd <- sum(candidates$length_metre, na.rm = T) - (stream_pick$length_metre / 2)
  }
  
  return(dd)
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


#simple calculation for work to reach migration segment

calculate_work <- function(elev, dist) {
  work <- 0.0001 * elev * dist
}

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