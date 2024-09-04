###############################################################################
# Functions to load and summarize data relevant to exposure indicators

###############################################################################

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



#------------------------------------------------------------------------------
# Function to calculate mean difference between historic and projected
#------------------------------------------------------------------------------

calc_mean_diff <- function(
    indicator = "peakFlow"
) {
  
  indicator_hist <- indicator[,1]
  
  indicator_proj <- indicator[,2]
  
  hist_mean <- mean(indicator_hist, na.rm = T)
  proj_mean <- mean(indicator_proj, na.rm = T)
  
  diff <- proj_mean - hist_mean
  
  return(diff)
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