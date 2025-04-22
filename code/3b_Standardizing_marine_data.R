#working with NetCDFs
library(ncdf4)
library(stars)
library(ncmeta)
library(abind)
library(concaveman)
#working with rasters 
library(raster)
library(sp)
library("terra")
library("spdep")
library("lubridate")
library("gtable")
library("gridExtra")
library("grid")

#working with shapefiles
library(sf)

#other 
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(viridis)
library("spgwr")
library("spatstat")
library("tmap")
library("gstat")
library("maps")
library("devtools")
#install_github("pbs-assess/pacea")
library(pacea)

sf_use_s2(FALSE)  # remove spherical geometry (s2) for sf operations

# Simple feature objects with point SST and SSC and SSPH data for each model should be in the environment from marine import script.

#------Import bounding and clipping polygons ----------
# Load polygons shp files that will be used to set the extent of the interpolation or clip the final vector grid. 
EEZ<-read_sf(file.path(  spatial_dat, "BC_EEZ", "BC_EEZ.shp"))                              # Full BC EEZ 
SSCbox<-read_sf(file.path(climate_dat, "SalishSeaCast_MonthlyData", "SSC_boundingbox.shp")) # Box of full SSC extent
BCCM_mask<-read_sf(file.path(climate_dat, "BCCM", "BCCM_mask.shp"))                         # EEZ excluding north west corner 
SSC_mask<-read_sf(file.path(climate_dat, "SalishSeaCast_MonthlyData", "SSC_mask.shp"))      # surrounds the SSC data but excludes inlets and USA 
NEP_mask<-read_sf(file.path(climate_dat, "NEP36_MonthlyData", "NEP_mask.shp"))              # Clips out uncertain results including those on East coast of Haida Gwaii
CI_mask<-read_sf(file.path(  spatial_dat, "CumulativeImpacts_shp", "CI_mask.shp"))         # EEZ polygon made from extent of original CI layer

#Transform polygon sf objects to 3005 to match climate data 
EEZ<-st_transform(EEZ,crs = "EPSG:3005" )
SSCbox<-st_transform(SSCbox,crs = "EPSG:3005" )
NEP_mask<-st_transform(NEP_mask,crs = "EPSG:3005" ) 
SSC_mask<-st_transform(SSC_mask,crs = "EPSG:3005" )
BCCM_mask<-st_transform(BCCM_mask,crs = "EPSG:3005" )
CI_mask<-st_transform(CI_mask,crs = "EPSG:3005" )

# ---------Installing function point2rast from PACEA ------------------
#- code copied from https://github.com/pbs-assess/pacea/blob/main/R/interpolate.R
point2rast <- function(data, spatobj, loc = c("x", "y"), cellsize, nnmax = 4, 
                       as = c("SpatRast","SpatVect")) {
  
  requireNamespace("methods", quietly = TRUE)
  requireNamespace("terra", quietly = TRUE)
  requireNamespace("gstat", quietly = TRUE)
  requireNamespace("sf", quietly = TRUE)
  requireNamespace("stats", quietly = TRUE)
  
  stopifnot("must provide cellsize value" = exists("cellsize"))
  stopifnot("must specify valid value for 'as'" = as %in% c("SpatRast", "SpatVect"))
  
  if(!any(sapply(c("sf", "Spatial"), function(cl) methods::is(data, cl)))) {
    
    data <- as.data.frame(data)
    
    if(!length(dim(loc))){
      
      stopifnot("loc vector must of be of length==2 " = length(loc)==2)
      stopifnot("loc names not found in data" = loc %in% names(data))
      
      coords <- setNames(as.data.frame(data[, loc]), c("x", "y"))
      
      tdat <- as.data.frame(data[, -which(colnames(data) %in% loc), drop = FALSE])
      
    } else { 
      
      stopifnot("loc data must be a matrix or dataframe of two columns" = 
                  length(dim(loc)) == 2 & dim(loc)[2] == 2)
      stopifnot("loc data is not of equal length to data" = nrow(data) == nrow(loc))
      
      coords <- setNames(as.data.frame(loc), c("x", "y"))
      
      tdat <- as.data.frame(data[, !colnames(data) %in% colnames(loc), drop = FALSE])
      
    }
  }
  
  if(methods::is(data, "Spatial")) {
    coords <- setNames(as.data.frame(data)[,c("coords.x1", "coords.x2")], c("x", "y"))
    tdat <- as.data.frame(data)[, names(data), drop = FALSE]    
  }
  
  if(methods::is(data, "sf")) {
    coords <- setNames(as.data.frame(matrix(unlist(data$geometry), ncol=2, byrow = TRUE)), c("x", "y"))
    tdat <- as.data.frame(data)[, -which(names(data) == "geometry"), drop = FALSE]
  }
  
  tbb <- terra::ext(spatobj)
  if(!any(coords$x >= tbb$xmin & coords$x <= tbb$xmax & 
          coords$y >= tbb$ymin & coords$y <= tbb$ymax)) {
    warning("'loc' coordinates within spatobj extent = 0; check crs or extent of spatobj")
  }
  
  terror <- try(terra::crs(spatobj), silent = TRUE)
  if("try-error" %in% class(terror)) {
    r <- terra::rast(terra::ext(spatobj), res = c(cellsize))
  } else {
    r <- terra::rast(terra::ext(spatobj), res = c(cellsize), crs = terra::crs(spatobj))
  }
  
  nn.pred <- apply(tdat, 2, FUN = nnfit, r=r, loc=loc, coords=coords, nnmax=nnmax)
  xyz <- cbind(as.data.frame(suppressWarnings(terra::crds(r))), nn.pred)
  
  if(as[1]=="SpatRast"){
    spat <- terra::rast(xyz, type="xyz", crs = terra::crs(r))
  } 
  if(as[1]=="SpatVect"){
    spat <- terra::vect(xyz, geom = c("x", "y"), crs = terra::crs(r))
  } 
  
  return(spat)
}

#' nearest neighbour fit function
#' @noRd
nnfit <- function(x, r, loc, coords, nnmax) {
  
  requireNamespace("stats", quietly = TRUE)
  
  xdat <- stats::na.omit(data.frame(xvar = as.vector(x), coords))
  
  f <- paste0("xvar", " ~ 1")
  lf <- paste0("~", paste(loc, collapse = "+"))
  
  gs <- gstat::gstat(formula = xvar~1, locations = ~x+y, data = xdat, nmax = nnmax, set=list(idp = 0))
  nn <- terra::interpolate(r, gs, debug.level=0)
  return(as.vector(nn$var1.pred))
}

# ---------INTERPOLATE-----------------------------------
# Interpolate climate data using the PACEA nearest neighbour interpolation function, point2rast

# Establish parameters of point2rast function
llnames <- c("x", "y")
nmax <- 4 #I think this is how many points the nearest neighbour function considers, 4 is small, meaning its a local interpolation 

# Interpolate BCCM
BCCM_SST_interpolation <- point2rast(data = BCCM_SST,
                                 spatobj = EEZ,
                                 loc = llnames,
                                 cellsize = 3000,
                                 nnmax = nmax,
                                 as = "SpatRast")
#plot(BCCM__SST_interpolation)   
BCCM_SSS_interpolation <- point2rast(data = BCCM_SSS,
                                     spatobj = EEZ,
                                     loc = llnames,
                                     cellsize = 3000,
                                     nnmax = nmax,
                                     as = "SpatRast")
#plot(BCCM__SSS_interpolation)   
BCCM_SSPH_interpolation <- point2rast(data = BCCM_SSPH,
                                     spatobj = EEZ,
                                     loc = llnames,
                                     cellsize = 3000,
                                     nnmax = nmax,
                                     as = "SpatRast")
#plot(BCCM__SSPH_interpolation)   

#Interpolate SSC
SSC_SST_interpolation <- point2rast(data = SSC_SST,
                                spatobj = SSCbox,
                                loc = llnames,
                                cellsize = 500,
                                nnmax = nmax,
                                as = "SpatRast")
#plot(SSC__SST_interpolation) 
SSC_SSS_interpolation <- point2rast(data = SSC_SSS,
                                spatobj = SSCbox,
                                loc = llnames,
                                cellsize = 500,
                                nnmax = nmax,
                                as = "SpatRast")
#plot(SSC_interpolation) 

#Interpolate NEP 36
NEP_SSS_interpolation <- point2rast(data = NEP_SSS,
                                   spatobj = EEZ,
                                   loc = llnames,
                                   cellsize = 3000,
                                   nnmax = nmax,
                                   as = "SpatRast")
#plot(NEP_SSS_interpolation)
NEP_SST_interpolation <- point2rast(data = NEP_SST,
                                    spatobj = EEZ,
                                    loc = llnames,
                                    cellsize = 3000,
                                    nnmax = nmax,
                                    as = "SpatRast")
#plot(NEP_SST_interpolation)
NEP_SSPH_interpolation <- point2rast(data = NEP_SSPH,
                                    spatobj = EEZ,
                                    loc = llnames,
                                    cellsize = 3000,
                                    nnmax = nmax,
                                    as = "SpatRast")
#plot(NEP_SSPH_interpolation)

#Cumulative impacts 
CI_interpolation<- point2rast(data = CI_points,
                              spatobj = CI_mask,
                              loc = llnames,
                              cellsize = 1000,
                              nnmax = nmax,
                              as = "SpatRast")
#plot(CI_interpolation)

#---------RESAMPLE-----------------
# Resample interpolated rasters to the same grid. Using BCCM as base as it has the largest resolution
# Use nearest neighbor function because we want the local values to have the largest impact 
# BCCM doesn't need to be resampled as we are converting the other grids to its resolution
# if you get an "Error: external pointer is not valid" try clearing the environment and rerun the code starting at the marine import script
NEP_SST_resampled <-resample(NEP_SST_interpolation,  BCCM_SST_interpolation, method = "near")
NEP_SSS_resampled <-resample(NEP_SSS_interpolation,  BCCM_SST_interpolation, method = "near")
NEP_SSPH_resampled<-resample(NEP_SSPH_interpolation, BCCM_SST_interpolation, method = "near")
SSC_SST_resampled <-resample(SSC_SST_interpolation,  BCCM_SST_interpolation, method = "near")
SSC_SSS_resampled <-resample(SSC_SSS_interpolation,  BCCM_SST_interpolation, method = "near")
CI_resampled      <-resample(CI_interpolation,       BCCM_SST_interpolation, method="near")

#--------CROP-----------
#Crop out grid cells with polygon masks 
BCCM_SST_cropped <-BCCM_SST_interpolation %>%
  mask(BCCM_mask) %>% # if you end code here it is a spatraster
  stars::st_as_stars() %>%  
  sf::st_as_sf() 
BCCM_SSS_cropped <-BCCM_SSS_interpolation %>%
  mask(BCCM_mask) %>% # if you end code here it is a spatraster
  stars::st_as_stars() %>%  
  sf::st_as_sf() 
BCCM_SSPH_cropped <-BCCM_SSPH_interpolation %>%
  mask(BCCM_mask) %>% # if you end code here it is a spatraster
  stars::st_as_stars() %>%  
  sf::st_as_sf() 

NEP_SST_cropped <- NEP_SST_resampled %>%
  mask(NEP_mask) %>%  
  stars::st_as_stars() %>%  
  sf::st_as_sf() 
NEP_SSS_cropped <- NEP_SSS_resampled %>%
  mask(NEP_mask) %>%
  stars::st_as_stars() %>%  
  sf::st_as_sf()
NEP_SSPH_cropped <- NEP_SSPH_resampled %>%
  mask(NEP_mask) %>%  
  stars::st_as_stars() %>%  
  sf::st_as_sf() 

SSC_SST_cropped <- SSC_SST_resampled %>%
  mask(SSC_mask) %>%
  stars::st_as_stars() %>%  
  sf::st_as_sf() 
SSC_SSS_cropped <- SSC_SSS_resampled %>%
  mask(SSC_mask) %>%
  stars::st_as_stars() %>%  
  sf::st_as_sf()

CI_cropped <- CI_resampled%>%
  mask(CI_mask) %>%
  stars::st_as_stars() %>%  
  sf::st_as_sf() 

#----------EXPORT as shp----------------
#Export cropped layers as vector grid files 
sf::st_write(BCCM_SST_cropped, file.path(climate_dat, "Standardized_Marine_data/BCCM_SST_cropped.shp"),   driver = "ESRI Shapefile" )
sf::st_write(BCCM_SSS_cropped, file.path(climate_dat, "Standardized_Marine_data/BCCM_SSS_cropped.shp"  ), driver = "ESRI Shapefile")
sf::st_write(BCCM_SSPH_cropped,file.path(climate_dat, "Standardized_Marine_data/BCCM_SSPH_cropped.shp" ), driver = "ESRI Shapefile")
sf::st_write(NEP_SST_cropped,  file.path(climate_dat, "Standardized_Marine_data/NEP_SST_cropped.shp"),    driver = "ESRI Shapefile")
sf::st_write(NEP_SSS_cropped,  file.path(climate_dat, "Standardized_Marine_data/NEP_SSS_cropped.shp" ),   driver = "ESRI Shapefile")
sf::st_write(NEP_SSPH_cropped, file.path(climate_dat, "Standardized_Marine_data/NEP_SSPH_cropped.shp"),   driver = "ESRI Shapefile")
sf::st_write(SSC_SST_cropped,  file.path(climate_dat, "Standardized_Marine_data/SSC_SST_cropped.shp"),    driver = "ESRI Shapefile")
sf::st_write(SSC_SSS_cropped,  file.path(climate_dat, "Standardized_Marine_data/SSC_SSS_cropped.shp"),    driver = "ESRI Shapefile")
sf::st_write(CI_cropped,       file.path(climate_dat, "Standardized_Marine_data/CI_cropped.shp"),         driver = "ESRI Shapefile")

