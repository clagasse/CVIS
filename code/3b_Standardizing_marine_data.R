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

#Set Working Directory
WD<-setwd("C:/Users/houtmann/Documents/CCVA/GIS Freshwater/GIS Marine/StandarizedGridData") 

#Loading data downloaded from Pacea Github https://github.com/pbs-assess/pacea/tree/main/data
  load("C:/Users/houtmann/Documents/CCVA/GIS Freshwater/GIS Marine/StandarizedGridData/bccm_eez_poly.rda")
  load("C:/Users/houtmann/Documents/CCVA/GIS Freshwater/GIS Marine/StandarizedGridData/bc_coast.rda")
  load("C:/Users/houtmann/Documents/CCVA/GIS Freshwater/GIS Marine/StandarizedGridData/bc_eez.rda")
  sf::st_write(bccm_eez_poly, "C:/Users/houtmann/Documents/CCVA/GIS Freshwater/GIS Marine/StandarizedGridData/bccm_eez_poly_pacea.shp")
  sf::st_write(bc_coast, "C:/Users/houtmann/Documents/CCVA/GIS Freshwater/GIS Marine/StandarizedGridData/bc_coast_pacea.shp")
  sf::st_write(bc_eez, "C:/Users/houtmann/Documents/CCVA/GIS Freshwater/GIS Marine/StandarizedGridData/bc_eez_pacea.shp")
  
# Loading SSC and NEP36 models as vector point data 
  ## SSC
      SSC<-read_sf(dsn=WD, layer="SSC_MonthlyDataCombined")
      #Box made in 3005 bounding SSC (all of the original data points) that will be used to define extent of interpolation
      SSCbox<-read_sf(dsn=WD, layer= "SSC_boundingbox") 
      #SSC mask is a polygon encompassing SSC points that are in canada and not in inlets. Inlets should be excluded due to lower certainty in their results   
      SSC_mask <- read_sf(dsn=WD, layer= "SSC_mask") 
        
  ## NEP 36
    #This NEP36 version excludes the NEP36 masked out values but includes the east coast of HG
        NEP_temp<-read_sf(dsn=WD, layer= "Temp_FutureAndHistorical")
        NEP_sal<-read_sf(dsn=WD, layer= "Salinity_FutureAndHistorical")
    # NEP 36 mask which only includes the area
       NEP_mask<-read_sf(dsn=WD, layer= "NEP36_PolyMaskForCertainResults")
       NEP_mask2<-read_sf(dsn= WD, layer="MAZ_toClipNEP36andSSC")
  ## BCCM Mask - EEZ polygon excluding the northwest corner where there is no BCCM data 
        BCCM_mask<-read_sf(dsn=WD, layer= "BCCM_Mask") 
    #BC EEZ polygon to define BCCM and NEP36 interpolation extent  
        EEZ<-read_sf(dsn=WD, layer= "BC_EEZ")     

    ##Cumulative impacts model
        CI<-read_sf(dsn=WD, layer="CI_MAZJoined")
      #Another EEZ polygon to define the CI boundaries. This polygon was derived from the original CI layer (dissolved all CI polygons.)
        CI_mask<-read_sf(dsn=WD, layer= "CI_mask") 
        
#Transform sf files to 3005
SSC<-st_transform(SSC, crs = "EPSG:3005")
NEP_temp<-st_transform(NEP_temp, crs = "EPSG:3005")
NEP_sal<-st_transform(NEP_sal, crs = "EPSG:3005")
EEZ<-st_transform(EEZ,crs = "EPSG:3005" )
SSCbox<-st_transform(SSCbox,crs = "EPSG:3005" )
NEP_mask<-st_transform(NEP_mask,crs = "EPSG:3005" ) 
NEP_mask2<-st_transform(NEP_mask,crs = "EPSG:3005" )
SSC_mask<-st_transform(SSC_mask,crs = "EPSG:3005" )
BCCM_mask<-st_transform(BCCM_mask,crs = "EPSG:3005" )
CI<-st_transform(CI,crs = "EPSG:3005" )
CI_mask<-st_transform(CI_mask,crs = "EPSG:3005" )

# calculate centroids of CI polygon grid
CI_points<- st_centroid(CI)

#plotting data, choosing two random variables in each shp
View(SSC)
plot(SSC['H03votempe'])
plot(NEP_sal["09_S_F_sal"]) 

#loading BCCM data
BCCM_85_SST<- read_ncdf(file.path(WD, "bcc42_bioNew_can85_2046to2065_monSST.nc"), proxy = FALSE, var = c("lat_rho","lon_rho", "temp", "months"), make_time = TRUE)
BCCM_45_SST<- read_ncdf(file.path(WD, "bcc42_bioNew_can45_2046to2065_monSST.nc"), proxy = FALSE, var = c("lat_rho","lon_rho", "temp", "months"), make_time = TRUE)
BCCM_Hist_SST<- read_ncdf(file.path(WD, "bcc42_bioNew_his_1986to2005_monSST.nc"), proxy = FALSE, var = c("lat_rho","lon_rho", "temp", "months"),  make_time = TRUE)
BCCM_85_SSS<- read_ncdf(file.path(WD, "bcc42_bioNew_can85_2046to2065_monSSS.nc"), proxy = FALSE, var = c("lat_rho","lon_rho", "salt", "months"), make_time = TRUE)
BCCM_45_SSS<- read_ncdf(file.path(WD, "bcc42_bioNew_can45_2046to2065_monSSS.nc"), proxy = FALSE, var = c("lat_rho","lon_rho", "salt", "months"),  make_time = TRUE)
BCCM_Hist_SSS<- read_ncdf(file.path(WD, "bcc42_bioNew_his_1986to2005_monSSS.nc"), proxy = FALSE, var = c("lat_rho","lon_rho", "salt", "months"),  make_time = TRUE)
print(BCCM_45_SST) # if the offset is 0.5 in both x and y, it means the points are centroids of cells 
                    # if the x and y delta values are positive it means the 1, 1 cell is in the bottom left, and both variables increase with the axes 

#------Build a BCCM data frame------- 
  #First, separate each variable 
BCCM_lon <- as.vector(BCCM_45_SST$lon_rho)
BCCM_lat <- as.vector(BCCM_45_SST$lat_rho)
BCCM_mask<- as.vector(BCCM_Hist_SST$mask_rho)

T85_01    <- as.vector(BCCM_85_SST$temp[,,1])
T85_02    <- as.vector(BCCM_85_SST$temp[,,2])
T85_03    <- as.vector(BCCM_85_SST$temp[,,3])
T85_04    <- as.vector(BCCM_85_SST$temp[,,4])
T85_05    <- as.vector(BCCM_85_SST$temp[,,5])
T85_06    <- as.vector(BCCM_85_SST$temp[,,6])
T85_07    <- as.vector(BCCM_85_SST$temp[,,7])
T85_08    <- as.vector(BCCM_85_SST$temp[,,8])
T85_09    <- as.vector(BCCM_85_SST$temp[,,9])
T85_10    <- as.vector(BCCM_85_SST$temp[,,10])
T85_11    <- as.vector(BCCM_85_SST$temp[,,11])
T85_12    <- as.vector(BCCM_85_SST$temp[,,12])

T45_01    <- as.vector(BCCM_45_SST$temp[,,1])
T45_02    <- as.vector(BCCM_45_SST$temp[,,2])
T45_03    <- as.vector(BCCM_45_SST$temp[,,3])
T45_04    <- as.vector(BCCM_45_SST$temp[,,4])
T45_05    <- as.vector(BCCM_45_SST$temp[,,5])
T45_06    <- as.vector(BCCM_45_SST$temp[,,6])
T45_07    <- as.vector(BCCM_45_SST$temp[,,7])
T45_08    <- as.vector(BCCM_45_SST$temp[,,8])
T45_09    <- as.vector(BCCM_45_SST$temp[,,9])
T45_10    <- as.vector(BCCM_45_SST$temp[,,10])
T45_11    <- as.vector(BCCM_45_SST$temp[,,11])
T45_12    <- as.vector(BCCM_45_SST$temp[,,12])

TH01    <- as.vector(BCCM_Hist_SST$temp[,,1])
TH02    <- as.vector(BCCM_Hist_SST$temp[,,2])
TH03    <- as.vector(BCCM_Hist_SST$temp[,,3])
TH04    <- as.vector(BCCM_Hist_SST$temp[,,4])
TH05    <- as.vector(BCCM_Hist_SST$temp[,,5])
TH06    <- as.vector(BCCM_Hist_SST$temp[,,6])
TH07    <- as.vector(BCCM_Hist_SST$temp[,,7])
TH08    <- as.vector(BCCM_Hist_SST$temp[,,8])
TH09    <- as.vector(BCCM_Hist_SST$temp[,,9])
TH10    <- as.vector(BCCM_Hist_SST$temp[,,10])
TH11    <- as.vector(BCCM_Hist_SST$temp[,,11])
TH12    <- as.vector(BCCM_Hist_SST$temp[,,12])

S85_01    <- as.vector(BCCM_85_SSS$salt[,,1])
S85_02    <- as.vector(BCCM_85_SSS$salt[,,2])
S85_03    <- as.vector(BCCM_85_SSS$salt[,,3])
S85_04    <- as.vector(BCCM_85_SSS$salt[,,4])
S85_05    <- as.vector(BCCM_85_SSS$salt[,,5])
S85_06    <- as.vector(BCCM_85_SSS$salt[,,6])
S85_07    <- as.vector(BCCM_85_SSS$salt[,,7])
S85_08    <- as.vector(BCCM_85_SSS$salt[,,8])
S85_09    <- as.vector(BCCM_85_SSS$salt[,,9])
S85_10    <- as.vector(BCCM_85_SSS$salt[,,10])
S85_11    <- as.vector(BCCM_85_SSS$salt[,,11])
S85_12    <- as.vector(BCCM_85_SSS$salt[,,12])

S45_01    <- as.vector(BCCM_45_SSS$salt[,,1])
S45_02    <- as.vector(BCCM_45_SSS$salt[,,2])
S45_03    <- as.vector(BCCM_45_SSS$salt[,,3])
S45_04    <- as.vector(BCCM_45_SSS$salt[,,4])
S45_05    <- as.vector(BCCM_45_SSS$salt[,,5])
S45_06    <- as.vector(BCCM_45_SSS$salt[,,6])
S45_07    <- as.vector(BCCM_45_SSS$salt[,,7])
S45_08    <- as.vector(BCCM_45_SSS$salt[,,8])
S45_09    <- as.vector(BCCM_45_SSS$salt[,,9])
S45_10    <- as.vector(BCCM_45_SSS$salt[,,10])
S45_11    <- as.vector(BCCM_45_SSS$salt[,,11])
S45_12    <- as.vector(BCCM_45_SSS$salt[,,12])

SH01    <- as.vector(BCCM_Hist_SSS$salt[,,1])
SH02    <- as.vector(BCCM_Hist_SSS$salt[,,2])
SH03    <- as.vector(BCCM_Hist_SSS$salt[,,3])
SH04    <- as.vector(BCCM_Hist_SSS$salt[,,4])
SH05    <- as.vector(BCCM_Hist_SSS$salt[,,5])
SH06    <- as.vector(BCCM_Hist_SSS$salt[,,6])
SH07    <- as.vector(BCCM_Hist_SSS$salt[,,7])
SH08    <- as.vector(BCCM_Hist_SSS$salt[,,8])
SH09    <- as.vector(BCCM_Hist_SSS$salt[,,9])
SH10    <- as.vector(BCCM_Hist_SSS$salt[,,10])
SH11    <- as.vector(BCCM_Hist_SSS$salt[,,11])
SH12    <- as.vector(BCCM_Hist_SSS$salt[,,12])

  # secondly, Recombine variables into a data frame and reproject into 3005 (BC albers)
  BCCM<- data.frame(x = BCCM_lon,y = BCCM_lat,
                      T45_01, T45_02, T45_03, T45_04, T45_05, T45_06, T45_07,T45_08, T45_09, T45_10, T45_11, T45_12,
                      T85_01, T85_02, T85_03, T85_04, T85_05, T85_06, T85_07,T85_08, T85_09, T85_10, T85_11, T85_12,
                      TH01, TH02, TH03, TH04, TH05, TH06, TH07, TH08, TH09, TH10, TH11, TH12,
                      S45_01, S45_02, S45_03, S45_04, S45_05, S45_06, S45_07, S45_08, S45_09, S45_10, S45_11, S45_12,
                      S85_01, S85_02, S85_03, S85_04, S85_05, S85_06, S85_07, S85_08, S85_09, S85_10, S85_11, S85_12,
                      SH01, SH02, SH03, SH04, SH05, SH06, SH07, SH08, SH09, SH10, SH11, SH12) %>%
    st_as_sf(coords = c("x", "y"),
           crs = "EPSG:4326") %>%
    st_transform(crs = "EPSG:3005")
  #plot(BCCM)

  # 3) remove the extra vectors 
  rm(T45_01, T45_02, T45_03, T45_04, T45_05, T45_06, T45_07,T45_08, T45_09, T45_10, T45_11, T45_12,
     T85_01, T85_02, T85_03, T85_04, T85_05, T85_06, T85_07,T85_08, T85_09, T85_10, T85_11, T85_12,
     TH01, TH02, TH03, TH04, TH05, TH06, TH07, TH08, TH09, TH10, TH11, TH12,
     S45_01, S45_02, S45_03, S45_04, S45_05, S45_06, S45_07, S45_08, S45_09, S45_10, S45_11, S45_12,
     S85_01, S85_02, S85_03, S85_04, S85_05, S85_06, S85_07, S85_08, S85_09, S85_10, S85_11, S85_12,
     SH01, SH02, SH03, SH04, SH05, SH06, SH07, SH08, SH09, SH10, SH11, SH12)

  # 4) Export the points as a shape file
 sf::st_write(BCCM, "C:/Users/houtmann/Documents/CCVA/GIS Freshwater/GIS Marine/StandarizedGridData/BCCMmonthly_SST_SSS.shp")

#---------Installing function point2rast from PACEA ------------------
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
#using the PACEA nearest neighbour interpolation function, point2rast
       
llnames <- c("x", "y")
nmax <- 4 #I think this is how many points the nearest neighbour function considers, 4 is small, meaning its a local interpolation 
##BCCM
BCCM_interpolation <- point2rast(data = BCCM,
                           spatobj = EEZ,
                           loc = llnames,
                           cellsize = 3000,
                           nnmax = nmax,
                           as = "SpatRast")

#plot(BCCM_interpolation)   

SSC_interpolation <- point2rast(data = SSC,
                                 spatobj = SSCbox,
                                 loc = llnames,
                                 cellsize = 500,
                                 nnmax = nmax,
                                 as = "SpatRast")
#plot(SSC_interpolation) 

NEPsal_interpolation <- point2rast(data = NEP_sal,
                                spatobj = EEZ,
                                loc = llnames,
                                cellsize = 3000,
                                nnmax = nmax,
                                as = "SpatRast")
#plot(NEPsal_interpolation)

NEPtemp_interpolation <- point2rast(data = NEP_temp,
                                   spatobj = EEZ,
                                   loc = llnames,
                                   cellsize = 3000,
                                   nnmax = nmax,
                                   as = "SpatRast")
#plot(NEPtemp_interpolation)

CI_interpolation<- point2rast(data = CI_points,
                              spatobj = CI_mask,
                              loc = llnames,
                              cellsize = 1000,
                              nnmax = nmax,
                              as = "SpatRast")
#plot(CI_interpolation)

#plot(BCCM_cropped)
#terra::writeRaster(BCCM_interpolation,"C:/Users/houtmann/Documents/CCVA/GIS Freshwater/GIS Marine/StandarizedGridData/BCCM_Cropped.tif")
#sf::st_write(BCCM_cropped, "C:/Users/houtmann/Documents/CCVA/GIS Freshwater/GIS Marine/StandarizedGridData/BCCM_Cropped.shp")

#---------RESAMPLE-----------------
# Resample interpolated rasters to the same grid. Using BCCM as base as it has the largest resolution
# use nearest neighbor function because we want the local values to have the largest impact 
# BCCM doesn't need to be resampled as we are converting the other grids to its resolution
NEPsal_resampled<-resample(NEPsal_interpolation, BCCM_interpolation,  method = "near")
NEPtemp_resampled<-resample(NEPtemp_interpolation, BCCM_interpolation,  method = "near")
SSC_resampled<-resample(SSC_interpolation, BCCM_interpolation,  method = "near")
CI_resampled<- resample(CI_interpolation, BCCM_interpolation, method="near")

#--------CROP-----------
#Crop out grid cells with polygon masks 

BCCM_cropped <-BCCM_interpolation %>%
  mask(bccm_eez_poly) %>% # if you end code here it is a spatraster
  stars::st_as_stars() %>%  
  sf::st_as_sf() 

NEPsal_cropped <- NEPsal_resampled %>%
  mask(NEP_mask) %>%  
  mask(NEP_mask2) %>%
  stars::st_as_stars() %>%  
  sf::st_as_sf() 

NEPtemp_cropped <- NEPtemp_resampled %>%
  mask(NEP_mask) %>%
  mask(NEP_mask2) %>%
  stars::st_as_stars() %>%  
  sf::st_as_sf() 

SSC_cropped <- SSC_resampled %>%
  mask(SSC_mask) %>%
  stars::st_as_stars() %>%  
  sf::st_as_sf() 

CI_cropped <- CI_resampled%>%
  mask(CI_mask) %>%
  stars::st_as_stars() %>%  
  sf::st_as_sf() 

#----------EXPORT as shp----------------
#Export cropped layers as vector grid files 

sf::st_write(BCCM_cropped, "C:/Users/houtmann/Documents/CCVA/GIS Freshwater/GIS Marine/StandarizedGridData/BCCM_Cropped2.shp")

sf::st_write(NEPsal_cropped, "C:/Users/houtmann/Documents/CCVA/GIS Freshwater/GIS Marine/StandarizedGridData/NEPsal_Cropped3.shp")
sf::st_write(NEPtemp_cropped, "C:/Users/houtmann/Documents/CCVA/GIS Freshwater/GIS Marine/StandarizedGridData/NEPtemp_Cropped3.shp")
sf::st_write(SSC_cropped, "C:/Users/houtmann/Documents/CCVA/GIS Freshwater/GIS Marine/StandarizedGridData/SSC_Cropped.shp")
sf::st_write(CI_cropped, "C:/Users/houtmann/Documents/CCVA/GIS Freshwater/GIS Marine/StandarizedGridData/CI_Cropped.shp")

