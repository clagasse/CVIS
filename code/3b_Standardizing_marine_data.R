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

source(file.path(code_root, "3_marine_utils.R"))

sf_use_s2(FALSE)  # remove spherical geometry (s2) for sf operations

# Load climate data that was the result of 3a marine data import data 
BCCM_SST<-read_sf( file.path(climate_dat, "BCCM", "BCCMmonthly_SST.shp"))
BCCM_SSS<-read_sf( file.path(climate_dat, "BCCM", "BCCMmonthly_SSS.shp"))
BCCM_SSPH<-read_sf( file.path(climate_dat, "BCCM", "BCCMmonthly_SSPH.shp"))
NEP_SST<- read_sf( file.path(climate_dat, "NEP36_MonthlyData","NEPmonthly_SST.shp"))
NEP_SSS<- read_sf( file.path(climate_dat, "NEP36_MonthlyData","NEPmonthly_SSS.shp"))
NEP_SSPH<- read_sf( file.path(climate_dat, "NEP36_MonthlyData","NEPmonthly_SSPH.shp"))
SSC_SST<- read_sf( file.path(climate_dat, "SalishSeaCast_MonthlyData","SSCmonthly_SST.shp"))
SSC_SSS<- read_sf( file.path(climate_dat, "SalishSeaCast_MonthlyData","SSCmonthly_SSS.shp"))
CI_points<-read_sf(file.path(spatial_dat, "CumulativeImpacts_shp", "CI_points.shp"))

#------Import bounding and masking/clipping polygons ----------
# Load polygons shp files that will be used to set the extent of the interpolation or clip the final vector grid. 
EEZ<-read_sf(file.path(  spatial_dat, "BC_EEZ", "BC_EEZ.shp"))                              # Full BC EEZ 
SSCbox<-read_sf(file.path(climate_dat, "SalishSeaCast_MonthlyData", "SSC_boundingbox.shp")) # Box of full SSC extent
BCCM_mask<-read_sf(file.path(climate_dat, "BCCM", "BCCM_mask2.shp"))                         # EEZ excluding north west corner 
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

#remove remnant attributes from masks 
#BCCM_mask= subset(BCCM_mask, select = -c(NAME_E, MAZ_Acrony, CI_AvgScor, Long_name))
BCCM_mask= subset(BCCM_mask, select = -c(id, area, perimeter))
SSC_mask= subset(SSC_mask, select = -c(NAME_E, MAZ_Acrony, CI_AvgScor, Long_name))
NEP_mask= subset(NEP_mask, select = -c(FID, disolve))


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

#------------ Remove zero value (land) From SSC -----
# there are some cells with zeros in all temp and salinity columns in the SSC files . 
# Mapping these showed these zero values fall on the land 
# This was not found for the other models  
SSC_SST_cropped<- filter(SSC_SST_cropped,SST_H_01 != 0 )
SSC_SSS_cropped<- filter(SSC_SSS_cropped,SSS_H_01 != 0 )

#------------- Join cropped files to MAZ---------
# load and transform MAZ file 
MAZ<-read_sf(file.path(spatial_dat, "MAZ", "MAZ_Final.shp")) %>% st_transform(,crs = "EPSG:3005" )
#Change MAZ acronym variable to a factor variable
MAZ$MAZ_Acrony<- as.factor(MAZ$MAZ_Acrony)

#join cropped files to MAZ
BCCM_SSS_cropped <- st_join(BCCM_SSS_cropped, left = FALSE, MAZ["MAZ_Acrony"]) # left= true means that points outside of MAZ polygons will be preserved 
BCCM_SST_cropped<- st_join(BCCM_SST_cropped, left = FALSE, MAZ["MAZ_Acrony"])
BCCM_SSPH_cropped<- st_join(BCCM_SSPH_cropped, left = FALSE, MAZ["MAZ_Acrony"])
NEP_SSS_cropped<- st_join(NEP_SSS_cropped, left = FALSE, MAZ["MAZ_Acrony"])
NEP_SST_cropped <- st_join(NEP_SST_cropped, left = FALSE, MAZ["MAZ_Acrony"])
NEP_SSPH_cropped <- st_join(NEP_SSPH_cropped, left = FALSE, MAZ["MAZ_Acrony"])
SSC_SSS_cropped<- st_join(SSC_SSS_cropped, left = FALSE, MAZ["MAZ_Acrony"])
SSC_SST_cropped <- st_join(SSC_SST_cropped, left = FALSE, MAZ["MAZ_Acrony"])
CI_points_cropped <- st_join(CI_cropped, left = FALSE, MAZ["MAZ_Acrony"])

rm(MAZ)
#----------EXPORT as shp----------------
#Export cropnped layers as vector grid files 
sf::st_write(BCCM_SST_cropped, file.path(climate_dat, "Standardized_Marine_data/BCCM_SST_cropped.shp"),   driver = "ESRI Shapefile" )
sf::st_write(BCCM_SSS_cropped, file.path(climate_dat, "Standardized_Marine_data/BCCM_SSS_cropped.shp"  ), driver = "ESRI Shapefile")
sf::st_write(BCCM_SSPH_cropped,file.path(climate_dat, "Standardized_Marine_data/BCCM_SSPH_cropped.shp" ), driver = "ESRI Shapefile")
sf::st_write(NEP_SST_cropped,  file.path(climate_dat, "Standardized_Marine_data/NEP_SST_cropped.shp"),    driver = "ESRI Shapefile")
sf::st_write(NEP_SSS_cropped,  file.path(climate_dat, "Standardized_Marine_data/NEP_SSS_cropped.shp" ),   driver = "ESRI Shapefile")
sf::st_write(NEP_SSPH_cropped, file.path(climate_dat, "Standardized_Marine_data/NEP_SSPH_cropped.shp"),   driver = "ESRI Shapefile")
sf::st_write(SSC_SST_cropped,  file.path(climate_dat, "Standardized_Marine_data/SSC_SST_cropped.shp"),    driver = "ESRI Shapefile")
sf::st_write(SSC_SSS_cropped,  file.path(climate_dat, "Standardized_Marine_data/SSC_SSS_cropped.shp"),    driver = "ESRI Shapefile")
sf::st_write(CI_cropped,       file.path(climate_dat, "Standardized_Marine_data/CI_cropped.shp"),         driver = "ESRI Shapefile")

# ----------- Remove Extra objects------
rm( BCCM_SST_interpolation, BCCM_SSS_interpolation, BCCM_SSPH_interpolation, NEP_SST_interpolation, 
    NEP_SSS_interpolation, NEP_SSS_interpolation, SSC_SST_interpolation, SSC_SSS_interpolation, 
   EEZ, CI_mask, BCCM_SST_cropped, BCCM_SSS_cropped, BCCM_SSPH_cropped, NEP_SST_cropped, NEP_SSS_cropped, 
   NEP_SSPH_cropped, SSC_SSS_cropped, SSC_SST_cropped, NEP_mask, BCCM_mask, SSCbox, SSC_mask, 
   llnames, nmax, nnfit, point2rast)
