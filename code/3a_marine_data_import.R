
library(stars)
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

#climate_dat <- file.path("C:/Users/houtmann/OneDrive - DFO-MPO/0_data_climate")
#spatial_dat <- file.path("C:/Users/houtmann/OneDrive - DFO-MPO/0_data_spatial")

# This script opens up the original NetCDF files for the 3 climate models (BCCM, NEP36, and SSC),
# extracts surface data for the variables of interest and recombines the variables
# of interest into point data. For the Cumulative impacts model, the centroids of 
# the SHP file grid were also extracted. 
# The end product of this script is a series of simple feature objects for each 
# model/variable combination with spatial point data representing the centroids 
# of the original netCDF grids. These simple feature objects are converted to 
# CRS 3005 in this script and feed directly into the next script. If you want to 
# change the CRS, changing it in both this script and the next is important. 
# It is important to run the code in order and run all sections for each model. 

# Note: I tried to load mask data in and add it as an attribute to the resulting 
# simple feature objects, but I don't think this is necessary given that the results
# are masked in the next script after they are standardized to a grid.

############  LOAD NEP Temperature DATA################
# Load NEP 36 temperature data 
NEP_T_H<-nc_open(file.path(climate_dat, "NEP36_MonthlyData", "NEP36-CanOE_temp_historical_1986-2005_monthly.nc"))
NEP_T_H<-nc_open(file.path(climate_dat, "NEP36_MonthlyData", "NEP36-CanOE_temp_historical_1986-2005_monthly.nc"))
NEP_T_45<-nc_open(file.path(climate_dat, "NEP36_MonthlyData", "NEP36-CanOE_temp_RCP45_2046-2065_monthly.nc"))
NEP_T_85<-nc_open(file.path(climate_dat, "NEP36_MonthlyData", "NEP36-CanOE_temp_RCP85_2046-2065_monthly.nc"))
NEP_mask<-nc_open(file.path(climate_dat, "NEP36_MonthlyData", "NEP36-CanOE-MASK.nc"))
#print(NEP_T_H) #use this to figure out names of variables and dimensions 

#Read in the temperature variable for each scenario
TH<-ncvar_get(NEP_T_H, "temp", verbose = FALSE) # read temp variable
T45<-ncvar_get(NEP_T_45, "temp", verbose = FALSE)
T85<-ncvar_get(NEP_T_85, "temp", verbose = FALSE)
#dim(T85) #use this to figure out what order the dimensions are in. it will be the same for all 

#Read in lat and long from one file, they are the same for all of them
lon <- ncvar_get(NEP_T_H, "nav_lon", verbose = FALSE) # read lon variable
lat <- ncvar_get(NEP_T_H, "nav_lat", verbose = FALSE) # read lat variable
mask <- ncvar_get(NEP_mask, "mask", verbose=FALSE)

##figure out what value is used to define no data 
fillvalue <- ncatt_get(NEP_T_H, "temp", "_FillValue")
#print(fillvalue$value)

fillvalue_mask <- ncatt_get(NEP_mask, "mask", "_FillValue") # I dont know if this is necessary. I have a polygon mask that will be used to clip the net CDF later anyways 
#print(fillvalue_mask$value)

#replace fill values with standard NA value
TH[TH == fillvalue$value] <- NA
T45[T45 == fillvalue$value] <- NA
T85[T85 == fillvalue$value] <- NA
mask[mask==fillvalue_mask$value]<- NA # I dont know if this is necessary becuase the mask has binary values (easy to mask)

#Extracting variables as vectors 
NEPlon<-as.vector(lon)
NEPlat<-as.vector(lat)
NEP_mask<-as.vector(mask)

#Extract surface data (3rd dimension) for each month (4th dimension) as vectors
SST_H_01 <- as.vector(TH[, ,1,1])
SST_H_02 <- as.vector(TH[, ,1,2])
SST_H_03 <- as.vector(TH[, ,1,3])
SST_H_04 <- as.vector(TH[, ,1,4])
SST_H_05 <- as.vector(TH[, ,1,5])
SST_H_06 <- as.vector(TH[, ,1,6])
SST_H_07 <- as.vector(TH[, ,1,7])
SST_H_08 <- as.vector(TH[, ,1,8])
SST_H_09 <- as.vector(TH[, ,1,9])
SST_H_10 <- as.vector(TH[, ,1,10])
SST_H_11 <- as.vector(TH[, ,1,11])
SST_H_12 <- as.vector(TH[, ,1,12])

SST_45_01 <- as.vector(T45[, ,1,1])
SST_45_02 <- as.vector(T45[, ,1,2])
SST_45_03 <- as.vector(T45[, ,1,3])
SST_45_04 <- as.vector(T45[, ,1,4])
SST_45_05 <- as.vector(T45[, ,1,5])
SST_45_06 <- as.vector(T45[, ,1,6])
SST_45_07 <- as.vector(T45[, ,1,7])
SST_45_08 <- as.vector(T45[, ,1,8])
SST_45_09 <- as.vector(T45[, ,1,9])
SST_45_10 <- as.vector(T45[, ,1,10])
SST_45_11 <- as.vector(T45[, ,1,11])
SST_45_12 <- as.vector(T45[, ,1,12])

SST_85_01 <- as.vector(T85[, ,1,1])
SST_85_02 <- as.vector(T85[, ,1,2])
SST_85_03 <- as.vector(T85[, ,1,3])
SST_85_04 <- as.vector(T85[, ,1,4])
SST_85_05 <- as.vector(T85[, ,1,5])
SST_85_06 <- as.vector(T85[, ,1,6])
SST_85_07 <- as.vector(T85[, ,1,7])
SST_85_08 <- as.vector(T85[, ,1,8])
SST_85_09 <- as.vector(T85[, ,1,9])
SST_85_10 <- as.vector(T85[, ,1,10])
SST_85_11 <- as.vector(T85[, ,1,11])
SST_85_12 <- as.vector(T85[, ,1,12])

#Recombine information as spatial data frame in CRS 3005
NEP_SST<- data.frame(x = NEPlon,y = NEPlat, NEP_mask,
                            SST_H_01,SST_H_02,SST_H_03,SST_H_04,SST_H_05,SST_H_06,
                            SST_H_07,SST_H_08,SST_H_09,SST_H_10,SST_H_11,SST_H_12,
                            SST_45_01,SST_45_02,SST_45_03,SST_45_04,SST_45_05,SST_45_06,
                            SST_45_07,SST_45_08,SST_45_09,SST_45_10,SST_45_11,SST_45_12,
                            SST_85_01,SST_85_02,SST_85_03,SST_85_04,SST_85_05,SST_85_06,
                            SST_85_07,SST_85_08,SST_85_09,SST_85_10,SST_85_11,SST_85_12) %>%
  st_as_sf(coords = c("x", "y"),
           crs = "EPSG:4326") %>%
  st_transform(crs = "EPSG:3005")
#plot(NEP_SST)

#remove unneeded variables 
rm(T45, TH, T85,
   SST_H_01,SST_H_02,SST_H_03,SST_H_04,SST_H_05,SST_H_06,
   SST_H_07,SST_H_08,SST_H_09,SST_H_10,SST_H_11,SST_H_12,
   SST_45_01,SST_45_02,SST_45_03,SST_45_04,SST_45_05,SST_45_06,
   SST_45_07,SST_45_08,SST_45_09,SST_45_10,SST_45_11,SST_45_12,
   SST_85_01,SST_85_02,SST_85_03,SST_85_04,SST_85_05,SST_85_06,
   SST_85_07,SST_85_08,SST_85_09,SST_85_10,SST_85_11,SST_85_12)


############  LOAD NEP Salinity DATA################
# Load NEP 36 temperature data 
NEP_S_H<-nc_open(file.path(climate_dat, "NEP36_MonthlyData","NEP36-CanOE_salt_historical_1986-2005_monthly.nc"))
NEP_S_45<-nc_open(file.path(climate_dat, "NEP36_MonthlyData","NEP36-CanOE_salt_RCP45_2046-2065_monthly.nc"))

#Read in the temperature variable for each scenario
SH<-ncvar_get(NEP_S_H, "salt", verbose = FALSE) # read temp variable
S45<-ncvar_get(NEP_S_45, "salt", verbose = FALSE)
dim(S45) #use this to figure out what order the dimensions are in 

#replace fill values with standard NA value
SH[SH == fillvalue$value] <- NA
S45[S45 == fillvalue$value] <- NA

#Extract surface data (3rd dimension) for each month (4th dimension) as vectors
SSS_H_01 <- as.vector(SH[, ,1,1])
SSS_H_02 <- as.vector(SH[, ,1,2])
SSS_H_03 <- as.vector(SH[, ,1,3])
SSS_H_04 <- as.vector(SH[, ,1,4])
SSS_H_05 <- as.vector(SH[, ,1,5])
SSS_H_06 <- as.vector(SH[, ,1,6])
SSS_H_07 <- as.vector(SH[, ,1,7])
SSS_H_08 <- as.vector(SH[, ,1,8])
SSS_H_09 <- as.vector(SH[, ,1,9])
SSS_H_10 <- as.vector(SH[, ,1,10])
SSS_H_11 <- as.vector(SH[, ,1,11])
SSS_H_12 <- as.vector(SH[, ,1,12])

SSS_45_01 <- as.vector(S45[, ,1,1])
SSS_45_02 <- as.vector(S45[, ,1,2])
SSS_45_03 <- as.vector(S45[, ,1,3])
SSS_45_04 <- as.vector(S45[, ,1,4])
SSS_45_05 <- as.vector(S45[, ,1,5])
SSS_45_06 <- as.vector(S45[, ,1,6])
SSS_45_07 <- as.vector(S45[, ,1,7])
SSS_45_08 <- as.vector(S45[, ,1,8])
SSS_45_09 <- as.vector(S45[, ,1,9])
SSS_45_10 <- as.vector(S45[, ,1,10])
SSS_45_11 <- as.vector(S45[, ,1,11])
SSS_45_12 <- as.vector(S45[, ,1,12])

#Recombine information as spatial data frame in CRS 3005
NEP_SSS<- data.frame(x = NEPlon,y = NEPlat, NEP_mask,
                     SSS_H_01,SSS_H_02,SSS_H_03,SSS_H_04,SSS_H_05,SSS_H_06,
                     SSS_H_07,SSS_H_08,SSS_H_09,SSS_H_10,SSS_H_11,SSS_H_12,
                     SSS_45_01,SSS_45_02,SSS_45_03,SSS_45_04,SSS_45_05,SSS_45_06,
                     SSS_45_07,SSS_45_08,SSS_45_09,SSS_45_10,SSS_45_11,SSS_45_12) %>%
  st_as_sf(coords = c("x", "y"),
           crs = "EPSG:4326") %>%
  st_transform(crs = "EPSG:3005")
#plot(NEP_SST)

#remove unneeded variables 
rm( SH, S45, 
      SSS_H_01,SSS_H_02,SSS_H_03,SSS_H_04,SSS_H_05,SSS_H_06,
      SSS_H_07,SSS_H_08,SSS_H_09,SSS_H_10,SSS_H_11,SSS_H_12,
      SSS_45_01,SSS_45_02,SSS_45_03,SSS_45_04,SSS_45_05,SSS_45_06,
      SSS_45_07,SSS_45_08,SSS_45_09,SSS_45_10,SSS_45_11,SSS_45_12)

############  LOAD NEP PH data #####
# Load NEP 36 temperature data 
NEP_PH_H<-nc_open(file.path(climate_dat, "NEP36_MonthlyData","NEP36-CanOE_PH_historical_1986-2005_monthly.nc"))
NEP_PH_45<-nc_open(file.path(climate_dat, "NEP36_MonthlyData","NEP36-CanOE_PH_RCP45_2046-2065_monthly.nc"))

#Read in the temperature variable for each scenario
PHH<-ncvar_get(NEP_PH_H, "PH", verbose = FALSE) # read temp variable
PH45<-ncvar_get(NEP_PH_45, "PH", verbose = FALSE)

#replace fill values with standard NA value
PHH[PHH == fillvalue$value] <- NA
PH45[PH45 == fillvalue$value] <- NA
#Extract surface data (3rd dimension) for each month (4th dimension) as vectors
SSPH_H_01 <- as.vector(PHH[, ,1,1])
SSPH_H_02 <- as.vector(PHH[, ,1,2])
SSPH_H_03 <- as.vector(PHH[, ,1,3])
SSPH_H_04 <- as.vector(PHH[, ,1,4])
SSPH_H_05 <- as.vector(PHH[, ,1,5])
SSPH_H_06 <- as.vector(PHH[, ,1,6])
SSPH_H_07 <- as.vector(PHH[, ,1,7])
SSPH_H_08 <- as.vector(PHH[, ,1,8])
SSPH_H_09 <- as.vector(PHH[, ,1,9])
SSPH_H_10 <- as.vector(PHH[, ,1,10])
SSPH_H_11 <- as.vector(PHH[, ,1,11])
SSPH_H_12 <- as.vector(PHH[, ,1,12])

SSPH_45_01 <- as.vector(PH45[, ,1,1])
SSPH_45_02 <- as.vector(PH45[, ,1,2])
SSPH_45_03 <- as.vector(PH45[, ,1,3])
SSPH_45_04 <- as.vector(PH45[, ,1,4])
SSPH_45_05 <- as.vector(PH45[, ,1,5])
SSPH_45_06 <- as.vector(PH45[, ,1,6])
SSPH_45_07 <- as.vector(PH45[, ,1,7])
SSPH_45_08 <- as.vector(PH45[, ,1,8])
SSPH_45_09 <- as.vector(PH45[, ,1,9])
SSPH_45_10 <- as.vector(PH45[, ,1,10])
SSPH_45_11 <- as.vector(PH45[, ,1,11])
SSPH_45_12 <- as.vector(PH45[, ,1,12])

#Recombine information as spatial data frame in CRS 3005
NEP_SSPH<- data.frame(x = NEPlon,y = NEPlat, NEP_mask,
                     SSPH_H_01,SSPH_H_02,SSPH_H_03,SSPH_H_04,SSPH_H_05,SSPH_H_06,
                     SSPH_H_07,SSPH_H_08,SSPH_H_09,SSPH_H_10,SSPH_H_11,SSPH_H_12,
                     SSPH_45_01,SSPH_45_02,SSPH_45_03,SSPH_45_04,SSPH_45_05,SSPH_45_06,
                     SSPH_45_07,SSPH_45_08,SSPH_45_09,SSPH_45_10,SSPH_45_11,SSPH_45_12) %>%
  st_as_sf(coords = c("x", "y"),
           crs = "EPSG:4326") %>%
  st_transform(crs = "EPSG:3005")
#plot(NEP_SSPH)

#remove unneeded variables 
rm(lat, lon, fillvalue, fillvalue_mask, PHH, PH45, NEPlat, NEPlon, NEP_mask, mask,
   NEP_T_H, NEP_T_45, NEP_T_85, NEP_S_H, NEP_S_45,NEP_PH_H,NEP_PH_45,
   SSPH_H_01,SSPH_H_02,SSPH_H_03,SSPH_H_04,SSPH_H_05,SSPH_H_06,
   SSPH_H_07,SSPH_H_08,SSPH_H_09,SSPH_H_10,SSPH_H_11,SSPH_H_12,
   SSPH_45_01,SSPH_45_02,SSPH_45_03,SSPH_45_04,SSPH_45_05,SSPH_45_06,
   SSPH_45_07,SSPH_45_08,SSPH_45_09,SSPH_45_10,SSPH_45_11,SSPH_45_12)

#Export NEP points as shp file if needed
#sf::st_write(NEP_SSS, file.path(climate_dat, "NEP36_MonthlyData", "NEPmonthly_SSS.shp"))
#sf::st_write(NEP_SST, file.path(climate_dat, "NEP36_MonthlyData","NEPmonthly_SST.shp"))
#sf::st_write(NEP_SSPH, file.path(climate_dat, "NEP36_MonthlyData","NEPmonthly_SSPH.shp"))

############  LOAD SSC temperature and salinity data ##### 
#NOTE: NO PH Data yet, could ask Amber for it
SSC_H<-nc_open(file.path(climate_dat, "SalishSeaCast_MonthlyData", "SalishSeaCast-VNR023_1d_grid_T_mean12_1986-05.nc"))
SSC_45<-nc_open(file.path(climate_dat, "SalishSeaCast_MonthlyData", "SalishSeaCast-VNR033_1d_grid_T_mean12_4.5_2046-65.nc"))
SSC_85<-nc_open(file.path(climate_dat, "SalishSeaCast_MonthlyData", "SalishSeaCast-VNR034_1d_grid_T_mean12_8.5_2046-65.nc"))
#print(SSC_H) ## See the names of the variables and dimensions

#Read in the temperature and salinity variable for each scenario
TH<-ncvar_get(SSC_H, "votemper", verbose = FALSE) # read temp variable
T45<-ncvar_get(SSC_45, "votemper", verbose = FALSE)
T85<-ncvar_get(SSC_85, "votemper", verbose = FALSE)
SH<-ncvar_get(SSC_H, "vosaline", verbose = FALSE) # read temp variable
S45<-ncvar_get(SSC_45, "vosaline", verbose = FALSE)
S85<-ncvar_get(SSC_85, "vosaline", verbose = FALSE)

# figure out what order the dimensions are in. here the order is [long, lat, depth, month]
#dim(T85) 

#Read in lat and long from one of the files, they are the same for all of them
lon <- ncvar_get(SSC_H, "nav_lon", verbose = FALSE) # read lon variable
lat <- ncvar_get(SSC_H, "nav_lat", verbose = FALSE) # read lat variable

##figure out what value is used to define no data. This is the same value for salinity and temperature 
fillvalue <- ncatt_get(SSC_H, "votemper", "_FillValue")
print(fillvalue$value)

#replace fill values with standard NA value
TH[TH == fillvalue$value] <- NA
T45[T45 == fillvalue$value] <- NA
T85[T85 == fillvalue$value] <- NA
SH[SH == fillvalue$value] <- NA
S45[S45 == fillvalue$value] <- NA
S85[S85 == fillvalue$value] <- NA

#extract surface data (3rd dimension) for each month (4th dimension) as vectors
SSClon<-as.vector(lon)
SSClat<-as.vector(lat)

SST_H_01 <- as.vector(TH[, ,1,1])
SST_H_02 <- as.vector(TH[, ,1,2])
SST_H_03 <- as.vector(TH[, ,1,3])
SST_H_04 <- as.vector(TH[, ,1,4])
SST_H_05 <- as.vector(TH[, ,1,5])
SST_H_06 <- as.vector(TH[, ,1,6])
SST_H_07 <- as.vector(TH[, ,1,7])
SST_H_08 <- as.vector(TH[, ,1,8])
SST_H_09 <- as.vector(TH[, ,1,9])
SST_H_10 <- as.vector(TH[, ,1,10])
SST_H_11 <- as.vector(TH[, ,1,11])
SST_H_12 <- as.vector(TH[, ,1,12])

SST_45_01 <- as.vector(T45[, ,1,1])
SST_45_02 <- as.vector(T45[, ,1,2])
SST_45_03 <- as.vector(T45[, ,1,3])
SST_45_04 <- as.vector(T45[, ,1,4])
SST_45_05 <- as.vector(T45[, ,1,5])
SST_45_06 <- as.vector(T45[, ,1,6])
SST_45_07 <- as.vector(T45[, ,1,7])
SST_45_08 <- as.vector(T45[, ,1,8])
SST_45_09 <- as.vector(T45[, ,1,9])
SST_45_10 <- as.vector(T45[, ,1,10])
SST_45_11 <- as.vector(T45[, ,1,11])
SST_45_12 <- as.vector(T45[, ,1,12])

SST_85_01 <- as.vector(T85[, ,1,1])
SST_85_02 <- as.vector(T85[, ,1,2])
SST_85_03 <- as.vector(T85[, ,1,3])
SST_85_04 <- as.vector(T85[, ,1,4])
SST_85_05 <- as.vector(T85[, ,1,5])
SST_85_06 <- as.vector(T85[, ,1,6])
SST_85_07 <- as.vector(T85[, ,1,7])
SST_85_08 <- as.vector(T85[, ,1,8])
SST_85_09 <- as.vector(T85[, ,1,9])
SST_85_10 <- as.vector(T85[, ,1,10])
SST_85_11 <- as.vector(T85[, ,1,11])
SST_85_12 <- as.vector(T85[, ,1,12])

#Recombine information as spatial data frame in CRS 3005
SSC_SST<- data.frame(x = SSClon,y = SSClat, 
                     SST_H_01,SST_H_02,SST_H_03,SST_H_04,SST_H_05,SST_H_06,
                     SST_H_07,SST_H_08,SST_H_09,SST_H_10,SST_H_11,SST_H_12,
                     SST_45_01,SST_45_02,SST_45_03,SST_45_04,SST_45_05,SST_45_06,
                     SST_45_07,SST_45_08,SST_45_09,SST_45_10,SST_45_11,SST_45_12,
                     SST_85_01,SST_85_02,SST_85_03,SST_85_04,SST_85_05,SST_85_06,
                     SST_85_07,SST_85_08,SST_85_09,SST_85_10,SST_85_11,SST_85_12) %>%
  st_as_sf(coords = c("x", "y"),
           crs = "EPSG:4326") %>%
  st_transform(crs = "EPSG:3005")
#plot(SSC_SST)

# Repeat for salinity 
SSS_H_01 <- as.vector(SH[, ,1,1])
SSS_H_02 <- as.vector(SH[, ,1,2])
SSS_H_03 <- as.vector(SH[, ,1,3])
SSS_H_04 <- as.vector(SH[, ,1,4])
SSS_H_05 <- as.vector(SH[, ,1,5])
SSS_H_06 <- as.vector(SH[, ,1,6])
SSS_H_07 <- as.vector(SH[, ,1,7])
SSS_H_08 <- as.vector(SH[, ,1,8])
SSS_H_09 <- as.vector(SH[, ,1,9])
SSS_H_10 <- as.vector(SH[, ,1,10])
SSS_H_11 <- as.vector(SH[, ,1,11])
SSS_H_12 <- as.vector(SH[, ,1,12])

SSS_45_01 <- as.vector(S45[, ,1,1])
SSS_45_02 <- as.vector(S45[, ,1,2])
SSS_45_03 <- as.vector(S45[, ,1,3])
SSS_45_04 <- as.vector(S45[, ,1,4])
SSS_45_05 <- as.vector(S45[, ,1,5])
SSS_45_06 <- as.vector(S45[, ,1,6])
SSS_45_07 <- as.vector(S45[, ,1,7])
SSS_45_08 <- as.vector(S45[, ,1,8])
SSS_45_09 <- as.vector(S45[, ,1,9])
SSS_45_10 <- as.vector(S45[, ,1,10])
SSS_45_11 <- as.vector(S45[, ,1,11])
SSS_45_12 <- as.vector(S45[, ,1,12])

SSS_85_01 <- as.vector(S85[, ,1,1])
SSS_85_02 <- as.vector(S85[, ,1,2])
SSS_85_03 <- as.vector(S85[, ,1,3])
SSS_85_04 <- as.vector(S85[, ,1,4])
SSS_85_05 <- as.vector(S85[, ,1,5])
SSS_85_06 <- as.vector(S85[, ,1,6])
SSS_85_07 <- as.vector(S85[, ,1,7])
SSS_85_08 <- as.vector(S85[, ,1,8])
SSS_85_09 <- as.vector(S85[, ,1,9])
SSS_85_10 <- as.vector(S85[, ,1,10])
SSS_85_11 <- as.vector(S85[, ,1,11])
SSS_85_12 <- as.vector(S85[, ,1,12])

#Recombine information as spatial data frame in CRS 3005
SSC_SSS<- data.frame(x = SSClon,y = SSClat, 
                     SSS_H_01,SSS_H_02,SSS_H_03,SSS_H_04,SSS_H_05,SSS_H_06,
                     SSS_H_07,SSS_H_08,SSS_H_09,SSS_H_10,SSS_H_11,SSS_H_12,
                     SSS_45_01,SSS_45_02,SSS_45_03,SSS_45_04,SSS_45_05,SSS_45_06,
                     SSS_45_07,SSS_45_08,SSS_45_09,SSS_45_10,SSS_45_11,SSS_45_12,
                     SSS_85_01,SSS_85_02,SSS_85_03,SSS_85_04,SSS_85_05,SSS_85_06,
                     SSS_85_07,SSS_85_08,SSS_85_09,SSS_85_10,SSS_85_11,SSS_85_12) %>%
  st_as_sf(coords = c("x", "y"),
           crs = "EPSG:4326") %>%
  st_transform(crs = "EPSG:3005")

# Remove objects no longer needed 
rm(lat, lon, fillvalue,  T45, TH, T85,SH, S45, S85,
   SSClat, SSClon, SSC_45, SSC_85, SSC_H,
   SST_H_01,SST_H_02,SST_H_03,SST_H_04,SST_H_05,SST_H_06,
   SST_H_07,SST_H_08,SST_H_09,SST_H_10,SST_H_11,SST_H_12,
   SST_45_01,SST_45_02,SST_45_03,SST_45_04,SST_45_05,SST_45_06,
   SST_45_07,SST_45_08,SST_45_09,SST_45_10,SST_45_11,SST_45_12,
   SST_85_01,SST_85_02,SST_85_03,SST_85_04,SST_85_05,SST_85_06,
   SST_85_07,SST_85_08,SST_85_09,SST_85_10,SST_85_11,SST_85_12,
   SSS_H_01,SSS_H_02,SSS_H_03,SSS_H_04,SSS_H_05,SSS_H_06,
   SSS_H_07,SSS_H_08,SSS_H_09,SSS_H_10,SSS_H_11,SSS_H_12,
   SSS_45_01,SSS_45_02,SSS_45_03,SSS_45_04,SSS_45_05,SSS_45_06,
   SSS_45_07,SSS_45_08,SSS_45_09,SSS_45_10,SSS_45_11,SSS_45_12,
   SSS_85_01,SSS_85_02,SSS_85_03,SSS_85_04,SSS_85_05,SSS_85_06,
   SSS_85_07,SSS_85_08,SSS_85_09,SSS_85_10,SSS_85_11,SSS_85_12)

#Export points as shp file
#sf::st_write(SSC_SSS, file.path(climate_dat, "SalishSeaCast_MonthlyData", "SSCmonthly_SSS.shp"))
#sf::st_write(SSC_SST, file.path(climate_dat, "SalishSeaCast_MonthlyData","SSCmonthly_SST.shp")

############  LOAD SSC MASK############
#Loading the SSC mask isnt working in this r code. There are 4 variables that could be the mask in the netCDF.
#SSC is masked later in the standardizing process, so i don't think adding the mask here matters
# Below is the code I tried to use 

#SSC_mask<-nc_open(file.path(climate_dat, "SalishSeaCast_MonthlyData","SSC_mesh_mask202108us.nc"))
#print(SSC_mask)
#tmask <- ncvar_get(SSC_mask, "tmask", verbose=FALSE)
#umask <- ncvar_get(SSC_mask, "umask", verbose=FALSE)
#vmask <- ncvar_get(SSC_mask, "vmask", verbose=FALSE)
#fmask <- ncvar_get(SSC_mask, "fmask", verbose=FALSE)
#lat <- ncvar_get(SSC_mask, "nav_lat", verbose=FALSE)
#lon <- ncvar_get(SSC_mask, "nav_lon", verbose=FALSE)

#fillvalue_mask <- ncatt_get(SSC_mask, "tmask", "_FillValue") # I dont know if this is necessary. I have a polygon mask that will be used to clip the net CDF later anyways 
#print(fillvalue_mask$value)
#tmask[tmask==fillvalue_mask$value]<- NA # I dont know if this is necessary
#umask[umask==fillvalue_mask$value]<- NA
#vmask[vmask==fillvalue_mask$value]<- NA
#fmask[fmask==fillvalue_mask$value]<- NA

#SSC_tmask<-as.vector(tmask)
#SSC_umask<-as.vector(umask)
#SSC_vmask<-as.vector(vmask)
#SSC_fmask<-as.vector(fmask)
#SSC_lon<-as.vector(lon)
#SSC_lat<-as.vector(lat)

#SSC_masks<- data.frame(x = SSClon,y = SSClat, SSC_tmask, SSC_umask, SSC_vmask, SSC_fmask) %>%
  #st_as_sf(coords = c("x", "y"),
          # crs = "EPSG:4326") %>%
  #st_transform(crs = "EPSG:3005")
#sf::st_write(SSC_masks, climate_dat, "SalishSeaCast_MonthlyData", "SSC_masks.shp")

############  LOAD BCCM surface data ############
BCCM_85_SST<- read_ncdf(file.path(climate_dat, "BCCM", "bcc42_bioNew_can85_2046to2065_monSST.nc"), proxy = FALSE, var = c("lat_rho","lon_rho", "temp", "months"), make_time = TRUE)
BCCM_45_SST<- read_ncdf(file.path(climate_dat, "BCCM", "bcc42_bioNew_can45_2046to2065_monSST.nc"), proxy = FALSE, var = c("lat_rho","lon_rho", "temp", "months"), make_time = TRUE)
BCCM_Hist_SST<- read_ncdf(file.path(climate_dat, "BCCM", "bcc42_bioNew_his_1986to2005_monSST.nc"), proxy = FALSE, var = c("lat_rho","lon_rho", "temp", "months"),  make_time = TRUE)
BCCM_85_SSS<- read_ncdf(file.path(climate_dat, "BCCM", "bcc42_bioNew_can85_2046to2065_monSSS.nc"), proxy = FALSE, var = c("lat_rho","lon_rho", "salt", "months"), make_time = TRUE)
BCCM_45_SSS<- read_ncdf(file.path(climate_dat, "BCCM", "bcc42_bioNew_can45_2046to2065_monSSS.nc"), proxy = FALSE, var = c("lat_rho","lon_rho", "salt", "months"),  make_time = TRUE)
BCCM_Hist_SSS<- read_ncdf(file.path(climate_dat, "BCCM", "bcc42_bioNew_his_1986to2005_monSSS.nc"), proxy = FALSE, var = c("lat_rho","lon_rho", "salt", "months"),  make_time = TRUE)
BCCM_85_SSPH<- read_ncdf(file.path(climate_dat, "BCCM", "bcc42_bioNew_can85_2046to2065_monSSPH.nc"), proxy = FALSE, var = c("lat_rho","lon_rho", "pH", "months"), make_time = TRUE)
BCCM_45_SSPH<- read_ncdf(file.path(climate_dat, "BCCM", "bcc42_bioNew_can45_2046to2065_monSSPH.nc"), proxy = FALSE, var = c("lat_rho","lon_rho", "pH", "months"),  make_time = TRUE)
BCCM_Hist_SSPH<- read_ncdf(file.path(climate_dat, "BCCM", "bcc42_bioNew_his_1986to2005_monSSPH.nc"), proxy = FALSE, var = c("lat_rho","lon_rho", "pH", "months"),  make_time = TRUE)

#print(BCCM_45_SST) # if the offset is 0.5 in both x and y, it means the points are centroids of cells 
# if the x and y delta values are positive it means the 1, 1 cell is in the bottom left, and both variables increase with the axes 

# Extract each variable as a vector
BCCM_lon <- as.vector(BCCM_45_SST$lon_rho)
BCCM_lat <- as.vector(BCCM_45_SST$lat_rho)
BCCM_mask<- as.vector(BCCM_Hist_SST$mask_rho)

SST_85_01    <- as.vector(BCCM_85_SST$temp[,,1])
SST_85_02    <- as.vector(BCCM_85_SST$temp[,,2])
SST_85_03    <- as.vector(BCCM_85_SST$temp[,,3])
SST_85_04    <- as.vector(BCCM_85_SST$temp[,,4])
SST_85_05    <- as.vector(BCCM_85_SST$temp[,,5])
SST_85_06    <- as.vector(BCCM_85_SST$temp[,,6])
SST_85_07    <- as.vector(BCCM_85_SST$temp[,,7])
SST_85_08    <- as.vector(BCCM_85_SST$temp[,,8])
SST_85_09    <- as.vector(BCCM_85_SST$temp[,,9])
SST_85_10    <- as.vector(BCCM_85_SST$temp[,,10])
SST_85_11    <- as.vector(BCCM_85_SST$temp[,,11])
SST_85_12    <- as.vector(BCCM_85_SST$temp[,,12])

SST_45_01    <- as.vector(BCCM_45_SST$temp[,,1])
SST_45_02    <- as.vector(BCCM_45_SST$temp[,,2])
SST_45_03    <- as.vector(BCCM_45_SST$temp[,,3])
SST_45_04    <- as.vector(BCCM_45_SST$temp[,,4])
SST_45_05    <- as.vector(BCCM_45_SST$temp[,,5])
SST_45_06    <- as.vector(BCCM_45_SST$temp[,,6])
SST_45_07    <- as.vector(BCCM_45_SST$temp[,,7])
SST_45_08    <- as.vector(BCCM_45_SST$temp[,,8])
SST_45_09    <- as.vector(BCCM_45_SST$temp[,,9])
SST_45_10    <- as.vector(BCCM_45_SST$temp[,,10])
SST_45_11    <- as.vector(BCCM_45_SST$temp[,,11])
SST_45_12    <- as.vector(BCCM_45_SST$temp[,,12])

SST_H_01    <- as.vector(BCCM_Hist_SST$temp[,,1])
SST_H_02    <- as.vector(BCCM_Hist_SST$temp[,,2])
SST_H_03    <- as.vector(BCCM_Hist_SST$temp[,,3])
SST_H_04    <- as.vector(BCCM_Hist_SST$temp[,,4])
SST_H_05    <- as.vector(BCCM_Hist_SST$temp[,,5])
SST_H_06    <- as.vector(BCCM_Hist_SST$temp[,,6])
SST_H_07    <- as.vector(BCCM_Hist_SST$temp[,,7])
SST_H_08    <- as.vector(BCCM_Hist_SST$temp[,,8])
SST_H_09    <- as.vector(BCCM_Hist_SST$temp[,,9])
SST_H_10    <- as.vector(BCCM_Hist_SST$temp[,,10])
SST_H_11    <- as.vector(BCCM_Hist_SST$temp[,,11])
SST_H_12    <- as.vector(BCCM_Hist_SST$temp[,,12])

SSS_85_01    <- as.vector(BCCM_85_SSS$salt[,,1])
SSS_85_02    <- as.vector(BCCM_85_SSS$salt[,,2])
SSS_85_03    <- as.vector(BCCM_85_SSS$salt[,,3])
SSS_85_04    <- as.vector(BCCM_85_SSS$salt[,,4])
SSS_85_05    <- as.vector(BCCM_85_SSS$salt[,,5])
SSS_85_06    <- as.vector(BCCM_85_SSS$salt[,,6])
SSS_85_07    <- as.vector(BCCM_85_SSS$salt[,,7])
SSS_85_08    <- as.vector(BCCM_85_SSS$salt[,,8])
SSS_85_09    <- as.vector(BCCM_85_SSS$salt[,,9])
SSS_85_10    <- as.vector(BCCM_85_SSS$salt[,,10])
SSS_85_11    <- as.vector(BCCM_85_SSS$salt[,,11])
SSS_85_12    <- as.vector(BCCM_85_SSS$salt[,,12])

SSS_45_01    <- as.vector(BCCM_45_SSS$salt[,,1])
SSS_45_02    <- as.vector(BCCM_45_SSS$salt[,,2])
SSS_45_03    <- as.vector(BCCM_45_SSS$salt[,,3])
SSS_45_04    <- as.vector(BCCM_45_SSS$salt[,,4])
SSS_45_05    <- as.vector(BCCM_45_SSS$salt[,,5])
SSS_45_06    <- as.vector(BCCM_45_SSS$salt[,,6])
SSS_45_07    <- as.vector(BCCM_45_SSS$salt[,,7])
SSS_45_08    <- as.vector(BCCM_45_SSS$salt[,,8])
SSS_45_09    <- as.vector(BCCM_45_SSS$salt[,,9])
SSS_45_10    <- as.vector(BCCM_45_SSS$salt[,,10])
SSS_45_11    <- as.vector(BCCM_45_SSS$salt[,,11])
SSS_45_12    <- as.vector(BCCM_45_SSS$salt[,,12])

SSS_H_01    <- as.vector(BCCM_Hist_SSS$salt[,,1])
SSS_H_02    <- as.vector(BCCM_Hist_SSS$salt[,,2])
SSS_H_03    <- as.vector(BCCM_Hist_SSS$salt[,,3])
SSS_H_04    <- as.vector(BCCM_Hist_SSS$salt[,,4])
SSS_H_05    <- as.vector(BCCM_Hist_SSS$salt[,,5])
SSS_H_06    <- as.vector(BCCM_Hist_SSS$salt[,,6])
SSS_H_07    <- as.vector(BCCM_Hist_SSS$salt[,,7])
SSS_H_08    <- as.vector(BCCM_Hist_SSS$salt[,,8])
SSS_H_09    <- as.vector(BCCM_Hist_SSS$salt[,,9])
SSS_H_10    <- as.vector(BCCM_Hist_SSS$salt[,,10])
SSS_H_11    <- as.vector(BCCM_Hist_SSS$salt[,,11])
SSS_H_12    <- as.vector(BCCM_Hist_SSS$salt[,,12])

SSPH_85_01    <- as.vector(BCCM_85_SSPH$pH[,,1])
SSPH_85_02    <- as.vector(BCCM_85_SSPH$pH[,,2])
SSPH_85_03    <- as.vector(BCCM_85_SSPH$pH[,,3])
SSPH_85_04    <- as.vector(BCCM_85_SSPH$pH[,,4])
SSPH_85_05    <- as.vector(BCCM_85_SSPH$pH[,,5])
SSPH_85_06    <- as.vector(BCCM_85_SSPH$pH[,,6])
SSPH_85_07    <- as.vector(BCCM_85_SSPH$pH[,,7])
SSPH_85_08    <- as.vector(BCCM_85_SSPH$pH[,,8])
SSPH_85_09    <- as.vector(BCCM_85_SSPH$pH[,,9])
SSPH_85_10    <- as.vector(BCCM_85_SSPH$pH[,,10])
SSPH_85_11    <- as.vector(BCCM_85_SSPH$pH[,,11])
SSPH_85_12    <- as.vector(BCCM_85_SSPH$pH[,,12])

SSPH_45_01    <- as.vector(BCCM_45_SSPH$pH[,,1])
SSPH_45_02    <- as.vector(BCCM_45_SSPH$pH[,,2])
SSPH_45_03    <- as.vector(BCCM_45_SSPH$pH[,,3])
SSPH_45_04    <- as.vector(BCCM_45_SSPH$pH[,,4])
SSPH_45_05    <- as.vector(BCCM_45_SSPH$pH[,,5])
SSPH_45_06    <- as.vector(BCCM_45_SSPH$pH[,,6])
SSPH_45_07    <- as.vector(BCCM_45_SSPH$pH[,,7])
SSPH_45_08    <- as.vector(BCCM_45_SSPH$pH[,,8])
SSPH_45_09    <- as.vector(BCCM_45_SSPH$pH[,,9])
SSPH_45_10    <- as.vector(BCCM_45_SSPH$pH[,,10])
SSPH_45_11    <- as.vector(BCCM_45_SSPH$pH[,,11])
SSPH_45_12    <- as.vector(BCCM_45_SSPH$pH[,,12])

SSPH_H_01    <- as.vector(BCCM_Hist_SSPH$pH[,,1])
SSPH_H_02    <- as.vector(BCCM_Hist_SSPH$pH[,,2])
SSPH_H_03    <- as.vector(BCCM_Hist_SSPH$pH[,,3])
SSPH_H_04    <- as.vector(BCCM_Hist_SSPH$pH[,,4])
SSPH_H_05    <- as.vector(BCCM_Hist_SSPH$pH[,,5])
SSPH_H_06    <- as.vector(BCCM_Hist_SSPH$pH[,,6])
SSPH_H_07    <- as.vector(BCCM_Hist_SSPH$pH[,,7])
SSPH_H_08    <- as.vector(BCCM_Hist_SSPH$pH[,,8])
SSPH_H_09    <- as.vector(BCCM_Hist_SSPH$pH[,,9])
SSPH_H_10    <- as.vector(BCCM_Hist_SSPH$pH[,,10])
SSPH_H_11    <- as.vector(BCCM_Hist_SSPH$pH[,,11])
SSPH_H_12    <- as.vector(BCCM_Hist_SSPH$pH[,,12])

# Recombine variables into a data frame and reproject into 3005 (BC albers)
BCCM_SSS<- data.frame(x = BCCM_lon,y = BCCM_lat,
                      SSS_H_01,SSS_H_02,SSS_H_03,SSS_H_04,SSS_H_05,SSS_H_06,
                      SSS_H_07,SSS_H_08,SSS_H_09,SSS_H_10,SSS_H_11,SSS_H_12,
                      SSS_45_01,SSS_45_02,SSS_45_03,SSS_45_04,SSS_45_05,SSS_45_06,
                      SSS_45_07,SSS_45_08,SSS_45_09,SSS_45_10,SSS_45_11,SSS_45_12,
                      SSS_85_01,SSS_85_02,SSS_85_03,SSS_85_04,SSS_85_05,SSS_85_06,
                      SSS_85_07,SSS_85_08,SSS_85_09,SSS_85_10,SSS_85_11,SSS_85_12) %>%
  st_as_sf(coords = c("x", "y"),
           crs = "EPSG:4326") %>%
  st_transform(crs = "EPSG:3005")
#plot(BCCM)

BCCM_SST<- data.frame(x = BCCM_lon,y = BCCM_lat,
                      SST_H_01,SST_H_02,SST_H_03,SST_H_04,SST_H_05,SST_H_06,
                      SST_H_07,SST_H_08,SST_H_09,SST_H_10,SST_H_11,SST_H_12,
                      SST_45_01,SST_45_02,SST_45_03,SST_45_04,SST_45_05,SST_45_06,
                      SST_45_07,SST_45_08,SST_45_09,SST_45_10,SST_45_11,SST_45_12,
                      SST_85_01,SST_85_02,SST_85_03,SST_85_04,SST_85_05,SST_85_06,
                      SST_85_07,SST_85_08,SST_85_09,SST_85_10,SST_85_11,SST_85_12) %>%
  st_as_sf(coords = c("x", "y"),
           crs = "EPSG:4326") %>%
  st_transform(crs = "EPSG:3005")

BCCM_SSPH<- data.frame(x = BCCM_lon,y = BCCM_lat,
                      SSPH_H_01,SSPH_H_02,SSPH_H_03,SSPH_H_04,SSPH_H_05,SSPH_H_06,
                      SSPH_H_07,SSPH_H_08,SSPH_H_09,SSPH_H_10,SSPH_H_11,SSPH_H_12,
                      SSPH_45_01,SSPH_45_02,SSPH_45_03,SSPH_45_04,SSPH_45_05,SSPH_45_06,
                      SSPH_45_07,SSPH_45_08,SSPH_45_09,SSPH_45_10,SSPH_45_11,SSPH_45_12,
                      SSPH_85_01,SSPH_85_02,SSPH_85_03,SSPH_85_04,SSPH_85_05,SSPH_85_06,
                      SSPH_85_07,SSPH_85_08,SSPH_85_09,SSPH_85_10,SSPH_85_11,SSPH_85_12) %>%
  st_as_sf(coords = c("x", "y"),
           crs = "EPSG:4326") %>%
  st_transform(crs = "EPSG:3005")

# remove the extra vectors 
rm(BCCM_45_SSS, BCCM_45_SST, BCCM_45_SSPH, BCCM_85_SSS, BCCM_85_SST,BCCM_85_SSPH,BCCM_Hist_SSS, BCCM_Hist_SST,BCCM_Hist_SSPH,
      BCCM_lat, BCCM_lon, BCCM_mask,
   SST_H_01,SST_H_02,SST_H_03,SST_H_04,SST_H_05,SST_H_06,
      SST_H_07,SST_H_08,SST_H_09,SST_H_10,SST_H_11,SST_H_12,
      SST_45_01,SST_45_02,SST_45_03,SST_45_04,SST_45_05,SST_45_06,
      SST_45_07,SST_45_08,SST_45_09,SST_45_10,SST_45_11,SST_45_12,
      SST_85_01,SST_85_02,SST_85_03,SST_85_04,SST_85_05,SST_85_06,
      SST_85_07,SST_85_08,SST_85_09,SST_85_10,SST_85_11,SST_85_12,
   SSS_H_01,SSS_H_02,SSS_H_03,SSS_H_04,SSS_H_05,SSS_H_06,
      SSS_H_07,SSS_H_08,SSS_H_09,SSS_H_10,SSS_H_11,SSS_H_12,
      SSS_45_01,SSS_45_02,SSS_45_03,SSS_45_04,SSS_45_05,SSS_45_06,
      SSS_45_07,SSS_45_08,SSS_45_09,SSS_45_10,SSS_45_11,SSS_45_12,
      SSS_85_01,SSS_85_02,SSS_85_03,SSS_85_04,SSS_85_05,SSS_85_06,
      SSS_85_07,SSS_85_08,SSS_85_09,SSS_85_10,SSS_85_11,SSS_85_12,
   SSPH_H_01,SSPH_H_02,SSPH_H_03,SSPH_H_04,SSPH_H_05,SSPH_H_06,
      SSPH_H_07,SSPH_H_08,SSPH_H_09,SSPH_H_10,SSPH_H_11,SSPH_H_12,
      SSPH_45_01,SSPH_45_02,SSPH_45_03,SSPH_45_04,SSPH_45_05,SSPH_45_06,
      SSPH_45_07,SSPH_45_08,SSPH_45_09,SSPH_45_10,SSPH_45_11,SSPH_45_12,
      SSPH_85_01,SSPH_85_02,SSPH_85_03,SSPH_85_04,SSPH_85_05,SSPH_85_06,
      SSPH_85_07,SSPH_85_08,SSPH_85_09,SSPH_85_10,SSPH_85_11,SSPH_85_12)
  
# Export at shapefile 
#sf::st_write(BCCM_SST, file.path(climate_dat, "BCCM", "BCCMmonthly_SST.shp")
#sf::st_write(BCCM_SSS, file.path(climate_dat,"BCCM", "BCCMmonthly_SSS.shp")

############  LOAD Cumulative Impacts Data #####

CI<-read_sf(file.path(spatial_dat, "CumulativeImpacts_shp", "CI.shp"))
CI<-st_transform(CI,crs = "EPSG:3005" )
# calculate centroids of CI polygon grid
CI_points<- st_centroid(CI)
rm(CI)

