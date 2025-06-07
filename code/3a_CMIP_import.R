
library(here)
here()
source(here("code","0_setup.R"))


#devtools::install_github("https://github.com/JGCRI/RCMIP5.git")
library(RCMIP5)
library(ncdf4)
library(pacea)

## For CMIP6 files, try:
#install.packages("RGtk2")
#install.packages("rattle")
#options(guiToolkit "RGtk2")
#install.packages("gWidgetsRGtk2", dep=TRUE)

############# LOAD CMIP 5 SST Data ###########
# See the information associated with the CMIP 5 models 
C5_files <- getFileInfo(file.path(paths$climate, "Marine_CMIP5"))

# group the CMIP 5 SST models  
SST_files <- filter(C5_files, variable == "tos")

# Check the time frames of the data 
checkTimePeriod(SST_files)  

# Load models and convert to data frames
ENSMED_45_SST <- loadCMIP5("tos", "ensemblemedian", "rcp45",                # Selects variable, model, and scenario
                           path=file.path(paths$climate, "Marine_CMIP5"),   # points to file location 
                           verbose=T, yearRange=c(2040, 2070))%>%           # Selects time range
  filterDimensions(lonRange=c(210,245),latRange=c(45,60),verbose=T) %>%     # Selects the spatial area based on lat and long ranges
  as.data.frame() %>%     # Creating a data frame
  mutate(year = floor(time),                             # creating a year column 
         month = floor((time - floor(time)) * 12 + 1),   #Creating a month column
         season = case_when(                             # Creating a season column based on the month column
           month %in% c(1, 2, 3) ~ "Winter",
           month %in% c(4, 5, 6) ~ "Spring",
           month %in% c(7, 8, 9) ~ "Summer",
           month %in% c(10, 11, 12) ~ "Fall"),
         value = value - 273.15,              # Converting Kelvin to Celcius 
         scenario = "rcp45",                  # Creating a column filled with rcp45 as this is the scenario selected in the first line 
         model="ensemblemedian")              # Creating a column to specify the model                 

ENSMED_85_SST <- loadCMIP5("tos", "ensemblemedian", "rcp85", path=file.path(paths$climate, "Marine_CMIP5"), verbose=T, yearRange=c(2040, 2070))%>%
  filterDimensions( lonRange=c(210, 245), latRange=c(45, 60), verbose=T) %>%
  as.data.frame() %>% 
  mutate(year = floor(time),
         month = floor((time - floor(time)) * 12 + 1),
         season = case_when(
           month %in% c(1, 2, 3) ~ "Winter",
           month %in% c(4, 5, 6) ~ "Spring",
           month %in% c(7, 8, 9) ~ "Summer",
           month %in% c(10, 11, 12) ~ "Fall"),
         value = value - 273.15,
         scenario = "rcp85",                 
         model="ensemblemedian")

ENSMED_H_SST <- loadCMIP5("tos", "ensemblemedian", "hist", path=file.path(paths$climate, "Marine_CMIP5"), verbose=T, yearRange=c(1980, 2010))%>%
  filterDimensions( lonRange=c(210, 245), latRange=c(45, 60), verbose=T) %>%
  as.data.frame() %>% 
  mutate(year = floor(time),
         month = floor((time - floor(time)) * 12 + 1),
         season = case_when(
           month %in% c(1, 2, 3) ~ "Winter",
           month %in% c(4, 5, 6) ~ "Spring",
           month %in% c(7, 8, 9) ~ "Summer",
           month %in% c(10, 11, 12) ~ "Fall"),
         value = value - 273.15,
         scenario = "historical",                 
         model="ensemblemedian")
ENSMIN_45_SST <- loadCMIP5("tos", "ensemblemin", "rcp45", path=file.path(paths$climate, "Marine_CMIP5"), verbose=T, yearRange=c(2040, 2070))%>%
  filterDimensions( lonRange=c(210, 245), latRange=c(45, 60), verbose=T) %>%
  as.data.frame() %>% 
  mutate(year = floor(time),
         month = floor((time - floor(time)) * 12 + 1),
         season = case_when(
           month %in% c(1, 2, 3) ~ "Winter",
           month %in% c(4, 5, 6) ~ "Spring",
           month %in% c(7, 8, 9) ~ "Summer",
           month %in% c(10, 11, 12) ~ "Fall"),
         value = value - 273.15,
         scenario = "rcp45",                 
         model="ensemblemin")
ENSMIN_85_SST <- loadCMIP5("tos", "ensemblemin", "rcp85", path=file.path(paths$climate, "Marine_CMIP5"), verbose=T, yearRange=c(2040, 2070))%>%
  filterDimensions( lonRange=c(210, 245), latRange=c(45, 60), verbose=T) %>%
  as.data.frame() %>% 
  mutate(year = floor(time),
         month = floor((time - floor(time)) * 12 + 1),
         season = case_when(
           month %in% c(1, 2, 3) ~ "Winter",
           month %in% c(4, 5, 6) ~ "Spring",
           month %in% c(7, 8, 9) ~ "Summer",
           month %in% c(10, 11, 12) ~ "Fall"),
         value = value - 273.15,
         scenario = "rcp85",                 
         model="ensemblemin")
ENSMIN_H_SST <- loadCMIP5("tos", "ensemblemin", "hist", path=file.path(paths$climate, "Marine_CMIP5"), verbose=T, yearRange=c(1980, 2010))%>%
  filterDimensions( lonRange=c(210, 245), latRange=c(45, 60), verbose=T) %>%
  as.data.frame() %>% 
  mutate(year = floor(time),
         month = floor((time - floor(time)) * 12 + 1),
         season = case_when(
           month %in% c(1, 2, 3) ~ "Winter",
           month %in% c(4, 5, 6) ~ "Spring",
           month %in% c(7, 8, 9) ~ "Summer",
           month %in% c(10, 11, 12) ~ "Fall"),
         value = value - 273.15,
         scenario = "historical",                 
         model="ensemblemin")

ENSMAX_45_SST <- loadCMIP5("tos", "ensemblemax", "rcp45", path=file.path(paths$climate, "Marine_CMIP5"), verbose=T, yearRange=c(2040, 2070))%>%
  filterDimensions( lonRange=c(210, 245), latRange=c(45, 60), verbose=T) %>%
  as.data.frame() %>% 
  mutate(year = floor(time),
         month = floor((time - floor(time)) * 12 + 1),
         season = case_when(
           month %in% c(1, 2, 3) ~ "Winter",
           month %in% c(4, 5, 6) ~ "Spring",
           month %in% c(7, 8, 9) ~ "Summer",
           month %in% c(10, 11, 12) ~ "Fall"),
         value = value - 273.15,
         scenario = "rcp45",                 
         model="ensemblemax")
ENSMAX_85_SST <- loadCMIP5("tos", "ensemblemax", "rcp85", path=file.path(paths$climate, "Marine_CMIP5"), verbose=T, yearRange=c(2040, 2070))%>%
  filterDimensions( lonRange=c(210, 245), latRange=c(45, 60), verbose=T) %>%
  as.data.frame() %>% 
  mutate(year = floor(time),
         month = floor((time - floor(time)) * 12 + 1),
         season = case_when(
           month %in% c(1, 2, 3) ~ "Winter",
           month %in% c(4, 5, 6) ~ "Spring",
           month %in% c(7, 8, 9) ~ "Summer",
           month %in% c(10, 11, 12) ~ "Fall"),
         value = value - 273.15,
         scenario = "rcp85",                 
         model="ensemblemax")
ENSMAX_H_SST <- loadCMIP5("tos", "ensemblemax", "hist", path=file.path(paths$climate, "Marine_CMIP5"), verbose=T, yearRange=c(1980, 2010))%>%
  filterDimensions( lonRange=c(210, 245), latRange=c(45, 60), verbose=T) %>%
  as.data.frame() %>% 
  mutate(year = floor(time),
         month = floor((time - floor(time)) * 12 + 1),
         season = case_when(
           month %in% c(1, 2, 3) ~ "Winter",
           month %in% c(4, 5, 6) ~ "Spring",
           month %in% c(7, 8, 9) ~ "Summer",
           month %in% c(10, 11, 12) ~ "Fall"),
         value = value - 273.15,
         scenario = "historical",                 
         model="ensemblemax")

CANESM_45_SST <- loadCMIP5("tos", "CanESM2", "rcp45", path=file.path(paths$climate, "Marine_CMIP5"), verbose=T, yearRange=c(2040, 2070))%>%
  filterDimensions( lonRange=c(210, 245), latRange=c(45, 60), verbose=T) %>%
  as.data.frame() %>% 
  mutate(year = floor(time),
         month = floor((time - floor(time)) * 12 + 1),
         season = case_when(
           month %in% c(1, 2, 3) ~ "Winter",
           month %in% c(4, 5, 6) ~ "Spring",
           month %in% c(7, 8, 9) ~ "Summer",
           month %in% c(10, 11, 12) ~ "Fall"),
         value = value - 273.15,
         scenario = "rcp45",                 
         model="CanESM2")
CANESM_85_SST <- loadCMIP5("tos", "CanESM2", "rcp85", path=file.path(paths$climate, "Marine_CMIP5"), verbose=T, yearRange=c(2040, 2070))%>%
  filterDimensions( lonRange=c(210, 245), latRange=c(45, 60), verbose=T) %>%
  as.data.frame() %>% 
  mutate(year = floor(time),
         month = floor((time - floor(time)) * 12 + 1),
         season = case_when(
           month %in% c(1, 2, 3) ~ "Winter",
           month %in% c(4, 5, 6) ~ "Spring",
           month %in% c(7, 8, 9) ~ "Summer",
           month %in% c(10, 11, 12) ~ "Fall"),
         value = value - 273.15,
         scenario = "rcp85",                 
         model="CanESM2")
CANESM_H_SST <- loadCMIP5("tos", "CanESM2", "hist", path=file.path(paths$climate, "Marine_CMIP5"), verbose=T, yearRange=c(1980, 2010))%>%
  filterDimensions( lonRange=c(210, 245), latRange=c(45, 60), verbose=T) %>%
  as.data.frame() %>% 
  mutate(year = floor(time),
         month = floor((time - floor(time)) * 12 + 1),
         season = case_when(
           month %in% c(1, 2, 3) ~ "Winter",
           month %in% c(4, 5, 6) ~ "Spring",
           month %in% c(7, 8, 9) ~ "Summer",
           month %in% c(10, 11, 12) ~ "Fall" ),
         value = value - 273.15,
         scenario = "historical",                 
         model="CanESM2")

MPIESM_45_SST <- loadCMIP5("tos", "MPI-ESM-LR", "rcp45", path=file.path(paths$climate, "Marine_CMIP5"), verbose=T, yearRange=c(2040, 2070))%>%
  filterDimensions( lonRange=c(210, 245), latRange=c(45, 60), verbose=T) %>%
  as.data.frame() %>% 
  mutate(year = floor(time),
         month = floor((time - floor(time)) * 12 + 1),
         season = case_when(
           month %in% c(1, 2, 3) ~ "Winter",
           month %in% c(4, 5, 6) ~ "Spring",
           month %in% c(7, 8, 9) ~ "Summer",
           month %in% c(10, 11, 12) ~ "Fall"),
         value = value - 273.15,
         scenario = "rcp45",                 
         model="MPI-ESM-LR")
MPIESM_85_SST <- loadCMIP5("tos", "MPI-ESM-LR", "rcp85", path=file.path(paths$climate, "Marine_CMIP5"), verbose=T, yearRange=c(2040, 2070))%>%
  filterDimensions( lonRange=c(210, 245), latRange=c(45, 60), verbose=T) %>%
  as.data.frame() %>% 
  mutate(year = floor(time),
         month = floor((time - floor(time)) * 12 + 1),
         season = case_when(
           month %in% c(1, 2, 3) ~ "Winter",
           month %in% c(4, 5, 6) ~ "Spring",
           month %in% c(7, 8, 9) ~ "Summer",
           month %in% c(10, 11, 12) ~ "Fall"),
         value = value - 273.15,
         scenario = "rcp85",                 
         model="MPI-ESM-LR")
MPIESM_H_SST <- loadCMIP5("tos", "MPI-ESM-LR", "historical", path=file.path(paths$climate, "Marine_CMIP5"), verbose=T, yearRange=c(1980, 2005))%>%
  filterDimensions( lonRange=c(210, 245), latRange=c(45, 60), verbose=T)%>%
  as.data.frame() %>% 
  mutate(year = floor(time),
         month = floor((time - floor(time)) * 12 + 1),
         season = case_when(
           month %in% c(1, 2, 3) ~ "Winter",
           month %in% c(4, 5, 6) ~ "Spring",
           month %in% c(7, 8, 9) ~ "Summer",
           month %in% c(10, 11, 12) ~ "Fall"),
         value = value - 273.15,
         scenario = "historical",                 
         model="MPI-ESM-LR")

IPSLCM_45_SST <- loadCMIP5("tos", "IPSL-CM5A-LR", "rcp45", path=file.path(paths$climate, "Marine_CMIP5"), verbose=T, yearRange=c(2040, 2070))%>%
  filterDimensions( lonRange=c(210, 245), latRange=c(45, 60), verbose=T) %>%
  as.data.frame() %>% 
  mutate(year = floor(time),
         month = floor((time - floor(time)) * 12 + 1),
         season = case_when(
           month %in% c(1, 2, 3) ~ "Winter",
           month %in% c(4, 5, 6) ~ "Spring",
           month %in% c(7, 8, 9) ~ "Summer",
           month %in% c(10, 11, 12) ~ "Fall"
         ),
         value = value - 273.15,
         scenario = "rcp45",                 
         model="IPSL-CM5A-LR")
IPSLCM_85_SST <- loadCMIP5("tos", "IPSL-CM5A-LR", "rcp85", path=file.path(paths$climate, "Marine_CMIP5"), verbose=T, yearRange=c(2040, 2070))%>%
  filterDimensions( lonRange=c(210, 245), latRange=c(45, 60), verbose=T) %>%
  as.data.frame() %>% 
  mutate(year = floor(time),
         month = floor((time - floor(time)) * 12 + 1),
         season = case_when(
           month %in% c(1, 2, 3) ~ "Winter",
           month %in% c(4, 5, 6) ~ "Spring",
           month %in% c(7, 8, 9) ~ "Summer",
           month %in% c(10, 11, 12) ~ "Fall"
         ),
         value = value - 273.15,
         scenario = "rcp85",                 
         model="IPSL-CM5A-LR")
IPSLCM_H_SST <- loadCMIP5("tos", "IPSL-CM5A-LR", "historical", path=file.path(paths$climate, "Marine_CMIP5"), verbose=T, yearRange=c(1980, 2010))%>%
  filterDimensions( lonRange=c(210, 245), latRange=c(45, 60), verbose=T) %>%
  as.data.frame() %>% 
  mutate(year = floor(time),
         month = floor((time - floor(time)) * 12 + 1),
         season = case_when(
           month %in% c(1, 2, 3) ~ "Winter",
           month %in% c(4, 5, 6) ~ "Spring",
           month %in% c(7, 8, 9) ~ "Summer",
           month %in% c(10, 11, 12) ~ "Fall"),
         value = value - 273.15,
         scenario = "historical",                 
         model="IPSL-CM5A-LR")


# Join the climate model scenario data together 
ENSMED_SST <- bind_rows(ENSMED_H_SST, ENSMED_45_SST, ENSMED_85_SST)
ENSMAX_SST <- bind_rows(ENSMAX_H_SST, ENSMAX_45_SST, ENSMAX_85_SST)
ENSMIN_SST <- bind_rows(ENSMIN_H_SST, ENSMIN_45_SST, ENSMIN_85_SST)
CANESM_SST <- bind_rows(CANESM_H_SST, CANESM_45_SST, CANESM_85_SST)
MPIESM_SST <- bind_rows(MPIESM_H_SST, MPIESM_45_SST, MPIESM_85_SST)
IPSLCM_SST <- bind_rows(IPSLCM_H_SST, IPSLCM_45_SST, IPSLCM_85_SST)

CMIP5_SST<- bind_rows(ENSMED_SST, ENSMAX_SST,ENSMIN_SST,CANESM_SST,MPIESM_SST, IPSLCM_SST)

# This is how I can get it to work
# Turn the data frame into a spatial feature by establishing the coordinate data 
CMIP5_SST<- CMIP5_SST %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) #%>%
#st_transform(crs = "EPSG:3005") 
coords <- st_coordinates(CMIP5_SST) #  Ran this first Take the coordinates from SF and put it in coords object 
coords[, "Y"] <- abs(coords[, "Y"]) 
coords[,"X"] <- coords[,"X"] - 360
new_geom <- st_sfc(lapply(1:nrow(coords), function(i) st_point(coords[i, ])), crs = st_crs(CMIP5_SST))
CMIP5_SST<- st_set_geometry(CMIP5_SST, new_geom)
sf::st_write(CMIP5_SST, file.path(paths$climate, "Standardized_Marine_data/CMIP5_SST.gdb"),   driver = "OpenFileGDB", append = FALSE )

#remove extra variables
rm(ENSMED_H_SST, ENSMED_45_SST, ENSMED_85_SST, ENSMAX_H_SST, ENSMAX_45_SST, ENSMAX_85_SST,
ENSMIN_H_SST, ENSMIN_45_SST, ENSMIN_85_SST, CANESM_H_SST, CANESM_45_SST, CANESM_85_SST,
MPIESM_H_SST, MPIESM_45_SST, MPIESM_85_SST, IPSLCM_H_SST, IPSLCM_45_SST, IPSLCM_85_SST)

# Calculate the SST monthly average for each location and each scenario 
SST_avg <- SST %>%
  group_by(lon, lat, model, scenario, season, month) %>%
  summarise(mean = mean(value, na.rm = TRUE)) %>%
  ungroup()


# calculate the monthly average for each scenario (n= 36)
SST_month <- SST_avg %>%
  group_by(scenario, season, month) %>%
  summarise(average = mean(mean, na.rm = TRUE),
            q05 = quantile(mean, probs = 0.05, na.rm = TRUE),
            q95 = quantile(mean, probs = 0.95, na.rm = TRUE)) %>%
  ungroup()

# Calculate the seasonal average for each scenario (n=12)
SST_seasons <- SST_avg %>%
  group_by(scenario, season) %>%
  summarise(average = mean(mean, na.rm = TRUE),
            q05 = quantile(mean, probs = 0.05, na.rm = TRUE),
            q95 = quantile(mean, probs = 0.95, na.rm = TRUE)) %>%
  ungroup()

# Calcualte the average for each scenario (n=3)
SST_annual <- SST_avg %>%
  group_by(scenario) %>%
  summarise(average = mean(mean, na.rm = TRUE),
            q05 = quantile(mean, probs = 0.05, na.rm = TRUE),
            q95 = quantile(mean, probs = 0.95, na.rm = TRUE)) %>%
  ungroup()

# Turn the data frame into a spatial feature by establishing the coordinate data 
SST_sf <- SST_df %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) #%>%
  #st_transform(crs = "EPSG:3005") 

# Not sure what this is doing, not working for me
coords[, "Y"] <- abs(coords[, "Y"]) # Ran this second should take the absolute values of Y 
  coords <- st_coordinates(SST_sf) #  Ran this first Take the coordinates from SF and put it in coords object 
st_coordinates(SST_sf)[,"X"] <- st_coordinates(SST_sf)[,"X"] - 360 # Not working for me but 
coords[,"X"] <- coords[,"X"] - 360
new_geom <- st_sfc(lapply(1:nrow(coords), function(i) st_point(coords[i, ])), crs = st_crs(SST_sf))
SST_sf <- st_set_geometry(SST_sf, new_geom)
# SST_sf_spr <- filter(SST_sf, month >= 4 & month <= 6) %>%
#   group_by(lon, lat) %>%
#   summarise(mean = mean(mean, na.rm = TRUE)) %>%
#   ungroup() 

# This is how I can get it to work
coords <- st_coordinates(SST_sf) #  Ran this first Take the coordinates from SF and put it in coords object 
coords[, "Y"] <- abs(coords[, "Y"]) 
coords[,"X"] <- coords[,"X"] - 360
new_geom <- st_sfc(lapply(1:nrow(coords), function(i) st_point(coords[i, ])), crs = st_crs(SST_sf))
SST_sf <- st_set_geometry(SST_sf, new_geom)

# this isnt working for me because SST_mcm does not exist earlier in the script 
SST_raster <- st_rasterize(SST_mcm) #%>%
  st_set_dimensions(3, name = "month", values = month)

#SST_means_sf <- st_as_sf(SST_annual, coords = c("lon", "lat"), crs = 4326)

bc_coast_4326 <- st_transform(bc_coast, crs = "EPSG:4326")
  
##----- Simple plots ----------------#
# this one is working. April values for year 2050
ggplot(filter(SST_sf, month == 4, year == 2050)) +
  geom_sf(aes(colour = value), size = 4) +
  geom_sf(data = bc_coast, fill = NA, colour = "black") +
  scale_colour_viridis_c()

ggplot(SST_avg_all, aes(x = month, y = average, ymin = q05, ymax = q95, colour = scenario)) +
  geom_line() +
  geom_point() +
  geom_ribbon(alpha = 0.2) +
  labs(title = "CMIP5 SST Lat 45-60, Lon 180-240", x = "Month", y = "Mean Temperature (C)", colour = "Scenario") +
  theme(legend.position = "bottom")

SST_avg <- SST_avg %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)
coords <- st_coordinates(SST_avg) 
coords[, "Y"] <- abs(coords[, "Y"]) 
coords[,"X"] <- coords[,"X"] - 360
new_geom <- st_sfc(lapply(1:nrow(coords), function(i) st_point(coords[i, ])), crs = st_crs(SST_avg))
SST_avg <- st_set_geometry(SST_avg, new_geom)

ggplot(filter(SST_avg, month == 4)) +
  geom_sf(aes(colour = mean), size = 4) +
  geom_sf(data = bc_coast, fill = NA, colour = "black") +
  scale_colour_viridis_c()

#------------ Save CMIP5 data as shp file -------
#sf::st_write(CMIP_H_SST, file.path(paths$climate, "Standardized_Marine_data/CanESM2_H_SST.gdb"),   driver = "OpenFileGDB" )
#sf::st_write(CMIP_45_SST, file.path(paths$climate, "Standardized_Marine_data/CanESM2_P_SST.gdb"),   driver = "OpenFileGDB" )
#sf::st_write(CMIP_85_SST, file.path(paths$climate, "Standardized_Marine_data/CanESM2_P_SST.gdb"),   driver = "OpenFileGDB" )

sf::st_write(CMIP5_SST, file.path(paths$climate, "Standardized_Marine_data/CMIP5_SST.gdb"),   driver = "OpenFileGDB", append = FALSE )


#---------------#---------------FALSE



# 
# ### Process without a package - no longer being used
# ############# LOAD CMIP 5 Data ###########
# #CMIP_H_SST<- read_ncdf(file.path(climate_dat, "Marine_CMIP5", "tos_Omon_ensemblemedian_hist_r1i1p1_190001-200512.nc"), proxy = FALSE, var = c("lat","lon","tos"), make_time = TRUE)
# CMIP_H_SST<-nc_open(file.path(climate_dat, "Marine_CMIP5", "tos_Omon_CanESM2_hist_r1i1p1_190001-200512.nc"))
# CMIP_45_SST<-nc_open(file.path(climate_dat, "Marine_CMIP5", "tos_Omon_CanESM2_rcp45_r1i1p1_200601-210012.nc"))
# CMIP_85_SST<-nc_open(file.path(climate_dat, "Marine_CMIP5", "tos_Omon_CanESM2_rcp85_r1i1p1_200601-210012.nc"))
# 
# #Read in the temperature and salinity variable for each scenario
# TH<-ncvar_get(CMIP_H_SST, "tos", verbose = FALSE)
# T45<-ncvar_get(CMIP_45_SST, "tos", verbose = FALSE)
# T85<-ncvar_get(CMIP_85_SST, "tos", verbose = FALSE)
# 
# # Compare the results from these two functions tofigure out what order the dimensions are in. 
# # here the order is [lon, lat, time]
# dim(TH)   # The time size is 1140, meaning there is a month avg for each year 1140/12= 95 (number of yrs between 1900 and 2005)
# dim(T85)  # The time size is 1272, meaning there is a month avg for each year 1272/12= 106 (number of yrs between 2005 and 2100)
# #dim(TH2)
# #dim(TimeBnds)
# 
# #Read in lat and long from one of the files, they are the same for all of them
# lon <- ncvar_get(CMIP_H_SST, "lon", verbose = FALSE) 
# lat <- ncvar_get(CMIP_H_SST, "lat", verbose = FALSE) 
# 
# ##figure out what value is used to define no data. This is the same value for salinity and temperature 
# fillvalue <- ncatt_get(CMIP_H_SST, "tos", "_FillValue")
# 
# #replace fill values with standard NA value
# TH[TH == fillvalue$value] <- NA
# T45[T45 == fillvalue$value] <- NA
# T85[T85 == fillvalue$value] <- NA
# 
# #extract surface data (3rd dimension) for each month (4th dimension) as vectors
# CMIPlon<-as.vector(lon)
# CMIPlat<-as.vector(lat)
# 
# ## create historical spatial object
# n_month <- dim(TH)[3]
# 
# vec_month <- rep(seq(1, 12), n_month / 12) # This is the sequence of months in the time dimension
# vec_year <- rep(seq(1900, 2005, by = 1), 64800) %>%
#   sort() # This is the sequence of years in the time dimension
# 
# #recast the 3D array into a matrix, one column for each month
# temp <- data.frame(matrix(TH, nrow=dim(TH)[3], byrow = TRUE))
# 
# SST_H_01 <- temp[vec_month ==1,]  %>% as.vector() 
# SST_H_02 <- temp[vec_month ==2,] %>% t() %>% as.vector()
# SST_H_03 <- temp[vec_month ==3,]%>% t() %>% as.vector()
# SST_H_04 <- temp[vec_month ==4,]%>% t() %>% as.vector()
# SST_H_05 <- temp[vec_month ==5,]%>% t() %>% as.vector()
# SST_H_06 <- temp[vec_month ==6,]%>% t() %>% as.vector()
# SST_H_07 <- temp[vec_month ==7,]%>% t() %>% as.vector()
# SST_H_08 <- temp[vec_month ==8,]%>% t() %>% as.vector()
# SST_H_09 <- temp[vec_month ==9,]%>% t() %>% as.vector()
# SST_H_10 <- temp[vec_month ==10,]%>% t() %>% as.vector()
# SST_H_11 <- temp[vec_month ==11,]%>% t() %>% as.vector()
# SST_H_12 <- temp[vec_month ==12,]%>% t() %>% as.vector()
# 
# CMIP_H_SST <- data.frame(x = CMIPlon, y = CMIPlat, year = vec_year,
#                          SST_H_01, SST_H_02, SST_H_03, SST_H_04, SST_H_05, SST_H_06,
#                          SST_H_07, SST_H_08, SST_H_09, SST_H_10, SST_H_11, SST_H_12) %>%
#   st_as_sf(coords = c("x", "y"),
#            crs = "EPSG:4326") #%>%
# st_crop(xmin = 120, xmax = 270, ymin = 30, ymax = 80)
# 
# #create projections spatial object
# 
# n_month <- dim(T45)[3]
# vec_month <- rep(seq(1, 12), n_month / 12) # This is the sequence of months in the time dimension
# vec_year <- rep(seq(2006, 2100, by = 1), 64800) %>%
#   sort() # This is the sequence of years in the time dimension
# 
# #recast the 3D array into a matrix, one column for each month
# temp <- data.frame(matrix(T45, nrow=dim(T45)[3], byrow = TRUE))
# 
# SST_45_01 <- temp[vec_month ==1,] %>% t() %>% as.vector()
# SST_45_02 <- temp[vec_month ==2,] %>% t() %>% as.vector()
# SST_45_03 <- temp[vec_month ==3,]%>% t() %>% as.vector()
# SST_45_04 <- temp[vec_month ==4,]%>% t() %>% as.vector()
# SST_45_05 <- temp[vec_month ==5,]%>% t() %>% as.vector()
# SST_45_06 <- temp[vec_month ==6,]%>% t() %>% as.vector()
# SST_45_07 <- temp[vec_month ==7,]%>% t() %>% as.vector()
# SST_45_08 <- temp[vec_month ==8,]%>% t() %>% as.vector()
# SST_45_09 <- temp[vec_month ==9,]%>% t() %>% as.vector()
# SST_45_10 <- temp[vec_month ==10,]%>% t() %>% as.vector()
# SST_45_11 <- temp[vec_month ==11,]%>% t() %>% as.vector()
# SST_45_12 <- temp[vec_month ==12,]%>% t() %>% as.vector()
# 
# #recast the 3D array into a matrix, one column for each month
# temp <- data.frame(matrix(T85, nrow=dim(T85)[3], byrow = TRUE))
# 
# SST_85_01 <- temp[vec_month ==1,] %>% t() %>% as.vector()
# SST_85_02 <- temp[vec_month ==2,] %>% t() %>% as.vector()
# SST_85_03 <- temp[vec_month ==3,]%>% t() %>% as.vector()
# SST_85_04 <- temp[vec_month ==4,]%>% t() %>% as.vector()
# SST_85_05 <- temp[vec_month ==5,]%>% t() %>% as.vector()
# SST_85_06 <- temp[vec_month ==6,]%>% t() %>% as.vector()
# SST_85_07 <- temp[vec_month ==7,]%>% t() %>% as.vector()
# SST_85_08 <- temp[vec_month ==8,]%>% t() %>% as.vector()
# SST_85_09 <- temp[vec_month ==9,]%>% t() %>% as.vector()
# SST_85_10 <- temp[vec_month ==10,]%>% t() %>% as.vector()
# SST_85_11 <- temp[vec_month ==11,]%>% t() %>% as.vector()
# SST_85_12 <- temp[vec_month ==12,]%>% t() %>% as.vector()
# 
# 
# CMIP_P_SST <- data.frame(x = CMIPlon, y = CMIPlat, year = vec_year,
#                          SST_45_01, SST_45_02, SST_45_03, SST_45_04, SST_45_05, SST_45_06,
#                          SST_45_07, SST_45_08, SST_45_09, SST_45_10, SST_45_11, SST_45_12,
#                          SST_85_01, SST_85_02, SST_85_03, SST_85_04, SST_85_05, SST_85_06,
#                          SST_85_07, SST_85_08, SST_85_09, SST_85_10, SST_85_11, SST_85_12) %>%
#   st_as_sf(coords = c("x", "y"),
#            crs = "EPSG:4326") %>%
#   st_crop(., xmin = 120, xmax = 270, ymin = 30, ymax = 80)

