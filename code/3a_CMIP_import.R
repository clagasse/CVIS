
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

# group the CMIP 5  models into variables 
SST_files <- filter(C5_files, variable == "tos")
SSS_files <-filter(C5_files, variable== "sos")
SSpH_files<- filter(C5_files, variable=="ph")

# Check the time frames of the data 
checkTimePeriod(SST_files)  

######### Load models and convert to data frames###########
####### Load Temperature models and convert to data frames  ############
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

########### SSS models ############
ENSMED_45_SSS <- loadCMIP5("sos", "ensemblemedian", "rcp45",                
                           path=file.path(paths$climate, "Marine_CMIP5"),    
                           verbose=T, yearRange=c(2040, 2070))%>%           
  filterDimensions(lonRange=c(210,245),latRange=c(45,60),verbose=T) %>%     
  as.data.frame() %>%     # Creating a data frame
  mutate(year = floor(time),                             
         month = floor((time - floor(time)) * 12 + 1),   
         season = case_when(                             
           month %in% c(1, 2, 3) ~ "Winter",
           month %in% c(4, 5, 6) ~ "Spring",
           month %in% c(7, 8, 9) ~ "Summer",
           month %in% c(10, 11, 12) ~ "Fall"),
         value = value,            
         scenario = "rcp45",                  
         model="ensemblemedian")                             

ENSMED_85_SSS <- loadCMIP5("sos", "ensemblemedian", "rcp85", path=file.path(paths$climate, "Marine_CMIP5"), verbose=T, yearRange=c(2040, 2070))%>%
  filterDimensions( lonRange=c(210, 245), latRange=c(45, 60), verbose=T) %>%
  as.data.frame() %>% 
  mutate(year = floor(time),
         month = floor((time - floor(time)) * 12 + 1),
         season = case_when(
           month %in% c(1, 2, 3) ~ "Winter",
           month %in% c(4, 5, 6) ~ "Spring",
           month %in% c(7, 8, 9) ~ "Summer",
           month %in% c(10, 11, 12) ~ "Fall"),
         value = value,
         scenario = "rcp85",                 
         model="ensemblemedian")

ENSMED_H_SSS <- loadCMIP5("sos", "ensemblemedian", "hist", path=file.path(paths$climate, "Marine_CMIP5"), verbose=T, yearRange=c(1980, 2010))%>%
  filterDimensions( lonRange=c(210, 245), latRange=c(45, 60), verbose=T) %>%
  as.data.frame() %>% 
  mutate(year = floor(time),
         month = floor((time - floor(time)) * 12 + 1),
         season = case_when(
           month %in% c(1, 2, 3) ~ "Winter",
           month %in% c(4, 5, 6) ~ "Spring",
           month %in% c(7, 8, 9) ~ "Summer",
           month %in% c(10, 11, 12) ~ "Fall"),
         value = value ,
         scenario = "historical",                 
         model="ensemblemedian")
ENSMIN_45_SSS<- loadCMIP5("sos", "ensemblemin", "rcp45", path=file.path(paths$climate, "Marine_CMIP5"), verbose=T, yearRange=c(2040, 2070))%>%
  filterDimensions( lonRange=c(210, 245), latRange=c(45, 60), verbose=T) %>%
  as.data.frame() %>% 
  mutate(year = floor(time),
         month = floor((time - floor(time)) * 12 + 1),
         season = case_when(
           month %in% c(1, 2, 3) ~ "Winter",
           month %in% c(4, 5, 6) ~ "Spring",
           month %in% c(7, 8, 9) ~ "Summer",
           month %in% c(10, 11, 12) ~ "Fall"),
         value = value ,
         scenario = "rcp45",                 
         model="ensemblemin")
ENSMIN_85_SSS <- loadCMIP5("sos", "ensemblemin", "rcp85", path=file.path(paths$climate, "Marine_CMIP5"), verbose=T, yearRange=c(2040, 2070))%>%
  filterDimensions( lonRange=c(210, 245), latRange=c(45, 60), verbose=T) %>%
  as.data.frame() %>% 
  mutate(year = floor(time),
         month = floor((time - floor(time)) * 12 + 1),
         season = case_when(
           month %in% c(1, 2, 3) ~ "Winter",
           month %in% c(4, 5, 6) ~ "Spring",
           month %in% c(7, 8, 9) ~ "Summer",
           month %in% c(10, 11, 12) ~ "Fall"),
         value = value ,
         scenario = "rcp85",                 
         model="ensemblemin")
ENSMIN_H_SSS <- loadCMIP5("sos", "ensemblemin", "hist", path=file.path(paths$climate, "Marine_CMIP5"), verbose=T, yearRange=c(1980, 2010))%>%
  filterDimensions( lonRange=c(210, 245), latRange=c(45, 60), verbose=T) %>%
  as.data.frame() %>% 
  mutate(year = floor(time),
         month = floor((time - floor(time)) * 12 + 1),
         season = case_when(
           month %in% c(1, 2, 3) ~ "Winter",
           month %in% c(4, 5, 6) ~ "Spring",
           month %in% c(7, 8, 9) ~ "Summer",
           month %in% c(10, 11, 12) ~ "Fall"),
         value = value ,
         scenario = "historical",                 
         model="ensemblemin")

ENSMAX_45_SSS <- loadCMIP5("sos", "ensemblemax", "rcp45", path=file.path(paths$climate, "Marine_CMIP5"), verbose=T, yearRange=c(2040, 2070))%>%
  filterDimensions( lonRange=c(210, 245), latRange=c(45, 60), verbose=T) %>%
  as.data.frame() %>% 
  mutate(year = floor(time),
         month = floor((time - floor(time)) * 12 + 1),
         season = case_when(
           month %in% c(1, 2, 3) ~ "Winter",
           month %in% c(4, 5, 6) ~ "Spring",
           month %in% c(7, 8, 9) ~ "Summer",
           month %in% c(10, 11, 12) ~ "Fall"),
         value = value ,
         scenario = "rcp45",                 
         model="ensemblemax")
ENSMAX_85_SSS <- loadCMIP5("sos", "ensemblemax", "rcp85", path=file.path(paths$climate, "Marine_CMIP5"), verbose=T, yearRange=c(2040, 2070))%>%
  filterDimensions( lonRange=c(210, 245), latRange=c(45, 60), verbose=T) %>%
  as.data.frame() %>% 
  mutate(year = floor(time),
         month = floor((time - floor(time)) * 12 + 1),
         season = case_when(
           month %in% c(1, 2, 3) ~ "Winter",
           month %in% c(4, 5, 6) ~ "Spring",
           month %in% c(7, 8, 9) ~ "Summer",
           month %in% c(10, 11, 12) ~ "Fall"),
         value = value ,
         scenario = "rcp85",                 
         model="ensemblemax")
ENSMAX_H_SSS <- loadCMIP5("sos", "ensemblemax", "hist", path=file.path(paths$climate, "Marine_CMIP5"), verbose=T, yearRange=c(1980, 2010))%>%
  filterDimensions( lonRange=c(210, 245), latRange=c(45, 60), verbose=T) %>%
  as.data.frame() %>% 
  mutate(year = floor(time),
         month = floor((time - floor(time)) * 12 + 1),
         season = case_when(
           month %in% c(1, 2, 3) ~ "Winter",
           month %in% c(4, 5, 6) ~ "Spring",
           month %in% c(7, 8, 9) ~ "Summer",
           month %in% c(10, 11, 12) ~ "Fall"),
         value = value ,
         scenario = "historical",                 
         model="ensemblemax")

########### SSPH models ############
ENSMED_45_SSPH <- loadCMIP5("ph", "ensemblemedian", "rcp45",                
                           path=file.path(paths$climate, "Marine_CMIP5"),    
                           verbose=T, yearRange=c(2040, 2070))%>%           
  filterDimensions(lonRange=c(210,245),latRange=c(45,60),verbose=T) %>%     
  as.data.frame() %>%     # Creating a data frame
  mutate(year = floor(time),                             
         month = floor((time - floor(time)) * 12 + 1),   
         season = case_when(                             
           month %in% c(1, 2, 3) ~ "Winter",
           month %in% c(4, 5, 6) ~ "Spring",
           month %in% c(7, 8, 9) ~ "Summer",
           month %in% c(10, 11, 12) ~ "Fall"),
         value = value,            
         scenario = "rcp45",                  
         model="ensemblemedian")                             

ENSMED_85_SSPH <- loadCMIP5("ph", "ensemblemedian", "rcp85", path=file.path(paths$climate, "Marine_CMIP5"), verbose=T, yearRange=c(2040, 2070))%>%
  filterDimensions( lonRange=c(210, 245), latRange=c(45, 60), verbose=T) %>%
  as.data.frame() %>% 
  mutate(year = floor(time),
         month = floor((time - floor(time)) * 12 + 1),
         season = case_when(
           month %in% c(1, 2, 3) ~ "Winter",
           month %in% c(4, 5, 6) ~ "Spring",
           month %in% c(7, 8, 9) ~ "Summer",
           month %in% c(10, 11, 12) ~ "Fall"),
         value = value,
         scenario = "rcp85",                 
         model="ensemblemedian")

ENSMED_H_SSPH <- loadCMIP5("ph", "ensemblemedian", "hist", path=file.path(paths$climate, "Marine_CMIP5"), verbose=T, yearRange=c(1980, 2010))%>%
  filterDimensions( lonRange=c(210, 245), latRange=c(45, 60), verbose=T) %>%
  as.data.frame() %>% 
  mutate(year = floor(time),
         month = floor((time - floor(time)) * 12 + 1),
         season = case_when(
           month %in% c(1, 2, 3) ~ "Winter",
           month %in% c(4, 5, 6) ~ "Spring",
           month %in% c(7, 8, 9) ~ "Summer",
           month %in% c(10, 11, 12) ~ "Fall"),
         value = value ,
         scenario = "historical",                 
         model="ensemblemedian")
ENSMIN_45_SSPH<- loadCMIP5("ph", "ensemblemin", "rcp45", path=file.path(paths$climate, "Marine_CMIP5"), verbose=T, yearRange=c(2040, 2070))%>%
  filterDimensions( lonRange=c(210, 245), latRange=c(45, 60), verbose=T) %>%
  as.data.frame() %>% 
  mutate(year = floor(time),
         month = floor((time - floor(time)) * 12 + 1),
         season = case_when(
           month %in% c(1, 2, 3) ~ "Winter",
           month %in% c(4, 5, 6) ~ "Spring",
           month %in% c(7, 8, 9) ~ "Summer",
           month %in% c(10, 11, 12) ~ "Fall"),
         value = value ,
         scenario = "rcp45",                 
         model="ensemblemin")
ENSMIN_85_SSPH <- loadCMIP5("ph", "ensemblemin", "rcp85", path=file.path(paths$climate, "Marine_CMIP5"), verbose=T, yearRange=c(2040, 2070))%>%
  filterDimensions( lonRange=c(210, 245), latRange=c(45, 60), verbose=T) %>%
  as.data.frame() %>% 
  mutate(year = floor(time),
         month = floor((time - floor(time)) * 12 + 1),
         season = case_when(
           month %in% c(1, 2, 3) ~ "Winter",
           month %in% c(4, 5, 6) ~ "Spring",
           month %in% c(7, 8, 9) ~ "Summer",
           month %in% c(10, 11, 12) ~ "Fall"),
         value = value ,
         scenario = "rcp85",                 
         model="ensemblemin")
ENSMIN_H_SSPH <- loadCMIP5("ph", "ensemblemin", "hist", path=file.path(paths$climate, "Marine_CMIP5"), verbose=T, yearRange=c(1980, 2010))%>%
  filterDimensions( lonRange=c(210, 245), latRange=c(45, 60), verbose=T) %>%
  as.data.frame() %>% 
  mutate(year = floor(time),
         month = floor((time - floor(time)) * 12 + 1),
         season = case_when(
           month %in% c(1, 2, 3) ~ "Winter",
           month %in% c(4, 5, 6) ~ "Spring",
           month %in% c(7, 8, 9) ~ "Summer",
           month %in% c(10, 11, 12) ~ "Fall"),
         value = value ,
         scenario = "historical",                 
         model="ensemblemin")

ENSMAX_45_SSPH <- loadCMIP5("ph", "ensemblemax", "rcp45", path=file.path(paths$climate, "Marine_CMIP5"), verbose=T, yearRange=c(2040, 2070))%>%
  filterDimensions( lonRange=c(210, 245), latRange=c(45, 60), verbose=T) %>%
  as.data.frame() %>% 
  mutate(year = floor(time),
         month = floor((time - floor(time)) * 12 + 1),
         season = case_when(
           month %in% c(1, 2, 3) ~ "Winter",
           month %in% c(4, 5, 6) ~ "Spring",
           month %in% c(7, 8, 9) ~ "Summer",
           month %in% c(10, 11, 12) ~ "Fall"),
         value = value ,
         scenario = "rcp45",                 
         model="ensemblemax")
ENSMAX_85_SSPH <- loadCMIP5("ph", "ensemblemax", "rcp85", path=file.path(paths$climate, "Marine_CMIP5"), verbose=T, yearRange=c(2040, 2070))%>%
  filterDimensions( lonRange=c(210, 245), latRange=c(45, 60), verbose=T) %>%
  as.data.frame() %>% 
  mutate(year = floor(time),
         month = floor((time - floor(time)) * 12 + 1),
         season = case_when(
           month %in% c(1, 2, 3) ~ "Winter",
           month %in% c(4, 5, 6) ~ "Spring",
           month %in% c(7, 8, 9) ~ "Summer",
           month %in% c(10, 11, 12) ~ "Fall"),
         value = value ,
         scenario = "rcp85",                 
         model="ensemblemax")
ENSMAX_H_SSPH <- loadCMIP5("ph", "ensemblemax", "hist", path=file.path(paths$climate, "Marine_CMIP5"), verbose=T, yearRange=c(1980, 2010))%>%
  filterDimensions( lonRange=c(210, 245), latRange=c(45, 60), verbose=T) %>%
  as.data.frame() %>% 
  mutate(year = floor(time),
         month = floor((time - floor(time)) * 12 + 1),
         season = case_when(
           month %in% c(1, 2, 3) ~ "Winter",
           month %in% c(4, 5, 6) ~ "Spring",
           month %in% c(7, 8, 9) ~ "Summer",
           month %in% c(10, 11, 12) ~ "Fall"),
         value = value ,
         scenario = "historical",                 
         model="ensemblemax")

######### Combine CMIP5 data #########
ENSMED_SST <- bind_rows(ENSMED_H_SST, ENSMED_45_SST, ENSMED_85_SST)
ENSMAX_SST <- bind_rows(ENSMAX_H_SST, ENSMAX_45_SST, ENSMAX_85_SST)
ENSMIN_SST <- bind_rows(ENSMIN_H_SST, ENSMIN_45_SST, ENSMIN_85_SST)
#CANESM_SST <- bind_rows(CANESM_H_SST, CANESM_45_SST, CANESM_85_SST)
#MPIESM_SST <- bind_rows(MPIESM_H_SST, MPIESM_45_SST, MPIESM_85_SST)
#IPSLCM_SST <- bind_rows(IPSLCM_H_SST, IPSLCM_45_SST, IPSLCM_85_SST)

ENSMED_SSS <- bind_rows(ENSMED_H_SSS, ENSMED_45_SSS, ENSMED_85_SSS)
ENSMAX_SSS <- bind_rows(ENSMAX_H_SSS, ENSMAX_45_SSS, ENSMAX_85_SSS)
ENSMIN_SSS <- bind_rows(ENSMIN_H_SSS, ENSMIN_45_SSS, ENSMIN_85_SSS)

ENSMED_SSPH <- bind_rows(ENSMED_H_SSPH, ENSMED_45_SSPH, ENSMED_85_SSPH)
ENSMAX_SSPH <- bind_rows(ENSMAX_H_SSPH, ENSMAX_45_SSPH, ENSMAX_85_SSPH)
ENSMIN_SSPH <- bind_rows(ENSMIN_H_SSPH, ENSMIN_45_SSPH, ENSMIN_85_SSPH)

#CMIP5_SST <- bind_rows(ENSMED_SST,CANESM_SST,MPIESM_SST, IPSLCM_SST)
CMIP5_ENS_SST<- bind_rows(ENSMED_SST, ENSMAX_SST,ENSMIN_SST)
CMIP5_ENS_SSS<- bind_rows(ENSMED_SSS, ENSMAX_SSS,ENSMIN_SSS)
CMIP5_ENS_SSPH<- bind_rows(ENSMED_SSPH, ENSMAX_SSPH,ENSMIN_SSPH)

# turn CMIP5_SST into a spatial data frame 
#CMIP5_SST<- st_as_sf(CMIP5_SST,coords = c("lon", "lat"), crs = 4326)%>% st_transform( crs = "EPSG:3005") 
CMIP5_ENS_SST<- st_as_sf(CMIP5_ENS_SST,coords = c("lon", "lat"), crs = 4326)%>% st_transform( crs = "EPSG:3005") 
CMIP5_ENS_SSS<- st_as_sf(CMIP5_ENS_SSS,coords = c("lon", "lat"), crs = 4326)%>% st_transform( crs = "EPSG:3005")
CMIP5_ENS_SSPH<- st_as_sf(CMIP5_ENS_SSPH,coords = c("lon", "lat"), crs = 4326)%>% st_transform( crs = "EPSG:3005")

# Im not sure what this additional code does but the product is the same as above
coords <- st_coordinates(CMIP5_ENS_SST) #  Ran this first Take the coordinates from SF and put it in coords object 
coords[, "Y"] <- abs(coords[, "Y"]) 
coords[,"X"] <- coords[,"X"] - 360
new_geom <- st_sfc(lapply(1:nrow(coords), function(i) st_point(coords[i, ])), crs = st_crs(CMIP5_ENS_SST))
#CMIP5_SST<- st_set_geometry(CMIP5_SST, new_geom)
CMIP5_ENS_SST<- st_set_geometry(CMIP5_ENS_SST, new_geom)
CMIP5_ENS_SSS<- st_set_geometry(CMIP5_ENS_SSS, new_geom)
CMIP5_ENS_SSPH<- st_set_geometry(CMIP5_ENS_SSPH, new_geom)

#Plot data 
bc_coast_3005 <- st_transform(bc_coast, crs = "EPSG:3005")
#ggplot(filter(CMIP5_SST, month == 4, year == 2050)) +
#  geom_sf(aes(colour = value), size = 4) +
#  geom_sf(data = bc_coast, fill = NA, colour = "black") +
#  scale_colour_viridis_c()

# Join CMIP 5 data to MAZ
MAZ<-read_sf(file.path(paths$spatial, "MAZ", "MAZ_Final.shp")) %>% st_transform(,crs = "EPSG:3005" )
MAZ$MAZ_Acrony<- as.factor(MAZ$MAZ_Acrony)#Change MAZ acronym variable to a factor variable

CMIP5_ENS_SST<-st_join(CMIP5_ENS_SST, left = FALSE, MAZ["MAZ_Acrony"]) %>%
  sf::st_write(file.path(paths$climate, "Standardized_Marine_data/CMIP5_ENS_SST.gdb"),  driver = "OpenFileGDB",  append = FALSE )
CMIP5_ENS_SSS<-st_join(CMIP5_ENS_SSS, left = FALSE, MAZ["MAZ_Acrony"]) %>%
  sf::st_write(file.path(paths$climate, "Standardized_Marine_data/CMIP5_ENS_SSS.gdb"),  driver = "OpenFileGDB",  append = FALSE )
CMIP5_ENS_SSPH<-st_join(CMIP5_ENS_SSPH, left = FALSE, MAZ["MAZ_Acrony"]) %>%
  sf::st_write(file.path(paths$climate, "Standardized_Marine_data/CMIP5_ENS_SSPH.gdb"),  driver = "OpenFileGDB",  append = FALSE )

#CMIP5_SST<-st_join(CMIP5_SST, left = FALSE, MAZ["MAZ_Acrony"]) %>%
 # sf::st_write(file.path(paths$climate, "Standardized_Marine_data/CMIP5_SST.gdb"),  driver = "OpenFileGDB",  append = FALSE )

#remove extra variables
rm(ENSMED_H_SST, ENSMED_45_SST, ENSMED_85_SST, ENSMAX_H_SST, ENSMAX_45_SST, ENSMAX_85_SST,
   ENSMIN_H_SST, ENSMIN_45_SST, ENSMIN_85_SST, CANESM_H_SST, CANESM_45_SST, CANESM_85_SST,
   MPIESM_H_SST, MPIESM_45_SST, MPIESM_85_SST, IPSLCM_H_SST, IPSLCM_45_SST, IPSLCM_85_SST,
   
   ENSMED_H_SSS, ENSMED_45_SSS, ENSMED_85_SSS, ENSMAX_H_SSS, ENSMAX_45_SSS, ENSMAX_85_SSS,
   ENSMIN_H_SSS, ENSMIN_45_SSS, ENSMIN_85_SSS, 
   
   ENSMED_H_SSPH, ENSMED_45_SSPH, ENSMED_85_SSPH, ENSMAX_H_SSPH, ENSMAX_45_SSPH, ENSMAX_85_SSPH,
   ENSMIN_H_SSPH, ENSMIN_45_SSPH, ENSMIN_85_SSPH,
   
   ENSMED_SST, ENSMAX_SST, ENSMIN_SST,    CANESM_SST, MPIESM_SST, IPSLCM_SST,
   ENSMED_SSS, ENSMAX_SSS, ENSMIN_SSS,    ENSMED_SSPH, ENSMAX_SSPH, ENSMIN_SSPH, 
  
   SSS_files, SST_files, SSpH_files,coords, C5_files, new_geom)






##############Extra code ###################################

# Calculate the SST monthly average for each location and each scenario 
SST_MonthlyAvg <- CMIP5_SST %>%
  group_by(model, scenario, season, month) %>%
  summarise(average = mean(value, na.rm = TRUE),
        q05 = quantile(value, probs = 0.05, na.rm = TRUE),
        q95 = quantile(value, probs = 0.95, na.rm = TRUE)) %>%
  ungroup()

SST_MonthlyAvgENS<- CMIP5_ENS_SST %>%
  group_by(model, scenario, season, month) %>%
  summarise(average = mean(value, na.rm = TRUE),
            q05 = quantile(value, probs = 0.05, na.rm = TRUE),
            q95 = quantile(value, probs = 0.95, na.rm = TRUE)) %>%
  ungroup()

ggplot(filter(SST_MonthlyAvg, model== "CanESM2", scenario =="historical", month == 4)) +
  geom_sf(aes(colour = average), size = 4) +
  geom_sf(data = bc_coast, fill = NA, colour = "black") +
  scale_colour_viridis_c()

ggplot(SST_MonthlyAvg, aes(x = month, y = average, colour = model)) +
  geom_line() +
  geom_point() +
  geom_ribbon(alpha = 0.2, aes(ymin = q05, ymax = q95, colour=model)) +
  labs(title = "CMIP5 SST Lat 45-60, Lon 180-240", x = "Month", y = "Mean Temperature (C)", colour = "model") +
  theme(legend.position = "bottom")

ggplot(SST_MonthlyAvgENS, aes(x = month, y = average, ymin = q05, ymax = q95, colour = model)) +
  geom_line() +
  geom_point() +
  #geom_ribbon(alpha = 0.2) +
  labs(title = "CMIP5 SST Lat 45-60, Lon 180-240", x = "Month", y = "Mean Temperature (C)", colour = "model") +
  theme(legend.position = "bottom")


# calculate the monthly average for each scenario (n= 36)
#SST_month <- CMIP5_SST %>%
#  group_by(scenario, season, month) %>%
#  summarise(average = mean(mean, na.rm = TRUE),
 #           q05 = quantile(mean, probs = 0.05, na.rm = TRUE),
#            q95 = quantile(mean, probs = 0.95, na.rm = TRUE)) %>%
#  ungroup()

# Calculate the seasonal average for each scenario (n=12)
#SST_seasons <- SST_avg %>%
#  group_by(scenario, season) %>%
#  summarise(average = mean(mean, na.rm = TRUE),
#            q05 = quantile(mean, probs = 0.05, na.rm = TRUE),
#            q95 = quantile(mean, probs = 0.95, na.rm = TRUE)) %>%
#  ungroup()

# Calcualte the average for each scenario (n=3)
#SST_annual <- SST_avg %>%
#  group_by(scenario) %>%
#  summarise(average = mean(mean, na.rm = TRUE),
#            q05 = quantile(mean, probs = 0.05, na.rm = TRUE),
#            q95 = quantile(mean, probs = 0.95, na.rm = TRUE)) %>%
#  ungroup()





# this isnt working for me because SST_mcm does not exist earlier in the script 
#SST_raster <- st_rasterize(SST_mcm) #%>%
#  st_set_dimensions(3, name = "month", values = month)




  
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

# CMIP processing 
# Select all points within 400km of fraser outflow to compare to other models 
# Load point location of fraser outflow 
FraserEst<-st_read( file.path(paths$spatial, "OutflowLocations", "OutflowLocations.shp"))
FraserBuff<- st_buffer(FraserEst, dist=400000)

FraserEst |> 
  ggplot() +
  
  # plot the buffer layer first so it doesn't cover point
  geom_sf(data = FraserBuff, fill = "yellow", color = "yellow") +
  geom_sf(data = bc_coast, fill = NA, colour = "black") +
  geom_sf(color = "blue")

ggplot(CMIP5_SST) +
     geom_sf( size = 4) +  scale_colour_viridis_c()+
  geom_sf(data = FraserBuff, fill = "yellow", color = "yellow")+
  geom_sf(data = bc_coast, fill = NA, colour = "black") 

SST_clipped<-st_intersection(CMIP5_SST, FraserBuff)

ggplot(SST_clipped) +
  geom_sf( size = 4) +  scale_colour_viridis_c()+
  geom_sf(data = FraserBuff, fill = "yellow", color = "yellow")+
  geom_sf(data = bc_coast, fill = NA, colour = "black") 
