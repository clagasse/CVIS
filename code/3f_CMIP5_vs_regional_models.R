#CMIP 5 ensemble models vs Regional models 
# Load CMIP data 
CMIP5_ENS_SST<- read_sf( file.path(paths$climate, "Standardized_Marine_data/CMIP5_ENS_SST.gdb"))
CMIP5_ENS_SSS<- read_sf( file.path(paths$climate, "Standardized_Marine_data/CMIP5_ENS_SSS.gdb"))
CMIP5_ENS_SSPH<-read_sf( file.path(paths$climate, "Standardized_Marine_data/CMIP5_ENS_SSPH.gdb"))

# Load climate Regional data subsetted
BCCM_SST_sub<-  read_sf( file.path(paths$climate, "Standardized_Marine_data/BCCM_SST_sub.gdb"))
BCCM_SSS_sub<-  read_sf( file.path(paths$climate, "Standardized_Marine_data/BCCM_SSS_sub.gdb"))
BCCM_SSPH_sub<- read_sf( file.path(paths$climate, "Standardized_Marine_data/BCCM_SSPH_sub.gdb"))
NEP_SST_sub<-   read_sf( file.path(paths$climate, "Standardized_Marine_data/NEP_SST_sub.gdb"))
NEP_SSS_sub<-   read_sf( file.path(paths$climate, "Standardized_Marine_data/NEP_SSS_sub.gdb"))
NEP_SSPH_sub<-  read_sf( file.path(paths$climate, "Standardized_Marine_data/NEP_SSPH_sub.gdb"))
SSC_SST_sub<-   read_sf( file.path(paths$climate, "Standardized_Marine_data/SSC_SST_sub.gdb"))
SSC_SSS_sub<-   read_sf( file.path(paths$climate, "Standardized_Marine_data/SSC_SSS_sub.gdb"))

######### FORMAT regional model data ############
# This series of code will make the regional data match the format of the CMIP5 data 
#BCCM model
BCCM_SST_long <- BCCM_SST_sub %>%
  pivot_longer(
    cols = -c("SHAPE", "MAZ_Acrony"),
    names_to = c("scenario", "month"),
    names_pattern = 'SST_([A-Za-z0-9]+)_([0-9]+)',
    values_to = "value"
  ) %>%
mutate(model = "BCCM",
       month = as.numeric(month))
#st_geometry(BCCM_SST_long) <- "geometry" # Rename the column that contains the geometry to match CMIP data
BCCM_SST_long<- subset(BCCM_SST_long, select= -c(MAZ_Acrony) ) #Remove MAZ acronym column
BCCM_SST_long<-BCCM_SST_long[,c(1,5, 2,3,4)] # Reorder columns to match CMIP 5 data 
oldmonths<-c(01,02,03,04,05,06,07,08,09,10,11,12)
newmonths<-c(1,2,3,4,5,6,7,8,9,10,11,12)
BCCM_SST_long$month<-replace(BCCM_SST_long$month, BCCM_SST_long$month %in% oldmonths, newmonths)
BCCM_SST_long$month<- as.factor(BCCM_SST_long$month)

BCCM_SSS_long <- BCCM_SSS_sub %>%
  pivot_longer(
    cols = -c("SHAPE", "MAZ_Acrony"),
    names_to = c("scenario", "month"),
    names_pattern = 'SSS_([A-Za-z0-9]+)_([0-9]+)',
    values_to = "value"
  ) %>%
  mutate(model = "BCCM",
         month = as.numeric(month))
#st_geometry(BCCM_SSS_long) <- "geometry" # Rename the column that contains the geometry to match CMIP data
BCCM_SSS_long<- subset(BCCM_SSS_long, select= -c(MAZ_Acrony) ) #Remove MAZ acronym column
BCCM_SSS_long<-BCCM_SSS_long[,c(1,5, 2,3,4)] # Reorder columns to match CMIP 5 data 
BCCM_SSS_long$month<-replace(BCCM_SSS_long$month, BCCM_SSS_long$month %in% oldmonths, newmonths)
BCCM_SSS_long$month<- as.factor(BCCM_SSS_long$month)

BCCM_SSPH_long <- BCCM_SSPH_sub %>%
  pivot_longer(
    cols = -c("SHAPE", "MAZ_Acrony"),
    names_to = c("scenario", "month"),
    names_pattern = 'SSPH_([A-Za-z0-9]+)_([0-9]+)',
    values_to = "value"
  ) %>%
  mutate(model = "BCCM",
         month = as.numeric(month))
#st_geometry(BCCM_SSPH_long) <- "geometry" # Rename the column that contains the geometry to match CMIP data
BCCM_SSPH_long<- subset(BCCM_SSPH_long, select= -c(MAZ_Acrony) ) #Remove MAZ acronym column
BCCM_SSPH_long<-BCCM_SSPH_long[,c(1,5, 2,3,4)] # Reorder columns to match CMIP 5 data 
BCCM_SSPH_long$month<-replace(BCCM_SSPH_long$month, BCCM_SSPH_long$month %in% oldmonths, newmonths)
BCCM_SSPH_long$month<- as.factor(BCCM_SSPH_long$month)

# SSC model 
SSC_SST_long <- SSC_SST_sub %>%
  pivot_longer(
    cols = -c("SHAPE", "MAZ_Acrony"),
    names_to = c("scenario", "month"),
    names_pattern = 'SST_([A-Za-z0-9]+)_([0-9]+)',
    values_to = "value"
  ) %>%
  mutate(model = "SSC",
         month = as.numeric(month))
#st_geometry(SSC_SST_long) <- "geometry" # Rename the column that contains the geometry to match CMIP data
SSC_SST_long<- subset(SSC_SST_long, select= -c(MAZ_Acrony) ) #Remove MAZ acronym column
SSC_SST_long<-SSC_SST_long[,c(1,5, 2,3,4)] # Reorder columns to match CMIP 5 data 
SSC_SST_long$month<-replace(SSC_SST_long$month, SSC_SST_long$month %in% oldmonths, newmonths)
SSC_SST_long$month<- as.factor(SSC_SST_long$month)

SSC_SSS_long <- SSC_SSS_sub %>%
  pivot_longer(
    cols = -c("SHAPE", "MAZ_Acrony"),
    names_to = c("scenario", "month"),
    names_pattern = 'SSS_([A-Za-z0-9]+)_([0-9]+)',
    values_to = "value"
  ) %>%
  mutate(model = "SSC",
         month = as.numeric(month))
#st_geometry(SSC_SSS_long) <- "geometry" # Rename the column that contains the geometry to match CMIP data
SSC_SSS_long<- subset(SSC_SSS_long, select= -c(MAZ_Acrony) ) #Remove MAZ acronym column
SSC_SSS_long<-SSC_SSS_long[,c(1,5, 2,3,4)] # Reorder columns to match CMIP 5 data 
SSC_SSS_long$month<-replace(SSC_SSS_long$month, SSC_SSS_long$month %in% oldmonths, newmonths)
SSC_SSS_long$month<- as.factor(SSC_SSS_long$month)

# NEP36
NEP_SST_long <- NEP_SST_sub %>%
  pivot_longer(
    cols = -c("SHAPE", "MAZ_Acrony"),
    names_to = c("scenario", "month"),
    names_pattern = 'SST_([A-Za-z0-9]+)_([0-9]+)',
    values_to = "value"
  ) %>%
  mutate(model = "NEP",
         month = as.numeric(month))
#st_geometry(NEP_SST_long) <- "geometry" # Rename the column that contains the geometry to match CMIP data
NEP_SST_long<- subset(NEP_SST_long, select= -c(MAZ_Acrony) ) #Remove MAZ acronym column
NEP_SST_long<-NEP_SST_long[,c(1,5, 2,3,4)] # Reorder columns to match CMIP 5 data 
NEP_SST_long$month<-replace(NEP_SST_long$month, NEP_SST_long$month %in% oldmonths, newmonths)
NEP_SST_long$month<- as.factor(NEP_SST_long$month)

NEP_SSS_long <- NEP_SSS_sub %>%
  pivot_longer(
    cols = -c("SHAPE", "MAZ_Acrony"),
    names_to = c("scenario", "month"),
    names_pattern = 'SSS_([A-Za-z0-9]+)_([0-9]+)',
    values_to = "value"
  ) %>%
  mutate(model = "NEP",
         month = as.numeric(month))
#st_geometry(NEP_SSS_long) <- "geometry" # Rename the column that contains the geometry to match CMIP data
NEP_SSS_long<- subset(NEP_SSS_long, select= -c(MAZ_Acrony) ) #Remove MAZ acronym column
NEP_SSS_long<-NEP_SSS_long[,c(1,5, 2,3,4)] # Reorder columns to match CMIP 5 data 
NEP_SSS_long$month<-replace(NEP_SSS_long$month, NEP_SSS_long$month %in% oldmonths, newmonths)
NEP_SSS_long$month<- as.factor(NEP_SSS_long$month)

NEP_SSPH_long <- NEP_SSPH_sub %>%
  pivot_longer(
    cols = -c("SHAPE", "MAZ_Acrony"),
    names_to = c("scenario", "month"),
    names_pattern = 'SSPH_([A-Za-z0-9]+)_([0-9]+)',
    values_to = "value"
  ) %>%
  mutate(model = "NEP",
         month = as.numeric(month))
st_geometry(NEP_SSPH_long) <- "geometry" # Rename the column that contains the geometry to match CMIP data
NEP_SSPH_long<- subset(NEP_SSPH_long, select= -c(MAZ_Acrony) ) #Remove MAZ acronym column
NEP_SSPH_long<-NEP_SSPH_long[,c(1,5, 2,3,4)] # Reorder columns to match CMIP 5 data 
NEP_SSPH_long$month<-replace(NEP_SSPH_long$month, NEP_SSPH_long$month %in% oldmonths, newmonths)
NEP_SSPH_long$month<- as.factor(NEP_SSPH_long$month)

rm(BCCM_SST_sub,NEP_SST_sub,SSC_SST_sub, BCCM_SSS_sub,NEP_SSS_sub,SSC_SSS_sub,    
   BCCM_SSPH_sub,NEP_SSPH_sub)

######## Format CMIP 5 data  ######
### Filter and mask CMIP data to match regional models extent
# load masks
BCCM_mask<-read_sf(file.path(paths$climate, "BCCM", "BCCM_mask2.shp")) %>% st_transform(crs = "EPSG:3005" )                       
NEP_mask<-read_sf(file.path(paths$climate, "NEP36_MonthlyData", "NEP_mask.shp"))%>% st_transform(crs = "EPSG:3005" )        
EEZ<-read_sf(file.path(paths$spatial, "BC_EEZ", "BC_EEZ.shp"))%>%st_transform(,crs = "EPSG:3005" )     
# create a 400km buffer around the fraser estuary to spatially mask CMIP5 data with to compare to the SSC model
FraserEst<-st_read( file.path(paths$spatial, "OutflowLocations", "OutflowLocations.shp"))
FraserBuff<- st_buffer(FraserEst, dist=400000)

# Filter time and spatially mask CMIP5 data to match regional models 
#SST
CMIP_ENS_SST_BCCM<- filter(CMIP5_ENS_SST, between(year, 1981, 2010) | between(year, 2041, 2070)) %>%
  filter(!is.na(value)) %>%     # remove points with NA values 
  st_intersection( BCCM_mask)

CMIP_ENS_SST_NEP<- filter(CMIP5_ENS_SST, between(year, 1986, 2005) | between(year, 2046, 2065)) %>%
  filter(!is.na(value)) %>%
  st_intersection( NEP_mask)

CMIP_ENS_SST_SSC<- filter(CMIP5_ENS_SST, between(year, 1986, 2005) | between(year, 2046, 2065)) %>%
  filter(!is.na(value)) %>%
  st_intersection( FraserBuff) %>% st_intersection(EEZ)
#SSS
CMIP_ENS_SSS_BCCM<- filter(CMIP5_ENS_SSS, between(year, 1981, 2010) | between(year, 2041, 2070)) %>%
  filter(!is.na(value)) %>%     # remove points with NA values 
  st_intersection( BCCM_mask)

CMIP_ENS_SSS_NEP<- filter(CMIP5_ENS_SSS, between(year, 1986, 2005) | between(year, 2046, 2065)) %>%
  filter(!is.na(value)) %>%
  st_intersection( NEP_mask)

CMIP_ENS_SSS_SSC<- filter(CMIP5_ENS_SSS, between(year, 1986, 2005) | between(year, 2046, 2065)) %>%
  filter(!is.na(value)) %>%
  st_intersection( FraserBuff) %>% st_intersection(EEZ)
#SSPH
CMIP_ENS_SSPH_BCCM<- filter(CMIP5_ENS_SSPH, between(year, 1981, 2010) | between(year, 2041, 2070)) %>%
  filter(!is.na(value)) %>%     # remove points with NA values 
  st_intersection( BCCM_mask)

CMIP_ENS_SSPH_NEP<- filter(CMIP5_ENS_SSPH, between(year, 1986, 2005) | between(year, 2046, 2065)) %>%
  filter(!is.na(value)) %>%
  st_intersection( NEP_mask)

#plot CMIP5 data exent
ggplot(CMIP_ENS_SST_BCCM) +
  geom_sf(data = EEZ,  fill=NA, colour = "black")+
  geom_sf(data = FraserBuff, fill=NA, colour = "darkred")+
  geom_sf(data = bc_coast,  colour = "black") +
  geom_sf(data = CMIP_ENS_SST_NEP, colour = "black", size=4)+
  geom_sf(data = CMIP_ENS_SST_SSC, colour = "red", size=3)+
  geom_sf(data = CMIP_ENS_SST_BCCM, colour = "blue", size = 2) +
  geom_sf(data = FraserEst, colour = "darkred", size = 4)

#Summarize by location, scenario and month so each point has a monthly average across all years 
#SST
CMIP_ENS_SST_BCCM_avg <- CMIP_ENS_SST_BCCM %>%
  group_by(SHAPE,model,scenario, month) %>%
  summarise(average = mean(value, na.rm = TRUE)) %>%
  ungroup()

CMIP_ENS_SST_NEP_avg <- CMIP_ENS_SST_NEP %>%
  group_by(SHAPE,model, scenario, month) %>%
  summarise(average = mean(value, na.rm = TRUE)) %>%
  ungroup()

CMIP_ENS_SST_SSC_avg <- CMIP_ENS_SST_SSC %>%
  group_by(SHAPE,model, scenario, month) %>%
  summarise(average = mean(value, na.rm = TRUE)) %>%
  ungroup()
#SSS
CMIP_ENS_SSS_BCCM_avg <- CMIP_ENS_SSS_BCCM %>%
  group_by(SHAPE,model,scenario, month) %>%
  summarise(average = mean(value, na.rm = TRUE)) %>%
  ungroup()

CMIP_ENS_SSS_NEP_avg <- CMIP_ENS_SSS_NEP %>%
  group_by(SHAPE,model, scenario, month) %>%
  summarise(average = mean(value, na.rm = TRUE)) %>%
  ungroup()

CMIP_ENS_SSS_SSC_avg <- CMIP_ENS_SSS_SSC %>%
  group_by(SHAPE,model, scenario, month) %>%
  summarise(average = mean(value, na.rm = TRUE)) %>%
  ungroup()
#SSPH
CMIP_ENS_SSPH_BCCM_avg <- CMIP_ENS_SSPH_BCCM %>%
  group_by(SHAPE,model,scenario, month) %>%
  summarise(average = mean(value, na.rm = TRUE)) %>%
  ungroup()

CMIP_ENS_SSPH_NEP_avg <- CMIP_ENS_SSPH_NEP %>%
  group_by(SHAPE,model, scenario, month) %>%
  summarise(average = mean(value, na.rm = TRUE)) %>%
  ungroup()

# Try plotting it
#ggplot(filter(CMIP_ENS_SST_SSC_avg, model== "ensemblemedian", scenario =="historical", month == 4)) +
 # geom_sf(aes(colour = average), size = 4) +
  #geom_sf(data = bc_coast, fill = NA, colour = "black") +
  #scale_colour_viridis_c()

# Look at monthly variation
#SST
CMIP_ENS_SST_BCCM_avg$month<- as.factor(CMIP_ENS_SST_BCCM_avg$month) # Set month at a factor variable
colnames(CMIP_ENS_SST_BCCM_avg) <- c("SHAPE","model", "scenario", "month", "value") #rename columns so they match the regional models column names 

CMIP_ENS_SST_NEP_avg$month<- as.factor(CMIP_ENS_SST_NEP_avg$month) # Set month at a factor variable
colnames(CMIP_ENS_SST_NEP_avg) <- c("SHAPE","model", "scenario", "month", "value")

CMIP_ENS_SST_SSC_avg$month<- as.factor(CMIP_ENS_SST_SSC_avg$month) # Set month at a factor variable
colnames(CMIP_ENS_SST_SSC_avg) <- c("SHAPE","model", "scenario", "month", "value")
#SSS
CMIP_ENS_SSS_BCCM_avg$month<- as.factor(CMIP_ENS_SSS_BCCM_avg$month) # Set month at a factor variable
colnames(CMIP_ENS_SSS_BCCM_avg) <- c("SHAPE","model", "scenario", "month", "value") #rename columns so they match the regional models column names 

CMIP_ENS_SSS_NEP_avg$month<- as.factor(CMIP_ENS_SSS_NEP_avg$month) # Set month at a factor variable
colnames(CMIP_ENS_SSS_NEP_avg) <- c("SHAPE","model", "scenario", "month", "value")

CMIP_ENS_SSS_SSC_avg$month<- as.factor(CMIP_ENS_SSS_SSC_avg$month) # Set month at a factor variable
colnames(CMIP_ENS_SSS_SSC_avg) <- c("SHAPE","model", "scenario", "month", "value")

#SSPH
CMIP_ENS_SSPH_BCCM_avg$month<- as.factor(CMIP_ENS_SSPH_BCCM_avg$month) # Set month at a factor variable
colnames(CMIP_ENS_SSPH_BCCM_avg) <- c("SHAPE","model", "scenario", "month", "value") #rename columns so they match the regional models column names 

CMIP_ENS_SSPH_NEP_avg$month<- as.factor(CMIP_ENS_SSPH_NEP_avg$month) # Set month at a factor variable
colnames(CMIP_ENS_SSPH_NEP_avg) <- c("SHAPE","model", "scenario", "month", "value")


#Plot CMIP BCCM data 
#CMIP_BCCM_SSTMonth <- ggplot(filter(CMIP_ENS_SST_BCCM_avg, model== "ensemblemedian"),  
  #                           aes(x=month, y=value, fill=scenario)) + 
  #geom_boxplot(position=position_dodge(1))

rm(BCCM_mask, NEP_mask, FraserBuff, FraserEst)

############ Combine regional and CMIP data ############
#Combine and plot regional and CMIP climate data 
#bind long form BCCM_SST_sub with CMIP5_BCCM data 
BCCMvsCMIP5_SST<-bind_rows(BCCM_SST_long,  CMIP_ENS_SST_BCCM_avg)
BCCMvsCMIP5_SSS<-bind_rows(BCCM_SSS_long,  CMIP_ENS_SSS_BCCM_avg)
BCCMvsCMIP5_SSPH<-bind_rows(BCCM_SSPH_long,  CMIP_ENS_SSPH_BCCM_avg)

NEPvsCMIP5_SST<-bind_rows(NEP_SST_long,  CMIP_ENS_SST_NEP_avg)
NEPvsCMIP5_SSS<-bind_rows(NEP_SSS_long,  CMIP_ENS_SSS_NEP_avg)
NEPvsCMIP5_SSPH<-bind_rows(NEP_SSPH_long,  CMIP_ENS_SSPH_NEP_avg)

SSCvsCMIP5_SST<-bind_rows(SSC_SST_long,  CMIP_ENS_SST_SSC_avg)
SSCvsCMIP5_SSS<-bind_rows(SSC_SSS_long,  CMIP_ENS_SSS_SSC_avg)


############# Compare regional and CMIP data ############
# ERROR is being produced and Im not sure why! Its worrying

ggplot(filter(BCCMvsCMIP5_SST,  scenario=="historical"|scenario=="H", na.rm=TRUE),  
       aes(x=month, y=value, fill=model)) + 
  geom_boxplot(position=position_dodge(1))+
  ggtitle("BCCM vs CMIP5 ensemble: SST Historical")
ggplot(filter(NEPvsCMIP5_SST, scenario=="historical"|scenario=="H"),  
       aes(x=month, y=value, fill=model)) + 
  geom_boxplot(position=position_dodge(1))+
  ggtitle("NEP vs CMIP5 ensemble: SST Historical")
ggplot(filter(SSCvsCMIP5_SST,  scenario=="historical"|scenario=="H"),  
       aes(x=month, y=value, fill=model)) + 
  geom_boxplot(position=position_dodge(1))+
  ggtitle("SSC vs CMIP5 ensemble: SST Historical")

ggplot(filter(BCCMvsCMIP5_SSS,  scenario=="historical"|scenario=="H"),  
       aes(x=month, y=value, fill=model)) + 
  geom_boxplot(position=position_dodge(1))+
  ggtitle("BCCM vs CMIP5 ensemble: SSS Historical")
ggplot(filter(NEPvsCMIP5_SSS,  scenario=="historical"|scenario=="H"),  
       aes(x=month, y=value, fill=model)) + 
  geom_boxplot(position=position_dodge(1))+
  ggtitle("NEP vs CMIP5 ensemble: SSS Historical")
ggplot(filter(SSCvsCMIP5_SSS,  scenario=="historical"|scenario=="H"),  
       aes(x=month, y=value, fill=model)) + 
  geom_boxplot(position=position_dodge(1))+
  ggtitle("SSC vs CMIP5 ensemble: SSS Historical")

ggplot(filter(BCCMvsCMIP5_SSPH,  scenario=="historical"|scenario=="H"),  
       aes(x=month, y=value, fill=model)) + 
  geom_boxplot(position=position_dodge(1))+
  ggtitle("BCCM vs CMIP5 ensemble: SSPH Historical")
ggplot(filter(NEPvsCMIP5_SSPH,  scenario=="historical"|scenario=="H"),  
       aes(x=month, y=value, fill=model)) + 
  geom_boxplot(position=position_dodge(1))+
  ggtitle("NEP vs CMIP5 ensemble: SSPH Historical")


ggplot(filter(BCCMvsCMIP5_SST,  scenario=="rcp45"|scenario=="45", na.rm=TRUE),  
       aes(x=month, y=value, fill=model)) + 
  geom_boxplot(position=position_dodge(1))+
  ggtitle("BCCM vs CMIP5 ensemble: SST RCP45")
ggplot(filter(NEPvsCMIP5_SST, scenario=="rcp45"|scenario=="45"),  
       aes(x=month, y=value, fill=model)) + 
  geom_boxplot(position=position_dodge(1))+
  ggtitle("NEP vs CMIP5 ensemble: SST RCP45")
ggplot(filter(SSCvsCMIP5_SST,  scenario=="rcp45"|scenario=="45"),  
       aes(x=month, y=value, fill=model)) + 
  geom_boxplot(position=position_dodge(1))+
  ggtitle("SSC vs CMIP5 ensemble: SST RCP45")

ggplot(filter(BCCMvsCMIP5_SSS,  scenario=="rcp45"|scenario=="45"),  
       aes(x=month, y=value, fill=model)) + 
  geom_boxplot(position=position_dodge(1))+
  ggtitle("BCCM vs CMIP5 ensemble: SSS RCP 4.5")
ggplot(filter(NEPvsCMIP5_SSS,  scenario=="rcp45"|scenario=="45"),  
       aes(x=month, y=value, fill=model)) + 
  geom_boxplot(position=position_dodge(1))+
  ggtitle("NEP vs CMIP5 ensemble: SSS RCP 4.5")
ggplot(filter(SSCvsCMIP5_SSS,  scenario=="rcp45"|scenario=="45"),  
       aes(x=month, y=value, fill=model)) + 
  geom_boxplot(position=position_dodge(1))+
  ggtitle("SSC vs CMIP5 ensemble: SSS RCP 4.5")

ggplot(filter(BCCMvsCMIP5_SSPH,  scenario=="rcp45"|scenario=="45"),  
       aes(x=month, y=value, fill=model)) + 
  geom_boxplot(position=position_dodge(1))+
  ggtitle("BCCM vs CMIP5 ensemble: SSpH RCP 4.5")
ggplot(filter(NEPvsCMIP5_SSPH,  scenario=="rcp45"|scenario=="45"),  
       aes(x=month, y=value, fill=model)) + 
  geom_boxplot(position=position_dodge(1))+
  ggtitle("NEP vs CMIP5 ensemble: SSpH RCP 4.5")



ggplot(filter(RegionalvsCMIP5_SST, model== "ensemblemedian"|model== "BCCM"|model== "NEP"|model== "SSC", scenario=="historical"|scenario=="H"),  
                             aes(x=month, y=value, fill=model)) + 
  geom_boxplot(position=position_dodge(1))+
  ggtitle("")

ggplot(filter(RegionalvsCMIP5_SSS, model== "ensemblemedian"|model== "BCCM"|model== "NEP"|model== "SSC", scenario=="historical"|scenario=="H"),  
       aes(x=month, y=value, fill=model)) + 
  geom_boxplot(position=position_dodge(1))

ggplot(filter(RegionalvsCMIP5_SSPH, model== "ensemblemedian"|model== "BCCM"|model== "NEP", scenario=="rcp45"|scenario=="45"),  
       aes(x=month, y=value, fill=model)) + 
  geom_boxplot(position=position_dodge(1))



