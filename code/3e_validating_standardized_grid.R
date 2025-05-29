# comparing MAZ summaries from points to interpolated standarized grids to see how 
# well the interpolation preserves these averages

#Import cropnped layers as vector grid files 
BCCM_SST_cropped<-read_sf(file.path(climate_dat, "Standardized_Marine_data/BCCM_SST_cropped.shp"))
BCCM_SSS_cropped<-read_sf( file.path(climate_dat, "Standardized_Marine_data/BCCM_SSS_cropped.shp"  ))
BCCM_SSPH_cropped<-read_sf(file.path(climate_dat, "Standardized_Marine_data/BCCM_SSPH_cropped.shp" ))
NEP_SST_cropped<-read_sf(file.path(climate_dat, "Standardized_Marine_data/NEP_SST_cropped.shp"))
NEP_SSS_cropped<-read_sf(file.path(climate_dat, "Standardized_Marine_data/NEP_SSS_cropped.shp" ))
NEP_SSPH_cropped<-read_sf(file.path(climate_dat, "Standardized_Marine_data/NEP_SSPH_cropped.shp"))
SSC_SST_cropped<-read_sf( file.path(climate_dat, "Standardized_Marine_data/SSC_SST_cropped.shp"))
SSC_SSS_cropped<-read_sf(file.path(climate_dat, "Standardized_Marine_data/SSC_SSS_cropped.shp"))
CI_cropped<-read_sf(file.path(climate_dat, "Standardized_Marine_data/CI_cropped.shp"))

# descriptive stats
# mean, median, mode
mean(BCCM_SST_cropped$SST_H_01, na.rm=TRUE)
mean(BCCM_SST_sub$SST_H_01, na.rm=TRUE)

median(BCCM_SST_cropped$SST_H_01, na.rm=TRUE)
median(BCCM_SST_sub$SST_H_01, na.rm=TRUE)

hist(BCCM_SST_cropped$SST_H_01)
hist(BCCM_SST_sub$SST_H_01)

max(BCCM_SST_cropped$SST_H_01, na.rm=TRUE)
max(BCCM_SST_sub$SST_H_01, na.rm=TRUE)

min(BCCM_SST_cropped$SST_H_01, na.rm=TRUE)
min(BCCM_SST_sub$SST_H_01, na.rm=TRUE)

# Try calculating marine indicators with cropped data 

BCCM_SST_sub<-BCCM_SST_cropped
BCCM_SSS_sub<-BCCM_SSS_cropped
BCCM_SSPH_sub<-BCCM_SSPH_cropped
NEP_SST_sub<- NEP_SST_cropped
NEP_SSS_sub<- NEP_SSS_cropped
NEP_SSPH_sub<- NEP_SSPH_cropped
SSC_SST_sub<- SSC_SST_cropped
SSC_SSS_sub<- SSC_SSS_cropped
CI_points_sub<- CI_cropped

######### Spring difference ###############
#1) Calculate difference between the historic and future spring values in new column 
BCCM_SST_sub$BCCM_SST_Spring_Diff <-((BCCM_SST_sub$SST_45_04+BCCM_SST_sub$SST_45_05+BCCM_SST_sub$SST_45_06)/3)-((BCCM_SST_sub$SST_H_04 + BCCM_SST_sub$SST_H_05+BCCM_SST_sub$SST_H_06)/3)
BCCM_SSS_sub$BCCM_SSS_Spring_Diff <-((BCCM_SSS_sub$SSS_45_04+BCCM_SSS_sub$SSS_45_05+BCCM_SSS_sub$SSS_45_06)/3)-((BCCM_SSS_sub$SSS_H_04 + BCCM_SSS_sub$SSS_H_05+BCCM_SSS_sub$SSS_H_06)/3)
BCCM_SSPH_sub$BCCM_SSPH_Spring_Diff <-((BCCM_SSPH_sub$SSPH_45_04+BCCM_SSPH_sub$SSPH_45_05+BCCM_SSPH_sub$SSPH_45_06)/3)-((BCCM_SSPH_sub$SSPH_H_04 + BCCM_SSPH_sub$SSPH_H_05+BCCM_SSPH_sub$SSPH_H_06)/3)

NEP_SST_sub$NEP_SST_Spring_Diff <-((NEP_SST_sub$SST_45_04+NEP_SST_sub$SST_45_05+NEP_SST_sub$SST_45_06)/3)-((NEP_SST_sub$SST_H_04 + NEP_SST_sub$SST_H_05+NEP_SST_sub$SST_H_06)/3)
NEP_SSS_sub$NEP_SSS_Spring_Diff <-((NEP_SSS_sub$SSS_45_04+ NEP_SSS_sub$SSS_45_05+NEP_SSS_sub$SSS_45_06)/3)-((NEP_SSS_sub$SSS_H_04 + NEP_SSS_sub$SSS_H_05+ NEP_SSS_sub$SSS_H_06) /3)
NEP_SSPH_sub$NEP_SSPH_Spring_Diff <-((NEP_SSPH_sub$SSPH_45_04+ NEP_SSPH_sub$SSPH_45_05+NEP_SSPH_sub$SSPH_45_06)/3)-((NEP_SSPH_sub$SSPH_H_04 + NEP_SSPH_sub$SSPH_H_05 +NEP_SSPH_sub$SSPH_H_06 )/3)

SSC_SST_sub$SSC_SST_Spring_Diff <-((SSC_SST_sub$SST_45_04+SSC_SST_sub$SST_45_05+SSC_SST_sub$SST_45_06)/3)-((SSC_SST_sub$SST_H_04 + SSC_SST_sub$SST_H_05+SSC_SST_sub$SST_H_06)/3)
SSC_SSS_sub$SSC_SSS_Spring_Diff <-((SSC_SSS_sub$SSS_45_04+SSC_SSS_sub$SSS_45_05+SSC_SSS_sub$SSS_45_06)/3)-((SSC_SSS_sub$SSS_H_04 + SSC_SSS_sub$SSS_H_05+SSC_SSS_sub$SSS_H_06)/3)

# 2) average differences across MAZs 
###PLEASE double check this code, not sure its doing the right thing
SST_BCCM<- BCCM_SST_sub %>% group_by(MAZ_Acrony) %>%
  summarise(across(starts_with("BCCM_SST_Spring_Diff"), mean, na.rm=TRUE)) %>%
  as.data.frame() 
SSS_BCCM<-BCCM_SSS_sub %>% group_by(MAZ_Acrony) %>%
  summarise(across(starts_with("BCCM_SSS_Spring_Diff"), mean, na.rm=TRUE ))%>%
  as.data.frame()
SSPH_BCCM<-BCCM_SSPH_sub %>% group_by(MAZ_Acrony) %>%
  summarise(across(starts_with("BCCM_SSPH_Spring_Diff"), mean,na.rm=TRUE ))%>%
  as.data.frame()

SST_NEP<-NEP_SST_sub %>% group_by(MAZ_Acrony) %>%
  summarise(across(starts_with("NEP_SST_Spring_Diff"), mean))%>%
  as.data.frame()
SSS_NEP<-NEP_SSS_sub %>% group_by(MAZ_Acrony) %>%
  summarise(across(starts_with("NEP_SSS_Spring_Diff"), mean))%>%
  as.data.frame()
SSPH_NEP<-NEP_SSPH_sub %>% group_by(MAZ_Acrony) %>%
  summarise(across(starts_with("NEP_SSPH_Spring_Diff"), mean))%>%
  as.data.frame()

SST_SSC<-SSC_SST_sub %>% group_by(MAZ_Acrony) %>%
  summarise(across(starts_with("SSC_SST_Spring_Diff"), mean,na.rm=TRUE)) %>%
  as.data.frame()
SSS_SSC<-SSC_SSS_sub %>% group_by(MAZ_Acrony) %>%
  summarise(across(starts_with("SSC_SSS_Spring_Diff"), mean,na.rm=TRUE))%>%
  as.data.frame()

#Merge spring differences back together for each variable 
#SST
SST_SpringDiff1<-merge(SST_BCCM, SST_NEP,  by="MAZ_Acrony", all=TRUE)
SST_SpringDiff<-merge(SST_SpringDiff1, SST_SSC, by="MAZ_Acrony", all=TRUE)
SST_SpringDiff= subset(SST_SpringDiff, select = -c(geometry,geometry.x, geometry.y))%>% 
  mutate(across(where(is.numeric), ~ round(., 2)))
#SSS
SSS_SpringDiff1<-merge(SSS_BCCM,SSS_NEP,  by="MAZ_Acrony", all=TRUE) 
SSS_SpringDiff<-merge(SSS_SpringDiff1, SSS_SSC, by="MAZ_Acrony", all=TRUE)
SSS_SpringDiff= subset(SSS_SpringDiff, select = -c(geometry,geometry.x, geometry.y))%>% 
  mutate(across(where(is.numeric), ~ round(., 2)))  
#SSPH  
SSPH_SpringDiff<-merge(SSPH_BCCM,SSPH_NEP,  by="MAZ_Acrony", all=TRUE) 
SSPH_SpringDiff= subset(SSPH_SpringDiff, select = -c(geometry.x, geometry.y)) %>% 
  mutate(across(where(is.numeric), ~ round(., 2))) 

SpringDiff1<- merge(SST_SpringDiff, SSS_SpringDiff, by="MAZ_Acrony", all=TRUE)
SpringDiff<- merge(SpringDiff1, SSPH_SpringDiff, by="MAZ_Acrony", all=TRUE)
### Put data in tables
#rename columns for tables
colAll<-c('MAZ', 'BCCM','NEP',  'SSC','BCCM','NEP', 'SSC','BCCM','NEP' )
colNEPBCCM<- c('MAZ','BCCM','NEP')
colnames(SpringDiff)<- colAll
#colnames(SST_SpringDiff)<- colAll
#colnames(SSS_SpringDiff)<- colAll
#colnames(SSPH_SpringDiff)<- colNEPBCCM

# Create table ## Not sure why there are a bunch of extra columns 
grid.newpage()
grid.table(SpringDiff)
#grid.table(SST_SpringDiff)
#grid.newpage()
#grid.table(SSS_SpringDiff)
#grid.newpage()
#grid.table(SSPH_SpringDiff)

#remove variables no longer needed 
rm(SST_SpringDiff1, SSS_SpringDiff1,SpringDiff1, SSS_SpringDiff, SST_SpringDiff, SSPH_SpringDiff,
   SSS_BCCM, SSS_NEP, SSS_SSC, SST_BCCM, SST_NEP, SST_SSC, SSPH_BCCM, SSPH_NEP)

############# Spring mean Projected temperature #######
BCCM_SST_sub$BCCM_SST_SpringAvg <-((BCCM_SST_sub$SST_45_04+BCCM_SST_sub$SST_45_05+BCCM_SST_sub$SST_45_06)/3)
BCCM_SSS_sub$BCCM_SSS_SpringAvg <-((BCCM_SSS_sub$SSS_45_04+BCCM_SSS_sub$SSS_45_05+BCCM_SSS_sub$SSS_45_06)/3)
BCCM_SSPH_sub$BCCM_SSPH_SpringAvg <-((BCCM_SSPH_sub$SSPH_45_04+BCCM_SSPH_sub$SSPH_45_05+BCCM_SSPH_sub$SSPH_45_06)/3)

NEP_SST_sub$NEP_SST_SpringAvg <-((NEP_SST_sub$SST_45_04+NEP_SST_sub$SST_45_05+NEP_SST_sub$SST_45_06)/3)
NEP_SSS_sub$NEP_SSS_SpringAvg <-((NEP_SSS_sub$SSS_45_04+ NEP_SSS_sub$SSS_45_05+NEP_SSS_sub$SSS_45_06)/3)
NEP_SSPH_sub$NEP_SSPH_SpringAvg <-((NEP_SSPH_sub$SSPH_45_04+ NEP_SSPH_sub$SSPH_45_05+NEP_SSPH_sub$SSPH_45_06)/3)

SSC_SST_sub$SSC_SST_SpringAvg <-((SSC_SST_sub$SST_45_04+SSC_SST_sub$SST_45_05+SSC_SST_sub$SST_45_06)/3)
SSC_SSS_sub$SSC_SSS_SpringAvg <-((SSC_SSS_sub$SSS_45_04+SSC_SSS_sub$SSS_45_05+SSC_SSS_sub$SSS_45_06)/3)

# 2) average differences across MAZs 
###PLEASE double check this code, not sure its doing the right thing
SST_BCCM<- BCCM_SST_sub %>% group_by(MAZ_Acrony) %>%
  summarise(across(starts_with("BCCM_SST_SpringAvg"), mean, na.rm=TRUE)) %>%
  as.data.frame() 
SSS_BCCM<-BCCM_SSS_sub %>% group_by(MAZ_Acrony) %>%
  summarise(across(starts_with("BCCM_SSS_SpringAvg"), mean, na.rm=TRUE ))%>%
  as.data.frame()
SSPH_BCCM<-BCCM_SSPH_sub %>% group_by(MAZ_Acrony) %>%
  summarise(across(starts_with("BCCM_SSPH_SpringAvg"), mean,na.rm=TRUE ))%>%
  as.data.frame()

SST_NEP<-NEP_SST_sub %>% group_by(MAZ_Acrony) %>%
  summarise(across(starts_with("NEP_SST_SpringAvg"), mean))%>%
  as.data.frame()
SSS_NEP<-NEP_SSS_sub %>% group_by(MAZ_Acrony) %>%
  summarise(across(starts_with("NEP_SSS_SpringAvg"), mean))%>%
  as.data.frame()
SSPH_NEP<-NEP_SSPH_sub %>% group_by(MAZ_Acrony) %>%
  summarise(across(starts_with("NEP_SSPH_SpringAvg"), mean))%>%
  as.data.frame()

SST_SSC<-SSC_SST_sub %>% group_by(MAZ_Acrony) %>%
  summarise(across(starts_with("SSC_SST_SpringAvg"), mean,na.rm=TRUE)) %>%
  as.data.frame()
SSS_SSC<-SSC_SSS_sub %>% group_by(MAZ_Acrony) %>%
  summarise(across(starts_with("SSC_SSS_SpringAvg"), mean,na.rm=TRUE))%>%
  as.data.frame()

#Merge spring differences back together for each variable 
#SST
SST_SpringAvg1<-merge(SST_BCCM, SST_NEP,  by="MAZ_Acrony", all=TRUE)
SST_SpringAvg<-merge(SST_SpringAvg1, SST_SSC, by="MAZ_Acrony", all=TRUE)
SST_SpringAvg= subset(SST_SpringAvg, select = -c(geometry,geometry.x, geometry.y))%>% 
  mutate(across(where(is.numeric), ~ round(., 2)))
#SSS
SSS_SpringAvg1<-merge(SSS_BCCM,SSS_NEP,  by="MAZ_Acrony", all=TRUE) 
SSS_SpringAvg<-merge(SSS_SpringAvg1, SSS_SSC, by="MAZ_Acrony", all=TRUE)
SSS_SpringAvg= subset(SSS_SpringAvg, select = -c(geometry,geometry.x, geometry.y))%>% 
  mutate(across(where(is.numeric), ~ round(., 2)))  
#SSPH  
SSPH_SpringAvg<-merge(SSPH_BCCM,SSPH_NEP,  by="MAZ_Acrony", all=TRUE) 
SSPH_SpringAvg= subset(SSPH_SpringAvg, select = -c(geometry.x, geometry.y)) %>% 
  mutate(across(where(is.numeric), ~ round(., 2))) 

SpringAvg1<- merge(SST_SpringAvg, SSS_SpringAvg, by="MAZ_Acrony", all=TRUE)
SpringAvg<- merge(SpringAvg1, SSPH_SpringAvg, by="MAZ_Acrony", all=TRUE)
### Put data in tables
#rename columns for tables
colAll<-c('MAZ', 'BCCM','NEP',  'SSC','BCCM','NEP', 'SSC','BCCM','NEP' )
colNEPBCCM<- c('MAZ','BCCM','NEP')
colnames(SpringAvg)<- colAll
#colnames(SST_SpringDiff)<- colAll
#colnames(SSS_SpringDiff)<- colAll
#colnames(SSPH_SpringDiff)<- colNEPBCCM

# Create table ## Not sure why there are a bunch of extra columns 
grid.newpage()
grid.table(SpringAvg)
#grid.table(SST_SpringDiff)
#grid.newpage()
#grid.table(SSS_SpringDiff)
#grid.newpage()
#grid.table(SSPH_SpringDiff)

#remove variable no longer needed 
rm(SST_SpringAvg1, SSS_SpringAvg1,SpringAvg1, SSS_SpringAvg, SST_SpringAvg, SSPH_SpringAvg,
   SSS_BCCM, SSS_NEP, SSS_SSC, SST_BCCM, SST_NEP, SST_SSC, SSPH_BCCM, SSPH_NEP)

######### Project spring SST Descriptions ###############
######### Spring difference ###############
#1) Calculate difference between the historic and future spring values in new column 
BCCM_SST_sub$BCCM_SST_Spring_Diff <-((BCCM_SST_sub$SST_45_04+BCCM_SST_sub$SST_45_05+BCCM_SST_sub$SST_45_06)/3)-((BCCM_SST_sub$SST_H_04 + BCCM_SST_sub$SST_H_05+BCCM_SST_sub$SST_H_06)/3)
BCCM_SSS_sub$BCCM_SSS_Spring_Diff <-((BCCM_SSS_sub$SSS_45_04+BCCM_SSS_sub$SSS_45_05+BCCM_SSS_sub$SSS_45_06)/3)-((BCCM_SSS_sub$SSS_H_04 + BCCM_SSS_sub$SSS_H_05+BCCM_SSS_sub$SSS_H_06)/3)
BCCM_SSPH_sub$BCCM_SSPH_Spring_Diff <-((BCCM_SSPH_sub$SSPH_45_04+BCCM_SSPH_sub$SSPH_45_05+BCCM_SSPH_sub$SSPH_45_06)/3)-((BCCM_SSPH_sub$SSPH_H_04 + BCCM_SSPH_sub$SSPH_H_05+BCCM_SSPH_sub$SSPH_H_06)/3)

NEP_SST_sub$NEP_SST_Spring_Diff <-((NEP_SST_sub$SST_45_04+NEP_SST_sub$SST_45_05+NEP_SST_sub$SST_45_06)/3)-((NEP_SST_sub$SST_H_04 + NEP_SST_sub$SST_H_05+NEP_SST_sub$SST_H_06)/3)
NEP_SSS_sub$NEP_SSS_Spring_Diff <-((NEP_SSS_sub$SSS_45_04+ NEP_SSS_sub$SSS_45_05+NEP_SSS_sub$SSS_45_06)/3)-((NEP_SSS_sub$SSS_H_04 + NEP_SSS_sub$SSS_H_05+ NEP_SSS_sub$SSS_H_06) /3)
NEP_SSPH_sub$NEP_SSPH_Spring_Diff <-((NEP_SSPH_sub$SSPH_45_04+ NEP_SSPH_sub$SSPH_45_05+NEP_SSPH_sub$SSPH_45_06)/3)-((NEP_SSPH_sub$SSPH_H_04 + NEP_SSPH_sub$SSPH_H_05 +NEP_SSPH_sub$SSPH_H_06 )/3)

SSC_SST_sub$SSC_SST_Spring_Diff <-((SSC_SST_sub$SST_45_04+SSC_SST_sub$SST_45_05+SSC_SST_sub$SST_45_06)/3)-((SSC_SST_sub$SST_H_04 + SSC_SST_sub$SST_H_05+SSC_SST_sub$SST_H_06)/3)
SSC_SSS_sub$SSC_SSS_Spring_Diff <-((SSC_SSS_sub$SSS_45_04+SSC_SSS_sub$SSS_45_05+SSC_SSS_sub$SSS_45_06)/3)-((SSC_SSS_sub$SSS_H_04 + SSC_SSS_sub$SSS_H_05+SSC_SSS_sub$SSS_H_06)/3)

# 2) average differences across MAZs 
###PLEASE double check this code, not sure its doing the right thing
SST_BCCM<- BCCM_SST_sub %>% group_by(MAZ_Acrony) %>%
  summarise(across(starts_with("BCCM_SST_Spring_Diff"), mean, na.rm=TRUE)) %>%
  as.data.frame() 
SSS_BCCM<-BCCM_SSS_sub %>% group_by(MAZ_Acrony) %>%
  summarise(across(starts_with("BCCM_SSS_Spring_Diff"), mean, na.rm=TRUE ))%>%
  as.data.frame()
SSPH_BCCM<-BCCM_SSPH_sub %>% group_by(MAZ_Acrony) %>%
  summarise(across(starts_with("BCCM_SSPH_Spring_Diff"), mean,na.rm=TRUE ))%>%
  as.data.frame()

SST_NEP<-NEP_SST_sub %>% group_by(MAZ_Acrony) %>%
  summarise(across(starts_with("NEP_SST_Spring_Diff"), mean))%>%
  as.data.frame()
SSS_NEP<-NEP_SSS_sub %>% group_by(MAZ_Acrony) %>%
  summarise(across(starts_with("NEP_SSS_Spring_Diff"), mean))%>%
  as.data.frame()
SSPH_NEP<-NEP_SSPH_sub %>% group_by(MAZ_Acrony) %>%
  summarise(across(starts_with("NEP_SSPH_Spring_Diff"), mean))%>%
  as.data.frame()

SST_SSC<-SSC_SST_sub %>% group_by(MAZ_Acrony) %>%
  summarise(across(starts_with("SSC_SST_Spring_Diff"), mean,na.rm=TRUE )) %>%
  as.data.frame()
SSS_SSC<-SSC_SSS_sub %>% group_by(MAZ_Acrony) %>%
  summarise(across(starts_with("SSC_SSS_Spring_Diff"), mean,na.rm=TRUE ))%>%
  as.data.frame()

#Merge spring differences back together for each variable 
#SST
SST_SpringDiff1<-merge(SST_BCCM, SST_NEP,  by="MAZ_Acrony", all=TRUE)
SST_SpringDiff<-merge(SST_SpringDiff1, SST_SSC, by="MAZ_Acrony", all=TRUE)
SST_SpringDiff= subset(SST_SpringDiff, select = -c(geometry,geometry.x, geometry.y))%>% 
  mutate(across(where(is.numeric), ~ round(., 2)))
#SSS
SSS_SpringDiff1<-merge(SSS_BCCM,SSS_NEP,  by="MAZ_Acrony", all=TRUE) 
SSS_SpringDiff<-merge(SSS_SpringDiff1, SSS_SSC, by="MAZ_Acrony", all=TRUE)
SSS_SpringDiff= subset(SSS_SpringDiff, select = -c(geometry,geometry.x, geometry.y))%>% 
  mutate(across(where(is.numeric), ~ round(., 2)))  
#SSPH  
SSPH_SpringDiff<-merge(SSPH_BCCM,SSPH_NEP,  by="MAZ_Acrony", all=TRUE) 
SSPH_SpringDiff= subset(SSPH_SpringDiff, select = -c(geometry.x, geometry.y)) %>% 
  mutate(across(where(is.numeric), ~ round(., 2))) 

SpringDiff1<- merge(SST_SpringDiff, SSS_SpringDiff, by="MAZ_Acrony", all=TRUE)
SpringDiff<- merge(SpringDiff1, SSPH_SpringDiff, by="MAZ_Acrony", all=TRUE)
### Put data in tables
#rename columns for tables
colAll<-c('MAZ', 'BCCM','NEP',  'SSC','BCCM','NEP', 'SSC','BCCM','NEP' )
colNEPBCCM<- c('MAZ','BCCM','NEP')
colnames(SpringDiff)<- colAll
#colnames(SST_SpringDiff)<- colAll
#colnames(SSS_SpringDiff)<- colAll
#colnames(SSPH_SpringDiff)<- colNEPBCCM

# Create table ## Not sure why there are a bunch of extra columns 
grid.newpage()
grid.table(SpringDiff)
#grid.table(SST_SpringDiff)
#grid.newpage()
#grid.table(SSS_SpringDiff)
#grid.newpage()
#grid.table(SSPH_SpringDiff)

#remove variable no longer needed 
rm(SST_SpringDiff1, SSS_SpringDiff1,SpringDiff1, SSS_SpringDiff, SST_SpringDiff, SSPH_SpringDiff,
   SSS_BCCM, SSS_NEP, SSS_SSC, SST_BCCM, SST_NEP, SST_SSC, SSPH_BCCM, SSPH_NEP)

############# Spring mean Projected temperature #######
BCCM_SST_sub$BCCM_SST_SpringAvg <-((BCCM_SST_sub$SST_45_04+BCCM_SST_sub$SST_45_05+BCCM_SST_sub$SST_45_06)/3)
BCCM_SSS_sub$BCCM_SSS_SpringAvg <-((BCCM_SSS_sub$SSS_45_04+BCCM_SSS_sub$SSS_45_05+BCCM_SSS_sub$SSS_45_06)/3)
BCCM_SSPH_sub$BCCM_SSPH_SpringAvg <-((BCCM_SSPH_sub$SSPH_45_04+BCCM_SSPH_sub$SSPH_45_05+BCCM_SSPH_sub$SSPH_45_06)/3)

NEP_SST_sub$NEP_SST_SpringAvg <-((NEP_SST_sub$SST_45_04+NEP_SST_sub$SST_45_05+NEP_SST_sub$SST_45_06)/3)
NEP_SSS_sub$NEP_SSS_SpringAvg <-((NEP_SSS_sub$SSS_45_04+ NEP_SSS_sub$SSS_45_05+NEP_SSS_sub$SSS_45_06)/3)
NEP_SSPH_sub$NEP_SSPH_SpringAvg <-((NEP_SSPH_sub$SSPH_45_04+ NEP_SSPH_sub$SSPH_45_05+NEP_SSPH_sub$SSPH_45_06)/3)

SSC_SST_sub$SSC_SST_SpringAvg <-((SSC_SST_sub$SST_45_04+SSC_SST_sub$SST_45_05+SSC_SST_sub$SST_45_06)/3)
SSC_SSS_sub$SSC_SSS_SpringAvg <-((SSC_SSS_sub$SSS_45_04+SSC_SSS_sub$SSS_45_05+SSC_SSS_sub$SSS_45_06)/3)

# 2) average differences across MAZs 
###PLEASE double check this code, not sure its doing the right thing
SST_BCCM<- BCCM_SST_sub %>% group_by(MAZ_Acrony) %>%
  summarise(across(starts_with("BCCM_SST_SpringAvg"), mean, na.rm=TRUE)) %>%
  as.data.frame() 
SSS_BCCM<-BCCM_SSS_sub %>% group_by(MAZ_Acrony) %>%
  summarise(across(starts_with("BCCM_SSS_SpringAvg"), mean, na.rm=TRUE ))%>%
  as.data.frame()
SSPH_BCCM<-BCCM_SSPH_sub %>% group_by(MAZ_Acrony) %>%
  summarise(across(starts_with("BCCM_SSPH_SpringAvg"), mean,na.rm=TRUE ))%>%
  as.data.frame()

SST_NEP<-NEP_SST_sub %>% group_by(MAZ_Acrony) %>%
  summarise(across(starts_with("NEP_SST_SpringAvg"), mean))%>%
  as.data.frame()
SSS_NEP<-NEP_SSS_sub %>% group_by(MAZ_Acrony) %>%
  summarise(across(starts_with("NEP_SSS_SpringAvg"), mean))%>%
  as.data.frame()
SSPH_NEP<-NEP_SSPH_sub %>% group_by(MAZ_Acrony) %>%
  summarise(across(starts_with("NEP_SSPH_SpringAvg"), mean))%>%
  as.data.frame()

SST_SSC<-SSC_SST_sub %>% group_by(MAZ_Acrony) %>%
  summarise(across(starts_with("SSC_SST_SpringAvg"), mean)) %>%
  as.data.frame()
SSS_SSC<-SSC_SSS_sub %>% group_by(MAZ_Acrony) %>%
  summarise(across(starts_with("SSC_SSS_SpringAvg"), mean))%>%
  as.data.frame()

#Merge spring differences back together for each variable 
#SST
SST_SpringAvg1<-merge(SST_BCCM, SST_NEP,  by="MAZ_Acrony", all=TRUE)
SST_SpringAvg<-merge(SST_SpringAvg1, SST_SSC, by="MAZ_Acrony", all=TRUE)
SST_SpringAvg= subset(SST_SpringAvg, select = -c(geometry,geometry.x, geometry.y))%>% 
  mutate(across(where(is.numeric), ~ round(., 2)))
#SSS
SSS_SpringAvg1<-merge(SSS_BCCM,SSS_NEP,  by="MAZ_Acrony", all=TRUE) 
SSS_SpringAvg<-merge(SSS_SpringAvg1, SSS_SSC, by="MAZ_Acrony", all=TRUE)
SSS_SpringAvg= subset(SSS_SpringAvg, select = -c(geometry,geometry.x, geometry.y))%>% 
  mutate(across(where(is.numeric), ~ round(., 2)))  
#SSPH  
SSPH_SpringAvg<-merge(SSPH_BCCM,SSPH_NEP,  by="MAZ_Acrony", all=TRUE) 
SSPH_SpringAvg= subset(SSPH_SpringAvg, select = -c(geometry.x, geometry.y)) %>% 
  mutate(across(where(is.numeric), ~ round(., 2))) 

SpringAvg1<- merge(SST_SpringAvg, SSS_SpringAvg, by="MAZ_Acrony", all=TRUE)
SpringAvg<- merge(SpringAvg1, SSPH_SpringAvg, by="MAZ_Acrony", all=TRUE)
### Put data in tables
#rename columns for tables
colAll<-c('MAZ', 'BCCM','NEP',  'SSC','BCCM','NEP', 'SSC','BCCM','NEP' )
colNEPBCCM<- c('MAZ','BCCM','NEP')
colnames(SpringAvg)<- colAll
#colnames(SST_SpringDiff)<- colAll
#colnames(SSS_SpringDiff)<- colAll
#colnames(SSPH_SpringDiff)<- colNEPBCCM

# Create table ## Not sure why there are a bunch of extra columns 
grid.newpage()
grid.table(SpringAvg)
#grid.table(SST_SpringDiff)
#grid.newpage()
#grid.table(SSS_SpringDiff)
#grid.newpage()
#grid.table(SSPH_SpringDiff)

#remove variable no longer needed 
rm(SST_SpringAvg1, SSS_SpringAvg1,SpringAvg1, SSS_SpringAvg, SST_SpringAvg, SSPH_SpringAvg,
   SSS_BCCM, SSS_NEP, SSS_SSC, SST_BCCM, SST_NEP, SST_SSC, SSPH_BCCM, SSPH_NEP)

######### Decadal rate of change ######
# calculate the decadal ROC by first calculating the annual average for the historic
#and future time periods, substracting the historic ann avg from the future ann avg,
# and dividing the difference by 6 (the number of decades between the future and historic time periods)
BCCM_SST_sub$BCCM_SST_DecadalROC <-
  (((BCCM_SST_sub$SST_45_01+BCCM_SST_sub$SST_45_02+BCCM_SST_sub$SST_45_03+
       BCCM_SST_sub$SST_45_04+BCCM_SST_sub$SST_45_05+BCCM_SST_sub$SST_45_06+
       BCCM_SST_sub$SST_45_07+BCCM_SST_sub$SST_45_08+BCCM_SST_sub$SST_45_09+
       BCCM_SST_sub$SST_45_10+BCCM_SST_sub$SST_45_11+BCCM_SST_sub$SST_45_12)/12)-
     ((BCCM_SST_sub$SST_H_01+BCCM_SST_sub$SST_H_02+BCCM_SST_sub$SST_H_03+
         BCCM_SST_sub$SST_H_04+BCCM_SST_sub$SST_H_05+BCCM_SST_sub$SST_H_06+
         BCCM_SST_sub$SST_H_07+BCCM_SST_sub$SST_H_08+BCCM_SST_sub$SST_H_09+
         BCCM_SST_sub$SST_H_10+BCCM_SST_sub$SST_H_11+BCCM_SST_sub$SST_H_12
     )/12))/6 #6= number of decades between the midpoint of the historic and future predictions

BCCM_SSS_sub$BCCM_SSS_DecadalROC<-
  (((BCCM_SSS_sub$SSS_45_01+BCCM_SSS_sub$SSS_45_02+BCCM_SSS_sub$SSS_45_03+
       BCCM_SSS_sub$SSS_45_04+BCCM_SSS_sub$SSS_45_05+BCCM_SSS_sub$SSS_45_06+
       BCCM_SSS_sub$SSS_45_07+BCCM_SSS_sub$SSS_45_08+BCCM_SSS_sub$SSS_45_09+
       BCCM_SSS_sub$SSS_45_10+BCCM_SSS_sub$SSS_45_11+BCCM_SSS_sub$SSS_45_12)/12)-
     ((BCCM_SSS_sub$SSS_H_01+BCCM_SSS_sub$SSS_H_02+BCCM_SSS_sub$SSS_H_03+
         BCCM_SSS_sub$SSS_H_04+BCCM_SSS_sub$SSS_H_05+BCCM_SSS_sub$SSS_H_06+
         BCCM_SSS_sub$SSS_H_07+BCCM_SSS_sub$SSS_H_08+BCCM_SSS_sub$SSS_H_09+
         BCCM_SSS_sub$SSS_H_10+BCCM_SSS_sub$SSS_H_11+BCCM_SSS_sub$SSS_H_12)/12))/6

BCCM_SSPH_sub$BCCM_SSPH_DecadalROC<-
  (((BCCM_SSPH_sub$SSPH_45_01+BCCM_SSPH_sub$SSPH_45_02+BCCM_SSPH_sub$SSPH_45_03+
       BCCM_SSPH_sub$SSPH_45_04+BCCM_SSPH_sub$SSPH_45_05+BCCM_SSPH_sub$SSPH_45_06+
       BCCM_SSPH_sub$SSPH_45_07+BCCM_SSPH_sub$SSPH_45_08+BCCM_SSPH_sub$SSPH_45_09+
       BCCM_SSPH_sub$SSPH_45_10+BCCM_SSPH_sub$SSPH_45_11+BCCM_SSPH_sub$SSPH_45_12)/12)-
     ((BCCM_SSPH_sub$SSPH_H_01+BCCM_SSPH_sub$SSPH_H_02+BCCM_SSPH_sub$SSPH_H_03+
         BCCM_SSPH_sub$SSPH_H_04+BCCM_SSPH_sub$SSPH_H_05+BCCM_SSPH_sub$SSPH_H_06+
         BCCM_SSPH_sub$SSPH_H_07+BCCM_SSPH_sub$SSPH_H_08+BCCM_SSPH_sub$SSPH_H_09+
         BCCM_SSPH_sub$SSPH_H_10+BCCM_SSPH_sub$SSPH_H_11+BCCM_SSPH_sub$SSPH_H_12)/12))/6

NEP_SST_sub$NEP_SST_DecadalROC <-
  (((NEP_SST_sub$SST_45_01+NEP_SST_sub$SST_45_02+NEP_SST_sub$SST_45_03+
       NEP_SST_sub$SST_45_04+NEP_SST_sub$SST_45_05+NEP_SST_sub$SST_45_06+
       NEP_SST_sub$SST_45_07+NEP_SST_sub$SST_45_08+NEP_SST_sub$SST_45_09+
       NEP_SST_sub$SST_45_10+NEP_SST_sub$SST_45_11+NEP_SST_sub$SST_45_12)/12)-
     ((NEP_SST_sub$SST_H_01+NEP_SST_sub$SST_H_02+NEP_SST_sub$SST_H_03+
         NEP_SST_sub$SST_H_04+NEP_SST_sub$SST_H_05+NEP_SST_sub$SST_H_06+
         NEP_SST_sub$SST_H_07+NEP_SST_sub$SST_H_08+NEP_SST_sub$SST_H_09+
         NEP_SST_sub$SST_H_10+NEP_SST_sub$SST_H_11+NEP_SST_sub$SST_H_12)/12))/6

NEP_SSS_sub$NEP_SSS_DecadalROC<-
  (((NEP_SSS_sub$SSS_45_01+NEP_SSS_sub$SSS_45_02+NEP_SSS_sub$SSS_45_03+
       NEP_SSS_sub$SSS_45_04+NEP_SSS_sub$SSS_45_05+NEP_SSS_sub$SSS_45_06+
       NEP_SSS_sub$SSS_45_07+NEP_SSS_sub$SSS_45_08+NEP_SSS_sub$SSS_45_09+
       NEP_SSS_sub$SSS_45_10+NEP_SSS_sub$SSS_45_11+NEP_SSS_sub$SSS_45_12)/12)-
     ((NEP_SSS_sub$SSS_H_01+NEP_SSS_sub$SSS_H_02+NEP_SSS_sub$SSS_H_03+
         NEP_SSS_sub$SSS_H_04+NEP_SSS_sub$SSS_H_05+NEP_SSS_sub$SSS_H_06+
         NEP_SSS_sub$SSS_H_07+NEP_SSS_sub$SSS_H_08+NEP_SSS_sub$SSS_H_09+
         NEP_SSS_sub$SSS_H_10+NEP_SSS_sub$SSS_H_11+NEP_SSS_sub$SSS_H_12)/12))/6

NEP_SSPH_sub$NEP_SSPH_DecadalROC<-
  (((NEP_SSPH_sub$SSPH_45_01+NEP_SSPH_sub$SSPH_45_02+NEP_SSPH_sub$SSPH_45_03+
       NEP_SSPH_sub$SSPH_45_04+NEP_SSPH_sub$SSPH_45_05+NEP_SSPH_sub$SSPH_45_06+
       NEP_SSPH_sub$SSPH_45_07+NEP_SSPH_sub$SSPH_45_08+NEP_SSPH_sub$SSPH_45_09+
       NEP_SSPH_sub$SSPH_45_10+NEP_SSPH_sub$SSPH_45_11+NEP_SSPH_sub$SSPH_45_12)/12)-
     ((NEP_SSPH_sub$SSPH_H_01+NEP_SSPH_sub$SSPH_H_02+NEP_SSPH_sub$SSPH_H_03+
         NEP_SSPH_sub$SSPH_H_04+NEP_SSPH_sub$SSPH_H_05+NEP_SSPH_sub$SSPH_H_06+
         NEP_SSPH_sub$SSPH_H_07+NEP_SSPH_sub$SSPH_H_08+NEP_SSPH_sub$SSPH_H_09+
         NEP_SSPH_sub$SSPH_H_10+NEP_SSPH_sub$SSPH_H_11+NEP_SSPH_sub$SSPH_H_12)/12))/6

SSC_SST_sub$SSC_SST_DecadalROC <-
  (((SSC_SST_sub$SST_45_01+SSC_SST_sub$SST_45_02+SSC_SST_sub$SST_45_03+
       SSC_SST_sub$SST_45_04+SSC_SST_sub$SST_45_05+SSC_SST_sub$SST_45_06+
       SSC_SST_sub$SST_45_07+SSC_SST_sub$SST_45_08+SSC_SST_sub$SST_45_09+
       SSC_SST_sub$SST_45_10+SSC_SST_sub$SST_45_11+SSC_SST_sub$SST_45_12)/12)-
     ((SSC_SST_sub$SST_H_01+SSC_SST_sub$SST_H_02+SSC_SST_sub$SST_H_03+
         SSC_SST_sub$SST_H_04+SSC_SST_sub$SST_H_05+SSC_SST_sub$SST_H_06+
         SSC_SST_sub$SST_H_07+SSC_SST_sub$SST_H_08+SSC_SST_sub$SST_H_09+
         SSC_SST_sub$SST_H_10+SSC_SST_sub$SST_H_11+SSC_SST_sub$SST_H_12)/12))/6

SSC_SSS_sub$SSC_SSS_DecadalROC<-
  (((SSC_SSS_sub$SSS_45_01+SSC_SSS_sub$SSS_45_02+SSC_SSS_sub$SSS_45_03+
       SSC_SSS_sub$SSS_45_04+SSC_SSS_sub$SSS_45_05+SSC_SSS_sub$SSS_45_06+
       SSC_SSS_sub$SSS_45_07+SSC_SSS_sub$SSS_45_08+SSC_SSS_sub$SSS_45_09+
       SSC_SSS_sub$SSS_45_10+SSC_SSS_sub$SSS_45_11+SSC_SSS_sub$SSS_45_12)/12)-
     ((SSC_SSS_sub$SSS_H_01+SSC_SSS_sub$SSS_H_02+SSC_SSS_sub$SSS_H_03+
         SSC_SSS_sub$SSS_H_04+SSC_SSS_sub$SSS_H_05+SSC_SSS_sub$SSS_H_06+
         SSC_SSS_sub$SSS_H_07+SSC_SSS_sub$SSS_H_08+SSC_SSS_sub$SSS_H_09+
         SSC_SSS_sub$SSS_H_10+SSC_SSS_sub$SSS_H_11+SSC_SSS_sub$SSS_H_12)/12))/6

# 2) average differences across MAZs 
## Same issues with this step as in 
SST_BCCM<- BCCM_SST_sub %>% group_by(MAZ_Acrony) %>%
  summarise(across(starts_with("BCCM_SST_DecadalROC"), mean, na.rm=TRUE)) %>%
  as.data.frame() 
SSS_BCCM<-BCCM_SSS_sub %>% group_by(MAZ_Acrony) %>%
  summarise(across(starts_with("BCCM_SSS_DecadalROC"), mean, na.rm=TRUE))%>%
  as.data.frame()
SSPH_BCCM<-BCCM_SSPH_sub %>% group_by(MAZ_Acrony) %>%
  summarise(across(starts_with("BCCM_SSPH_DecadalROC"), mean, na.rm=TRUE))%>%
  as.data.frame()

SST_NEP<-NEP_SST_sub %>% group_by(MAZ_Acrony) %>%
  summarise(across(starts_with("NEP_SST_DecadalROC"), mean, na.rm=TRUE))%>%
  as.data.frame()
SSS_NEP<-NEP_SSS_sub %>% group_by(MAZ_Acrony) %>%
  summarise(across(starts_with("NEP_SSS_DecadalROC"), mean, na.rm=TRUE))%>%
  as.data.frame()
SSPH_NEP<-NEP_SSPH_sub %>% group_by(MAZ_Acrony) %>%
  summarise(across(starts_with("NEP_SSPH_DecadalROC"), mean, na.rm=TRUE))%>%
  as.data.frame()

SST_SSC<-SSC_SST_sub %>% group_by(MAZ_Acrony) %>%
  summarise(across(starts_with("SSC_SST_DecadalROC"), mean, na.rm=TRUE)) %>%
  as.data.frame()
SSS_SSC<-SSC_SSS_sub %>% group_by(MAZ_Acrony) %>%
  summarise(across(starts_with("SSC_SSS_DecadalROC"), mean, na.rm=TRUE))%>%
  as.data.frame()

# Combine the Decadal ROC together based on the variable
SST_DecROC1<-merge(SST_BCCM,SST_NEP,  by="MAZ_Acrony", all=TRUE)
SST_DecROC<-merge(SST_DecROC1, SST_SSC, by="MAZ_Acrony", all=TRUE)
SST_DecROC= subset(SST_DecROC, select = -c(geometry,geometry.x, geometry.y))%>% 
  mutate(across(where(is.numeric), ~ round(., 2)))
#SSS
SSS_DecROC1<-merge(SSS_BCCM,SSS_NEP,  by="MAZ_Acrony", all=TRUE) 
SSS_DecROC<-merge(SSS_DecROC1, SSS_SSC, by="MAZ_Acrony", all=TRUE)
SSS_DecROC= subset(SSS_DecROC, select = -c(geometry,geometry.x, geometry.y))%>% 
  mutate(across(where(is.numeric), ~ round(., 2)))  
#SSPH  
SSPH_DecROC<-merge(SSPH_BCCM,SSPH_NEP,  by="MAZ_Acrony", all=TRUE) 
SSPH_DecROC= subset(SSPH_DecROC, select = -c(geometry.x, geometry.y))  %>% 
  mutate(across(where(is.numeric), ~ round(., 2)))

DecROC1<- merge(SST_DecROC, SSS_DecROC, by="MAZ_Acrony", all=TRUE)
DecROC<- merge(DecROC1, SSPH_DecROC, by="MAZ_Acrony", all=TRUE)

### Put data in tables
#rename columns for tables
colnames(DecROC)<- colAll
#colnames(SST_DecROC)<- colAll
#colnames(SSS_DecROC)<- colAll
#colnames(SSPH_DecROC)<- colNEPBCCM

# Create table ## Not sure why there are a bunch of extra columns 
grid.newpage()
grid.table(DecROC)
#grid.newpage()
#grid.table(SST_DecROC)
#grid.newpage()
#grid.table(SSS_DecROC)
#grid.newpage()
#grid.table(SSPH_DecROC)

#remove variable no longer needed 
rm(SST_DecROC, SSS_DecROC, SSPH_DecROC,DecROC1, SSS_DecROC1,SST_DecROC1,SSPH_BCCM, 
   SSPH_NEP, SSS_BCCM, SSS_NEP, SSS_SSC, SST_BCCM, SST_NEP, SST_SSC)

##### Cumulative Impact score ####
# join summarize CI score by MAZ acronym
CI_MAZ<-CI_points_sub %>% group_by(MAZ_Acrony) %>%
  summarise(across(starts_with("Cumul_Impa"), mean))%>%
  as.data.frame()
CI_MAZ= subset(CI_MAZ, select = -c(geometry)) %>% 
  mutate(across(where(is.numeric), ~ round(., 2)))

colCI<-c('MAZ', 'Total CI Score')
colnames(CI_MAZ)<- colCI
grid.newpage()
grid.table(CI_MAZ)

######### Seasonality #########
# avg each variable/model values for each month by MAZ
BCCM_SST_Month_Avg<-BCCM_SST_sub %>% group_by(MAZ_Acrony) %>%
  summarise(across(starts_with("SST_"), mean, na.rm=TRUE))%>%
  as.data.frame()
BCCM_SST_Month_Avg= subset(BCCM_SST_Month_Avg, select = -c(geometry))

BCCM_SSS_Month_Avg<-BCCM_SSS_sub %>% group_by(MAZ_Acrony) %>%
  summarise(across(starts_with("SSS_"), mean, na.rm=TRUE))%>%
  as.data.frame()
BCCM_SSS_Month_Avg= subset(BCCM_SSS_Month_Avg, select = -c(geometry))

BCCM_SSPH_Month_Avg<-BCCM_SSPH_sub %>% group_by(MAZ_Acrony) %>%
  summarise(across(starts_with("SSPH_"), mean, na.rm=TRUE))%>%
  as.data.frame()
BCCM_SSPH_Month_Avg= subset(BCCM_SSPH_Month_Avg, select = -c(geometry))

NEP_SST_Month_Avg<-NEP_SST_sub %>% group_by(MAZ_Acrony) %>%
  summarise(across(starts_with("SST_"), mean, na.rm=TRUE))%>%
  as.data.frame()
NEP_SST_Month_Avg= subset(NEP_SST_Month_Avg, select = -c(geometry))

NEP_SSS_Month_Avg<-NEP_SSS_sub %>% group_by(MAZ_Acrony) %>%
  summarise(across(starts_with("SSS_"), mean, na.rm=TRUE))%>%
  as.data.frame()
NEP_SSS_Month_Avg= subset(NEP_SSS_Month_Avg, select = -c(geometry))

NEP_SSPH_Month_Avg<-NEP_SSPH_sub %>% group_by(MAZ_Acrony) %>%
  summarise(across(starts_with("SSPH_"), mean, na.rm=TRUE))%>%
  as.data.frame()
NEP_SSPH_Month_Avg= subset(NEP_SSPH_Month_Avg, select = -c(geometry))

SSC_SST_Month_Avg<-SSC_SST_sub %>% group_by(MAZ_Acrony) %>%
  summarise(across(starts_with("SST_"), mean, na.rm=TRUE))%>%
  as.data.frame()
SSC_SST_Month_Avg= subset(SSC_SST_Month_Avg, select = -c(geometry))

SSC_SSS_Month_Avg<-SSC_SSS_sub %>% group_by(MAZ_Acrony) %>%
  summarise(across(starts_with("SSS_"), mean, na.rm=TRUE))%>%
  as.data.frame()
SSC_SSS_Month_Avg= subset(SSC_SSS_Month_Avg, select = -c(geometry))

# Combine all MAZ data into new data frames
GSTR_monthly <- merge()
SSPHDecROC<-merge(ROC_SSPH_NEP, ROC_SSPH_BCCM, by="MAZ_Acrony", all=TRUE)

#ggplot(SSC_SST_Month_Avg, aes(x=month, y=temperature))

######### Projected/future absolute value ########
#1) Calculate projected annual averages  
BCCM_SST_sub$BCCM_SST_45AnnAvg <-
  (BCCM_SST_sub$SST_45_01+BCCM_SST_sub$SST_45_02+BCCM_SST_sub$SST_45_03+
     BCCM_SST_sub$SST_45_04+BCCM_SST_sub$SST_45_05+BCCM_SST_sub$SST_45_06+
     BCCM_SST_sub$SST_45_07+BCCM_SST_sub$SST_45_08+BCCM_SST_sub$SST_45_09+
     BCCM_SST_sub$SST_45_10+BCCM_SST_sub$SST_45_11+BCCM_SST_sub$SST_45_12)/12

BCCM_SSS_sub$BCCM_SSS_45AnnAvg<-
  (BCCM_SSS_sub$SSS_45_01+BCCM_SSS_sub$SSS_45_02+BCCM_SSS_sub$SSS_45_03+
     BCCM_SSS_sub$SSS_45_04+BCCM_SSS_sub$SSS_45_05+BCCM_SSS_sub$SSS_45_06+
     BCCM_SSS_sub$SSS_45_07+BCCM_SSS_sub$SSS_45_08+BCCM_SSS_sub$SSS_45_09+
     BCCM_SSS_sub$SSS_45_10+BCCM_SSS_sub$SSS_45_11+BCCM_SSS_sub$SSS_45_12)/12

BCCM_SSPH_sub$BCCM_SSPH_45AnnAvg<-
  (BCCM_SSPH_sub$SSPH_45_01+BCCM_SSPH_sub$SSPH_45_02+BCCM_SSPH_sub$SSPH_45_03+
     BCCM_SSPH_sub$SSPH_45_04+BCCM_SSPH_sub$SSPH_45_05+BCCM_SSPH_sub$SSPH_45_06+
     BCCM_SSPH_sub$SSPH_45_07+BCCM_SSPH_sub$SSPH_45_08+BCCM_SSPH_sub$SSPH_45_09+
     BCCM_SSPH_sub$SSPH_45_10+BCCM_SSPH_sub$SSPH_45_11+BCCM_SSPH_sub$SSPH_45_12)/12

NEP_SST_sub$NEP_SST_45AnnAvg <-
  (NEP_SST_sub$SST_45_01+NEP_SST_sub$SST_45_02+NEP_SST_sub$SST_45_03+
     NEP_SST_sub$SST_45_04+NEP_SST_sub$SST_45_05+NEP_SST_sub$SST_45_06+
     NEP_SST_sub$SST_45_07+NEP_SST_sub$SST_45_08+NEP_SST_sub$SST_45_09+
     NEP_SST_sub$SST_45_10+NEP_SST_sub$SST_45_11+NEP_SST_sub$SST_45_12)/12

NEP_SSS_sub$NEP_SSS_45AnnAvg<-
  (NEP_SSS_sub$SSS_45_01+NEP_SSS_sub$SSS_45_02+NEP_SSS_sub$SSS_45_03+
     NEP_SSS_sub$SSS_45_04+NEP_SSS_sub$SSS_45_05+NEP_SSS_sub$SSS_45_06+
     NEP_SSS_sub$SSS_45_07+NEP_SSS_sub$SSS_45_08+NEP_SSS_sub$SSS_45_09+
     NEP_SSS_sub$SSS_45_10+NEP_SSS_sub$SSS_45_11+NEP_SSS_sub$SSS_45_12)/12

NEP_SSPH_sub$NEP_SSPH_45AnnAvg<-
  (NEP_SSPH_sub$SSPH_45_01+NEP_SSPH_sub$SSPH_45_02+NEP_SSPH_sub$SSPH_45_03+
     NEP_SSPH_sub$SSPH_45_04+NEP_SSPH_sub$SSPH_45_05+NEP_SSPH_sub$SSPH_45_06+
     NEP_SSPH_sub$SSPH_45_07+NEP_SSPH_sub$SSPH_45_08+NEP_SSPH_sub$SSPH_45_09+
     NEP_SSPH_sub$SSPH_45_10+NEP_SSPH_sub$SSPH_45_11+NEP_SSPH_sub$SSPH_45_12)/12

SSC_SST_sub$SSC_SST_45AnnAvg <-
  (SSC_SST_sub$SST_45_01+SSC_SST_sub$SST_45_02+SSC_SST_sub$SST_45_03+
     SSC_SST_sub$SST_45_04+SSC_SST_sub$SST_45_05+SSC_SST_sub$SST_45_06+
     SSC_SST_sub$SST_45_07+SSC_SST_sub$SST_45_08+SSC_SST_sub$SST_45_09+
     SSC_SST_sub$SST_45_10+SSC_SST_sub$SST_45_11+SSC_SST_sub$SST_45_12)/12

SSC_SSS_sub$SSC_SSS_45AnnAvg<-
  (SSC_SSS_sub$SSS_45_01+SSC_SSS_sub$SSS_45_02+SSC_SSS_sub$SSS_45_03+
     SSC_SSS_sub$SSS_45_04+SSC_SSS_sub$SSS_45_05+SSC_SSS_sub$SSS_45_06+
     SSC_SSS_sub$SSS_45_07+SSC_SSS_sub$SSS_45_08+SSC_SSS_sub$SSS_45_09+
     SSC_SSS_sub$SSS_45_10+SSC_SSS_sub$SSS_45_11+SSC_SSS_sub$SSS_45_12)/12

### Summarize each model by MAZ, calculate the mean for each MAZ 
SST_BCCM<- BCCM_SST_sub %>% group_by(MAZ_Acrony) %>%
  summarise(across(starts_with("BCCM_SST_45AnnAvg"), mean, na.rm=TRUE)) %>%
  as.data.frame() 
SSS_BCCM<-BCCM_SSS_sub %>% group_by(MAZ_Acrony) %>%
  summarise(across(starts_with("BCCM_SSS_45AnnAvg"), mean, na.rm=TRUE ))%>%
  as.data.frame()
SSPH_BCCM<-BCCM_SSPH_sub %>% group_by(MAZ_Acrony) %>%
  summarise(across(starts_with("BCCM_SSPH_45AnnAvg"), mean,na.rm=TRUE ))%>%
  as.data.frame()

SST_NEP<-NEP_SST_sub %>% group_by(MAZ_Acrony) %>%
  summarise(across(starts_with("NEP_SST_45AnnAvg"), mean))%>%
  as.data.frame()
SSS_NEP<-NEP_SSS_sub %>% group_by(MAZ_Acrony) %>%
  summarise(across(starts_with("NEP_SSS_45AnnAvg"), mean))%>%
  as.data.frame()
SSPH_NEP<-NEP_SSPH_sub %>% group_by(MAZ_Acrony) %>%
  summarise(across(starts_with("NEP_SSPH_45AnnAvg"), mean))%>%
  as.data.frame()

SST_SSC<-SSC_SST_sub %>% group_by(MAZ_Acrony) %>%
  summarise(across(starts_with("SSC_SST_45AnnAvg"), mean,na.rm=TRUE)) %>%
  as.data.frame()
SSS_SSC<-SSC_SSS_sub %>% group_by(MAZ_Acrony) %>%
  summarise(across(starts_with("SSC_SSS_45AnnAvg"), mean,na.rm=TRUE))%>%
  as.data.frame()

#Merge spring differences back together for each variable 
#SST
SST_AnnAvg1<-merge(SST_BCCM,SST_NEP,  by="MAZ_Acrony", all=TRUE)
SST_AnnAvg<-merge(SST_AnnAvg1, SST_SSC, by="MAZ_Acrony", all=TRUE)
SST_AnnAvg= subset(SST_AnnAvg, select = -c(geometry,geometry.x, geometry.y))%>% 
  mutate(across(where(is.numeric), ~ round(., 10)))
#SSS
SSS_AnnAvg1<-merge( SSS_BCCM,SSS_NEP, by="MAZ_Acrony", all=TRUE) 
SSS_AnnAvg<-merge(SSS_AnnAvg1, SSS_SSC, by="MAZ_Acrony", all=TRUE)
SSS_AnnAvg= subset(SSS_AnnAvg, select = -c(geometry,geometry.x, geometry.y))%>% 
  mutate(across(where(is.numeric), ~ round(., 10)))  
#SSPH  
SSPH_AnnAvg<-merge(SSPH_BCCM,SSPH_NEP,  by="MAZ_Acrony", all=TRUE) 
SSPH_AnnAvg= subset(SSPH_AnnAvg, select = -c(geometry.x, geometry.y)) %>% 
  mutate(across(where(is.numeric), ~ round(., 10))) 

# This indicator will be printed in tables with the next indicator

# remove extra variables
rm(SST_AnnAvg1, SSS_AnnAvg1,
   SSS_BCCM, SSS_NEP, SSS_SSC, SST_BCCM, SST_NEP, SST_SSC, SSPH_BCCM, SSPH_NEP)

####### Historic annual average ###### 
#1) Calculate historic mean in a new column
BCCM_SST_sub$BCCM_SST_HAnnAvg <-
  (BCCM_SST_sub$SST_H_01+BCCM_SST_sub$SST_H_02+BCCM_SST_sub$SST_H_03+
     BCCM_SST_sub$SST_H_04+BCCM_SST_sub$SST_H_05+BCCM_SST_sub$SST_H_06+
     BCCM_SST_sub$SST_H_07+BCCM_SST_sub$SST_H_08+BCCM_SST_sub$SST_H_09+
     BCCM_SST_sub$SST_H_10+BCCM_SST_sub$SST_H_11+BCCM_SST_sub$SST_H_12)/12

BCCM_SSS_sub$BCCM_SSS_HAnnAvg<-
  (BCCM_SSS_sub$SSS_H_01+BCCM_SSS_sub$SSS_H_02+BCCM_SSS_sub$SSS_H_03+
     BCCM_SSS_sub$SSS_H_04+BCCM_SSS_sub$SSS_H_05+BCCM_SSS_sub$SSS_H_06+
     BCCM_SSS_sub$SSS_H_07+BCCM_SSS_sub$SSS_H_08+BCCM_SSS_sub$SSS_H_09+
     BCCM_SSS_sub$SSS_H_10+BCCM_SSS_sub$SSS_H_11+BCCM_SSS_sub$SSS_H_12)/12

BCCM_SSPH_sub$BCCM_SSPH_HAnnAvg<-
  (BCCM_SSPH_sub$SSPH_H_01+BCCM_SSPH_sub$SSPH_H_02+BCCM_SSPH_sub$SSPH_H_03+
     BCCM_SSPH_sub$SSPH_H_04+BCCM_SSPH_sub$SSPH_H_05+BCCM_SSPH_sub$SSPH_H_06+
     BCCM_SSPH_sub$SSPH_H_07+BCCM_SSPH_sub$SSPH_H_08+BCCM_SSPH_sub$SSPH_H_09+
     BCCM_SSPH_sub$SSPH_H_10+BCCM_SSPH_sub$SSPH_H_11+BCCM_SSPH_sub$SSPH_H_12)/12

NEP_SST_sub$NEP_SST_HAnnAvg <-
  (NEP_SST_sub$SST_H_01+NEP_SST_sub$SST_H_02+NEP_SST_sub$SST_H_03+
     NEP_SST_sub$SST_H_04+NEP_SST_sub$SST_H_05+NEP_SST_sub$SST_H_06+
     NEP_SST_sub$SST_H_07+NEP_SST_sub$SST_H_08+NEP_SST_sub$SST_H_09+
     NEP_SST_sub$SST_H_10+NEP_SST_sub$SST_H_11+NEP_SST_sub$SST_H_12)/12

NEP_SSS_sub$NEP_SSS_HAnnAvg<-
  (NEP_SSS_sub$SSS_H_01+NEP_SSS_sub$SSS_H_02+NEP_SSS_sub$SSS_H_03+
     NEP_SSS_sub$SSS_H_04+NEP_SSS_sub$SSS_H_05+NEP_SSS_sub$SSS_H_06+
     NEP_SSS_sub$SSS_H_07+NEP_SSS_sub$SSS_H_08+NEP_SSS_sub$SSS_H_09+
     NEP_SSS_sub$SSS_H_10+NEP_SSS_sub$SSS_H_11+NEP_SSS_sub$SSS_H_12)/12

NEP_SSPH_sub$NEP_SSPH_HAnnAvg<-
  (NEP_SSPH_sub$SSPH_H_01+NEP_SSPH_sub$SSPH_H_02+NEP_SSPH_sub$SSPH_H_03+
     NEP_SSPH_sub$SSPH_H_04+NEP_SSPH_sub$SSPH_H_05+NEP_SSPH_sub$SSPH_H_06+
     NEP_SSPH_sub$SSPH_H_07+NEP_SSPH_sub$SSPH_H_08+NEP_SSPH_sub$SSPH_H_09+
     NEP_SSPH_sub$SSPH_H_10+NEP_SSPH_sub$SSPH_H_11+NEP_SSPH_sub$SSPH_H_12)/12

SSC_SST_sub$SSC_SST_HAnnAvg <-
  (SSC_SST_sub$SST_H_01+SSC_SST_sub$SST_H_02+SSC_SST_sub$SST_H_03+
     SSC_SST_sub$SST_H_04+SSC_SST_sub$SST_H_05+SSC_SST_sub$SST_H_06+
     SSC_SST_sub$SST_H_07+SSC_SST_sub$SST_H_08+SSC_SST_sub$SST_H_09+
     SSC_SST_sub$SST_H_10+SSC_SST_sub$SST_H_11+SSC_SST_sub$SST_H_12)/12

SSC_SSS_sub$SSC_SSS_HAnnAvg<-
  (SSC_SSS_sub$SSS_H_01+SSC_SSS_sub$SSS_H_02+SSC_SSS_sub$SSS_H_03+
     SSC_SSS_sub$SSS_H_04+SSC_SSS_sub$SSS_H_05+SSC_SSS_sub$SSS_H_06+
     SSC_SSS_sub$SSS_H_07+SSC_SSS_sub$SSS_H_08+SSC_SSS_sub$SSS_H_09+
     SSC_SSS_sub$SSS_H_10+SSC_SSS_sub$SSS_H_11+SSC_SSS_sub$SSS_H_12)/12

### Summarize each model by MAZ, calculate the mean for each MAZ 
SST_BCCM<- BCCM_SST_sub %>% group_by(MAZ_Acrony) %>%
  summarise(across(starts_with("BCCM_SST_HAnnAvg"), mean, na.rm=TRUE)) %>%
  as.data.frame() 
SSS_BCCM<-BCCM_SSS_sub %>% group_by(MAZ_Acrony) %>%
  summarise(across(starts_with("BCCM_SSS_HAnnAvg"), mean, na.rm=TRUE ))%>%
  as.data.frame()
SSPH_BCCM<-BCCM_SSPH_sub %>% group_by(MAZ_Acrony) %>%
  summarise(across(starts_with("BCCM_SSPH_HAnnAvg"), mean,na.rm=TRUE ))%>%
  as.data.frame()

SST_NEP<-NEP_SST_sub %>% group_by(MAZ_Acrony) %>%
  summarise(across(starts_with("NEP_SST_HAnnAvg"), mean))%>%
  as.data.frame()
SSS_NEP<-NEP_SSS_sub %>% group_by(MAZ_Acrony) %>%
  summarise(across(starts_with("NEP_SSS_HAnnAvg"), mean))%>%
  as.data.frame()
SSPH_NEP<-NEP_SSPH_sub %>% group_by(MAZ_Acrony) %>%
  summarise(across(starts_with("NEP_SSPH_HAnnAvg"), mean))%>%
  as.data.frame()

SST_SSC<-SSC_SST_sub %>% group_by(MAZ_Acrony) %>%
  summarise(across(starts_with("SSC_SST_HAnnAvg"), mean,na.rm=TRUE )) %>%
  as.data.frame()
SSS_SSC<-SSC_SSS_sub %>% group_by(MAZ_Acrony) %>%
  summarise(across(starts_with("SSC_SSS_HAnnAvg"), mean,na.rm=TRUE ))%>%
  as.data.frame()

#Merge spring differences back together for each variable 
#SST
SST_AnnAvg1<-merge(SST_AnnAvg, SST_BCCM,  by="MAZ_Acrony", all=TRUE)
SST_AnnAvg2<-merge(SST_AnnAvg1,SST_NEP,  by="MAZ_Acrony", all=TRUE)
SST_AnnAvg3<-merge(SST_AnnAvg2,SST_SSC,  by="MAZ_Acrony", all=TRUE)
SST_AnnAvg= subset(SST_AnnAvg3, select = -c(geometry,geometry.x, geometry.y))%>% 
  mutate(across(where(is.numeric), ~ round(., 10)))  
rm(SST_AnnAvg1, SST_AnnAvg2, SST_AnnAvg3)

#SSS
SSS_AnnAvg1<-merge(SSS_AnnAvg, SSS_BCCM,  by="MAZ_Acrony", all=TRUE)
SSS_AnnAvg2<-merge(SSS_AnnAvg1,SSS_NEP,  by="MAZ_Acrony", all=TRUE)
SSS_AnnAvg3<-merge(SSS_AnnAvg2,SSS_SSC,  by="MAZ_Acrony", all=TRUE)
SSS_AnnAvg= subset(SSS_AnnAvg3, select = -c(geometry,geometry.x, geometry.y))%>% 
  mutate(across(where(is.numeric), ~ round(., 10)))  
rm(SSS_AnnAvg1, SSS_AnnAvg2, SSS_AnnAvg3)
#SSPH  
SSPH_AnnAvg1<-merge(SSPH_AnnAvg, SSPH_BCCM,  by="MAZ_Acrony", all=TRUE)
SSPH_AnnAvg2<-merge(SSPH_AnnAvg1,SSPH_NEP,  by="MAZ_Acrony", all=TRUE)
SSPH_AnnAvg= subset(SSPH_AnnAvg2, select = -c(geometry.x, geometry.y))%>% 
  mutate(across(where(is.numeric), ~ round(., 10)))  
rm(SSPH_AnnAvg1, SSPH_AnnAvg2)

# remove extra variables
rm(SSS_BCCM, SSS_NEP, SSS_SSC, SST_BCCM, SST_NEP, SST_SSC, SSPH_BCCM, SSPH_NEP)

######### Z score of Projected value #######
# formula for z score = (X - popmean)/popsd
# For this X is the Annual average SS_ for each MAZ as calculated in the last indicator
# Population mean and sd are the mean and SD of the whole model 

#1) calculate models means and SDs across all model points 
## Historical 
BCCM_SST_H_mean <- mean(BCCM_SST_sub$BCCM_SST_HAnnAvg, na.rm=TRUE)
BCCM_SSS_H_mean <- mean(BCCM_SSS_sub$BCCM_SSS_HAnnAvg, na.rm=TRUE)
BCCM_SSPH_H_mean<- mean(BCCM_SSPH_sub$BCCM_SSPH_HAnnAvg, na.rm=TRUE)
BCCM_SST_H_sd   <- sd(BCCM_SST_sub$BCCM_SST_HAnnAvg, na.rm=TRUE)
BCCM_SSS_H_sd   <- sd(BCCM_SSS_sub$BCCM_SSS_HAnnAvg, na.rm=TRUE)
BCCM_SSPH_H_sd  <- sd(BCCM_SSPH_sub$BCCM_SSPH_HAnnAvg, na.rm=TRUE)         

NEP_SST_H_mean <- mean(NEP_SST_sub$NEP_SST_HAnnAvg, na.rm=TRUE)
NEP_SSS_H_mean <- mean(NEP_SSS_sub$NEP_SSS_HAnnAvg, na.rm=TRUE)
NEP_SSPH_H_mean<- mean(NEP_SSPH_sub$NEP_SSPH_HAnnAvg, na.rm=TRUE)
NEP_SST_H_sd   <- sd(NEP_SST_sub$NEP_SST_HAnnAvg, na.rm=TRUE)
NEP_SSS_H_sd   <- sd(NEP_SSS_sub$NEP_SSS_HAnnAvg, na.rm=TRUE)
NEP_SSPH_H_sd  <- sd(NEP_SSPH_sub$NEP_SSPH_HAnnAvg, na.rm=TRUE)  

SSC_SST_H_mean <- mean(SSC_SST_sub$SSC_SST_HAnnAvg, na.rm=TRUE)
SSC_SSS_H_mean <- mean(SSC_SSS_sub$SSC_SSS_HAnnAvg, na.rm=TRUE)
SSC_SST_H_sd   <- sd(SSC_SST_sub$SSC_SST_HAnnAvg, na.rm=TRUE)
SSC_SSS_H_sd   <- sd(SSC_SSS_sub$SSC_SSS_HAnnAvg, na.rm=TRUE)

## Future 
BCCM_SST_45_mean <- mean(BCCM_SST_sub$BCCM_SST_45AnnAvg, na.rm=TRUE)
BCCM_SSS_45_mean <- mean(BCCM_SSS_sub$BCCM_SSS_45AnnAvg, na.rm=TRUE)
BCCM_SSPH_45_mean<- mean(BCCM_SSPH_sub$BCCM_SSPH_45AnnAvg, na.rm=TRUE)
BCCM_SST_45_sd   <- sd(BCCM_SST_sub$BCCM_SST_45AnnAvg, na.rm=TRUE)
BCCM_SSS_45_sd   <- sd(BCCM_SSS_sub$BCCM_SSS_45AnnAvg, na.rm=TRUE)
BCCM_SSPH_45_sd  <- sd(BCCM_SSPH_sub$BCCM_SSPH_45AnnAvg, na.rm=TRUE)         

NEP_SST_45_mean <- mean(NEP_SST_sub$NEP_SST_45AnnAvg, na.rm=TRUE)
NEP_SSS_45_mean <- mean(NEP_SSS_sub$NEP_SSS_45AnnAvg, na.rm=TRUE)
NEP_SSPH_45_mean<- mean(NEP_SSPH_sub$NEP_SSPH_45AnnAvg, na.rm=TRUE)
NEP_SST_45_sd   <- sd(NEP_SST_sub$NEP_SST_45AnnAvg, na.rm=TRUE)
NEP_SSS_45_sd   <- sd(NEP_SSS_sub$NEP_SSS_45AnnAvg, na.rm=TRUE)
NEP_SSPH_45_sd  <- sd(NEP_SSPH_sub$NEP_SSPH_45AnnAvg, na.rm=TRUE)  

SSC_SST_45_mean <- mean(SSC_SST_sub$SSC_SST_45AnnAvg, na.rm=TRUE)
SSC_SSS_45_mean <- mean(SSC_SSS_sub$SSC_SSS_45AnnAvg, na.rm=TRUE)
SSC_SST_45_sd   <- sd(SSC_SST_sub$SSC_SST_45AnnAvg, na.rm=TRUE)
SSC_SSS_45_sd   <- sd(SSC_SSS_sub$SSC_SSS_45AnnAvg, na.rm=TRUE)

# Calculate z score difference for each MAZ
SST_AnnAvg$BCCM_zscorediff<-((SST_AnnAvg$BCCM_SST_45AnnAvg- BCCM_SST_45_mean)/BCCM_SST_45_sd)-
  ((SST_AnnAvg$BCCM_SST_HAnnAvg- BCCM_SST_H_mean)/BCCM_SST_H_sd)

SST_AnnAvg$NEP_zscorediff<-((SST_AnnAvg$NEP_SST_45AnnAvg- NEP_SST_45_mean)/NEP_SST_45_sd)-
  ((SST_AnnAvg$NEP_SST_HAnnAvg- NEP_SST_H_mean)/NEP_SST_H_sd)

SST_AnnAvg$SSC_zscorediff<-((SST_AnnAvg$SSC_SST_45AnnAvg- SSC_SST_45_mean)/SSC_SST_45_sd)-
  ((SST_AnnAvg$SSC_SST_HAnnAvg- SSC_SST_H_mean)/SSC_SST_H_sd)


SSS_AnnAvg$BCCM_zscorediff<-((SSS_AnnAvg$BCCM_SSS_45AnnAvg- BCCM_SSS_45_mean)/BCCM_SSS_45_sd)-
  ((SSS_AnnAvg$BCCM_SSS_HAnnAvg- BCCM_SSS_H_mean)/BCCM_SSS_H_sd)

SSS_AnnAvg$NEP_zscorediff<-((SSS_AnnAvg$NEP_SSS_45AnnAvg- NEP_SSS_45_mean)/NEP_SSS_45_sd)-
  ((SSS_AnnAvg$NEP_SSS_HAnnAvg - NEP_SSS_H_mean) /NEP_SSS_H_sd)

SSS_AnnAvg$SSC_zscorediff<-((SSS_AnnAvg$SSC_SSS_45AnnAvg- SSC_SSS_45_mean)/SSC_SSS_45_sd)-
  ((SSS_AnnAvg$SSC_SSS_HAnnAvg- SSC_SSS_H_mean)/SSC_SSS_H_sd)


SSPH_AnnAvg$BCCM_zscorediff<-((SSPH_AnnAvg$BCCM_SSPH_45AnnAvg- BCCM_SSPH_45_mean)/BCCM_SSPH_45_sd)-
  ((SSPH_AnnAvg$BCCM_SSPH_HAnnAvg- BCCM_SSPH_H_mean)/BCCM_SSPH_H_sd)

SSPH_AnnAvg$NEP_zscorediff<-((SSPH_AnnAvg$NEP_SSPH_45AnnAvg- NEP_SSPH_45_mean)/NEP_SSPH_45_sd)-
  ((SSPH_AnnAvg$NEP_SSPH_HAnnAvg- NEP_SSPH_H_mean)/NEP_SSPH_H_sd)

# set precision 
SST_AnnAvg<-SST_AnnAvg %>% mutate(across(where(is.numeric), ~ round(., 2))) 
SSS_AnnAvg<-SSS_AnnAvg%>% mutate(across(where(is.numeric), ~ round(., 2)))
SSPH_AnnAvg<-SSPH_AnnAvg%>% mutate(across(where(is.numeric), ~ round(., 2)))

# Rename table columns
colAnAvgZsc_all<-c('MAZ', 'BCCM 4.5','NEP 4.5','SSC 4.5','BCCM H','NEP H','SSC H','BCCM Z','NEP Z', 'SSC Z')
colAnAvgZsc_2<- c('MAZ', 'BCCM 4.5','NEP 4.5','BCCM H','NEP H','BCCM Z','NEP Z')
colnames(SST_AnnAvg)<- colAnAvgZsc_all
colnames(SSS_AnnAvg)<- colAnAvgZsc_all
colnames(SSPH_AnnAvg)<- colAnAvgZsc_2

# Create table ## Not sure why there are a bunch of extra columns 
grid.newpage()
grid.table(SST_AnnAvg)
grid.newpage()
grid.table(SSS_AnnAvg)
grid.newpage()
grid.table(SSPH_AnnAvg)

#remove variable no longer needed 
rm(BCCM_SST_45_mean,BCCM_45_SSS_mean,BCCM_SSPH_45_mean,BCCM_45_SST_sd,BCCM_SSS_45_sd,
   BCCM_SSPH_45_sd,NEP_SST_45_mean,NEP_SSS_45_mean ,NEP_SSPH_45_mean,NEP_SST_45_sd,
   NEP_SSS_45_sd,NEP_SSPH_45_sd,SSC_SST_45_mean ,SSC_SSS_45_mean,SSC_SST_45_sd,SSC_SSS_45_sd,
   
   BCCM_SST_H_mean,BCCM_SSS_H_mean,BCCM_SSPH_H_mean,BCCM_SST_H_sd,BCCM_SSS_H_sd,
   BCCM_SSPH_H_sd,NEP_SST_H_mean,NEP_SSS_H_mean ,NEP_SSPH_H_mean,NEP_SST_H_sd,
   NEP_SSS_H_sd,NEP_SSPH_H_sd,SSC_SST_H_mean ,SSC_SSS_H_mean,SSC_SST_H_sd,SSC_SSS_H_sd,
   
   colAnAvgZsc_all, colAnAvgZsc_2)

