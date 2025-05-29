

library(here)
setwd(here())
source(file.path(here(), "code", "0_setup.R"))

library(pacea)

#months to use for summarizing ocean entry SSTs
months_choose <- c(4, 5, 6, 7)

#read marine adaptive zones
MAZ     <- st_read(file.path(paths$spatial, "MAZ", "MAZ_Final.shp"))
#read OISST data from pacea
oisst_month <- st_transform(oisst_month,crs = "EPSG:3005")

#download hotssea data  - use hotssea_all_variables()
#after downloading get hotssea SST
hotssea_SST <- hotssea_surface_temperature_mean() %>%
  pivot_longer(cols = c(`1980_1`:`2018_12`),
               names_to = c("year", "month"),
               names_sep = "_",
               values_to = "sst")

#subset to GStr MAZ (exclude Juan de Fuca, Johnstone Strait)
hotssea_SST_GStr <- hotssea_SST[MAZ[MAZ$MAZ_Acrony == "GStr",], ]

#import SST productivity model coefs
SST_coefs <- read_csv(file.path(paths$salmon, "SST_productivity_model", "ERA_coefs_May152025.csv"))

model_SSTs_BY <- read_csv(file.path(paths$salmon, "SST_productivity_model", "ERA_SSTvalues_May152025.csv"))
model_SSTs_CY <- read_csv(file.path(paths$salmon, "SST_productivity_model", "sst_yr_1_stock_anomalies_May282025.csv")) %>%
  left_join(select(model_SSTs_BY, Stock, Stock.ID, Species, Ocean.Region2),
            join_by(Stock.ID, Species), multiple = "first") %>%
  filter(!is.na(Stock)) %>%
  rename(year = Year)


model_SSTs_WC <- filter(model_SSTs_CY, Ocean.Region2 == "WC") #%>%
  # mutate(year = if_else(Species == "Pink", BY + 1,
  #                       if_else(Species == "Chum", BY + 1,
  #                               if_else(Species == "Sockeye", BY + 2, NA_real_))))

SST_coefs_WC <- filter(SST_coefs, region == "WC")


#filter out all stocks except Sproat Lake (for WVI) and Georgia Strait Stocks
model_SSTs_GStr <- filter(model_SSTs_WC, Stock %notin% c("Great Central Lake","Nisqually-Odd",
                                                "S South Misc.-Odd",          "Puyallup-Odd" ,             
                                                     "Hood Canal-Odd" , "Snohomish-Odd" ,"Dungeness-Odd"   ,  "Stillaguamish-Odd",         
                                                    "Skagit-Odd", "Nooksack-Odd",           
                                                "Southern Fjords-Even",       "Hecate Lowlands-Even",       "Hecate Strait-Fjords-Even", 
                                                "Hecate Strait-Lowlands-Odd", "Hecate Strait-Fjords-Odd",   "Willapa Bay",                "Grays Harbour",             
                                                "S Sound Summer",             "S Sound Fall" ,              "S Sound Winter",             "Hood Canal", 
                                                "Strait of Juan de Fuca",     "Port Susan" ,                "Skagit" ,                    "Bellingham",
                                                "BC South (no Fraser)",       "Mussel-Kynoch",              "Hecate Lowlands",
                                                "Douglas-Gardner",            "Lower Skeena",               "Middle Skeena")) %>%
  mutate(MAZ_Acrony = "GStr_ERSST")

model_SSTs_WVI <- filter(model_SSTs_WC, Stock %in% c("Sproat Lake")) %>%
  mutate(MAZ_Acrony = "WVI_ERSST")

#----------------- Spatial Processing-------------------------

#match oisst points to MAZ polygons
oisst_month$MAZ_Acrony <- NA
container <- st_contains(MAZ, oisst_month)
for(i in 1:length(container)) {
  if(length(container[[i]]) > 0) {
    oisst_month$MAZ_Acrony[container[[i]]] <- MAZ$MAZ_Acrony[i]
  }
}

oisst_spring <- oisst_month %>%
  filter(month %in% months_choose, year < 2025 ) %>%
  group_by(MAZ_Acrony, year) %>%
  summarise(mean_sst = mean(sst, na.rm = TRUE),
            n = n()) %>%
  ungroup()  %>%
  mutate(MAZ_Acrony = str_c(MAZ_Acrony, "_OISST"))

oisst_spring_dt <- data.table(oisst_spring) %>%
  filter(!is.na(MAZ_Acrony)) %>%
  select(-geometry)

hotssea_SST_spring <- hotssea_SST_GStr %>%
  filter(month %in% months_choose) %>%
  group_by(year) %>%
  summarise(mean_sst = mean(sst, na.rm = TRUE),
            n = n()) %>%
  ungroup()

hotssea_SST_spring_dt <- data.table(hotssea_SST_spring) %>%
  mutate(MAZ_Acrony = "GStr_HOTSSEA") %>%
  relocate(MAZ_Acrony) %>%
  select(-geometry)

#summarize model SSTs
model_SSTs_summary <- rbind(model_SSTs_GStr, model_SSTs_WVI) %>%
  group_by(year, MAZ_Acrony) %>%
  summarise(mean_sst = mean(sst_raw, na.rm = TRUE),
            n = n()) %>%
  ungroup() %>%
  relocate(MAZ_Acrony)


#combine SST values
combined_spring <-  rbind(oisst_spring_dt, hotssea_SST_spring_dt, model_SSTs_summary, ignore.attr = TRUE) %>%
  mutate(year = as.numeric(year)) %>%
  filter(year > 1980)


###----- ROM historical summaries and projections

#Salish Sea Cast
SSC_SST_sub<- read_sf( file.path(paths$climate, "Standardized_Marine_data/SSC_SST_sub.shp"))
#BCCM 
BCCM_SST_sub<-read_sf(file.path(paths$climate, "Standardized_Marine_data/BCCM_SST_sub.shp"))

SSC_SST_long <- SSC_SST_sub %>%
  pivot_longer(
    cols = c(contains("H"), contains("45"),contains("85")),
    names_to = c("scenario", "month"),
    names_pattern = 'SST_([A-Za-z0-9]+)_([0-9]+)',
    values_to = "value"
  ) %>%
  mutate(model = "SSC",
         month = as.numeric(month))

BCCM_SST_long <- BCCM_SST_sub %>%
  pivot_longer(
    cols = c(contains("H"), contains("45"),contains("85")),
    names_to = c("scenario", "month"),
    names_pattern = 'SST_([A-Za-z0-9]+)_([0-9]+)',
    values_to = "value"
  ) %>%
  mutate(model = "BCCM",
         month = as.numeric(month))

ROM_SST <- bind_rows(BCCM_SST_long, SSC_SST_long) %>%
  filter(MAZ_Acrony != "SFj")

ROM_SST_spring_summary <- data.table(ROM_SST) %>%
  filter(month %in% months_choose) %>%
  select(-c(geometry, BCCM_mask)) %>%
  group_by(scenario, MAZ_Acrony) %>%
  summarize(mean_sst = mean(value, na.rm = T),
            SST_05 = quantile(value, 0.05, na.rm = T),
            SST_95 = quantile(value, 0.95, na.rm = T),
            n = n()) %>%
    ungroup()

ROM_SST_compare <- ROM_SST_spring_summary %>%
  mutate(year = if_else(scenario == "H", 1995,
                        if_else(scenario == "45", 2050,
                                if_else(scenario == "85", 2050, NA_real_)))) %>%
  mutate(MAZ_Acrony = if_else(scenario == "H", str_c(MAZ_Acrony, "_ROM_H"),
                              if_else(scenario == "45", str_c(MAZ_Acrony, "_ROM_45"),
                                      if_else(scenario == "85", str_c(MAZ_Acrony, "_ROM_85"), NA_character_)))) %>%
  select(-scenario, -SST_05, -SST_95) %>%
  relocate(year, .after = MAZ_Acrony)
  

combined_spring_ROMs <- bind_rows(combined_spring, ROM_SST_compare) %>%
  filter(year <= 2010 | year == 2050) %>%
  mutate(Region = str_extract(MAZ_Acrony, "[^_]+"),
         Model = str_extract(MAZ_Acrony, "_[^*]+")) %>%
  mutate(Model = substring(Model, 2))


spring_means <- combined_spring %>%
  group_by(MAZ_Acrony) %>%
  summarize(avg_sst = mean(mean_sst, na.rm = TRUE)) 

spring_anoms <- combined_spring %>% 
  left_join(spring_means, by = "MAZ_Acrony") %>%
  mutate(sst_anomaly = mean_sst - avg_sst)



#----------------- Make report------------------------

rmarkdown::render(
  file.path(here("reports","compare_model_SSTs.Rmd")),
  output_file = paste(today, "SST_comparisons.html", sep = "_"),
  output_dir = here("output"),
  output_format = "html_document")

#----------------- Plots -------------------------

ggplot(oisst_spring_dt) +
  geom_line(aes(x = year, y = mean_sst, color = MAZ_Acrony)) +
  geom_point(aes(x = year, y = mean_sst, color = MAZ_Acrony)) +
  labs(title = "April-June SST in GStr and NStr",
       x = "Year",
       y = "Mean SST") +
  theme_minimal() +
  theme(legend.position = "bottom")

p1 <- ggplot() +
  geom_sf(data = filter(oisst_month, year == c(2012)), aes(colour = MAZ_Acrony)) +
  geom_sf(data = MAZ, aes(colour = MAZ_Acrony), alpha = 0.5) +
  scale_fill_viridis_c() +
  labs(color = "MAZ")

p2 <- ggplot(data = filter(combined_spring, MAZ_Acrony %in% 
                             c("GStr_OISST", "GStr_HOTSSEA", "GStr_ERSST", "WVI_OISST", "WVI_ERSST", "Offshore_OISST"))) +
  geom_line(aes(x = year, y = mean_sst, group = MAZ_Acrony)) +
  geom_point(aes(x = year, y = mean_sst, color = MAZ_Acrony)) +
  labs(title = "April-July mean annual SST",
       x = "Year",
       y = "Mean SST",
       color = "MAZ_Model") 

p3 <- ggplot(data = filter(combined_spring_ROMs, 
                     MAZ_Acrony %in% c("GStr_OISST", "GStr_HOTSSEA", "GStr_ERSST", "GStr_ROM_H", "GStr_ROM_45", "GStr_ROM_85",
                                       "WVI_ERSST", "WVI_OISST", "WVI_ROM_H", "WVI_ROM_45", "WVI_ROM_85", 
                                       "Offshore_OISST", "Offshore_ROM_H", "Offshore_ROM_45"))) +
  geom_boxplot(aes(x = Model, y = mean_sst, fill = Model)) +
  labs(title = "April-July mean annual SST",
       x = "Model/scenario by marine adaptive zone",
       y = "Annual SST",
       fill = "Model") +
  facet_wrap(~Region, ncol = 1)


p4 <- ggplot(data = filter(spring_anoms, MAZ_Acrony %in% c("GStr_OISST", "GStr_HOTSSEA", "GStr_ERSST", "WVI_OISST", "WVI_ERSST", "Offshore_OISST"))) +
  geom_line(aes(x = year, y = sst_anomaly, group = MAZ_Acrony, color = MAZ_Acrony)) +
  geom_point(aes(x = year, y = sst_anomaly, color = MAZ_Acrony)) +
  labs(title = "April-July annual SST anomaly",
       x = "Year",
       y = "SST anomaly",
       color = "MAZ_Model") 

ggsave(filename = file.path(paths$figures, "SST_comparisons_boxplot.png"),
       plot = p3,
       width = 6,
       height =8,
       dpi = 300)

ggsave(filename = file.path(paths$figures, "SST_comparisons_timeseries.png"),
       plot = p2,
       width = 10,
       height = 6,
       dpi = 300)

ggsave(filename = file.path(paths$figures, "MAZ_OISST_strata.png"),
       plot = p1,
       width = 10,
       height = 6,
       dpi = 300) 

ggsave(filename = file.path(paths$figures, "SST_anomaly_timeseries.png"),
       plot = p4,
       width = 10,
       height = 6,
       dpi = 300)

