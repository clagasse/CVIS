## 2x_FW_PCIC_period_average.R
# script to import full daily time series of PCIC VIC-GL model
# and create daily and monthly averages across 20-30 year time periods

# outputs are saved into the PCIC_processed folder for further analysis
#####################################################################

library(here)
setwd(here())
source(file.path(here(), "code", "0_setup.R"))

PCIC_file_loc <- file.path("D:", "PCIC", "fraser")
x_dim_max <- 176 #number of x grid cells  used when importing ncdf file
y_dim_max <- 128 #number of y grid cells 

#time ranges used for taking averages
time_periods <- tribble(
  ~start, ~end,
  1981, 2010,
  2041, 2060,
  2061, 2080,
  2081, 2099
)

#function for aggregating by month of year - see https://github.com/r-spatial/stars/issues/134
by_month <- function(x) {
  as.factor(month(x))
}
by_day <- function(x) {
  as.factor(format(x, "%m-%d"))
}

PCIC_files <- list.files(PCIC_file_loc)

PCIC_ESMs <- c("MPI-ESM-LR", 
                 "ACCESS1-0",
                 "CanESM2", 
                 "CCSM4", 
                 "HadGEM2-ES",
                 "CNRM-CM5")

#get unique model run names, trimming discharge and waterTemp from start of file name
PCIC_models <- PCIC_files[grepl("discharge_", PCIC_files)] 
PCIC_models <- gsub("^(discharge)_", "", PCIC_models)

#get files corresponding to each model
PCIC_file_models <- lapply(PCIC_models, function(m) {
  grep(m, PCIC_files, value = TRUE)
})



#length(PCIC_file_models)
#iterate over each group of model runs, creating output combining discharge and waterTemp
for(i in 1:1) {
  
  models_pick <- PCIC_file_models[[i]]
  
  output_name <- gsub("^(discharge|waterTemp)_day_dynWat-VICGL_", "", models_pick)[1]
  
  #load the output for each variable (discharge or waterTemp) within each model run
  for(j in 1:length(models_pick)) {
    
    file_pick <- models_pick[j]
    
    #subset and summarize monthly and daily values over each time period
    for(f in 1:nrow(time_periods)) {
      
      s_date <- date(paste0(time_periods$start[f],"-01-01")) - date("1945-01-01") + 1
      int_date <- date(paste0(time_periods$end[f],"-12-30")) - date(paste0(time_periods$start[f],"-01-01"))

      PCIC <- read_ncdf(file.path(PCIC_file_loc, file_pick),
                        ncsub = cbind(start = c(1, 1, s_date), count = c(x_dim_max, y_dim_max, int_date)), proxy = FALSE)
      st_crs(PCIC) <- 4269
      if(attributes(PCIC)$names %in% "waterTemperature") PCIC$waterTemperature <- set_units(PCIC$waterTemperature, "Celsius")
      
      ## calculate average monthly Q/T across time period
      tm <- PCIC %>%
        aggregate(by = by_month, FUN = mean, na.rm =T) %>%
        st_set_dimensions(which = "geometry", names ="time")
      
      mdate <- as.Date(paste0(time_periods$start[f], "-", st_get_dimension_values(tm, "time"), "-15"), format = "%Y-%m-%d")
      
      tm <- tm %>% 
        st_set_dimensions(which = "time", values = mdate) %>%
        aperm(c(2, 3, 1)) #reorder dimensions
      
      ## calculate average daily Q/T across time period
      td <- PCIC %>%   
        aggregate(by = by_day, FUN = mean, na.rm =T) %>%
        st_set_dimensions(which = "geometry", names ="time") %>%
        filter(time != "02-29") #remove leap days
      
      ddate <- as.Date(paste0(time_periods$start[f], "-", st_get_dimension_values(td, "time")), format = "%Y-%m-%d")
      
      td <- td %>%
        st_set_dimensions(which = "time", values = ddate) %>%
        aperm(c(2, 3, 1))
      
      time_d <- st_get_dimension_values(td, "time")
      
      #combine monthly and daily averaged values across all time periods
      if(f ==1) {
        PCIC_m <- tm
        PCIC_d <- td
        day_time <- time_d
      } 
      if(f > 1) {
        PCIC_m <- c(PCIC_m, tm, along = 3)
        PCIC_d   <- c(PCIC_d, td, along = 3)
        day_time <- c(day_time, time_d)
      }
      
      gc()
      
    }
    
    #combine discharge and waterTemp variables into a single stars object
    if(j == 1) {
      PCIC_month <- PCIC_m
      PCIC_day <- PCIC_d
      
      PCIC_day_Q <- PCIC_d
    } 
    if(j > 1) {
      PCIC_month <- c(PCIC_month, PCIC_m)
      PCIC_day <- c(PCIC_day, PCIC_d)
      
      PCIC_day_T <- PCIC_d
    }
  }
 
  #write averaged monthly and daily outputs for each model run to a file
  m_out <- paste0("monthly_", output_name)
  d_out <- paste0("daily_", output_name)
  
  print(paste0("Writing output to ", m_out))
  write_mdim(PCIC_month, file.path(paths$climate, "PCIC_processed", m_out))
  write_mdim(PCIC_day, file.path(paths$climate, "PCIC_processed", d_out))
  
}

#rm(PCIC, PCIC_m, PCIC_d)

temp_d <- read_mdim(file.path(paths$climate, "PCIC_processed", "daily_ACCESS1-0_rcp45_r1i1p1_19450101-20991231_fraser.nc"))
temp_m <- read_mdim(file.path(paths$climate, "PCIC_processed", "monthly_ACCESS1-0_rcp45_r1i1p1_19450101-20991231_fraser.nc"))


time_d <- st_get_dimension_values(temp_d, "time")
time_m <- st_get_dimension_values(temp_m, "time")
