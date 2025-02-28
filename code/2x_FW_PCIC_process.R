## 2x_FW_PCIC_process.R
# script to summarize daily model outputs from PCIC gridded hydrologic model
#####################################################################

#function for aggregating by month of year - see https://github.com/r-spatial/stars/issues/134
by_month <- function(x) {
  as.factor(month(x))
}
by_day <- function(x) {
  as.factor(format(x, "%m-%d"))
}

PCIC_files <- list.files(file.path(climate_dat, "PCIC"))

for(f in 1:length(int_starts)) {
  
  s_date <- date(paste0(int_starts[f],"-01-01")) - date("1945-01-01") + 1
  int_date <- date(paste0(int_starts[f]+20,"-01-01")) - date(paste0(int_starts[f],"-01-01")) + 1
  
  for(i in 1:length(PCIC_files)) {
    PCIC <- read_ncdf(file.path(climate_dat, "PCIC", PCIC_files[i]),
                      ncsub = cbind(start = c(1, 1, s_date), count = c(176, 128, int_date)), proxy = FALSE)
    st_crs(PCIC) <- 4269
    
    if(grepl(names(PCIC), "waterTemperature")) PCIC$waterTemperature <- set_units(PCIC$waterTemperature, "Celsius")
    
    tm <- PCIC %>%
      aggregate(by = by_month, FUN = mean, na.rm =T) %>%
      st_set_dimensions(which = "geometry", names ="time")
    
    mdate <- as.Date(paste0(int_starts[f], "-", st_get_dimension_values(tm, "time"), "-15"), format = "%Y-%m-%d")
    
    tm <- tm %>% 
      st_set_dimensions(which = "time", values = mdate) %>%
      aperm(c(2, 3, 1)) #reorder dimensions
    
    td <- PCIC %>%   
      aggregate(by = by_day, FUN = mean, na.rm =T) %>%
      st_set_dimensions(which = "geometry", names ="time") %>%
      filter(time != "02-29") #remove leap days
    
    ddate <- as.Date(paste0(int_starts[f], "-", st_get_dimension_values(td, "time")), format = "%Y-%m-%d")
    
    td <- td %>%
      st_set_dimensions(which = "time", values = ddate) %>%
      aperm(c(2, 3, 1))
    
    if(i ==1) {
      PCIC_m <- tm
      PCIC_d <- td
    } else {
      PCIC_m <- c(PCIC_m, tm)
      PCIC_d   <- c(PCIC_d, td)
    }
  }
  
  if(f == 1) {
    PCIC_month <- PCIC_m
    PCIC_day <- PCIC_d
  } else {
    PCIC_month <- c(PCIC_month, PCIC_m, along = 3)
    PCIC_day <- c(PCIC_day, PCIC_d, along = 3)
  }
}

rm(PCIC, PCIC_m, PCIC_d)

write_mdim(PCIC_month, file.path(climate_dat, "PCIC_processed", "PCIC_monthly.nc"))
write_mdim(PCIC_day, file.path(climate_dat, "PCIC_processed", "PCIC_daily.nc"))




