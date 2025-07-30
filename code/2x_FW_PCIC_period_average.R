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
  ~start, ~end, ~abbrev,
  1981, 2010, 0,
  2011, 2020, 1,
  2021, 2040, 2,
  2041, 2060, 3,
  2061, 2080, 4,
  2081, 2099, 5
)

#function for aggregating by month of year - see https://github.com/r-spatial/stars/issues/134
by_month <- function(x) {
  as.factor(month(x))
}
by_day <- function(x) {
  as.factor(format(x, "%m-%d"))
}

PCIC_files <- list.files(PCIC_file_loc)

#get unique model run names, trimming discharge and waterTemp from start of file name
PCIC_models <- PCIC_files[grepl("discharge_", PCIC_files)] 
PCIC_models <- gsub("^(discharge)_", "", PCIC_models)

#get files corresponding to each model
PCIC_file_models <- lapply(PCIC_models, function(m) {
  grep(m, PCIC_files, value = TRUE)
})



#length(PCIC_file_models)
#iterate over each group of model runs, creating output combining discharge and waterTemp
for(i in 1:length(PCIC_file_models)) {
  
  models_pick <- PCIC_file_models[[i]]
  
  output_name <- gsub("^(discharge|waterTemp)_day_dynWat-VICGL_", "", models_pick)[1]
  
  #load the output for each variable (discharge or waterTemp) within each model run
  for(j in 1:length(models_pick)) {
    
    file_pick <- models_pick[j]
    
    #subset and summarize monthly and daily values over each time period
    s_date <- date("1981-01-01") - date("1945-01-01") + 1
    #int_date <- date(paste0(time_periods$end[f],"-12-30")) - date(paste0(time_periods$start[f],"-01-01"))
    e_date <- date("2099-12-31") - date("1981-01-01")
    
    PCIC <- read_ncdf(file.path(PCIC_file_loc, file_pick),
                      ncsub = cbind(start = c(1, 1, s_date), count = c(x_dim_max, y_dim_max, e_date)), proxy = FALSE)
    st_crs(PCIC) <- 4269
    #if(attributes(PCIC)$names %in% "waterTemperature") PCIC$waterTemperature <- set_units(PCIC$waterTemperature, "Celsius")
    
    time_vals <- as.Date(st_get_dimension_values(PCIC, "time"))
    
    # Define time periods
    periods <- cut(
      time_vals,
      breaks = as.Date(c(paste0(time_periods$start, "-01-01"), "2099-12-31"))-1,
      #c("1980-12-31", "2010-12-31", "2040-12-31", "2060-12-31", "2080-12-31", "2099-12-31")),
      labels = paste0(time_periods$start, "-", time_periods$end),
      right = TRUE
    )
    
    time_info <- data.frame(
      time = time_vals,
      period = periods,
      month = month(time_vals),
      day_of_year = yday(time_vals)
    )
    
    # Compute daily averages by period
    daily_avg_by_period <- lapply(levels(periods), function(p) {
      # Get time indices for this period
      times_in_period <- time_info %>% filter(period == p)
      
      # Subset stars object
      stars_sub <- PCIC[,,,which(time_vals %in% times_in_period$time), drop = FALSE]
      
      stars_sub <- stars_sub %>%
        aggregate(by = by_day, FUN = mean, na.rm =T) %>%
        st_set_dimensions(which = "geometry", names ="time") %>%
        filter(time != "02-29") %>%  
        aperm(c(2, 3, 1))
      
    })

    PCIC_day_j <- do.call(c, c(daily_avg_by_period, along = "period"))
    PCIC_day_j<- st_set_dimensions(PCIC_day_j, "period", values = levels(periods))
    
    monthly_avg_by_period <- lapply(levels(periods), function(p) {
      times_in_period <- time_info %>% filter(period == p)
      
      stars_sub <- PCIC[,,,which(time_vals %in% times_in_period$time), drop = FALSE]
      
      stars_sub <- stars_sub %>%
        aggregate(by = by_month, FUN = mean, na.rm =T) %>%
        st_set_dimensions(which = "geometry", names ="time") %>%
        aperm(c(2, 3, 1))
    })

    PCIC_month_j <- do.call(c, c(monthly_avg_by_period, along = "period"))
    PCIC_month_j<- st_set_dimensions(PCIC_month_j, "period", values = levels(periods))
    
    #combine discharge and waterTemp variables into a single stars object
    if(j == 1) {
      PCIC_month <- PCIC_month_j
      PCIC_day <- PCIC_day_j
    } 
    if(j > 1) {
      PCIC_month <- c(PCIC_month, PCIC_month_j)
      PCIC_day <- c(PCIC_day, PCIC_day_j)
    }
    
    gc()
  }
  
  #write averaged monthly and daily outputs for each model run to a file
  m_out <- paste0("monthly_", output_name)
  d_out <- paste0("daily_", output_name)
  
  d_out <- "test.nc"
  
  print(paste0("Writing output to ", m_out))
  write_mdim(PCIC_month, file.path(paths$climate, "PCIC_processed", m_out))
  write_mdim(PCIC_day, file.path(paths$climate, "PCIC_processed", d_out))

}

#rm(PCIC, PCIC_m, PCIC_d)

# temp_d <- read_mdim(file.path(paths$climate, "PCIC_processed", "daily_ACCESS1-0_rcp45_r1i1p1_19450101-20991231_fraser.nc"))
# temp_m <- read_mdim(file.path(paths$climate, "PCIC_processed", "monthly_ACCESS1-0_rcp45_r1i1p1_19450101-20991231_fraser.nc"))
# 
# time_d <- st_get_dimension_values(temp_d, "time")
# time_m <- st_get_dimension_values(temp_m, "time")



