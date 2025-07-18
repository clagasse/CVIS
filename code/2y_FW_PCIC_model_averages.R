## 2x_FW_PCIC_period_average.R
# script to import the period averages from 2x_FW_PCIC_period_average.R
# and do further processing to calculate an ensemble average

# outputs are saved into the PCIC_processed folder for further analysis
#####################################################################

library(here)
setwd(here())
source(file.path(here(), "code", "0_setup.R"))

PCIC_file_loc <- file.path(paths$climate, "PCIC_processed")

# get the processed PC
PCIC_files <- list.files(PCIC_file_loc)

#list of files for rcp45 and rcp85
PCIC_files_rcp45 <- PCIC_files[grep("rcp45", PCIC_files)]
PCIC_files_rcp85 <- PCIC_files[grep("rcp85", PCIC_files)]
