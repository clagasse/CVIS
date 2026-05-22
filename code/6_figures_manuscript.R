##  6_figures_manuscript  ####

# Script to make png outputs for CVIS manuscript  
# using plotting functions and data sources

# These png outputs are then read by the 6_CVIS_manuscript for knitting
# Uses existing plotting functions and data sets

# 

# Settings ----------------------------------------------------------------
# set-up used in every script
library(here)
setwd(here())
source(file.path(here(), "code", "0_setup.R"))

#output directory 
output_dir <- file.path(paths$figures, "manuscript")

# select case study CUs for manuscript
casestudy_1 <- "CK-12"
casestudy_2 <- "CM-02"


# Load data ---------------------------------------------------------------

### ----- Load frequently used data sets- ------
# load marine adaptive zone spatial object
load(file.path(paths$marine, "MAZ.Rds"))
# load watershed basins R object
load(file.path(paths$fw, "basins_shp.Rds"))
# make a Fraser basin version
Fr_basin <- filter(basins, BASIN == "FRASER")

# freshwater stream subsets by CU boundary
load(file.path(paths$fw, "fw_streampicks_tscapes.Rdata"))
# migration paths
load(file.path(paths$fw, "fw_upstream_paths.Rdata"))

# spatial models
# stream model outputs for freshwater spawning and rearing indicators
load(file.path(paths$fw, "fw_models_tscapes.Rds"))
# indicator spatial outputs
load(file.path(paths$fw, "fw_stream_indicators_sp.Rds"))
# lakes_Fr - freshwater lakes for plotting
load(file.path(paths$fw, "BC_FWA_LAKES_FR.Rds"))

# Freshwater
fw_file <- get_latest_file(paths$fw, "fw_rearing_indicators.Rdata")
load(fw_file) # loads fw_all, ss_all

# Migration
migr_file <- get_latest_file(paths$fw, "migr_stats.Rdata")
load(migr_file) # loads migr_all, etc.

# Marine
mar_file <- get_latest_file(paths$marine, "marine_stats.Rdata") # look for .Rds
load(mar_file)


#scoring data
load(file.path(paths$output, "scoring_results.Rdata")) 
# Load outputs from 4b (Indicator metrics and sensitivity analysis)
load(file.path(paths$output, "sensitivity_analysis.Rdata")) # loads overall_sensitivity



# subset CU data ----------------------------------------------------------

cu_i <- casestudy_1

cu_run_i <- cu_run[cu_run$FULL_CU_IN == cu_i, ]
#sp_pick <- cu_run$spp[cu_run$FULL_CU_IN == cu_i] # species abbr
#sp_pick_bcfp <- spp_lookup$spp_abr_bcfp[spp_lookup$spp_abr == sp_pick]

cu_timing_i <- cu_timing_Fr %>% filter(FULL_CU_IN == cu_i)

cu_boundary_i <- cu_boundary[cu_boundary$FULL_CU_IN == cu_i, ]

#Lakes within CU boundary (for plotting)
temp <- unlist(st_intersects(cu_boundary_i, lakes_Fr))
lakes_cu <- lakes_Fr[temp,]

# subset nuseds observations
nuseds_cu <- nuseds_Fr[nuseds_Fr$FULL_CU_IN == cu_i, ]

# subset migration path
migr_cu <- migr_list[[cu_i]]

#subset fw models
fw_models_cu <- subset_fw_models(
  fw_models = fw_models,
  cu_i = cu_i,
  stream_cu_picks = stream_cu_picks,
  cu_run = cu_run,
  spp_lookup = spp_lookup,
  to_factor = TRUE
)

fw_sp_ind_cu <- subset_fw_models(
  fw_models = fw_sp_ind,
  cu_i = cu_i,
  stream_cu_picks = stream_cu_picks,
  cu_run = cu_run,
  spp_lookup = spp_lookup,
  to_factor = TRUE
)

cu_timing_long_i <- cu_timing_long[cu_timing_long$FULL_CU_IN == cu_i, ]
cu_timing_i <- cu_timing_Fr[cu_timing_Fr$FULL_CU_IN == cu_i, ]



# Figure 2 - Map of values for freshwater spawning and rearing indicators within a CU boundary

f2 <- stream_indicator_multipanel_plot(fw_sp_ind_cu,
  cu_boundary_i,
  lakes_cu,
  variables = c("favchange_chinook_85_3", "cthr_anad", "tw8_9_45_3", "deltatw8_9_45_3", "deltaflow8_9_45_3", "deltaflow18_9_45_3"),
  plot_titles = c("favchange", "cthr", "tw8proj", "tw8rate", "flow8", "flow18"),
  scico_palette = "roma",
  palette_directions = c(1, -1, -1, -1, 1, -1))

#save as png
ggsave(filename = file.path(output_dir, "figure_2.png"), plot = f2, width = 8, height = 5) 

# Figure 3 - Upstream migration path and time period used to determine migration temperature

f3 <- migration_path_plot(migr_cu,
                          nuseds_cu, 
                          cu_boundary_i)

ggsave(filename = file.path(output_dir, "figure_3.png"), plot = f3) 
                          

# Figure 4 - Lollilop plot of indicator values and standardization function for change in August flow

f4 <- plot_lollipop(all_std_long,
                    indicator_pick = "migrTproj",
                    rcp_pick = sens_rcp_base,
                    period_pick = sens_period_base,
                    dsmodel_pick = "pcicgrid")

ggsave(filename = file.path(output_dir, "figure_4.png"), plot = f4,
       width = 6, height = 8) 

# Figure 5 - Mapped vulnerability scores for freshwater spawning and rearing category.

f5 <- spatial_indicator_plot(all_std_long,
                             cu_boundary,
                            outline = Fr_basin,
                              sp_pick = c("Chinook", "Coho", "Sockeye"),
                              indicator_pick = "flow8pdelta",
                             rcp_pick = sens_rcp_base,
                             period_pick = sens_period_base)
    
ggsave(filename = file.path(output_dir, "figure_5.png"), plot = f5, width = 7, height = 5)                                
                                   
# Figure 6 - Marine adaptive zones and associated mean indicator scores for each indicator - SSTproj, SSTrate, CImpact.

f6 <- plot_maz_lollipop(maz_all)

ggsave(filename = file.path(output_dir, "figure_6.png"), plot = f6)   

# Figure 7 - INdicator tile plot of overall vulnerability scores and individual indicator scores.

f7 <- indicator_cu_tile_plot(all_std_long,
                       scores_tidy)


ggsave(filename = file.path(output_dir, "figure_7.png"), plot = f7,
       width = 8, height = 10) 


# Figure 8 - Violin plot of spread of indicator values for 50 CUs across different scenarios.

#see plots created in script 5c

# Figure 9 - Deviations in overall vulnerability scores across sources of variation.

# see plots created in script 5d

# Figure 10 - Species-level bump plots of change in vulnerability rank for each CU across different sources of variation.

