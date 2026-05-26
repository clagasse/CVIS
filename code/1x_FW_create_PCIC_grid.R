# ==============================================================================
# CVIS PCIC Spatial Grid Creator (1x_FW_create_PCIC_grid.R)
#
# Description:
#   Creates a spatial polygon grid Rds object from the raw PCIC grid points.
#   This script only needs to be run once to generate inputs for spatial analysis.
#
# Workflow Steps:
#   1. Load setup environment.
#   2. Read the raw PCIC grid points CSV.
#   3. Construct cell polygon boundaries and convert to sf polygons.
#   4. Save the generated grid polygons RDS object to processed_data/freshwater/.
#
# Inputs:
#   - processed_data/freshwater/PCIC-grid-points_bccoast.csv
#
# Outputs:
#   - processed_data/freshwater/grid_polys_fw.rds
#
# Dependencies:
#   - Requires 0_setup.R.
# ==============================================================================

# ==================== 1. Setup & Load Grid Points ====================
library(here)
setwd(here())
source(file.path(here(), "code", "0_setup.R"))

grid_points0 <- read.csv(file.path(paths$fw, "PCIC-grid-points_bccoast.csv"))

# ==================== 2. Generate Grid Polygons ====================
n <- length(grid_points0$lon)
d <- 1/16

grid_polys <- st_as_sf(data.frame(
  id = rep(grid_points0$id, each = 5),
  rep(c("SW0", "NW", "NE", "SE", "SW1"), n),
  lon = c(rep(grid_points0$lon, each = 5) + rep(c(- d/2, -d/2, d/2,  d/2, -d/2), n)),
  lat = c(rep(grid_points0$lat, each = 5) + rep(c(- d/2,  d/2, d/2, -d/2, -d/2), n))
), coords = c("lon", "lat"), crs = 4269) %>% 
  group_by(id) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON") 

# ==================== 3. Save Grid Polygons ====================
saveRDS(grid_polys, file = file.path(paths$fw, "grid_polys_fw.rds"))