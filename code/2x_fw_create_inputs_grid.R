#### Create a grid points rds object for spatial analysis

# This script only needs to be run once before calculating the indicators


# Read in PCIC grid
grid_points0 <- read.csv("freshwater/output/PCIC-grid-points_bccoast.csv") 
#read.csv("freshwater/data/processed-data/PCIC-grid-points_fraser.csv") 
# Create grid polys
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

saveRDS(grid_polys, file = here("freshwater", "data", "grid_polys_fw.rds"))