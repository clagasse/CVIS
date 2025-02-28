### 3a_Marine_import


# SSC <- read_ncdf(file.path(climate_dat, "SalishSeaCast-VNR023_1d_grid_T_mean12.nc"), proxy = FALSE)
# 
# SSC_surf <- read_ncdf(file.path(climate_dat, "SalishSeaCast-VNR023_1d_grid_T_mean12.nc"),proxy = FALSE)
#  SSC <-  st_transform(SSC, 4269)
# SSC_mask <- read_ncdf(file.path(climate_dat, "mesh_mask202108us.nc"),var = c("nav_lat", "nav_lon"), proxy = FALSE)
# 
# 
# surf_nc_lon <- as.vector(SSC_mask$nav_lon)
# surf_nc_lat <- as.vector(SSC_mask$nav_lat)
# surf_var    <- as.vector(SSC$votemper[,,1,1])
# 
# # These are points
# surf_dat <- data.frame(x = surf_nc_lon,
#                        y = surf_nc_lat,
#                        value = surf_var) %>%
#   st_as_sf(coords = c("x", "y"),
#            crs = "EPSG:4326") %>%
#   st_transform(crs = "EPSG:3005")
# 
# # expect_equal(summary(surf_var),
# #              summary(surf_dat$value))
# 
# surf_dat_cave <- surf_dat %>%
#   na.omit() %>%
#   concaveman::concaveman()
# 
# ggplot() +
#   geom_sf(data = bc_coast) +
#   geom_sf(data = surf_dat_cave, col = NA, fill = "red")
# 

# SSC_mask_x <- merge(SSC_mask)
# st_crs(SSC_mask) <- 4269
# 
# SSC_fix2 <- st_crop(SSC, SSC_mask)
# 
# SSC_fix <- SSC[,st_get_dimension_values(SSC, "x") < 0]
# SSC_fix <- SSC_fix[,,st_get_dimension_values(SSC, "y") > 0]
# 
# ggplot() +
#   geom_stars(data = SSC_fix[2,,,1,1])
