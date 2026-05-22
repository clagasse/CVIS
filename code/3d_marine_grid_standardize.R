# ==============================================================================
# CVIS Marine Grid Standardization (3d_marine_grid_standardize.R)
#
# Description:
#   Interpolates scattered point climate estimates (BCCM, NEP36, SalishSeaCast, 
#   Cumulative Impacts) onto standardized spatial grids, crops them to model-specific 
#   extents using bounding polygon masks, intersects cells with Marine Adaptive Zones 
#   (MAZ), and combines regional grids into unified BC coast SST and SSS databases.
#
# Workflow Steps:
#   1. Load setup environment and load climate point data/spatial bounding polygons.
#   2. Define nearest-neighbour interpolation function (point2rast).
#   3. Interpolate point variables (SST, SSS, SSPH, Cumulative Impacts) to rasters.
#   4. Resample rasters to a standard 3km BCCM resolution grid, mask, and join to MAZ.
#   5. Combine SalishSeaCast, HOTSSea, and BCCM grids into unified coastal SST and SSS grids.
#
# Inputs:
#   - Processed point datasets (.gdb) under Standardized_Marine_data/Points/
#   - Spatial bounding boxes and mask shapefiles under paths$spatial
#
# Outputs:
#   - Standardized cropped vector grids (.gdb) under Standardized_Marine_data/Grid/
#   - Unified BC coast grids (SST_bc_coast.gdb, SSS_bc_coast.gdb) under Standardized_Marine_data/Grid/
#
# Dependencies:
#   - Requires 0_setup.R and 3_marine_utils.R.
# ==============================================================================

# ==================== 1. Setup & Load Climate Data ====================
library(ncdf4)
library(ncmeta)
library(abind)
library(concaveman)
library(pacea)
library(here)

setwd(here())
source(file.path(here(), "code", "0_setup.R"))
source(file.path(code_root, "3_marine_utils.R"))


# remove spherical geometry (s2) for sf operations
sf_use_s2(FALSE)

# Load HOTSSea historical outputs
hotssea_SST <- read_sf(file.path(paths$climate, "Standardized_Marine_data", "Points", "HOTSSea_SST.gdb"))  %>%
  rename(geometry = SHAPE)
st_geometry(hotssea_SST) <- "geometry"   # format spatial data column so it works with sequential functions

# reshape to consistent format with other files
hotssea_SST <- hotssea_SST %>%
  pivot_wider(id_cols = c("geometry"),
    names_from = c("scenario", "month"),
    names_prefix = "SST_",
    values_fn = mean)


# Load climate data that was the result of 3a marine data import data
BCCM_SST <- read_sf(file.path(paths$climate, "BCCM", "BCCMmonthly_SST.gdb"))  %>%
  rename(geometry = SHAPE)
st_geometry(BCCM_SST) <- "geometry"   # format spatial data column so it works with sequential functions

BCCM_SSS <- read_sf(file.path(paths$climate, "BCCM", "BCCMmonthly_SSS.gdb")) %>%
  rename(geometry = SHAPE)
st_geometry(BCCM_SSS) <- "geometry"

BCCM_SSPH <- read_sf(file.path(paths$climate, "BCCM", "BCCMmonthly_SSPH.gdb")) %>%
  rename(geometry = SHAPE)
st_geometry(BCCM_SSPH) <- "geometry"

NEP_SST <- read_sf(file.path(paths$climate, "NEP36_MonthlyData", "NEPmonthly_SST.gdb")) %>%
  rename(geometry = SHAPE)
st_geometry(NEP_SST) <- "geometry"

NEP_SSS <- read_sf(file.path(paths$climate, "NEP36_MonthlyData", "NEPmonthly_SSS.gdb")) %>%
  rename(geometry = SHAPE)
st_geometry(NEP_SSS) <- "geometry"

NEP_SSPH <- read_sf(file.path(paths$climate, "NEP36_MonthlyData", "NEPmonthly_SSPH.gdb")) %>%
  rename(geometry = SHAPE)
st_geometry(NEP_SSPH) <- "geometry"

SSC_SST <- read_sf(file.path(paths$climate, "SalishSeaCast_MonthlyData", "SSCmonthly_SST.gdb")) %>%
  rename(geometry = SHAPE)
st_geometry(SSC_SST) <- "geometry"

SSC_SSS <- read_sf(file.path(paths$climate, "SalishSeaCast_MonthlyData", "SSCmonthly_SSS.gdb")) %>%
  rename(geometry = SHAPE)
st_geometry(SSC_SSS) <- "geometry"

CI_points <- read_sf(file.path(paths$climate, "Standardized_Marine_data", "Points", "CI_points.gdb")) %>%
  rename(geometry = SHAPE)
st_geometry(CI_points) <- "geometry"

CMIP_SST <- read_sf(file.path(paths$climate, "Standardized_Marine_data", "Points", "CMIP6_ssp245_SST_1981-2010.gdb")) %>%
  rename(geometry = SHAPE)

#------Import bounding and masking/clipping polygons ----------
# Load polygons shp files that will be used to set the extent of the interpolation or clip the final vector grid.
EEZ <- read_sf(file.path(paths$spatial, "BC_EEZ", "BC_EEZ.shp")) %>% st_transform(, crs = "EPSG:3005")                              # Full BC EEZ
SSCbox <- read_sf(file.path(paths$climate, "SalishSeaCast_MonthlyData", "SSC_boundingbox.shp")) %>% st_transform(, crs = "EPSG:3005") # Box of full SSC extent
BCCM_mask2 <- read_sf(file.path(paths$climate, "BCCM", "BCCM_mask2.shp"))  %>% st_transform(, crs = "EPSG:3005")                       # EEZ excluding north west corner
SSC_mask <- read_sf(file.path(paths$climate, "SalishSeaCast_MonthlyData", "SSC_mask2.shp")) %>% st_transform(, crs = "EPSG:3005")      # surrounds the SSC data with values but excludes inlets and USA, and NA values
NEP_mask <- read_sf(file.path(paths$climate, "NEP36_MonthlyData", "NEP_mask.shp"))     %>% st_transform(, crs = "EPSG:3005")         # Clips out uncertain results including those on East coast of Haida Gwaii
CI_mask <- read_sf(file.path(paths$spatial, "CumulativeImpacts", "CI_mask.shp"))  %>% st_transform(, crs = "EPSG:3005")       # EEZ polygon made from extent of original CI layer
MAZ <- read_sf(file.path(paths$spatial, "MAZ", "MAZ_Final.shp")) %>% st_transform(, crs = "EPSG:3005")     # MAZ polygons to join to completed grid
MAZ$MAZ_Acrony <- as.factor(MAZ$MAZ_Acrony) # Change MAZ acronym variable to a factor variable

# ==================== 2. Define Interpolation Functions ====================
point2rast <- function(data, spatobj, loc = c("x", "y"), cellsize, nnmax = 4,
                       as = c("SpatRast", "SpatVect")) {

  requireNamespace("methods", quietly = TRUE)
  requireNamespace("terra", quietly = TRUE)
  requireNamespace("gstat", quietly = TRUE)
  requireNamespace("sf", quietly = TRUE)
  requireNamespace("stats", quietly = TRUE)

  stopifnot("must provide cellsize value" = exists("cellsize"))
  stopifnot("must specify valid value for 'as'" = as %in% c("SpatRast", "SpatVect"))

  if (!any(sapply(c("sf", "Spatial"), function(cl) methods::is(data, cl)))) {

    data <- as.data.frame(data)

    if (!length(dim(loc))) {

      stopifnot("loc vector must of be of length==2 " = length(loc) == 2)
      stopifnot("loc names not found in data" = loc %in% names(data))

      coords <- setNames(as.data.frame(data[, loc]), c("x", "y"))

      tdat <- as.data.frame(data[, -which(colnames(data) %in% loc), drop = FALSE])

    } else {

      stopifnot("loc data must be a matrix or dataframe of two columns" =
        length(dim(loc)) == 2 & dim(loc)[2] == 2)
      stopifnot("loc data is not of equal length to data" = nrow(data) == nrow(loc))

      coords <- setNames(as.data.frame(loc), c("x", "y"))

      tdat <- as.data.frame(data[, !colnames(data) %in% colnames(loc), drop = FALSE])

    }
  }

  if (methods::is(data, "Spatial")) {
    coords <- setNames(as.data.frame(data)[, c("coords.x1", "coords.x2")], c("x", "y"))
    tdat <- as.data.frame(data)[, names(data), drop = FALSE]
  }

  if (methods::is(data, "sf")) {
    coords <- setNames(as.data.frame(matrix(unlist(data$geometry), ncol = 2, byrow = TRUE)), c("x", "y"))
    tdat <- as.data.frame(data)[, -which(names(data) == "geometry"), drop = FALSE]
  }

  tbb <- terra::ext(spatobj)
  if (!any(coords$x >= tbb$xmin & coords$x <= tbb$xmax &
    coords$y >= tbb$ymin & coords$y <= tbb$ymax)) {
    warning("'loc' coordinates within spatobj extent = 0; check crs or extent of spatobj")
  }

  terror <- try(terra::crs(spatobj), silent = TRUE)
  if ("try-error" %in% class(terror)) {
    r <- terra::rast(terra::ext(spatobj), res = c(cellsize))
  } else {
    r <- terra::rast(terra::ext(spatobj), res = c(cellsize), crs = terra::crs(spatobj))
  }

  nn.pred <- apply(tdat, 2, FUN = nnfit, r = r, loc = loc, coords = coords, nnmax = nnmax)
  xyz <- cbind(as.data.frame(suppressWarnings(terra::crds(r))), nn.pred)

  if (as[1] == "SpatRast") {
    spat <- terra::rast(xyz, type = "xyz", crs = terra::crs(r))
  }
  if (as[1] == "SpatVect") {
    spat <- terra::vect(xyz, geom = c("x", "y"), crs = terra::crs(r))
  }

  return(spat)
}

#' nearest neighbour fit function
#' @noRd
nnfit <- function(x, r, loc, coords, nnmax) {

  requireNamespace("stats", quietly = TRUE)

  xdat <- stats::na.omit(data.frame(xvar = as.vector(x), coords))

  f <- paste0("xvar", " ~ 1")
  lf <- paste0("~", paste(loc, collapse = "+"))

  gs <- gstat::gstat(formula = xvar ~ 1, locations = ~ x + y, data = xdat, nmax = nnmax, set = list(idp = 0))
  nn <- terra::interpolate(r, gs, debug.level = 0)
  return(as.vector(nn$var1.pred))
}

# ==================== 3. Interpolate Point Data to Rasters ====================
# Interpolate climate data using the PACEA nearest neighbour interpolation function, point2rast
# Establish parameters of point2rast function
llnames <- c("x", "y")
nmax <- 4 # I think this is how many points the nearest neighbour function considers, 4 is small, meaning its a local interpolation

# Interpolate BCCM
BCCM_SST_interpolation <- point2rast(data = BCCM_SST,
  spatobj = EEZ,
  loc = llnames,
  cellsize = 3000,
  nnmax = nmax,
  as = "SpatRast")

BCCM_SSS_interpolation <- point2rast(data = BCCM_SSS,
  spatobj = EEZ,
  loc = llnames,
  cellsize = 3000,
  nnmax = nmax,
  as = "SpatRast")

BCCM_SSPH_interpolation <- point2rast(data = BCCM_SSPH,
  spatobj = EEZ,
  loc = llnames,
  cellsize = 3000,
  nnmax = nmax,
  as = "SpatRast")

NEP_SST_interpolation <- point2rast(data = NEP_SST,
  spatobj = EEZ,
  loc = llnames,
  cellsize = 3000,
  nnmax = nmax,
  as = "SpatRast")

NEP_SSS_interpolation <- point2rast(data = NEP_SSS,
  spatobj = EEZ,
  loc = llnames,
  cellsize = 3000,
  nnmax = nmax,
  as = "SpatRast")

NEP_SSPH_interpolation <- point2rast(data = NEP_SSPH,
  spatobj = EEZ,
  loc = llnames,
  cellsize = 3000,
  nnmax = nmax,
  as = "SpatRast")

SSC_SSS_interpolation <- point2rast(data = SSC_SSS,
  spatobj = SSCbox,
  loc = llnames,
  cellsize = 500,
  nnmax = nmax,
  as = "SpatRast")

SSC_SST_interpolation <- point2rast(data = SSC_SST,
  spatobj = SSCbox,
  loc = llnames,
  cellsize = 500,
  nnmax = nmax,
  as = "SpatRast")

CI_interpolation <- point2rast(data = CI_points,
  spatobj = CI_mask,
  loc = llnames,
  cellsize = 1000,
  nnmax = nmax,
  as = "SpatRast")

hotssea_interpolation <- point2rast(data = hotssea_SST,
  spatobj = SSCbox,
  loc = llnames,
  cellsize = 1000,
  nnmax = nmax,
  as = "SpatRast")


# ==================== 4. Resample, Mask, Join to MAZ & Export Grids ====================
BCCM_SST_cropped <- BCCM_SST_interpolation %>%      # BCCM doesn't need to be resampled as we are converting the other grids to its resolution
  mask(BCCM_mask2) %>%                              # Mask the raster. if you end code here it is a spatraster
  stars::st_as_stars() %>%                         # These two lines turn it into raster into a vector grid
  sf::st_as_sf()  %>%
  st_join(left = FALSE, MAZ["MAZ_Acrony"]) %>%     # Join vector grid to MAZ polygons, adding the column "MAZ_Acrony"
  sf::st_write(file.path(paths$climate, "Standardized_Marine_data", "Grid",           # Save as GDB
    "BCCM_SST_cropped.gdb"), driver = "OpenFileGDB", append = FALSE)

BCCM_SSS_cropped <- BCCM_SSS_interpolation %>%
  mask(BCCM_mask2) %>%
  stars::st_as_stars() %>%
  sf::st_as_sf()  %>%
  st_join(left = FALSE, MAZ["MAZ_Acrony"]) %>%
  sf::st_write(file.path(paths$climate,  "Standardized_Marine_data", "Grid",
    "BCCM_SSS_cropped.gdb"), driver = "OpenFileGDB", append = FALSE)

BCCM_SSPH_cropped <- BCCM_SSPH_interpolation %>%
  mask(BCCM_mask2) %>%
  stars::st_as_stars() %>%
  sf::st_as_sf()  %>%
  st_join(left = FALSE, MAZ["MAZ_Acrony"]) %>%
  sf::st_write(file.path(paths$climate,  "Standardized_Marine_data", "Grid",
    "BCCM_SSPH_cropped.gdb"), driver = "OpenFileGDB", append = FALSE)

NEP_SST_cropped <- NEP_SST_interpolation %>%
  resample(BCCM_SST_interpolation,    # Resample interpolated raster to BCCM Grid standard as it has the largest resolution
    method = "near") %>%       # Use nearest neighbor method because we want the local values to have the largest impact
  mask(NEP_mask) %>%
  stars::st_as_stars() %>%
  sf::st_as_sf()  %>%
  st_join(left = FALSE, MAZ["MAZ_Acrony"]) %>%
  sf::st_write(file.path(paths$climate,       "Standardized_Marine_data", "Grid",
    "NEP_SST_cropped.gdb"), driver = "OpenFileGDB", append = FALSE)

NEP_SSS_cropped <- NEP_SSS_interpolation %>%
  resample(BCCM_SST_interpolation, method = "near") %>%
  mask(NEP_mask) %>%
  stars::st_as_stars() %>%
  sf::st_as_sf() %>%
  st_join(left = FALSE, MAZ["MAZ_Acrony"]) %>%
  sf::st_write(file.path(paths$climate,  "Standardized_Marine_data", "Grid",
    "NEP_SSS_cropped.gdb"), driver = "OpenFileGDB", append = FALSE)

NEP_SSPH_cropped <- NEP_SSPH_interpolation %>%
  resample(BCCM_SST_interpolation, method = "near") %>%
  mask(NEP_mask) %>%
  stars::st_as_stars() %>%
  sf::st_as_sf()  %>%
  st_join(left = FALSE, MAZ["MAZ_Acrony"]) %>%
  sf::st_write(file.path(paths$climate,    "Standardized_Marine_data", "Grid",
    "NEP_SSPH_cropped.gdb"), driver = "OpenFileGDB", append = FALSE)

SSC_SST_cropped <- SSC_SST_interpolation %>%
  resample(BCCM_SST_interpolation, method = "near") %>%
  mask(SSC_mask) %>%
  stars::st_as_stars() %>%
  sf::st_as_sf()  %>%
  st_join(left = FALSE, MAZ["MAZ_Acrony"]) %>%
  sf::st_write(file.path(paths$climate,     "Standardized_Marine_data", "Grid",
    "SSC_SST_cropped.gdb"), driver = "OpenFileGDB", append = FALSE)

SSC_SSS_cropped <- SSC_SSS_interpolation %>%
  resample(BCCM_SST_interpolation, method = "near") %>%
  mask(SSC_mask) %>%
  stars::st_as_stars() %>%
  sf::st_as_sf() %>%
  st_join(left = FALSE, MAZ["MAZ_Acrony"]) %>%
  sf::st_write(file.path(paths$climate, "Standardized_Marine_data", "Grid",
    "SSC_SSS_cropped.gdb"), driver = "OpenFileGDB", append = FALSE)

CI_cropped <- CI_interpolation %>%
  resample(BCCM_SST_interpolation, method = "near") %>%
  mask(CI_mask) %>%
  stars::st_as_stars() %>%
  sf::st_as_sf()  %>%
  st_join(left = FALSE, MAZ["MAZ_Acrony"]) %>%
  sf::st_write(file.path(paths$climate, "Standardized_Marine_data", "Grid",
    "CI_cropped.gdb"), driver = "OpenFileGDB", append = FALSE)

hotssea_cropped <- hotssea_interpolation %>%
  resample(BCCM_SST_interpolation, method = "near") %>%
  mask(SSC_mask) %>%
  stars::st_as_stars() %>%
  sf::st_as_sf() %>%
  st_join(left = FALSE, MAZ["MAZ_Acrony"]) %>%
  mutate(MAZ_Acrony = as.character(MAZ_Acrony)) %>%
  sf::st_write(file.path(paths$climate, "Standardized_Marine_data", "Grid",
    "hotssea_SST_cropped.gdb"), driver = "OpenFileGDB", append = FALSE)


# ==================== 5. Combine Regional Grids into Unified Coast Grids ====================

## SST - use hotssea for historic and SSC for projections in GStr, use BCCM everywhere else
SSC_SST_cropped <- st_read(file.path(paths$climate, "Standardized_Marine_data", "Grid", "SSC_SST_cropped.gdb"))
BCCM_SST_cropped <- st_read(file.path(paths$climate, "Standardized_Marine_data", "Grid", "BCCM_SST_cropped.gdb"))
hotssea_cropped <- st_read(file.path(paths$climate, "Standardized_Marine_data", "Grid", "hotssea_SST_cropped.gdb"))

GSTR_SST_bind <- SSC_SST_cropped %>%
  select(-contains("_H_")) %>%
  cbind(st_drop_geometry(hotssea_cropped)) %>%
  select(-MAZ_Acrony.1)
# st_join(hotssea_cropped, left = FALSE)

# Fix month names for consistency with BCCM
h_cols_index <- grep("_H_", names(GSTR_SST_bind))
new_names <- sprintf("SST_H_%02d", seq_along(h_cols_index))
colnames(GSTR_SST_bind)[h_cols_index] <- new_names

# create grid object of SST for entire coast
SST_grid <- BCCM_SST_cropped %>%
  filter(MAZ_Acrony != "GStr") %>%
  rbind(GSTR_SST_bind)

## SSS - use SSC for historic and projections in GStr, use BCCM everywhere else
SSC_SSS_cropped <- st_read(file.path(paths$climate, "Standardized_Marine_data", "Grid", "SSC_SSS_cropped.gdb"))
BCCM_SSS_cropped <- st_read(file.path(paths$climate, "Standardized_Marine_data", "Grid", "BCCM_SSS_cropped.gdb"))

SSS_grid <- BCCM_SSS_cropped %>%
  filter(MAZ_Acrony != "GStr") %>%
  rbind(SSC_SSS_cropped)


## write combined objects for SSS and SST
sf::st_write(SST_grid, file.path(paths$climate, "Standardized_Marine_data", "Grid",
  "SST_bc_coast.gdb"), driver = "OpenFileGDB", append = FALSE)

sf::st_write(SSS_grid, file.path(paths$climate, "Standardized_Marine_data", "Grid",
  "SSS_bc_coast.gdb"), driver = "OpenFileGDB", append = FALSE)

# SST_ROM_grid <- st_read(file.path(paths$climate, "Standardized_Marine_data", "Grid",
#                "SST_bc_coast.gdb"))

# save(SST_ROM_grid, file = file.path(paths$marine, "SST_ROM_grid.Rds"))

# compare stats on gridded vs points
point_SST <- SSC_SST_long %>%
  as_tibble() %>%
  filter(month %in% c(4, 5, 6, 7), MAZ_Acrony == "GStr") %>%
  group_by(scenario) %>%
  summarize(mean    = mean(value, na.rm = T),
    qlowsp = quantile(value, qlowsp, na.rm = T),
    qhighsp = quantile(value, qhighsp, na.rm = T),
    .groups = "drop")

grid_SST <- SSC_SST_long_grid %>%
  as_tibble() %>%
  filter(month %in% c(4, 5, 6, 7), MAZ_Acrony == "GStr") %>%
  group_by(scenario) %>%
  summarize(mean    = mean(value, na.rm = T),
    qlowsp = quantile(value, qlowsp, na.rm = T),
    qhighsp = quantile(value, qhighsp, na.rm = T),
    .groups = "drop")
