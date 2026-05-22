# ==============================================================================
# CVIS Marine Data Import & Preparation (3a_marine_data_import.R)
#
# Description:
#   Opens NEP36, SalishSeaCast, and BCCM climate model NetCDFs, extracts monthly 
#   averages of surface variables (SST, SSS, SSpH) for historical and future periods, 
#   projects them to BC Albers (EPSG:3005), intersects/masks points to the EEZ/MAZ, 
#   and loads/saves HOTSSea and CMIP6 SST projections.
#
# Workflow Steps:
#   1. Load setup environment and configuration variables.
#   2. Load and process NEP36 temperature, salinity, and pH NetCDF files.
#   3. Load, format, and filter SalishSeaCast temperature and salinity NetCDFs.
#   4. Load and process BCCM NetCDFs (SST, SSS, SSPH) and output surface point GDBs.
#   5. Load Cumulative Impacts spatial data and extract centroids.
#   6. Mask model points, intersect with Marine Adaptive Zones (MAZ), and export subset point GDBs.
#   7. Load and process HOTSSea model data using pacea.
#   8. Load and process CMIP6 high resolution SST NetCDF projections.
#
# Inputs:
#   - Model NetCDF datasets in paths$climate
#   - Cumulative Impacts GDB: paths$spatial/Cumulative_Impacts_Pacfic_Canada.gdb
#   - Marine Adaptive Zones (MAZ) Final shapefile: paths$spatial/MAZ/MAZ_Final.shp
#
# Outputs:
#   - Processed point databases (.gdb) under paths$climate and paths$spatial
#
# Dependencies:
#   - Requires 0_setup.R.
# ==============================================================================

# ==================== 1. Setup & Environment ====================
library(here)
here()
source(here("code", "0_setup.R"))

# start year for historical period in HOTSSEA and CMIP6
h_start <- period_lookup$start_year[period_lookup$model == "CMIP6_SST" & period_lookup$period_code == 0]
h_end   <- period_lookup$end_year[period_lookup$model == "CMIP6_SST" & period_lookup$period_code == 0]
# start and end year of mid-century period
p3_start <- period_lookup$start_year[period_lookup$model == "CMIP6_SST" & period_lookup$period_code == 3]
p3_end <- period_lookup$end_year[period_lookup$model == "CMIP6_SST" & period_lookup$period_code == 3]
# start and end year of end-of-century period
p5_start <- period_lookup$start_year[period_lookup$model == "CMIP6_SST" & period_lookup$period_code == 5]
p5_end <- period_lookup$end_year[period_lookup$model == "CMIP6_SST" & period_lookup$period_code == 5]



if (!require("pacman")) install.packages("pacman")
# working with NetCDFs
pacman::p_load(ncdf4)
pacman::p_load(ncmeta)
pacman::p_load(abind)
pacman::p_load(concaveman)

pacman::p_load("spgwr")
pacman::p_load("spatstat")
pacman::p_load("tmap")
pacman::p_load("gstat")
pacman::p_load("maps")
# install_github("pbs-assess/pacea")
pacman::p_load(pacea)



# ==================== 2. NEP36 Data Import ====================
# Load NEP 36 temperature data
NEP_T_H <- nc_open(file.path(paths$climate, "NEP36_MonthlyData", "NEP36-CanOE_temp_historical_1986-2005_monthly.nc"))
NEP_T_45 <- nc_open(file.path(paths$climate, "NEP36_MonthlyData", "NEP36-CanOE_temp_RCP45_2046-2065_monthly.nc"))
NEP_T_85 <- nc_open(file.path(paths$climate, "NEP36_MonthlyData", "NEP36-CanOE_temp_RCP85_2046-2065_monthly.nc"))
# print(NEP_T_H) #use this to figure out names of variables and dimensions

# Read in the temperature variable for each scenario
TH <- ncvar_get(NEP_T_H, "temp", verbose = FALSE) # read temp variable
T45 <- ncvar_get(NEP_T_45, "temp", verbose = FALSE)
T85 <- ncvar_get(NEP_T_85, "temp", verbose = FALSE)
# dim(T85) #use this to figure out what order the dimensions are in. it will be the same for all

# Read in lat and long from one file, they are the same for all of them
lon <- ncvar_get(NEP_T_H, "nav_lon", verbose = FALSE) # read lon variable
lat <- ncvar_get(NEP_T_H, "nav_lat", verbose = FALSE) # read lat variable

## figure out what value is used to define no data
fillvalue <- ncatt_get(NEP_T_H, "temp", "_FillValue")
# print(fillvalue$value)

# replace fill values with standard NA value
TH[TH == fillvalue$value] <- NA
T45[T45 == fillvalue$value] <- NA
T85[T85 == fillvalue$value] <- NA

# Extracting variables as vectors
NEPlon <- as.vector(lon)
NEPlat <- as.vector(lat)

# Extract surface data (3rd dimension) for each month (4th dimension) as vectors
SST_H_01 <- as.vector(TH[, , 1, 1])
SST_H_02 <- as.vector(TH[, , 1, 2])
SST_H_03 <- as.vector(TH[, , 1, 3])
SST_H_04 <- as.vector(TH[, , 1, 4])
SST_H_05 <- as.vector(TH[, , 1, 5])
SST_H_06 <- as.vector(TH[, , 1, 6])
SST_H_07 <- as.vector(TH[, , 1, 7])
SST_H_08 <- as.vector(TH[, , 1, 8])
SST_H_09 <- as.vector(TH[, , 1, 9])
SST_H_10 <- as.vector(TH[, , 1, 10])
SST_H_11 <- as.vector(TH[, , 1, 11])
SST_H_12 <- as.vector(TH[, , 1, 12])

SST_45_01 <- as.vector(T45[, , 1, 1])
SST_45_02 <- as.vector(T45[, , 1, 2])
SST_45_03 <- as.vector(T45[, , 1, 3])
SST_45_04 <- as.vector(T45[, , 1, 4])
SST_45_05 <- as.vector(T45[, , 1, 5])
SST_45_06 <- as.vector(T45[, , 1, 6])
SST_45_07 <- as.vector(T45[, , 1, 7])
SST_45_08 <- as.vector(T45[, , 1, 8])
SST_45_09 <- as.vector(T45[, , 1, 9])
SST_45_10 <- as.vector(T45[, , 1, 10])
SST_45_11 <- as.vector(T45[, , 1, 11])
SST_45_12 <- as.vector(T45[, , 1, 12])

SST_85_01 <- as.vector(T85[, , 1, 1])
SST_85_02 <- as.vector(T85[, , 1, 2])
SST_85_03 <- as.vector(T85[, , 1, 3])
SST_85_04 <- as.vector(T85[, , 1, 4])
SST_85_05 <- as.vector(T85[, , 1, 5])
SST_85_06 <- as.vector(T85[, , 1, 6])
SST_85_07 <- as.vector(T85[, , 1, 7])
SST_85_08 <- as.vector(T85[, , 1, 8])
SST_85_09 <- as.vector(T85[, , 1, 9])
SST_85_10 <- as.vector(T85[, , 1, 10])
SST_85_11 <- as.vector(T85[, , 1, 11])
SST_85_12 <- as.vector(T85[, , 1, 12])

# Recombine information as spatial data frame in CRS 3005
NEP_SST <- data.frame(x = NEPlon, y = NEPlat,
  SST_H_01, SST_H_02, SST_H_03, SST_H_04, SST_H_05, SST_H_06,
  SST_H_07, SST_H_08, SST_H_09, SST_H_10, SST_H_11, SST_H_12,
  SST_45_01, SST_45_02, SST_45_03, SST_45_04, SST_45_05, SST_45_06,
  SST_45_07, SST_45_08, SST_45_09, SST_45_10, SST_45_11, SST_45_12,
  SST_85_01, SST_85_02, SST_85_03, SST_85_04, SST_85_05, SST_85_06,
  SST_85_07, SST_85_08, SST_85_09, SST_85_10, SST_85_11, SST_85_12) %>%
  st_as_sf(coords = c("x", "y"),
    crs = "EPSG:4326") %>%
  st_transform(crs = "EPSG:3005")
# plot(NEP_SST)


# remove unneeded variables
rm(T45, TH, T85,
  SST_H_01, SST_H_02, SST_H_03, SST_H_04, SST_H_05, SST_H_06,
  SST_H_07, SST_H_08, SST_H_09, SST_H_10, SST_H_11, SST_H_12,
  SST_45_01, SST_45_02, SST_45_03, SST_45_04, SST_45_05, SST_45_06,
  SST_45_07, SST_45_08, SST_45_09, SST_45_10, SST_45_11, SST_45_12,
  SST_85_01, SST_85_02, SST_85_03, SST_85_04, SST_85_05, SST_85_06,
  SST_85_07, SST_85_08, SST_85_09, SST_85_10, SST_85_11, SST_85_12)


# Load NEP 36 salinity data
NEP_S_H <- nc_open(file.path(paths$climate, "NEP36_MonthlyData", "NEP36-CanOE_salt_historical_1986-2005_monthly.nc"))
NEP_S_45 <- nc_open(file.path(paths$climate, "NEP36_MonthlyData", "NEP36-CanOE_salt_RCP45_2046-2065_monthly.nc"))

# Read in the temperature variable for each scenario
SH <- ncvar_get(NEP_S_H, "salt", verbose = FALSE) # read temp variable
S45 <- ncvar_get(NEP_S_45, "salt", verbose = FALSE)
# dim(S45) #use this to figure out what order the dimensions are in

# replace fill values with standard NA value
SH[SH == fillvalue$value] <- NA
S45[S45 == fillvalue$value] <- NA

# Extract surface data (3rd dimension) for each month (4th dimension) as vectors
SSS_H_01 <- as.vector(SH[, , 1, 1])
SSS_H_02 <- as.vector(SH[, , 1, 2])
SSS_H_03 <- as.vector(SH[, , 1, 3])
SSS_H_04 <- as.vector(SH[, , 1, 4])
SSS_H_05 <- as.vector(SH[, , 1, 5])
SSS_H_06 <- as.vector(SH[, , 1, 6])
SSS_H_07 <- as.vector(SH[, , 1, 7])
SSS_H_08 <- as.vector(SH[, , 1, 8])
SSS_H_09 <- as.vector(SH[, , 1, 9])
SSS_H_10 <- as.vector(SH[, , 1, 10])
SSS_H_11 <- as.vector(SH[, , 1, 11])
SSS_H_12 <- as.vector(SH[, , 1, 12])

SSS_45_01 <- as.vector(S45[, , 1, 1])
SSS_45_02 <- as.vector(S45[, , 1, 2])
SSS_45_03 <- as.vector(S45[, , 1, 3])
SSS_45_04 <- as.vector(S45[, , 1, 4])
SSS_45_05 <- as.vector(S45[, , 1, 5])
SSS_45_06 <- as.vector(S45[, , 1, 6])
SSS_45_07 <- as.vector(S45[, , 1, 7])
SSS_45_08 <- as.vector(S45[, , 1, 8])
SSS_45_09 <- as.vector(S45[, , 1, 9])
SSS_45_10 <- as.vector(S45[, , 1, 10])
SSS_45_11 <- as.vector(S45[, , 1, 11])
SSS_45_12 <- as.vector(S45[, , 1, 12])

# Recombine information as spatial data frame in CRS 3005
NEP_SSS <- data.frame(x = NEPlon, y = NEPlat,
  SSS_H_01, SSS_H_02, SSS_H_03, SSS_H_04, SSS_H_05, SSS_H_06,
  SSS_H_07, SSS_H_08, SSS_H_09, SSS_H_10, SSS_H_11, SSS_H_12,
  SSS_45_01, SSS_45_02, SSS_45_03, SSS_45_04, SSS_45_05, SSS_45_06,
  SSS_45_07, SSS_45_08, SSS_45_09, SSS_45_10, SSS_45_11, SSS_45_12) %>%
  st_as_sf(coords = c("x", "y"),
    crs = "EPSG:4326") %>%
  st_transform(crs = "EPSG:3005")
# plot(NEP_SST)

# remove unneeded variables
rm(SH, S45,
  SSS_H_01, SSS_H_02, SSS_H_03, SSS_H_04, SSS_H_05, SSS_H_06,
  SSS_H_07, SSS_H_08, SSS_H_09, SSS_H_10, SSS_H_11, SSS_H_12,
  SSS_45_01, SSS_45_02, SSS_45_03, SSS_45_04, SSS_45_05, SSS_45_06,
  SSS_45_07, SSS_45_08, SSS_45_09, SSS_45_10, SSS_45_11, SSS_45_12)

# Load NEP 36 pH data
NEP_PH_H <- nc_open(file.path(paths$climate, "NEP36_MonthlyData", "NEP36-CanOE_PH_historical_1986-2005_monthly.nc"))
NEP_PH_45 <- nc_open(file.path(paths$climate, "NEP36_MonthlyData", "NEP36-CanOE_PH_RCP45_2046-2065_monthly.nc"))

# Read in the temperature variable for each scenario
PHH <- ncvar_get(NEP_PH_H, "PH", verbose = FALSE) # read temp variable
PH45 <- ncvar_get(NEP_PH_45, "PH", verbose = FALSE)

# replace fill values with standard NA value
PHH[PHH == fillvalue$value] <- NA
PH45[PH45 == fillvalue$value] <- NA
# Extract surface data (3rd dimension) for each month (4th dimension) as vectors
SSPH_H_01 <- as.vector(PHH[, , 1, 1])
SSPH_H_02 <- as.vector(PHH[, , 1, 2])
SSPH_H_03 <- as.vector(PHH[, , 1, 3])
SSPH_H_04 <- as.vector(PHH[, , 1, 4])
SSPH_H_05 <- as.vector(PHH[, , 1, 5])
SSPH_H_06 <- as.vector(PHH[, , 1, 6])
SSPH_H_07 <- as.vector(PHH[, , 1, 7])
SSPH_H_08 <- as.vector(PHH[, , 1, 8])
SSPH_H_09 <- as.vector(PHH[, , 1, 9])
SSPH_H_10 <- as.vector(PHH[, , 1, 10])
SSPH_H_11 <- as.vector(PHH[, , 1, 11])
SSPH_H_12 <- as.vector(PHH[, , 1, 12])

SSPH_45_01 <- as.vector(PH45[, , 1, 1])
SSPH_45_02 <- as.vector(PH45[, , 1, 2])
SSPH_45_03 <- as.vector(PH45[, , 1, 3])
SSPH_45_04 <- as.vector(PH45[, , 1, 4])
SSPH_45_05 <- as.vector(PH45[, , 1, 5])
SSPH_45_06 <- as.vector(PH45[, , 1, 6])
SSPH_45_07 <- as.vector(PH45[, , 1, 7])
SSPH_45_08 <- as.vector(PH45[, , 1, 8])
SSPH_45_09 <- as.vector(PH45[, , 1, 9])
SSPH_45_10 <- as.vector(PH45[, , 1, 10])
SSPH_45_11 <- as.vector(PH45[, , 1, 11])
SSPH_45_12 <- as.vector(PH45[, , 1, 12])

# Recombine information as spatial data frame in CRS 3005
NEP_SSPH <- data.frame(x = NEPlon, y = NEPlat,
  SSPH_H_01, SSPH_H_02, SSPH_H_03, SSPH_H_04, SSPH_H_05, SSPH_H_06,
  SSPH_H_07, SSPH_H_08, SSPH_H_09, SSPH_H_10, SSPH_H_11, SSPH_H_12,
  SSPH_45_01, SSPH_45_02, SSPH_45_03, SSPH_45_04, SSPH_45_05, SSPH_45_06,
  SSPH_45_07, SSPH_45_08, SSPH_45_09, SSPH_45_10, SSPH_45_11, SSPH_45_12) %>%
  st_as_sf(coords = c("x", "y"),
    crs = "EPSG:4326") %>%
  st_transform(crs = "EPSG:3005")
# plot(NEP_SSPH)

# remove unneeded variables
rm(lat, lon, fillvalue,  PHH, PH45, NEPlat, NEPlon,
  NEP_T_H, NEP_T_45, NEP_T_85, NEP_S_H, NEP_S_45, NEP_PH_H, NEP_PH_45,
  SSPH_H_01, SSPH_H_02, SSPH_H_03, SSPH_H_04, SSPH_H_05, SSPH_H_06,
  SSPH_H_07, SSPH_H_08, SSPH_H_09, SSPH_H_10, SSPH_H_11, SSPH_H_12,
  SSPH_45_01, SSPH_45_02, SSPH_45_03, SSPH_45_04, SSPH_45_05, SSPH_45_06,
  SSPH_45_07, SSPH_45_08, SSPH_45_09, SSPH_45_10, SSPH_45_11, SSPH_45_12)

# Export NEP points as shp file if needed
sf::st_write(NEP_SSS, file.path(paths$climate, "NEP36_MonthlyData", "NEPmonthly_SSS.gdb"), driver = "OpenFileGDB", append = FALSE)
sf::st_write(NEP_SST, file.path(paths$climate, "NEP36_MonthlyData", "NEPmonthly_SST.gdb"), driver = "OpenFileGDB", append = FALSE)
sf::st_write(NEP_SSPH, file.path(paths$climate, "NEP36_MonthlyData", "NEPmonthly_SSPH.gdb"), driver = "OpenFileGDB",  append = FALSE)



# ==================== 3. SalishSeaCast Data Import ====================
# list variables
# nc_vars(file.path(paths$climate, "SalishSeaCast_MonthlyData", "SalishSeaCast-VNR033_1d_grid_T_mean12_4.5_2046-65.nc"))
# NOTE: NO PH Data yet, could ask Amber for it
SSC_H <- nc_open(file.path(paths$climate, "SalishSeaCast_MonthlyData", "SalishSeaCast-VNR023_1d_grid_T_mean12_1986-05.nc"))
SSC_45 <- nc_open(file.path(paths$climate, "SalishSeaCast_MonthlyData", "SalishSeaCast-VNR033_1d_grid_T_mean12_4.5_2046-65.nc"))
SSC_85 <- nc_open(file.path(paths$climate, "SalishSeaCast_MonthlyData", "SalishSeaCast-VNR034_1d_grid_T_mean12_8.5_2046-65.nc"))
# print(SSC_H) ## See the names of the variables and dimensions

vo_pick <- c("votemper", "vosaline")

vodf <- tribble(
  ~var, ~abbr, ~long_name, ~units,
  "votemper", "SST", "Temperature", "Celsius",
  "vosaline", "SSS", "Salinity", "PSU"
)

for (v in 1:nrow(vodf)) {
  # Read in the temperature and salinity variable for each scenario
  TH <- ncvar_get(SSC_H, vodf$var[v], verbose = FALSE) # read temp variable
  T45 <- ncvar_get(SSC_45, vodf$var[v], verbose = FALSE)
  T85 <- ncvar_get(SSC_85, vodf$var[v], verbose = FALSE)

  # figure out what order the dimensions are in. here the order is [long, lat, depth, month]
  # dim(T85)

  ## figure out what value is used to define no data. This is the same value for salinity and temperature
  fillvalue <- ncatt_get(SSC_H, vodf$var[v], "_FillValue")
  # print(fillvalue$value)

  # replace fill values with standard NA value
  TH[TH == fillvalue$value] <- NA
  T45[T45 == fillvalue$value] <- NA
  T85[T85 == fillvalue$value] <- NA

  # Read in lat and long from one of the files, they are the same for all of them
  lon <- ncvar_get(SSC_H, "nav_lon", verbose = FALSE) # read lon variable
  lat <- ncvar_get(SSC_H, "nav_lat", verbose = FALSE) # read lat variable

  # extract surface data (3rd dimension) for each month (4th dimension) as vectors
  SSClon <- as.vector(lon)
  SSClat <- as.vector(lat)

  for (i in 1:12) {
    VO_H_temp <- as.vector(TH[, , 1, i])
    VO_45_temp <- as.vector(T45[, , 1, i])
    VO_85_temp <- as.vector(T85[, , 1, i])

    if (i == 1) {
      VO_H <- VO_H_temp
      VO_45 <- VO_45_temp
      VO_85 <- VO_85_temp
    } else {
      VO_H <- cbind(VO_H, VO_H_temp)
      VO_45 <- cbind(VO_45, VO_45_temp)
      VO_85 <- cbind(VO_85, VO_85_temp)
    }
  }

  dimnames(VO_H)[[2]] <- paste(vo_pick[v], "H", sprintf("%02d", 1:12), sep = "_")
  dimnames(VO_45)[[2]] <- paste(vo_pick[v], "45", sprintf("%02d", 1:12), sep = "_")
  dimnames(VO_85)[[2]] <- paste(vo_pick[v], "85", sprintf("%02d", 1:12), sep = "_")


  # Recombine information as spatial data frame in CRS 3005
  VO_pts <- data.frame(x = SSClon, y = SSClat, VO_H, VO_45, VO_85) %>%
    st_as_sf(coords = c("x", "y"),
      crs = "EPSG:4326") %>%
    st_transform(crs = "EPSG:3005")

  # Export points as shp file
  sf::st_write(VO_pts, file.path(paths$climate, "SalishSeaCast_MonthlyData", paste0("SSCmonthly_", vodf$abbr[v], ".gdb")), driver = "OpenFileGDB", append = FALSE)
  # sf::st_write(SSC_SST, file.path(paths$climate, "SalishSeaCast_MonthlyData","SSCmonthly_SST.shp"))

}
## Remove extra objects
rm(fillvalue, lat, lon, SSC_45, SSC_85, SSC_H, VO_45, VO_85, VO_H, VO_pts, vodf,
  i, SSClat, SSClon, T45, T85, TH,  v, VO_45_temp, VO_85_temp, VO_H_temp, vo_pick)

# ----------- Format SSC data ---------------
SSC_SST <- read_sf(file.path(paths$climate, "SalishSeaCast_MonthlyData", "SSCmonthly_SST.gdb"))
SSC_SSS <- read_sf(file.path(paths$climate, "SalishSeaCast_MonthlyData", "SSCmonthly_SSS.gdb"))

# rename columns so they work with subsequent scripts
colnames(SSC_SST) <- c("SST_H_01", "SST_H_02", "SST_H_03", "SST_H_04", "SST_H_05", "SST_H_06",
  "SST_H_07", "SST_H_08", "SST_H_09", "SST_H_10", "SST_H_11", "SST_H_12",
  "SST_45_01", "SST_45_02", "SST_45_03", "SST_45_04", "SST_45_05", "SST_45_06",
  "SST_45_07", "SST_45_08", "SST_45_09", "SST_45_10", "SST_45_11", "SST_45_12",
  "SST_85_01", "SST_85_02", "SST_85_03", "SST_85_04", "SST_85_05", "SST_85_06",
  "SST_85_07", "SST_85_08", "SST_85_09", "SST_85_10", "SST_85_11", "SST_85_12", "geometry")
st_geometry(SSC_SST) <- "geometry"  # Format the geometry column

colnames(SSC_SSS) <- c("SSS_H_01", "SSS_H_02", "SSS_H_03", "SSS_H_04", "SSS_H_05", "SSS_H_06",
  "SSS_H_07", "SSS_H_08", "SSS_H_09", "SSS_H_10", "SSS_H_11", "SSS_H_12",
  "SSS_45_01", "SSS_45_02", "SSS_45_03", "SSS_45_04", "SSS_45_05", "SSS_45_06",
  "SSS_45_07", "SSS_45_08", "SSS_45_09", "SSS_45_10", "SSS_45_11", "SSS_45_12",
  "SSS_85_01", "SSS_85_02", "SSS_85_03", "SSS_85_04", "SSS_85_05", "SSS_85_06",
  "SSS_85_07", "SSS_85_08", "SSS_85_09", "SSS_85_10", "SSS_85_11", "SSS_85_12", "geometry")
st_geometry(SSC_SSS) <- "geometry"

# Replace 0s with Nulls in SSC data (all 0s fall on land) And resave
SSC_SST <- SSC_SST %>% mutate_if(is.numeric, ~ na_if(., 0)) %>%
  sf::st_write(file.path(paths$climate, "SalishSeaCast_MonthlyData", "SSCmonthly_SST.gdb"), driver = "OpenFileGDB",  append = FALSE)
SSC_SSS <- SSC_SSS %>% mutate_if(is.numeric, ~ na_if(., 0)) %>%
  sf::st_write(file.path(paths$climate, "SalishSeaCast_MonthlyData",  "SSCmonthly_SSS.gdb"), driver = "OpenFileGDB", append = FALSE)

# ==================== 4. BCCM Data Import ====================
BCCM_85_SST <- read_ncdf(file.path(paths$climate, "BCCM", "bcc42_bioNew_can85_2046to2065_monSST.nc"), proxy = FALSE, var = c("lat_rho", "lon_rho", "temp", "months"), make_time = TRUE)
BCCM_45_SST <- read_ncdf(file.path(paths$climate, "BCCM", "bcc42_bioNew_can45_2046to2065_monSST.nc"), proxy = FALSE, var = c("lat_rho", "lon_rho", "temp", "months"), make_time = TRUE)
BCCM_Hist_SST <- read_ncdf(file.path(paths$climate, "BCCM", "bcc42_bioNew_his_1986to2005_monSST.nc"), proxy = FALSE, var = c("lat_rho", "lon_rho", "temp", "months"),  make_time = TRUE)
BCCM_85_SSS <- read_ncdf(file.path(paths$climate, "BCCM", "bcc42_bioNew_can85_2046to2065_monSSS.nc"), proxy = FALSE, var = c("lat_rho", "lon_rho", "salt", "months"), make_time = TRUE)
BCCM_45_SSS <- read_ncdf(file.path(paths$climate, "BCCM", "bcc42_bioNew_can45_2046to2065_monSSS.nc"), proxy = FALSE, var = c("lat_rho", "lon_rho", "salt", "months"),  make_time = TRUE)
BCCM_Hist_SSS <- read_ncdf(file.path(paths$climate, "BCCM", "bcc42_bioNew_his_1986to2005_monSSS.nc"), proxy = FALSE, var = c("lat_rho", "lon_rho", "salt", "months"),  make_time = TRUE)
BCCM_85_SSPH <- read_ncdf(file.path(paths$climate, "BCCM", "bcc42_bioNew_can85_2046to2065_monSSPH.nc"), proxy = FALSE, var = c("lat_rho", "lon_rho", "pH", "months"), make_time = TRUE)
BCCM_45_SSPH <- read_ncdf(file.path(paths$climate, "BCCM", "bcc42_bioNew_can45_2046to2065_monSSPH.nc"), proxy = FALSE, var = c("lat_rho", "lon_rho", "pH", "months"),  make_time = TRUE)
BCCM_Hist_SSPH <- read_ncdf(file.path(paths$climate, "BCCM", "bcc42_bioNew_his_1986to2005_monSSPH.nc"), proxy = FALSE, var = c("lat_rho", "lon_rho", "pH", "months"),  make_time = TRUE)

# print(BCCM_45_SST) # if the offset is 0.5 in both x and y, it means the points are centroids of cells
# if the x and y delta values are positive it means the 1, 1 cell is in the bottom left, and both variables increase with the axes

# Extract each variable as a vector
BCCM_lon <- as.vector(BCCM_45_SST$lon_rho)
BCCM_lat <- as.vector(BCCM_45_SST$lat_rho)

SST_85_01    <- as.vector(BCCM_85_SST$temp[, , 1])
SST_85_02    <- as.vector(BCCM_85_SST$temp[, , 2])
SST_85_03    <- as.vector(BCCM_85_SST$temp[, , 3])
SST_85_04    <- as.vector(BCCM_85_SST$temp[, , 4])
SST_85_05    <- as.vector(BCCM_85_SST$temp[, , 5])
SST_85_06    <- as.vector(BCCM_85_SST$temp[, , 6])
SST_85_07    <- as.vector(BCCM_85_SST$temp[, , 7])
SST_85_08    <- as.vector(BCCM_85_SST$temp[, , 8])
SST_85_09    <- as.vector(BCCM_85_SST$temp[, , 9])
SST_85_10    <- as.vector(BCCM_85_SST$temp[, , 10])
SST_85_11    <- as.vector(BCCM_85_SST$temp[, , 11])
SST_85_12    <- as.vector(BCCM_85_SST$temp[, , 12])

SST_45_01    <- as.vector(BCCM_45_SST$temp[, , 1])
SST_45_02    <- as.vector(BCCM_45_SST$temp[, , 2])
SST_45_03    <- as.vector(BCCM_45_SST$temp[, , 3])
SST_45_04    <- as.vector(BCCM_45_SST$temp[, , 4])
SST_45_05    <- as.vector(BCCM_45_SST$temp[, , 5])
SST_45_06    <- as.vector(BCCM_45_SST$temp[, , 6])
SST_45_07    <- as.vector(BCCM_45_SST$temp[, , 7])
SST_45_08    <- as.vector(BCCM_45_SST$temp[, , 8])
SST_45_09    <- as.vector(BCCM_45_SST$temp[, , 9])
SST_45_10    <- as.vector(BCCM_45_SST$temp[, , 10])
SST_45_11    <- as.vector(BCCM_45_SST$temp[, , 11])
SST_45_12    <- as.vector(BCCM_45_SST$temp[, , 12])

SST_H_01    <- as.vector(BCCM_Hist_SST$temp[, , 1])
SST_H_02    <- as.vector(BCCM_Hist_SST$temp[, , 2])
SST_H_03    <- as.vector(BCCM_Hist_SST$temp[, , 3])
SST_H_04    <- as.vector(BCCM_Hist_SST$temp[, , 4])
SST_H_05    <- as.vector(BCCM_Hist_SST$temp[, , 5])
SST_H_06    <- as.vector(BCCM_Hist_SST$temp[, , 6])
SST_H_07    <- as.vector(BCCM_Hist_SST$temp[, , 7])
SST_H_08    <- as.vector(BCCM_Hist_SST$temp[, , 8])
SST_H_09    <- as.vector(BCCM_Hist_SST$temp[, , 9])
SST_H_10    <- as.vector(BCCM_Hist_SST$temp[, , 10])
SST_H_11    <- as.vector(BCCM_Hist_SST$temp[, , 11])
SST_H_12    <- as.vector(BCCM_Hist_SST$temp[, , 12])

SSS_85_01    <- as.vector(BCCM_85_SSS$salt[, , 1])
SSS_85_02    <- as.vector(BCCM_85_SSS$salt[, , 2])
SSS_85_03    <- as.vector(BCCM_85_SSS$salt[, , 3])
SSS_85_04    <- as.vector(BCCM_85_SSS$salt[, , 4])
SSS_85_05    <- as.vector(BCCM_85_SSS$salt[, , 5])
SSS_85_06    <- as.vector(BCCM_85_SSS$salt[, , 6])
SSS_85_07    <- as.vector(BCCM_85_SSS$salt[, , 7])
SSS_85_08    <- as.vector(BCCM_85_SSS$salt[, , 8])
SSS_85_09    <- as.vector(BCCM_85_SSS$salt[, , 9])
SSS_85_10    <- as.vector(BCCM_85_SSS$salt[, , 10])
SSS_85_11    <- as.vector(BCCM_85_SSS$salt[, , 11])
SSS_85_12    <- as.vector(BCCM_85_SSS$salt[, , 12])

SSS_45_01    <- as.vector(BCCM_45_SSS$salt[, , 1])
SSS_45_02    <- as.vector(BCCM_45_SSS$salt[, , 2])
SSS_45_03    <- as.vector(BCCM_45_SSS$salt[, , 3])
SSS_45_04    <- as.vector(BCCM_45_SSS$salt[, , 4])
SSS_45_05    <- as.vector(BCCM_45_SSS$salt[, , 5])
SSS_45_06    <- as.vector(BCCM_45_SSS$salt[, , 6])
SSS_45_07    <- as.vector(BCCM_45_SSS$salt[, , 7])
SSS_45_08    <- as.vector(BCCM_45_SSS$salt[, , 8])
SSS_45_09    <- as.vector(BCCM_45_SSS$salt[, , 9])
SSS_45_10    <- as.vector(BCCM_45_SSS$salt[, , 10])
SSS_45_11    <- as.vector(BCCM_45_SSS$salt[, , 11])
SSS_45_12    <- as.vector(BCCM_45_SSS$salt[, , 12])

SSS_H_01    <- as.vector(BCCM_Hist_SSS$salt[, , 1])
SSS_H_02    <- as.vector(BCCM_Hist_SSS$salt[, , 2])
SSS_H_03    <- as.vector(BCCM_Hist_SSS$salt[, , 3])
SSS_H_04    <- as.vector(BCCM_Hist_SSS$salt[, , 4])
SSS_H_05    <- as.vector(BCCM_Hist_SSS$salt[, , 5])
SSS_H_06    <- as.vector(BCCM_Hist_SSS$salt[, , 6])
SSS_H_07    <- as.vector(BCCM_Hist_SSS$salt[, , 7])
SSS_H_08    <- as.vector(BCCM_Hist_SSS$salt[, , 8])
SSS_H_09    <- as.vector(BCCM_Hist_SSS$salt[, , 9])
SSS_H_10    <- as.vector(BCCM_Hist_SSS$salt[, , 10])
SSS_H_11    <- as.vector(BCCM_Hist_SSS$salt[, , 11])
SSS_H_12    <- as.vector(BCCM_Hist_SSS$salt[, , 12])

SSPH_85_01    <- as.vector(BCCM_85_SSPH$pH[, , 1])
SSPH_85_02    <- as.vector(BCCM_85_SSPH$pH[, , 2])
SSPH_85_03    <- as.vector(BCCM_85_SSPH$pH[, , 3])
SSPH_85_04    <- as.vector(BCCM_85_SSPH$pH[, , 4])
SSPH_85_05    <- as.vector(BCCM_85_SSPH$pH[, , 5])
SSPH_85_06    <- as.vector(BCCM_85_SSPH$pH[, , 6])
SSPH_85_07    <- as.vector(BCCM_85_SSPH$pH[, , 7])
SSPH_85_08    <- as.vector(BCCM_85_SSPH$pH[, , 8])
SSPH_85_09    <- as.vector(BCCM_85_SSPH$pH[, , 9])
SSPH_85_10    <- as.vector(BCCM_85_SSPH$pH[, , 10])
SSPH_85_11    <- as.vector(BCCM_85_SSPH$pH[, , 11])
SSPH_85_12    <- as.vector(BCCM_85_SSPH$pH[, , 12])

SSPH_45_01    <- as.vector(BCCM_45_SSPH$pH[, , 1])
SSPH_45_02    <- as.vector(BCCM_45_SSPH$pH[, , 2])
SSPH_45_03    <- as.vector(BCCM_45_SSPH$pH[, , 3])
SSPH_45_04    <- as.vector(BCCM_45_SSPH$pH[, , 4])
SSPH_45_05    <- as.vector(BCCM_45_SSPH$pH[, , 5])
SSPH_45_06    <- as.vector(BCCM_45_SSPH$pH[, , 6])
SSPH_45_07    <- as.vector(BCCM_45_SSPH$pH[, , 7])
SSPH_45_08    <- as.vector(BCCM_45_SSPH$pH[, , 8])
SSPH_45_09    <- as.vector(BCCM_45_SSPH$pH[, , 9])
SSPH_45_10    <- as.vector(BCCM_45_SSPH$pH[, , 10])
SSPH_45_11    <- as.vector(BCCM_45_SSPH$pH[, , 11])
SSPH_45_12    <- as.vector(BCCM_45_SSPH$pH[, , 12])

SSPH_H_01    <- as.vector(BCCM_Hist_SSPH$pH[, , 1])
SSPH_H_02    <- as.vector(BCCM_Hist_SSPH$pH[, , 2])
SSPH_H_03    <- as.vector(BCCM_Hist_SSPH$pH[, , 3])
SSPH_H_04    <- as.vector(BCCM_Hist_SSPH$pH[, , 4])
SSPH_H_05    <- as.vector(BCCM_Hist_SSPH$pH[, , 5])
SSPH_H_06    <- as.vector(BCCM_Hist_SSPH$pH[, , 6])
SSPH_H_07    <- as.vector(BCCM_Hist_SSPH$pH[, , 7])
SSPH_H_08    <- as.vector(BCCM_Hist_SSPH$pH[, , 8])
SSPH_H_09    <- as.vector(BCCM_Hist_SSPH$pH[, , 9])
SSPH_H_10    <- as.vector(BCCM_Hist_SSPH$pH[, , 10])
SSPH_H_11    <- as.vector(BCCM_Hist_SSPH$pH[, , 11])
SSPH_H_12    <- as.vector(BCCM_Hist_SSPH$pH[, , 12])

# Recombine variables into a data frame, reproject into 3005 (BC albers), and save as file GDB
BCCM_SSS <- data.frame(x = BCCM_lon, y = BCCM_lat,
  SSS_H_01, SSS_H_02, SSS_H_03, SSS_H_04, SSS_H_05, SSS_H_06,
  SSS_H_07, SSS_H_08, SSS_H_09, SSS_H_10, SSS_H_11, SSS_H_12,
  SSS_45_01, SSS_45_02, SSS_45_03, SSS_45_04, SSS_45_05, SSS_45_06,
  SSS_45_07, SSS_45_08, SSS_45_09, SSS_45_10, SSS_45_11, SSS_45_12,
  SSS_85_01, SSS_85_02, SSS_85_03, SSS_85_04, SSS_85_05, SSS_85_06,
  SSS_85_07, SSS_85_08, SSS_85_09, SSS_85_10, SSS_85_11, SSS_85_12) %>%
  st_as_sf(coords = c("x", "y"),
    crs = "EPSG:4326") %>%
  st_transform(crs = "EPSG:3005") %>%
  sf::st_write(file.path(paths$climate, "BCCM", "BCCMmonthly_SSS.gdb"), driver = "OpenFileGDB", append = FALSE)

BCCM_SST <- data.frame(x = BCCM_lon, y = BCCM_lat,
  SST_H_01, SST_H_02, SST_H_03, SST_H_04, SST_H_05, SST_H_06,
  SST_H_07, SST_H_08, SST_H_09, SST_H_10, SST_H_11, SST_H_12,
  SST_45_01, SST_45_02, SST_45_03, SST_45_04, SST_45_05, SST_45_06,
  SST_45_07, SST_45_08, SST_45_09, SST_45_10, SST_45_11, SST_45_12,
  SST_85_01, SST_85_02, SST_85_03, SST_85_04, SST_85_05, SST_85_06,
  SST_85_07, SST_85_08, SST_85_09, SST_85_10, SST_85_11, SST_85_12) %>%
  st_as_sf(coords = c("x", "y"),
    crs = "EPSG:4326") %>%
  st_transform(crs = "EPSG:3005") %>%
  sf::st_write(file.path(paths$climate, "BCCM", "BCCMmonthly_SST.gdb"), driver = "OpenFileGDB", append = FALSE)

BCCM_SSPH <- data.frame(x = BCCM_lon, y = BCCM_lat,
  SSPH_H_01, SSPH_H_02, SSPH_H_03, SSPH_H_04, SSPH_H_05, SSPH_H_06,
  SSPH_H_07, SSPH_H_08, SSPH_H_09, SSPH_H_10, SSPH_H_11, SSPH_H_12,
  SSPH_45_01, SSPH_45_02, SSPH_45_03, SSPH_45_04, SSPH_45_05, SSPH_45_06,
  SSPH_45_07, SSPH_45_08, SSPH_45_09, SSPH_45_10, SSPH_45_11, SSPH_45_12,
  SSPH_85_01, SSPH_85_02, SSPH_85_03, SSPH_85_04, SSPH_85_05, SSPH_85_06,
  SSPH_85_07, SSPH_85_08, SSPH_85_09, SSPH_85_10, SSPH_85_11, SSPH_85_12) %>%
  st_as_sf(coords = c("x", "y"),
    crs = "EPSG:4326") %>%
  st_transform(crs = "EPSG:3005") %>%
  sf::st_write(file.path(paths$climate, "BCCM", "BCCMmonthly_SSPH.gdb"), driver = "OpenFileGDB", append = FALSE)

# remove the extra vectors
rm(BCCM_45_SSS, BCCM_45_SST, BCCM_45_SSPH, BCCM_85_SSS, BCCM_85_SST, BCCM_85_SSPH, BCCM_Hist_SSS, BCCM_Hist_SST, BCCM_Hist_SSPH,
  BCCM_lat, BCCM_lon,
  SST_H_01, SST_H_02, SST_H_03, SST_H_04, SST_H_05, SST_H_06,
  SST_H_07, SST_H_08, SST_H_09, SST_H_10, SST_H_11, SST_H_12,
  SST_45_01, SST_45_02, SST_45_03, SST_45_04, SST_45_05, SST_45_06,
  SST_45_07, SST_45_08, SST_45_09, SST_45_10, SST_45_11, SST_45_12,
  SST_85_01, SST_85_02, SST_85_03, SST_85_04, SST_85_05, SST_85_06,
  SST_85_07, SST_85_08, SST_85_09, SST_85_10, SST_85_11, SST_85_12,
  SSS_H_01, SSS_H_02, SSS_H_03, SSS_H_04, SSS_H_05, SSS_H_06,
  SSS_H_07, SSS_H_08, SSS_H_09, SSS_H_10, SSS_H_11, SSS_H_12,
  SSS_45_01, SSS_45_02, SSS_45_03, SSS_45_04, SSS_45_05, SSS_45_06,
  SSS_45_07, SSS_45_08, SSS_45_09, SSS_45_10, SSS_45_11, SSS_45_12,
  SSS_85_01, SSS_85_02, SSS_85_03, SSS_85_04, SSS_85_05, SSS_85_06,
  SSS_85_07, SSS_85_08, SSS_85_09, SSS_85_10, SSS_85_11, SSS_85_12,
  SSPH_H_01, SSPH_H_02, SSPH_H_03, SSPH_H_04, SSPH_H_05, SSPH_H_06,
  SSPH_H_07, SSPH_H_08, SSPH_H_09, SSPH_H_10, SSPH_H_11, SSPH_H_12,
  SSPH_45_01, SSPH_45_02, SSPH_45_03, SSPH_45_04, SSPH_45_05, SSPH_45_06,
  SSPH_45_07, SSPH_45_08, SSPH_45_09, SSPH_45_10, SSPH_45_11, SSPH_45_12,
  SSPH_85_01, SSPH_85_02, SSPH_85_03, SSPH_85_04, SSPH_85_05, SSPH_85_06,
  SSPH_85_07, SSPH_85_08, SSPH_85_09, SSPH_85_10, SSPH_85_11, SSPH_85_12)

# ==================== 5. Cumulative Impacts Centroids ====================
CI <- read_sf(file.path(paths$spatial, "Cumulative_Impacts_Pacfic_Canada.gdb"))
CI <- st_transform(CI, crs = "EPSG:3005")
# calculate centroids of CI polygon grid
CImpact_points <- st_centroid(CI)
rm(CI)
sf::st_write(CImpact_points, file.path(paths$spatial, "CumulativeImpacts", "CI_points.gdb"), driver = "OpenFileGDB", append = FALSE)





# ==================== 6. Masking & Marine Adaptive Zones Intersect ====================
# Load and transform masking polygons and MAZ polygons
BCCM_mask <- read_sf(file.path(paths$climate, "BCCM", "BCCM_mask.shp")) %>% st_transform(crs = "EPSG:3005")
SSC_mask <- read_sf(file.path(paths$climate, "SalishSeaCast_MonthlyData", "SSC_mask.shp")) %>% st_transform(crs = "EPSG:3005")
NEP_mask <- read_sf(file.path(paths$climate, "NEP36_MonthlyData", "NEP_mask.shp")) %>% st_transform(crs = "EPSG:3005")
MAZ <- read_sf(file.path(paths$spatial, "MAZ", "MAZ_Final.shp")) # %>% st_transform(crs = "EPSG:3005")

# Change MAZ acronym variable to a factor variable
MAZ$MAZ_Acrony <- as.factor(MAZ$MAZ_Acrony)

# Crop model points to mask polygons and Join to MAZ polygons and save as a file GDB
BCCM_SST_sub <- st_intersection(BCCM_SST, BCCM_mask) %>%
  st_join(left = FALSE, MAZ["MAZ_Acrony"])
BCCM_SST_sub$MAZ_Acrony <- BCCM_SST_sub$MAZ_Acrony.y
BCCM_SST_sub <- subset(BCCM_SST_sub, select = -c(NAME_E, MAZ_Acrony.x, MAZ_Acrony.y, CI_AvgScor, Long_name)) %>%
  sf::st_write(file.path(paths$climate, "Standardized_Marine_data/Points/BCCM_SST_sub.gdb"),  driver = "OpenFileGDB",  append = FALSE)

BCCM_SSS_sub <- st_intersection(BCCM_SSS, BCCM_mask) %>%
  st_join(left = FALSE, MAZ["MAZ_Acrony"])
BCCM_SSS_sub$MAZ_Acrony <- BCCM_SSS_sub$MAZ_Acrony.y
BCCM_SSS_sub <- subset(BCCM_SSS_sub, select = -c(NAME_E, MAZ_Acrony.x, MAZ_Acrony.y, CI_AvgScor, Long_name)) %>%
  sf::st_write(file.path(paths$climate, "Standardized_Marine_data/Points/BCCM_SSS_sub.gdb"),   driver = "OpenFileGDB", append = FALSE)

BCCM_SSPH_sub <- st_intersection(BCCM_SSPH, BCCM_mask) %>%
  st_join(left = FALSE, MAZ["MAZ_Acrony"]) %>%
  sf::st_write(file.path(paths$climate, "Standardized_Marine_data/Points/BCCM_SSPH_sub.gdb"),   driver = "OpenFileGDB", append = FALSE)

NEP_SST_sub <- st_intersection(NEP_SST, NEP_mask) %>%
  st_join(left = FALSE, MAZ["MAZ_Acrony"]) %>%
  sf::st_write(file.path(paths$climate, "Standardized_Marine_data/Points/NEP_SST_sub.gdb"),   driver = "OpenFileGDB", append = FALSE)

NEP_SSS_sub <- st_intersection(NEP_SSS, NEP_mask) %>%
  st_join(left = FALSE, MAZ["MAZ_Acrony"]) %>%
  sf::st_write(file.path(paths$climate, "Standardized_Marine_data/Points/NEP_SSS_sub.gdb"),   driver = "OpenFileGDB", append = FALSE)

NEP_SSPH_sub <- st_intersection(NEP_SSPH, NEP_mask) %>%
  st_join(left = FALSE, MAZ["MAZ_Acrony"]) %>%
  sf::st_write(file.path(paths$climate, "Standardized_Marine_data/Points/NEP_SSPH_sub.gdb"),   driver = "OpenFileGDB", append = FALSE)

SSC_SST_sub <- st_intersection(SSC_SST, SSC_mask) %>%
  st_join(left = FALSE, MAZ["MAZ_Acrony"]) %>%
  sf::st_write(file.path(paths$climate, "Standardized_Marine_data/Points/SSC_SST_sub.gdb"),   driver = "OpenFileGDB", append = FALSE)

SSC_SSS_sub <- st_intersection(SSC_SSS, SSC_mask) %>%
  st_join(left = FALSE, MAZ["MAZ_Acrony"]) %>%
  sf::st_write(file.path(paths$climate, "Standardized_Marine_data/Points/SSC_SSS_sub.gdb"),   driver = "OpenFileGDB", append = FALSE)

CI_points_sub <- st_join(CI_points, left = FALSE, MAZ["MAZ_Acrony"]) %>%
  sf::st_write(file.path(paths$climate, "Standardized_Marine_data/Points/CI_points_sub.gdb"),   driver = "OpenFileGDB", append = FALSE)




# ==================== 7. HOTSSea Model Processing ====================

# use PacEA package to load
# most processing is already done but need to join to MAZ and summarize across years


# download hotssea data  - use hotssea_all_variables()
hotssea_SST <- hotssea_surface_temperature_mean() %>%
  pivot_longer(cols = c(`1980_1`:`2018_12`),
    names_to = c("year", "month"),
    names_sep = "_",
    values_to = "sst") %>%
  filter(year >= h_start & year <= h_end) %>%
  mutate(month = sprintf("%02d", as.numeric(month))) %>%
  pivot_wider(id_cols = c(geometry, year),
    names_from = month,
    names_prefix = "SST_",
    values_from = sst)

hotssea_SST_join <- st_join(x = hotssea_SST, y = MAZ, left = FALSE)

sf::st_write(hotssea_SST_join, file.path(paths$climate, "Standardized_Marine_data", "Points", "HOTSSea_SST.gdb"),
  driver = "OpenFileGDB", append = FALSE)


# ==================== 8. CMIP6 High-Resolution SST Processing ====================

period_CMIP6 <- filter(period_lookup, model == "CMIP6_SST")

# SSP 245 -get historical data and convert to sf
CMIP_SST <- ncdf_to_dt(file_name = "ensemble-percentiles_mon_QDM+OSTIA_historical+ssp245_1950-2100_BC.nc",
  file_path = file.path(paths$climate, "Marine_CMIP6"),
  MAZ_obj = MAZ,
  start_year = h_start,
  end_year = h_end)
sf::st_write(CMIP_SST, file.path(paths$climate, "Standardized_Marine_Data", "Points",
  paste0("CMIP6_ssp245_SST_", h_start, "-", h_end, ".gdb")),
driver = "OpenFileGDB", append = FALSE)

# repeat for ssp245 P3
CMIP_SST <- ncdf_to_dt(file_name = "ensemble-percentiles_mon_QDM+OSTIA_historical+ssp245_1950-2100_BC.nc",
  file_path = file.path(paths$climate, "Marine_CMIP6"),
  MAZ_obj = MAZ,
  start_year = p3_start,
  end_year = p3_end)

sf::st_write(CMIP_SST, file.path(paths$climate, "Standardized_Marine_Data", "Points",
  paste0("CMIP6_ssp245_SST_", p3_start, "-", p3_end, ".gdb")),
driver = "OpenFileGDB", append = FALSE)

# repeat for ssp245 P5
CMIP_SST <- ncdf_to_dt(file_name = "ensemble-percentiles_mon_QDM+OSTIA_historical+ssp245_1950-2100_BC.nc",
  file_path = file.path(paths$climate, "Marine_CMIP6"),
  MAZ_obj = MAZ,
  start_year = p5_start,
  end_year = p5_end)

sf::st_write(CMIP_SST, file.path(paths$climate, "Standardized_Marine_Data", "Points",
  paste0("CMIP6_ssp245_SST_", p5_start, "-", p5_end, ".gdb")),
driver = "OpenFileGDB", append = FALSE)

# SSP 585 -get historical data and convert to sf
CMIP_SST <- ncdf_to_dt(file_name = "ensemble-percentiles_mon_QDM+OSTIA_historical+ssp585_1950-2100_BC.nc",
  file_path = file.path(paths$climate, "Marine_CMIP6"),
  MAZ_obj = MAZ,
  start_year = h_start,
  end_year = h_end)
sf::st_write(CMIP_SST, file.path(paths$climate, "Standardized_Marine_Data", "Points",
  paste0("CMIP6_ssp585_SST_", h_start, "-", h_end, ".gdb")),
driver = "OpenFileGDB", append = FALSE)

# repeat for ssp585 P3
CMIP_SST <- ncdf_to_dt(file_name = "ensemble-percentiles_mon_QDM+OSTIA_historical+ssp585_1950-2100_BC.nc",
  file_path = file.path(paths$climate, "Marine_CMIP6"),
  MAZ_obj = MAZ,
  start_year = p3_start,
  end_year = p3_end)

sf::st_write(CMIP_SST, file.path(paths$climate, "Standardized_Marine_Data", "Points",
  paste0("CMIP6_ssp585_SST_", p3_start, "-", p3_end, ".gdb")),
driver = "OpenFileGDB", append = FALSE)

# repeat for ssp585 P5
CMIP_SST <- ncdf_to_dt(file_name = "ensemble-percentiles_mon_QDM+OSTIA_historical+ssp585_1950-2100_BC.nc",
  file_path = file.path(paths$climate, "Marine_CMIP6"),
  MAZ_obj = MAZ,
  start_year = p5_start,
  end_year = p5_end)

sf::st_write(CMIP_SST, file.path(paths$climate, "Standardized_Marine_Data", "Points",
  paste0("CMIP6_ssp585_SST_", p5_start, "-", p5_end, ".gdb")),
driver = "OpenFileGDB", append = FALSE)
