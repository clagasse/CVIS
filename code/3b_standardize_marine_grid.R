# ----------3b_standardize_marine_grid.R----------

# Inputs (with R code names): 
#   3a1/Surface data: Spatial data frames of each models monthly surface estimates of each variable with centroid points (e.g. NEP_SST); 
# 3a2/CI points: CI centroid points ("CI_points")
# Masks: Polygon Mask shape files (e.g. "NEP_mask")
# MAZ: Polygons of marine adaptive zones ("MAZ")
# Input locations (with file names): 
#   3a1/Surface data: Saved as .gdb to model specific folders in 0_data_climate (e.g. "NEP36_MonthlyData.gdb") 
# 3a2/CI points: "\OneDrive - DFO-MPO\0_data_spatial\CumulativeImpacts\CI_points.gdb"
# Masks: located in model specific folders in 0_data_climate (e.g. "NEP_mask.shp") and the EEZ layer is located in 0_data_spatial/BC_EEZ/BC_EEZ.shp
# MAZ: OneDrive - DFO-MPO\0_data_spatial\MAZ\MAZ_Final.shp"
# Outputs: 
# 	3b1/Standardized Surface data: Standarized vector grids with model surface data and CI data, cropped to the extent of each model, and contain a column referencing the MAZ each cell overlaps with (e.g. "NEP_SST_cropped")
# Output locations: 
# 	3b1/Standardized Surface data: Saved to "\OneDrive - DFO-MPO\0_data_climate\Standardized_Marine_data" (e.g. "NEP_SST_cropped")
#
# This code takes the spatial data frames with all model points created in the 
# first part of code (e.g. BCCM_SST), interpolates the points to a standard grid, 
# crop the grids to the extent of the model using masks, and join the resulting 
# grids to MAZ polygons. 
# The 3b code follows these steps: 
# 1) Load the surface data (3a1), CI points(3a2), and masks/extent polygons   
# 2) Load an interpolation fuction ("point2rast") and associated function ("nnfit") from PACEA
# 3) Interpolate the surface data using the nearest neighbour interpolation "point2rast" function. 
#    Specify parameters so that a maximum of 4 neighbours are considered, cell size is equal 
#    to that of the original netCDF file, and the extent is similar to that of the original NetCDF points.  
#        Resolution of original models:
#          BCCM:  3km 	
#         NEP36: 3km
#          SSC:   0.5km
#           CI:    1km
# 4) Resample the interpolated surfaces to a standarized grid based on the BCCM model.The BCCM model 
#    was used as the standard as it has the broadest resolution (3km). The nearest neighbour method 
#    was used in the resample function because we wanted local values to have the greatest influence. 
# 5) The resampled surface data are masked with model-specific masks and converted into a spatial feature object. These objects are named with and ending of "_cropped" (e.g. "NEP_SST_cropped"). 
# 6) The "_cropped" spatial feature objects are joined to MAZ polygons so they gain a new column called "MAZ_Acrony" containing the MAZ acronmym that each grid cell overlaps. 
# 7) Save the "_cropped" data to "\OneDrive - DFO-MPO\0_data_climate\Standardized_Marine_data" (e.g. "NEP_SST_cropped.gdb")



#working with NetCDFs
library(ncdf4)
library(ncmeta)
library(abind)
library(concaveman)

#other 
#install_github("pbs-assess/pacea")
library(pacea)

library(here)
here()
source(here("code","0_setup.R"))
source(file.path(code_root, "3_marine_utils.R"))


# remove spherical geometry (s2) for sf operations
sf_use_s2(FALSE)  

# Load HOTSSea historical outputs
hotssea_SST <-read_sf( file.path(paths$climate, "Standardized_Marine_data", "Points", "HOTSSea_SST.gdb"))  %>% 
  rename(geometry = SHAPE) 
st_geometry(hotssea_SST) <- "geometry"   # format spatial data column so it works with sequential functions

#reshape to consistent format with other files
hotssea_SST <- hotssea_SST %>%
  pivot_wider(id_cols = c("geometry"),
              names_from = c("scenario", "month"),
              names_prefix = "SST_",
              values_fn = mean)


# Load climate data that was the result of 3a marine data import data 
BCCM_SST<-read_sf( file.path(paths$climate, "BCCM", "BCCMmonthly_SST.gdb"))  %>% 
  rename(geometry = SHAPE) 
  st_geometry(BCCM_SST) <- "geometry"   # format spatial data column so it works with sequential functions

BCCM_SSS<-read_sf( file.path(paths$climate, "BCCM", "BCCMmonthly_SSS.gdb"))%>% 
  rename(geometry = SHAPE) 
st_geometry(BCCM_SSS) <- "geometry"

BCCM_SSPH<-read_sf( file.path(paths$climate, "BCCM", "BCCMmonthly_SSPH.gdb"))%>% 
  rename(geometry = SHAPE) 
st_geometry(BCCM_SSPH) <- "geometry"

NEP_SST<- read_sf( file.path(paths$climate, "NEP36_MonthlyData","NEPmonthly_SST.gdb"))%>% 
  rename(geometry = SHAPE) 
st_geometry(NEP_SST) <- "geometry"

NEP_SSS<- read_sf( file.path(paths$climate, "NEP36_MonthlyData","NEPmonthly_SSS.gdb"))%>% 
  rename(geometry = SHAPE) 
st_geometry(NEP_SSS) <- "geometry"

NEP_SSPH<- read_sf( file.path(paths$climate, "NEP36_MonthlyData","NEPmonthly_SSPH.gdb"))%>% 
  rename(geometry = SHAPE) 
st_geometry(NEP_SSPH) <- "geometry"

SSC_SST<- read_sf( file.path(paths$climate, "SalishSeaCast_MonthlyData","SSCmonthly_SST.gdb"))%>% 
  rename(geometry = SHAPE) 
st_geometry(SSC_SST) <- "geometry"

SSC_SSS<- read_sf( file.path(paths$climate, "SalishSeaCast_MonthlyData","SSCmonthly_SSS.gdb"))%>% 
  rename(geometry = SHAPE) 
st_geometry(SSC_SSS) <- "geometry"

CI_points<-read_sf(file.path(paths$climate, "Standardized_Marine_data", "Points", "CI_points.gdb"))%>% 
  rename(geometry = SHAPE) 
st_geometry(CI_points) <- "geometry"

#------Import bounding and masking/clipping polygons ----------
# Load polygons shp files that will be used to set the extent of the interpolation or clip the final vector grid. 
EEZ<-read_sf(file.path(paths$spatial, "BC_EEZ", "BC_EEZ.shp"))%>%st_transform(,crs = "EPSG:3005" )                              # Full BC EEZ 
SSCbox<-read_sf(file.path(paths$climate, "SalishSeaCast_MonthlyData", "SSC_boundingbox.shp"))%>%st_transform(,crs = "EPSG:3005" ) # Box of full SSC extent
BCCM_mask2<-read_sf(file.path(paths$climate, "BCCM", "BCCM_mask2.shp"))  %>%st_transform(,crs = "EPSG:3005" )                       # EEZ excluding north west corner 
SSC_mask<-read_sf(file.path(paths$climate, "SalishSeaCast_MonthlyData", "SSC_mask2.shp"))%>%st_transform(,crs = "EPSG:3005" )      # surrounds the SSC data with values but excludes inlets and USA, and NA values 
NEP_mask<-read_sf(file.path(paths$climate, "NEP36_MonthlyData", "NEP_mask.shp"))     %>%st_transform(,crs = "EPSG:3005" )         # Clips out uncertain results including those on East coast of Haida Gwaii
CI_mask<-read_sf(file.path(  paths$spatial, "CumulativeImpacts", "CI_mask.shp"))  %>%st_transform(,crs = "EPSG:3005" )       # EEZ polygon made from extent of original CI layer
MAZ<-read_sf(file.path(paths$spatial, "MAZ", "MAZ_Final.shp")) %>% st_transform(,crs = "EPSG:3005" )     # MAZ polygons to join to completed grid
  MAZ$MAZ_Acrony<- as.factor(MAZ$MAZ_Acrony)#Change MAZ acronym variable to a factor variable

#----------Interpolation Function ---------
point2rast <- function(data, spatobj, loc = c("x", "y"), cellsize, nnmax = 4, 
                       as = c("SpatRast","SpatVect")) {
  
  requireNamespace("methods", quietly = TRUE)
  requireNamespace("terra", quietly = TRUE)
  requireNamespace("gstat", quietly = TRUE)
  requireNamespace("sf", quietly = TRUE)
  requireNamespace("stats", quietly = TRUE)
  
  stopifnot("must provide cellsize value" = exists("cellsize"))
  stopifnot("must specify valid value for 'as'" = as %in% c("SpatRast", "SpatVect"))
  
  if(!any(sapply(c("sf", "Spatial"), function(cl) methods::is(data, cl)))) {
    
    data <- as.data.frame(data)
    
    if(!length(dim(loc))){
      
      stopifnot("loc vector must of be of length==2 " = length(loc)==2)
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
  
  if(methods::is(data, "Spatial")) {
    coords <- setNames(as.data.frame(data)[,c("coords.x1", "coords.x2")], c("x", "y"))
    tdat <- as.data.frame(data)[, names(data), drop = FALSE]    
  }
  
  if(methods::is(data, "sf")) {
    coords <- setNames(as.data.frame(matrix(unlist(data$geometry), ncol=2, byrow = TRUE)), c("x", "y"))
    tdat <- as.data.frame(data)[, -which(names(data) == "geometry"), drop = FALSE]
  }
  
  tbb <- terra::ext(spatobj)
  if(!any(coords$x >= tbb$xmin & coords$x <= tbb$xmax & 
          coords$y >= tbb$ymin & coords$y <= tbb$ymax)) {
    warning("'loc' coordinates within spatobj extent = 0; check crs or extent of spatobj")
  }
  
  terror <- try(terra::crs(spatobj), silent = TRUE)
  if("try-error" %in% class(terror)) {
    r <- terra::rast(terra::ext(spatobj), res = c(cellsize))
  } else {
    r <- terra::rast(terra::ext(spatobj), res = c(cellsize), crs = terra::crs(spatobj))
  }
  
  nn.pred <- apply(tdat, 2, FUN = nnfit, r=r, loc=loc, coords=coords, nnmax=nnmax)
  xyz <- cbind(as.data.frame(suppressWarnings(terra::crds(r))), nn.pred)
  
  if(as[1]=="SpatRast"){
    spat <- terra::rast(xyz, type="xyz", crs = terra::crs(r))
  } 
  if(as[1]=="SpatVect"){
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
  
  gs <- gstat::gstat(formula = xvar~1, locations = ~x+y, data = xdat, nmax = nnmax, set=list(idp = 0))
  nn <- terra::interpolate(r, gs, debug.level=0)
  return(as.vector(nn$var1.pred))
}

# ---------INTERPOLATE-----------------------------------
# Interpolate climate data using the PACEA nearest neighbour interpolation function, point2rast
# Establish parameters of point2rast function
llnames <- c("x", "y")
nmax <- 4 #I think this is how many points the nearest neighbour function considers, 4 is small, meaning its a local interpolation 

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
 
CI_interpolation<- point2rast(data = CI_points,
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

#---------- Resample, Mask, Join, and Save --------------
BCCM_SST_cropped <-BCCM_SST_interpolation %>%      # BCCM doesn't need to be resampled as we are converting the other grids to its resolution
  mask(BCCM_mask2) %>%                              # Mask the raster. if you end code here it is a spatraster
  stars::st_as_stars() %>%                         # These two lines turn it into raster into a vector grid 
  sf::st_as_sf()  %>%
  st_join(left = FALSE, MAZ["MAZ_Acrony"]) %>%     # Join vector grid to MAZ polygons, adding the column "MAZ_Acrony"
  sf::st_write( file.path(paths$climate, "Standardized_Marine_data", "Grid",           # Save as GDB
                          "BCCM_SST_cropped.gdb" ), driver = "OpenFileGDB", append=FALSE)

BCCM_SSS_cropped <-BCCM_SSS_interpolation %>%
  mask(BCCM_mask2) %>% 
  stars::st_as_stars() %>%  
  sf::st_as_sf()  %>%
  st_join(left = FALSE, MAZ["MAZ_Acrony"])%>%
  sf::st_write( file.path(paths$climate,  "Standardized_Marine_data", "Grid",            
                          "BCCM_SSS_cropped.gdb" ), driver = "OpenFileGDB", append=FALSE)

BCCM_SSPH_cropped <-BCCM_SSPH_interpolation %>%
  mask(BCCM_mask2) %>% 
  stars::st_as_stars() %>%  
  sf::st_as_sf()  %>%
  st_join(left = FALSE, MAZ["MAZ_Acrony"])%>%
  sf::st_write( file.path(paths$climate,  "Standardized_Marine_data", "Grid",           
     "BCCM_SSPH_cropped.gdb" ), driver = "OpenFileGDB", append=FALSE)

NEP_SST_cropped <- NEP_SST_interpolation %>%
  resample(BCCM_SST_interpolation,    # Resample interpolated raster to BCCM Grid standard as it has the largest resolution
           method = "near") %>%       # Use nearest neighbor method because we want the local values to have the largest impact 
  mask(NEP_mask) %>%  
  stars::st_as_stars() %>%  
  sf::st_as_sf()  %>%
  st_join(left = FALSE, MAZ["MAZ_Acrony"])%>%
  sf::st_write( file.path(paths$climate,       "Standardized_Marine_data", "Grid",      
     "NEP_SST_cropped.gdb" ), driver = "OpenFileGDB", append=FALSE)

NEP_SSS_cropped <- NEP_SSS_interpolation %>%
  resample(BCCM_SST_interpolation, method = "near") %>% 
  mask(NEP_mask) %>%
  stars::st_as_stars() %>%  
  sf::st_as_sf() %>%
  st_join(left = FALSE, MAZ["MAZ_Acrony"])%>%
  sf::st_write( file.path(paths$climate,  "Standardized_Marine_data", "Grid",           
     "NEP_SSS_cropped.gdb" ), driver = "OpenFileGDB", append=FALSE)

NEP_SSPH_cropped <- NEP_SSPH_interpolation %>%
  resample(BCCM_SST_interpolation, method = "near") %>% 
  mask(NEP_mask) %>%  
  stars::st_as_stars() %>%  
  sf::st_as_sf()  %>%
  st_join(left = FALSE, MAZ["MAZ_Acrony"]) %>%
  sf::st_write( file.path(paths$climate,    "Standardized_Marine_data", "Grid",         
     "NEP_SSPH_cropped.gdb" ), driver = "OpenFileGDB", append=FALSE)

SSC_SST_cropped <- SSC_SST_interpolation %>%
  resample(BCCM_SST_interpolation, method = "near") %>% 
  mask(SSC_mask) %>%
  stars::st_as_stars() %>%  
  sf::st_as_sf()  %>%
  st_join(left = FALSE, MAZ["MAZ_Acrony"])%>%
  sf::st_write( file.path(paths$climate,     "Standardized_Marine_data", "Grid",        
     "SSC_SST_cropped.gdb" ), driver = "OpenFileGDB", append=FALSE)

SSC_SSS_cropped <- SSC_SSS_interpolation %>%
  resample(BCCM_SST_interpolation, method = "near") %>% 
  mask(SSC_mask) %>%
  stars::st_as_stars() %>%  
  sf::st_as_sf() %>%
  st_join(left = FALSE, MAZ["MAZ_Acrony"])%>%
  sf::st_write( file.path(paths$climate, "Standardized_Marine_data", "Grid",            
     "SSC_SSS_cropped.gdb" ), driver = "OpenFileGDB", append=FALSE)

CI_cropped <- CI_interpolation %>%
  resample(BCCM_SST_interpolation, method = "near") %>% 
  mask(CI_mask) %>%
  stars::st_as_stars() %>%  
  sf::st_as_sf()  %>%
  st_join(left = FALSE, MAZ["MAZ_Acrony"])%>%
  sf::st_write( file.path(paths$climate, "Standardized_Marine_data", "Grid",            
     "CI_cropped.gdb" ), driver = "OpenFileGDB", append=FALSE)

hotssea_cropped <- hotssea_interpolation %>%
  resample(BCCM_SST_interpolation, method = "near") %>%
  mask(SSC_mask) %>%
  stars::st_as_stars() %>%  
  sf::st_as_sf() %>%
  st_join(left = FALSE, MAZ["MAZ_Acrony"]) %>%
  mutate(MAZ_Acrony = as.character(MAZ_Acrony)) %>%
  sf::st_write( file.path(paths$climate, "Standardized_Marine_data", "Grid",            
                          "hotssea_SST_cropped.gdb" ), driver = "OpenFileGDB", append=FALSE)

#------------- Combine objects into a single spatial grid for each attribute

## SST - use hotssea for historic and SSC for projections in GStr, use BCCM everywhere else
SSC_SST_cropped <- st_read(file.path(paths$climate, "Standardized_Marine_data", "Grid", "SSC_SST_cropped.gdb"))
BCCM_SST_cropped <- st_read(file.path(paths$climate, "Standardized_Marine_data","Grid" ,"BCCM_SST_cropped.gdb"))
hotssea_cropped <- st_read(file.path(paths$climate, "Standardized_Marine_data", "Grid", "hotssea_SST_cropped.gdb"))

GSTR_SST_bind <- SSC_SST_cropped %>%
  select(-contains("_H_")) %>%
  cbind(st_drop_geometry(hotssea_cropped)) %>%
  select(-MAZ_Acrony.1)
  #st_join(hotssea_cropped, left = FALSE)

# Fix month names for consistency with BCCM
h_cols_index <- grep("_H_", names(GSTR_SST_bind))
new_names <- sprintf("SST_H_%02d", seq_along(h_cols_index)) 
colnames(GSTR_SST_bind)[h_cols_index] <- new_names

#create grid object of SST for entire coast
SST_grid <- BCCM_SST_cropped %>%
  filter(MAZ_Acrony != "GStr") %>%
  rbind(GSTR_SST_bind)

## SSS - use SSC for historic and projections in GStr, use BCCM everywhere else
SSC_SSS_cropped <- st_read(file.path(paths$climate, "Standardized_Marine_data", "Grid" ,"SSC_SSS_cropped.gdb")) 
BCCM_SSS_cropped <- st_read(file.path(paths$climate, "Standardized_Marine_data","Grid" ,"BCCM_SSS_cropped.gdb"))

SSS_grid <- BCCM_SSS_cropped %>%
  filter(MAZ_Acrony != "GStr") %>%
  rbind(SSC_SSS_cropped)


## write combined objects for SSS and SST
sf::st_write( SST_grid, file.path(paths$climate, "Standardized_Marine_data", "Grid",            
                        "SST_bc_coast.gdb" ), driver = "OpenFileGDB", append=FALSE)

sf::st_write( SSS_grid, file.path(paths$climate, "Standardized_Marine_data", "Grid",            
                                  "SSS_bc_coast.gdb" ), driver = "OpenFileGDB", append=FALSE)



#compare stats on gridded vs points
point_SST <- SSC_SST_long %>%
  as_tibble() %>%
  filter(month %in% c(4,5,6,7), MAZ_Acrony == "GStr") %>%
  group_by(scenario) %>%
  summarize(mean    = mean(value, na.rm = T),
            qlowsp = quantile(value, qlowsp, na.rm = T),
            qhighsp = quantile(value, qhighsp, na.rm = T),
            .groups = "drop")

grid_SST <- SSC_SST_long_grid %>%
  as_tibble() %>%
  filter(month %in% c(4,5,6,7), MAZ_Acrony == "GStr") %>%
  group_by(scenario) %>%
  summarize(mean    = mean(value, na.rm = T),
            qlowsp = quantile(value, qlowsp, na.rm = T),
            qhighsp = quantile(value, qhighsp, na.rm = T),
            .groups = "drop")

