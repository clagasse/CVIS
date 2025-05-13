###############################################################################
# 3_marine_utils.R

#Functions to load and summarize marine data relevant 

###############################################################################



# ---------Installing function point2rast from PACEA ------------------
#- code copied from https://github.com/pbs-assess/pacea/blob/main/R/interpolate.R
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


## function to assign points or polygons based on intersection with MAZ
assign_points <- function(x, y, var = "MAZ_Acrony") {
  t_int <- st_intersects(x, y)
  
  for(i in 1:nrow(x)) {
    t_vec <- y[[var]][t_int[[i]]]
    t_vec <- str_sort(t_vec)
    
    x[[var]][i] <- str_flatten(t_vec, collapse = ",")
  }
  return(x)
  
}


