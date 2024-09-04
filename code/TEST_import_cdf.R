
#code taken from:  
# https://pjbartlein.github.io/REarthSysSci/netCDF.html


# load the `ncdf4` and the `CFtime` packages
library(ncdf4)
library(CFtime)
library(lattice)
library(RColorBrewer)


# set path and filename

#read.csv(here("freshwater", "data", "spatial", "PCIC_indicators", 
#              "twMonth_mClimMean_ensMean_VICGL-dynWat_rcp45_2041-2070_bccoast+fraser.nc"))

ncpath <- here("freshwater", "data", "spatial", "PCIC_indicators")
ncname <- "twMonth_mClimMean_ensMean_VICGL-dynWat_rcp45_2041-2070_bccoast+fraser"  
ncfname <- paste(here(ncpath, ncname), ".nc", sep="")
dname <- "twMonth"  # note: tmp means temperature (not temporary)

# open a netCDF file
ncin <- nc_open(ncfname)
print(ncin)

# get longitude and latitude
lon <- ncvar_get(ncin,"lon")
nlon <- dim(lon)
head(lon)

lat <- ncvar_get(ncin,"lat")
nlat <- dim(lat)
head(lat)

id <- ncin$groups$id
nid <- dim(id)

# get time
time <- ncvar_get(ncin,"time")
tunits <- ncatt_get(ncin,"time","units")

# get temperature
PREC_array <- ncvar_get(ncin,dname)
dlname <- ncatt_get(ncin,dname,"long_name")
dunits <- ncatt_get(ncin,dname,"units")
fillvalue <- ncatt_get(ncin,dname,"_FillValue")
dim(tmp_array)


# replace netCDF fill values with NA's
tmp_array[tmp_array==fillvalue$value] <- NA

head(as.vector(tmp_array[,,1]))
length(na.omit(as.vector(tmp_array[,,1])))


# get a single slice or layer (January)
m <- 1
tmp_slice <- tmp_array[,,m]

# quick map
image(lon,lat,tmp_slice, col=rev(brewer.pal(10,"RdBu")))

# levelplot of the slice
grid <- expand.grid(lon=lon, lat=lat)
cutpts <- c(-50,-40,-30,-20,-10,0,10,20,30,40,50)
levelplot(tmp_slice ~ lon * lat, data=grid, at=cutpts, cuts=11, pretty=T, 
          col.regions=(rev(brewer.pal(10,"RdBu"))))


# create dataframe -- reshape data
# matrix (nlon*nlat rows by 2 cols) of lons and lats
lonlat <- as.matrix(expand.grid(lon,lat))
dim(lonlat)

# vector of `tmp` values
tmp_vec <- as.vector(tmp_slice)
length(tmp_vec)

# create dataframe and add names
tmp_df01 <- data.frame(cbind(lonlat,tmp_vec))
names(tmp_df01) <- c("lon","lat",paste(dname,as.character(m), sep="_"))
head(na.omit(tmp_df01), 10)

# set path and filename
csvpath <- "/freshwater/data/"
csvname <- "PCIC-grid-points_PREC.csv"
csvfile <- paste(csvpath, csvname, sep="")
write.table(na.omit(tmp_df01), csvfile, row.names=FALSE, sep=",")
