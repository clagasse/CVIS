## 2z_FWA_query.R
#Use the fwapgr package to query data from the freshwater atlas of BC

#package info:  https://github.com/poissonconsulting/fwapgr

# install.packages("devtools")
devtools::install_github("poissonconsulting/pgfeatureserv")
devtools::install_github("poissonconsulting/fwapgr")

### this code will query from the FWA database using an API, but is limited to 10,000 records
library(fwapgr)  #package for accessing BC FWA
library(tidyverse)

library(here)
setwd(here())
source(file.path(here(), "code", "0_setup.R"))

#get information about the collections or a collection’s properties:
fwa_cols <- fwa_collections()
fwa_collection_properties("whse_basemapping.fwa_basins_poly")

bc_basins <- fwa_query_collection("whse_basemapping.fwa_basins_poly")

wshed_groups <- fwa_query_collection("whse_basemapping.fwa_watershed_groups_poly")
wshed_groups <- st_transform(wshed_groups, crs = 3005)

collection_id <- "whse_basemapping.fwa_stream_networks_sp"
filter_watersheds <- setNames(as.list(FR_codes), rep("watershed_group_code", length(FR_codes)))

FWA_query <- fwa_query_collection(collection_id, filter = filter_watersheds)


campbell <- wshed_groups[wshed_groups$watershed_group_name == "Campbell River",]

ggplot() +
  geom_sf(data = campbell)





### load Vancouver Island watershed

#read ecological drainage units
EAU <-st_read(file.path(paths$spatial, "EAUBC_ECO_DRAINAGE_UNITS", "EAUBC_ECO_DRAINAGE_UNITS_SP.gpkg"))

EAU_VI <- EAU[EAU$ECO_DRAINAGE_UNIT == "Vancouver Island",]

ws

wshed_VI <- subset_intersections(wshed_groups, EAU_VI)

wshed_VI <- st_intersects(wshed_groups, EAU_VI, sparse = FALSE)

wshed_VI <- wshed_groups[apply(wshed_VI, 1, sum) > 0,]


cu_boundary <- st_read(file.path(paths$spatial, "CU_boundaries", "pse_conservation_units.gdb"))
            
CK29_boundary <- cu_boundary[cu_boundary$FULL_CU_IN == "CK-29",]

ggplot() +
  geom_sf(data = EAU_VI) +
  geom_sf(data = CK29_boundary, colour = "blue") +
  geom_sf(data = bcfph_VI)


bcfph <- st_read(file.path(paths$spatial, "BCFishpass", "bcfishpass_streams_habitat", "bcfishpass_streams_2024-12-09.gpkg")) #%>%

bcfph_VI <- subset_intersections(st_zm(bcfph), EAU_VI)

save(bcfph_VI, file = file.path(paths$fw, "BCFP_VI.Rds"))


bcph_CK <- subset_intersections(st_zm(bcfph_VI), CK29_boundary)



