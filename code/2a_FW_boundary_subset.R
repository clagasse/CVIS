
#---------------------2a. CU boundary stream subsetting ---------------

# Simple script to create matrices of selections of streams within CU boundaries
# Matrix selections are used for subsetting and statistical summaries in other scripts

library(here)
setwd(here())
source(file.path(here(), "code", "0_setup.R"))

# choose stream base network
base_network <- switch(2, "bcfpa", "tscapes")

if (base_network == "tscapes") {
  load(file.path(paths$fw, "fw_models_tscapes.Rds"))
  stream_base <- st_geometry(fw_models)
  stream_cu_picks <- matrix(ncol = n.CUs, nrow = nrow(fw_models))
}

if (base_network == "bcfpa") {
  load(file.path(paths$fw, "BCFP_combined_accessible_Fr.Rds"))
  stream_base <- st_geometry(bcfpa)
  stream_cu_picks <- matrix(ncol = n.CUs, nrow = nrow(bcfpa))
}

cu_boundary <- st_read(file.path(paths$spatial, "CU_boundaries", "fraser_cus.shp")) %>%
  st_make_valid() %>%
  st_transform(crs = 3005)  %>%  # crs 3005 is NAD83/BC Albers
  left_join(select(cu_Fr, cuid, FULL_CU_IN, spp),
    join_by(CUID == cuid)) %>%
  filter(!is.na(FULL_CU_IN))


# create matrix choosing streams are contained within each CU boundary
colnames(stream_cu_picks) <- cu_seq

# ENM_cu_picks <- matrix(ncol = n.CUs, nrow = nrow(reaches_ENM_all))
# colnames(ENM_cu_picks) <- cu_seq


for (i in 1:n.CUs) {
  cu_pick <- cu_boundary[cu_boundary$FULL_CU_IN == cu_seq[i], ]
  pick_st <- lengths(st_intersects(st_zm(stream_base), cu_pick)) > 0
  stream_cu_picks[, i] <- pick_st

  # pick_st <- lengths(st_intersects(st_zm(reaches_ENM_all), cu_pick)) > 0
  # ENM_cu_picks[,i] <- pick_st

  print(paste(cu_seq[i], "boundary stream selection done"))
}

if (base_network == "tscapes") save(stream_cu_picks,  file = file.path(paths$fw, "fw_streampicks_tscapes.Rdata"))
if (base_network == "bcfpa") save(stream_cu_picks,  file = file.path(paths$fw, "fw_streampicks_bcfpa.Rdata"))
