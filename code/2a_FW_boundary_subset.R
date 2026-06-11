# ==============================================================================
# CVIS CU Boundary Stream Subsetting (2a_FW_boundary_subset.R)
#
# Description:
#   Identifies and flags stream network segments that are spatially contained within
#   each Conservation Unit (CU) boundary. Creates a logical selection matrix used
#   to subset stream segments for subsequent freshwater indicators calculations.
#
# Workflow Steps:
#   1. Load setup environment and configuration variables.
#   2. Load the base stream network geometry (tscapes or fishpass).
#   3. Initialize a selection matrix (segments x CUs).
#   4. Spatial intersection loop: check which stream segments fall inside each CU boundary.
#   5. Save the resulting selection matrix RData to processed_data/freshwater/.
#
# Inputs:
#   - processed_data/freshwater/fw_models_tscapes.Rds (base thermalscapes stream network)
#   - cu_boundary spatial sf object (via 0_setup.R)
#
# Outputs:
#   - processed_data/freshwater/fw_streampicks_tscapes.Rdata
#
# Dependencies:
#   - Requires 0_setup.R. Run prior to rearing and migration stats calculations.
# ==============================================================================

# ==================== 1. Setup and Environment ====================
library(here)
setwd(here())
source(file.path(here(), "code", "0_setup.R"))

# ==================== 2. Initialize Stream Matrix Selection ====================
# choose stream base network
base_network <- switch(2, "bcfpa", "tscapes")

if (base_network == "tscapes") {
  load(file.path(paths$fw, "fw_models_tscapes.Rds"))
  fw_models_acc <- filter(fw_models, model_access_salmon %in% c("OBSERVED", "INFERRED"))
  
  stream_base <- st_geometry(fw_models)
  stream_base_acc <- st_geometry(fw_models_acc) #accessible streams only
  
  stream_cu_picks <- matrix(ncol = n.CUs, nrow = nrow(fw_models))
  stream_acc_cu_picks <- matrix(ncol = n.CUs, nrow = nrow(fw_models_acc)) 
  
  colnames(stream_acc_cu_picks) <- cu_seq
}

if (base_network == "bcfpa") {
  load(file.path(paths$fw, "BCFP_combined_accessible_Fr.Rds"))
  stream_base <- st_geometry(bcfpa)
  stream_cu_picks <- matrix(ncol = n.CUs, nrow = nrow(bcfpa))
}

# create matrix choosing streams contained within each CU boundary
colnames(stream_cu_picks)     <- cu_seq

# ==================== 3. Intersect Streams with CU Boundaries ====================
for (i in 1:n.CUs) {
  cu_pick <- cu_boundary[cu_boundary$FULL_CU_IN == cu_seq[i], ]
  
  pick_st <- lengths(st_intersects(st_zm(stream_base), cu_pick)) > 0
  stream_cu_picks[, i] <- pick_st
  
  if (base_network == "tscapes") {
    pick_st <- lengths(st_intersects(st_zm(stream_base_acc), cu_pick)) > 0
    stream_acc_cu_picks[, i] <- pick_st
  }

  print(paste(cu_seq[i], "boundary stream selection done"))
}

# ==================== 4. Save Outputs ====================
if (base_network == "tscapes") save(stream_cu_picks, stream_acc_cu_picks, file = file.path(paths$fw, "fw_streampicks_tscapes.Rdata"))
if (base_network == "bcfpa") save(stream_cu_picks, file = file.path(paths$fw, "fw_streampicks_bcfpa.Rdata"))
