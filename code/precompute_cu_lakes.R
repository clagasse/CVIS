# ==============================================================================
# CVIS CU Lakes Pre-computation Script (precompute_cu_lakes.R)
#
# Description:
#   Pre-cuts and simplifies FWA lakes for each CU to eliminate heavy spatial
#   intersection calculations at report runtime.
# ==============================================================================

# Ensure setup paths are loaded
source(here::here("code", "0_setup.R"))

cat("Loading spatial datasets...\n")
# Load CU boundaries
load(file.path(paths$fw, "cu_boundary.Rds"))
# Load full FWA lakes
load(file.path(paths$fw, "BC_FWA_LAKES_FR.Rds"))

cat("Starting spatial pre-cutting for lakes across CUs...\n")
cu_codes <- unique(cu_boundary$FULL_CU_IN)
cu_lakes_precut <- list()

for (i in seq_along(cu_codes)) {
  cu_i <- cu_codes[i]
  cat(sprintf("[%d/%d] Processing CU: %s\n", i, length(cu_codes), cu_i))
  
  cu_boundary_i <- cu_boundary %>% dplyr::filter(FULL_CU_IN == cu_i)
  
  # Run spatial intersection
  temp <- unlist(sf::st_intersects(cu_boundary_i, lakes_Fr))
  lakes_cu <- lakes_Fr[temp, ]
  
  # Simplify geometries (15-meter tolerance) to reduce HTML size and render time
  if (nrow(lakes_cu) > 0) {
    lakes_cu <- sf::st_simplify(lakes_cu, dTolerance = 15)
  }
  
  cu_lakes_precut[[cu_i]] <- lakes_cu
}

# Ensure output directory exists and save
dir.create(file.path(here::here(), "output"), showWarnings = FALSE)
output_path <- file.path(here::here(), "output", "cu_lakes_precut.Rds")
saveRDS(cu_lakes_precut, output_path)
cat("Successfully saved pre-cut lakes to:", output_path, "\n")
