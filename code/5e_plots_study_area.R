# ==============================================================================
# Fraser Basin Study Area Map with FAZs (5e_plots_study_area.R)
#
# Description:
#   Generates the study area map for the Fraser River Basin showing the 8
#   Freshwater Adaptive Zones (FAZs), the surrounding land basins, the BC coastline,
#   and the Marine Adaptive Zones (MAZs) with emphasis on the Salish Sea.
#   Below, species facets display the distribution of CUs cropped strictly to the basin.
#   This plot has no titles or subtitles on the canvas, suitable for manuscript inclusion.
# ==============================================================================

# Helper for standalone execution
if (!exists("paths")) {
  library(here)
  source(here("code", "0_setup.R"))
}

plot_fraser_basin_study_area <- function(save_path = NULL) {
  library(sf)
  library(tidyverse)
  library(patchwork)
  library(ggrepel)

  cat("Loading spatial layers for study area map...\n")
  # Load MAZ from processed_data/marine if not in environment
  if (!exists("MAZ")) {
    load(file.path(paths$marine, "MAZ.Rds")) # loads MAZ
  }
  
  # Setup bounding box limits for main map
  bbox_xlim <- c(350000, 1600000)
  bbox_ylim <- c(250000, 1300000)

  # Load GSHHS shoreline
  gshhs_path <- file.path(paths$spatial, "shoreline", "GSHHS_i_L1.shp")
  if (!file.exists(gshhs_path)) {
    stop("GSHHS shoreline not found at: ", gshhs_path)
  }
  gshhs <- st_read(gshhs_path, quiet = TRUE)
  
  # Load FAZ shapefile
  faz_path <- file.path(paths$spatial, "FAZ", "FreshwaterAdaptiveZones.shp")
  if (!file.exists(faz_path)) {
    stop("FreshwaterAdaptiveZones.shp not found at: ", faz_path)
  }
  FAZ_poly <- st_read(faz_path, quiet = TRUE)
  
  # Coordinate transformations & cleaning
  cat("Processing spatial layers...\n")
  gshhs_proj <- st_transform(gshhs, 3005)
  gshhs_lines <- st_boundary(gshhs_proj)
  
  bbox_sf <- st_as_sfc(st_bbox(c(xmin = bbox_xlim[1], ymin = bbox_ylim[1], xmax = bbox_xlim[2], ymax = bbox_ylim[2]), crs = 3005))
  sf_use_s2(FALSE)
  bc_coast_proj <- st_crop(gshhs_lines, bbox_sf)
  sf_use_s2(TRUE)
  
  FAZ_poly <- st_transform(FAZ_poly, 3005) %>% st_make_valid()
  Fr_basin <- filter(basins, BASIN == "FRASER") %>% st_make_valid()
  
  # Simplify detailed layers to speed up plotting
  basins_simple <- st_simplify(basins, preserveTopology = TRUE, dTolerance = 150) %>% st_make_valid()
  lakes_Fr_simple <- st_simplify(lakes_Fr, preserveTopology = TRUE, dTolerance = 100) %>% st_make_valid()
  
  # Filter FAZs within Fraser Basin (> 50,000 ha to get major 8 units)
  sf_use_s2(FALSE)
  intersects_faz <- st_intersects(FAZ_poly, Fr_basin, sparse = FALSE)[, 1]
  FAZ_fraser <- FAZ_poly[intersects_faz, ]
  FAZ_fraser_clip <- st_intersection(FAZ_fraser, Fr_basin) %>%
    mutate(area_clipped_ha = as.numeric(st_area(.)) / 10000) %>%
    filter(area_clipped_ha > 50000)
  sf_use_s2(TRUE)
  
  # Friendly names for the 8 FAZ units
  FAZ_fraser_clip <- FAZ_fraser_clip %>%
    mutate(
      FAZ_Label = case_when(
        FAZ_Acrony == "MFR" ~ "Middle Fraser",
        FAZ_Acrony == "UFR" ~ "Upper Fraser",
        FAZ_Acrony == "NTh" ~ "North Thompson",
        FAZ_Acrony == "LTh" ~ "Lower Thompson",
        FAZ_Acrony == "STh" ~ "South Thompson",
        FAZ_Acrony == "LFR" ~ "Lower Fraser",
        FAZ_Acrony == "LILL" ~ "Lillooet",
        FAZ_Acrony == "FRCany" ~ "Fraser Canyon",
        TRUE ~ FAZ_Name
      )
    )
  
  # Calculate centroids of FAZ units for labeling
  sf_use_s2(FALSE)
  faz_centroids <- FAZ_fraser_clip %>%
    st_centroid() %>%
    mutate(
      x = st_coordinates(.)[, 1],
      y = st_coordinates(.)[, 2]
    ) %>%
    mutate(
      x = if_else(FAZ_Acrony == "LILL", x + 15000, x),
      y = if_else(FAZ_Acrony == "LILL", y + 15000, y)
    )
  
  # Filter out Offshore MAZ and setup labels
  MAZ_no_offshore <- MAZ %>% filter(MAZ_Acrony != "Offshore") %>% st_make_valid()
  maz_centroids <- MAZ_no_offshore %>%
    st_centroid() %>%
    mutate(
      x = st_coordinates(.)[, 1],
      y = st_coordinates(.)[, 2]
    )
  sf_use_s2(TRUE)
  
  maz_labels <- tibble::tribble(
    ~MAZ_Acrony, ~MAZ_Label,
    "GStr", "Salish Sea\n(Georgia Strait)",
    "SFj", "Southern\nFjords",
    "WVI", "West Vancouver\nIsland",
    "HStr", "Hecate Strait",
    "NQCI", "North Queen\nCharlotte Islands",
    "WQCI", "West Queen\nCharlotte Islands",
    "NSKEst", "Nass Skeena\nEstuary"
  )
  maz_centroids <- maz_centroids %>%
    left_join(maz_labels, by = "MAZ_Acrony") %>%
    mutate(
      x = if_else(MAZ_Acrony == "GStr", 1100000, x),
      y = if_else(MAZ_Acrony == "GStr", 480000, y)
    )
  
  # Extract the Salish Sea (Georgia Strait) MAZ for bold outline emphasis
  gstr_maz <- MAZ_no_offshore %>% filter(MAZ_Acrony == "GStr")
  

  # Colors for 8 FAZ units
  faz_colors <- c(
    "Middle Fraser" = "#8dd3c7",
    "Upper Fraser" = "#fdb462",
    "North Thompson" = "#bebada",
    "Lower Thompson" = "#fb8072",
    "South Thompson" = "#80b1d3",
    "Lower Fraser" = "#b3de69",
    "Lillooet" = "#fccde5",
    "Fraser Canyon" = "#bc80bd"
  )
  
  # ----------------- Main Map -----------------
  p_main <- ggplot() +
    # Draw ocean background
    geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = "#eef4f8") +
    # Draw land basins (surrounding watersheds)
    geom_sf(data = basins_simple, fill = "#f5f5f2", color = "#e0e0dc", linewidth = 0.3) +
    # Draw FAZ drainage units within Fraser Basin
    geom_sf(data = FAZ_fraser_clip, aes(fill = FAZ_Label), color = "#ffffff", linewidth = 0.5) +
    scale_fill_manual(values = faz_colors) +
    # Draw major lakes inside Fraser Basin
    geom_sf(data = lakes_Fr_simple, fill = "#a9cce3", color = "#5dade2", linewidth = 0.1) +
    # Draw BC coastline outline
    geom_sf(data = bc_coast_proj, fill = NA, color = "#bdc3c7", linewidth = 0.5) +
    # Draw Marine Adaptive Zones (MAZs) except Georgia Strait
    geom_sf(data = filter(MAZ_no_offshore, MAZ_Acrony != "GStr"), fill = "#85c1e9", color = "#3498db", alpha = 0.35, linewidth = 0.4) +
    # Draw Georgia Strait (Salish Sea) MAZ with a different fill color and standard outline
    geom_sf(data = gstr_maz, fill = "#2471a3", color = "#1a5276", alpha = 0.55, linewidth = 0.4) +
    # Draw thick Fraser Basin outline
    geom_sf(data = Fr_basin, fill = NA, color = "#2c3e50", linewidth = 1.2) +
    # Add labels for Fraser FAZ drainage units
    geom_label_repel(
      data = faz_centroids,
      aes(x = x, y = y, label = FAZ_Label),
      size = 2.8,
      fontface = "bold",
      color = "#2c3e50",
      fill = "#ffffff",
      alpha = 0.9,
      box.padding = 0.25,
      max.overlaps = 15
    ) +
    # Add labels for MAZs (in black for manuscript visibility)
    geom_text_repel(
      data = maz_centroids,
      aes(x = x, y = y, label = MAZ_Label),
      size = 2.4,
      fontface = "bold",
      color = "black",
      bg.color = "#ffffff",
      bg.r = 0.1,
      box.padding = 0.4,
      max.overlaps = 15
    ) +
    coord_sf(xlim = bbox_xlim, ylim = bbox_ylim, expand = FALSE) +
    annotation_north_arrow(
      location = "tl",
      which_north = "true",
      pad_x = unit(0.4, "in"),
      pad_y = unit(0.4, "in"),
      style = north_arrow_minimal(
        text_col = "grey20",
        line_col = "grey20"
      )
    ) +
    labs(x = NULL, y = NULL) +
    theme_minimal() +
    theme(
      panel.grid.major = element_line(color = "grey80", linewidth = 0.2, linetype = "dashed"),
      panel.grid.minor = element_blank(),
      axis.text = element_text(size = 8, color = "grey40"),
      axis.title = element_blank(),
      legend.position = "none",
      plot.margin = margin(t = 2, r = 5, b = 2, l = 5),
      panel.border = element_rect(color = "grey50", fill = NA, linewidth = 0.5)
    )
  
  # ----------------- Species Facets -----------------
  # Filter cu_boundary to Fraser Basin CUs only
  sf_use_s2(FALSE)
  intersects_fraser <- st_intersects(cu_boundary, Fr_basin, sparse = FALSE)[, 1]
  cu_fraser <- cu_boundary[intersects_fraser, ]
  sf_use_s2(TRUE)
  
  cu_fraser <- cu_fraser %>% filter(!is.na(SPECIES_NAME))
  
  species_list <- c("Chinook", "Sockeye", "Coho", "Chum", "Pink")
  
  # Colors for species
  sp_palette <- get("species_palette", envir = .GlobalEnv)
  
  # Extract bounding box of Fraser Basin to crop the species plots exactly
  fr_bbox <- st_bbox(Fr_basin)
  fr_xlim <- c(fr_bbox["xmin"] - 15000, fr_bbox["xmax"] + 15000)
  fr_ylim <- c(fr_bbox["ymin"] - 15000, fr_bbox["ymax"] + 15000)
  
  sp_plots <- list()
  
  for (sp in species_list) {
    sp_cu <- cu_fraser %>% filter(SPECIES_NAME == sp)
    sp_color <- sp_palette[sp]
    
    p_sp <- ggplot() +
      # Draw Fraser Basin background
      geom_sf(data = Fr_basin, fill = "#fafafa", color = NA) +
      # Draw FAZ boundaries inside Fraser Basin faintly
      geom_sf(data = FAZ_fraser_clip, fill = NA, color = "#e0e0dc", linewidth = 0.3) +
      # Draw CU boundaries
      geom_sf(data = sp_cu, fill = sp_color, color = sp_color, alpha = 0.3, linewidth = 0.4) +
      # Fraser Basin outline
      geom_sf(data = Fr_basin, fill = NA, color = "#2c3e50", linewidth = 1.0) +
      coord_sf(xlim = fr_xlim, ylim = fr_ylim, expand = FALSE) +
      labs(
        title = paste(sp, "CUs"),
        subtitle = sprintf("%d CUs", nrow(sp_cu))
      ) +
      theme_void() +
      theme(
        plot.title = element_text(face = "bold", size = 11, color = sp_color, hjust = 0.5),
        plot.subtitle = element_text(size = 8.5, color = "#7f8c8d", hjust = 0.5, margin = margin(b = 5)),
        plot.margin = margin(t = 2, r = 2, b = 2, l = 2),
        legend.position = "none"
      )
    
    sp_plots[[sp]] <- p_sp
  }
  
  p_species_grid <- wrap_plots(sp_plots, ncol = 5) +
    plot_layout(guides = "collect") &
    theme(plot.margin = margin(t = 2, r = 2, b = 2, l = 2))
  
  # Combine Main Map and Species Grid with optimized height ratio 3.5 : 1
  p_final <- p_main / p_species_grid +
    plot_layout(heights = c(3.0, 1.2)) +
    plot_annotation(
      theme = theme(
        plot.margin = margin(t = 2, r = 2, b = 2, l = 2)
      )
    )
  
  if (!is.null(save_path)) {
    cat("Saving plot to:", save_path, "\n")
    # Ensure directory exists
    dir.create(dirname(save_path), showWarnings = FALSE, recursive = TRUE)
    ggsave(save_path, plot = p_final, width = 8, height = 8.7, dpi = 200, bg = "white")
  }
  
  return(p_final)
}

# Standalone execution
if (sys.nframe() == 0) {
  # Save to the standard figures folder and manuscript folder
  fig_output_path <- file.path(paths$figures, "fraser_basin_study_area_faz.png")
  manuscript_output_path <- file.path(paths$figures, "manuscript", "fraser_basin_study_area_faz.png")
  
  plot_fraser_basin_study_area(save_path = fig_output_path)
  
  cat("Copying to manuscript directory...\n")
  dir.create(dirname(manuscript_output_path), showWarnings = FALSE, recursive = TRUE)
  file.copy(fig_output_path, manuscript_output_path, overwrite = TRUE)
  
  cat("Study area map generated and saved successfully!\n")
}
