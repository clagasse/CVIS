# ==============================================================================
# CVIS Conservation Unit Plotting Functions (5a_plots_CU.R)
#
# Description:
#   Collection of functions to plot, map, and visualize climate vulnerability 
#   indicators, physical habitats, and trends at the Conservation Unit (CU) level.
#   Includes stream networks, timing, migration routes, marine indicators, and 
#   risk score lollipop charts.
#
# List of Plotting Functions:
#   1. cu_timing_plot()                 - CU life stage timing with indicator periods.
#   2. stream_accessible_plot()         - Maps accessible streams and NUSEDS sites.
#   3. stream_indicator_plot()          - Stream map + histogram of a given indicator.
#   4. stream_indicator_multipanel_plot()- Side-by-side stream network panels.
#   5. migration_path_timing_plot()     - Combined geographic migration route map and projected mainstem temperatures plot.
#   6. cu_boundary_highlight()          - General locator map highlighting a single CU.
#   7. cu_hydrologic_regime()           - Maps CU boundaries with flow gauges and regimes.
#   8. plot_cu_lolli()                  - Lollipop chart comparing CU vs species means.
#   9. marine_indicator_plot()          - Maps marine SST and SSS indicators.
#   10. abundance_status_plot()         - Timeline of wild spawner abundance and WSP status.
#   11. plot_cu_indicators_lollipop()   - Comprehensive multi-indicator CU risk profile.
#   12. MAZ_boundary_highlight()        - General locator map highlighting a single MAZ.
#
# Dependencies:
#   - Requires ggplot2, sf, scico, patchwork, and standard CVIS data inputs.
# ==============================================================================

# Helper function for geometry simplification to reduce HTML file sizes and rendering times
simplify_geom_if_needed <- function(sf_obj, dTolerance = NULL) {
  if (is.null(sf_obj) || !inherits(sf_obj, "sf")) {
    return(sf_obj)
  }
  
  tol <- dTolerance
  if (is.null(tol)) {
    if (exists("geom_simplify_tol")) {
      tol <- get("geom_simplify_tol")
    } else {
      tol <- 0
    }
  }
  
  if (!is.null(tol) && !is.na(tol) && tol > 0) {
    if (nrow(sf_obj) == 0) {
      return(sf_obj)
    }
    
    sf_obj_simple <- tryCatch({
      sf::st_simplify(sf_obj, preserveTopology = TRUE, dTolerance = tol)
    }, error = function(e) {
      warning("Geometry simplification failed: ", e$message)
      sf_obj
    })
    return(sf_obj_simple)
  }
  
  return(sf_obj)
}


# ==================== 1. CU Timing Plot ====================
# Improved CU timing plot function - Version 2

# Shows life stage timing with indicator calculation periods

# Improved CU timing plot function - Version 2
# Shows life stage timing with indicator calculation periods

plot_timing_comparison <- function(cu_timing_long,
                                   selected_cu = NULL,
                                   cu_select = NULL,
                                   species_select = NULL,
                                   life_stages = c(
                                     "spawning", "run_timing",
                                     "ocean_entry", "freshwater_migration"
                                   ),
                                   sort_by = "species",
                                   show_peaks = TRUE,
                                   show_indicator_periods = FALSE,
                                   species_palette = NULL,
                                   life_stage_palette = NULL,
                                   date_breaks = "1 month",
                                   y_text_size = NULL,
                                   zoom_to_selected = FALSE) {

  # 1. Clean and filter input dataset
  data_plot <- cu_timing_long

  if (!is.null(cu_select)) {
    data_plot <- data_plot %>% filter(FULL_CU_IN %in% cu_select)
  }

  if (!is.null(species_select)) {
    if ("SPECIES_NAME" %in% names(data_plot)) {
      data_plot <- data_plot %>% filter(SPECIES_NAME %in% species_select)
    } else if ("species" %in% names(data_plot)) {
      data_plot <- data_plot %>% filter(species %in% species_select)
    }
  }

  if (!is.null(life_stages)) {
    data_plot <- data_plot %>% filter(life_stage %in% life_stages)
  }

  # If there is a selected_cu, make sure it is set
  if (is.null(selected_cu) && length(unique(data_plot$FULL_CU_IN)) == 1) {
    selected_cu <- unique(data_plot$FULL_CU_IN)[1]
  }

  # 1b. Pull full dataset for comparison if selected_cu is active and we only have that CU in our data_plot
  # (This matches the behavior of cu_timing_plot where it compares the single CU against all others)
  if (!is.null(selected_cu) && length(unique(data_plot$FULL_CU_IN)) == 1) {
    if (exists("cu_timing_long", envir = .GlobalEnv)) {
      timing_all <- get("cu_timing_long", envir = .GlobalEnv)
    } else {
      timing_file <- file.path("processed_data", "CU", "cu_timing_data.Rdata")
      if (file.exists(timing_file)) {
        temp_env <- new.env()
        load(timing_file, envir = temp_env)
        timing_all <- temp_env$cu_timing_long
      } else {
        timing_all <- data_plot
      }
    }
    # Keep the filter criteria on the timing_all for the plot
    if (!is.null(life_stages)) {
      timing_all <- timing_all %>% filter(life_stage %in% life_stages)
    }
    # Ensure the selected_cu is in the plot dataset
    data_plot <- timing_all
  }

  if (nrow(data_plot) == 0) {
    return(NULL)
  }

  # 2. Get species color palette
  if (is.null(species_palette)) {
    if (exists("species_palette", envir = .GlobalEnv)) {
      species_palette <- get("species_palette", envir = .GlobalEnv)
    } else {
      species_palette <- c(
        "Chinook" = "#1b9e77",
        "Coho" = "darkblue",
        "Sockeye" = "firebrick4",
        "Pink" = "purple3",
        "Chum" = "goldenrod4"
      )
    }
  }

  # 3. Clean and prepare labels
  if (!"life_stage_label" %in% names(data_plot)) {
    data_plot <- data_plot %>%
      mutate(
        life_stage_label = case_when(
          life_stage == "spawning" ~ "Spawning",
          life_stage == "run_timing" ~ "Upstream Run Timing",
          life_stage == "ocean_entry" ~ "Ocean Entry",
          life_stage == "freshwater_migration" ~ "Juvenile FW Migration",
          TRUE ~ life_stage
        )
      )
  }

  data_plot <- data_plot %>%
    mutate(
      life_stage_label = factor(life_stage_label,
        levels = c(
          "Spawning", "Upstream Run Timing",
          "Ocean Entry", "Juvenile FW Migration"
        )
      )
    )

  df_plot <- data_plot %>%
    filter(!is.na(start), !is.na(end)) %>%
    mutate(
      date_start = as.Date("2000-01-01") + start,
      date_peak = as.Date("2000-01-01") + peak,
      date_end = as.Date("2000-01-01") + end
    )

  # 4. Extract data quality info and timing events for the selected CU
  dq_summary <- NULL
  rt_start_date <- rt_end_date <- sp_start_date <- sp_peak_date <- NULL
  sel_timing <- NULL

  if (!is.null(selected_cu)) {
    sel_timing <- df_plot %>% filter(FULL_CU_IN == selected_cu)
    if (nrow(sel_timing) > 0) {
      get_dq_str <- function(stage_name) {
        dq <- sel_timing %>% filter(life_stage == stage_name) %>% pull(dat_qual)
        if (length(dq) > 0 && !is.na(dq[1])) as.character(dq[1]) else "N/A"
      }
      
      fm_dq <- get_dq_str("freshwater_migration")
      oe_dq <- get_dq_str("ocean_entry")
      rt_dq <- get_dq_str("run_timing")
      sp_dq <- get_dq_str("spawning")
      
      dq_summary <- sprintf("Data Quality: Spawning=%s, Run Timing=%s, Ocean Entry=%s, FW Migration=%s (1=Best, 6=Worst)", 
                            sp_dq, rt_dq, oe_dq, fm_dq)

      # Get vertical line values for selected CU timing milestones
      rt_start_val <- sel_timing %>% filter(life_stage == "run_timing") %>% pull(start)
      rt_end_val <- sel_timing %>% filter(life_stage == "run_timing") %>% pull(end)
      sp_start_val <- sel_timing %>% filter(life_stage == "spawning") %>% pull(start)
      sp_peak_val <- sel_timing %>% filter(life_stage == "spawning") %>% pull(peak)
      
      rt_start_date <- if (length(rt_start_val) > 0) as.Date("2000-01-01") + rt_start_val[1] else NULL
      rt_end_date <- if (length(rt_end_val) > 0) as.Date("2000-01-01") + rt_end_val[1] else NULL
      sp_start_date <- if (length(sp_start_val) > 0) as.Date("2000-01-01") + sp_start_val[1] else NULL
      sp_peak_date <- if (length(sp_peak_val) > 0) as.Date("2000-01-01") + sp_peak_val[1] else NULL
    }
  }

  # 5. Build y-axis factors and highlights
  cu_labels_df <- df_plot %>%
    select(FULL_CU_IN, CVIS_NAME, culabel, SPECIES_NAME) %>%
    distinct()
  
  if (!is.null(selected_cu) && !selected_cu %in% cu_labels_df$FULL_CU_IN) {
    selected_row <- cu_timing_long %>% 
      filter(FULL_CU_IN == selected_cu) %>%
      select(FULL_CU_IN, CVIS_NAME, culabel, SPECIES_NAME) %>% 
      distinct()
    if (nrow(selected_row) > 0) {
      cu_labels_df <- bind_rows(cu_labels_df, selected_row) %>% distinct()
    }
  }

  # Define label sizes based on selected_cu mode
  default_size <- if (!is.null(selected_cu)) "5.2pt" else "8pt"
  y_sz <- if (!is.null(y_text_size)) paste0(y_text_size, "pt") else default_size

  cu_labels_df <- cu_labels_df %>%
    mutate(
      color_hex = species_palette[SPECIES_NAME],
      color_hex = if_else(is.na(color_hex), "#4B5563", color_hex),
      label_clean = paste0(culabel, " (", FULL_CU_IN, ")")
    )

  if (!is.null(selected_cu)) {
    cu_labels_df <- cu_labels_df %>%
      mutate(
        label_formatted = if_else(
          FULL_CU_IN == selected_cu,
          paste0("<span style='color:", color_hex, "; font-size:8.5pt;'><b>▶ ", label_clean, "</b></span>"),
          paste0("<span style='color:", color_hex, "; font-size:", y_sz, ";'>", label_clean, "</span>")
        )
      )
  } else {
    cu_labels_df <- cu_labels_df %>%
      mutate(
        label_formatted = paste0("<span style='color:", color_hex, "; font-size:", y_sz, ";'>", label_clean, "</span>")
      )
  }

  # Sort CUs
  if (sort_by == "species") {
    cu_labels_ordered <- cu_labels_df %>%
      arrange(desc(SPECIES_NAME), desc(CVIS_NAME))
  } else if (sort_by == "peak_spawn") {
    sp_peaks <- df_plot %>%
      filter(life_stage == "spawning") %>%
      arrange(desc(peak)) %>%
      left_join(select(cu_labels_df, FULL_CU_IN, label_formatted), by = "FULL_CU_IN") %>%
      select(label_formatted) %>%
      distinct()
    cu_labels_ordered <- cu_labels_df %>%
      mutate(label_formatted = factor(label_formatted, levels = unique(c(sp_peaks$label_formatted, label_formatted)))) %>%
      arrange(label_formatted)
  } else if (sort_by == "peak_oe") {
    oe_peaks <- df_plot %>%
      filter(life_stage == "ocean_entry") %>%
      arrange(desc(peak)) %>%
      left_join(select(cu_labels_df, FULL_CU_IN, label_formatted), by = "FULL_CU_IN") %>%
      select(label_formatted) %>%
      distinct()
    cu_labels_ordered <- cu_labels_df %>%
      mutate(label_formatted = factor(label_formatted, levels = unique(c(oe_peaks$label_formatted, label_formatted)))) %>%
      arrange(label_formatted)
  } else {
    cu_labels_ordered <- cu_labels_df
  }

  df_plot <- df_plot %>%
    left_join(select(cu_labels_df, FULL_CU_IN, label_clean, label_formatted), by = "FULL_CU_IN")
  
  levels_ordered <- unique(cu_labels_ordered$label_formatted)
  df_plot$y_axis_factor <- factor(df_plot$label_formatted, levels = levels_ordered)

  highlight_idx <- if (!is.null(selected_cu)) {
    which(levels_ordered == cu_labels_df$label_formatted[cu_labels_df$FULL_CU_IN == selected_cu])
  } else {
    integer(0)
  }

  # Add data quality category columns
  df_plot <- df_plot %>%
    mutate(dat_qual_cat = case_when(
      dat_qual %in% c(1, 2) ~ "High (1-2)",
      dat_qual %in% c(3, 4) ~ "Medium (3-4)",
      dat_qual %in% c(5, 6) ~ "Low (5-6)",
      TRUE ~ "Unknown"
    )) %>%
    mutate(dat_qual_cat = factor(dat_qual_cat, levels = c("High (1-2)", "Medium (3-4)", "Low (5-6)", "Unknown")))

  # 6. Define colors for life stages
  if (is.null(life_stage_palette)) {
    life_stage_palette <- c(
      "Spawning" = "#66C2A5",
      "Upstream Run Timing" = "#FC8D62",
      "Ocean Entry" = "#8DA0CB",
      "Juvenile FW Migration" = "#E78AC3"
    )
  }

  p <- ggplot(df_plot, aes(y = y_axis_factor))

  # Background highlight for the selected CU
  if (length(highlight_idx) > 0) {
    p <- p + annotate("rect",
      xmin = as.Date("2000-01-01"), xmax = as.Date("2000-12-31"),
      ymin = highlight_idx - 0.45, ymax = highlight_idx + 0.45,
      fill = "#FFE0B2", alpha = 0.35
    )
  }

  # Highlight timing milestones of the selected CU vertically
  if (!is.null(rt_start_date)) {
    p <- p + geom_vline(xintercept = rt_start_date, linetype = "dashed", color = "#3498db", linewidth = 0.5, alpha = 0.6)
  }
  if (!is.null(rt_end_date)) {
    p <- p + geom_vline(xintercept = rt_end_date, linetype = "dashed", color = "#3498db", linewidth = 0.5, alpha = 0.6)
  }
  if (!is.null(sp_start_date)) {
    p <- p + geom_vline(xintercept = sp_start_date, linetype = "dashed", color = "#e74c3c", linewidth = 0.5, alpha = 0.6)
  }
  if (!is.null(sp_peak_date)) {
    p <- p + geom_vline(xintercept = sp_peak_date, linetype = "dashed", color = "#e74c3c", linewidth = 0.5, alpha = 0.6)
  }

  # Draw range segments
  if (!is.null(selected_cu)) {
    # Thick segments for selected CU, thin for others
    p <- p + geom_segment(
      data = filter(df_plot, FULL_CU_IN != selected_cu),
      aes(x = date_start, xend = date_end, yend = y_axis_factor, color = life_stage_label),
      linewidth = 1.4, alpha = 0.65
    ) + geom_segment(
      data = filter(df_plot, FULL_CU_IN == selected_cu),
      aes(x = date_start, xend = date_end, yend = y_axis_factor, color = life_stage_label),
      linewidth = 5.5, alpha = 0.95
    )
  } else {
    # Standard width segments for comparison plot
    p <- p + geom_segment(
      aes(x = date_start, xend = date_end, yend = y_axis_factor, color = life_stage_label),
      linewidth = 4, alpha = 0.7
    )
  }

  # Draw peak points if show_peaks is TRUE
  if (show_peaks) {
    if (!is.null(selected_cu)) {
      # Large points for selected CU, small for others
      p <- p + geom_point(
        data = filter(df_plot, FULL_CU_IN != selected_cu),
        aes(x = date_peak, fill = life_stage_label, shape = dat_qual_cat),
        color = "grey30", size = 1.4, stroke = 0.3
      ) + geom_point(
        data = filter(df_plot, FULL_CU_IN == selected_cu),
        aes(x = date_peak, fill = life_stage_label, shape = dat_qual_cat),
        color = "black", size = 4.0, stroke = 1.2
      )
    } else {
      # Standard size points for comparison plot
      p <- p + geom_point(
        aes(x = date_peak, fill = life_stage_label, shape = dat_qual_cat),
        color = "grey30", size = 3, stroke = 1.0
      )
    }
  }

  p <- p +
    scale_color_manual(values = life_stage_palette, name = "Life Stage") +
    scale_fill_manual(values = life_stage_palette, name = "Life Stage") +
    scale_shape_manual(
      name = "Data Quality",
      values = c(
        "High (1-2)" = 21,
        "Medium (3-4)" = 24,
        "Low (5-6)" = 22,
        "Unknown" = 4
      ),
      guide = guide_legend(override.aes = list(size = 3, fill = "white", stroke = 1.0))
    )

  if (show_indicator_periods) {
    p <- p + geom_rect(
      aes(xmin = as.Date("2000-08-01"), xmax = as.Date("2000-08-31"), ymin = -Inf, ymax = Inf),
      fill = "#FFE0B2", alpha = 0.08, inherit.aes = FALSE
    )
  }

  # Zoom x-axis
  xlim_start <- as.Date("2000-01-01")
  xlim_end <- as.Date("2000-12-31")
  
  if (zoom_to_selected && !is.null(sel_timing) && nrow(sel_timing) > 0) {
    pad_start <- min(sel_timing$date_start, na.rm = TRUE)
    pad_end <- max(sel_timing$date_end, na.rm = TRUE)
    if (!is.na(pad_start) && !is.na(pad_end)) {
      xlim_start <- pad_start - 30
      xlim_end <- pad_end + 30
      if (xlim_start < as.Date("2000-01-01")) xlim_start <- as.Date("2000-01-01")
      if (xlim_end > as.Date("2000-12-31")) xlim_end <- as.Date("2000-12-31")
    }
  }

  p <- p +
    scale_y_discrete(limits = levels_ordered) +
    scale_x_date(
      date_breaks = date_breaks,
      date_labels = "%b",
      limits = c(xlim_start, xlim_end),
      expand = c(0.01, 0)
    ) +
    labs(
      title = title_text,
      subtitle = subtitle_text,
      x = "Date",
      y = NULL,
      caption = caption_text
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 9, angle = if (is.null(selected_cu)) 45 else 0, hjust = if (is.null(selected_cu)) 1 else 0.5),
      axis.text.y = ggtext::element_markdown(lineheight = 0.8),
      axis.text.y.left = ggtext::element_markdown(lineheight = 0.8),
      axis.title.x = element_text(size = 10, face = "bold"),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = if (is.null(selected_cu)) "right" else "bottom",
      legend.box = "horizontal",
      legend.margin = margin(t = 0),
      legend.title = element_text(size = 9, face = "bold"),
      legend.text = element_text(size = 8.5),
      plot.title = element_text(face = "bold", size = 12),
      plot.subtitle = element_text(size = 9.5, face = "italic", color = "grey30"),
      plot.caption = element_text(size = 8, color = "grey50", hjust = 0),
      plot.margin = margin(5, 5, 5, 5)
    )

  return(p)
}

cu_timing_plot <- function(data, show_indicator_periods = FALSE) {
  # Wrapper that detects selected_cu and delegates to consolidated plot_timing_comparison
  selected_cu <- unique(data$FULL_CU_IN)[1]
  plot_timing_comparison(
    cu_timing_long = data,
    selected_cu = selected_cu,
    show_indicator_periods = show_indicator_periods,
    zoom_to_selected = TRUE
  )
}

# # # Load your timing data
# cu_timing_long_i <- cu_timing_long %>% filter(FULL_CU_IN == "CK-11")
# # # Create plot
# p <- cu_timing_plot(cu_timing_long_i, show_indicator_periods = F)
# print(p)



# ==================== 2. Stream Network Accessible Plot ====================
# input subset of stream network, nuseds data, and CU boundary for a specific CU
stream_accessible_plot <- function(stream_data,
                                   nuseds_data,
                                   cu_boundary,
                                   lakes_cu) {
  # Simplify geometries if needed
  if (exists("cu_boundary_i", envir = .GlobalEnv)) {
    cu_boundary_i <- simplify_geom_if_needed(get("cu_boundary_i", envir = .GlobalEnv))
  } else {
    cu_boundary_i <- simplify_geom_if_needed(cu_boundary)
  }
  lakes_cu <- simplify_geom_if_needed(lakes_cu)
  stream_data <- simplify_geom_if_needed(stream_data)

  p1 <- ggplot() +
    geom_sf(data = cu_boundary_i, color = "black", alpha = 0.3)

  if (nrow(lakes_cu) > 0) p1 <- p1 + geom_sf(data = lakes_cu, color = "darkblue", alpha = 0.7)

  p1 <- p1 +
    geom_sf(data = nuseds_cu, aes(fill = SPECIES), size = 2, alpha = 0.6) +
    geom_sf(data = st_zm(stream_data), aes(color = model_rs)) +
    coord_sf(
      xlim = st_bbox(cu_boundary_i)[c(1, 3)],
      ylim = st_bbox(cu_boundary_i)[c(2, 4)]
    ) +
    labs(
      colour = "BC FishPass",
      fill = "NUSEDS sites"
    )

  return(p1)
}


# ==================== 3. Stream Indicator Map & Histogram ====================

stream_indicator_plot <- function(fwModels,
                                  cu_boundary,
                                  lakes_cu,
                                  Tw_stations,
                                  variable = "CT_anad",
                                  plot_title = "",
                                  histogram_fill = "model_rs",
                                  xlim = NA,
                                  unit_label = NULL,
                                  temp_stations = FALSE,
                                  risk_palette = cvis_risk_palette,
                                  palette_direction = 1) {

  # Simplify geometries if needed
  fwModels <- simplify_geom_if_needed(fwModels)
  cu_boundary <- simplify_geom_if_needed(cu_boundary)
  lakes_cu <- simplify_geom_if_needed(lakes_cu)

  # Filter stream segments to those that intersect with the CU boundary polygon
  cu_boundary_union <- sf::st_union(cu_boundary)
  intersects_mask <- sf::st_intersects(fwModels, cu_boundary_union, sparse = FALSE)[, 1]
  if (any(intersects_mask)) {
    fwModels <- fwModels[intersects_mask, ]
  }

  var_sym <- sym(variable)
  hist_sym <- sym(histogram_fill)

  color_range <- range(fwModels[[as.character(var_sym)]], na.rm = TRUE)

  ## stream map
  p1 <- ggplot() +
    geom_sf(data = fwModels, aes(color = !!var_sym), linewidth = 1.) +
    scale_color_cvis(
      palette = risk_palette,
      direction = palette_direction,
      limits = color_range
    ) +
    geom_sf(data = cu_boundary, color = "black", alpha = 0.3) +
    coord_sf(
      xlim = st_bbox(cu_boundary)[c(1, 3)],
      ylim = st_bbox(cu_boundary)[c(2, 4)],
      datum = NA
    ) + # this eliminates axis labels
    labs(
      subtitle = plot_title,
      color = unit_label
    )

  if (nrow(lakes_cu) > 0) p1 <- p1 + geom_sf(data = lakes_cu, color = "darkblue", alpha = 0.7)

  if (sum(!is.na(xlim)) > 0) {
    p1 <- p1 + scale_color_cvis(palette = risk_palette, direction = palette_direction, limits = xlim)
  }

  if (temp_stations == TRUE) {
    p1 <- p1 +
      geom_sf(
        data = Tw_stations, aes(shape = "Temperature Gauge"),
        size = 0.6, show.legend = TRUE
      ) +
      coord_sf(
        xlim = st_bbox(cu_boundary)[c(1, 3)],
        ylim = st_bbox(cu_boundary)[c(2, 4)],
        datum = NA
      ) +
      scale_shape_manual(
        values = c("Temperature Gauge" = 16),
        name = NULL
      )
  }

  ## histogram plotting
  # If using BC Fishpass, set factor levels
  if (histogram_fill == "model_rs") fwModels[[histogram_fill]] <- factor(fwModels[[histogram_fill]], levels = c("2-ACCESSIBLE", "1-SPAWNING/REARING"))

  h1 <- ggplot(fwModels) +
    geom_histogram(aes(x = !!var_sym, fill = !!hist_sym))

  # add custom labels for BCFishPass
  if (histogram_fill == "model_rs") {
    h1 <- h1 + labs(fill = "Habitat Potential (BC Fishpass)") +
      scale_fill_manual(values = c(
        "2-ACCESSIBLE" = "darkgrey",
        "1-SPAWNING/REARING" = "forestgreen"
      ))
  }


  # Extract max y from built plot
  y_pos <- max(ggplot_build(h1)$data[[1]]$count, na.rm = TRUE) * 1.05
  # add mean value line and label
  h1 <- h1 +
    # geom_vline(aes(xintercept = mean(!!var_sym, na.rm = TRUE)),
    #   color = "red", linetype = "dashed"
    # ) +
    # annotate("text",
    #   x = mean(fwModels[[variable]], na.rm = TRUE),
    #   y = y_pos,
    #   label = "mean"
    # ) +
    theme_void() +
    theme(
      axis.line.x = element_line(color = "black"),
      axis.text.x = element_text(color = "black", margin = margin(t = 6)),
      axis.ticks.x = element_line(color = "black"),
      axis.title.x = element_text(color = "black", margin = margin(t = 6)),
      plot.margin = margin(1, 2, 12, 2)
    )

  if (sum(!is.na(xlim)) > 0) {
    h1 <- h1 + xlim(xlim)
  }

  p1 / h1 + plot_layout(heights = c(4, 1))
  
}


#' Multi-panel stream network plot of indicator values within a CU boundary
#' 
#' Plots multiple indicators side-by-side using patchwork with histograms,
#' using compact legends.
#'
#' @param fwModels sf object containing stream network and model outputs.
#' @param cu_boundary sf object representing the CU boundary.
#' @param lakes_cu sf object containing lakes within the CU (optional).
#' @param variables Character vector of variables (indicators) to plot.
#' @param plot_titles Optional character vector or named vector of titles.
#' @param unit_labels Optional character vector or named vector of unit labels.
#' @param risk_palette Character vector or named vector of palettes. Default cvis_risk_palette.
#' @param palette_directions Integer vector or named vector of directions. Default 1.
#' @param ncol Integer. Number of columns in layout.
#' @param nrow Integer. Number of rows in layout.
#'
stream_indicator_multipanel_plot <- function(fwModels,
                                             cu_boundary,
                                             lakes_cu = NULL,
                                             variables = c("CT_anad"),
                                             plot_titles = NULL,
                                             unit_labels = NULL,
                                             risk_palette = cvis_risk_palette,
                                             palette_directions = 1,
                                             ncol = NULL,
                                             nrow = NULL) {
  # Simplify geometries if needed
  fwModels <- simplify_geom_if_needed(fwModels)
  cu_boundary <- simplify_geom_if_needed(cu_boundary)
  lakes_cu <- simplify_geom_if_needed(lakes_cu)

  # Helper to resolve parameter by index or name
  get_param_by_name <- function(param, var_name, idx, default_val) {
    if (is.null(param)) {
      return(default_val)
    }
    if (!is.null(names(param)) && var_name %in% names(param)) {
      return(param[[var_name]])
    }
    if (length(param) >= idx) {
      return(param[idx])
    }
    return(param[1])
  }

  plots <- list()
  for (i in seq_along(variables)) {
    var_name <- variables[i]
    var_sym <- sym(var_name)
    
    # Check if variable exists in the dataframe
    if (!var_name %in% names(fwModels)) {
      warning("Variable '", var_name, "' not found in fwModels. Skipping.")
      next
    }
    
    p_title <- get_param_by_name(plot_titles, var_name, i, var_name)
    u_label <- get_param_by_name(unit_labels, var_name, i, NULL)
    
    # Try loading unit from tbl_indicators if not provided
    if (is.null(u_label) && exists("tbl_indicators")) {
      match_idx <- sapply(tbl_indicators$abbrev, function(ab) {
        startsWith(var_name, ab)
      })
      if (any(match_idx)) {
        u_label <- tbl_indicators$unit[which(match_idx)[1]]
      }
    }
    
    # Shorten unit names for display in the histogram
    if (!is.null(u_label)) {
      u_label <- case_when(
        u_label == "Temperature change per decade (°C)" ~ "°C / decade",
        u_label == "Temperature (°C)" ~ "°C",
        u_label == "Proportion change from baseline" ~ "Prop. change",
        u_label == "Threat score" ~ "Threat",
        u_label == "Favourability" ~ "Fav. change",
        TRUE ~ u_label
      )
    }
    
    p_palette <- get_param_by_name(risk_palette, var_name, i, "roma")
    p_dir <- get_param_by_name(palette_directions, var_name, i, 1)
    
    # Filter out stream segments with NA values for this indicator
    panel_data <- fwModels[!is.na(st_drop_geometry(fwModels)[[var_name]]), ]
    
    # Filter stream segments to those that intersect with the CU boundary polygon (not just the bounding box)
    cu_boundary_union <- sf::st_union(cu_boundary)
    intersects_mask <- sf::st_intersects(panel_data, cu_boundary_union, sparse = FALSE)[, 1]
    if (any(intersects_mask)) {
      panel_data <- panel_data[intersects_mask, ]
    }
    
    # Crop to the bounding box of the CU boundary so data/ranges only include visible streams
    panel_data <- suppressWarnings(sf::st_crop(panel_data, sf::st_bbox(cu_boundary)))
    
    if (nrow(panel_data) == 0) {
      warning("No visible stream data inside the CU boundary for variable '", var_name, "'. Skipping.")
      next
    }
    
    # Calculate limits for this indicator to avoid issues with NA or empty ranges
    vals <- panel_data[[var_name]]
    val_range <- range(vals, na.rm = TRUE)
    if (any(is.infinite(val_range)) || any(is.nan(val_range))) {
      val_range <- c(0, 1) # Fallback range
    } else {
      # Clamp to IQR-based outlier thresholds: [Q1 - 1.5*IQR, Q3 + 1.5*IQR]
      q <- quantile(vals, probs = c(0.25, 0.75), na.rm = TRUE)
      iqr <- q[2] - q[1]
      if (iqr > 0) {
        val_range[1] <- max(val_range[1], q[1] - 1.5 * iqr)
        val_range[2] <- min(val_range[2], q[2] + 1.5 * iqr)
      }
    }
    
    # Constrain range to [-1, 1] for favourability and proportion change indicators
    if (startsWith(var_name, "favchange") || startsWith(var_name, "flow8pdelta") || startsWith(var_name, "flow18pdelta")) {
      val_range[1] <- max(val_range[1], -1)
      val_range[2] <- min(val_range[2], 1)
    }
    
    p <- ggplot() +
      geom_sf(data = panel_data, aes(color = !!var_sym)) +
      scale_color_cvis(
        palette = p_palette,
        direction = p_dir,
        limits = val_range,
        guide = "none",
        oob = scales::squish
      ) +
      geom_sf(data = cu_boundary, color = "black", alpha = 0.05)
      
    if (!is.null(lakes_cu) && inherits(lakes_cu, "sf") && nrow(lakes_cu) > 0) {
      p <- p + geom_sf(data = lakes_cu, color = "darkgrey", alpha = 0.7)
    }
    
    p <- p +
      coord_sf(
        xlim = st_bbox(cu_boundary)[c(1, 3)],
        ylim = st_bbox(cu_boundary)[c(2, 4)],
        datum = NA
      ) +
      labs(title = p_title) +
      theme_void() +
      theme(
        plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
        plot.margin = margin(5, 5, 5, 5)
      )

    # Inset histogram for distribution of values across stream segments
    p_hist <- ggplot(st_drop_geometry(panel_data), aes(x = !!var_sym)) +
      geom_histogram(aes(fill = after_stat(x)), bins = 15, color = "white", linewidth = 0.1, show.legend = FALSE, na.rm = TRUE) +
      scale_fill_cvis(
        palette = p_palette,
        direction = p_dir,
        limits = val_range,
        oob = scales::squish
      ) +
      scale_x_continuous(
        limits = val_range,
        oob = scales::squish,
        breaks = c(val_range[1], (val_range[1] + val_range[2])/2, val_range[2]),
        labels = function(x) sprintf("%.1f", x)
      ) +
      labs(x = u_label) +
      theme_void() +
      theme(
        axis.line.x = element_line(color = "black", linewidth = 0.5),
        axis.text.x = element_text(color = "black", size = 7, face = "bold", margin = margin(t = 6)),
        axis.ticks.x = element_line(color = "black", linewidth = 0.5),
        axis.title.x = if (!is.null(u_label)) element_text(color = "black", size = 6.5, face = "bold", margin = margin(t = 6)) else element_blank(),
        plot.background = element_rect(fill = "white", color = NA),
        plot.margin = margin(1, 2, 12, 2)
      )

    p_combined <- p + patchwork::inset_element(
      p_hist,
      left = 0.03,
      bottom = 0.03,
      right = 0.38,
      top = 0.32,
      align_to = "panel"
    )
      
    plots[[length(plots) + 1]] <- p_combined
  }
  
  if (length(plots) == 0) {
    stop("No valid variables plotted.")
  }
  
  wrap_plots(plots, ncol = ncol, nrow = nrow)
}
# ==================== 4. Migration Path Map ====================

migration_path_timing_plot <- function(migr_path = NULL,
                                       nuseds_data = NULL,
                                       cu_boundary = NULL,
                                       migr_daily_all = NULL,
                                       cu_i = NULL,
                                       timing = NULL,
                                       rcp = "45",
                                       period_choose = c("1981-2010", "2041-2060"),
                                       plot_title = "") {

    # Retrieve missing variables from global environment if available
  if (is.null(cu_i)) {
    if (exists("cu_i", envir = .GlobalEnv)) {
      cu_i <- get("cu_i", envir = .GlobalEnv)
    } else if (exists("params", envir = .GlobalEnv)) {
      params_obj <- get("params", envir = .GlobalEnv)
      if ("FULL_CU_IN" %in% names(params_obj)) {
        cu_i <- params_obj$FULL_CU_IN
      }
    }
  }

  if (is.null(migr_daily_all)) {
    if (exists("migr_daily_all", envir = .GlobalEnv)) {
      migr_daily_all <- get("migr_daily_all", envir = .GlobalEnv)
    }
  }

  if (is.null(timing)) {
    if (exists("cu_timing_i", envir = .GlobalEnv)) {
      timing <- get("cu_timing_i", envir = .GlobalEnv)
    } else if (exists("cu_timing_Fr", envir = .GlobalEnv) && !is.null(cu_i)) {
      cu_timing_Fr_obj <- get("cu_timing_Fr", envir = .GlobalEnv)
      timing <- cu_timing_Fr_obj %>% dplyr::filter(FULL_CU_IN == cu_i)
    }
  }

  if (is.null(migr_path)) {
    if (exists("migr_cu", envir = .GlobalEnv)) {
      migr_path <- get("migr_cu", envir = .GlobalEnv)
    } else if (exists("migr_list", envir = .GlobalEnv) && !is.null(cu_i)) {
      migr_list_obj <- get("migr_list", envir = .GlobalEnv)
      if (cu_i %in% names(migr_list_obj)) {
        migr_path <- migr_list_obj[[cu_i]]
      }
    }
  }

  if (is.null(nuseds_data)) {
    if (exists("nuseds_cu", envir = .GlobalEnv)) {
      nuseds_data <- get("nuseds_cu", envir = .GlobalEnv)
    } else if (exists("nuseds_Fr", envir = .GlobalEnv) && !is.null(cu_i)) {
      nuseds_Fr_obj <- get("nuseds_Fr", envir = .GlobalEnv)
      nuseds_data <- nuseds_Fr_obj %>% dplyr::filter(FULL_CU_IN == cu_i)
    }
  }

  if (is.null(cu_boundary)) {
    if (exists("cu_boundary_i", envir = .GlobalEnv)) {
      cu_boundary <- get("cu_boundary_i", envir = .GlobalEnv)
    } else if (exists("cu_boundary", envir = .GlobalEnv) && !is.null(cu_i)) {
      cu_boundary_full <- get("cu_boundary", envir = .GlobalEnv)
      cu_boundary <- cu_boundary_full %>% dplyr::filter(FULL_CU_IN == cu_i)
    }
  }

  # Simplify geometries if needed
  migr_path <- simplify_geom_if_needed(migr_path)
  cu_boundary <- simplify_geom_if_needed(cu_boundary)

  # Build the Temperature & Timing Plot (p_temp)
  p_temp <- NULL
  df_all <- NULL
  if (!is.null(migr_daily_all) && !is.null(cu_i) && !is.null(timing) && nrow(timing) > 0) {
    components <- c("mean", "0.1", "0.9")

    if (is.list(migr_daily_all) && !is.data.frame(migr_daily_all)) {
      migrT_select <- migr_daily_all[[rcp]][[cu_i]][["doy"]]
      long_list <- components %>%
        purrr::set_names() %>%
        map(~ migrT_select[[.x]] %>%
          as.data.frame() %>%
          mutate(doy = as.numeric(rownames(.))) %>%
          pivot_longer(-doy, names_to = "period", values_to = .x))
      df_all <- reduce(long_list, left_join, by = c("doy", "period")) %>%
        drop_na() %>%
        mutate(period = factor(period, levels = rev(sort(unique(period))))) %>%
        filter(period %in% period_choose)
    } else {
      if (exists("period_lookup")) {
        period_codes <- period_lookup %>%
          dplyr::filter(dsmodel == "pcicgrid", period %in% period_choose) %>%
          dplyr::pull(period_code) %>%
          unique()
      } else {
        period_codes <- NULL
      }

      query <- migr_daily_all %>%
        dplyr::filter(
          FULL_CU_IN == cu_i,
          rcp == !!rcp,
          attr == "migrT"
        )

      if (!is.null(period_codes) && length(period_codes) > 0) {
        query <- query %>% dplyr::filter(period_code %in% period_codes)
      } else {
        query <- query %>% dplyr::filter(period %in% period_choose)
      }

      df_unnested <- query %>%
        dplyr::select(time) %>%
        tidyr::unnest(time) %>%
        dplyr::mutate(doy = as.numeric(time))

      df_all <- df_unnested %>%
        dplyr::group_by(period, doy) %>%
        dplyr::summarise(
          mean = mean(migrT, na.rm = TRUE),
          `0.1` = as.numeric(stats::quantile(migrT, probs = 0.1, na.rm = TRUE)),
          `0.9` = as.numeric(stats::quantile(migrT, probs = 0.9, na.rm = TRUE)),
          .groups = "drop"
        ) %>%
        dplyr::mutate(period = factor(period, levels = rev(sort(unique(df_unnested$period))))) %>%
        dplyr::filter(period %in% period_choose) %>%
        dplyr::select(doy, period, mean, `0.1`, `0.9`)
    }

    # Determine x-axis range based on CU timing limits + 1 month padding
    events <- c(timing$rt_start, timing$rt_end, timing$sp_start, timing$sp_peak)
    events <- events[!is.na(events)]
    if (length(events) > 0) {
      min_event <- min(events)
      max_event <- max(events)
    } else {
      min_event <- 150
      max_event <- 280
    }

    x_min <- max(1, min_event - 30)
    x_max <- min(365, max_event + 30)

    month_breaks <- c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335)
    month_labels <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    in_range <- month_breaks >= x_min & month_breaks <= x_max
    breaks_to_use <- month_breaks[in_range]
    labels_to_use <- month_labels[in_range]
    if (length(breaks_to_use) == 0) {
      breaks_to_use <- month_breaks
      labels_to_use <- month_labels
    }

    # Filter future projection period for GCM range ribbon (exclude historical range)
    future_period <- tail(period_choose, 1)
    df_future_ribbon <- df_all %>% dplyr::filter(period == future_period)

    p_temp <- ggplot(df_all, aes(x = doy, y = mean, color = period)) +
      # Show GCM ribbon ONLY for the projection period
      geom_ribbon(data = df_future_ribbon, aes(x = doy, ymin = `0.1`, ymax = `0.9`, group = period), alpha = 0.15, fill = "#e74c3c", color = NA, inherit.aes = FALSE) +
      geom_path(linewidth = 1) +
      geom_vline(xintercept = timing$rt_start, linetype = "dashed", color = "#3498db", linewidth = 0.8) +
      geom_vline(xintercept = timing$rt_end, linetype = "dashed", color = "#3498db", linewidth = 0.8) +
      geom_vline(xintercept = timing$sp_start, linetype = "dashed", color = "#e74c3c", linewidth = 0.8) +
      geom_vline(xintercept = timing$sp_peak, linetype = "dashed", color = "#e74c3c", linewidth = 0.8) +
      annotate("text", x = timing$rt_start, y = Inf, label = "Run Timing Start", vjust = 2, color = "#3498db", angle = 90, hjust = 1.1, size = 3, fontface = "bold") +
      annotate("text", x = timing$rt_end, y = Inf, label = "Run Timing End", vjust = 2, color = "#3498db", angle = 90, hjust = 1.1, size = 3, fontface = "bold") +
      annotate("text", x = timing$sp_start, y = Inf, label = "Spawning Start", vjust = -1, color = "#e74c3c", angle = 90, hjust = 1.1, size = 3, fontface = "bold") +
      annotate("text", x = timing$sp_peak, y = Inf, label = "Spawning Peak", vjust = -1, color = "#e74c3c", angle = 90, hjust = 1.1, size = 3, fontface = "bold") +
      labs(
        x = "Month",
        y = "Migration temperature (°C)",
        color = "Period"
      ) +
      scale_color_manual(
        values = c(
          "1981-2010" = "#2c3e50",
          "2041-2060" = "#e74c3c",
          "1981-2000" = "#2c3e50"
        )
      ) +
      scale_x_continuous(
        breaks = breaks_to_use,
        labels = labels_to_use
      ) +
      coord_cartesian(xlim = c(x_min, x_max), expand = FALSE) +
      theme_bw() +
      theme(
        plot.margin = margin(t = 5, r = 5, b = 5, l = 5, unit = "pt"),
        legend.position = "bottom"
      ) +
      annotation_custom(grid::textGrob("a", x = unit(0.96, "npc"), y = unit(0.92, "npc"), gp = grid::gpar(fontface = "bold", fontsize = 12)))
  }

  # Build the Map Plot (p_map)
  p_map <- NULL
  if (!is.null(migr_path) && nrow(migr_path) > 0) {
    target_crs <- sf::st_crs(migr_path)

    # Reproject other map layers
    if (exists("bc_coast", envir = .GlobalEnv)) {
      bc_coast_proj <- sf::st_transform(get("bc_coast", envir = .GlobalEnv), target_crs)
    } else if (exists("paths") && !is.null(paths$marine) && file.exists(file.path(paths$marine, "bc_coast.Rds"))) {
      bc_coast_proj <- sf::st_transform(readRDS(file.path(paths$marine, "bc_coast.Rds")), target_crs)
    } else {
      library(pacea)
      bc_coast_proj <- sf::st_transform(pacea::bc_coast, target_crs)
    }

    if (exists("Fr_basin", envir = .GlobalEnv)) {
      Fr_basin_proj <- sf::st_transform(get("Fr_basin", envir = .GlobalEnv), target_crs)
    } else {
      if (exists("paths") && !is.null(paths$fw) && file.exists(file.path(paths$fw, "basins_shp.Rds"))) {
        load(file.path(paths$fw, "basins_shp.Rds"))
        Fr_basin_proj <- sf::st_transform(dplyr::filter(basins, BASIN == "FRASER"), target_crs)
      } else {
        Fr_basin_proj <- NULL
      }
    }

    if (exists("lakes_Fr", envir = .GlobalEnv)) {
      lakes_proj <- sf::st_transform(get("lakes_Fr", envir = .GlobalEnv), target_crs)
    } else {
      if (exists("paths") && !is.null(paths$fw) && file.exists(file.path(paths$fw, "BC_FWA_LAKES_FR.Rds"))) {
        load(file.path(paths$fw, "BC_FWA_LAKES_FR.Rds"))
        lakes_proj <- sf::st_transform(lakes_Fr, target_crs)
      } else {
        lakes_proj <- NULL
      }
    }

    # Reproject cu_boundary
    if (!is.null(cu_boundary) && nrow(cu_boundary) > 0) {
      cu_boundary <- sf::st_transform(cu_boundary, target_crs)
    }

    # Simplify projected layers if needed to reduce file size and speed up rendering
    bc_coast_proj <- simplify_geom_if_needed(bc_coast_proj)
    if (!is.null(Fr_basin_proj)) {
      Fr_basin_proj <- simplify_geom_if_needed(Fr_basin_proj)
    }
    if (!is.null(lakes_proj)) {
      lakes_proj <- simplify_geom_if_needed(lakes_proj)
    }

    # Crop bounding box with margin, incorporating the ENTIRE CU boundary if available
    bbox_path <- sf::st_bbox(migr_path)
    if (!is.null(cu_boundary) && nrow(cu_boundary) > 0) {
      bbox_boundary <- sf::st_bbox(cu_boundary)
      bbox <- c(
        xmin = min(bbox_path["xmin"], bbox_boundary["xmin"], na.rm = TRUE),
        ymin = min(bbox_path["ymin"], bbox_boundary["ymin"], na.rm = TRUE),
        xmax = max(bbox_path["xmax"], bbox_boundary["xmax"], na.rm = TRUE),
        ymax = max(bbox_path["ymax"], bbox_boundary["ymax"], na.rm = TRUE)
      )
      class(bbox) <- "bbox"
    } else {
      bbox <- bbox_path
    }

    x_range <- bbox["xmax"] - bbox["xmin"]
    y_range <- bbox["ymax"] - bbox["ymin"]
    margin_factor <- 0.08
    xlims <- c(bbox["xmin"] - margin_factor * x_range, bbox["xmax"] + margin_factor * x_range)
    ylims <- c(bbox["ymin"] - margin_factor * y_range, bbox["ymax"] + margin_factor * y_range)

    # Fallback for species palette
    if (exists("species_palette", envir = .GlobalEnv)) {
      spp_colors <- get("species_palette", envir = .GlobalEnv)
    } else {
      spp_colors <- c("Chinook" = "#E69F00", "Chum" = "#56B4E9", "Coho" = "#009E73", "Pink" = "#F0E442", "Sockeye" = "#D55E00")
    }

    # Calculate average historical flow for that CU during migration period
    avg_migr_flow <- NA_real_
    df_flow_all <- NULL

    if (is.data.frame(migr_daily_all)) {
      if (exists("period_lookup")) {
        period_codes <- period_lookup %>%
          dplyr::filter(dsmodel == "pcicgrid", period %in% period_choose) %>%
          dplyr::pull(period_code) %>%
          unique()
      } else {
        period_codes <- NULL
      }

      query_flow <- migr_daily_all %>%
        dplyr::filter(
          FULL_CU_IN == cu_i,
          rcp == !!rcp,
          attr == "migrQ"
        )

      if (!is.null(period_codes) && length(period_codes) > 0) {
        query_flow <- query_flow %>% dplyr::filter(period_code %in% period_codes)
      } else {
        query_flow <- query_flow %>% dplyr::filter(period %in% period_choose)
      }

      if (nrow(query_flow) > 0) {
        df_flow_unnested <- query_flow %>%
          dplyr::select(time) %>%
          tidyr::unnest(time) %>%
          dplyr::mutate(doy = as.numeric(time))

        df_flow_all <- df_flow_unnested %>%
          dplyr::group_by(period, doy) %>%
          dplyr::summarise(
            mean = mean(migrQ, na.rm = TRUE),
            .groups = "drop"
          )
      }
    } else if (is.list(migr_daily_all) && exists("migrQ_rcps", envir = .GlobalEnv)) {
      migrQ_rcps_obj <- get("migrQ_rcps", envir = .GlobalEnv)
      if (!is.null(migrQ_rcps_obj) && rcp %in% names(migrQ_rcps_obj) && cu_i %in% names(migrQ_rcps_obj[[rcp]])) {
        migrQ_select <- migrQ_rcps_obj[[rcp]][[cu_i]][["doy"]]
        df_flow_all <- migrQ_select[["mean"]] %>%
          as.data.frame() %>%
          mutate(doy = as.numeric(rownames(.))) %>%
          pivot_longer(-doy, names_to = "period", values_to = "mean")
      }
    }

    # Extract historical flow component within the migration timing window
    df_hist_flow <- NULL
    if (!is.null(df_flow_all) && nrow(df_flow_all) > 0) {
      hist_period <- period_choose[1]
      df_hist_flow <- df_flow_all %>% dplyr::filter(period == hist_period)
      if (nrow(df_hist_flow) == 0) {
        df_hist_flow <- df_flow_all %>% dplyr::filter(period == unique(df_flow_all$period)[1])
      }
      
      rt_s <- timing$rt_start[1]
      rt_e <- timing$rt_end[1]
      if (is.na(rt_s)) rt_s <- 1
      if (is.na(rt_e)) rt_e <- 365

      if (rt_s <= rt_e) {
        df_migr_flow <- df_hist_flow %>% dplyr::filter(doy >= rt_s, doy <= rt_e)
      } else {
        df_migr_flow <- df_hist_flow %>% dplyr::filter(doy >= rt_s | doy <= rt_e)
      }
      
      avg_migr_flow <- mean(df_migr_flow$mean, na.rm = TRUE)
      if (is.na(avg_migr_flow) || !is.finite(avg_migr_flow)) {
        avg_migr_flow <- mean(df_hist_flow$mean, na.rm = TRUE)
      }
    }

    # Fallback default value
    if (is.na(avg_migr_flow) || !is.finite(avg_migr_flow)) {
      avg_migr_flow <- 10.0
    }

    migr_path_temp <- migr_path
    migr_path_temp$migr_flow <- avg_migr_flow

    # Color scale limits derived from historical flow range
    if (!is.null(df_hist_flow) && nrow(df_hist_flow) > 0) {
      min_flow_val <- min(df_hist_flow$mean, na.rm = TRUE)
      max_flow_val <- max(df_hist_flow$mean, na.rm = TRUE)
    } else {
      min_flow_val <- 0
      max_flow_val <- 100
    }
    if (is.na(min_flow_val) || !is.finite(min_flow_val)) min_flow_val <- 0
    if (is.na(max_flow_val) || !is.finite(max_flow_val)) max_flow_val <- 100

    p_map <- ggplot() +
      geom_sf(data = bc_coast_proj, fill = "grey90", color = "grey75", linewidth = 0.3)
    
    if (!is.null(Fr_basin_proj)) {
      p_map <- p_map + geom_sf(data = Fr_basin_proj, fill = "antiquewhite", color = "grey60", linewidth = 0.4)
    }
    
    if (!is.null(lakes_proj)) {
      p_map <- p_map + geom_sf(data = lakes_proj, fill = "aliceblue", color = "aliceblue", linewidth = 0.1)
    }

    # Draw CU boundary outline
    if (!is.null(cu_boundary) && nrow(cu_boundary) > 0) {
      p_map <- p_map +
        geom_sf(
          data = cu_boundary,
          fill = "grey",
          alpha = 0.1
        )
    }

    # Color migration path dynamically by the average historical flow using the batlow palette
    p_map <- p_map +
      geom_sf(data = migr_path_temp, aes(color = migr_flow), linewidth = 1.2) +
      scico::scale_color_scico(
        palette = "batlow",
        name = "Historic Flow\n(m³/s)",
        direction = 1,
        limits = c(min_flow_val, max_flow_val),
        oob = scales::squish,
        labels = function(x) {
          ifelse(x >= 1000, paste0(format(round(x/1000, 1), nsmall = 0), "k"), round(x, 0))
        }
      )

    # Add NuSEDS sites points
    if (!is.null(nuseds_data) && nrow(nuseds_data) > 0) {
      p_map <- p_map +
        geom_sf(
          data = nuseds_data, aes(fill = SPECIES_LOOKUP),
          color = "black", # outline color
          size = 3, # increase point size
          shape = 21
        ) +
        scale_fill_manual(values = spp_colors, name = "NuSEDS sites")
    }

    p_map <- p_map +
      coord_sf(xlim = xlims, ylim = ylims, expand = FALSE) +
      theme_void() +
      theme(
        plot.margin = margin(t = 5, r = 5, b = 5, l = 5, unit = "pt"),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
        legend.position = "bottom"
      ) +
      annotation_custom(grid::textGrob("b", x = unit(0.94, "npc"), y = unit(0.94, "npc"), gp = grid::gpar(fontface = "bold", fontsize = 12)))
  }

  # Combine them side-by-side using patchwork
  if (!is.null(p_temp) && !is.null(p_map)) {
    p <- patchwork::wrap_plots(p_temp, p_map, ncol = 2, widths = c(1.8, 1.2)) +
      patchwork::plot_layout(guides = "collect") &
      theme(
        legend.box.spacing = unit(4, "pt"),
        legend.margin = margin(0, 0, 0, 0, "pt"),
        legend.position = "bottom"
      )
    return(p)
  } else if (!is.null(p_temp)) {
    return(p_temp)
  } else if (!is.null(p_map)) {
    return(p_map)
  } else {
    stop("No data available to generate map or timing plot.")
  }
}



# ==================== 6. CU Boundary Highlight ====================

# simple plot showing location of the CU boundary outline compared to all CU boundaries

cu_boundary_highlight <- function(cu_boundary,
                                  cu_pick) {
  cu_boundary_i <- filter(cu_boundary, FULL_CU_IN == cu_pick)

  p <- ggplot() +
    # annotation_map_tile(type = "cartolight") +
    geom_sf(data = cu_boundary, color = "black", alpha = 0.3) +
    geom_sf(data = cu_boundary_i, fill = "green")
  # labs(title = cu_pick)

  return(p)
}


# ==================== 7. Hydrologic Regimes & Gauges ====================
cu_hydrologic_regime <- function(cu_boundary_i,
                                 stream_data,
                                 watershed_flow_cu,
                                 stations_cu,
                                 fwModels_cu) {
  # Wrap the legend text before plotting
  stations_cu$Station.Wrapped <- str_wrap(stations_cu$Station.Name, width = 15) # Adjust width as needed


  p <- ggplot() +
    geom_sf(data = cu_boundary_i, color = "black", fill = "grey", alpha = 0.5) +
    geom_sf(data = st_zm(stream_data)) +
    geom_sf(data = watershed_flow_cu, aes(fill = regime), alpha = 0.7) +
    geom_sf(data = stations_cu, aes(colour = Station.Wrapped), size = 2) +
    scale_colour_brewer(palette = "Set1") +
    # geom_sf_text(data = stations_cu, aes(label = Station.Wrapped), size = 2) +
    # geom_sf(data = st_zm(fwModels_cu), alpha = 0.4) +
    coord_sf(
      xlim = st_bbox(cu_boundary_i)[c(1, 3)],
      ylim = st_bbox(cu_boundary_i)[c(2, 4)]
    ) +
    labs(
      fill = "Hydrologic regime",
      color = "Flow Gauge"
    )
}



# ==================== 8. CU Lollipop Chart ====================


# ind_cu <- get_CU_indicators(all_flat_std,
#   cu_i = "CK-10",
#   RCP_pick = "45",
#   period_pick = "3",
#   indicators_choose = tbl_indicators$abbrev)

plot_cu_lolli <- function(data, # need indicator data for a single CU, use get_cu_indicators()
                          # indicators_choose = c("migrTproj", "migrQpdelta", "migrA21", "migrdist"),
                          indicators_choose = c("CUstatus", "CUnmat"),
                          use_standardized = TRUE, # use raw or transformed (standardized values)
                          plot_colours = species_palette) {
  colors <- c(
    "CU" = "blue",
    "Species Mean" = "black",
    "Above Species Mean" = "darkred",
    "Below Species Mean" = "forestgreen",
    "GCM Variation" = "gray60"
  )

  data <- data %>%
    filter(indicator %in% indicators_choose)

  if ("stat" %in% names(data)) {
    gcm_check <- sum(data$stat == "qlowgcm")

    data_wide <- data %>%
      pivot_wider(
        id_cols = indicator,
        names_from = stat,
        values_from = c(cu_value, sp_value)
      )
  } else {
    gcm_check <- sum(c("cu_qlowgcm", "cu_value_qlowgcm") %in% names(data))

    data_wide <- data
  }

  data_wide <- data_wide %>%
    mutate(
      cu_value_mean = coalesce(cu_value_mean, cu_value),
      sp_value_mean = coalesce(sp_value_mean, sp_value),
      cu_value_qlowgcm = coalesce(cu_value_qlowgcm, cu_qlowgcm),
      cu_value_qhighgcm = coalesce(cu_value_qhighgcm, cu_qhighgcm),
      above_sp = case_when(
        is.na(sp_value_mean) ~ NA,
        cu_value_mean > sp_value_mean ~ "Above Species Mean",
        TRUE ~ "Below Species Mean"
      )
    )

  p <- ggplot(data_wide, aes(x = indicator))
  if (gcm_check >= 1) { # add gcm variation if data exists
    # GCM variation segment
    p <- p + geom_segment(aes(
      xend = indicator, y = cu_value_qlowgcm, yend = cu_value_qhighgcm,
      color = "GCM Variation"
    ), size = 2)
  }
  # CU vs Species Mean segment
  p <- p + geom_segment(
    aes(
      xend = indicator,
      y = cu_value_mean, yend = sp_value_mean,
      color = above_sp
    ),
    size = 3, alpha = 0.5
  ) +
    # Points
    geom_point(aes(y = cu_value_mean, color = "CU"), size = 3) +
    geom_point(aes(y = sp_value_mean, color = "Species Mean"), size = 2) +
    # Flip coordinates
    coord_flip() +
    # Labels and theme
    labs(
      x = "",
      y = "Standardized Value",
      color = "Legend"
    ) +
    scale_color_manual(values = colors) +
    ylim(0, 1)

  return(p)
}




# ==================== 9. Marine Indicators Map ====================

# SST_cu_sp <- subset_and_mean_sst(sf_data = filter(SST_grid, MAZ_Acrony == "GStr"),
#   timing_df = cu_mar,
#   RCP_pick = "45")
#
# MAZ <- st_read(file.path(paths$spatial, "MAZ", "MAZ_Final.shp"))
#
# MAZ_GStr <- filter(MAZ, MAZ_Acrony == "GStr")

marine_indicator_plot <- function(data,
                                  MAZ_sp,
                                  var = "SST_oe",
                                  unit_label = "Degrees C",
                                  risk_palette = cvis_risk_palette,
                                  plot_title = "",
                                  palette_direction = -1,
                                  palette_limits = c(9, 14)) {

  p <- ggplot() +
    geom_sf(data = data, aes(colour = !!sym(var))) +
    scale_color_cvis(
      palette   = risk_palette,
      direction = palette_direction,
      limits    = palette_limits # <-- set your min/max here
    ) +
    geom_sf(data = MAZ, fill = NA, color = "black") +
    coord_sf(
      xlim = st_bbox(data)[c(1, 3)],
      ylim = st_bbox(data)[c(2, 4)]
    ) +
    labs(
      color = unit_label,
      title = plot_title
    )

  return(p)
}


# ==================== 10. Abundance & Status Trend ====================

abundance_status_plot <- function(status_data,
                                  cu_i) {
  status_palette <- c(
    "Red" = "firebrick",
    "Amber" = "orange",
    "Green" = "green2",
    "None" = "grey40"
  )

  trend_palette <- c(
    "Annual Abundance" = "blue4",
    "Geometric Avg" = "darkred"
  )

  plot_data <- status_data %>%
    filter(FULL_CU_IN == cu_i) %>%
    mutate(ConfidenceRating5 = if_else(is.na(ConfidenceRating5), "None", ConfidenceRating5)) %>%
    mutate(
      RapidStatus = factor(RapidStatus, levels = c("Red", "Amber", "Green", "None")),
      ConfidenceRating5 = factor(ConfidenceRating5, levels = c("Low", "Moderate", "High", "None")),
      SpawnerAbundance = SpnForAbd_Wild / 1000,
      GeometricAvgAbundnace = GenAvgUsed / 1000
    )

  # latest_entry <- plot_data[which.max(plot_data$Year), ]
  latest_entry <- plot_data[which.max(ifelse(plot_data$RapidStatus != "None", plot_data$Year, -Inf)), ]
  latest_status <- latest_entry$RapidStatus
  # latest_abundance <- round(latest_entry$SpnForAbd_Wild)
  latest_abundance <- ifelse(is.na(latest_entry$SpnForAbd_Wild), "NA", round(latest_entry$SpnForAbd_Wild))
  if (is.numeric(latest_abundance)) scales::comma(latest_abundance)
  latest_genabd <- ifelse(is.na(latest_entry$GenAvgUsed), "NA", round(latest_entry$GenAvgUsed))
  if (is.numeric(latest_genabd)) scales::comma(latest_genabd)
  status_color <- status_palette[latest_status]

  if (latest_entry$DataType == "Abs_Abd" && !is.na(latest_entry$DataType)) data_type <- "Absolute Abundance"
  if (latest_entry$DataType == "Rel_Idx" && !is.na(latest_entry$DataType)) data_type <- "Relative Index"
  if (is.na(latest_entry$DataType)) data_type <- "NA"

  # Create a one-row data frame for annotation
  annotation_df <- data.frame(
    x = max(plot_data$Year),
    y = max(plot_data$SpawnerAbundance, na.rm = TRUE) * 0.96, # slightly below top
    label = paste0(
      plot_data$CVIS_NAME, "<br>",
      "Data Type: ", data_type, "</span><br>",
      "Most Recent Status: <span style='color:", status_color, "'>", latest_status, "</span><br>",
      "Recent Spawner Abundance: ", latest_abundance, "</span><br>",
      "Recent Generational Avg: ", latest_genabd
    )
  )

  legend_lines <- data.frame(
    Year = c(2000, 2000), # any values, won't be plotted
    Abundance = c(0, 0),
    LineType = c("Geometric Avg", "Annual Abundance")
  )

  p <- ggplot(data = plot_data) +
    # geom_hline(aes(yintercept = RelAbd_LBM/1000), linetype = "dashed", color = "red") +
    # geom_hline(aes(yintercept = RelAbd_UBM/1000), linetype = "dashed", color = "green") +
    geom_line(aes(x = Year, y = GeometricAvgAbundnace), color = trend_palette[2], size = 2) +
    geom_line(aes(x = Year, y = SpawnerAbundance), color = trend_palette[1], size = 2) +
    geom_point(
      aes(x = Year, y = 0, fill = RapidStatus, shape = ConfidenceRating5),
      size = 3,
      color = "black",
      stroke = 0.5
    ) +
    # Add blank geom to trigger custom legend
    geom_line(
      data = legend_lines,
      aes(x = Year, y = Abundance, color = LineType),
      size = 2,
      alpha = 0
    ) +
    scale_color_manual(
      values = trend_palette,
      name = "Abundance Type"
    ) +
    scale_fill_manual(
      values = status_palette,
      name = "Rapid Status",
      drop = FALSE
    ) +
    scale_shape_manual(
      values = c("Low" = 24, "Moderate" = 21, "High" = 22, "None" = 10),
      name = "Confidence Rating",
      drop = FALSE
    ) +
    guides(
      fill = guide_legend(override.aes = list(
        shape = c(21, 21, 21, 10),
        fill = status_palette,
        color = "black",
        size = 3
      )),
      shape = guide_legend(override.aes = list(
        shape = c(24, 21, 22, 10),
        fill = "grey",
        color = "black",
        stroke = 0.5
      )),
      color = guide_legend(override.aes = list(
        linetype = c("solid", "solid"),
        color = trend_palette,
        size = 2,
        alpha = 1
      ))
    ) +
    geom_richtext(
      data = annotation_df,
      aes(x = x, y = y, label = label),
      hjust = 1,
      vjust = 1,
      size = 3.5,
      fill = NA,
      label.color = NA
    ) +
    labs(y = "Wild Spawner Abundance (1000s)")


  return(p)
}

# abundance_status_plot(status_data,
#   cu_i = "SEL-03-02")


# ==================== 11. Comprehensive CU Indicators Lollipop ====================

# Function to create a comprehensive lollipop chart showing all indicators for one CU
# This is the OPPOSITE of plot_lollipop which shows one indicator across all CUs

plot_cu_indicators_lollipop <- function(data,
                                        indicators_choose = NULL, # NULL = all indicators
                                        group_by_category = TRUE, # Group indicators by type
                                        show_species_avg = TRUE, # Show species average comparison
                                        show_all_cu_avg = TRUE, # Show all CU average comparison
                                        show_gcm_variation = TRUE, # Show GCM uncertainty
                                        plot_title = NULL,
                                        y_limit = c(0, 1)) {
  sp_name <- data$SPECIES_NAME[1]
  cu_name <- data$CVIS_NAME[1]

  # If no indicators specified, use all
  if (is.null(indicators_choose)) {
    indicators_choose <- tbl_indicators$abbrev
  }

  # Prepare data for plotting
  plot_data <- data %>%
    left_join(select(tbl_indicators, abbrev, name, category),
      by = c("indicator" = "abbrev")
    ) %>%
    mutate(
      # Calculate difference from species average
      diff_from_avg = cu_value - sp_value,
      # Categorize as above or below average
      comparison = case_when(
        is.na(sp_value) ~ "No Comparison",
        cu_value > sp_value ~ "Above Average",
        cu_value < sp_value ~ "Below Average",
        TRUE ~ "At Average"
      ),

      # Category labels for grouping
      category_label = case_when(
        category %in% c("fwrs", "fwR") ~ "Spawning & Rearing",
        category == "migr" ~ "Upstream Migration",
        category == "dem" ~ "Demographics",
        category == "mar" ~ "Nearshore Marine",
        TRUE ~ "Other"
      )
    )

  # Define color palettes
  comparison_colors <- c(
    "Below Average" = "forestgreen",
    "Above Average" = "darkred",
    "At Average" = "grey50",
    "No Comparison" = "grey70"
  )

  # Order indicators by category if requested
  if (group_by_category) {
    plot_data <- plot_data %>%
      arrange(category, indicator) %>%
      mutate(name = factor(name, levels = unique(name)))
  } else {
    plot_data <- plot_data %>%
      arrange(desc(cu_value)) %>%
      mutate(name = factor(name, levels = unique(name)))
  }

  # Create the plot
  p <- ggplot(plot_data, aes(x = name, y = cu_value))

  # Add GCM variation bars if requested and available
  # get_CU_indicators now produces cu_qlowgcm, cu_qhighgcm etc.
  has_gcm_cols <- any(c("cu_qlowgcm", "cu_value_qlowgcm") %in% names(plot_data))

  if (show_gcm_variation && has_gcm_cols) {
    gcm_min_col <- if ("cu_qlowgcm" %in% names(plot_data)) "cu_qlowgcm" else "cu_value_qlowgcm"
    gcm_max_col <- if ("cu_qhighgcm" %in% names(plot_data)) "cu_qhighgcm" else "cu_value_qhighgcm"

    p <- p + geom_segment(
      aes(xend = name, y = !!sym(gcm_min_col), yend = !!sym(gcm_max_col)),
      color = "grey70",
      linewidth = 3,
      alpha = 0.5,
      na.rm = TRUE
    )
  }

  # Add species average comparison if requested
  if (show_species_avg) {
    p <- p + geom_segment(
      aes(xend = name, yend = cu_value, y = sp_value, color = comparison),
      linewidth = 1.5,
      arrow = arrow(length = unit(0.1, "inches"), type = "closed"),
      alpha = 0.7,
      na.rm = TRUE
    ) +
      geom_point(aes(y = sp_value, shape = "Species Average"),
        fill = "grey",
        color = "black",
        size = 3,
        na.rm = TRUE
      )
  }

  # Add all CU average if requested
  if (show_all_cu_avg) {
    p <- p + geom_point(aes(y = allcu_value, shape = "All Species Average"),
      fill = "gold",
      color = "black",
      size = 3,
      na.rm = TRUE
    )
  }

  # Add main points with continuous color scale (RdYlGn reversed so red = high risk)
  p <- p + geom_point(aes(fill = cu_value, shape = "CU Value"),
    color = "black",
    size = 4,
    stroke = 1
  ) +
    scale_fill_cvis(
      palette = cvis_risk_palette,
      direction = cvis_risk_direction, # Reversed: red for high values (high risk)
      limits = c(0, 1),
      name = "Risk Score",
      guide = guide_colorbar(order = 1)
    )

  # Build shape scale based on what's being shown
  shape_names <- "CU Value"
  shape_values <- c(21)
  shape_fills <- c("green3")
  shape_sizes <- c(4)

  if (show_species_avg) {
    shape_names <- c(shape_names, "Species Average")
    shape_values <- c(shape_values, 22)
    shape_fills <- c(shape_fills, "grey")
    shape_sizes <- c(shape_sizes, 3)
  }

  if (show_all_cu_avg) {
    shape_names <- c(shape_names, "All Species Average")
    shape_values <- c(shape_values, 23)
    shape_fills <- c(shape_fills, "gold")
    shape_sizes <- c(shape_sizes, 3)
  }

  shape_df <- tibble(shape_names, shape_values, shape_fills, shape_sizes)

  p <- p +
    scale_shape_manual(
      name = "Data Points",
      values = shape_df$shape_values,
      guide = guide_legend(
        order = 3,
        override.aes = list(
          fill = shape_df$shape_fills,
          size = shape_df$shape_sizes
        )
      )
    )

  # Add comparison color scale if showing species average
  if (show_species_avg) {
    p <- p + scale_color_manual(
      values = comparison_colors,
      name = "vs Species Avg",
      guide = guide_legend(order = 2)
    )
  }

  # Finalize plot
  caption_text <- "Large circles: CU value"
  if (show_species_avg) caption_text <- paste0(caption_text, " | Grey circles: Species average")
  if (show_all_cu_avg) caption_text <- paste0(caption_text, " | Gold diamonds: All CU average")
  if (show_species_avg) caption_text <- paste0(caption_text, "\nArrow direction: above/below species average")
  if (show_gcm_variation) caption_text <- paste0(caption_text, " | Grey bars: Climate model uncertainty")

  p <- p +
    coord_flip() +
    scale_y_continuous(limits = y_limit, breaks = seq(0, 1, 0.2)) +
    labs(
      title = if (is.null(plot_title)) paste0("Climate Vulnerability Indicators: ", cu_name) else plot_title,
      x = NULL,
      y = "Standardized Indicator Value (0 = Low Risk, 1 = High Risk)",
      caption = caption_text
    ) +
    theme(
      legend.position = "right",
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 11, color = "grey40"),
      plot.caption = element_text(size = 9, color = "grey50", hjust = 0),
      axis.text.y = element_text(size = 10),
      panel.grid.major.y = element_line(color = "grey90"),
      panel.grid.minor = element_blank()
    )

  # Add faceting by category if requested
  if (group_by_category) {
    p <- p + facet_grid(
      rows = vars(category_label),
      scales = "free_y",
      space = "free_y"
    ) +
      theme(
        strip.text.y = element_text(angle = 0, hjust = 0, face = "bold"),
        strip.background = element_rect(fill = "grey95", color = NA)
      ) +
      theme_minimal()
  }

  return(p)
}


# cu_ind <- get_CU_indicators(all_flat_std,
#   cu_i = "CK-06",
#   RCP_pick = "45",
#   period_pick = "3",
# )
#
# plot_cu_indicators_lollipop(cu_ind)




# ==================== 12. MAZ Boundary Highlight ====================

MAZ_boundary_highlight <- function(MAZ,
                                   MAZ_pick) {
  MAZ_i <- filter(MAZ, MAZ_Acrony == MAZ_pick)

  p <- ggplot() +
    # annotation_map_tile(type = "cartolight") +
    geom_sf(data = MAZ, color = "black", alpha = 0.3) +
    geom_sf(data = MAZ_i, fill = "green")

  return(p)
}


# ==================== 13. Testing / Diagnostic Plots (Commented) ====================
# 
# cu_i <- "CO-47"
# 
# #cu_i <- cu_run$FULL_CU_IN[i]
# cu_run_i <- cu_run[cu_run$FULL_CU_IN == cu_i, ]
# sp_pick <- cu_run$spp[cu_run$FULL_CU_IN == cu_i] # species abbr
# sp_pick_bcfp <- spp_lookup$spp_abr_bcfp[spp_lookup$spp_abr == sp_pick]
# 
# cu_timing_i <- cu_timing_Fr %>% filter(FULL_CU_IN == cu_i)
# 
# cu_boundary_i <- cu_boundary[cu_boundary$FULL_CU_IN == cu_i, ]
# 
# #Lakes within CU boundary (for plotting)
# temp <- unlist(st_intersects(cu_boundary_i, lakes_Fr))
# lakes_cu <- lakes_Fr[temp,]
# 
# # # subset nuseds observations
# # nuseds_cu <- nuseds_Fr[nuseds_Fr$FULL_CU_IN == cu_i, ]
# #
# # subset migration path
# migr_cu <- migr_list[[cu_i]]
# 
# stream_cu_sub <- stream_cu_picks[, colnames(stream_cu_picks) == cu_i]
# 
# fwModels_cu <- fw_models[stream_cu_sub, ] %>%
#   rename(keep_model_spawning = contains(paste0("model_spawning_", sp_pick_bcfp)),
#     keep_model_rearing  = contains(paste0("model_rearing_", sp_pick_bcfp))) %>%
#   mutate(model_rs = if_any(starts_with("keep_model"), ~ . == TRUE)) %>% # get boolean for model spawning and rearing
#   mutate(model_rs = factor(model_rs, levels = c(TRUE, FALSE),
#     labels = c("1-SPAWNING/REARING", "2-ACCESSIBLE"))) %>%
#   select(-starts_with(c("model_spawning", "model_rearing", "known_rearing", "known_spawning")))
# 
# 
# cu_timing_long_i <- cu_timing_long[cu_timing_long$FULL_CU_IN == cu_i, ]
# cu_timing_i <- cu_timing_Fr[cu_timing_Fr$FULL_CU_IN == cu_i, ]
# 
# 
# # stream_accessible_plot(fwModels_cu,
# #                        lakes_cu,
# #   nuseds_cu,
# #   cu_boundary_i)
# 
# stream_indicator_plot(fwModels_cu,
#   cu_boundary_i,
#   lakes_cu,
#   variable = "tw8_9_45_3",
#   #plot_title = "Change in August flow - 2041-2060",
#   unit_label = "°C",
#   scico_palette = "roma",
#   palette_direction = -1)

#
# # migr_UFR <- filter(migr_cu, watershed_group_code == "UFRA")
# #
# # migration_path_timing_plot(migr_cu, nuseds_cu, cu_boundary_i, migr_daily_all, cu_i, cu_timing_i)
# # # #
# # cu_timing_plot(cu_timing_long_i)
# #
# migrT_cu <- migrT_rcps[["45"]][[cu_i]]
