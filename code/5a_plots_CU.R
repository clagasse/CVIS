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
#   1. plot_timing_comparison()         - Life-history stage timing comparison plot.
#   2. stream_accessible_plot()         - Maps accessible streams and NUSEDS sites.
#   3. stream_indicator_multipanel_plot()- Side-by-side stream network panels for indicators.
#   4. migration_path_timing_plot()     - Combined geographic migration route map and projected mainstem temperatures.
#   5. cu_boundary_highlight()          - Locator map highlighting a single CU.
#   6. cu_hydrologic_regime()           - Maps CU boundaries with flow gauges and regimes.
#   7. fraser_hydrologic_regime_comparison_plot() - Hydrologic regime comparisons across Fraser basin.
#   8. abundance_status_plot()          - Timeline of wild spawner abundance and WSP status.
#   9. MAZ_boundary_highlight()         - Locator map highlighting a single MAZ.
#   10. plot_cu_vulnerability_summary() - Individual CU-level vulnerability dashboard.
#   11. plot_cu_sensitivity_scores()    - Individual CU-level sensitivity scores across variation sources.
#   12. plot_cu_sensitivity_indicators()- Individual CU-level sensitivity to indicators.
#
# Dependencies:
#   - Requires ggplot2, sf, scico, patchwork, and standard CVIS data inputs.
# ==============================================================================

# Helper function for geometry simplification to reduce HTML file sizes and rendering times
library(pacea)

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
    species_palette <- get("species_palette", envir = .GlobalEnv)
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
    select(FULL_CU_IN, CVIS_LABEL, SPECIES_NAME) %>%
    distinct()
  
  if (!is.null(selected_cu) && !selected_cu %in% cu_labels_df$FULL_CU_IN) {
    selected_row <- cu_timing_long %>% 
      filter(FULL_CU_IN == selected_cu) %>%
      select(FULL_CU_IN, CVIS_LABEL, SPECIES_NAME) %>% 
      distinct()
    if (nrow(selected_row) > 0) {
      cu_labels_df <- bind_rows(cu_labels_df, selected_row) %>% distinct()
    }
  }

  # Define label sizes based on selected_cu mode and n_cus
  n_cus <- length(unique(df_plot$FULL_CU_IN))
  y_text_size_val <- if (!is.null(y_text_size)) {
    y_text_size
  } else {
    if (n_cus > 40) 6.5 else if (n_cus > 20) 8 else 9
  }
  default_size <- if (!is.null(selected_cu)) "6.5pt" else "8pt"
  y_sz <- if (!is.null(y_text_size)) paste0(y_text_size, "pt") else default_size

  cu_labels_df <- cu_labels_df %>%
    mutate(
      color_hex = species_palette[SPECIES_NAME],
      color_hex = if_else(is.na(color_hex), "#4B5563", color_hex),
      label_clean = CVIS_LABEL # Omit (FULL_CU_IN) to match indicator_cu_tile_plot
    )

  if (!is.null(selected_cu)) {
    cu_labels_df <- cu_labels_df %>%
      mutate(
        label_formatted = if_else(
          FULL_CU_IN == selected_cu,
          paste0("<strong>➔ <span style='color:", color_hex, "'>", label_clean, "</span></strong>"),
          paste0("<span style='color:", color_hex, "'>", label_clean, "</span>")
        )
      )
  } else {
    cu_labels_df <- cu_labels_df %>%
      mutate(
        label_formatted = paste0("<span style='color:", color_hex, "'>", label_clean, "</span>")
      )
  }

  # Sort CUs
  if (sort_by == "species") {
    cu_labels_ordered <- cu_labels_df %>%
      arrange(desc(SPECIES_NAME), desc(CVIS_LABEL))
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
    left_join(select(cu_labels_df, FULL_CU_IN, label_clean, label_formatted, color_hex), by = "FULL_CU_IN")
  
  levels_ordered <- unique(cu_labels_ordered$label_formatted)
  header_y_label <- "<b>CU</b>"
  levels_ordered <- c(levels_ordered, header_y_label)
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

  # Add a dummy row to ensure all Data Quality levels exist in the dataset so the legend renders completely
  if (nrow(df_plot) > 0) {
    dummy_row <- df_plot[1, ]
    dummy_row$dat_qual_cat <- factor("Unknown", levels = c("High (1-2)", "Medium (3-4)", "Low (5-6)", "Unknown"))
    dummy_row$date_start <- as.Date(NA)
    dummy_row$date_end <- as.Date(NA)
    dummy_row$date_peak <- as.Date(NA)
    dummy_row$start <- NA
    dummy_row$end <- NA
    dummy_row$peak <- NA
    dummy_row$FULL_CU_IN <- "DUMMY_UNKNOWN_LEGEND"
    df_plot <- bind_rows(df_plot, dummy_row)
  }

  # 6. Define colors for life stages (richer, darker colors)
  if (is.null(life_stage_palette)) {
    life_stage_palette <- c(
      "Spawning" = "#1b7837",
      "Upstream Run Timing" = "#b33d00",
      "Ocean Entry" = "#1f4e79",
      "Juvenile FW Migration" = "#7b3294"
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

  # Zoom x-axis
  xlim_start_date <- as.Date("2000-01-01")
  xlim_end_date <- as.Date("2000-12-31")
  
  if (zoom_to_selected && !is.null(sel_timing) && nrow(sel_timing) > 0) {
    pad_start <- min(sel_timing$date_start, na.rm = TRUE)
    pad_end <- max(sel_timing$date_end, na.rm = TRUE)
    if (!is.na(pad_start) && !is.na(pad_end)) {
      xlim_start_date <- pad_start - 30
      xlim_end_date <- pad_end + 30
      if (xlim_start_date < as.Date("2000-01-01")) xlim_start_date <- as.Date("2000-01-01")
      if (xlim_end_date > as.Date("2000-12-31")) xlim_end_date <- as.Date("2000-12-31")
    }
  }

  # Draw thin background connecting lines for each CU
  df_connectors <- df_plot %>%
    filter(!is.na(date_start), !is.na(date_end)) %>%
    group_by(y_axis_factor) %>%
    summarise(
      conn_start = min(date_start, na.rm = TRUE),
      conn_end = max(date_end, na.rm = TRUE),
      .groups = "drop"
    )
  
  p <- p + geom_segment(
    data = df_connectors,
    aes(x = conn_start, xend = conn_end, y = y_axis_factor, yend = y_axis_factor),
    color = "grey80", linetype = "dotted", linewidth = 0.5, inherit.aes = FALSE
  )

  # Split background and selected CU datasets safely
  if (!is.null(selected_cu)) {
    df_bg <- filter(df_plot, FULL_CU_IN != selected_cu)
    df_sel <- filter(df_plot, FULL_CU_IN == selected_cu)
  } else {
    df_bg <- df_plot
    df_sel <- filter(df_plot, FALSE)
  }

  # Draw segments representing life stage durations
  # Background CUs
  p <- p + 
    geom_segment(
      data = filter(df_bg, life_stage == "run_timing"),
      aes(x = date_start, xend = date_end, yend = y_axis_factor, color = life_stage_label, alpha = dat_qual_cat),
      linewidth = 5.0
    ) +
    geom_segment(
      data = filter(df_bg, life_stage == "ocean_entry"),
      aes(x = date_start, xend = date_end, yend = y_axis_factor, color = life_stage_label, alpha = dat_qual_cat),
      linewidth = 4.2
    ) +
    geom_segment(
      data = filter(df_bg, life_stage == "spawning"),
      aes(x = date_start, xend = date_end, yend = y_axis_factor, color = life_stage_label, alpha = dat_qual_cat),
      linewidth = 3.5
    ) +
    geom_segment(
      data = filter(df_bg, life_stage == "freshwater_migration"),
      aes(x = date_start, xend = date_end, yend = y_axis_factor, color = life_stage_label, alpha = dat_qual_cat),
      linewidth = 2.8
    )

  # Selected CU
  p <- p + 
    geom_segment(
      data = filter(df_sel, life_stage == "run_timing"),
      aes(x = date_start, xend = date_end, yend = y_axis_factor, color = life_stage_label, alpha = dat_qual_cat),
      linewidth = 8.5
    ) +
    geom_segment(
      data = filter(df_sel, life_stage == "ocean_entry"),
      aes(x = date_start, xend = date_end, yend = y_axis_factor, color = life_stage_label, alpha = dat_qual_cat),
      linewidth = 7.5
    ) +
    geom_segment(
      data = filter(df_sel, life_stage == "spawning"),
      aes(x = date_start, xend = date_end, yend = y_axis_factor, color = life_stage_label, alpha = dat_qual_cat),
      linewidth = 6.5
    ) +
    geom_segment(
      data = filter(df_sel, life_stage == "freshwater_migration"),
      aes(x = date_start, xend = date_end, yend = y_axis_factor, color = life_stage_label, alpha = dat_qual_cat),
      linewidth = 5.5
    )

  # Draw peak points if show_peaks is TRUE
  if (show_peaks) {
    p <- p + geom_point(
      data = df_bg,
      aes(x = date_peak, fill = life_stage_label, shape = dat_qual_cat),
      color = "grey30", size = 1.4, stroke = 0.3
    ) + geom_point(
      data = df_sel,
      aes(x = date_peak, fill = life_stage_label, shape = dat_qual_cat),
      color = "black", size = 4.0, stroke = 1.2
    )
  }

  # Add FW residency text on the far right
  x_fwres <- xlim_end_date + 0.035 * as.numeric(xlim_end_date - xlim_start_date)
  
  if (!"fwres_mean" %in% names(df_plot)) {
    df_plot$fwres_mean <- NA
  }

  df_fwres <- df_plot %>%
    select(y_axis_factor, fwres_mean, FULL_CU_IN, color_hex) %>%
    distinct() %>%
    mutate(
      fwres_label = if_else(is.na(fwres_mean), "N/A", sprintf("%.0f d", fwres_mean))
    )
  
  sel_cu_val <- if (is.null(selected_cu)) "" else selected_cu
  df_fwres <- df_fwres %>%
    mutate(
      fwres_formatted = if_else(
        FULL_CU_IN == sel_cu_val,
        paste0("<span style='color:", color_hex, "; font-size:9.5pt;'><b>▶ ", fwres_label, "</b></span>"),
        paste0("<span style='color:grey40; font-size:", default_size, ";'>", fwres_label, "</span>")
      )
    )

  # Add header row for the Freshwater Residency column on the right
  df_fwres <- df_fwres %>%
    add_row(
      y_axis_factor = factor(header_y_label, levels = levels_ordered),
      fwres_mean = NA,
      FULL_CU_IN = "HEADER",
      color_hex = "#000000",
      fwres_label = "<b>FW Res</b>",
      fwres_formatted = "<b>FW Res</b>"
    )

  p <- p + ggtext::geom_richtext(
    data = df_fwres,
    aes(x = x_fwres, y = y_axis_factor, label = fwres_formatted),
    hjust = 0,
    fill = NA,
    label.color = NA,
    label.padding = grid::unit(0, "lines"),
    inherit.aes = FALSE
  ) + 
    scale_y_discrete(limits = levels_ordered, expand = c(0.02, 0)) +
    coord_cartesian(xlim = c(xlim_start_date, xlim_end_date), clip = "off")

  # Scales and Guides (Combining scales to merge the Data Quality legend!)
  p <- p +
    scale_color_manual(values = life_stage_palette, name = "Life Stage") +
    scale_fill_manual(values = life_stage_palette, name = "Life Stage") +
    scale_shape_manual(
      name = "Data Quality",
      values = c(
        "High (1-2)" = 21,
        "Medium (3-4)" = 24,
        "Low (5-6)" = 22,
        "Unknown" = 23
      ),
      drop = FALSE
    ) +
    scale_alpha_manual(
      name = "Data Quality",
      values = c(
        "High (1-2)" = 0.95,
        "Medium (3-4)" = 0.80,
        "Low (5-6)" = 0.60,
        "Unknown" = 0.40
      ),
      drop = FALSE
    ) +
    guides(
      color = guide_legend(order = 1),
      fill = "none",
      shape = guide_legend(order = 2, override.aes = list(
        size = 3.5, 
        shape = c(21, 24, 22, 23),
        color = "grey30",
        fill = "grey40", 
        stroke = 1.0, 
        alpha = c(1.0, 1.0, 1.0, 1.0)
      )),
      alpha = "none"
    )

  if (show_indicator_periods) {
    p <- p + geom_rect(
      aes(xmin = as.Date("2000-08-01"), xmax = as.Date("2000-08-31"), ymin = -Inf, ymax = Inf),
      fill = "#FFE0B2", alpha = 0.08, inherit.aes = FALSE
    )
  }

  p <- p +
    scale_x_date(
      date_breaks = date_breaks,
      date_labels = "%b",
      expand = c(0.01, 0)
    ) +
    labs(
      x = "Date",
      y = NULL
    ) +
    theme_cvis() +
    theme(
      axis.text.x = element_text(size = 9, angle = if (is.null(selected_cu)) 45 else 0, hjust = if (is.null(selected_cu)) 1 else 0.5),
      axis.text.y = ggtext::element_markdown(size = y_text_size_val, lineheight = 0.8),
      axis.text.y.left = ggtext::element_markdown(size = y_text_size_val, lineheight = 0.8),
      axis.title.x = element_text(size = 10, face = "bold"),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = if (is.null(selected_cu)) "right" else "bottom",
      legend.box = if (is.null(selected_cu)) "vertical" else "horizontal",
      legend.margin = margin(t = 0),
      legend.title = element_text(size = 9, face = "bold"),
      legend.text = element_text(size = 8.5),
      plot.title = element_text(face = "bold", size = 12),
      plot.subtitle = element_text(size = 9.5, face = "italic", color = "grey30"),
      plot.caption = element_text(size = 8, color = "grey50", hjust = 0),
      plot.margin = margin(5, 80, 5, 5)
    )

  return(p)
}





# ==================== 2. Stream Network Accessible Plot ====================
# input subset of stream network, nuseds data, and CU boundary for a specific CU
stream_accessible_plot <- function(stream_data,
                                   nuseds_data,
                                   cu_boundary,
                                   lakes_cu,
                                   variable = "elevation",
                                   xlim = NA,
                                   unit_label = "Elevation (m)",
                                   plot_title = "") {

  # Simplify geometries if needed
  if (exists("cu_boundary_i", envir = .GlobalEnv)) {
    cu_boundary_i <- simplify_geom_if_needed(get("cu_boundary_i", envir = .GlobalEnv))
  } else {
    cu_boundary_i <- simplify_geom_if_needed(cu_boundary)
  }
  lakes_cu <- simplify_geom_if_needed(lakes_cu)
  stream_data <- simplify_geom_if_needed(stream_data)

  # Check if nuseds_cu or nuseds_data exists. Use nuseds_data first, fallback to nuseds_cu
  if (is.null(nuseds_data) || !inherits(nuseds_data, "sf")) {
    if (exists("nuseds_cu", envir = .GlobalEnv)) {
      nuseds_data <- get("nuseds_cu", envir = .GlobalEnv)
    }
  }

  # Ensure model_rs is converted to a factor with levels 2-ACCESSIBLE and 1-SPAWNING/REARING
  if ("model_rs" %in% names(stream_data)) {
    if (is.logical(stream_data$model_rs)) {
      stream_data$model_rs <- factor(stream_data$model_rs, levels = c(TRUE, FALSE),
                                     labels = c("1-SPAWNING/REARING", "2-ACCESSIBLE"))
    }
    stream_data$model_rs <- factor(stream_data$model_rs, levels = c("2-ACCESSIBLE", "1-SPAWNING/REARING"))
  }

  # Filter stream segments to those that intersect with the CU boundary polygon
  cu_boundary_union <- sf::st_union(cu_boundary_i)
  intersects_mask <- sf::st_intersects(stream_data, cu_boundary_union, sparse = FALSE)[, 1]
  if (any(intersects_mask)) {
    stream_data <- stream_data[intersects_mask, ]
  }

  ## stream map
  p1 <- ggplot() +
    geom_sf(data = cu_boundary_i, color = "black", alpha = 0.05)

  if (nrow(lakes_cu) > 0) {
    p1 <- p1 + geom_sf(data = lakes_cu, color = "darkgrey", alpha = 0.8)
  }

  p1 <- p1 +
    geom_sf(data = st_zm(stream_data), aes(color = model_rs), linewidth = 1.0, show.legend = TRUE) +
    scale_color_manual(values = c(
      "2-ACCESSIBLE" = "lightgreen",
      "1-SPAWNING/REARING" = "forestgreen"
    ))

  if (!is.null(nuseds_data) && nrow(nuseds_data) > 0) {
    p1 <- p1 + geom_sf(data = nuseds_data, aes(fill = SPECIES), size = 2., alpha = 0.7, show.legend = FALSE)
  }

  p1 <- p1 +
    coord_sf(
      xlim = st_bbox(cu_boundary_i)[c(1, 3)],
      ylim = st_bbox(cu_boundary_i)[c(2, 4)],
      datum = NA
    ) +
    labs(
      subtitle = plot_title,
      color = "Habitat Potential"
    ) +
    guides(fill = "none") +
    theme(
      legend.key.size = unit(0.3, "cm"),
      legend.text = element_text(size = 7.5),
      legend.title = element_text(size = 8.5),
      legend.position = "right",
      legend.background = element_rect(fill = "white", color = "grey90", linewidth = 0.2)
    )

  return(p1)
}


# ==================== 3. Stream Indicator Map & Histogram ====================

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
                                             cu_boundary = NULL,
                                             lakes_cu = NULL,
                                             variables = c("CT_anad"),
                                             plot_titles = NULL,
                                             unit_labels = NULL,
                                             risk_palette = cvis_risk_palette,
                                             palette_directions = 1,
                                             ncol = NULL,
                                             nrow = NULL) {
  # If no cu_boundary is supplied, default to the entire area (e.g. Fraser Basin or bbox of fwModels)
  if (is.null(cu_boundary)) {
    if (exists("Fr_basin", envir = .GlobalEnv)) {
      cu_boundary <- get("Fr_basin", envir = .GlobalEnv)
    } else {
      # Fallback to the bounding box of the stream data
      cu_boundary <- sf::st_as_sf(sf::st_as_sfc(sf::st_bbox(fwModels)))
    }
  }

  # Simplify geometries if needed
  fwModels <- simplify_geom_if_needed(fwModels)
  cu_boundary <- simplify_geom_if_needed(cu_boundary)
  lakes_cu <- simplify_geom_if_needed(lakes_cu)

  # Dynamically determine ncol if not provided
  if (is.null(ncol)) {
    ncol <- if (length(variables) %% 3 == 0) 3 else 4
  }

  # Check if stream_order exists in fwModels and make sure it is numeric
  if ("stream_order" %in% names(fwModels)) {
    fwModels$stream_order <- as.numeric(fwModels$stream_order)
  }

  # Dynamically calculate elevation from Z coordinates of geometries if not present and if requested
  if ("elevation" %in% variables && !"elevation" %in% names(fwModels)) {
    fwModels$elevation <- sapply(sf::st_geometry(fwModels), function(geom) {
      coords <- sf::st_coordinates(geom)
      if ("Z" %in% colnames(coords)) {
        mean(coords[, "Z"], na.rm = TRUE)
      } else {
        NA_real_
      }
    })
  }

  # Determine best corner for legend inset based on quadrant area intersection
  best_quad <- "BL" # Default fallback
  tryCatch({
    bbox <- sf::st_bbox(cu_boundary)
    xmin <- bbox[["xmin"]]
    ymin <- bbox[["ymin"]]
    xmax <- bbox[["xmax"]]
    ymax <- bbox[["ymax"]]
    
    xmid <- (xmin + xmax) / 2
    ymid <- (ymin + ymax) / 2
    crs_cu <- sf::st_crs(cu_boundary)
    
    # Helper function to create polygon for a quadrant
    make_quad_poly <- function(x1, y1, x2, y2, crs) {
      sf::st_sfc(sf::st_polygon(list(matrix(c(
        x1, y1,
        x2, y1,
        x2, y2,
        x1, y2,
        x1, y1
      ), ncol = 2, byrow = TRUE))), crs = crs)
    }
    
    poly_bl <- make_quad_poly(xmin, ymin, xmid, ymid, crs_cu)
    poly_br <- make_quad_poly(xmid, ymin, xmax, ymid, crs_cu)
    poly_tl <- make_quad_poly(xmin, ymid, xmid, ymax, crs_cu)
    poly_tr <- make_quad_poly(xmid, ymid, xmax, ymax, crs_cu)
    
    cu_geom <- sf::st_make_valid(sf::st_union(cu_boundary))
    
    area_bl <- as.numeric(sf::st_area(sf::st_intersection(cu_geom, poly_bl)))
    area_br <- as.numeric(sf::st_area(sf::st_intersection(cu_geom, poly_br)))
    area_tl <- as.numeric(sf::st_area(sf::st_intersection(cu_geom, poly_tl)))
    area_tr <- as.numeric(sf::st_area(sf::st_intersection(cu_geom, poly_tr)))
    
    area_bl <- if (length(area_bl) == 1 && !is.na(area_bl)) area_bl else Inf
    area_br <- if (length(area_br) == 1 && !is.na(area_br)) area_br else Inf
    area_tl <- if (length(area_tl) == 1 && !is.na(area_tl)) area_tl else Inf
    area_tr <- if (length(area_tr) == 1 && !is.na(area_tr)) area_tr else Inf
    
    areas <- c(BL = area_bl, BR = area_br, TL = area_tl, TR = area_tr)
    best_quad <- names(which.min(areas))
  }, error = function(e) {
    # Fallback to centroid logic if st_intersection fails
    tryCatch({
      cu_geom <- sf::st_make_valid(sf::st_union(cu_boundary))
      centroid <- sf::st_coordinates(sf::st_centroid(cu_geom))
      bbox <- sf::st_bbox(cu_boundary)
      xmid <- (bbox[["xmin"]] + bbox[["xmax"]]) / 2
      ymid <- (bbox[["ymin"]] + bbox[["ymax"]]) / 2
      
      lr <- if (centroid[1] > xmid) "L" else "R"
      tb <- if (centroid[2] > ymid) "B" else "T"
      best_quad <<- paste0(tb, lr)
    }, error = function(e2) {
      best_quad <<- "BL" # Hard fallback
    })
  })
  
  # Assign inset coordinates based on best quadrant
  inset_coords <- switch(best_quad,
    "BL" = list(left = 0.03, bottom = 0.03, right = 0.38, top = 0.32),
    "BR" = list(left = 0.62, bottom = 0.03, right = 0.97, top = 0.32),
    "TL" = list(left = 0.03, bottom = 0.68, right = 0.38, top = 0.97),
    "TR" = list(left = 0.62, bottom = 0.68, right = 0.97, top = 0.97),
    list(left = 0.03, bottom = 0.03, right = 0.38, top = 0.32) # default
  )

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
    if (var_name == "stream_order" && p_title == "stream_order") p_title <- "Stream Order"
    if (var_name == "elevation" && p_title == "elevation") p_title <- "Elevation"

    u_label <- get_param_by_name(unit_labels, var_name, i, NULL)
    if (var_name == "stream_order" && is.null(u_label)) u_label <- "Order"
    if (var_name == "elevation" && is.null(u_label)) u_label <- "Elevation (m)"
    
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
    if (var_name == "stream_order") p_palette <- "Blues"
    if (var_name == "elevation") p_palette <- "YlGn"
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
    is_discrete <- is.character(vals) || is.factor(vals) || is.logical(vals)
    
    if (is_discrete) {
      panel_data[[var_name]] <- as.factor(vals)
      if (var_name == "model_access_salmon") {
        p_palette <- "Set2"
      }
    } else {
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
    }
    
    if ("stream_order" %in% variables && "stream_order" %in% names(panel_data) && !all(is.na(panel_data$stream_order))) {
      p <- ggplot() +
        geom_sf(data = panel_data, aes(color = !!var_sym, linewidth = stream_order)) +
        scale_linewidth_continuous(range = c(0.2, 1.2), guide = "none")
    } else {
      p <- ggplot() +
        geom_sf(data = panel_data, aes(color = !!var_sym))
    }
    
    if (is_discrete) {
      p <- p +
        scale_color_brewer(palette = p_palette, na.value = "grey95", guide = "none") +
        geom_sf(data = cu_boundary, color = "black", alpha = 0.05)
    } else {
      p <- p +
        scale_color_cvis(
          palette = p_palette,
          direction = p_dir,
          limits = val_range,
          guide = "none",
          oob = scales::squish
        ) +
        geom_sf(data = cu_boundary, color = "black", alpha = 0.05)
    }
      
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

    # Inset histogram/bar chart for distribution of values across stream segments
    if (is_discrete) {
      p_hist <- ggplot(st_drop_geometry(panel_data), aes(x = !!var_sym)) +
        geom_bar(aes(fill = !!var_sym), color = "white", linewidth = 0.1, show.legend = FALSE, na.rm = TRUE) +
        scale_fill_brewer(palette = p_palette, na.value = "grey95") +
        labs(x = u_label) +
        theme_void() +
        theme(
          axis.line.x = element_line(color = "black", linewidth = 0.5),
          axis.text.x = element_text(color = "black", size = 5, face = "bold", angle = 30, hjust = 1, margin = margin(t = 2)),
          axis.ticks.x = element_line(color = "black", linewidth = 0.5),
          axis.title.x = if (!is.null(u_label)) element_text(color = "black", size = 6.5, face = "bold", margin = margin(t = 6)) else element_blank(),
          plot.background = element_rect(fill = "white", color = NA),
          plot.margin = margin(1, 2, 12, 2)
        )
    } else {
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
    }

    p_combined <- p + patchwork::inset_element(
      p_hist,
      left = inset_coords$left,
      bottom = inset_coords$bottom,
      right = inset_coords$right,
      top = inset_coords$top,
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
      theme_cvis() +
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

    # Species palette
    spp_colors <- get("species_palette", envir = .GlobalEnv)

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


# ==================== 7b. Fraser Hydrologic Regimes & Flow Change Comparison ====================
# Compares hydrologic regimes with change in August flow & Winter flow across the Fraser Basin
fraser_hydrologic_regime_comparison_plot <- function(watershed_flow = NULL,
                                                     stream_data = NULL,
                                                     lakes_df = NULL,
                                                     fraser_boundary = NULL,
                                                     variable_aug = "flow8pdelta_9_45_3",
                                                     variable_win = "flow18pdelta_9_45_3",
                                                     xlim_aug = c(-1, 0),
                                                     xlim_win = c(-0.5, 1),
                                                     unit_label = "Proportional Change",
                                                     risk_palette = cvis_risk_palette,
                                                     palette_direction = 1,
                                                     inset_quad = "BL") {
  # 1. Load data from global environment if not provided, or load from file
  if (is.null(watershed_flow)) {
    if (exists("watershed_flow", envir = .GlobalEnv)) {
      watershed_flow <- get("watershed_flow", envir = .GlobalEnv)
    } else {
      temp_env <- new.env()
      fgd_file <- file.path(paths$fw, "flow_gauge_data.Rdata")
      if (file.exists(fgd_file)) {
        load(fgd_file, envir = temp_env)
        watershed_flow <- temp_env$watershed_flow
      } else if (file.exists("D:/Streamflow/station_data/catchment_polygons.gpkg")) {
        watershed_flow <- st_read("D:/Streamflow/station_data/catchment_polygons.gpkg", quiet = TRUE) %>%
          rename(ID = gauge_id) %>%
          mutate(regime = factor("Snowfall"))
      }
    }
  }
  if (is.null(stream_data) && exists("fw_sp_ind", envir = .GlobalEnv)) {
    stream_data <- get("fw_sp_ind", envir = .GlobalEnv)
  }
  if (is.null(fraser_boundary) && exists("Fr_basin", envir = .GlobalEnv)) {
    fraser_boundary <- get("Fr_basin", envir = .GlobalEnv)
  }
  if (is.null(lakes_df) && exists("lakes_Fr", envir = .GlobalEnv)) {
    lakes_df <- get("lakes_Fr", envir = .GlobalEnv)
  }

  # Ensure datasets are present
  if (is.null(watershed_flow) || is.null(stream_data) || is.null(fraser_boundary)) {
    stop("Required datasets (watershed_flow, stream_data, fraser_boundary) could not be found.")
  }

  # Simplify geometries if needed
  fraser_boundary <- simplify_geom_if_needed(fraser_boundary)
  watershed_flow <- simplify_geom_if_needed(watershed_flow)
  stream_data <- simplify_geom_if_needed(stream_data)
  if (!is.null(lakes_df)) lakes_df <- simplify_geom_if_needed(lakes_df)

  # Transform CRS and crop watershed_flow to Fraser boundary for focused display
  if (sf::st_crs(watershed_flow) != sf::st_crs(fraser_boundary)) {
    watershed_flow <- sf::st_transform(watershed_flow, sf::st_crs(fraser_boundary))
  }
  watershed_flow_valid <- tryCatch(sf::st_make_valid(watershed_flow), error = function(e) watershed_flow)
  fraser_bbox <- sf::st_bbox(fraser_boundary)
  watershed_flow_cropped <- suppressWarnings(sf::st_crop(watershed_flow_valid, fraser_bbox))

  # Determine inset coordinates based on quadrant
  inset_coords <- switch(inset_quad,
    "BL" = list(left = 0.03, bottom = 0.03, right = 0.38, top = 0.32),
    "BR" = list(left = 0.62, bottom = 0.03, right = 0.97, top = 0.32),
    "TL" = list(left = 0.03, bottom = 0.68, right = 0.38, top = 0.97),
    "TR" = list(left = 0.62, bottom = 0.68, right = 0.97, top = 0.97),
    list(left = 0.03, bottom = 0.03, right = 0.38, top = 0.32)
  )

  # ==================== PANEL A: Hydrologic Regimes (Cropped to Fraser) ====================
  p_regime <- ggplot() +
    geom_sf(data = fraser_boundary, color = "black", fill = "grey95", linewidth = 0.6) +
    geom_sf(data = watershed_flow_cropped, aes(fill = regime), alpha = 0.65, color = NA) +
    scale_fill_brewer(palette = "Set2", guide = "none") +
    coord_sf(datum = NA, xlim = fraser_bbox[c(1,3)], ylim = fraser_bbox[c(2,4)]) +
    labs(title = "Hydrologic Regimes") +
    theme_void() +
    theme(
      plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
      plot.margin = margin(3, 3, 3, 3)
    )

  regime_counts <- as.data.frame(sf::st_drop_geometry(watershed_flow_cropped)) %>%
    filter(!is.na(regime)) %>%
    group_by(regime) %>%
    summarise(count = n(), .groups = "drop") %>%
    mutate(prop = count / sum(count))

  p_regime_hist <- ggplot(regime_counts, aes(x = regime, y = prop, fill = regime)) +
    geom_bar(stat = "identity", color = "white", linewidth = 0.1, show.legend = FALSE) +
    scale_fill_brewer(palette = "Set2") +
    scale_y_continuous(labels = function(y) sprintf("%.0f%%", y * 100)) +
    labs(x = NULL, y = "Proportion") +
    theme_void() +
    theme(
      axis.line.y = element_line(color = "black", linewidth = 0.5),
      axis.text.y = element_text(color = "black", size = 6, face = "bold"),
      axis.text.x = element_text(color = "black", size = 6, face = "bold", angle = 45, hjust = 1),
      axis.ticks.y = element_line(color = "black", linewidth = 0.5),
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(1, 2, 8, 2)
    )

  p_regime_combined <- p_regime + patchwork::inset_element(
    p_regime_hist, left = inset_coords$left, bottom = inset_coords$bottom,
    right = inset_coords$right, top = inset_coords$top, align_to = "panel"
  )

  # ==================== PANEL B: August Flow (Station Model) ====================
  if (!exists("wp_vm", envir = .GlobalEnv)) {
    load(file.path(paths$fw, "Statistical_flow_projections.Rds"), envir = .GlobalEnv)
  }
  wp_vm <- get("wp_vm", envir = .GlobalEnv)

  if (exists("stations_flow", envir = .GlobalEnv)) {
    stations_flow <- get("stations_flow", envir = .GlobalEnv)
  } else {
    temp_env <- new.env()
    load(file.path(paths$fw, "flow_gauge_data.Rdata"), envir = temp_env)
    stations_flow <- temp_env$stations_flow
  }

  stations_flow <- simplify_geom_if_needed(stations_flow)
  if (sf::st_crs(stations_flow) != sf::st_crs(fraser_boundary)) {
    stations_flow <- sf::st_transform(stations_flow, sf::st_crs(fraser_boundary))
  }
  stations_flow_cropped <- suppressWarnings(sf::st_crop(stations_flow, fraser_bbox))

  rcp_val <- "45"
  period_val <- "3"
  m_var <- stringr::str_match(variable_aug, "^flow8pdelta_\\d+_(\\d+)_(\\d)$")
  if (!is.na(m_var[1, 1])) {
    rcp_val <- m_var[1, 2]
    period_val <- m_var[1, 3]
  }

  wp_flat_aug <- wp_vm %>%
    dplyr::mutate(mean_val = sapply(data, function(df) {
      if (is.null(df) || nrow(df) == 0) return(NA_real_)
      mean(df$mean, na.rm = TRUE)
    })) %>%
    dplyr::select(ID, rcp, gcm_name, period, mean = mean_val)

  wp_hist_aug <- wp_flat_aug %>%
    dplyr::filter(period == "0") %>%
    dplyr::group_by(ID, gcm_name) %>%
    dplyr::summarise(mean_hist = mean(mean, na.rm = TRUE), .groups = "drop")

  wp_future_aug <- wp_flat_aug %>% dplyr::filter(rcp == rcp_val, period == period_val)

  wp_delta_aug <- wp_future_aug %>%
    dplyr::left_join(wp_hist_aug %>% dplyr::select(ID, mean_hist), by = "ID") %>%
    dplyr::mutate(delta = (mean - mean_hist) / mean_hist) %>%
    dplyr::group_by(ID) %>%
    dplyr::summarise(value = mean(delta, na.rm = TRUE), .groups = "drop")

  watershed_flow_aug <- watershed_flow_cropped %>%
    dplyr::left_join(wp_delta_aug, by = "ID")

  p_stn_aug <- ggplot() +
    geom_sf(data = fraser_boundary, color = "black", fill = "grey95", linewidth = 0.6) +
    geom_sf(data = watershed_flow_aug, aes(fill = value), alpha = 0.65, color = NA) +
    scale_fill_cvis(palette = risk_palette, direction = palette_direction, limits = xlim_aug, guide = "none", oob = scales::squish) +
    geom_sf(data = stations_flow_cropped, color = "black", size = 1) +
    coord_sf(datum = NA, xlim = fraser_bbox[c(1,3)], ylim = fraser_bbox[c(2,4)]) +
    labs(title = "August Flow (Station model)") +
    theme_void() +
    theme(plot.title = element_text(size = 11, face = "bold", hjust = 0.5), plot.margin = margin(3, 3, 3, 3))

  p_stn_aug_hist <- ggplot(sf::st_drop_geometry(watershed_flow_aug), aes(x = value)) +
    geom_histogram(aes(fill = after_stat(x)), bins = 15, color = "white", linewidth = 0.1, show.legend = FALSE, na.rm = TRUE) +
    scale_fill_cvis(palette = risk_palette, direction = palette_direction, limits = xlim_aug, oob = scales::squish) +
    scale_x_continuous(limits = xlim_aug, oob = scales::squish, breaks = c(xlim_aug[1], (xlim_aug[1] + xlim_aug[2])/2, xlim_aug[2]), labels = function(x) sprintf("%.1f", x)) +
    labs(x = unit_label) +
    theme_void() +
    theme(
      axis.line.x = element_line(color = "black", linewidth = 0.5),
      axis.text.x = element_text(color = "black", size = 6.5, face = "bold", margin = margin(t = 4)),
      axis.ticks.x = element_line(color = "black", linewidth = 0.5),
      axis.title.x = element_text(color = "black", size = 6, face = "bold", margin = margin(t = 4)),
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(1, 2, 10, 2)
    )

  p_stn_aug_combined <- p_stn_aug + patchwork::inset_element(p_stn_aug_hist, left = inset_coords$left, bottom = inset_coords$bottom, right = inset_coords$right, top = inset_coords$top, align_to = "panel")

  # ==================== PANEL C: August Flow (Streamdyn Model) ====================
  if (variable_aug %in% names(stream_data)) {
    panel_aug_data <- stream_data[!is.na(sf::st_drop_geometry(stream_data)[[variable_aug]]), ]
    panel_aug_data <- suppressWarnings(sf::st_crop(panel_aug_data, fraser_bbox))
    var_aug_sym <- sym(variable_aug)

    p_sd_aug <- ggplot() +
      geom_sf(data = fraser_boundary, color = "black", fill = "grey95", linewidth = 0.6) +
      geom_sf(data = panel_aug_data, aes(color = !!var_aug_sym, linewidth = as.numeric(stream_order))) +
      scale_linewidth_continuous(range = c(0.1, 0.9), guide = "none") +
      scale_color_cvis(palette = risk_palette, direction = palette_direction, limits = xlim_aug, guide = "none", oob = scales::squish) +
      coord_sf(datum = NA, xlim = fraser_bbox[c(1,3)], ylim = fraser_bbox[c(2,4)]) +
      labs(title = "August Flow (Streamdyn model)") +
      theme_void() +
      theme(plot.title = element_text(size = 11, face = "bold", hjust = 0.5), plot.margin = margin(3, 3, 3, 3))

    p_sd_aug_hist <- ggplot(sf::st_drop_geometry(panel_aug_data), aes(x = !!var_aug_sym)) +
      geom_histogram(aes(fill = after_stat(x)), bins = 15, color = "white", linewidth = 0.1, show.legend = FALSE, na.rm = TRUE) +
      scale_fill_cvis(palette = risk_palette, direction = palette_direction, limits = xlim_aug, oob = scales::squish) +
      scale_x_continuous(limits = xlim_aug, oob = scales::squish, breaks = c(xlim_aug[1], (xlim_aug[1] + xlim_aug[2])/2, xlim_aug[2]), labels = function(x) sprintf("%.1f", x)) +
      labs(x = unit_label) +
      theme_void() +
      theme(
        axis.line.x = element_line(color = "black", linewidth = 0.5),
        axis.text.x = element_text(color = "black", size = 6.5, face = "bold", margin = margin(t = 4)),
        axis.ticks.x = element_line(color = "black", linewidth = 0.5),
        axis.title.x = element_text(color = "black", size = 6, face = "bold", margin = margin(t = 4)),
        plot.background = element_rect(fill = "white", color = NA),
        plot.margin = margin(1, 2, 10, 2)
      )

    p_sd_aug_combined <- p_sd_aug + patchwork::inset_element(p_sd_aug_hist, left = inset_coords$left, bottom = inset_coords$bottom, right = inset_coords$right, top = inset_coords$top, align_to = "panel")
  } else {
    p_sd_aug_combined <- ggplot() + theme_void() + labs(title = "August Flow (Streamdyn model)")
  }

  # ==================== PANEL D: Winter Flow (Station Model) ====================
  wp_flat_win <- wp_vm %>%
    dplyr::mutate(mean_val = sapply(data, function(df) {
      if (is.null(df) || nrow(df) == 0) return(NA_real_)
      if ("mean_ndj" %in% names(df)) mean(df$mean_ndj, na.rm = TRUE) else NA_real_
    })) %>%
    dplyr::filter(!is.na(mean_val)) %>%
    dplyr::select(ID, rcp, gcm_name, period, mean_ndj = mean_val)

  if (nrow(wp_flat_win) > 0) {
    wp_hist_win <- wp_flat_win %>%
      dplyr::filter(period == "0") %>%
      dplyr::group_by(ID, gcm_name) %>%
      dplyr::summarise(mean_hist = mean(mean_ndj, na.rm = TRUE), .groups = "drop")

    wp_future_win <- wp_flat_win %>% dplyr::filter(rcp == rcp_val, period == period_val)

    wp_delta_win <- wp_future_win %>%
      dplyr::left_join(wp_hist_win %>% dplyr::select(ID, mean_hist), by = "ID") %>%
      dplyr::mutate(delta = (mean_ndj - mean_hist) / mean_hist) %>%
      dplyr::group_by(ID) %>%
      dplyr::summarise(value = mean(delta, na.rm = TRUE), .groups = "drop")

    watershed_flow_win <- watershed_flow_cropped %>% dplyr::left_join(wp_delta_win, by = "ID")
  } else {
    watershed_flow_win <- watershed_flow_cropped %>% dplyr::mutate(value = NA_real_)
  }

  p_stn_win <- ggplot() +
    geom_sf(data = fraser_boundary, color = "black", fill = "grey95", linewidth = 0.6) +
    geom_sf(data = watershed_flow_win, aes(fill = value), alpha = 0.65, color = NA) +
    scale_fill_cvis(palette = risk_palette, direction = -palette_direction, limits = xlim_win, guide = "none", oob = scales::squish) +
    geom_sf(data = stations_flow_cropped, color = "black", size = 1) +
    coord_sf(datum = NA, xlim = fraser_bbox[c(1,3)], ylim = fraser_bbox[c(2,4)]) +
    labs(title = "Winter Flow (Station model)") +
    theme_void() +
    theme(plot.title = element_text(size = 11, face = "bold", hjust = 0.5), plot.margin = margin(3, 3, 3, 3))

  p_stn_win_hist <- ggplot(sf::st_drop_geometry(watershed_flow_win), aes(x = value)) +
    geom_histogram(aes(fill = after_stat(x)), bins = 15, color = "white", linewidth = 0.1, show.legend = FALSE, na.rm = TRUE) +
    scale_fill_cvis(palette = risk_palette, direction = -palette_direction, limits = xlim_win, oob = scales::squish) +
    scale_x_continuous(limits = xlim_win, oob = scales::squish, breaks = c(xlim_win[1], (xlim_win[1] + xlim_win[2])/2, xlim_win[2]), labels = function(x) sprintf("%.1f", x)) +
    labs(x = unit_label) +
    theme_void() +
    theme(
      axis.line.x = element_line(color = "black", linewidth = 0.5),
      axis.text.x = element_text(color = "black", size = 6.5, face = "bold", margin = margin(t = 4)),
      axis.ticks.x = element_line(color = "black", linewidth = 0.5),
      axis.title.x = element_text(color = "black", size = 6, face = "bold", margin = margin(t = 4)),
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(1, 2, 10, 2)
    )

  p_stn_win_combined <- p_stn_win + patchwork::inset_element(p_stn_win_hist, left = inset_coords$left, bottom = inset_coords$bottom, right = inset_coords$right, top = inset_coords$top, align_to = "panel")

  # ==================== PANEL E: Winter Flow (Streamdyn Model) ====================
  if (variable_win %in% names(stream_data)) {
    panel_win_data <- stream_data[!is.na(sf::st_drop_geometry(stream_data)[[variable_win]]), ]
    panel_win_data <- suppressWarnings(sf::st_crop(panel_win_data, fraser_bbox))
    var_win_sym <- sym(variable_win)

    p_sd_win <- ggplot() +
      geom_sf(data = fraser_boundary, color = "black", fill = "grey95", linewidth = 0.6) +
      geom_sf(data = panel_win_data, aes(color = !!var_win_sym, linewidth = as.numeric(stream_order))) +
      scale_linewidth_continuous(range = c(0.1, 0.9), guide = "none") +
      scale_color_cvis(palette = risk_palette, direction = -palette_direction, limits = xlim_win, guide = "none", oob = scales::squish) +
      coord_sf(datum = NA, xlim = fraser_bbox[c(1,3)], ylim = fraser_bbox[c(2,4)]) +
      labs(title = "Winter Flow (Streamdyn model)") +
      theme_void() +
      theme(plot.title = element_text(size = 11, face = "bold", hjust = 0.5), plot.margin = margin(3, 3, 3, 3))

    p_sd_win_hist <- ggplot(sf::st_drop_geometry(panel_win_data), aes(x = !!var_win_sym)) +
      geom_histogram(aes(fill = after_stat(x)), bins = 15, color = "white", linewidth = 0.1, show.legend = FALSE, na.rm = TRUE) +
      scale_fill_cvis(palette = risk_palette, direction = -palette_direction, limits = xlim_win, oob = scales::squish) +
      scale_x_continuous(limits = xlim_win, oob = scales::squish, breaks = c(xlim_win[1], (xlim_win[1] + xlim_win[2])/2, xlim_win[2]), labels = function(x) sprintf("%.1f", x)) +
      labs(x = unit_label) +
      theme_void() +
      theme(
        axis.line.x = element_line(color = "black", linewidth = 0.5),
        axis.text.x = element_text(color = "black", size = 6.5, face = "bold", margin = margin(t = 4)),
        axis.ticks.x = element_line(color = "black", linewidth = 0.5),
        axis.title.x = element_text(color = "black", size = 6, face = "bold", margin = margin(t = 4)),
        plot.background = element_rect(fill = "white", color = NA),
        plot.margin = margin(1, 2, 10, 2)
      )

    p_sd_win_combined <- p_sd_win + patchwork::inset_element(p_sd_win_hist, left = inset_coords$left, bottom = inset_coords$bottom, right = inset_coords$right, top = inset_coords$top, align_to = "panel")
  } else {
    p_sd_win_combined <- ggplot() + theme_void() + labs(title = "Winter Flow (Streamdyn model)")
  }

  # Combine panels into a 2x3 layout
  p_out <- (p_regime_combined | p_stn_aug_combined | p_sd_aug_combined) /
           (patchwork::plot_spacer() | p_stn_win_combined | p_sd_win_combined) +
           patchwork::plot_layout(heights = c(1, 1))

  return(p_out)
}




# ==================== 8. CU Lollipop Chart ====================


# ind_cu <- get_CU_indicators(all_flat_std,
#   cu_i = "CK-10",
#   RCP_pick = "45",
#   period_pick = "3",
#   indicators_choose = tbl_indicators$abbrev)






# ==================== 9. Marine Indicators Map ====================

# SST_cu_sp <- subset_and_mean_sst(sf_data = filter(SST_grid, MAZ_Acrony == "GStr"),
#   timing_df = cu_mar,
#   RCP_pick = "45")
#
# MAZ <- st_read(file.path(paths$spatial, "MAZ", "MAZ_Final.shp"))
#
# MAZ_GStr <- filter(MAZ, MAZ_Acrony == "GStr")




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
      plot_data$CVIS_LABEL, "<br>",
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


# ==================== 14. Single CU Vulnerability Summary Plot ====================

#' Vectorized helper to format raw indicator values into readable strings with units
#'
#' @param indicator Character vector of indicator codes.
#' @param val Numeric vector of raw values.
#' @return Character vector of formatted strings.
format_cvis_raw_value <- function(indicator, val) {
  mapply(function(ind, v) {
    if (is.na(v) || is.nan(v)) return("N/A")
    case_when(
      ind == "favchange" ~ sprintf("%+.2f", v),
      ind == "cthr"      ~ sprintf("%.2f", v),
      ind == "tw8rate"   ~ sprintf("%+.2f °C/dec", v),
      ind == "tw8proj"   ~ sprintf("%.1f °C", v),
      ind == "flow8pdelta" ~ sprintf("%+d%%", round(v * 100)),
      ind == "flow18pdelta" ~ sprintf("%+d%%", round(v * 100)),
      ind == "fwres"     ~ sprintf("%d d", round(v)),
      ind == "migrTproj" ~ sprintf("%.1f °C", v),
      ind == "migrQpdelta" ~ sprintf("%+d%%", round(v * 100)),
      ind == "migrdist"  ~ if (v > 1000) sprintf("%.0f km", v / 1000) else sprintf("%.1f km", v),
      ind == "SSTproj"   ~ sprintf("%.1f °C", v),
      ind == "SSTrate"   ~ sprintf("%+.2f °C/dec", v),
      ind == "CImpact"   ~ sprintf("%.2f", v),
      ind == "CUstatus"  ~ case_when(
        v %in% c(1, 0) ~ "Green",
        v %in% c(2, 0.5) ~ "Amber",
        v %in% c(3, 1) ~ "Red",
        TRUE ~ as.character(v)
      ),
      ind == "CUnmat"    ~ {
        if (v >= 1000000) sprintf("%.1fM", v / 1000000)
        else if (v >= 1000) sprintf("%.1fK", v / 1000)
        else sprintf("%.0f", v)
      },
      ind == "hetzyg"    ~ sprintf("%.3f", v),
      ind == "genoff"    ~ sprintf("%.3f", v),
      TRUE ~ sprintf("%.2f", v)
    )
  }, indicator, val)
}

#' Creates a comprehensive visual summary of vulnerability for a single Conservation Unit.
#'
#' Plots the Overall Vulnerability score, Category vulnerability scores, and individual 
#' vulnerability indicators in a single vertically-stacked panel using patchwork.
#' Shows the species-wide distribution in the background as violins.
#'
#' @param cu_code Character string specifying the Conservation Unit code (e.g. "CK-03").
#' @param scores_tidy Data frame containing overall and category vulnerability scores. Defaults to scores_tidy_baseline.
#' @param all_std_long Data frame containing standardized indicator values. Defaults to all_std_long_baseline.
#' @param indicators_metadata Data frame containing indicator definitions. Defaults to tbl_indicators.
#' @return A patchwork combined ggplot object.
plot_cu_vulnerability_summary <- function(cu_code,
                                          scores_tidy = NULL,
                                          all_std_long = NULL,
                                          indicators_metadata = NULL) {
  # Resolve inputs
  if (is.null(scores_tidy)) {
    if (exists("scores_tidy_baseline", envir = .GlobalEnv)) {
      scores_tidy <- get("scores_tidy_baseline", envir = .GlobalEnv)
    } else if (exists("scores_tidy", envir = .GlobalEnv)) {
      scores_tidy <- get("scores_tidy", envir = .GlobalEnv)
    } else {
      stop("scores_tidy dataset not found.")
    }
  }
  
  if (is.null(all_std_long)) {
    if (exists("all_std_long_baseline", envir = .GlobalEnv)) {
      all_std_long <- get("all_std_long_baseline", envir = .GlobalEnv)
    } else if (exists("all_std_long", envir = .GlobalEnv)) {
      all_std_long <- get("all_std_long", envir = .GlobalEnv)
    } else {
      stop("all_std_long dataset not found.")
    }
  }
  
  if (is.null(indicators_metadata)) {
    if (exists("tbl_indicators", envir = .GlobalEnv)) {
      indicators_metadata <- get("tbl_indicators", envir = .GlobalEnv)
    } else {
      stop("tbl_indicators not found.")
    }
  }

  # Find baseline parameters from global env or defaults
  rcp_val <- if (exists("sens_rcp_base", envir = .GlobalEnv)) get("sens_rcp_base", envir = .GlobalEnv) else "45"
  period_val <- if (exists("sens_period_base", envir = .GlobalEnv)) get("sens_period_base", envir = .GlobalEnv) else "3"
  gcm_val <- if (exists("sens_gcm_base", envir = .GlobalEnv)) get("sens_gcm_base", envir = .GlobalEnv) else "9"
  std_method_val <- if (exists("std_method_base", envir = .GlobalEnv)) get("std_method_base", envir = .GlobalEnv) else "mix"

  # Filter to baseline if multiple scenarios exist
  if ("rcp" %in% names(scores_tidy) && length(unique(scores_tidy$rcp)) > 1) {
    scores_tidy <- scores_tidy %>% filter(rcp %in% c("0", rcp_val))
  }
  if ("period_code" %in% names(scores_tidy) && length(unique(scores_tidy$period_code)) > 1) {
    scores_tidy <- scores_tidy %>% filter(period_code %in% c("0", period_val))
  }
  if ("gcm" %in% names(scores_tidy) && length(unique(scores_tidy$gcm)) > 1) {
    scores_tidy <- scores_tidy %>% filter(gcm %in% c("0", gcm_val))
  }
  if ("std_method" %in% names(scores_tidy) && length(unique(scores_tidy$std_method)) > 1) {
    scores_tidy <- scores_tidy %>% filter(std_method == std_method_val)
  }

  # Filter all_std_long
  if ("rcp" %in% names(all_std_long) && length(unique(all_std_long$rcp)) > 1) {
    all_std_long <- all_std_long %>% filter(rcp %in% c("0", rcp_val))
  }
  if ("period_code" %in% names(all_std_long) && length(unique(all_std_long$period_code)) > 1) {
    all_std_long <- all_std_long %>% filter(period_code %in% c("0", period_val))
  }
  if ("gcm" %in% names(all_std_long) && length(unique(all_std_long$gcm)) > 1) {
    all_std_long <- all_std_long %>% filter(gcm %in% c("0", gcm_val))
  }
  if ("std_method" %in% names(all_std_long) && length(unique(all_std_long$std_method)) > 1) {
    all_std_long <- all_std_long %>% filter(std_method == std_method_val)
  }

  # Check if cu_code exists in the dataset
  if (!cu_code %in% scores_tidy$FULL_CU_IN) {
    stop(paste("CU code", cu_code, "not found in scores dataset."))
  }

  cu_row <- scores_tidy %>% filter(FULL_CU_IN == cu_code) %>% slice(1)
  
  # Fallback naming logic
  cu_name <- if ("CVIS_LABEL" %in% names(cu_row)) cu_row$CVIS_LABEL[1] else cu_code
  sp_name <- if ("SPECIES_NAME" %in% names(cu_row)) cu_row$SPECIES_NAME[1] else "Salmon"
  cu_common <- if ("CU_COMMON_NAME" %in% names(cu_row)) cu_row$CU_COMMON_NAME[1] else ""
  
  # Clean up underscores and prefixes for display
  clean_cu_name <- gsub(paste0("^", cu_code, "[_-]"), "", cu_name)
  clean_cu_name <- gsub("_", " ", clean_cu_name)
  clean_cu_common <- gsub("_", " ", cu_common)
  
  # Category color mapping (dynamic from global indicator_palette with muted hex fallbacks)
  if (exists("indicator_palette", envir = .GlobalEnv)) {
    indicator_pal <- get("indicator_palette", envir = .GlobalEnv)
    cat_colors <- c(
      "dem"  = as.character(indicator_pal["Demographics"]),
      "fwrs" = as.character(indicator_pal["Spawning & Rearing"]),
      "gen"  = as.character(indicator_pal["Genetics"]),
      "mar"  = as.character(indicator_pal["Nearshore Marine"]),
      "migr" = as.character(indicator_pal["Upstream Migration"])
    )
  } else {
    cat_colors <- c(
      "dem"  = "#9E6B7A",
      "fwrs" = "#8AA382",
      "gen"  = "#D9946C",
      "mar"  = "#698B93",
      "migr" = "#7D8CA3"
    )
  }
  
  # Align category order from top to bottom
  category_order <- c("dem", "fwrs", "gen", "mar", "migr")

  # ---------------------------------------------
  # Part 1: Prepare Category & Overall Scores Data
  # ---------------------------------------------
  
  # CU specific scores
  cu_cat_scores <- scores_tidy %>%
    filter(FULL_CU_IN == cu_code, method == "avg", category %in% c("dem", "fwrs", "gen", "mar", "migr")) %>%
    select(category, score = score100_all)
    
  cu_overall_score <- scores_tidy %>%
    filter(FULL_CU_IN == cu_code, method == "catavg", category == "all") %>%
    select(category, score = score100_all)
    
  cu_scores <- bind_rows(cu_overall_score, cu_cat_scores) %>%
    mutate(
      label = case_when(
        category == "all" ~ "Overall Vulnerability",
        category == "dem" ~ "Demographics",
        category == "fwrs" ~ "Spawning & Rearing",
        category == "gen" ~ "Genetics",
        category == "mar" ~ "Nearshore Marine",
        category == "migr" ~ "Upstream Migration",
        TRUE ~ category
      )
    )
    
  # Distribution for violins (all CUs)
  sp_cat_scores <- scores_tidy %>%
    filter(method == "avg", category %in% c("dem", "fwrs", "gen", "mar", "migr")) %>%
    select(category, score = score100_all)
    
  sp_overall_scores <- scores_tidy %>%
    filter(method == "catavg", category == "all") %>%
    select(category, score = score100_all)
    
  sp_scores_all <- bind_rows(sp_overall_scores, sp_cat_scores) %>%
    mutate(
      label = case_when(
        category == "all" ~ "Overall Vulnerability",
        category == "dem" ~ "Demographics",
        category == "fwrs" ~ "Spawning & Rearing",
        category == "gen" ~ "Genetics",
        category == "mar" ~ "Nearshore Marine",
        category == "migr" ~ "Upstream Migration",
        TRUE ~ category
      ),
      # Enforce factor levels so Upstream Migration is at the bottom and Overall Vulnerability is at the top
      label = factor(label, levels = c(
        "Upstream Migration",
        "Nearshore Marine",
        "Genetics",
        "Spawning & Rearing",
        "Demographics",
        "Overall Vulnerability"
      ))
    )
    
  # Color category labels dynamically using markdown styling
  sp_scores_all <- sp_scores_all %>%
    mutate(
      color_val = case_when(
        category == "all" ~ "#1A365D",
        TRUE ~ cat_colors[category]
      ),
      label_colored = if_else(
        category == "all",
        paste0("<span style='color:", color_val, "; font-size:10.5pt;'><b>", label, "</b></span>"),
        paste0("<span style='color:", color_val, ";'>", label, "</span>")
      )
    )
  
  levels_top_colored <- sp_scores_all %>%
    arrange(label) %>%
    pull(label_colored) %>%
    unique()
    
  sp_scores_all$label_colored <- factor(sp_scores_all$label_colored, levels = levels_top_colored)
  
  plot_data_top <- cu_scores %>%
    left_join(unique(select(sp_scores_all, category, label_colored)), by = "category") %>%
    mutate(
      label_colored = factor(label_colored, levels = levels_top_colored)
    )

  # ---------------------------------------------
  # Part 2: Prepare Indicators Data
  # ---------------------------------------------
  
  # CU specific indicator values
  cu_indicators <- all_std_long %>%
    filter(FULL_CU_IN == cu_code, stat == "mean") %>%
    select(indicator, std_value, value, category)
    
  # Distribution for indicators (all CUs, standardized 0 to 1 scale)
  sp_ind_values <- all_std_long %>%
    filter(stat == "mean") %>%
    left_join(indicators_metadata %>% select(abbrev, name), by = c("indicator" = "abbrev")) %>%
    mutate(
      name = coalesce(name, indicator),
      # Clean indicator names by removing unit descriptions inside parentheses
      name_clean = gsub(" \\(.*\\)", "", name),
      # Replace abbreviations with full names for y-axis labels
      name_clean = gsub("SST", "sea surface temperature", name_clean),
      name_clean = gsub("CU", "conservation unit", name_clean),
      name_clean = gsub("ENM", "ecological niche model", name_clean),
      indicator_label = name_clean,
      ind_score = std_value,  # 0 to 1 scale
      category = factor(category, levels = category_order),
      category_label = case_when(
        category == "dem" ~ "Demographics",
        category == "fwrs" ~ "Spawning & Rearing",
        category == "gen" ~ "Genetics",
        category == "mar" ~ "Nearshore Marine",
        category == "migr" ~ "Upstream Migration",
        TRUE ~ as.character(category)
      )
    ) %>%
    arrange(category)
    
  plot_data_bottom <- cu_indicators %>%
    left_join(indicators_metadata %>% select(abbrev, name), by = c("indicator" = "abbrev")) %>%
    mutate(
      name = coalesce(name, indicator),
      name_clean = gsub(" \\(.*\\)", "", name),
      # Replace abbreviations with full names for y-axis labels
      name_clean = gsub("SST", "sea surface temperature", name_clean),
      name_clean = gsub("CU", "conservation unit", name_clean),
      name_clean = gsub("ENM", "ecological niche model", name_clean),
      cu_score = std_value,   # 0 to 1 scale
      category = factor(category, levels = category_order),
      category_label = case_when(
        category == "dem" ~ "Demographics",
        category == "fwrs" ~ "Spawning & Rearing",
        category == "gen" ~ "Genetics",
        category == "mar" ~ "Nearshore Marine",
        category == "migr" ~ "Upstream Migration",
        TRUE ~ as.character(category)
      ),
      indicator_label = name_clean,
      raw_label = format_cvis_raw_value(indicator, value)
    ) %>%
    arrange(category, indicator) %>%
    mutate(indicator_label = factor(indicator_label, levels = unique(indicator_label)))

  # Sync factor levels between species distribution and CU subset for indicators
  sp_ind_values$indicator_label <- factor(sp_ind_values$indicator_label, levels = levels(plot_data_bottom$indicator_label))

  # Define category labels in order
  category_labels_ordered <- paste0("<span style='color:", cat_colors[category_order], ";'><b>", c("Demographics", "Spawning & Rearing", "Genetics", "Nearshore Marine", "Upstream Migration"), "</b></span>")

  # Color the category facet strips dynamically using HTML/Markdown and set factor levels
  sp_ind_values <- sp_ind_values %>%
    mutate(
      category_label_colored = paste0("<span style='color:", cat_colors[category], ";'><b>", category_label, "</b></span>"),
      category_label_colored = factor(category_label_colored, levels = category_labels_ordered)
    )
    
  plot_data_bottom <- plot_data_bottom %>%
    mutate(
      category_label_colored = paste0("<span style='color:", cat_colors[category], ";'><b>", category_label, "</b></span>"),
      category_label_colored = factor(category_label_colored, levels = category_labels_ordered)
    )

  # ---------------------------------------------
  # Plot A: Category & Overall Vulnerability Scores (0-100 Scale)
  # ---------------------------------------------
  
  if (exists("cvis_risk_palette_colors", envir = .GlobalEnv)) {
    cvis_pal_cols <- get("cvis_risk_palette_colors", envir = .GlobalEnv)
  } else {
    cvis_pal_cols <- c("#3060AF", "#78A7F5", "#EBCC5A", "#E1AF00", "#C21A1D")
  }
  
  # Category-specific colors for top plot (dynamic from global indicator_palette with muted hex fallbacks)
  if (exists("indicator_palette", envir = .GlobalEnv)) {
    indicator_pal <- get("indicator_palette", envir = .GlobalEnv)
    cat_colors_top <- c(
      "all"  = "#1A365D",
      "dem"  = as.character(indicator_pal["Demographics"]),
      "fwrs" = as.character(indicator_pal["Spawning & Rearing"]),
      "gen"  = as.character(indicator_pal["Genetics"]),
      "mar"  = as.character(indicator_pal["Nearshore Marine"]),
      "migr" = as.character(indicator_pal["Upstream Migration"])
    )
  } else {
    cat_colors_top <- c(
      "all"  = "#1A365D",
      "dem"  = "#9E6B7A",
      "fwrs" = "#8AA382",
      "gen"  = "#D9946C",
      "mar"  = "#698B93",
      "migr" = "#7D8CA3"
    )
  }

  p_top <- ggplot()
  
  # Background violins showing species distributions colored by category palette
  for (cat in names(cat_colors_top)) {
    p_top <- p_top +
      geom_violin(
        data = sp_scores_all %>% filter(category == cat),
        aes(x = score, y = label_colored),
        fill = cat_colors_top[cat],
        color = cat_colors_top[cat],
        linewidth = 0.55,
        alpha = 0.18,
        scale = "width",
        width = 0.65,
        orientation = "y"
      )
  }
    
  # CU score point (colored by score)
  p_top <- p_top +
    geom_point(
      data = plot_data_top,
      aes(x = score, y = label_colored, fill = score),
      shape = 21,
      color = "black",
      size = 5.2,
      stroke = 1.2
    ) +
    scale_y_discrete(limits = levels_top_colored) +
    scale_fill_gradientn(
      colors = cvis_pal_cols,
      limits = c(0, 100),
      name = "Vulnerability Score",
      guide = guide_colorbar(title.position = "top", barwidth = 10, barheight = 0.5)
    ) +
    scale_x_continuous(limits = c(0, 118), breaks = seq(0, 100, 20), expand = c(0.02, 0)) +
    geom_hline(yintercept = 5.5, color = "grey60", linetype = "solid", linewidth = 0.5) +
    labs(
      title = paste0("Vulnerability Profile: ", clean_cu_name, " (", cu_code, ")"),
      x = NULL,
      y = NULL
    ) +
    theme_cvis(base_size = 12) +
    theme(
      plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
      axis.text.y = ggtext::element_markdown(lineheight = 0.8),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(color = "grey93", linewidth = 0.5),
      legend.position = "top",
      legend.box = "horizontal",
      legend.margin = margin(b = -5)
    )

  # ---------------------------------------------
  # Plot B: Indicators by Category (0-1 Scale)
  # ---------------------------------------------
  
  p_bottom <- ggplot()
  
  # Background violins showing species distributions colored by category palette
  for (cat in category_order) {
    p_bottom <- p_bottom +
      geom_violin(
        data = sp_ind_values %>% filter(category == cat),
        aes(x = ind_score, y = indicator_label),
        fill = cat_colors[cat],
        color = cat_colors[cat],
        linewidth = 0.55,
        alpha = 0.18,
        scale = "width",
        width = 0.65,
        orientation = "y",
        na.rm = TRUE
      )
  }
    
  # CU indicator point (colored by standardized score, mapped internally to 0-100 for color scale sync)
  p_bottom <- p_bottom +
    geom_point(
      data = plot_data_bottom,
      aes(x = cu_score, y = indicator_label, fill = cu_score * 100),
      shape = 21,
      color = "black",
      size = 4.0,
      stroke = 1.0
    ) +
    # Raw value text labels next to the points
    geom_text(
      data = plot_data_bottom,
      aes(x = cu_score, y = indicator_label, label = raw_label),
      hjust = -0.25,
      vjust = 0.5,
      size = 3.0,
      fontface = "bold",
      color = "grey15"
    ) +
    scale_fill_gradientn(
      colors = cvis_pal_cols,
      limits = c(0, 100),
      guide = "none"
    ) +
    scale_x_continuous(limits = c(0, 1.18), breaks = seq(0, 1.0, 0.2), expand = c(0.02, 0)) +
    coord_cartesian(clip = "off") +
    facet_grid(
      rows = vars(category_label_colored),
      scales = "free_y",
      space = "free_y"
    ) +
    labs(
      x = "Standardized Indicator Value (0 = Low Risk, 1 = High Risk)",
      y = NULL
    ) +
    theme_cvis(base_size = 11) +
    theme(
      strip.text.y = ggtext::element_markdown(angle = 0, hjust = 0, face = "bold", size = 8.5),
      strip.background = element_rect(fill = "grey95", color = NA),
      axis.text.y = element_text(size = 8.5),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(color = "grey93", linewidth = 0.5)
    )

  # Assemble using patchwork
  p_combined <- p_top / p_bottom +
    plot_layout(heights = c(1, 2.5), guides = "collect") &
    theme(legend.position = "top")

  return(p_combined)
}


# ==================== 11. Individual CU Sensitivity Plots ====================

plot_cu_sensitivity_scores <- function(
  overall_sensitivity,
  cu_code
) {
  # Categories mapping including all categories and overall vulnerability
  cat_labels <- c(
    "all"  = "Overall Vulnerability",
    "dem"  = "Demographics",
    "fwrs" = "Spawning & Rearing",
    "migr" = "Upstream Migration",
    "mar"  = "Nearshore Marine",
    "gen"  = "Genetics"
  )

  # 1. Prepare score deviations data
  dev_raw <- overall_sensitivity$deviations %>%
    filter(category %in% names(cat_labels), FULL_CU_IN != "ALL") %>%
    select(FULL_CU_IN, category, starts_with("raw_dev_")) %>%
    pivot_longer(cols = starts_with("raw_dev_"), names_to = "source_label", values_to = "raw_deviation") %>%
    mutate(
        source_label = str_remove(source_label, "raw_dev_"),
        source_type = case_when(
            str_detect(source_label, "^GCM") ~ "GCM",
            str_detect(source_label, "^RCP") ~ "Scenario",
            str_detect(source_label, "^Method") ~ "Method",
            str_detect(source_label, "^Model") ~ "dsmethod",
            str_detect(source_label, "^dsmethod") ~ "dsmethod",
            str_detect(source_label, "^stdmethod") ~ "stdmethod",
            TRUE ~ "Other"
        ),
        source = case_when(
            source_type == "Method" ~ str_remove(source_label, "^Method_"),
            TRUE ~ source_label
        )
    ) %>%
    filter(!(source %in% c("cube", "cube_all") | (source == "flag" & category != "all"))) %>%
    mutate(
      category_label = factor(cat_labels[category], levels = cat_labels)
    )

  # Factor levels for consistency
  source_levels <- c("GCM1", "GCM4", "GCM6", "RCP45_P5", "RCP85_P3", "RCP85_P5", "dsmethod", "stdmethod", "flag", "avgcube")
  source_labels <- c("CanESM2 (GCM 1)", "HadGEM2 (GCM 4)", "MPI (GCM 6)", "RCP 4.5 (P5)", "RCP 8.5 (P3)", "RCP 8.5 (P5)", "Downscaling Method",
                     "Standardize Method", "Red Flag Scoring", "Avg Cube Scoring")
  
  dev_raw <- dev_raw %>%
    filter(source %in% source_levels) %>%
    mutate(source = factor(source, levels = rev(source_levels), labels = rev(source_labels)))

  # Selected CU data
  dev_raw_cu <- dev_raw %>% filter(FULL_CU_IN == cu_code)
  # All other CUs
  dev_raw_others <- dev_raw %>% filter(FULL_CU_IN != cu_code)

  # Retrieve colors using sens_source_palette
  sens_palette <- get("sens_source_palette", envir = .GlobalEnv)
  
  # Retrieve indicator colors from global environment for coloring violins
  ind_colors <- get("indicator_palette", envir = .GlobalEnv)
  ind_colors["Overall Vulnerability"] <- "grey30"
  
  y_colors <- sapply(rev(source_levels), function(x) {
    if (x %in% names(sens_palette)) sens_palette[[x]] else "black"
  })
  y_colors <- unname(y_colors)

  # Plot: Score deviations faceted and colored by category
  p1 <- ggplot(dev_raw_others, aes(y = source, x = raw_deviation, fill = category_label)) +
    geom_violin(color = "grey60", alpha = 0.4, scale = "width") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
    geom_point(data = dev_raw_cu, aes(x = raw_deviation, y = source), color = "#E67E22", size = 4, shape = 18) +
    facet_wrap(~category_label, ncol = 3) +
    scale_fill_manual(values = ind_colors, guide = "none") +
    labs(
      title = paste("Vulnerability Score Shifts for CU:", cu_code),
      subtitle = "Violins show Fraser CUs distribution; Orange diamond shows selected CU",
      x = "Score Deviation (Scenario - Baseline)",
      y = "Assumption / Scenario"
    ) +
    theme_cvis(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", size = 12, color = "black"),
      plot.subtitle = element_text(size = 9, color = "grey40"),
      panel.grid.minor = element_blank(),
      axis.text.y = element_text(color = y_colors, face = "bold", size = 9),
      strip.text = element_text(face = "bold", size = 8.5, color = "black"),
      strip.background = element_blank()
    )

  return(p1)
}


plot_cu_sensitivity_indicators <- function(
  overall_sensitivity,
  tbl_indicators,
  cu_code
) {
  # Prepare indicator directional shifts data relative to baseline
  ind_shift_cus <- overall_sensitivity$indicator_metrics %>%
    filter(FULL_CU_IN != "ALL") %>%
    mutate(
        val_Baseline = base_raw_mean,
        val_GCM1 = base_raw_mean + raw_dev_GCM1,
        val_GCM4 = base_raw_mean + raw_dev_GCM4,
        val_GCM6 = base_raw_mean + raw_dev_GCM6,
        val_RCP45_P5 = base_raw_mean + raw_dev_RCP45_P5,
        val_RCP85_P3 = base_raw_mean + raw_dev_RCP85_P3,
        val_RCP85_P5 = base_raw_mean + raw_dev_RCP85_P5,
        val_dsmethod = base_raw_mean + raw_dev_dsmethod,
        val_stdmethod = base_raw_mean + raw_dev_stdmethod
    ) %>%
    select(FULL_CU_IN, indicator, category, base_raw_mean, starts_with("val_")) %>%
    pivot_longer(cols = starts_with("val_"), names_to = "source", names_prefix = "val_", values_to = "val") %>%
    filter(!is.na(val))

  # Keep only indicators that have non-NA values for the selected CU
  valid_indicators <- ind_shift_cus %>%
    filter(FULL_CU_IN == cu_code, !is.na(val)) %>%
    pull(indicator) %>%
    unique()

  # Categories mapping for ordering and coloring
  cat_labels <- c(
    "dem"  = "Demographics",
    "fwrs" = "Spawning & Rearing",
    "migr" = "Upstream Migration",
    "mar"  = "Nearshore Marine",
    "gen"  = "Genetics"
  )

  # Calculate variation for each indicator for this CU
  var_indicators_df <- ind_shift_cus %>%
    filter(FULL_CU_IN == cu_code) %>%
    group_by(indicator, category) %>%
    summarise(
      val_range = max(val, na.rm = TRUE) - min(val, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(!is.na(val_range), val_range > 1e-5) %>%
    arrange(desc(val_range))

  # If there are no varying indicators, fall back to all valid indicators
  if (nrow(var_indicators_df) == 0) {
    var_indicators <- valid_indicators
  } else {
    # Take top 9 varying indicators
    if (nrow(var_indicators_df) > 9) {
      var_indicators_df <- var_indicators_df[1:9, ]
    }
    # Sort those 9 indicators by category order, then name
    category_order <- c("dem", "fwrs", "migr", "mar", "gen")
    var_indicators_df <- var_indicators_df %>%
      mutate(category_factor = factor(category, levels = category_order)) %>%
      arrange(category_factor, indicator)
    var_indicators <- var_indicators_df$indicator
  }

  ind_shift_cus <- ind_shift_cus %>%
    filter(indicator %in% var_indicators) %>%
    mutate(
      indicator = factor(indicator, levels = var_indicators),
      category_label = factor(cat_labels[category], levels = cat_labels)
    )

  # Scenario levels for indicators
  ind_source_levels <- c("Baseline", "GCM1", "GCM4", "GCM6", "RCP45_P5", "RCP85_P3", "RCP85_P5", "dsmethod")
  ind_source_labels <- c("Baseline", "CanESM2 (GCM 1)", "HadGEM2 (GCM 4)", "MPI (GCM 6)", "RCP 4.5 (P5)", "RCP 8.5 (P3)", "RCP 8.5 (P5)", "Downscaling Method")

  ind_shift_cus <- ind_shift_cus %>%
    filter(source %in% ind_source_levels) %>%
    mutate(source = factor(source, levels = rev(ind_source_levels), labels = rev(ind_source_labels)))

  ind_shift_cu <- ind_shift_cus %>% filter(FULL_CU_IN == cu_code)
  ind_shift_others <- ind_shift_cus %>% filter(FULL_CU_IN != cu_code)

  # Label map with units from tbl_indicators
  ind_label_units <- tbl_indicators %>% 
    dplyr::mutate(
      facet_label = paste0(abbrev, " (", unit_short, ")")
    ) %>% 
    dplyr::select(abbrev, facet_label) %>% 
    tibble::deframe()

  # Retrieve colors using sens_source_palette
  sens_palette <- get("sens_source_palette", envir = .GlobalEnv)
  
  # Retrieve indicator colors from global environment for coloring violins
  ind_colors <- get("indicator_palette", envir = .GlobalEnv)
  
  ind_y_colors <- sapply(rev(ind_source_levels), function(x) {
    if (x == "Baseline") {
      "black"
    } else if (x %in% names(sens_palette)) {
      sens_palette[[x]]
    } else {
      "black"
    }
  })
  ind_y_colors <- unname(ind_y_colors)

  # Extract baseline value for vertical reference lines
  baseline_line_data <- ind_shift_cu %>%
    filter(source == "Baseline") %>%
    select(indicator, x_intercept = val)

  # Plot: Indicator values across scenarios
  p2 <- ggplot(ind_shift_others, aes(y = source, x = val, fill = category_label)) +
    geom_vline(data = baseline_line_data, aes(xintercept = x_intercept), linetype = "dashed", color = "grey50") +
    geom_violin(color = "#CBD5E0", alpha = 0.5, scale = "width") +
    geom_point(data = ind_shift_cu, aes(x = val, y = source), color = "#E67E22", size = 3, shape = 18) +
    facet_wrap(~indicator, scales = "free_x", ncol = 3, labeller = labeller(indicator = ind_label_units)) +
    scale_fill_manual(values = ind_colors, guide = "none") +
    labs( x = "Raw Indicator Value (units vary)",
      y = "Scenario"
    ) +
    theme_cvis(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", size = 12, color = "black"),
      plot.subtitle = element_text(size = 9, color = "grey40"),
      strip.text = element_text(face = "bold", size = 8, color = "black"),
      panel.grid.minor = element_blank(),
      axis.text.y = element_text(color = ind_y_colors, face = "bold", size = 9)
    )

  return(p2)
}

