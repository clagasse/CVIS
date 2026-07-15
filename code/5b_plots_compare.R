# ==============================================================================
# CVIS Comparative Plotting Functions (5b_plots_compare.R)
#
# Description:
#   Collection of plotting and analysis functions to compare climate indicators,
#   vulnerability risk scores, and life history timings across multiple Conservation
#   Units (CUs) or Marine Adaptive Zones (MAZs).
#
# List of Functions:
#   1. Diagnostic and Mapping Plots:
#      - plot_lollipop()              - Single indicator comparison across CUs.
#      - plot_std_vs_raw()            - Plots raw vs standardized indicator values.
#      - plot_indicator_histogram()   - Plots raw and standardized score distributions.
#      - spatial_indicator_plot()     - Maps indicator values by CU boundary.
#   2. Multi-panel and Spatial Group Plots:
#      - spatial_fw_rearing_indicators_plot() - Multi-panel map of FW indicators.
#   3. Correlation and Comparison Plots:
#      - xy_indicator_plot()          - Scatter plot comparing two indicators.
#   4. WSP Abundance and Status Table (cu_status_table)
#   5. Life History Timing Comparison (plot_timing_comparison)
#   6. Combined Tile Plot for All CUs & Indicators (indicator_cu_tile_plot)
#   7. Combined Marine Adaptive Zone (MAZ) Map (combined_maz_marine_plot)
#
# Dependencies:
#   - Requires ggplot2, sf, dplyr, stringr, patchwork, ggtext, and standard CVIS data inputs.
# ==============================================================================

library(pacea)

# Long format plots -------------------------------------------------------

#' Lollipop plot (long-format) for a single RCP/period with GCM variation
#' Always shows all CUs; missing data show as blank (no segment/point).
#'
#' @param data           Long-format table with (at least):
#'   rcp, period_code, indicator, gcm, CVIS_LABEL, SPECIES_NAME, FULL_CU_IN,
#'   (optional) dsmodel, std_value (and optionally raw_value/value for raw plotting)
#' @param indicator_pick Character; indicator id to plot (e.g., "stream_temp")
#' @param dsmodel_pick   Optional character scalar/vector; if provided, filter to these dsmodel values
#' @param indicator_name Optional pretty subtitle for indicator
#' @param indicator_unit Optional y-axis label for raw plots (ignored when standardized)
#' @param use_standardized Logical; FALSE plots raw on the axis; TRUE plots standardized (default FALSE)
#' @param log_scale      Logical; use log10 scale for y
#' @param threshold_value Numeric; optional horizontal threshold line (NA to skip)
#' @param show_gcm_points Logical; show per-GCM points in addition to min–max + mean
#' @param std_method     Character; optional standardization method filter ("linear" or "exponential")
#' @param tbl            Indicators table (default: tbl_indicators)
#' @return ggplot object
plot_lollipop <- function(data,
                          indicator_pick,
                          dsmodel_pick = NULL,
                          indicator_name = NULL,
                          indicator_unit = NULL,
                          use_standardized = FALSE,
                          log_scale = FALSE,
                          threshold_value = NA_real_,
                          show_gcm_points = FALSE,
                          tbl = tbl_indicators) {
  
  # For backwards compatibility:
  all_std_long <- data

  # Look up metadata from tbl if not provided
  if (!is.null(tbl)) {
    ind_row <- tbl[tbl$abbrev == indicator_pick, ]
    if (is.null(indicator_name) && nrow(ind_row) > 0) {
      indicator_name <- ind_row$name[1]
    }
    if (is.null(indicator_unit) && nrow(ind_row) > 0) {
      indicator_unit <- ind_row$unit[1]
    }
  }

  # ---- CU roster (ensures all CUs are shown regardless of data presence) ----
  cu_roster <- all_std_long %>%
    distinct(FULL_CU_IN, SPECIES_NAME, CVIS_LABEL, SMU_SIMPLE)

  # ---- Resolve value column for plotting Y (ranges & center_y) ----
  val_col <- if (use_standardized) {
    if (!"std_value" %in% names(all_std_long)) {
      stop("Column 'std_value' not found in all_std_long.")
    }
    "std_value"
  } else {
    if ("raw_value" %in% names(all_std_long)) {
      "raw_value"
    } else if ("value" %in% names(all_std_long)) {
      "value"
    } else {
      stop("No raw value column found. Provide 'raw_value' or 'value', or set use_standardized = TRUE.")
    }
  }

  # ---- Filter scenarios/models ----
  dat_full <- all_std_long %>%
    filter(
      .data$indicator == indicator_pick
    )

  if (!is.null(dsmodel_pick) && length(dsmodel_pick) > 0 && "dsmodel" %in% names(dat_full)) {
    dat_full <- dat_full %>% filter(.data$dsmodel %in% dsmodel_pick)
  }

  # Ensure rcp and dsmodel are factor-like for plotting
  dat_full <- dat_full %>%
    mutate(
      rcp = factor(rcp),
      dsmodel = if ("dsmodel" %in% names(.)) factor(dsmodel) else factor("default")
    )

  # Note: even if dat_full is empty, we still want to show all CUs with blanks.
  if (!"std_value" %in% names(dat_full)) {
    if (nrow(dat_full) > 0) stop("Column 'std_value' is required for colouring points by standardized score.")
  }

  # ---- Reduce to needed columns ----
  dat <- dat_full %>%
    select(FULL_CU_IN, SPECIES_NAME, CVIS_LABEL, SMU_SIMPLE, gcm, rcp, dsmodel, stat,
      val = dplyr::all_of(val_col),
      std_val = dplyr::all_of(if ("std_value" %in% names(dat_full)) "std_value" else val_col)
    )

  # ---- Summaries across GCM per CU/Scenario/Model ----
  cu_summary <- dat %>%
    filter(gcm %in% c("9", "0")) %>%
    group_by(FULL_CU_IN, SPECIES_NAME, CVIS_LABEL, SMU_SIMPLE, rcp, dsmodel) %>%
    summarise(
      min_gcm    = val[stat %in% c("qlowgcm", "qlowsp", "qlow")][1],
      max_gcm    = val[stat %in% c("qhighgcm", "qhighsp", "qhigh")][1],
      center_y   = val[stat == "mean"][1],
      center_std = std_val[stat == "mean"][1],
      .groups    = "drop"
    )

  # ---- Right-join summaries to the full CU roster to ensure all CUs are present ----
  cu_plot <- cu_roster %>%
    left_join(cu_summary, by = c("FULL_CU_IN", "SPECIES_NAME", "CVIS_LABEL", "SMU_SIMPLE"))

  # ---- HTML labels coloured by species ----
  # Pre-calculate colors to avoid length mismatch errors
  species_palette <- get("species_palette", envir = .GlobalEnv)
  sp_col_vec <- species_palette[as.character(cu_plot$SPECIES_NAME)]
  
  cu_plot <- cu_plot %>%
    mutate(
      label_color = coalesce(as.character(sp_col_vec), "#666666"),
      id_label_html = paste0("<span style='color:", label_color, "'>", CVIS_LABEL, "</span>")
    ) %>%
    arrange(SPECIES_NAME, CVIS_LABEL) %>%
    mutate(id_label_html = factor(id_label_html, levels = unique(id_label_html)))

  # ---- Plot ----
  dodge_width <- 0.8
  p <- ggplot(cu_plot, aes(x = id_label_html, color = rcp, group = interaction(rcp, dsmodel)))

  # 1. Background "Cloud" for GCM uncertainty (wide grey bar)
  p <- p +
    geom_segment(aes(xend = id_label_html, y = min_gcm, yend = max_gcm),
      color = "grey92", linewidth = 6, alpha = 0.8, na.rm = TRUE,
      position = position_dodge(width = dodge_width)
    )

  # 2. Inner GCM range line (thin line for contrast)
  p <- p +
    geom_segment(aes(xend = id_label_html, y = min_gcm, yend = max_gcm),
      linewidth = 1.5, alpha = 0.8, na.rm = TRUE,
      position = position_dodge(width = dodge_width)
    )

  # (Optional) Individual GCM points
  if (isTRUE(show_gcm_points) && nrow(dat) > 0) {
    p <- p +
      geom_point(
        data = dat %>%
          left_join(
            cu_plot %>% select(FULL_CU_IN, SPECIES_NAME, CVIS_LABEL, id_label_html, rcp, dsmodel),
            by = c("FULL_CU_IN", "SPECIES_NAME", "CVIS_LABEL", "rcp", "dsmodel")
          ),
        aes(x = id_label_html, y = val, group = interaction(rcp, dsmodel)),
        position = position_dodge(width = dodge_width),
        size = 0.8, alpha = 0.25, inherit.aes = FALSE, color = "black"
      )
  }

  # 3. Lollipop Head (Future ensemble mean color-coded by risk score)
  p <- p +
    geom_point(aes(y = center_y, fill = center_std, shape = dsmodel),
      color = "black", size = 3.2, stroke = 0.8, na.rm = TRUE,
      position = position_dodge(width = dodge_width)
    ) +
    scale_fill_cvis(
      name = "Standardized Score",
      palette = cvis_risk_palette, direction = -1,
      limits = c(0, 1), na.value = "transparent"
    )

  # # Saturated Scenario Colors - supporting both "45"/"85" and "4.5"/"8.5" formats
  p <- p +
    scale_color_manual(
      name = "GCM variation",
      values = c(
        "45" = "darkblue", "4.5" = "darkblue",
        "85" = "#B22222", "8.5" = "#B22222"
      ),
      labels = c(
        "45" = "10-90% range", "4.5" = "10-90% range",
        "85" = "10-90% range", "8.5" = "10-90% range"
      ),
      na.translate = FALSE
    ) +
    scale_shape_manual(
      name = "Downscaling method",
      values = c(21, 24, 22, 23, 25),
      na.translate = FALSE,
      guide = "none"
    )

  # Axis scale
  if (isTRUE(log_scale)) {
    if (any(cu_plot$min_gcm <= 0, na.rm = TRUE)) {
      warning("log_scale = TRUE but some values are <= 0; those rows will be dropped by scale_y_log10.")
    }
    p <- p + scale_y_log10()
  }

  # Labels
  subtitle_lab <- if (is.null(indicator_name)) indicator_pick else indicator_name
  y_lab <- if (isTRUE(use_standardized)) {
    "Standardized score" # no units for standardized
  } else {
    if (is.null(indicator_unit) || is.na(indicator_unit) || indicator_unit == "") {
      "Value"
    } else {
      paste0(indicator_unit)
    }
  }
  dsmodel_lab <- if (!is.null(dsmodel_pick) && length(dsmodel_pick) > 0 && "dsmodel" %in% names(dat_full)) {
    paste(unique(dat_full$dsmodel), collapse = ", ")
  } else {
    NA_character_
  }

  p <- p +
    labs(
      y = y_lab,
      x = NULL
    ) +
    coord_flip() +
    theme_cvis(base_size = 11) +
    theme(
      axis.text.y = ggtext::element_markdown(size = 7.5, vjust = 0.5),
      axis.text.y.left = ggtext::element_markdown(size = 7.5, vjust = 0.5),
      legend.position = "right",
      panel.grid.major.y = element_blank()
    )

  return(p)
}


#' Plot standardized vs raw values for an indicator
#'
#' @param data             Long-format table with indicator, value, std_value, SPECIES_NAME, etc.
#' @param indicator_pick   The abbreviation of the indicator (e.g. "tw8proj")
#' @param indicator_name   Optional friendly name of the indicator
#' @param indicator_unit   Optional unit of the indicator
#' @param tbl              Indicators table (default: tbl_indicators)
#' @return A ggplot object.
plot_std_vs_raw <- function(data,
                            indicator_pick,
                            indicator_name = NULL,
                            indicator_unit = NULL,
                            tbl = tbl_indicators) {
  # Look up metadata from tbl if not provided
  ind_row <- tbl[tbl$abbrev == indicator_pick, ]
  if (is.null(indicator_name) && nrow(ind_row) > 0) {
    indicator_name <- ind_row$name[1]
  }
  if (is.null(indicator_name)) indicator_name <- indicator_pick
  
  if (is.null(indicator_unit) && nrow(ind_row) > 0) {
    indicator_unit <- ind_row$unit[1]
  }
  
  # Filter data
  plot_data <- data %>%
    filter(.data$indicator == indicator_pick) %>%
    filter(!is.na(std_value))

  has_raw <- "value" %in% names(plot_data) && any(!is.na(plot_data$value))
  
  if (!has_raw || nrow(plot_data) == 0) {
    p <- ggplot() +
      theme_void() +
      labs(title = paste("No raw data to compare for", indicator_name))
    return(p)
  }
  
  # Select ensemble/GCM 0 and stat == "mean" to avoid duplication
  scatter_dat <- plot_data
  if ("stat" %in% names(scatter_dat) && any(scatter_dat$stat == "mean", na.rm = TRUE)) {
    scatter_dat <- scatter_dat %>% filter(.data$stat == "mean")
  }
  if ("gcm" %in% names(scatter_dat)) {
    if (any(scatter_dat$gcm == "9", na.rm = TRUE)) {
      scatter_dat <- scatter_dat %>% filter(.data$gcm == "9")
    } else if (any(scatter_dat$gcm == 0, na.rm = TRUE)) {
      scatter_dat <- scatter_dat %>% filter(.data$gcm == 0)
    }
  }
  
  x_label <- paste0(if (!is.null(indicator_unit) && !is.na(indicator_unit) && indicator_unit != "") indicator_unit else "value")
  
  p <- ggplot(scatter_dat, aes(x = value, y = std_value, color = SPECIES_NAME)) +
    geom_point(size = 2.5) +
    scale_color_manual(values = species_palette) +
    labs(
      color  = "Species",
      x      = x_label,
      y      = "Standardized Score"
      #title  = paste("Standardization function -", indicator_name)
    ) +
    theme_cvis(base_size = 10) +
    theme(
      plot.title = element_text(face = "bold", size = 11, hjust = 0.5)
    )
  
  return(p)
}


#' Spatial mapping of indicator values by CU boundary
#'
#' @param data             Long-format table with indicator, value, std_value, etc.
#' @param cu_boundary      Spatial sf object containing CU boundaries and species column.
#' @param indicator_pick   The abbreviation of the indicator (e.g. "tw8proj")
#' @param outline          Watershed outline sf object (default: Fr_basin).
#' @param sp_pick          Species to plot (default: all 5 species).
#' @param sp_col_name      Column name for species in the data and boundary (default: "SPECIES_NAME").
#' @param indicator_name   Optional friendly name of the indicator.
#' @param use_standardized Logical; if TRUE, uses std_value (0-1), else uses raw value (default: TRUE).
#' @param id_col           CU ID column (default: "FULL_CU_IN").
#' @param brewer_palette   Color palette (default: cvis_risk_palette).
#' @param palette_direction Color direction (default: -1).
#' @param tbl              Indicators table (default: tbl_indicators)
#' @return A combined patchwork ggplot object.
spatial_indicator_plot <- function(data,
                                   cu_boundary,
                                   indicator_pick,
                                   outline = NULL,
                                   sp_pick = c("Chinook", "Coho", "Sockeye", "Chum", "Pink"),
                                   sp_col_name = "SPECIES_NAME",
                                   indicator_name = NULL,
                                   use_standardized = TRUE,
                                   id_col = "FULL_CU_IN",
                                   risk_palette = cvis_risk_palette,
                                   palette_direction = -1,
                                   tbl = tbl_indicators) {
  # Dynamically load Fr_basin if outline is NULL and it's not in the environment
  if (is.null(outline)) {
    if (exists("Fr_basin", envir = .GlobalEnv)) {
      outline <- get("Fr_basin", envir = .GlobalEnv)
    } else {
      basins_path <- file.path(paths$fw, "basins_shp.Rds")
      if (file.exists(basins_path)) {
        temp_env <- new.env()
        load(basins_path, envir = temp_env)
        if (exists("basins", envir = temp_env)) {
          outline <- dplyr::filter(temp_env$basins, BASIN == "FRASER")
        }
      }
    }
  }
  if (is.null(outline)) {
    stop("Fr_basin watershed outline not found. Please provide outline argument.")
  }

  # Look up metadata from tbl if not provided
  ind_row <- tbl[tbl$abbrev == indicator_pick, ]
  if (is.null(indicator_name) && nrow(ind_row) > 0) {
    indicator_name <- ind_row$name[1]
  }
  if (is.null(indicator_name)) indicator_name <- indicator_pick

  # Filter data to the specified indicator
  plot_data <- data %>% filter(.data$indicator == indicator_pick)
  
  #filter for mean value only
  plot_data <- plot_data %>% filter(stat == "mean")

  # For GCMs, default to 9 (ensemble mean) or 0 (baseline) or first unique
  if ("gcm" %in% names(plot_data)) {
    if (any(plot_data$gcm == "9", na.rm = TRUE)) {
      plot_data <- plot_data %>% filter(.data$gcm == "9")
    } else if (any(plot_data$gcm == 0, na.rm = TRUE)) {
      plot_data <- plot_data %>% filter(.data$gcm == 0)
    } else {
      # Take first available GCM per CU to avoid duplication
      plot_data <- plot_data %>%
        group_by(FULL_CU_IN) %>%
        slice(1) %>%
        ungroup()
    }
  }

  value_var <- if (use_standardized) "std_value" else "value"

  # It's possible the data doesn't have the raw 'value' column but was requested
  if (value_var == "value" && !"value" %in% names(plot_data)) {
    value_var <- "std_value"
    warning("Raw value not found in data, falling back to std_value.")
  }

  plot_data <- plot_data %>%
    rename(plot_value = !!sym(value_var))

  cu_boundary_plot <- cu_boundary %>%
    left_join(select(plot_data, !!sym(id_col), plot_value, !!sym(sp_col_name)), 
              by = join_by(!!sym(id_col), !!sym(sp_col_name))) %>%
    rename(sp_col = !!sym(sp_col_name)) %>%
    filter(
      !is.na(plot_value),
      sp_col %in% sp_pick
    )

  if (nrow(cu_boundary_plot) == 0) {
    p <- ggplot() +
      geom_sf(data = outline, colour = "black", fill = NA, alpha = 0.3) +
      theme_void() +
      labs(title = paste("No valid data for", indicator_name))
    return(p)
  }

  # Define big and small species groups
  big_spp <- c("Chinook", "Sockeye")
  small_spp <- c("Coho", "Chum", "Pink")

  present_spp <- unique(cu_boundary_plot$sp_col)
  big_spp_present <- intersect(big_spp, present_spp)
  small_spp_present <- intersect(small_spp, present_spp)

  val_range <- range(cu_boundary_plot$plot_value, na.rm = TRUE)
  fill_scale <- scale_fill_cvis(
    palette = risk_palette, 
    direction = palette_direction, 
    limits = val_range,
    name = indicator_pick
  )

  p_big <- NULL
  p_small <- NULL

  # Adjust plot margins negatively to reduce the left/right padding of panel layouts
  if (length(big_spp_present) > 0) {
    p_big <- ggplot() +
      geom_sf(data = filter(cu_boundary_plot, sp_col %in% big_spp_present), aes(fill = plot_value), alpha = 1) +
      fill_scale +
      geom_sf(data = outline, colour = "black", fill = NA, alpha = 0.3) +
      labs(fill = indicator_pick) +
      facet_wrap(~sp_col, ncol = length(big_spp_present)) +
      coord_sf(datum = NA, expand = FALSE, clip = "on") +
      theme_void() +
      theme(
        strip.text = element_text(face = "bold", size = 10, margin = margin(b = 5)),
        plot.margin = margin(t = 2, r = -40, b = 2, l = -40, unit = "pt"),
        panel.spacing = grid::unit(4, "pt")
      )
  }

  if (length(small_spp_present) > 0) {
    p_small <- ggplot() +
      geom_sf(data = filter(cu_boundary_plot, sp_col %in% small_spp_present), aes(fill = plot_value), alpha = 1) +
      fill_scale +
      geom_sf(data = outline, colour = "black", fill = NA, alpha = 0.3) +
      labs(fill = indicator_pick) +
      facet_wrap(~sp_col, ncol = length(small_spp_present)) +
      coord_sf(datum = NA, expand = FALSE, clip = "on") +
      theme_void() +
      theme(
        strip.text = element_text(face = "bold", size = 10, margin = margin(b = 5)),
        plot.margin = margin(t = 2, r = -40, b = 2, l = -40, unit = "pt"),
        panel.spacing = grid::unit(4, "pt")
      )
  }

  # Calculate dynamic legend spacing to pull the legend close to the map
  cols_in_bottom <- if (!is.null(p_small)) length(small_spp_present) else length(big_spp_present)
  spacing_val <- if (cols_in_bottom == 1) {
    -5.0
  } else if (cols_in_bottom == 2) {
    -3.5
  } else if (cols_in_bottom == 3) {
    -2.0
  } else {
    0
  }

  if (!is.null(p_big) && !is.null(p_small)) {
    # Combine with p_big on top and p_small on bottom.
    combined <- patchwork::wrap_plots(p_big, p_small, ncol = 1, heights = c(1.6, 1)) +
      patchwork::plot_layout(guides = "collect") &
      theme(
        legend.position = "right",
        legend.box.spacing = grid::unit(spacing_val, "cm"),
        legend.margin = margin(0, 0, 0, 0),
        plot.margin = margin(0, 0, 0, 0)
      )
  } else if (!is.null(p_big)) {
    combined <- p_big + 
      theme(
        legend.position = "right",
        legend.box.spacing = grid::unit(spacing_val, "cm"),
        legend.margin = margin(0, 0, 0, 0),
        plot.margin = margin(0, 0, 0, 0)
      )
  } else {
    combined <- p_small + 
      theme(
        legend.position = "right",
        legend.box.spacing = grid::unit(spacing_val, "cm"),
        legend.margin = margin(0, 0, 0, 0),
        plot.margin = margin(0, 0, 0, 0)
      )
  }

  return(combined)
}


#' Spatial multi-panel plot of the 6 freshwater rearing indicators for a single species
#'
#' @param data               Long-format table with (at least):
#'   indicator, SPECIES_NAME, FULL_CU_IN, std_value, value, and scenario/period/gcm columns
#' @param cu_boundary        Spatial sf object containing CU boundaries and species column.
#' @param outline            Watershed outline sf object (default: Fr_basin).
#' @param species_pick       The single species to filter for (default: "Chinook").
#' @param sp_col_name        Column name for species in the data and boundary (default: "SPECIES_NAME").
#' @param use_standardized   Logical; if TRUE, uses std_value (0-1), else uses raw value (default: TRUE).
#' @param id_col             CU ID column (default: "FULL_CU_IN").
#' @param risk_palette       Color palette (default: cvis_risk_palette).
#' @param palette_direction  Direction for brewer palette (default: -1).
#' @param ncol               Number of columns in the facet layout (default: 3).
#' @param brewer_palette     Deprecated alias for risk_palette
#' @return A ggplot object.
spatial_fw_rearing_indicators_plot <- function(data,
                                               cu_boundary,
                                               outline = Fr_basin,
                                               species_pick = "Chinook",
                                               sp_col_name = "SPECIES_NAME",
                                               use_standardized = TRUE,
                                               id_col = "FULL_CU_IN",
                                               risk_palette = cvis_risk_palette,
                                               palette_direction = -1,
                                               ncol = 3) {
  
  # The 6 freshwater rearing indicators (excluding fwres)
  fw_indicators <- c("cthr", "tw8rate", "tw8proj", "flow8pdelta", "flow18pdelta", "favchange")
  
  # Filter data to the 6 FW rearing indicators and chosen species
  plot_data <- data %>% 
    filter(
      .data$indicator %in% fw_indicators,
      .data[[sp_col_name]] == species_pick
    )
  
  if (nrow(plot_data) == 0) {
    stop(paste("No data found for species:", species_pick, "and the 6 FW rearing indicators."))
  }
  
  # Filter for mean value only if stat column exists
  if ("stat" %in% names(plot_data)) {
    plot_data <- plot_data %>% filter(stat == "mean")
  }
  
  # Split by indicator to apply dynamic filtering (GCM, RCP, period)
  # This ensures that indicators without projections (e.g. cthr, which only has gcm == 0)
  # are still preserved when filtering other indicators to future RCPs/periods.
  data_list <- split(plot_data, plot_data$indicator)
  for (ind in names(data_list)) {
    df_ind <- data_list[[ind]]
    
    # Select GCM: prefer 9 (ensemble mean) or 0 (baseline) or first unique
    if ("gcm" %in% names(df_ind)) {
      if (any(df_ind$gcm == "9", na.rm = TRUE)) {
        df_ind <- df_ind %>% filter(.data$gcm == "9")
      } else if (any(df_ind$gcm == "0" | df_ind$gcm == 0, na.rm = TRUE)) {
        df_ind <- df_ind %>% filter(.data$gcm == "0" | .data$gcm == 0)
      } else {
        # Take first available GCM per CU to avoid duplication
        df_ind <- df_ind %>%
          group_by(!!sym(id_col)) %>%
          slice(1) %>%
          ungroup()
      }
    }
    
    data_list[[ind]] <- df_ind
  }
  
  plot_data <- bind_rows(data_list)
  
  value_var <- if (use_standardized) "std_value" else "value"
  
  # Fallback to std_value if raw value requested but missing
  if (value_var == "value" && !"value" %in% names(plot_data)) {
    value_var <- "std_value"
    warning("Raw value not found in data, falling back to std_value.")
  }
  
  plot_data <- plot_data %>%
    rename(plot_value = !!sym(value_var))
  
  # Join with spatial boundaries
  cu_boundary_plot <- cu_boundary %>%
    left_join(
      select(plot_data, !!sym(id_col), plot_value, indicator),
      by = id_col
    ) %>%
    filter(!is.na(plot_value))
  
  if (nrow(cu_boundary_plot) == 0) {
    p <- ggplot() +
      geom_sf(data = outline, colour = "black", fill = NA, alpha = 0.3) +
      theme_void() +
      labs(title = paste("No valid data for FW rearing indicators (Species:", species_pick, ")"))
    return(p)
  }
  
  # Add friendly names to indicators if tbl_indicators exists
  if (exists("tbl_indicators")) {
    indicator_names <- tbl_indicators %>% 
      filter(abbrev %in% fw_indicators) %>% 
      select(abbrev, name) %>%
      mutate(
        name = as.character(name),
        clean_name = sub("\\s*\\([^)]*\\)", "", name),
        clean_name = case_when(
          abbrev == "favchange"    ~ "Change in ENM Favourability",
          abbrev == "cthr"         ~ "Cumulative Threat Score",
          abbrev == "tw8proj"      ~ "August Mean Temperature",
          abbrev == "tw8rate"      ~ "Rate of Temp. Change",
          abbrev == "flow8pdelta"  ~ "Change in August Flow",
          abbrev == "flow18pdelta" ~ "Change in Nov-Jan Flow",
          TRUE                     ~ clean_name
        ),
        name_with_abbrev = paste0(clean_name, " (", abbrev, ")"),
        abbrev = factor(abbrev, levels = fw_indicators)
      ) %>%
      arrange(abbrev)
    
    cu_boundary_plot <- cu_boundary_plot %>%
      left_join(select(indicator_names, abbrev, clean_name), by = c("indicator" = "abbrev")) %>%
      mutate(
        indicator_lbl = coalesce(clean_name, indicator),
        indicator_lbl = factor(indicator_lbl, levels = indicator_names$clean_name)
      )
    
    facet_var <- "indicator_lbl"
  } else {
    cu_boundary_plot <- cu_boundary_plot %>%
      mutate(indicator = factor(indicator, levels = fw_indicators))
    facet_var <- "indicator"
  }
  
  p <- ggplot() +
    geom_sf(data = cu_boundary_plot, aes(fill = plot_value), alpha = 1) +
    scale_fill_cvis(
      palette = risk_palette, 
      direction = palette_direction, 
      limits = if (use_standardized) c(0, 1) else NULL
    ) +
    geom_sf(data = outline, colour = "black", fill = NA, alpha = 0.3) +
    labs(
      fill = if (use_standardized) "Standardized\n Risk Score" else "Value"
    ) +
    facet_wrap(vars(!!sym(facet_var)), ncol = ncol) +
    coord_sf(datum = NA, expand = FALSE, clip = "on") +
    theme_void() +
    theme(
      plot.title = element_text(face = "bold", size = 12, hjust = 0.5, margin = margin(b = 10)),
      strip.text = element_text(face = "bold", size = 9, margin = margin(b = 5)),
      panel.spacing = grid::unit(8, "pt"),
      legend.position = "right"
    )
  
  return(p)
}




# ==================== 4. Two-Indicator Scatter Plot Comparison ====================

xy_indicator_plot <- function(data,
                              x_pick = "lowQpdelta",
                              y_pick = "st8pdelta",
                              use_raw = T,
                              point_col = "SPECIES_NAME") {
  plot_data_x <- subset_ind_table(data,
    indicators_choose = x_pick,
    id_col = "CVIS_LABEL",
    sp_col = point_col,
    get_raw = use_raw,
    get_gcm = T
  ) %>%
    rename_ind_table(
      indicator_abbrev = x_pick,
      name_suffix = "x_"
    )

  plot_data_y <- subset_ind_table(data,
    indicators_choose = y_pick,
    id_col = "CVIS_LABEL",
    sp_col = point_col,
    get_raw = use_raw,
    get_gcm = T
  ) %>%
    rename_ind_table(
      indicator_abbrev = y_pick,
      name_suffix = "y_"
    )


  plot_data <- bind_cols(
    plot_data_x,
    select(plot_data_y, contains("y_"))
  )


  p <- ggplot(plot_data) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    geom_errorbar(aes(x = x_raw, y = y_raw, ymin = y_min_gcm, ymax = y_max_gcm), colour = "blue") +
    geom_errorbar(aes(x = x_raw, y = y_raw, xmin = x_min_gcm, xmax = x_max_gcm)) +
    geom_point(aes(x = x_raw, y = y_raw, col = sp), size = 3) +
    labs(
      subtitle = "statistical station model (st8) vs PCIC stream model with GCM Q10-Q90 variation",
      x = "PCIC stream model change in August flow",
      y = "Station model change in August flow"
    )
}



# ==================== 5. WSP Abundance and Status Table (cu_status_table) ====================

#' Create a formatted table of CU conservation status
#'
#' Generates a table showing Wild Salmon Policy status (color-coded as Red/Amber/Green),
#' recent generational average of spawners, most recent assessment year, and data type
#' (relative index or absolute abundance).
#'
#' @param status_data Data frame containing CU status assessment information
#' @param species_filter Optional character vector of species names to include (default: all species)
#' @param sort_by Column to sort by: "status", "abundance", "year", or "cu_name" (default: "status")
#' @param include_confidence Logical, whether to include confidence rating column (default: TRUE)
#' @param show_all_years Logical, whether to show all assessment years or just most recent (default: FALSE)
#'
#' @return A gt table object with formatted CU status information
#'
#' @examples
#' # Basic usage - all CUs
#' cu_status_table(status_data)
#'
#' # Filter to specific species
#' cu_status_table(status_data, species_filter = c("Sockeye", "Chinook"))
#'
#' # Sort by abundance
#' cu_status_table(status_data, sort_by = "abundance")
#'
#' # Exclude confidence rating
#' cu_status_table(status_data, include_confidence = FALSE)
#'
cu_status_table <- function(status_data,
                            species_filter = NULL,
                            sort_by = "cu_name",
                            include_confidence = TRUE,
                            show_all_years = FALSE) {
  # Filter by species if specified
  if (!is.null(species_filter)) {
    status_data <- status_data %>%
      filter(SPECIES_NAME %in% species_filter)
  }

  # Get most recent year for each CU if not showing all years
  if (!show_all_years) {
    status_data <- status_data %>%
      group_by(FULL_CU_IN) %>%
      # Find most recent year with non-NA status
      filter(Year == max(Year[!is.na(RapidStatus)], na.rm = TRUE)) %>%
      ungroup()
  }

  # Prepare table data
  table_data <- status_data %>%
    mutate(
      # Handle missing values
      RapidStatus = if_else(is.na(RapidStatus), "None", RapidStatus),
      ConfidenceRating5 = if_else(is.na(ConfidenceRating5), "None", ConfidenceRating5),

      # Format data type
      DataType_formatted = case_when(
        DataType == "Abs_Abd" ~ "Absolute Abundance",
        DataType == "Rel_Idx" ~ "Relative Index",
        is.na(DataType) ~ "Not Available",
        TRUE ~ as.character(DataType)
      ),

      # Format spawner abundance
      SpawnerAbundance = if_else(
        is.na(SpnForAbd_Wild),
        NA_real_,
        SpnForAbd_Wild
      ),

      # Format generational average
      GenAverage = if_else(
        is.na(GenAvgUsed),
        NA_real_,
        GenAvgUsed
      ),

      # Create status factor for sorting
      status_order = factor(
        RapidStatus,
        levels = c("Red", "Amber", "Green", "None")
      )
    ) %>%
    select(
      # FULL_CU_IN,
      CVIS_LABEL,
      SPECIES_NAME,
      RapidStatus,
      status_order,
      ConfidenceRating5,
      # Year,
      # SpawnerAbundance,
      GenAverage,
      DataType_formatted
      # CyclicCU
    )

  # Sort according to user specification
  table_data <- switch(sort_by,
    "status" = arrange(table_data, status_order, SPECIES_NAME, CVIS_LABEL),
    "abundance" = arrange(table_data, desc(GenAverage)),
    "cu_name" = arrange(table_data, SPECIES_NAME, CVIS_LABEL),
    arrange(table_data, status_order, SPECIES_NAME, CVIS_LABEL) # default
  )

  # Define status colors
  status_colors <- c(
    "Red" = "#DC2626", # Tailwind red-600
    "Amber" = "#F59E0B", # Tailwind amber-500
    "Green" = "#10B981", # Tailwind green-500
    "None" = "#9CA3AF" # Tailwind gray-400
  )

  # Build base table
  gt_table <- table_data %>%
    select(-status_order) %>% # Remove helper column
    gt() %>%
    # Column labels
    cols_label(
      # FULL_CU_IN = "CU ID",
      CVIS_LABEL = "CU Name",
      SPECIES_NAME = "Species",
      RapidStatus = "Status",
      ConfidenceRating5 = "Confidence",
      # Year = "Assessment Year",
      # SpawnerAbundance = "Recent Spawners",
      GenAverage = "Number of mature individuals",
      DataType_formatted = "Data Type"
      # CyclicCU = "Cyclic CU"
    ) %>%
    # Format numbers with commas
    fmt_number(
      columns = c(GenAverage),
      decimals = 0,
      use_seps = TRUE
    ) %>%
    # Color-code the status column
    data_color(
      columns = RapidStatus,
      fn = function(x) {
        status_colors[x]
      },
      apply_to = "fill"
    ) %>%
    # Make status text white for visibility
    tab_style(
      style = cell_text(color = "white", weight = "bold"),
      locations = cells_body(columns = RapidStatus)
    ) %>%
    # Center align specific columns
    cols_align(
      align = "center",
      columns = c(RapidStatus, ConfidenceRating5)
    ) %>%
    # Right align numeric columns
    cols_align(
      align = "right",
      columns = c(GenAverage)
    ) %>%
    # Add table header
    tab_header(
      title = "Conservation Unit Status Summary"
    ) %>%
    # Style the table
    tab_options(
      table.font.size = "small",
      data_row.padding = px(4),
      column_labels.font.weight = "bold",
      row_group.font.weight = "bold",
      heading.title.font.size = px(18),
      heading.subtitle.font.size = px(14),
      source_notes.font.size = px(10)
    ) %>%
    # Add striping for readability
    opt_row_striping()

  # Conditionally hide confidence column if not needed
  if (!include_confidence) {
    gt_table <- gt_table %>%
      cols_hide(columns = ConfidenceRating5)
  }

  return(gt_table)
}




# ==================== 6. Life History Timing Comparison (plot_timing_comparison) ====================

#' Plot life history timing comparison across CUs
#'
#' Creates a single comparative visualization showing start-to-end timing ranges
#' for different life history stages across multiple conservation units. Segments
#' are colored by life stage, and CU names are colored by species.
#'
#' @param cu_timing_long Data frame in long format containing life history timing
#'   data with columns: FULL_CU_IN, CVIS_LABEL, SPECIES_NAME, life_stage, start,
#'   peak, end, dat_qual
#' @param cu_select Character vector of CU IDs to include. If NULL, includes all CUs
#' @param species_select Character vector of species names to include. If NULL, includes all species
#' @param life_stages Character vector of life stages to plot. Options: "spawning",
#'   "run_timing", "ocean_entry", "freshwater_migration". Default is all stages.
#' @param sort_by How to sort CUs: "species" (default), "peak_spawn", "peak_oe", or "none"
#' @param show_peaks Logical, whether to show peak timing points (default TRUE)
#' @param species_palette Named vector of colors for species
#' @param life_stage_palette Named vector of colors for life stages
#' @param date_breaks Character, date breaks for x-axis (e.g., "1 month", "2 weeks")
#' @param y_text_size Numeric, size of y-axis text (default 8)
#'
#' @return A ggplot object
#'
#' @examples
#' # Basic comparison across all CUs
#' plot_timing_comparison(cu_timing_long)
#'
#' # Compare specific species
#' plot_timing_comparison(cu_timing_long, species_select = c("Chinook", "Sockeye"))
#'
#' # Focus on spawning and run timing only
#' plot_timing_comparison(cu_timing_long, life_stages = c("run_timing", "spawning"))
#'
# plot_timing_comparison function has been consolidated and moved to code/5a_plots_CU.R
# to avoid code redundancy and ensure availability in the Shiny app.


# ==================== 7. Combined Tile Plot for All CUs & Indicators ====================

#' Tile plot of all indicators and CUs, plus overall score (catavg)
#'
#' @param all_std_long Standardized indicators long format table
#' @param scores_tidy Combined scores table from 4a
#' @param gcm_pick Character, GCM code (default "9" for ensemble)
#' @param risk_palette Character, color palette (default cvis_risk_palette gives green to red with direction -1)
#' @param palette_direction Numeric, direction of palette
#' @param brewer_palette Deprecated alias for risk_palette
indicator_cu_tile_plot <- function(all_std_long,
                                   scores_tidy,
                                   indicators_metadata = tbl_indicators,
                                   gcm_pick = "9",
                                   dsmodel_pick = NULL,
                                   risk_palette = cvis_risk_palette,
                                   palette_direction = -1,
                                   cu_code_emphasize = NULL) {
  # 1. Filter indicators dynamically with fallback to baseline (0)
  d_ind_list <- split(all_std_long, all_std_long$indicator)
  for (ind in names(d_ind_list)) {
    df_temp <- d_ind_list[[ind]]
    
    if (!is.null(dsmodel_pick) && "dsmodel" %in% names(df_temp) && length(dsmodel_pick) > 0) {
      if (any(df_temp$dsmodel %in% dsmodel_pick, na.rm = TRUE)) {
        df_temp <- df_temp %>% filter(dsmodel %in% dsmodel_pick)
      }
    }
    
    if ("gcm" %in% names(df_temp)) {
      if (any(df_temp$gcm == gcm_pick, na.rm = TRUE)) {
        df_temp <- df_temp %>% filter(gcm == gcm_pick)
      } else if (any(df_temp$gcm == 0, na.rm = TRUE)) {
        df_temp <- df_temp %>% filter(gcm == 0)
      }
    }
    
    d_ind_list[[ind]] <- df_temp
  }
  
  ind_dat <- bind_rows(d_ind_list) %>%
    group_by(FULL_CU_IN, CVIS_LABEL, CU_COMMON_NAME, SMU_SIMPLE, SPECIES_NAME, indicator) %>%
    slice_head(n = 1) %>%
    ungroup() %>%
    select(FULL_CU_IN, CVIS_LABEL, CU_COMMON_NAME, SMU_SIMPLE, SPECIES_NAME, indicator, value = std_value)
  
  if (!is.null(indicators_metadata) && "abbrev" %in% names(indicators_metadata) && "category" %in% names(indicators_metadata)) {
    meta_df <- indicators_metadata %>% select(indicator = abbrev, category)
    ind_dat <- ind_dat %>%
      left_join(meta_df, by = "indicator") %>%
      mutate(category = ifelse(is.na(category), "Other", category))
  } else {
    ind_dat <- ind_dat %>% mutate(category = "Other")
  }
  
  # 2. Filter scores
  score_dat <- scores_tidy
  
  score_dat <- score_dat %>%
    filter(method == "catavg", category == "all")
  
  if ("gcm" %in% names(score_dat)) {
    if (any(score_dat$gcm == gcm_pick, na.rm = TRUE)) {
      score_dat <- score_dat %>% filter(gcm == gcm_pick)
    } else if (any(score_dat$gcm == 0, na.rm = TRUE)) {
      score_dat <- score_dat %>% filter(gcm == 0)
    }
  }
  
  score_dat <- score_dat %>%
    group_by(FULL_CU_IN, CVIS_LABEL, CU_COMMON_NAME, SMU_SIMPLE, SPECIES_NAME) %>%
    slice_head(n = 1) %>%
    ungroup() %>%
    select(FULL_CU_IN, CVIS_LABEL, CU_COMMON_NAME, SMU_SIMPLE, SPECIES_NAME, value = score100_all) %>%
    mutate(value = value / 100, indicator = "catavg", category = "overall")
  
  # Combine
  plot_dat <- bind_rows(ind_dat, score_dat)
  
  # Set desired species order (Chinook first, then Chum)
  sp_base_order <- c("Chinook", "Chum", "Coho", "Sockeye", "Pink")
  plot_dat <- plot_dat %>%
    mutate(SPECIES_NAME = factor(SPECIES_NAME, levels = intersect(sp_base_order, unique(SPECIES_NAME))))
  
  
  # Order CUs by SPECIES_NAME then by SMU then by FULL_CU_IN, then form colored labels
  cu_order <- plot_dat %>%
    distinct(FULL_CU_IN, CVIS_LABEL, SPECIES_NAME, SMU_SIMPLE) %>%
    arrange(SPECIES_NAME, SMU_SIMPLE, FULL_CU_IN) %>%
    mutate(
      color = smu_palette[as.character(SMU_SIMPLE)],
      color = ifelse(is.na(color), "black", color),
      colored_label = if (!is.null(cu_code_emphasize)) {
        ifelse(FULL_CU_IN == cu_code_emphasize,
               paste0("<strong>➔ <span style='color:", color, "'>", CVIS_LABEL, "</span></strong>"),
               paste0("<span style='color:", color, "'>", CVIS_LABEL, "</span>"))
      } else {
        paste0("<span style='color:", color, "'>", CVIS_LABEL, "</span>")
      }
    )
  
  plot_dat <- plot_dat %>%
    left_join(select(cu_order, FULL_CU_IN, colored_label), by = "FULL_CU_IN") %>%
    mutate(colored_label = factor(colored_label, levels = rev(cu_order$colored_label)))
  
  # Make colored species labels for the facets
  sp_levels <- plot_dat %>%
    distinct(SPECIES_NAME) %>%
    arrange(SPECIES_NAME) %>%
    mutate(
      color = species_palette[as.character(SPECIES_NAME)],
      color = ifelse(is.na(color), "black", color),
      colored_species = paste0("<span style='color:", color, "'>", SPECIES_NAME, "</span>")
    )
  
  plot_dat <- plot_dat %>%
    left_join(select(sp_levels, SPECIES_NAME, colored_species), by = "SPECIES_NAME") %>%
    mutate(colored_species = factor(colored_species, levels = sp_levels$colored_species))
  
  type_labels <- c(
    "overall" = "Overall",
    "fwrs" = "Freshwater",
    "migr" = "Migration",
    "dem" = "Demogr.",
    "gen" = "Genetics",
    "mar" = "Marine",
    "Other" = "Other"
  )
  
  plot_dat <- plot_dat %>%
    mutate(type_label = type_labels[as.character(category)]) %>%
    mutate(type_label = ifelse(is.na(type_label), as.character(category), type_label)) %>%
    mutate(type_label = factor(type_label, levels = unique(c(type_labels, unique(type_label)))))
  
  ind_summary <- plot_dat %>%
    distinct(indicator, type_label) %>%
    arrange(type_label, indicator)
  
  plot_dat <- plot_dat %>%
    mutate(indicator = factor(indicator, levels = ind_summary$indicator))
  
  n_cus <- length(unique(plot_dat$FULL_CU_IN))
  y_text_size <- if (n_cus > 40) 8 else if (n_cus > 20) 10 else 11
  
  tile_text_size <- if (n_cus > 40) 1.6 else if (n_cus > 20) 2.0 else 2.4
  
  # Plot
  p <- ggplot(plot_dat, aes(x = indicator, y = colored_label)) +
    geom_tile(aes(fill = value), color = "white", linewidth = 0.3)
    
  # Add border outline around the emphasized CU if specified
  if (!is.null(cu_code_emphasize)) {
    emp_dat <- plot_dat %>% filter(FULL_CU_IN == cu_code_emphasize)
    if (nrow(emp_dat) > 0) {
      p <- p + geom_tile(data = emp_dat, aes(x = indicator, y = colored_label),
                         color = "black", linewidth = 1.2, fill = NA, inherit.aes = FALSE)
    }
  }

  p <- p +
    geom_text(aes(label = ifelse(is.na(value), "", sprintf("%.1f", value)),
                  color = ifelse(value > 0.7 | value < 0.25, "white", "black")),
              size = tile_text_size, fontface = "bold", na.rm = TRUE) +
    scale_color_identity() +
    scale_fill_cvis(
      palette = risk_palette, direction = palette_direction,
      limits = c(0, 1), na.value = "grey95"
    ) +
    theme_cvis(base_size = 12) +
    theme(
      axis.line = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 10, color = "black"),
      axis.text.y = ggtext::element_markdown(size = y_text_size, hjust = 1, vjust = 0.5),
      axis.text.y.left = ggtext::element_markdown(size = y_text_size, hjust = 1, vjust = 0.5),
      legend.position = "none",
      legend.title = element_text(face = "bold", size = 11),
      legend.text = element_text(size = 10),
      panel.grid = element_blank(),
      strip.background = element_blank(),
      strip.text.x.bottom = element_text(face = "bold", size = 8, angle = 0, hjust = 0.5, vjust = 1, margin = margin(t = 2, b = 0)),
      strip.text.y = ggtext::element_markdown(size = 11, angle = 90, margin = margin(r = 5, l = 0), hjust = 0.5, vjust = 0.5),
      strip.text.y.left = ggtext::element_markdown(size = 11, angle = 90, margin = margin(r = 5, l = 0), hjust = 0.5, vjust = 0.5),
      panel.spacing.y = unit(0.2, "lines"),
      panel.spacing.x = unit(0.3, "lines"),
      plot.margin = margin(10, 10, 10, 10),
      strip.placement = "outside",
      strip.clip = "off"
    ) +
    labs(
      x = NULL,
      y = NULL,
      fill = "Vulnerability\n Score",
      title = NULL
    ) +
    facet_grid(colored_species ~ type_label, scales = "free", space = "free", switch = "both") +
    coord_cartesian(clip = "off")
  
  return(p)
}

# ==================== 8. Combined Tile Plot for Comparing Scoring Methods ====================

#' Tile plot comparing category-level and overall vulnerability scores across different methods
#'
#' @param scores_tidy Combined scores table from 4a (e.g. scores_tidy_baseline)
#' @param gcm_pick Character, GCM code (default "9" for ensemble)
#' @param brewer_palette Character, color palette (default "RdYlGn" gives green to red with direction -1)
#' @param palette_direction Numeric, direction of palette
#' @return A ggplot object
plot_methods_compare_tile <- function(scores_tidy,
                                      gcm_pick = "9",
                                      risk_palette = cvis_risk_palette,
                                      palette_direction = -1) {
  
  # ---- Input checks ----
  req_cols <- c("FULL_CU_IN", "SPECIES_NAME", "CVIS_LABEL", "CU_COMMON_NAME", "SMU_SIMPLE", "method", "category", "score100_all")
  if (!all(req_cols %in% names(scores_tidy))) {
    stop("Missing required columns in 'scores_tidy': ", paste(setdiff(req_cols, names(scores_tidy)), collapse = ", "))
  }

  # ---- GCM filtering ----
  score_dat <- scores_tidy
  if ("gcm" %in% names(score_dat)) {
    if (any(score_dat$gcm == gcm_pick, na.rm = TRUE)) {
      score_dat <- score_dat %>% filter(gcm == gcm_pick)
    } else if (any(score_dat$gcm == 0, na.rm = TRUE)) {
      score_dat <- score_dat %>% filter(gcm == 0)
    }
  }

  # ---- Filter to comparison methods and categories ----
  # Category-level: average (avg), cubic mean (cube), and soft red flags (flag)
  # Overall: average of averages (catavg), average of all (avgall), average of cubic means (avgcube), and total flags (flag)
  plot_dat <- score_dat %>%
    filter(
      (category %in% c("fwrs", "migr", "mar", "dem", "gen") & method %in% c("avg", "cube", "flag")) |
      (category == "all" & method %in% c("catavg", "avgall", "avgcube", "flag"))
    )

  if (nrow(plot_dat) == 0L) {
    stop("No rows in scores_tidy after filtering for scoring methods and categories.")
  }

  # Category labels mapping (matching CVIS categories)
  type_labels <- c(
    "all" = "Overall",
    "fwrs" = "Freshwater",
    "migr" = "Migration",
    "dem" = "Demographics",
    "gen" = "Genetics",
    "mar" = "Marine"
  )
  
  # Map method name dynamically to handle flag label at category vs overall level
  plot_dat <- plot_dat %>%
    mutate(
      method_name = case_when(
        method == "avg" ~ "Average",
        method == "cube" ~ "Cubic Mean",
        method == "flag" & category == "all" ~ "Total Flags",
        method == "flag" ~ "Red Flags",
        method == "catavg" ~ "Avg of Averages",
        method == "avgall" ~ "Avg of All",
        method == "avgcube" ~ "Avg of Cubic Means",
        TRUE ~ method
      )
    )

  plot_dat <- plot_dat %>%
    mutate(
      type_label = factor(type_labels[category], levels = c("Freshwater", "Migration", "Marine", "Demographics", "Genetics", "Overall")),
      method_label = factor(method_name, levels = c("Average", "Cubic Mean", "Red Flags", "Avg of Averages", "Avg of All", "Avg of Cubic Means", "Total Flags"))
    )

  # Set desired species order
  sp_base_order <- c("Chinook", "Chum", "Coho", "Sockeye", "Pink")
  plot_dat <- plot_dat %>%
    mutate(SPECIES_NAME = factor(SPECIES_NAME, levels = intersect(sp_base_order, unique(SPECIES_NAME))))

  species_palette <- get("species_palette", envir = .GlobalEnv)

  # Order CUs by SPECIES_NAME then by SMU then by FULL_CU_IN, then form colored labels
  cu_order <- plot_dat %>%
    distinct(FULL_CU_IN, CVIS_LABEL, SPECIES_NAME, SMU_SIMPLE) %>%
    arrange(SPECIES_NAME, SMU_SIMPLE, FULL_CU_IN) %>%
    mutate(
      color = smu_palette[as.character(SMU_SIMPLE)],
      color = ifelse(is.na(color), "black", color),
      colored_label = paste0("<span style='color:", color, "'>", CVIS_LABEL, "</span>")
    )

  plot_dat <- plot_dat %>%
    left_join(select(cu_order, FULL_CU_IN, colored_label), by = "FULL_CU_IN") %>%
    mutate(colored_label = factor(colored_label, levels = rev(cu_order$colored_label)))

  # Make colored species labels for the facets
  sp_levels <- plot_dat %>%
    distinct(SPECIES_NAME) %>%
    arrange(SPECIES_NAME) %>%
    mutate(
      color = species_palette[as.character(SPECIES_NAME)],
      color = ifelse(is.na(color), "black", color),
      colored_species = paste0("<span style='color:", color, "'>", SPECIES_NAME, "</span>")
    )

  plot_dat <- plot_dat %>%
    left_join(select(sp_levels, SPECIES_NAME, colored_species), by = "SPECIES_NAME") %>%
    mutate(colored_species = factor(colored_species, levels = sp_levels$colored_species))

  n_cus <- length(unique(plot_dat$FULL_CU_IN))
  y_text_size <- if (n_cus > 40) 8 else if (n_cus > 20) 10 else 11

  tile_text_size <- if (n_cus > 40) 1.8 else if (n_cus > 20) 2.2 else 2.6
  
  # Plot
  p <- ggplot(plot_dat, aes(x = method_label, y = colored_label)) +
    geom_tile(aes(fill = score100_all), color = "white", linewidth = 0.3) +
    geom_text(aes(label = ifelse(is.na(score100_all), "", sprintf("%.0f", score100_all)),
                  color = ifelse(score100_all > 70 | score100_all < 25, "white", "black")),
              size = tile_text_size, fontface = "bold", na.rm = TRUE) +
    scale_color_identity() +
    scale_fill_cvis(
      palette = risk_palette, direction = palette_direction,
      limits = c(0, 100), na.value = "grey95"
    ) +
    theme_cvis(base_size = 12) +
    theme(
      axis.line = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 9, color = "black"),
      axis.text.y = ggtext::element_markdown(size = y_text_size, hjust = 1, vjust = 0.5),
      axis.text.y.left = ggtext::element_markdown(size = y_text_size, hjust = 1, vjust = 0.5),
      legend.position = "none",
      legend.title = element_text(face = "bold", size = 11),
      legend.text = element_text(size = 10),
      panel.grid = element_blank(),
      strip.background = element_blank(),
      strip.text.x.bottom = element_text(face = "bold", size = 9, angle = 0, hjust = 0.5, vjust = 1, margin = margin(t = 2, b = 0)),
      strip.text.y = ggtext::element_markdown(size = 11, angle = 90, margin = margin(r = 5, l = 0), hjust = 0.5, vjust = 0.5),
      strip.text.y.left = ggtext::element_markdown(size = 11, angle = 90, margin = margin(r = 5, l = 0), hjust = 0.5, vjust = 0.5),
      panel.spacing.y = unit(0.2, "lines"),
      panel.spacing.x = unit(0.3, "lines"),
      plot.margin = margin(10, 10, 10, 10),
      strip.placement = "outside",
      strip.clip = "off"
    ) +
    labs(
      x = NULL,
      y = NULL,
      fill = "Vulnerability\n Score (0-100)",
      title = NULL
    ) +
    facet_grid(colored_species ~ type_label, scales = "free", space = "free", switch = "both") +
    coord_cartesian(clip = "off")

  return(p)
}

# 
#
# plot_methods_compare_tile(scores_tidy_baseline)
# 







#' Combined Marine Adaptive Zone (MAZ) Indicators Map (Regional & Local Point-Level)
#'
#' @param maz_all            Marine Adaptive Zone indicator dataset (raw values).
#' @param MAZ                Marine Adaptive Zone spatial sf object.
#' @param CMIP6_SST          Optional CMIP6 SST points sf dataset.
#' @param CImpact_points     Optional Cumulative impacts points sf dataset.
#' @param selected_maz       MAZ area to show high-resolution points for (default: "GStr").
#' @param outline            Optional background coastline sf object.
#' @param use_standardized   If TRUE, standardizes raw values to a 0-1 risk score.
#' @param brewer_palette     RColorBrewer palette (default: cvis_risk_palette).
#' @param palette_direction  Direction for brewer palette (default: -1).
#' @return A patchwork ggplot object.
combined_maz_marine_plot <- function(maz_all,
                                     MAZ,
                                     CMIP6_SST = NULL,
                                     CImpact_points = NULL,
                                     selected_maz = "GStr",
                                     outline = NULL,
                                     use_standardized = FALSE,
                                     risk_palette = cvis_risk_palette,
                                     palette_direction = -1) {

  # 1. Filter out Offshore MAZ for regional view
  MAZ_reg <- MAZ %>% filter(MAZ_Acrony != "Offshore")

  # 2. Get local MAZ boundary
  MAZ_local <- MAZ %>% filter(MAZ_Acrony == selected_maz)
  if (nrow(MAZ_local) == 0) {
    stop(paste("Selected MAZ", selected_maz, "not found in MAZ dataset."))
  }

  # 3. Load/determine background outline (coastline)
  if (is.null(outline)) {
    if (exists("bc_coast", envir = .GlobalEnv)) {
      outline <- get("bc_coast", envir = .GlobalEnv)
    } else if (exists("paths") && !is.null(paths$marine) && file.exists(file.path(paths$marine, "bc_coast.Rds"))) {
      outline <- readRDS(file.path(paths$marine, "bc_coast.Rds"))
    } else {
      outline <- pacea::bc_coast
    }
  }
  outline_proj <- sf::st_transform(outline, sf::st_crs(MAZ))

  # 4. Load point-level datasets if NULL
  if (is.null(CMIP6_SST)) {
    if (exists("CMIP6_SST", envir = .GlobalEnv)) {
      CMIP6_SST <- get("CMIP6_SST", envir = .GlobalEnv)
    } else if (exists("paths") && !is.null(paths$marine) && file.exists(file.path(paths$marine, "CMIP6_SST_periods.Rds"))) {
      load(file.path(paths$marine, "CMIP6_SST_periods.Rds"))
    } else {
      stop("CMIP6_SST data not found.")
    }
  }
  if (is.null(CImpact_points)) {
    if (exists("CImpact_points", envir = .GlobalEnv)) {
      CImpact_points <- get("CImpact_points", envir = .GlobalEnv)
    } else if (exists("paths") && !is.null(paths$marine) && file.exists(file.path(paths$marine, "CImpact_points.Rds"))) {
      load(file.path(paths$marine, "CImpact_points.Rds"))
    } else {
      stop("CImpact_points data not found.")
    }
  }

  # 5. Prep maz_all for standardization
  maz_prep <- maz_all %>%
    filter(MAZ != "Offshore") %>%
    mutate(
      FULL_CU_IN = as.character(MAZ),
      SPECIES_NAME = "Chinook",
      rcp = as.character(rcp),
      period_code = as.character(period_code),
      gcm = as.character(gcm),
      value = as.numeric(value)
    )

  grouping_vars_pick <- c("gcm", "rcp", "period_code", "dsmodel")

  # Standardize each indicator using tbl_standardize parameters
  std_results <- list()
  for (ind in c("SSTproj", "SSTrate", "CImpact")) {
    row_idx <- which(tbl_standardize$abbrev == ind)
    std_params_i <- as.list(tbl_standardize[row_idx, ])
    
    std_res <- standardize_long_indicator(
      data = maz_prep,
      calibration_data = maz_prep,
      grouping_vars = grouping_vars_pick,
      indicator_pick = ind,
      std_fun = tbl_standardize$std_fun[row_idx],
      std_params = std_params_i,
      calibration_gcm = "9"
    )
    
    default_method <- if (tbl_standardize$std_fun[row_idx] %in% c("linear_std", "invlinear_std")) "linear" else "exponential"
    std_res <- std_res %>% mutate(std_method = default_method)
    std_results[[ind]] <- std_res
  }
  std_maz_all <- bind_rows(std_results)

  # Filter to baseline scenario
  baseline_maz_std <- std_maz_all %>%
    filter(
      (indicator == "CImpact" & rcp == "0" & period_code == "0" & gcm == "0" & stat == "mean") |
      (indicator %in% c("SSTproj", "SSTrate") & rcp == "45" & period_code == "3" & gcm == "9" & stat == "mean" & dsmodel == "qdm")
    )

  value_var <- if (use_standardized) "std_value" else "value"
  baseline_maz_std <- baseline_maz_std %>%
    rename(plot_value = !!sym(value_var))

  # 6. Prep Point-Level Data for local zoom of selected MAZ
  start_month <- if (exists("ns_start_static", envir = .GlobalEnv)) get("ns_start_static", envir = .GlobalEnv) else 3
  end_month <- if (exists("ns_end_static", envir = .GlobalEnv)) get("ns_end_static", envir = .GlobalEnv) else 5
  months_include <- seq(start_month, end_month)
  month_chars <- sprintf("%02d", months_include)
  month_cols <- paste0("SST_", month_chars)

  p0 <- CMIP6_SST %>% filter(MAZ_Acrony == selected_maz, period_code == 0)
  p3 <- CMIP6_SST %>% filter(MAZ_Acrony == selected_maz, period_code == 3, rcp == "45")

  if (nrow(p0) > 0 && nrow(p3) > 0) {
    coords_0 <- as.data.frame(st_coordinates(p0))
    p0$X <- coords_0$X
    p0$Y <- coords_0$Y
    p0_df <- p0 %>%
      st_drop_geometry() %>%
      mutate(SST_hist = rowMeans(across(all_of(month_cols)), na.rm = TRUE)) %>%
      select(X, Y, SST_hist)

    coords_3 <- as.data.frame(st_coordinates(p3))
    p3$X <- coords_3$X
    p3$Y <- coords_3$Y

    sst_points <- p3 %>%
      mutate(SSTproj = rowMeans(across(all_of(month_cols)), na.rm = TRUE)) %>%
      left_join(p0_df, by = c("X", "Y")) %>%
      mutate(SSTrate = (SSTproj - SST_hist) / 5.5)
  } else {
    sst_points <- p3 %>% mutate(SSTproj = NA_real_, SSTrate = NA_real_)
  }

  ci_points <- CImpact_points %>% filter(MAZ_Acrony == selected_maz)

  # Standardize points if needed
  get_calib_limits <- function(ind_name, rcp_val, period_val, gcm_val) {
    val <- maz_prep %>%
      filter(indicator == ind_name, rcp == rcp_val, period_code == period_val, gcm == gcm_val, stat == "mean") %>%
      pull(value)
    val <- val[!is.na(val)]
    row_idx <- which(tbl_standardize$abbrev == ind_name)
    std_params <- as.list(tbl_standardize[row_idx, ])
    use_95 <- if (!is.null(std_params$use_95)) std_params$use_95 else TRUE
    xmin_val <- std_params$xmin
    xmax_val <- std_params$xmax
    if (is.na(xmin_val)) {
      xmin_val <- min(val, na.rm = TRUE)
      if (use_95) xmin_val <- unlist(quantile(val, na.rm = T, probs = 0.025))
    }
    if (is.na(xmax_val)) {
      xmax_val <- max(val, na.rm = TRUE)
      if (use_95) xmax_val <- unlist(quantile(val, na.rm = T, probs = 0.975))
    }
    list(xmin = xmin_val, xmax = xmax_val)
  }

  std_value_func <- function(x, ind_name, xmin_val, xmax_val) {
    row_idx <- which(tbl_standardize$abbrev == ind_name)
    std_fun_name <- tbl_standardize$std_fun[row_idx]
    std_fun <- get(std_fun_name)
    std_params <- as.list(tbl_standardize[row_idx, ])
    std_fun(x, xmin = xmin_val, xmax = xmax_val, lambda = std_params$lambda)
  }

  if (use_standardized) {
    lims_proj <- get_calib_limits("SSTproj", "45", 3, "9")
    sst_points$plot_value_SSTproj <- std_value_func(sst_points$SSTproj, "SSTproj", lims_proj$xmin, lims_proj$xmax)

    lims_rate <- get_calib_limits("SSTrate", "45", 3, "9")
    sst_points$plot_value_SSTrate <- std_value_func(sst_points$SSTrate, "SSTrate", lims_rate$xmin, lims_rate$xmax)

    lims_ci <- get_calib_limits("CImpact", "0", 0, "0")
    ci_points$plot_value_CImpact <- std_value_func(ci_points$Cumul_Impact_ALL, "CImpact", lims_ci$xmin, lims_ci$xmax)
  } else {
    sst_points$plot_value_SSTproj <- sst_points$SSTproj
    sst_points$plot_value_SSTrate <- sst_points$SSTrate
    ci_points$plot_value_CImpact <- ci_points$Cumul_Impact_ALL
  }

  # 7. Compute bounding boxes
  bbox_reg <- sf::st_bbox(MAZ_reg)
  x_range_reg <- bbox_reg["xmax"] - bbox_reg["xmin"]
  y_range_reg <- bbox_reg["ymax"] - bbox_reg["ymin"]
  margin_reg <- 0.05
  xlims_reg <- c(bbox_reg["xmin"] - margin_reg * x_range_reg, bbox_reg["xmax"] + margin_reg * x_range_reg)
  ylims_reg <- c(bbox_reg["ymin"] - margin_reg * y_range_reg, bbox_reg["ymax"] + margin_reg * y_range_reg)

  bbox_local <- sf::st_bbox(MAZ_local)
  x_range_local <- bbox_local["xmax"] - bbox_local["xmin"]
  y_range_local <- bbox_local["ymax"] - bbox_local["ymin"]
  margin_local <- 0.05
  xlims_local <- c(bbox_local["xmin"] - margin_local * x_range_local, bbox_local["xmax"] + margin_local * x_range_local)
  ylims_local <- c(bbox_local["ymin"] - margin_local * y_range_local, bbox_local["ymax"] + margin_local * y_range_local)

  # 8. Loop and build maps
  plot_list <- list()
  indicators_picks <- c("SSTproj", "SSTrate", "CImpact")

  for (i in seq_along(indicators_picks)) {
    ind <- indicators_picks[i]
    
    # Filter data for this indicator (regional)
    dat_ind <- baseline_maz_std %>% filter(indicator == ind)
    
    # Join with spatial sf object
    maz_sf_ind <- MAZ_reg %>%
      left_join(
        select(dat_ind, MAZ_Acrony = MAZ, plot_value),
        by = "MAZ_Acrony"
      ) %>%
      filter(!is.na(plot_value))
    
    # Get friendly name and unit
    ind_name <- ind
    ind_unit <- ""
    if (exists("tbl_indicators")) {
      ind_row <- tbl_indicators %>% filter(abbrev == ind)
      if (nrow(ind_row) > 0) {
        ind_name <- ind_row$name[1]
        ind_unit <- ind_row$unit[1]
      }
    }
    
    # Condense legend titles
    legend_label <- if (use_standardized) {
      "Risk Score"
    } else {
      if (ind == "SSTproj") {
        "SST (°C)"
      } else if (ind == "SSTrate") {
        "°C/decade"
      } else if (ind == "CImpact") {
        "Score"
      } else {
        if (is.na(ind_unit) || ind_unit == "") "Value" else ind_unit
      }
    }

    val_range <- range(dat_ind$plot_value, na.rm = TRUE)

    # Top Plot (Regional MAZ map)
    p_reg <- ggplot() +
      geom_sf(data = outline_proj, fill = "grey90", color = "grey75", linewidth = 0.3) +
      geom_sf(data = maz_sf_ind, aes(fill = plot_value), color = "black", linewidth = 0.4) +
      geom_sf(data = MAZ_local, fill = NA, color = "black", linewidth = 1.0) + # Highlight local MAZ
      scale_fill_cvis(
        palette = risk_palette, 
        direction = palette_direction,
        limits = if (use_standardized) c(0, 1) else val_range,
        oob = scales::squish
      ) +
      labs(
        title = ind_name,
        fill = legend_label
      ) +
      coord_sf(xlim = xlims_reg, ylim = ylims_reg, datum = NA, expand = FALSE) +
      theme_void() +
      theme(
        plot.title = element_text(face = "bold", size = 10, hjust = 0.5, margin = margin(b = 6)),
        legend.position = "right",
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 7.5),
        panel.spacing = grid::unit(12, "pt")
      )
      
    # Bottom Plot (Local points map inside selected MAZ)
    local_points <- if (ind == "CImpact") {
      ci_points %>% rename(plot_val_col = plot_value_CImpact)
    } else if (ind == "SSTproj") {
      sst_points %>% rename(plot_val_col = plot_value_SSTproj)
    } else {
      sst_points %>% rename(plot_val_col = plot_value_SSTrate)
    }

    local_title <- if (ind == "SSTproj") {
      paste("Projected SST (", selected_maz, ")", sep = "")
    } else if (ind == "SSTrate") {
      paste("SST Rate (", selected_maz, ")", sep = "")
    } else {
      paste("Cumulative Impacts (", selected_maz, ")", sep = "")
    }

    p_local <- ggplot() +
      geom_sf(data = outline_proj, fill = "grey90", color = "grey75", linewidth = 0.3) +
      geom_sf(data = MAZ_local, fill = NA, color = "black", linewidth = 0.5) +
      geom_sf(data = local_points, aes(color = plot_val_col), size = 1.2, alpha = 0.8) +
      scale_color_cvis(
        palette = risk_palette, 
        direction = palette_direction,
        limits = if (use_standardized) c(0, 1) else val_range,
        oob = scales::squish
      ) +
      labs(
        title = local_title,
        color = legend_label
      ) +
      coord_sf(xlim = xlims_local, ylim = ylims_local, datum = NA, expand = FALSE) +
      theme_void() +
      theme(
        plot.title = element_text(face = "bold", size = 10, hjust = 0.5, margin = margin(b = 6)),
        legend.position = "right",
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 7.5),
        panel.spacing = grid::unit(12, "pt")
      )

    plot_list[[i]] <- p_reg
    plot_list[[i + 3]] <- p_local
  }

  # Combine plots using patchwork: 2 rows of 3 columns
  p <- patchwork::wrap_plots(plot_list, ncol = 3)
  return(p)
}


# ==================== 9. Migration Timing Comparison (migration_compare_plot) ====================

migration_compare_plot <- function(migr_daily_all,
                                   timing = NULL,
                                   rcp = "45",
                                   period_choose = c("1981-2010", "2041-2060"),
                                   spatial_path_choose = "CK-12",
                                   min_stream_order = 9) {
  

  # Species palette
  spp_colors <- get("species_palette", envir = .GlobalEnv)

  # Summary plot mode for all CUs
  if (is.null(timing)) {
    if (exists("cu_timing_Fr", envir = .GlobalEnv)) {
      df_timing <- get("cu_timing_Fr", envir = .GlobalEnv)
    } else {
      stop("cu_timing_Fr timing data not found in global environment.")
    }
  } else {
    df_timing <- timing
  }

  df_timing <- df_timing %>%
    dplyr::filter(!is.na(rt_start), !is.na(sp_peak), !is.na(SPECIES_NAME)) %>%
    dplyr::arrange(SPECIES_NAME, rt_start) %>%
    dplyr::mutate(
      label_color = spp_colors[as.character(SPECIES_NAME)],
      id_label_html = paste0("<span style='color:", label_color, "'>", CVIS_LABEL, "</span>"),
      CU_label = factor(id_label_html, levels = unique(id_label_html))
    )

  # Retrieve baseline scenario migration indicator values (migrTproj, migrdist, migrQpdelta)
  if (!exists("all_std_long_baseline", envir = .GlobalEnv)) {
    if (exists("paths") && !is.null(paths$output) && file.exists(file.path(paths$output, "scoring_results.Rdata"))) {
      load(file.path(paths$output, "scoring_results.Rdata"), envir = .GlobalEnv)
    }
  }

  if (exists("all_std_long_baseline", envir = .GlobalEnv)) {
    baseline_df <- get("all_std_long_baseline", envir = .GlobalEnv)
    migr_raw_data <- baseline_df %>%
      dplyr::filter(indicator %in% c("migrTproj", "migrdist", "migrQpdelta"), stat == "mean") %>%
      dplyr::select(FULL_CU_IN, indicator, value, std_value)
  } else {
    migr_raw_data <- data.frame(FULL_CU_IN = character(), indicator = character(), value = numeric(), std_value = numeric())
  }

  cu_labels_map <- df_timing %>%
    dplyr::select(FULL_CU_IN, CU_label) %>%
    dplyr::distinct()

  grid_df <- expand.grid(
    FULL_CU_IN = unique(df_timing$FULL_CU_IN),
    indicator = c("migrTproj", "migrQpdelta", "migrdist"),
    stringsAsFactors = FALSE
  )

  migr_ind_data <- grid_df %>%
    dplyr::left_join(migr_raw_data, by = c("FULL_CU_IN", "indicator")) %>%
    dplyr::mutate(
      label_text = case_when(
        is.na(value) ~ "",
        indicator == "migrTproj" ~ sprintf("%.1f°C", value),
        indicator == "migrdist"  ~ sprintf("%.0f km", value / 1000),
        indicator == "migrQpdelta" ~ sprintf("%.2f", value),
        TRUE ~ sprintf("%.1f", value)
      ),
      indicator_label = factor(indicator, levels = c("migrTproj", "migrdist", "migrQpdelta"))
    ) %>%
    dplyr::inner_join(cu_labels_map, by = "FULL_CU_IN")

  # Temperature data from calendar
  if (exists("migr_daily_calendar", envir = .GlobalEnv)) {
    calendar_df <- get("migr_daily_calendar", envir = .GlobalEnv)
  } else if (is.data.frame(migr_daily_all) && "day_of_year" %in% names(migr_daily_all)) {
    calendar_df <- migr_daily_all
  } else {
    # Try to load it from migr_stats.Rdata if it exists
    if (exists("paths") && !is.null(paths$fw)) {
      load(file.path(paths$fw, "migr_stats.Rdata"))
      calendar_df <- migr_daily_calendar
    } else {
      stop("migr_daily_calendar not found. Please load migr_stats.Rdata first or pass it.")
    }
  }

  # Filter to only spatial_path == spatial_path_choose
  calendar_sub <- calendar_df %>%
    dplyr::filter(spatial_path == spatial_path_choose, rcp == !!rcp, period %in% period_choose) %>%
    dplyr::mutate(day_of_year = as.numeric(day_of_year))

  # Ensemble mean line
  df_temp_ensemble <- calendar_sub %>%
    dplyr::filter(gcm_name == "ensemble")

  # Min/max bounds across GCMs (excluding ensemble)
  df_temp_bounds <- calendar_sub %>%
    dplyr::filter(gcm_name != "ensemble") %>%
    dplyr::group_by(period, day_of_year) %>%
    dplyr::summarise(
      min_temp = min(migrTproj, na.rm = TRUE),
      max_temp = max(migrTproj, na.rm = TRUE),
      .groups = "drop"
    )

  # Shaded ribbon for mid-century GCM bounds ("2041-2060")
  df_bounds_mid <- df_temp_bounds %>%
    dplyr::filter(period == "2041-2060")

  p_upper <- ggplot(df_timing) +
    # Dotted connector from rt_end to sp_start (transition)
    geom_segment(aes(x = rt_end, xend = sp_start, y = CU_label, yend = CU_label, color = SPECIES_NAME), linetype = "dotted", linewidth = 0.6) +
    # Run timing bar (rt_start to rt_end) - narrower segment
    geom_segment(aes(x = rt_start, xend = rt_end, y = CU_label, yend = CU_label, color = SPECIES_NAME), linewidth = 1.5) +
    # Spawning timing bar (sp_start to sp_peak) - narrower segment
    geom_segment(aes(x = sp_start, xend = sp_peak, y = CU_label, yend = CU_label, color = SPECIES_NAME), linewidth = 0.8, alpha = 0.7) +
    scale_color_manual(values = spp_colors, guide = "none") + # Hide species legend
    labs(x = NULL, y = "CU", color = "Species") +
    scale_x_continuous(limits = c(1, 365),
                       breaks = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335),
                       labels = NULL) +
    theme_cvis() +
    theme(
      axis.text.y = ggtext::element_markdown(size = 5),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.x = element_blank(),
      panel.grid.major.y = element_blank(),
      legend.position = "right",
      plot.margin = margin(t = 2, r = 0, b = -2, l = 2, unit = "pt") # remove right margin to close gap
    ) +
    annotation_custom(grid::textGrob("a", x = unit(0.96, "npc"), y = unit(0.92, "npc"), gp = grid::gpar(fontface = "bold", fontsize = 12)))

  # Split df_temp_ensemble into historical and mid-century
  df_ens_hist <- df_temp_ensemble %>% dplyr::filter(period == "1981-2010")
  df_ens_mid <- df_temp_ensemble %>% dplyr::filter(period == "2041-2060")

  p_lower <- ggplot() +
    # Red ribbon for GCM bounds (daily min and max) for mid-century (no legend mapping)
    geom_ribbon(data = df_bounds_mid, aes(x = day_of_year, ymin = min_temp, ymax = max_temp), fill = "#e74c3c", alpha = 0.15) +
    # Historical Ensemble
    geom_line(data = df_ens_hist, aes(x = day_of_year, y = migrTproj, color = "1981-2010"), linewidth = 1) +
    # Mid-century Ensemble
    geom_line(data = df_ens_mid, aes(x = day_of_year, y = migrTproj, color = "2041-2060"), linewidth = 1) +
    labs(x = "Month", y = "Mainstem temperature (°C)") +
    scale_color_manual(
      name = "Period",
      values = c(
        "1981-2010" = "#2c3e50",
        "2041-2060" = "#e74c3c"
      )
    ) +
    scale_x_continuous(limits = c(1, 365),
                       breaks = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335),
                       labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
    theme_cvis() +
    theme(
      legend.position = "right",
      plot.margin = margin(t = -2, r = 0, b = 2, l = 2, unit = "pt") # remove right margin to align
    ) +
    annotation_custom(grid::textGrob("b", x = unit(0.96, "npc"), y = unit(0.85, "npc"), gp = grid::gpar(fontface = "bold", fontsize = 12)))

  # Panel C: Tile plot of baseline migration indicators
  if (exists("cvis_risk_palette", envir = .GlobalEnv)) {
    cvis_pal <- get("cvis_risk_palette", envir = .GlobalEnv)
  } else {
    cvis_pal <- "Zissou1"
  }

  p_tile <- ggplot(migr_ind_data, aes(x = indicator_label, y = CU_label)) +
    geom_tile(aes(fill = std_value), color = NA) + # fill based on std_value, remove inner borders
    geom_text(aes(label = label_text), size = 1.7, fontface = "bold", color = "black") + # black text
    scale_fill_cvis(
      palette = cvis_pal,
      direction = -1,
      limits = c(0, 1),
      na.value = "grey95",
      guide = "none" # Remove tile plot color legend
    ) +
    scale_x_discrete(position = "top") +
    labs(x = NULL, y = NULL) +
    theme_cvis() +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks.length.y = unit(0, "pt"),
      axis.text.x = element_text(size = 6.5, angle = 45, hjust = 0, face = "bold", color = "black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_blank(),
      panel.border = element_blank(),
      plot.margin = margin(t = 2, r = 2, b = -2, l = 0, unit = "pt") # remove left margin to close gap
    ) +
    annotation_custom(grid::textGrob("c", x = unit(0.92, "npc"), y = unit(0.92, "npc"), gp = grid::gpar(fontface = "bold", fontsize = 12)))

  # Assemble the plots using patchwork area design
  # A (p_upper) and B (p_lower) occupy columns 1 to 6. 
  # C (p_tile) occupies columns 7 to 9 (sitting beside A only).
  # The bottom-right column space (rows 6 to 7, columns 7 to 9) is occupied by the collected period legend.
  design <- c(
    patchwork::area(t = 1, l = 1, b = 5, r = 6), # A (p_upper)
    patchwork::area(t = 6, l = 1, b = 7, r = 6), # B (p_lower)
    patchwork::area(t = 1, l = 7, b = 5, r = 9), # C (p_tile)
    patchwork::area(t = 6, l = 7, b = 7, r = 9)  # Collected guide area (bottom right)
  )

  p <- p_upper + p_lower + p_tile + patchwork::guide_area() +
    patchwork::plot_layout(design = design, guides = "collect") &
    theme(
      panel.spacing.x = unit(0, "pt"), # remove horizontal spacing between columns
      legend.box.spacing = unit(4, "pt"),
      legend.margin = margin(0, 0, 0, 0, "pt")
    )

  return(p)
}


# ==================== 10. Baseline Raw Indicator Value Distributions (plot_raw_baseline_violins) ====================

plot_raw_baseline_violins <- function(all_std_long_baseline, tbl_indicators, cu_code = NULL) {
  # Filter to baseline scenario and 'mean' stat
  plot_data <- all_std_long_baseline %>%
    dplyr::filter(stat == "mean")

  # Retrieve colors from global environment
  ind_colors <- get("indicator_palette", envir = .GlobalEnv)
  spp_colors <- get("species_palette", envir = .GlobalEnv)

  # Retrieve short units from tbl_indicators
  short_units_map <- tbl_indicators %>%
    dplyr::select(abbrev, unit_short) %>%
    tibble::deframe()
  short_units_map["migrdist"] <- "km"  # Override as migrdist is scaled to km in this plot

  # Set category labels matching indicator_palette keys
  category_names <- c(
    "fwrs" = "Spawning & Rearing",
    "migr" = "Upstream Migration",
    "mar" = "Nearshore Marine",
    "dem" = "Demographics",
    "gen" = "Genetics"
  )

  # Map raw migrdist values from meters to kilometers if they are large
  if (any(plot_data$indicator == "migrdist" & plot_data$value > 1000, na.rm = TRUE)) {
    plot_data <- plot_data %>%
      dplyr::mutate(value = ifelse(indicator == "migrdist", value / 1000, value))
  }

  # Transform CUnmat values to log10 space
  plot_data <- plot_data %>%
    dplyr::mutate(value = ifelse(indicator == "CUnmat", log10(value), value))

  # Custom formatter for y-axis values to improve clarity (avoid scientific notation, round small decimals)
  cvis_y_formatter <- function(x) {
    ifelse(is.na(x), "", 
      ifelse(abs(x) >= 1000000, paste0(round(x / 1000000, 1), "M"),
        ifelse(abs(x) >= 1000, paste0(round(x / 1000, 1), "K"),
          format(round(x, 2), scientific = FALSE, drop0trailing = TRUE)
        )
      )
    )
  }

  plot_data <- plot_data %>%
    dplyr::mutate(
      unit_short = ifelse(indicator %in% names(short_units_map), short_units_map[indicator], ""),
      facet_label = paste0(indicator, " (", unit_short, ")"),
      category_label = factor(category_names[category], levels = category_names)
    )

  # Group panes by category first, then order alphabetically within category
  facet_order <- plot_data %>%
    dplyr::select(category_label, indicator, facet_label) %>%
    dplyr::distinct() %>%
    dplyr::arrange(category_label, indicator) %>%
    dplyr::pull(facet_label)

  # Ensure SPECIES_NAME is a factor with consistent levels matching spp_colors
  plot_data <- plot_data %>%
    dplyr::mutate(
      SPECIES_NAME = factor(SPECIES_NAME, levels = names(spp_colors)),
      facet_label = factor(facet_label, levels = facet_order)
    )

  # Extract highlighted CU data if cu_code is provided
  cu_point_data <- NULL
  if (!is.null(cu_code)) {
    target_cu_info <- plot_data %>%
      dplyr::filter(FULL_CU_IN == cu_code)
    
    if (nrow(target_cu_info) > 0) {
      all_species <- names(spp_colors)
      # Create dummy grid for all categories and species so position_dodge aligns correctly
      cu_point_data <- expand.grid(
        facet_label = unique(as.character(plot_data$facet_label)),
        SPECIES_NAME = all_species,
        stringsAsFactors = FALSE
      ) %>%
        dplyr::left_join(
          target_cu_info %>% 
            dplyr::mutate(facet_label = as.character(facet_label),
                          SPECIES_NAME = as.character(SPECIES_NAME)) %>%
            dplyr::select(facet_label, SPECIES_NAME, value),
          by = c("facet_label", "SPECIES_NAME")
        )
      
      # Fill category and category_label from plot_data mapping (guaranteed complete)
      facet_cat_map <- plot_data %>% 
        dplyr::select(facet_label, category, category_label) %>% 
        dplyr::distinct() %>%
        dplyr::mutate(
          facet_label = as.character(facet_label),
          category_label = as.character(category_label)
        )
      
      cu_point_data <- cu_point_data %>%
        dplyr::left_join(facet_cat_map, by = "facet_label") %>%
        dplyr::mutate(
          SPECIES_NAME = factor(SPECIES_NAME, levels = all_species),
          category_label = factor(category_label, levels = levels(plot_data$category_label)),
          facet_label = factor(facet_label, levels = levels(plot_data$facet_label))
        )
    }
  }

  # Subtitle depends on whether cu_code is provided
  sub_text <- if (!is.null(cu_code)) {
    paste0("Showing raw values (stat = 'mean') for all CUs under the baseline scenario. The orange diamond highlights ", cu_code, ".")
  } else {
    "Showing raw values (stat = 'mean') for all CUs under the baseline scenario. Violins show category-level distributions; points show CUs colored by species (dodged side-by-side inside violins)."
  }

  # Helper to make a subplot for a subset of categories/indicators
  make_sub_plot <- function(sub_data, sub_cu_point_data, ncol = 4) {
    p_sub <- ggplot(sub_data, aes(x = category_label, y = value, fill = category_label)) +
      geom_violin(alpha = 0.3, color = "grey50", scale = "width", linewidth = 0.5)

    # Beeswarm-like symmetric dots inside the violin (grouped and dodged by species)
    p_sub <- p_sub + ggdist::stat_dots(
      aes(color = SPECIES_NAME, group = SPECIES_NAME),
      side = "both",
      justification = 0.5,
      position = position_dodge(width = 0.6),
      binwidth = unit(0.06, "npc"),
      dotsize = 1.0,
      alpha = if (!is.null(cu_code)) 0.35 else 0.65,
      shape = 19,
      inherit.aes = TRUE
    )

    # Highlighted CU point aligned with the dodged species stack
    if (!is.null(sub_cu_point_data) && nrow(sub_cu_point_data) > 0) {
      p_sub <- p_sub + geom_point(
        data = sub_cu_point_data,
        aes(x = category_label, y = value, group = SPECIES_NAME),
        shape = 23,
        size = 3.5,
        fill = "#ED8936",
        color = "black",
        stroke = 1.2,
        position = position_dodge(width = 0.6),
        inherit.aes = FALSE
      )
    }

    p_sub <- p_sub +
      facet_wrap(~facet_label, scales = "free", ncol = ncol) +
      scale_y_continuous(labels = cvis_y_formatter) +
      scale_fill_manual(values = ind_colors, name = "Indicator Category") +
      scale_color_manual(values = spp_colors, name = "Species") +
      guides(
        fill = "none",
        color = guide_legend(title.position = "top", nrow = 1, order = 2)
      ) +
      labs(x = NULL, y = NULL) +
      theme_cvis(base_size = 11) +
      theme(
        strip.text = element_text(face = "bold", size = 8, color = "black"),
        strip.background = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 8, color = "#2D3748"),
        axis.line.y = element_line(color = "#CBD5E0", linewidth = 0.5),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.border = element_blank()
      )
    
    return(p_sub)
  }

  # Split data into category subsets
  data_fwrs <- plot_data %>% dplyr::filter(category == "fwrs")
  data_migr <- plot_data %>% dplyr::filter(category == "migr")
  data_mar  <- plot_data %>% dplyr::filter(category == "mar")
  data_dem_gen <- plot_data %>% dplyr::filter(category %in% c("dem", "gen"))

  # Split highlighted CU point data into category subsets
  cu_fwrs <- if (!is.null(cu_point_data)) cu_point_data %>% dplyr::filter(category == "fwrs") else NULL
  cu_migr <- if (!is.null(cu_point_data)) cu_point_data %>% dplyr::filter(category == "migr") else NULL
  cu_mar  <- if (!is.null(cu_point_data)) cu_point_data %>% dplyr::filter(category == "mar") else NULL
  cu_dem_gen <- if (!is.null(cu_point_data)) cu_point_data %>% dplyr::filter(category %in% c("dem", "gen")) else NULL

  # Create individual subplots (each starting on a new row)
  # Spawning and rearing occupies first 2 rows of panes (7 indicators, wrapped in 4 columns)
  p_fwrs <- make_sub_plot(data_fwrs, cu_fwrs, ncol = 4)
  # Migration occupies the 3rd row of panes (3 indicators, wrapped in 4 columns)
  p_migr <- make_sub_plot(data_migr, cu_migr, ncol = 4)
  # Marine occupies the 4th row of panes (3 indicators, wrapped in 4 columns)
  p_mar  <- make_sub_plot(data_mar, cu_mar, ncol = 4)
  # Demographics & Genetics occupy the last row together (4 indicators, wrapped in 4 columns)
  p_dem_gen <- make_sub_plot(data_dem_gen, cu_dem_gen, ncol = 4)

  # Combine using patchwork to align columns and collect legends
  p_combined <- (p_fwrs / p_migr / p_mar / p_dem_gen) +
    plot_layout(guides = "collect", heights = c(2, 1, 1, 1)) &
    theme(
      legend.position = "bottom",
      legend.box = "vertical"
    )

  return(p_combined)
}


# ==================== 11. Vulnerability Score Distributions (plot_cvis_vulnerability_violins) ====================

plot_cvis_vulnerability_violins <- function(scores_tidy_baseline, cu_code = NULL) {
  # 1. Category scores
  cat_scores <- scores_tidy_baseline %>%
    dplyr::filter(method == "avg", category %in% c("dem", "fwrs", "gen", "mar", "migr")) %>%
    dplyr::select(FULL_CU_IN, SPECIES_NAME, SMU_SIMPLE, category, score = score100_all)

  # 2. Overall scores
  overall_scores <- scores_tidy_baseline %>%
    dplyr::filter(method == "catavg", category == "all") %>%
    dplyr::select(FULL_CU_IN, SPECIES_NAME, SMU_SIMPLE, category, score = score100_all)

  # Combine
  plot_data <- dplyr::bind_rows(cat_scores, overall_scores)

  # Short names/pretty labels - Overall Vulnerability first
  category_labels <- c(
    "all"  = "Overall Vulnerability",
    "dem"  = "Demographics",
    "fwrs" = "Spawning & Rearing",
    "gen"  = "Genetics",
    "mar"  = "Nearshore Marine",
    "migr" = "Upstream Migration"
  )

  plot_data <- plot_data %>%
    dplyr::mutate(
      category_label = factor(category_labels[category], levels = category_labels)
    )

  # Retrieve colors from global environment
  ind_colors <- get("indicator_palette", envir = .GlobalEnv)
  vuln_colors <- c(ind_colors, "Overall Vulnerability" = "#1A365D")
  spp_colors <- get("species_palette", envir = .GlobalEnv)

  # Ensure SPECIES_NAME is a factor with consistent levels matching spp_colors
  plot_data <- plot_data %>%
    dplyr::mutate(
      SPECIES_NAME = factor(SPECIES_NAME, levels = names(spp_colors))
    )

  # Highlighted CU point
  cu_point_data <- NULL
  if (!is.null(cu_code)) {
    target_cu_info <- plot_data %>%
      dplyr::filter(FULL_CU_IN == cu_code)
    
    if (nrow(target_cu_info) > 0) {
      all_species <- names(spp_colors)
      # Create dummy grid for all categories and species so position_dodge aligns correctly
      cu_point_data <- expand.grid(
        category_label = as.character(levels(plot_data$category_label)),
        SPECIES_NAME = all_species,
        stringsAsFactors = FALSE
      ) %>%
        dplyr::left_join(
          target_cu_info %>% 
            dplyr::mutate(category_label = as.character(category_label),
                          SPECIES_NAME = as.character(SPECIES_NAME)) %>%
            dplyr::select(category_label, SPECIES_NAME, score),
          by = c("category_label", "SPECIES_NAME")
        ) %>%
        dplyr::mutate(
          SPECIES_NAME = factor(SPECIES_NAME, levels = all_species),
          category_label = factor(category_label, levels = levels(plot_data$category_label))
        )
    }
  }

  # Subtitle
  sub_text <- if (!is.null(cu_code)) {
    paste0("Violins show distributions across all CUs. The orange diamond highlights ", cu_code, ".")
  } else {
    "Violins show distributions across all CUs; points show CUs colored by species."
  }

  # Ensure ggdist is loaded if available, otherwise fallback to standard violin
  if (!requireNamespace("ggdist", quietly = TRUE)) {
    p <- ggplot(plot_data, aes(x = category_label, y = score, fill = category_label)) +
      geom_vline(xintercept = 1.5, linetype = "dashed", color = "grey60", linewidth = 0.6) +
      geom_violin(alpha = 0.3, color = "grey50", scale = "width", linewidth = 0.5)

    if (!is.null(cu_code)) {
      p <- p + geom_jitter(width = 0.12, height = 0, alpha = 0.35, size = 2.0, aes(color = SPECIES_NAME), shape = 16)
    } else {
      p <- p + geom_jitter(width = 0.12, height = 0, alpha = 0.65, size = 2.2, aes(color = SPECIES_NAME), shape = 16)
    }

    if (!is.null(cu_point_data) && nrow(cu_point_data) > 0) {
      p <- p + geom_point(data = cu_point_data, aes(x = category_label, y = score),
                          shape = 23, size = 4.0, fill = "#ED8936", color = "black", stroke = 1.2, inherit.aes = FALSE)
    }
  } else {
    p <- ggplot(plot_data, aes(x = category_label, y = score, fill = category_label)) +
      # Line separator between Overall Vulnerability and individual categories
      geom_vline(xintercept = 1.5, linetype = "dashed", color = "grey60", linewidth = 0.6) +
      
      # Full violin centered
      geom_violin(alpha = 0.3, color = "grey50", scale = "width", linewidth = 0.5) +

      # Beeswarm-like symmetric dots inside the violin (grouped and dodged by species)
      ggdist::stat_dots(
        aes(color = SPECIES_NAME, group = SPECIES_NAME),
        side = "both",
        justification = 0.5,
        position = position_dodge(width = 0.6),
        binwidth = 1.2,
        dotsize = 1.8,
        alpha = if (!is.null(cu_code)) 0.35 else 0.65,
        shape = 19,
        inherit.aes = TRUE
      )

      # Highlighted CU point aligned with the dodged species stack
      if (!is.null(cu_point_data) && nrow(cu_point_data) > 0) {
        p <- p + geom_point(
          data = cu_point_data,
          aes(x = category_label, y = score, group = SPECIES_NAME),
          shape = 23,
          size = 4.0,
          fill = "#ED8936",
          color = "black",
          stroke = 1.2,
          position = position_dodge(width = 0.6),
          inherit.aes = FALSE
        )
      }
  }

  p <- p +
    scale_fill_manual(values = vuln_colors, name = "Vulnerability Category") +
    scale_color_manual(values = spp_colors, name = "Species") +
    guides(
      fill = "none",
      color = guide_legend(title.position = "top", nrow = 1)
    ) +
    labs(
      title = if (!is.null(cu_code)) paste("Vulnerability score distribution and highlights for", cu_code) else "Vulnerability Score Distributions across Fraser CUs",
      subtitle = sub_text,
      x = NULL,
      y = "Vulnerability Score (0 - 100)"
    ) +
    theme_cvis(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", size = 12, color = "black"),
      plot.subtitle = element_text(size = 9, color = "grey40"),
      axis.text.x = element_text(face = "bold", size = 9, color = "black"),
      axis.line.y = element_line(color = "#CBD5E0", linewidth = 0.5),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      legend.position = "bottom"
    )

  return(p)
}






# 
# # combined_maz_marine_plot(maz_all, MAZ)
# 







