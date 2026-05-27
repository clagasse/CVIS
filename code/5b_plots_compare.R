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
#      - indicator_tile_plot()        - Multi-panel tile plot of indicators by species.
#   3. Correlation and Comparison Plots:
#      - get_correlation_matrix()     - Correlation analysis matrix of indicators.
#      - xy_indicator_plot()          - Scatter plot comparing two indicators.
#   4. WSP Abundance and Status Table (cu_status_table)
#   5. Life History Timing Comparison (plot_timing_comparison)
#   6. Combined Tile Plot for All CUs & Indicators (indicator_cu_tile_plot)
#   7. Marine Adaptive Zone Lollipop Chart (plot_maz_lollipop)
#
# Dependencies:
#   - Requires ggplot2, sf, dplyr, stringr, patchwork, ggtext, and standard CVIS data inputs.
# ==============================================================================

# Long format plots -------------------------------------------------------

#' Lollipop plot (long-format) for a single RCP/period with GCM variation
#' Always shows all CUs; missing data show as blank (no segment/point).
#'
#' @param data           Long-format table with (at least):
#'   rcp, period_code, indicator, gcm, CVIS_NAME, SPECIES_NAME, FULL_CU_IN,
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
  require(ggtext)
  
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
    distinct(FULL_CU_IN, SPECIES_NAME, CVIS_NAME, SMU_SIMPLE)

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
    select(FULL_CU_IN, SPECIES_NAME, CVIS_NAME, SMU_SIMPLE, gcm, rcp, dsmodel, stat,
      val = dplyr::all_of(val_col),
      std_val = dplyr::all_of(if ("std_value" %in% names(dat_full)) "std_value" else val_col)
    )

  # ---- Summaries across GCM per CU/Scenario/Model ----
  cu_summary <- dat %>%
    filter(gcm %in% c("9", "0")) %>%
    group_by(FULL_CU_IN, SPECIES_NAME, CVIS_NAME, SMU_SIMPLE, rcp, dsmodel) %>%
    summarise(
      min_gcm    = val[stat %in% c("qlowgcm", "qlowsp", "qlow")][1],
      max_gcm    = val[stat %in% c("qhighgcm", "qhighsp", "qhigh")][1],
      center_y   = val[stat == "mean"][1],
      center_std = std_val[stat == "mean"][1],
      .groups    = "drop"
    )

  # ---- Right-join summaries to the full CU roster to ensure all CUs are present ----
  cu_plot <- cu_roster %>%
    left_join(cu_summary, by = c("FULL_CU_IN", "SPECIES_NAME", "CVIS_NAME", "SMU_SIMPLE"))

  # ---- HTML labels coloured by SMU (matching indicator_cu_tile_plot) ----
  # Pre-calculate colors to avoid length mismatch errors in case_when
  smu_col_vec <- if (exists("smu_palette")) {
    smu_palette[as.character(cu_plot$SMU_SIMPLE)]
  } else {
    rep(NA_character_, nrow(cu_plot))
  }
  
  sp_col_vec <- if (exists("species_palette")) {
    species_palette[as.character(cu_plot$SPECIES_NAME)]
  } else {
    rep(NA_character_, nrow(cu_plot))
  }
  
  cu_plot <- cu_plot %>%
    mutate(
      label_color = coalesce(as.character(smu_col_vec), as.character(sp_col_vec), "#666666"),
      id_label_html = paste0("<span style='color:", label_color, "'>", CVIS_NAME, "</span>")
    ) %>%
    arrange(SPECIES_NAME, CVIS_NAME) %>%
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
            cu_plot %>% select(FULL_CU_IN, SPECIES_NAME, CVIS_NAME, id_label_html, rcp, dsmodel),
            by = c("FULL_CU_IN", "SPECIES_NAME", "CVIS_NAME", "rcp", "dsmodel")
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
    scale_fill_distiller(
      name = "Standardized Score",
      palette = cvis_risk_palette, direction = -1,
      limits = c(0, 1), na.value = "transparent"
    )

  # # Saturated Scenario Colors - supporting both "45"/"85" and "4.5"/"8.5" formats
  p <- p +
    scale_color_manual(
      name = "Scenario (RCP)",
      values = c(
        "45" = "darkblue", "4.5" = "darkblue",
        "85" = "#B22222", "8.5" = "#B22222"
      ),
      na.translate = FALSE
    ) +
    scale_shape_manual(
      name = "Downscaling method",
      values = c(21, 24, 22, 23, 25),
      na.translate = FALSE
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
      subtitle = subtitle_lab,
      y = y_lab,
      x = NULL
    ) +
    coord_flip() +
    theme_minimal(base_size = 11) +
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
  
  x_label <- paste0("Raw (", if (!is.null(indicator_unit) && !is.na(indicator_unit) && indicator_unit != "") indicator_unit else "value", ")")
  
  p <- ggplot(scatter_dat, aes(x = value, y = std_value, color = SPECIES_NAME)) +
    geom_point(size = 2.5) +
    scale_color_manual(values = species_palette) +
    labs(
      color  = "Species",
      x      = x_label,
      y      = "Standardized Score"
      #title  = paste("Standardization function -", indicator_name)
    ) +
    theme_minimal(base_size = 10) +
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
                                   brewer_palette = cvis_risk_palette,
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
  fill_scale <- scale_fill_distiller(
    palette = brewer_palette, 
    direction = palette_direction, 
    limits = val_range,
    name = indicator_pick
  )

  p_big <- NULL
  p_small <- NULL

  # Adjust plot margins negatively to reduce the left/right padding of panel layouts
  if (length(big_spp_present) > 0) {
    p_big <- ggplot() +
      geom_sf(data = filter(cu_boundary_plot, sp_col %in% big_spp_present), aes(fill = plot_value), alpha = 0.7) +
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
      geom_sf(data = filter(cu_boundary_plot, sp_col %in% small_spp_present), aes(fill = plot_value), alpha = 0.7) +
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
#' @param brewer_palette     RColorBrewer palette (default: "RdYlGn").
#' @param palette_direction  Direction for brewer palette (default: -1).
#' @param ncol               Number of columns in the facet layout (default: 3).
#' @return A ggplot object.
spatial_fw_rearing_indicators_plot <- function(data,
                                               cu_boundary,
                                               outline = Fr_basin,
                                               species_pick = "Chinook",
                                               sp_col_name = "SPECIES_NAME",
                                               use_standardized = TRUE,
                                               id_col = "FULL_CU_IN",
                                               brewer_palette = cvis_risk_palette,
                                               palette_direction = -1,
                                               ncol = 3) {
  
  # The 6 freshwater rearing indicators (excluding fwres)
  fw_indicators <- c("favchange", "cthr", "tw8rate", "tw8proj", "flow8pdelta", "flow18pdelta")
  
  # Filter data to the 6 FW rearing indicators and chosen species
  plot_data <- data %>% 
    filter(
      .data$indicator %in% fw_indicators,
      .data[[sp_col_name]] == species_pick
    )
  
  if (nrow(plot_data) == 0) {
    stop(paste("No data found for species:", species_pick, "and the 6 FW rearing indicators."))
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
      mutate(abbrev = factor(abbrev, levels = fw_indicators)) %>%
      arrange(abbrev)
    
    cu_boundary_plot <- cu_boundary_plot %>%
      left_join(indicator_names, by = c("indicator" = "abbrev")) %>%
      mutate(
        indicator_lbl = coalesce(name, indicator),
        indicator_lbl = factor(indicator_lbl, levels = indicator_names$name)
      )
    
    facet_var <- "indicator_lbl"
  } else {
    cu_boundary_plot <- cu_boundary_plot %>%
      mutate(indicator = factor(indicator, levels = fw_indicators))
    facet_var <- "indicator"
  }
  
  p <- ggplot() +
    geom_sf(data = cu_boundary_plot, aes(fill = plot_value), alpha = 0.3) +
    scale_fill_distiller(
      palette = brewer_palette, 
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


# ==================== 2. Multi-panel Indicator Tile Plot by Species ====================

#' Tile plot of standardized indicator values (long-format, by species)
#' Selects ensemble or GCM 0 (no averaging across GCMs).
#'
#' @param all_std_long       Long-format table with at least:
#'   rcp, period_code, indicator, CVIS_NAME, SPECIES_NAME, FULL_CU_IN,
#'   std_value, (optional) gcm, dsmodel, (optional) ensemble (logical)
#' @param indicators_choose  Character vector of indicator codes to include.
#' @param indicators_metadata Data frame with columns: abbrev (code), name (description).
#' @param brewer_palette     RColorBrewer palette name.
#' @param palette_direction  1 or -1 for palette direction.
#' @param plot_colours       Named or unnamed vector of species colors; if named, names match SPECIES_NAME.
#' @param dsmodel_pick       Optional character scalar/vector; filter to these dsmodel(s) if supplied.
#' @param gcm_pick           Optional scalar/vector; if supplied, filter to these GCM(s) after ensemble/0 fallback.
#' @param ncol               Columns in patchwork layout (auto-detected if NULL).
#' @param uniform_height     TRUE forces equal panel heights; FALSE scales by n CUs per species.
#' @param overall_score_cols Character vector of indicator codes to move to the end (if present).
#' @param category_name      Optional overall title for the assembled plot.
#' @param ensemble_regex     Regex pattern for detecting ensemble codes in character `gcm`.
#'
#' @return A patchwork ggplot object with one tile panel per species + indicator key.
indicator_tile_plot <- function(all_std_long,
                                indicators_choose = c("migrT", "migrQ", "migrA21", "migrdist"),
                                indicators_metadata = tbl_indicators,
                                brewer_palette = cvis_risk_palette,
                                palette_direction = -1,
                                plot_colours = species_palette,
                                dsmodel_pick = NULL,
                                gcm_pick = NULL,
                                ncol = NULL,
                                uniform_height = FALSE,
                                overall_score_cols = NULL,
                                category_name = NULL,
                                ensemble_regex = "(?i)(ens|ensemble|mmem|multi|mean|avg)") {
  # ---- Basic checks 
  req_cols <- c("indicator", "CVIS_NAME", "SPECIES_NAME", "FULL_CU_IN", "std_value")
  if (!all(req_cols %in% names(all_std_long))) {
    stop("Missing required columns in 'all_std_long': ", paste(setdiff(req_cols, names(all_std_long)), collapse = ", "))
  }

  # ---- Filter scenario/model/indicator 
  data_f <- all_std_long

  # Keep only requested indicators (plus any overall codes)
  keep_codes <- unique(c(indicators_choose, overall_score_cols))
  if (!is.null(keep_codes)) {
    data_f <- data_f %>% filter(.data$indicator %in% keep_codes)
  }

  # Split by indicator to apply dynamic filtering
  data_list <- split(data_f, data_f$indicator)
  for (ind in names(data_list)) {
    df_ind <- data_list[[ind]]

    # Filter dsmodel
    if (!is.null(dsmodel_pick) && "dsmodel" %in% names(df_ind) && length(dsmodel_pick) > 0) {
      if (any(df_ind$dsmodel %in% dsmodel_pick, na.rm = TRUE)) {
        df_ind <- df_ind %>% filter(dsmodel %in% dsmodel_pick)
      }
    }

    data_list[[ind]] <- df_ind
  }

  data_f <- bind_rows(data_list)

  if (nrow(data_f) == 0L) {
    stop("No rows after filtering by rcp/period/dsmodel/indicators (even with fallback to 0).")
  }

  # ---- Select ensemble or GCM 0 (no averaging) 
  # Start with all rows; then try to keep ensemble / gcm==0 / gcm_pick.
  data_s <- data_f

  has_ensemble_col <- "ensemble" %in% names(data_s)
  has_gcm <- "gcm" %in% names(data_s)

  if (has_ensemble_col && any(data_s$ensemble %in% TRUE, na.rm = TRUE)) {
    data_s <- data_s %>% filter(.data$ensemble %in% TRUE)
  } else if (has_gcm) {
    if (is.numeric(data_s$gcm)) {
      # Prefer numeric GCM code == 0
      if (any(data_s$gcm == 0, na.rm = TRUE)) {
        data_s <- data_s %>% filter(.data$gcm == 0)
      }
    } else if (is.character(data_s$gcm)) {
      # Prefer ensemble-like names by regex
      if (any(str_detect(data_s$gcm, ensemble_regex), na.rm = TRUE)) {
        data_s <- data_s %>% filter(str_detect(.data$gcm, ensemble_regex))
      }
    }
  }

  # If user supplied gcm_pick, filter to that (after ensemble/0)
  if (!is.null(gcm_pick) && has_gcm) {
    data_s <- data_s %>% filter(.data$gcm %in% gcm_pick)
  }

  # If multiple GCM rows still exist per CU×indicator, deterministically select the first and warn
  if (has_gcm) {
    multi_counts <- data_s %>%
      group_by(FULL_CU_IN, SPECIES_NAME, CVIS_NAME, indicator) %>%
      summarise(n_gcm = n_distinct(gcm), .groups = "drop") %>%
      filter(n_gcm > 1)
    if (nrow(multi_counts) > 0) {
      warning(glue::glue("{nrow(multi_counts)} CU×indicator groups had multiple GCM rows after selection; keeping the first deterministically. Consider setting gcm_pick=."))
      data_s <- data_s %>%
        arrange(SPECIES_NAME, FULL_CU_IN, CVIS_NAME, indicator, gcm) %>%
        group_by(FULL_CU_IN, SPECIES_NAME, CVIS_NAME, indicator) %>%
        slice_head(n = 1) %>%
        ungroup()
    }
  }

  # ---- Prepare plot data (no aggregation) ----
  plot_data <- data_s %>%
    select(FULL_CU_IN, SPECIES_NAME, CVIS_NAME, indicator, value = std_value)

  if (nrow(plot_data) == 0L) {
    stop("No rows remain after ensemble/GCM selection.")
  }

  # ---- Indicator order: regular first, overall at end ----
  indicator_order <- unique(plot_data$indicator)
  if (!is.null(overall_score_cols) && length(overall_score_cols) > 0) {
    regular_inds <- setdiff(indicator_order, overall_score_cols)
    overall_present <- overall_score_cols[overall_score_cols %in% indicator_order]
    indicator_order <- c(regular_inds, overall_present)
  }
  plot_data <- plot_data %>% mutate(indicator = factor(indicator, levels = indicator_order))

  # ---- Normalize color by indicator (0..1 scale) ----
  indicator_limits <- plot_data %>%
    group_by(indicator) %>%
    summarise(
      min_val = suppressWarnings(min(value, na.rm = TRUE)),
      max_val = suppressWarnings(max(value, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    mutate(
      min_val = ifelse(is.infinite(min_val), NA_real_, min_val),
      max_val = ifelse(is.infinite(max_val), NA_real_, max_val)
    )

  plot_data <- plot_data %>%
    left_join(indicator_limits, by = "indicator") %>%
    mutate(
      range_ok = !is.na(min_val) & !is.na(max_val) & (max_val > min_val),
      value_normalized = ifelse(range_ok, (value - min_val) / (max_val - min_val), 0.5)
    )

  # ---- Species color mapping for CU labels ----
  species_levels <- unique(plot_data$SPECIES_NAME)
  if (!is.null(names(plot_colours)) && all(species_levels %in% names(plot_colours))) {
    col_map <- plot_colours[names(plot_colours) %in% species_levels]
    col_map <- col_map[species_levels]
  } else {
    col_map <- setNames(rep(plot_colours, length.out = length(species_levels)), species_levels)
  }

  plot_data <- plot_data %>%
    mutate(
      sp = SPECIES_NAME,
      id = CVIS_NAME,
      id_label = paste0("<span style='color:", col_map[sp], "'>", id, "</span>")
    )

  # ---- Species list and CU counts ----
  species_list <- unique(plot_data$sp)
  cu_counts <- plot_data %>%
    group_by(sp) %>%
    summarise(
      n_cus = n_distinct(id_label),
      cu_name = if (n_distinct(id_label) == 1) first(id) else NA_character_,
      .groups = "drop"
    ) %>%
    arrange(match(sp, species_list))

  # ---- Auto-detect ncol ----
  if (is.null(ncol)) {
    ncol <- ifelse(length(species_list) == 5, 2, ceiling(sqrt(length(species_list))))
  }

  # ---- Panel heights ----
  if (!uniform_height) {
    heights <- cu_counts$n_cus
    heights <- heights / max(1, min(heights, na.rm = TRUE))
    heights <- pmin(heights, 3)
  } else {
    heights <- rep(1, length(species_list))
  }

  # ---- Bottom row detection ----
  n_plots <- length(species_list)
  n_rows <- ceiling(n_plots / ncol)
  bottom_row_start <- (n_rows - 1) * ncol + 1
  bottom_row_indices <- bottom_row_start:n_plots

  # ---- Build per-species subplots ----
  plot_list <- lapply(seq_along(species_list), function(idx) {
    species <- species_list[idx]
    data_sp <- plot_data %>% filter(sp == species)

    n_cus <- cu_counts$n_cus[idx]
    cu_name <- cu_counts$cu_name[idx]
    is_bottom_row <- idx %in% bottom_row_indices
    is_single_cu <- n_cus == 1

    y_text_size <- if (n_cus > 15) 7 else if (n_cus > 10) 8 else 9
    tile_text_size <- if (n_cus > 15) 1.8 else if (n_cus > 10) 2 else 2.5

    plot_title <- paste0(species, " (n=", n_cus, ")")
    plot_subtitle <- if (is_single_cu) cu_name else NULL

    p <- ggplot(data_sp, aes(x = indicator, y = id_label)) +
      geom_tile(aes(fill = value_normalized), color = "white", linewidth = 0.3, na.rm = FALSE) +
      geom_text(aes(label = ifelse(is.na(value), "", sprintf("%.1f", value))),
        size = tile_text_size, color = "black", na.rm = TRUE
      ) +
      scale_fill_distiller(
        palette = brewer_palette,
        direction = palette_direction,
        na.value = "grey95",
        limits = c(0, 1)
      ) +
      theme_minimal(base_size = 9) +
      theme(
        axis.title = element_blank(),
        legend.position = "none",
        panel.grid = element_blank(),
        panel.border = element_rect(color = "grey70", fill = NA, linewidth = 0.3),
        plot.title = element_text(face = "bold", size = 10, hjust = 0, margin = margin(b = 1)),
        plot.margin = margin(2, 2, 2, 2)
      ) +
      labs(y = NULL, x = NULL, title = plot_title)

    if (!is.null(plot_subtitle)) {
      p <- p +
        labs(subtitle = plot_subtitle) +
        theme(
          plot.subtitle = element_text(
            size = 8, hjust = 0, margin = margin(b = 2),
            color = plot_colours[species]
          )
        )
    }

    if (is_single_cu) {
      p <- p + theme(
        axis.text.y = element_blank(),
        axis.text.y.left = element_blank()
      )
    } else {
      p <- p + theme(
        axis.text.y = ggtext::element_markdown(size = y_text_size, hjust = 1, margin = margin(r = 2)),
        axis.text.y.left = ggtext::element_markdown(size = y_text_size, hjust = 1, margin = margin(r = 2))
      )
    }

    # Separator before overall indicator(s)
    if (!is.null(overall_score_cols) && length(overall_score_cols) > 0) {
      all_inds <- levels(data_sp$indicator)
      first_overall_idx <- which(all_inds %in% overall_score_cols)[1]
      if (!is.na(first_overall_idx)) {
        p <- p + geom_vline(xintercept = first_overall_idx - 0.5, color = "black", linewidth = 1)
        overall_positions <- which(all_inds %in% overall_score_cols)
        if (length(overall_positions) > 1) {
          for (i in 1:(length(overall_positions) - 1)) {
            p <- p + geom_vline(
              xintercept = overall_positions[i] + 0.5,
              color = "grey50", linewidth = 0.5, linetype = "dashed"
            )
          }
        }
      }
    }

    if (is_bottom_row) {
      p <- p + theme(
        axis.text.x = element_text(size = 8, angle = 45, hjust = 1, vjust = 1, margin = margin(t = 2))
      )
    } else {
      p <- p + theme(axis.text.x = element_blank())
    }

    return(p)
  })

  # ---- Legend (indicator key) ----
  indicators_in_plot <- as.character(indicator_order)
  legend_data <- indicators_metadata %>%
    filter(.data$abbrev %in% indicators_in_plot) %>%
    select(abbrev, name) %>%
    mutate(abbrev = factor(abbrev, levels = indicators_in_plot)) %>%
    arrange(abbrev) %>%
    rename(Code = abbrev, Description = name)

  # Add any overall codes missing in metadata
  if (!is.null(overall_score_cols) && length(overall_score_cols) > 0) {
    for (code in overall_score_cols) {
      if ((code %in% indicators_in_plot) && !(code %in% legend_data$Code)) {
        legend_data <- bind_rows(legend_data, tibble(Code = code, Description = paste("Overall:", code)))
      }
    }
    legend_data <- legend_data %>%
      mutate(Code = factor(Code, levels = indicators_in_plot)) %>%
      arrange(Code) %>%
      mutate(Code = as.character(Code))
  }

  legend_grob <- gridExtra::tableGrob(
    legend_data,
    rows = NULL,
    theme = gridExtra::ttheme_minimal(
      core = list(
        fg_params = list(hjust = 0, x = 0.05, fontsize = 8),
        bg_params = list(fill = "white", col = "grey70", lwd = 0.3)
      ),
      colhead = list(
        fg_params = list(fontface = "bold", fontsize = 9),
        bg_params = list(fill = "grey90", col = "grey70", lwd = 0.3)
      )
    )
  )

  legend_plot <- ggplot() +
    annotation_custom(legend_grob) +
    theme_void() +
    labs(title = "Indicator Key") +
    theme(
      plot.title = element_text(face = "bold", size = 10, hjust = 0, margin = margin(b = 5)),
      plot.margin = margin(2, 2, 2, 2)
    )

  plot_list <- c(plot_list, list(legend_plot))

  # ---- Assemble with patchwork ----
  n_rows <- ceiling((length(species_list) + 1) / ncol)
  if (!uniform_height) {
    height_matrix <- matrix(c(heights, 0.5), nrow = n_rows, ncol = ncol, byrow = TRUE)
    row_heights <- apply(height_matrix, 1, max, na.rm = TRUE)
    p <- wrap_plots(plot_list, ncol = ncol) + plot_layout(heights = row_heights)
  } else {
    heights_with_legend <- c(rep(1, length(species_list)), 0.5)
    p <- wrap_plots(plot_list, ncol = ncol) + plot_layout(heights = heights_with_legend)
  }

  # ---- Title / annotation ----
  if (!is.null(category_name)) {
    p <- p + plot_annotation(
      title = category_name
    ) +
    theme(
      plot.title = element_text(face = "bold", size = 12, hjust = 0.5, margin = margin(b = 10)),
      plot.margin = margin(5, 5, 5, 5),
      plot.background = element_rect(fill = "white", color = NA)
    )
  } else {
    p <- p + plot_annotation() +
    theme(
      plot.margin = margin(5, 5, 5, 5),
      plot.background = element_rect(fill = "white", color = NA)
    )
  }

  return(p)
}



#' Tile plots (long-format): all indicators by category + category avg (from scores_tidy)
#' Uses `all_std_long` for per-indicator tiles and `scores_tidy` for category averages & overall metrics.
#' No averaging across GCMs: selects ensemble -> gcm==0/ensemble-like -> gcm_pick -> first deterministic.
species_category_tile_plot_from_scores <- function(all_std_long,
                                                   scores_tidy,
                                                   species_pick,
                                                   cu_name_col = "CVIS_NAME",
                                                   indicators_metadata = tbl_indicators, # must have: abbrev, category
                                                   species_palette = NULL,
                                                   brewer_palette = cvis_risk_palette,
                                                   palette_direction = -1,
                                                   rank_method = "catavgs", # "catavgs"|"avgall"|"avgcube"
                                                   show_values = TRUE,
                                                   sort_by_rank = TRUE,
                                                   dsmodel_pick = NULL,
                                                   gcm_pick = NULL,
                                                   ensemble_regex = "(?i)(ens|ensemble|mmem|multi|mean|avg)") {
  # ---- Input checks ----
  req_ind_cols <- c("indicator", "SPECIES_NAME", "FULL_CU_IN", "CVIS_NAME", "std_value")
  if (!all(req_ind_cols %in% names(all_std_long))) {
    stop("all_std_long is missing: ", paste(setdiff(req_ind_cols, names(all_std_long)), collapse = ", "))
  }
  req_score_cols <- c(
    "FULL_CU_IN", "SPECIES_NAME", "CVIS_NAME", "gcm", "rcp", "period_code",
    "method", "category", "score", "rankspecies"
  )
  if (!all(req_score_cols %in% names(scores_tidy))) {
    stop("scores_tidy is missing: ", paste(setdiff(req_score_cols, names(scores_tidy)), collapse = ", "))
  }
  if (!cu_name_col %in% names(all_std_long)) {
    stop(sprintf("Column '%s' not found in all_std_long", cu_name_col))
  }
  if (!(rank_method %in% c("catavg", "avgall", "avgcube"))) {
    stop("rank_method must be one of: 'catavg','avgall','avgcube'.")
  }
  if (is.null(indicators_metadata) ||
    !all(c("abbrev", "category") %in% names(indicators_metadata))) {
    stop("Please provide 'indicators_metadata' with columns: 'abbrev' and 'category'.")
  }

  # ---- Default species palette ----
  if (is.null(species_palette)) {
    species_palette <- c(
      "Chinook" = "#E69F00", "Chum" = "#56B4E9", "Coho" = "#009E73", "Pink" = "#F0E442", "Sockeye" = "#D55E00"
    )
  }
  sp_color <- species_palette[species_pick]
  if (is.na(sp_color) || is.null(sp_color)) sp_color <- "black"

  # ---- Filter species + scenario ----
  d_ind <- all_std_long %>% filter(.data$SPECIES_NAME == species_pick)
  d_scores <- scores_tidy %>% filter(.data$SPECIES_NAME == species_pick)

  # Filter d_ind dynamically by indicator to enable fallback to 0
  d_ind_list <- split(d_ind, d_ind$indicator)
  for (ind in names(d_ind_list)) {
    df_temp <- d_ind_list[[ind]]

    if (!is.null(dsmodel_pick) && "dsmodel" %in% names(df_temp) && length(dsmodel_pick) > 0) {
      if (any(df_temp$dsmodel %in% dsmodel_pick, na.rm = TRUE)) {
        df_temp <- df_temp %>% filter(dsmodel %in% dsmodel_pick)
      }
    }

    d_ind_list[[ind]] <- df_temp
  }

  d_ind <- bind_rows(d_ind_list)

  if (nrow(d_ind) == 0 || nrow(d_scores) == 0) {
    stop("No rows after filtering for species / rcp / period (and dsmodel).")
  }

  # ---- Select ONE row per CU×indicator for indicators (no GCM averaging) ----
  has_ens_i <- "ensemble" %in% names(d_ind)
  has_gcm_i <- "gcm" %in% names(d_ind)
  d_i <- d_ind
  if (has_ens_i && any(d_i$ensemble %in% TRUE, na.rm = TRUE)) {
    d_i <- d_i %>% filter(.data$ensemble %in% TRUE)
  } else if (has_gcm_i) {
    if (is.numeric(d_i$gcm)) {
      if (any(d_i$gcm == 0, na.rm = TRUE)) d_i <- d_i %>% filter(.data$gcm == 0)
    } else if (is.character(d_i$gcm)) {
      if (any(stringr::str_detect(d_i$gcm, ensemble_regex), na.rm = TRUE)) {
        d_i <- d_i %>% filter(stringr::str_detect(.data$gcm, ensemble_regex))
      }
    }
  }
  if (!is.null(gcm_pick) && has_gcm_i) d_i <- d_i %>% filter(.data$gcm %in% gcm_pick)

  # ---- Ensure a single, clean 'category' column on the indicator side ----
  # If all_std_long already has 'category' for indicators, keep it; otherwise join metadata safely.
  if (!"category" %in% names(d_i)) {
    # join metadata but rename meta column to avoid suffixes, then coalesce to a single 'category'
    meta_map <- indicators_metadata %>%
      select(indicator = abbrev, category_meta = category)
    d_i <- d_i %>%
      left_join(meta_map, by = "indicator") %>%
      mutate(category = category_meta) %>%
      select(-category_meta)
  }
  # After this point, d_i MUST have 'category'
  if (!"category" %in% names(d_i)) {
    stop("Internal error: 'category' column missing after metadata join on indicators.")
  }

  # Deterministic single row per CU×indicator×category
  d_i <- d_i %>%
    arrange(.data$FULL_CU_IN, .data[[cu_name_col]], .data$indicator, .data$category, .data$gcm) %>%
    group_by(.data$FULL_CU_IN, .data$SPECIES_NAME, .data[[cu_name_col]], .data$indicator, .data$category) %>%
    slice_head(n = 1) %>%
    ungroup()

  ind_vals <- d_i %>%
    transmute(FULL_CU_IN, SPECIES_NAME,
      cu_lab = .data[[cu_name_col]],
      indicator, category, value = std_value
    )

  # ---- Select ONE row per CU×(method,category) for scores_tidy (no GCM averaging) ----
  has_gcm_s <- "gcm" %in% names(d_scores)
  d_s <- d_scores
  if (has_gcm_s) {
    if (is.numeric(d_s$gcm) && any(d_s$gcm == 0, na.rm = TRUE)) {
      d_s <- d_s %>% filter(.data$gcm == 0)
    } else if (!is.null(gcm_pick)) d_s <- d_s %>% filter(.data$gcm %in% gcm_pick)
  }
  d_s <- d_s %>%
    arrange(.data$FULL_CU_IN, .data[[cu_name_col]], .data$method, .data$category, .data$gcm) %>%
    group_by(.data$FULL_CU_IN, .data$SPECIES_NAME, .data[[cu_name_col]], .data$method, .data$category) %>%
    slice_head(n = 1) %>%
    ungroup()

  # ---- Category averages FROM scores_tidy (method == "avg", category in fwrs..gen) ----
  cat_avgs_from_scores <- d_s %>%
    filter(.data$method == "avg", .data$category %in% c("fwrs", "migr", "mar", "dem", "gen")) %>%
    transmute(
      FULL_CU_IN, SPECIES_NAME,
      cu_lab = .data[[cu_name_col]],
      category,
      indicator = paste0("avg", category), # e.g., "avgfwrs"
      value = score
    )

  # ---- Combine per-indicator values with category avg columns ----
  cat_panels <- bind_rows(ind_vals, cat_avgs_from_scores)

  # ---- Overall metrics FROM scores_tidy ----
  overall_long <- d_s %>%
    filter(.data$category == "all", .data$method %in% c("catavg", "avgall", "avgcube")) %>%
    transmute(
      FULL_CU_IN, SPECIES_NAME,
      cu_lab = .data[[cu_name_col]],
      indicator = dplyr::recode(method,
        "catavg" = "catavg",
        "avgall" = "avgall",
        "avgcube" = "avgcube"
      ),
      value = score
    )

  # ---- CU order from rankspecies in scores_tidy (based on chosen rank_method) ----
  rank_map <- c(catavg = "catavg", avgall = "avgall", avgcube = "avgcube")
  target_method <- unname(rank_map[[rank_method]])
  ranks <- d_s %>%
    filter(.data$method == target_method, .data$category == "all") %>%
    arrange(rankspecies) %>%
    transmute(cu_lab = .data[[cu_name_col]], rank_within = rankspecies)

  if (nrow(ranks) == 0) {
    stop("No ranks found in scores_tidy for method=", target_method, " (category == 'all').")
  }

  cu_order <- ranks$cu_lab
  cu_labels <- paste0("<span style='color:", sp_color, "'>", cu_order, "</span>")

  # ---- Normalize colour per indicator (0..1) ----
  norm_by_indicator <- function(df) {
    lims <- df %>%
      group_by(indicator) %>%
      summarise(
        min_val = suppressWarnings(min(value, na.rm = TRUE)),
        max_val = suppressWarnings(max(value, na.rm = TRUE)),
        .groups = "drop"
      ) %>%
      mutate(
        min_val = ifelse(is.infinite(min_val), NA_real_, min_val),
        max_val = ifelse(is.infinite(max_val), NA_real_, max_val)
      )
    df %>%
      left_join(lims, by = "indicator") %>%
      mutate(
        rng_ok = !is.na(min_val) & !is.na(max_val) & (max_val > min_val),
        value_norm = ifelse(rng_ok, (value - min_val) / (max_val - min_val), 0.5)
      )
  }

  cat_panels_norm <- norm_by_indicator(cat_panels) %>%
    mutate(
      cu_lab_html = paste0("<span style='color:", sp_color, "'>", cu_lab, "</span>"),
      cu_lab_html = factor(cu_lab_html, levels = cu_labels)
    )

  overall_norm <- norm_by_indicator(overall_long) %>%
    mutate(
      cu_lab_html = paste0("<span style='color:", sp_color, "'>", cu_lab, "</span>"),
      cu_lab_html = factor(cu_lab_html, levels = cu_labels),
      indicator = factor(indicator, levels = c("catavg", "avgall", "avgcube"))
    )

  # ---- Build per-category panel data (EXPLICIT filtering so panels differ) ----
  build_panel_df <- function(df, code, title) {
    df_t <- df %>%
      filter(.data$category == code | .data$indicator == paste0("avg", code))
    if (nrow(df_t) == 0) {
      return(NULL)
    }

    # Put avg<code> last
    non_avg <- df_t %>%
      filter(.data$indicator != paste0("avg", code)) %>%
      pull(indicator) %>%
      unique()
    ind_levels <- c(non_avg, paste0("avg", code))

    list(
      data    = df_t %>% mutate(indicator = factor(indicator, levels = ind_levels)),
      title   = title
    )
  }

  cat_defs <- list(
    list(code = "fwrs", title = "Freshwater Rearing & Spawning"),
    list(code = "migr", title = "Upstream Migration"),
    list(code = "mar", title = "Marine"),
    list(code = "dem", title = "Demographics"),
    list(code = "gen", title = "Genetics")
  )
  panel_data_list <- lapply(cat_defs, function(def) build_panel_df(cat_panels_norm, def$code, def$title))

  # ---- Plot helper ----
  make_tile <- function(pdat) {
    if (is.null(pdat) || is.null(pdat$data) || nrow(pdat$data) == 0) {
      return(NULL)
    }
    dat <- pdat$data
    p <- ggplot(dat, aes(x = indicator, y = cu_lab_html)) +
      geom_tile(aes(fill = value_norm), color = "white", linewidth = 0.3, na.rm = FALSE) +
      {
        if (isTRUE(show_values)) {
          geom_text(aes(label = ifelse(is.na(value), "", sprintf("%.2f", value))),
            size = 2.2, color = "black", na.rm = TRUE
          )
        }
      } +
      scale_fill_distiller(
        palette = brewer_palette, direction = palette_direction,
        na.value = "grey95", limits = c(0, 1)
      ) +
      theme_minimal(base_size = 9) +
      theme(
        axis.title = element_blank(),
        axis.text.y = ggtext::element_markdown(size = 8, hjust = 1),
        axis.text.y.left = ggtext::element_markdown(size = 8, hjust = 1),
        axis.text.x = element_text(size = 7, angle = 45, hjust = 1, vjust = 1),
        legend.position = "none",
        panel.grid = element_blank(),
        panel.border = element_rect(color = "grey70", fill = NA, linewidth = 0.5),
        plot.title = element_text(face = "bold", size = 10, hjust = 0.5),
        plot.margin = margin(2, 2, 2, 2)
      ) +
      labs(title = pdat$title)

    # Vertical separator before the last column if it's avg<code>
    last_is_avg <- grepl("^avg", tail(levels(dat$indicator), 1))
    if (isTRUE(last_is_avg)) {
      p <- p + geom_vline(xintercept = length(levels(dat$indicator)) - 0.5, color = "black", linewidth = 0.8)
    }
    p
  }

  panel_plots <- lapply(panel_data_list, make_tile)
  panel_plots <- panel_plots[!vapply(panel_plots, is.null, logical(1))]

  # ---- Overall panel ----
  p_overall <- ggplot(overall_norm, aes(x = indicator, y = cu_lab_html)) +
    geom_tile(aes(fill = value_norm), color = "white", linewidth = 0.3, na.rm = FALSE) +
    {
      if (isTRUE(show_values)) {
        geom_text(aes(label = ifelse(is.na(value), "", sprintf("%.2f", value))),
          size = 2.2, color = "black", na.rm = TRUE
        )
      }
    } +
    scale_fill_distiller(
      palette = brewer_palette, direction = palette_direction,
      na.value = "grey95", limits = c(0, 1)
    ) +
    theme_minimal(base_size = 9) +
    theme(
      axis.title = element_blank(),
      axis.text.y = ggtext::element_markdown(size = 8, hjust = 1),
      axis.text.y.left = ggtext::element_markdown(size = 8, hjust = 1),
      axis.text.x = element_text(size = 7, angle = 45, hjust = 1, vjust = 1),
      legend.position = "none",
      panel.grid = element_blank(),
      panel.border = element_rect(color = "grey70", fill = NA, linewidth = 0.5),
      plot.title = element_text(face = "bold", size = 10, hjust = 0.5),
      plot.margin = margin(2, 2, 2, 2)
    ) +
    labs(title = "Overall Vulnerability") +
    geom_vline(xintercept = 1.5, color = "black", linewidth = 0.8) +
    geom_vline(xintercept = 2.5, color = "grey50", linewidth = 0.5, linetype = "dashed")

  # ---- Optional: alphabetical order instead of ranks ----
  if (!isTRUE(sort_by_rank)) {
    cu_alpha <- d_i %>%
      distinct(cu_lab = .data[[cu_name_col]]) %>%
      arrange(cu_lab) %>%
      pull(cu_lab)
    cu_labels_alpha <- paste0("<span style='color:", sp_color, "'>", cu_alpha, "</span>")
    relevel_y <- function(p) p + scale_y_discrete(limits = cu_labels_alpha)
    panel_plots <- lapply(panel_plots, relevel_y)
    p_overall <- relevel_y(p_overall)
  }

  # ---- Combine panels ----
  plots_all <- c(panel_plots, list(p_overall))
  combined <- wrap_plots(plots_all, ncol = 2) +
    plot_layout(guides = "collect") +
    theme(
      plot.margin = margin(5, 5, 5, 5)
    )

  return(combined)
}


# p <- species_category_tile_plot_from_scores(all_std_long,
#                                        scores_tidy,
#                                        species_pick = "Chinook",
#                                        rank_method = "catavg")
# 
# print(p)


# ==================== 3. Indicator Correlation Matrix ====================

get_correlation_matrix <- function(data,
                                   indicators_choose = tbl_indicators$abbrev,
                                   use_standardized = T) {
  data_sub <- subset_ind_table(data,
    indicators_choose,
    get_raw = !use_standardized,
    get_std = use_standardized,
    get_gcm = F
  )

  # take column names that contain prefix with model type
  cols_sub <- names(data_sub)[str_detect(names(data_sub), paste0(indicators_choose, collapse = "|"))]

  # filter to numeric columns only
  numeric_cols <- cols_sub[sapply(data_sub[cols_sub], is.numeric)]

  # take just numeric indicator columns, remove sp and id
  cor_data <- data_sub %>%
    select(all_of(numeric_cols))

  # --- remove "std" from column names
  new_names <- numeric_cols %>%
    stringr::str_remove("(?i)^std_")
  names(cor_data) <- new_names

  cor(cor_data, use = "pairwise.complete.obs")
}



# ==================== 4. Two-Indicator Scatter Plot Comparison ====================

xy_indicator_plot <- function(data,
                              x_pick = "lowQpdelta",
                              y_pick = "st8pdelta",
                              use_raw = T,
                              point_col = "SPECIES_NAME") {
  plot_data_x <- subset_ind_table(data,
    indicators_choose = x_pick,
    id_col = "CVIS_NAME",
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
    id_col = "CVIS_NAME",
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
      CVIS_NAME,
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
    "status" = arrange(table_data, status_order, SPECIES_NAME, CVIS_NAME),
    "abundance" = arrange(table_data, desc(GenAverage)),
    "cu_name" = arrange(table_data, SPECIES_NAME, CVIS_NAME),
    arrange(table_data, status_order, SPECIES_NAME, CVIS_NAME) # default
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
      CVIS_NAME = "CU Name",
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
#'   data with columns: FULL_CU_IN, CVIS_NAME, SPECIES_NAME, life_stage, start,
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
plot_timing_comparison <- function(cu_timing_long,
                                   cu_select = NULL,
                                   species_select = NULL,
                                   life_stages = c(
                                     "spawning", "run_timing",
                                     "ocean_entry", "freshwater_migration"
                                   ),
                                   sort_by = "species",
                                   show_peaks = TRUE,
                                   species_palette = NULL,
                                   life_stage_palette = NULL,
                                   date_breaks = "1 month",
                                   y_text_size = 8) {
  # Default species palette if not provided
  if (is.null(species_palette)) {
    species_palette <- c(
      "Chinook" = "#E69F00",
      "Chum" = "#56B4E9",
      "Coho" = "#009E73",
      "Pink" = "#F0E442",
      "Sockeye" = "#D55E00"
    )
  }

  # Default life stage palette if not provided
  if (is.null(life_stage_palette)) {
    life_stage_palette <- c(
      "Spawning" = "#66C2A5",
      "Upstream Run Timing" = "#FC8D62",
      "Ocean Entry" = "#8DA0CB",
      "Juvenile FW Migration" = "#E78AC3"
    )
  }

  # Filter data
  data_plot <- cu_timing_long

  if (!is.null(cu_select)) {
    data_plot <- data_plot %>% filter(FULL_CU_IN %in% cu_select)
  }

  if (!is.null(species_select)) {
    data_plot <- data_plot %>% filter(SPECIES_NAME %in% species_select)
  }

  if (!is.null(life_stages)) {
    data_plot <- data_plot %>% filter(life_stage %in% life_stages)
  }

  # Check if life_stage_label column exists, if not create it from life_stage
  if (!"life_stage_label" %in% names(data_plot)) {
    data_plot <- data_plot %>%
      mutate(life_stage_label = case_when(
        life_stage == "spawning" ~ "Spawning",
        life_stage == "run_timing" ~ "Upstream Run Timing",
        life_stage == "ocean_entry" ~ "Ocean Entry",
        life_stage == "freshwater_migration" ~ "Juvenile FW Migration",
        TRUE ~ life_stage
      ))
  }

  # Ensure life_stage_label is a factor with correct levels
  data_plot <- data_plot %>%
    mutate(life_stage_label = factor(life_stage_label,
      levels = c(
        "Spawning", "Upstream Run Timing",
        "Ocean Entry", "Juvenile FW Migration"
      )
    ))

  # Convert day of year to date for plotting
  data_plot <- data_plot %>%
    mutate(
      date_start = as.Date("2000-01-01") + start,
      date_peak = as.Date("2000-01-01") + peak,
      date_end = as.Date("2000-01-01") + end
    ) %>%
    filter(!is.na(start), !is.na(end))

  # Create data quality categories for shape mapping
  data_plot <- data_plot %>%
    mutate(dat_qual_category = case_when(
      dat_qual %in% c(1, 2) ~ "High (1-2)",
      dat_qual %in% c(3, 4) ~ "Medium (3-4)",
      dat_qual %in% c(5, 6) ~ "Low (5-6)",
      TRUE ~ "Unknown"
    )) %>%
    mutate(dat_qual_category = factor(dat_qual_category,
      levels = c(
        "High (1-2)", "Medium (3-4)",
        "Low (5-6)", "Unknown"
      )
    ))

  # Create colored CU labels using species colors
  data_plot <- data_plot %>%
    mutate(cu_label_colored = paste0(
      "<span style='color:", species_palette[SPECIES_NAME], "'>",
      CVIS_NAME, " (", FULL_CU_IN, ")", "</span>"
    ))

  # Sort CUs
  if (sort_by == "species") {
    data_plot <- data_plot %>%
      arrange(SPECIES_NAME, FULL_CU_IN) %>%
      mutate(cu_label_colored = factor(cu_label_colored, levels = unique(cu_label_colored)))
  } else if (sort_by == "peak_spawn") {
    sp_peaks <- data_plot %>%
      filter(life_stage == "spawning") %>%
      arrange(peak)
    data_plot <- data_plot %>%
      mutate(cu_label_colored = factor(cu_label_colored,
        levels = unique(sp_peaks$cu_label_colored)
      ))
  } else if (sort_by == "peak_oe") {
    oe_peaks <- data_plot %>%
      filter(life_stage == "ocean_entry") %>%
      arrange(peak)
    data_plot <- data_plot %>%
      mutate(cu_label_colored = factor(cu_label_colored,
        levels = unique(oe_peaks$cu_label_colored)
      ))
  }

  # Create plot
  p <- ggplot(data_plot, aes(y = cu_label_colored))

  # Add range segments colored by life stage
  p <- p + geom_segment(
    aes(
      x = date_start, xend = date_end,
      yend = cu_label_colored,
      color = life_stage_label
    ),
    linewidth = 4, alpha = 0.7
  )

  # Add peaks if requested, with shape mapped to data quality category
  if (show_peaks) {
    p <- p + geom_point(
      aes(
        x = date_peak,
        color = life_stage_label,
        shape = dat_qual_category
      ),
      size = 3, fill = "white", stroke = 1.2
    ) +
      scale_shape_manual(
        name = "Data Quality",
        values = c(
          "High (1-2)" = 21, # Circle (filled)
          "Medium (3-4)" = 24, # Triangle
          "Low (5-6)" = 22, # Square
          "Unknown" = 4 # X
        ),
        guide = guide_legend(
          override.aes = list(size = 3, fill = "white", stroke = 1.2)
        )
      )
  }

  # Formatting
  p <- p +
    scale_x_date(
      date_breaks = date_breaks,
      date_labels = "%b",
      limits = c(as.Date("2000-01-01"), as.Date("2000-12-31")),
      expand = c(0.02, 0)
    ) +
    scale_color_manual(
      values = life_stage_palette,
      name = "Life Stage"
    ) +
    labs(
      title = "Life History Timing Comparison Across Conservation Units",
      x = "Date",
      y = "Conservation Unit",
      caption = "Point shape indicates data quality: ● = High (1-2), ▲ = Medium (3-4), ■ = Low (5-6)"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
      axis.text.y = ggtext::element_markdown(size = y_text_size, hjust = 1),
      axis.text.y.left = ggtext::element_markdown(size = y_text_size, hjust = 1),
      axis.title = element_text(size = 11, face = "bold"),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "right",
      legend.title = element_text(face = "bold", size = 10),
      legend.text = element_text(size = 9),
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
      plot.caption = element_text(size = 8, color = "grey50", hjust = 0),
      plot.margin = margin(10, 10, 10, 10)
    )

  return(p)
}


# ==================== 7. Combined Tile Plot for All CUs & Indicators ====================

#' Tile plot of all indicators and CUs, plus overall score (catavg)
#'
#' @param all_std_long Standardized indicators long format table
#' @param scores_tidy Combined scores table from 4a
#' @param gcm_pick Character, GCM code (default "9" for ensemble)
#' @param brewer_palette Character, color palette (default "RdYlGn" gives green to red with direction -1)
#' @param palette_direction Numeric, direction of palette
indicator_cu_tile_plot <- function(all_std_long,
                                   scores_tidy,
                                   indicators_metadata = tbl_indicators,
                                   gcm_pick = "9",
                                   dsmodel_pick = NULL,
                                   brewer_palette = cvis_risk_palette,
                                   palette_direction = -1) {
  require(ggtext)
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
    group_by(FULL_CU_IN, CVIS_NAME, CU_COMMON_NAME, SMU_SIMPLE, SPECIES_NAME, indicator) %>%
    slice_head(n = 1) %>%
    ungroup() %>%
    select(FULL_CU_IN, CVIS_NAME, CU_COMMON_NAME, SMU_SIMPLE, SPECIES_NAME, indicator, value = std_value)
  
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
    group_by(FULL_CU_IN, CVIS_NAME, CU_COMMON_NAME, SMU_SIMPLE, SPECIES_NAME) %>%
    slice_head(n = 1) %>%
    ungroup() %>%
    select(FULL_CU_IN, CVIS_NAME, CU_COMMON_NAME, SMU_SIMPLE, SPECIES_NAME, value = score100_all) %>%
    mutate(value = value / 100, indicator = "catavg", category = "overall")
  
  # Combine
  plot_dat <- bind_rows(ind_dat, score_dat)
  
  # Set desired species order (Chinook first, then Chum)
  sp_base_order <- c("Chinook", "Chum", "Coho", "Sockeye", "Pink")
  plot_dat <- plot_dat %>%
    mutate(SPECIES_NAME = factor(SPECIES_NAME, levels = intersect(sp_base_order, unique(SPECIES_NAME))))
  
  if (!exists("species_palette")) {
    species_palette <- c("Chinook" = "#E69F00", "Chum" = "#56B4E9", "Coho" = "#009E73", "Pink" = "#F0E442", "Sockeye" = "#D55E00")
  }
  
  # Order CUs by SPECIES_NAME then by SMU then by FULL_CU_IN, then form colored labels
  cu_order <- plot_dat %>%
    distinct(FULL_CU_IN, CU_COMMON_NAME, SPECIES_NAME, SMU_SIMPLE) %>%
    arrange(SPECIES_NAME, SMU_SIMPLE, FULL_CU_IN) %>%
    mutate(
      color = smu_palette[as.character(SMU_SIMPLE)],
      color = ifelse(is.na(color), "black", color),
      colored_label = paste0("<span style='color:", color, "'>", CU_COMMON_NAME, "</span>")
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
  
  # Plot
  p <- ggplot(plot_dat, aes(x = indicator, y = colored_label)) +
    geom_tile(aes(fill = value), color = "white", linewidth = 0.3) +
    scale_fill_distiller(
      palette = brewer_palette, direction = palette_direction,
      limits = c(0, 1), na.value = "grey95"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 10, color = "black"),
      axis.text.y = ggtext::element_markdown(size = y_text_size, hjust = 1, vjust = 0.5),
      axis.text.y.left = ggtext::element_markdown(size = y_text_size, hjust = 1, vjust = 0.5),
      legend.position = "right",
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
                                      brewer_palette = cvis_risk_palette,
                                      palette_direction = -1) {
  require(ggtext)
  
  # ---- Input checks ----
  req_cols <- c("FULL_CU_IN", "SPECIES_NAME", "CVIS_NAME", "CU_COMMON_NAME", "SMU_SIMPLE", "method", "category", "score100_all")
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

  if (!exists("species_palette")) {
    species_palette <- c("Chinook" = "#E69F00", "Chum" = "#56B4E9", "Coho" = "#009E73", "Pink" = "#F0E442", "Sockeye" = "#D55E00")
  }

  # Order CUs by SPECIES_NAME then by SMU then by FULL_CU_IN, then form colored labels
  cu_order <- plot_dat %>%
    distinct(FULL_CU_IN, CU_COMMON_NAME, SPECIES_NAME, SMU_SIMPLE) %>%
    arrange(SPECIES_NAME, SMU_SIMPLE, FULL_CU_IN) %>%
    mutate(
      color = smu_palette[as.character(SMU_SIMPLE)],
      color = ifelse(is.na(color), "black", color),
      colored_label = paste0("<span style='color:", color, "'>", CU_COMMON_NAME, "</span>")
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

  # Plot
  p <- ggplot(plot_dat, aes(x = method_label, y = colored_label)) +
    geom_tile(aes(fill = score100_all), color = "white", linewidth = 0.3) +
    scale_fill_distiller(
      palette = brewer_palette, direction = palette_direction,
      limits = c(0, 100), na.value = "grey95"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 9, color = "black"),
      axis.text.y = ggtext::element_markdown(size = y_text_size, hjust = 1, vjust = 0.5),
      axis.text.y.left = ggtext::element_markdown(size = y_text_size, hjust = 1, vjust = 0.5),
      legend.position = "right",
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




# ==================== 8. Marine Adaptive Zone Lollipop Chart ====================

#' Lollipop plot (long-format) for a single RCP/period with GCM variation

plot_maz_lollipop <- function(maz_all,
                          indicator_picks = c("SSTproj", "SSTrate", "CImpact"),
                          dsmodel_pick = NULL,
                          indicator_name = NULL,
                          indicator_unit = NULL,
                          use_standardized = FALSE, # default FALSE (Y uses raw)
                          show_gcm_points = FALSE,
                          tbl = tbl_indicators) {
  # ---- CU roster (ensures all CUs are shown regardless of data presence) 
  maz_roster <- maz_all %>%
    distinct(MAZ)
  
  # ---- Resolve value column for plotting Y (ranges & center_y) 
  val_col <- if (use_standardized) {
    if (!"std_value" %in% names(maz_all)) {
      stop("Column 'std_value' not found in maz_all.")
    }
    "std_value"
  } else {
    if ("raw_value" %in% names(maz_all)) {
      "raw_value"
    } else if ("value" %in% names(maz_all)) {
      "value"
    } else {
      stop("No raw value column found. Provide 'raw_value' or 'value', or set use_standardized = TRUE.")
    }
  }
  
  plot_list <- list()
  
  for (i in seq_along(indicator_picks)) {
    ind <- indicator_picks[i]
    
    # ---- Filter scenarios/models ----
    dat_full <- maz_all %>%
      filter(
        .data$indicator == ind
      )
    
    if (nrow(dat_full) == 0) {
      p_empty <- ggplot() + 
        theme_void() + 
        labs(title = ind, subtitle = "No data found")
      plot_list[[i]] <- p_empty
      next
    }
    
    if (!is.null(dsmodel_pick) && length(dsmodel_pick) > 0 && "dsmodel" %in% names(dat_full)) {
      dat_full <- dat_full %>% filter(.data$dsmodel %in% dsmodel_pick)
    }
    
    # Ensure rcp and dsmodel are factor-like for plotting
    dat_full <- dat_full %>%
      mutate(
        rcp = factor(rcp),
        dsmodel = if ("dsmodel" %in% names(.)) factor(dsmodel) else factor("default")
      )
    
    # ---- Reduce to needed columns ----
    dat <- dat_full %>%
      select(MAZ, gcm, rcp, dsmodel,
             val = dplyr::all_of(val_col)
      )
    
    # ---- Summaries across GCM per CU/Scenario/Model 
    maz_summary <- dat %>%
      group_by(MAZ, rcp, dsmodel) %>%
      summarise(
        n_gcm      = dplyr::n_distinct(gcm[!is.na(val)]),
        min_gcm    = if (sum(!is.na(val)) > 0) min(val, na.rm = TRUE) else NA_real_,
        max_gcm    = if (sum(!is.na(val)) > 0) max(val, na.rm = TRUE) else NA_real_,
        center_y   = if (sum(!is.na(val)) > 0) mean(val, na.rm = TRUE) else NA_real_, # Y position
        .groups    = "drop"
      )
    
    # ---- Right-join summaries to the full CU roster to ensure all CUs are present 
    maz_plot <- maz_roster %>%
      left_join(maz_summary, by = c("MAZ"))
    
    # ---- Plot ----
    dodge_width <- 0.8
    p <- ggplot(maz_plot, aes(x = MAZ, color = rcp, group = interaction(rcp, dsmodel)))
    
    
    # 1. Background "Cloud" for GCM uncertainty (wide grey bar)
    p <- p +
      geom_segment(aes(xend = MAZ, y = min_gcm, yend = max_gcm),
                   color = "grey92", linewidth = 6, alpha = 0.8, na.rm = TRUE,
                   position = position_dodge(width = dodge_width)
      )
    
    # 2. Inner GCM range line (thin line for contrast)
    p <- p +
      geom_segment(aes(xend = MAZ, y = min_gcm, yend = max_gcm),
                   linewidth = 1.5, alpha = 0.8, na.rm = TRUE,
                   position = position_dodge(width = dodge_width)
      )
    
    # 3. Lollipop Head (Future ensemble mean color-coded by risk score)
    p <- p +
      geom_point(aes(y = center_y, shape = dsmodel),
                 color = "black", size = 3.2, stroke = 0.8, na.rm = TRUE,
                 position = position_dodge(width = dodge_width)
      ) 
    
    # Saturated Scenario Colors
    p <- p +
      scale_color_manual(
        name = "Scenario (RCP)",
        values = c("45" = "darkblue", "85" = "#B22222", "0" = "black"),
        na.translate = FALSE
      ) +
      scale_shape_manual(
        name = "Downscaling method",
        values = c(21, 24, 22, 23, 25),
        na.translate = FALSE
      )
    
    # Labels
    ind_name <- if (is.null(indicator_name) || length(indicator_name) < i || is.na(indicator_name[i])) {
      if (!is.null(tbl) && ind %in% tbl$abbrev) {
        tbl$name[tbl$abbrev == ind][1]
      } else {
        ind
      }
    } else {
      indicator_name[i]
    }
    
    ind_unit <- if (is.null(indicator_unit) || length(indicator_unit) < i || is.na(indicator_unit[i])) {
      if (!is.null(tbl) && ind %in% tbl$abbrev) {
        tbl$unit[tbl$abbrev == ind][1]
      } else {
        ""
      }
    } else {
      indicator_unit[i]
    }
    
    subtitle_lab <- ind_name
    y_lab <- if (isTRUE(use_standardized)) {
      "Standardized score"
    } else {
      if (is.null(ind_unit) || is.na(ind_unit) || ind_unit == "") "Value" else ind_unit
    }
    
    p <- p +
      labs(
        subtitle = subtitle_lab,
        y = y_lab,
        x = NULL
      ) +
      coord_flip() +
      theme_minimal(base_size = 11) +
      theme(
        axis.text.y = ggtext::element_markdown(size = 8.5),
        panel.grid.major.y = element_blank()
      )
    
    # Hide y-axis labels and ticks for panels after the first to prevent repetition
    if (i > 1) {
      p <- p + theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
      )
    }
    
    plot_list[[i]] <- p
  }
  
  # Determine common caption info
  dsmodel_lab <- if (!is.null(dsmodel_pick) && length(dsmodel_pick) > 0 && "dsmodel" %in% names(maz_all)) {
    paste(unique(maz_all$dsmodel[maz_all$dsmodel %in% dsmodel_pick]), collapse = ", ")
  } else {
    NA_character_
  }
  
  unique_rcps <- if ("rcp" %in% names(maz_all)) unique(maz_all$rcp) else NULL
  unique_periods <- if ("period_code" %in% names(maz_all)) unique(maz_all$period_code) else NULL
  
  shared_caption <- paste0(
    if (!is.null(unique_rcps) && length(unique_rcps) > 0) paste0("RCP ", paste(unique_rcps, collapse = "/"), " • ") else "",
    if (!is.null(unique_periods) && length(unique_periods) > 0) paste0("Period ", paste(unique_periods, collapse = "/"), " • ") else "",
    if (!is.na(dsmodel_lab) && dsmodel_lab != "") paste0("dsmodel: ", dsmodel_lab, " • ") else "",
    "Point = mean across GCMs; Segment = min–max across GCMs",
    if (!use_standardized) " (Y: raw; colour: standardized)" else " (Y & colour: standardized)"
  )
  
  # Assemble with patchwork
  combined_plot <- patchwork::wrap_plots(plot_list, ncol = length(indicator_picks)) +
    patchwork::plot_layout(guides = "collect") &
    theme(legend.position = "right")
  
  combined_plot <- combined_plot +
    patchwork::plot_annotation(
      caption = shared_caption
    )
  
  return(combined_plot)
}

# ==================== 9. Migration Timing Comparison (migration_compare_plot) ====================

migration_compare_plot <- function(migr_daily_all,
                                   timing = NULL,
                                   rcp = "45",
                                   period_choose = c("1981-2010", "2041-2060"),
                                   spatial_path_choose = "CK-12",
                                   min_stream_order = 9) {
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
    dplyr::mutate(CU_label = factor(FULL_CU_IN, levels = unique(FULL_CU_IN)))

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

  # Fallback for species palette
  if (exists("species_palette", envir = .GlobalEnv)) {
    spp_colors <- get("species_palette", envir = .GlobalEnv)
  } else {
    spp_colors <- c("Chinook" = "#E69F00", "Chum" = "#56B4E9", "Coho" = "#009E73", "Pink" = "#F0E442", "Sockeye" = "#D55E00")
  }

  p_upper <- ggplot(df_timing) +
    # Dotted connector from rt_end to sp_start (transition)
    geom_segment(aes(x = rt_end, xend = sp_start, y = CU_label, yend = CU_label, color = SPECIES_NAME), linetype = "dotted", linewidth = 0.6) +
    # Run timing bar (rt_start to rt_end) - narrower segment
    geom_segment(aes(x = rt_start, xend = rt_end, y = CU_label, yend = CU_label, color = SPECIES_NAME), linewidth = 1.5) +
    # Spawning timing bar (sp_start to sp_peak) - narrower segment
    geom_segment(aes(x = sp_start, xend = sp_peak, y = CU_label, yend = CU_label, color = SPECIES_NAME), linewidth = 0.8, alpha = 0.7) +
    scale_color_manual(values = spp_colors) +
    labs(x = NULL, y = "CU", color = "Species") +
    scale_x_continuous(limits = c(1, 365),
                       breaks = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335),
                       labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
    theme_bw() +
    theme(
      axis.text.y = element_text(size = 5),
      panel.grid.major.y = element_blank(),
      legend.position = "right"
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
    theme_bw() +
    theme(
      legend.position = "right"
    ) +
    annotation_custom(grid::textGrob("b", x = unit(0.96, "npc"), y = unit(0.85, "npc"), gp = grid::gpar(fontface = "bold", fontsize = 12)))

  # Retrieve and filter the CK-12 migration path
  if (exists("migr_list", envir = .GlobalEnv)) {
    migr_list_obj <- get("migr_list", envir = .GlobalEnv)
  } else {
    if (exists("paths") && !is.null(paths$fw)) {
      load(file.path(paths$fw, "fw_upstream_paths.Rdata"))
      migr_list_obj <- migr_list
    } else {
      if (file.exists(file.path("processed_data", "freshwater", "fw_upstream_paths.Rdata"))) {
        load(file.path("processed_data", "freshwater", "fw_upstream_paths.Rdata"))
        migr_list_obj <- migr_list
      } else {
        stop("migr_list spatial object not found.")
      }
    }
  }

  if (!spatial_path_choose %in% names(migr_list_obj)) {
    stop(paste("Selected spatial path", spatial_path_choose, "not found in migr_list."))
  }
  migr_path_selected <- migr_list_obj[[spatial_path_choose]]
  migr_path_filtered <- migr_path_selected %>%
    dplyr::filter(stream_order >= min_stream_order)

  # Load fw_models to get August temperature
  if (exists("fw_models", envir = .GlobalEnv)) {
    fw_models_obj <- get("fw_models", envir = .GlobalEnv)
  } else {
    if (exists("paths") && !is.null(paths$fw) && file.exists(file.path(paths$fw, "fw_models_tscapes.Rds"))) {
      load(file.path(paths$fw, "fw_models_tscapes.Rds"))
      fw_models_obj <- fw_models
    } else {
      if (file.exists(file.path("processed_data", "freshwater", "fw_models_tscapes.Rds"))) {
        load(file.path("processed_data", "freshwater", "fw_models_tscapes.Rds"))
        fw_models_obj <- fw_models
      } else {
        fw_models_obj <- NULL
      }
    }
  }

  if (!is.null(fw_models_obj)) {
    proj_period <- setdiff(period_choose, c("1981-2010", "1981-2000"))[1]
    if (is.na(proj_period)) {
      proj_period <- tail(period_choose, 1)
    }
    
    p_code <- period_lookup %>%
      dplyr::filter(period == proj_period, dsmodel == "tscapes") %>%
      dplyr::pull(period_code) %>%
      unique() %>%
      head(1)
    
    if (length(p_code) == 0) {
      p_code <- "3"
    }
    
    temp_col <- paste0("tw8_9_", rcp, "_", p_code)
    temp_col_alt <- paste0("tw8proj_9_", rcp, "_", p_code)
    matching_col <- intersect(c(temp_col, temp_col_alt), names(fw_models_obj))[1]
    
    if (!is.na(matching_col)) {
      join_col <- intersect(c("linear_feature_id", "segmented_stream_id"), names(migr_path_filtered))
      join_col <- intersect(join_col, names(fw_models_obj))
      
      if (length(join_col) > 0) {
        temp_df <- sf::st_drop_geometry(fw_models_obj) %>%
          dplyr::select(dplyr::all_of(c(join_col[1], matching_col)))
        
        migr_path_filtered <- migr_path_filtered %>%
          dplyr::left_join(temp_df, by = join_col[1]) %>%
          dplyr::rename(august_temp = !!matching_col)
      }
    }
  }

  # Get projection CRS from the migration path
  target_crs <- sf::st_crs(migr_path_filtered)

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

  # Crop bounding box with margin
  bbox <- sf::st_bbox(migr_path_filtered)
  x_range <- bbox["xmax"] - bbox["xmin"]
  y_range <- bbox["ymax"] - bbox["ymin"]
  margin_factor <- 0.08
  xlims <- c(bbox["xmin"] - margin_factor * x_range, bbox["xmax"] + margin_factor * x_range)
  ylims <- c(bbox["ymin"] - margin_factor * y_range, bbox["ymax"] + margin_factor * y_range)

  # Map path prefix to species color
  path_prefix <- toupper(substr(spatial_path_choose, 1, 2))
  species_name <- switch(path_prefix,
    "CK" = "Chinook",
    "CO" = "Coho",
    "SE" = "Sockeye",
    "CM" = "Chum",
    "PK" = "Pink",
    "Chinook" # Default fallback
  )
  path_color <- if (species_name %in% names(spp_colors)) spp_colors[species_name] else "#E69F00"

  p_map <- ggplot() +
    geom_sf(data = bc_coast_proj, fill = "grey90", color = "grey75", linewidth = 0.3)
  
  if (!is.null(Fr_basin_proj)) {
    p_map <- p_map + geom_sf(data = Fr_basin_proj, fill = "antiquewhite", color = "grey60", linewidth = 0.4)
  }
  
  if (!is.null(lakes_proj)) {
    p_map <- p_map + geom_sf(data = lakes_proj, fill = "aliceblue", color = "aliceblue", linewidth = 0.1)
  }

  p_map <- p_map +
    geom_sf(data = migr_path_filtered, color = path_color, linewidth = 1.2)

  p_map <- p_map +
    coord_sf(xlim = xlims, ylim = ylims, expand = FALSE) +
    theme_void() +
    theme(
      plot.subtitle = element_text(size = 9, face = "bold", hjust = 0.5),
      plot.margin = margin(5, 5, 5, 5),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8)
    ) +
    annotation_custom(grid::textGrob("c", x = unit(0.94, "npc"), y = unit(0.94, "npc"), gp = grid::gpar(fontface = "bold", fontsize = 12)))

  p_left <- patchwork::wrap_plots(p_upper, p_lower, ncol = 1, heights = c(2.5, 1))
  p <- patchwork::wrap_plots(p_left, p_map, ncol = 2, widths = c(2, 1.2)) +
    patchwork::plot_layout(guides = "collect") &
    theme(
      plot.margin = margin(t = 2, r = 2, b = 2, l = 2, unit = "pt"),
      legend.box.spacing = unit(4, "pt"),
      legend.margin = margin(0, 0, 0, 0, "pt")
    )

  return(p)
}

# 
# plot_maz_lollipop(maz_all)
# 






