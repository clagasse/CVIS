################################################################################
#
# 5a_plots_CU.R
#
#  Functions for plotting and mapping data within a single conservation unit
#  Focus on freshwater stream indicators

### Plots:
# 1. cu_timing_plot() CU life stage timing and data quality plot
# 2. stream_accessible_plot() Stream network plot of accessible streams and NUSEDS site within CU boundary
# 3. stream_indicator_plot() Stream network plot of indicator values within a CU boundary
# 4. migration_path_plot() Migration path plot from river mouth to NUSEDS sites
# 5. Migration indicator values plot
# 6. Nearshore marine indicator plot


###############################################################################



# 1. CU timing plot -------------------------------------------------------
# Improved CU timing plot function - Version 2
# Shows life stage timing with indicator calculation periods

# Improved CU timing plot function - Version 2
# Shows life stage timing with indicator calculation periods

cu_timing_plot <- function(data, show_indicator_periods = FALSE) {
  # Extract CU name and ocean entry age for labels
  cu_name <- unique(data$FULL_CU_IN)[1]
  oe_age <- unique(data$oe_age)[1]

  # Create cleaner life stage labels
  data <- data %>%
    mutate(
      life_stage_label = case_when(
        life_stage == "spawning" ~ "Spawning",
        life_stage == "run_timing" ~ "Upstream Run Timing",
        life_stage == "ocean_entry" ~ "Ocean Entry",
        life_stage == "freshwater_migration" ~ "Juvenile FW Migration",
        TRUE ~ life_stage
      ),
      life_stage_label = factor(life_stage_label,
        levels = c("Spawning", "Upstream Run Timing",
          "Ocean Entry", "Juvenile FW Migration"))
    )

  # Calculate freshwater residency period
  spawn_data <- data %>% filter(life_stage == "spawning")
  ocean_data <- data %>% filter(life_stage == "ocean_entry")

  # Calculate FW residency accounting for ocean entry age
  fw_residency_days <- NA
  if (nrow(spawn_data) > 0 && nrow(ocean_data) > 0 && !is.na(oe_age)) {
    # Calculate base residency (spawn peak to ocean entry peak)
    fw_residency_days <- ocean_data$peak[1] - spawn_data$peak[1]
    if (fw_residency_days < 0) fw_residency_days <- fw_residency_days + 365

    # Add 365 days for each year of ocean entry age = 1 or greater
    if (oe_age >= 1) {
      fw_residency_days <- fw_residency_days + (floor(oe_age) * 365)
    }
  }

  # Define colors for life stages (colorblind-friendly palette)
  stage_colors <- c(
    "Spawning" = "#66C2A5",
    "Upstream Run Timing" = "#FC8D62",
    "Ocean Entry" = "#8DA0CB",
    "Juvenile FW Migration" = "#E78AC3"
  )

  # Create base plot
  p <- ggplot(data, aes(x = life_stage_label, xend = life_stage_label,
    y = as.Date("2000-01-01") + start,
    yend = as.Date("2000-01-01") + end)) +
    # Life stage duration bars (5th-95th percentile)
    geom_segment(aes(color = life_stage_label),
      linewidth = 8, alpha = 1) +
    # Peak timing points
    geom_point(aes(y = as.Date("2000-01-01") + peak,
      size = dat_qual, fill = life_stage_label),
    shape = 21, color = "gray20", stroke = 0.8) +
    scale_color_manual(values = stage_colors, guide = "none") +
    scale_fill_manual(values = stage_colors, guide = "none") +

    scale_size_area(
      name   = "Data Quality",
      max_size = 3,            # overall max point radius; adjust to taste
      trans  = "reverse",
      breaks = c(1, 2, 3, 4, 5),
      limits = c(6, 1)
    ) +

    scale_y_date(date_breaks = "1 month",
      date_labels = "%b",
      limits = c(as.Date("2000-01-01"), as.Date("2000-12-31")),
      expand = c(0.02, 0)) +
    coord_flip() +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.spacing = unit(0.1, "lines"),
      axis.title.y = element_blank(),
      axis.text.y = element_text(size = 10, margin = margin(r = 2)),
      plot.title = element_text(face = "bold", size = 12, margin = margin(b = 2)),
      plot.subtitle = element_text(size = 10, color = "grey40", margin = margin(b = 3)),
      plot.caption = element_text(size = 8, color = "grey50", hjust = 0, margin = margin(t = 2)),
      legend.position = "bottom",
      legend.margin = margin(t = 2),
      legend.box.spacing = unit(0.1, "lines"),
      plot.margin = margin(3, 5, 3, 3),
      aspect.ratio = 0.4
    )

  # Add indicator period highlights if requested
  if (show_indicator_periods) {
    # Define indicator periods with different colors
    indicator_rects <- data.frame(
      period_name = character(),
      ymin = as.Date(character()),
      ymax = as.Date(character()),
      xmin = numeric(),
      xmax = numeric(),
      color_fill = character(),
      stringsAsFactors = FALSE
    )

    # August period (for temperature and flow indicators) - Light orange
    august_start <- as.Date("2000-08-01")
    august_end <- as.Date("2000-08-31")
    indicator_rects <- rbind(indicator_rects, data.frame(
      period_name = "Stream T/Flow \n\n",
      ymin = august_start,
      ymax = august_end,
      xmin = 0.5,
      xmax = 4.1,
      color_fill = "#FFE0B2"  # Light orange
    ))

    # Peak ocean entry period (for marine SST) - Light blue
    if (nrow(ocean_data) > 0) {
      # Use ±1 month around peak ocean entry
      oe_peak_date <- as.Date("2000-01-01") + ocean_data$peak[1]
      ns_start_plot <- as.Date("2000-01-01") + (ocean_data$ns_start_month * 30) - 15
      ns_end_plot <- as.Date("2000-01-01") + (ocean_data$ns_end_month * 30) - 15

      indicator_rects <- rbind(indicator_rects, data.frame(
        period_name = "Nearshore marine (SST)",
        ymin = ns_start_plot,
        ymax = ns_end_plot,
        xmin = 0.5,
        xmax = 4.1,
        color_fill = "#B3E5FC"  # Light blue
      ))
    }

    # Run timing to spawning period (for migration indicators) - Light green
    run_data <- data %>% filter(life_stage == "run_timing")
    if (nrow(run_data) > 0 && nrow(spawn_data) > 0) {
      migr_start <- as.Date("2000-01-01") + run_data$start[1]
      migr_end <- as.Date("2000-01-01") + spawn_data$peak[1]

      indicator_rects <- rbind(indicator_rects, data.frame(
        period_name = "Upstream Migration",
        ymin = migr_start,
        ymax = migr_end,
        xmin = 0.5,
        xmax = 4.1,
        color_fill = "#C8E6C9"  # Light green
      ))
    }

    # Add shaded rectangles for indicator periods with different colors
    p <- p +
      geom_rect(data = indicator_rects,
        aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
          fill = I(color_fill)),
        alpha = 0.4, inherit.aes = FALSE) +
      geom_text(data = indicator_rects,
        aes(x = xmax + 0.1, y = ymin + (ymax - ymin) / 2,
          label = period_name),
        hjust = 0, size = 2.8, color = "grey20",
        lineheight = 0.85, inherit.aes = FALSE)
  }

  # Add title and subtitle with FW residency
  subtitle_text <- sprintf("Ocean Entry Age: %s", oe_age)
  if (!is.na(fw_residency_days)) {
    subtitle_text <- sprintf("Ocean Entry Age: %s  |  Freshwater Residency: ~%d days",
      oe_age, round(fw_residency_days))
  }

  p <- p +
    labs(
      title = sprintf("Life Stage Timing: %s", cu_name),
      subtitle = subtitle_text,
      x = NULL,
      y = "Date",
      caption = "Bars = 5th-95th percentile range | Points = peak timing (size = data quality: 1=best, 6=worst)\nShaded areas = periods used for indicator calculations"
    )

  return(p)

}

# # # Load your timing data
# cu_timing_long_i <- cu_timing_long %>% filter(FULL_CU_IN == "CK-11")
# # # Create plot
# p <- cu_timing_plot(cu_timing_long_i, show_indicator_periods = F)
# print(p)



# 2. Stream network accessible stream plot -----------------------------------
# input subset of stream network, nuseds data, and CU boundary for a specific CU
stream_accessible_plot <- function(stream_data,
                                   nuseds_data,
                                   cu_boundary,
                                   lakes_cu) {
  p1 <- ggplot() +
    geom_sf(data = cu_boundary_i, color = "black", alpha = 0.3)

  if (nrow(lakes_cu) > 0) p1 <- p1 + geom_sf(data = lakes_cu, color = "darkblue", alpha = 0.7)

  p1 <- p1 +
    geom_sf(data = nuseds_cu, aes(fill = SPECIES), size = 2, alpha = 0.6) +
    geom_sf(data = st_zm(stream_data), aes(color = model_rs)) +
    coord_sf(xlim = st_bbox(cu_boundary_i)[c(1, 3)],
      ylim = st_bbox(cu_boundary_i)[c(2, 4)]) +
    labs(colour = "BC FishPass",
      fill = "NUSEDS sites")

  return(p1)
}


# 3. fw cu boundary/stream plots --------------

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
                                  scico_palette = "roma",
                                  palette_direction = 1) {
  var_sym <- sym(variable)
  hist_sym <- sym(histogram_fill)

  color_range <- range(fwModels[[as.character(var_sym)]], na.rm = TRUE)

  ## stream map
  p1 <- ggplot() +
    geom_sf(data = fwModels, aes(color = !!var_sym), linewidth = 1.) +
    scale_color_scico(palette = scico_palette,
      direction = palette_direction,
      limits = color_range) +
    geom_sf(data = cu_boundary, color = "black", alpha = 0.3) +
    coord_sf(xlim = st_bbox(cu_boundary)[c(1, 3)],
      ylim = st_bbox(cu_boundary)[c(2, 4)],
      datum = NA) +    # this eliminates axis labels
    labs(subtitle = plot_title,
      color = unit_label)

  if (nrow(lakes_cu) > 0) p1 <- p1 + geom_sf(data = lakes_cu, color = "darkblue", alpha = 0.7)

  if (sum(!is.na(xlim)) > 0) {
    p1 <- p1 + scale_color_scico(palette = scico_palette, direction = palette_direction, limits = xlim)
  }

  if (temp_stations == TRUE) {
    p1 <- p1 +
      geom_sf(data = Tw_stations, aes(shape = "Temperature Gauge"),
        size = 0.6, show.legend = TRUE) +
      coord_sf(xlim = st_bbox(cu_boundary)[c(1, 3)],
        ylim = st_bbox(cu_boundary)[c(2, 4)],
        datum = NA) +
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
    h1 <- h1 +    labs(fill = "Habitat Potential (BC Fishpass)") +
      scale_fill_manual(values = c("2-ACCESSIBLE" = "darkgrey",
        "1-SPAWNING/REARING" = "forestgreen"))
  }


  # Extract max y from built plot
  y_pos <- max(ggplot_build(h1)$data[[1]]$count, na.rm = TRUE) * 1.05
  # add mean value line and label
  h1 <- h1 +
    geom_vline(aes(xintercept = mean(!!var_sym, na.rm = TRUE)),
      color = "red", linetype = "dashed") +
    annotate("text",
      x = mean(fwModels[[variable]], na.rm = TRUE),
      y = y_pos,
      label = "mean") +
    theme_void() +
    theme(
      axis.line.x = element_line(color = "black"),
      axis.text.x = element_text(color = "black"),
      axis.ticks.x = element_line(color = "black"),
      axis.title.x = element_text(color = "black")
    )

  if (sum(!is.na(xlim)) > 0) {
    h1 <- h1 + xlim(xlim)
  }

  p1 / h1 + plot_layout(heights = c(4, 1))
}


# 4. migration path plot -------------------

migration_path_plot <- function(migr_path,
                                nuseds_data,
                                cu_boundary,
                                plot_title = "",
                                colour_var = "mad_m3s",
                                colour_label = "Mean Annual Discharge (m3s)") {

  p <- ggplot() +
    annotation_map_tile(type = "cartolight") +
    geom_sf(data = cu_boundary,
      fill = "grey",
      alpha = 0.1) +
    geom_sf(data = migr_path, aes(colour = !!sym(colour_var)), linewidth = 2) +
    scale_color_scico(palette = "batlow") +
    geom_sf(data = nuseds_data, aes(fill = SPECIES_LOOKUP),
      color = "black",      # outline color
      size = 3,             # increase point size
      shape = 21)  +
    labs(color = colour_label, fill = "NuSEDS sites",
      subtitle = plot_title)

  return(p)

}


# 5. migration timing plot ---------------------------------------------

migr_timing_plot <- function(migrT,
                             cu_i,
                             timing,
                             rcp = "45",
                             period_choose = c("1981-2010", "2041-2060")) {


  migrT_select <- migrT[[rcp]][[cu_i]][["doy"]]

  # Names of the components you want to process
  components <- c("mean", "0.1", "0.9")

  # Convert each matrix to a long-format data frame with DOY
  long_list <- components %>%
    set_names() %>%
    map(~ migrT_select[[.x]] %>%
      as.data.frame() %>%
      mutate(doy = as.numeric(rownames(.))) %>%
      pivot_longer(-doy, names_to = "period", values_to = .x))

  # Combine all into one tidy data frame
  df_all <- reduce(long_list, left_join, by = c("doy", "period")) %>%
    drop_na() %>%
    mutate(period = factor(period, levels = rev(sort(unique(period))))) %>%
    filter(period %in% period_choose)

  overall_mean <- df_all %>%
    summarise(mean_temp = mean(mean, na.rm = TRUE)) %>%
    pull(mean_temp)

  p <- ggplot(df_all, aes(x = doy, y = mean, color = period)) +
    geom_ribbon(aes(ymin = `0.1`, ymax = `0.9`, group = period), alpha = 0.2, fill = "grey70") +
    geom_path(alpha = 0.6, position = "identity") +
    # Add vertical lines for rt_start, rt_end, sp_start
    geom_vline(xintercept = timing$rt_start, linetype = "dashed", color = "blue", linewidth = 0.8) +
    geom_vline(xintercept = timing$rt_end,   linetype = "dashed", color = "blue", linewidth = 0.8) +
    geom_vline(xintercept = timing$sp_start, linetype = "dashed", color = "red",  linewidth = 0.8) +
    geom_vline(xintercept = timing$sp_peak, linetype = "dashed", color = "red",  linewidth = 0.8) +

    # Add text annotations
    annotate("text", x = timing$rt_start, y = Inf, label = "RT Start", vjust = 2, color = "blue") +
    annotate("text", x = timing$rt_end,   y = Inf, label = "RT End",   vjust = 2, color = "blue") +
    annotate("text", x = timing$sp_start, y = Inf, label = "SP Start", vjust = 2, color = "red") +
    annotate("text", x = timing$sp_peak, y = Inf, label = "SP Peak", vjust = 2, color = "red") +

    # Add overall mean line
    # geom_hline(yintercept = overall_mean, linetype = "dotdash", color = "black", linewidth = 0.8) +
    # annotate("text", x = max(df_all$doy), y = overall_mean, label = paste0("Overall Mean: ", round(overall_mean, 2), "°C"),
    #   hjust = 1.1, vjust = -0.5, color = "black", size = 3.5) +

    scale_fill_brewer(palette = "Spectral") +
    labs(title = "Temperature Trends with 10–90% Quantile Bounds",
      x = "Day of Year (DOY)",
      y = "Temperature (°C)",
      fill = "Time Period")

  return(p)

}



# 6. migration indicator map ----------------------------------------------







# 7. CU Boundary location plot --------------------------------------------

# simple plot showing location of the CU boundary outline compared to all CU boundaries

cu_boundary_highlight <- function(cu_boundary,
                                  cu_pick) {

  cu_boundary_i <- filter(cu_boundary, FULL_CU_IN == cu_pick)

  p <- ggplot() +
    #annotation_map_tile(type = "cartolight") +
    geom_sf(data = cu_boundary, color = "black", alpha = 0.3) +
    geom_sf(data = cu_boundary_i, fill = "green") 
    #labs(title = cu_pick)
  
  return(p)

}


# 8. Hydrologic Regime ----------------------------------------------------
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
    coord_sf(xlim = st_bbox(cu_boundary_i)[c(1, 3)],
      ylim = st_bbox(cu_boundary_i)[c(2, 4)]) +
    labs(fill = "Hydrologic regime",
      color = "Flow Gauge")

}



# 9. CU lollipop plot of indicator values -------------------------------------------------


# ind_cu <- get_CU_indicators(all_flat_std,
#   cu_i = "CK-10",
#   RCP_pick = "45",
#   period_pick = "3",
#   indicators_choose = tbl_indicators$abbrev)

plot_cu_lolli <- function(data,   # need indicator data for a single CU, use get_cu_indicators()
                          # indicators_choose = c("migrT", "migrQ", "migrA21", "migrdist"),
                          indicators_choose = c("CUstatus", "CUnmat"),
                          use_standardized = TRUE,  # use raw or transformed (standardized values)
                          plot_colours = species_palette) {

  colors <- c(
    "CU" = "blue",
    "Species Mean" = "black",
    "Above Species Mean" = "darkred",
    "Below Species Mean" = "forestgreen",
    "GCM Variation" = "gray60"
  )

  data <- data %>%
    filter(indicator %in% indicators_choose) %>%
    mutate(
      above_sp = case_when(
        is.na(sp_value) ~ NA,
        cu_value > sp_value ~ "Above Species Mean",
        TRUE ~ "Below Species Mean"
      )
    )

  # check if gcm variation values exist
  gcm_check <- sum(data$stat == "qlowgcm")


  data_wide <- data %>%
    pivot_wider(id_cols = indicator,
      names_from = stat,
      values_from = c(cu_value, sp_value)
    ) %>%
    mutate(
      above_sp = case_when(
        is.na(sp_value_mean) ~ NA,
        cu_value_mean > sp_value_mean ~ "Above Species Mean",
        TRUE ~ "Below Species Mean"
      )
    )

  p <- ggplot(data_wide, aes(x = indicator))
  if (gcm_check >= 1) {   # add gcm variation if data exists
    # GCM variation segment
    p <- p + geom_segment(aes(xend = indicator, y = cu_value_qlowgcm, yend = cu_value_qhighgcm,
      color = "GCM Variation"), size = 2)
  }
  # CU vs Species Mean segment
  p <- p + geom_segment(aes(xend = indicator,
    y = cu_value_mean, yend = sp_value_mean,
    color = above_sp),
  size = 3, alpha = 0.5) +
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




# 10. Marine indicators map -----------------------------------------------

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
                                  scico_palette = "roma",
                                  plot_title = "",
                                  palette_direction = -1,
                                  palette_limits = c(9, 14)) {

  p <- ggplot() +
    geom_sf(data = data, aes(colour = !!sym(var))) +
    scico::scale_color_scico(
      palette   = scico_palette,
      direction = palette_direction,
      limits    = palette_limits         # <-- set your min/max here
    ) +
    geom_sf(data = MAZ, fill = NA, color = "black") +
    coord_sf(xlim = st_bbox(data)[c(1, 3)],
      ylim = st_bbox(data)[c(2, 4)]) +
    labs(color = unit_label,
      title = plot_title)

  return(p)

}


# 11. CU Abundance and Status ---------------------------------------------

abundance_status_plot <- function(status_data,
                                  cu_i) {

  status_palette <- c(
    "Red" = "firebrick",
    "Amber" = "orange",
    "Green" = "green2",
    "None"  = "grey40"
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
  if(is.numeric(latest_abundance)) scales::comma(latest_abundance)
  latest_genabd   <- ifelse(is.na(latest_entry$GenAvgUsed), "NA", round(latest_entry$GenAvgUsed))
  if(is.numeric(latest_genabd)) scales::comma(latest_genabd)
  status_color <- status_palette[latest_status]

  if (latest_entry$DataType == "Abs_Abd" && !is.na(latest_entry$DataType)) data_type <- "Absolute Abundance"
  if (latest_entry$DataType == "Rel_Idx" && !is.na(latest_entry$DataType)) data_type <- "Relative Index"
  if (is.na(latest_entry$DataType)) data_type <- "NA"

  # Create a one-row data frame for annotation
  annotation_df <- data.frame(
    x = max(plot_data$Year),
    y = max(plot_data$SpawnerAbundance, na.rm = TRUE) * 0.96,  # slightly below top
    label = paste0(
      plot_data$CVIS_NAME, "<br>",
      "Data Type: ", data_type,  "</span><br>",
      "Most Recent Status: <span style='color:", status_color, "'>", latest_status, "</span><br>",
      "Recent Spawner Abundance: ", latest_abundance, "</span><br>",
      "Recent Generational Avg: ", latest_genabd
    )
  )

  legend_lines <- data.frame(
    Year = c(2000, 2000),  # any values, won't be plotted
    Abundance = c(0, 0),
    LineType = c("Geometric Avg", "Annual Abundance")
  )

  p <-   ggplot(data = plot_data) +
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


# 12. CU all indicators plot --------------------------------------------------

# Function to create a comprehensive lollipop chart showing all indicators for one CU
# This is the OPPOSITE of plot_lollipop which shows one indicator across all CUs

plot_cu_indicators_lollipop <- function(data,
                                        indicators_choose = NULL,  # NULL = all indicators
                                        group_by_category = TRUE,  # Group indicators by type
                                        show_species_avg = TRUE,   # Show species average comparison
                                        show_all_cu_avg = TRUE,    # Show all CU average comparison
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
    left_join(select(tbl_indicators, abbrev, name, type),
      by = c("indicator" = "abbrev")) %>%
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
        type == "fwR" ~ "Spawning & Rearing",
        type == "migr" ~ "Upstream Migration",
        type == "dem" ~ "Demographics",
        type == "mar" ~ "Nearshore Marine",
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
      arrange(type, indicator) %>%
      mutate(name = factor(name, levels = unique(name)))
  } else {
    plot_data <- plot_data %>%
      arrange(desc(cu_value)) %>%
      mutate(name = factor(name, levels = unique(name)))
  }

  # Create the plot
  p <- ggplot(plot_data, aes(x = name, y = cu_value))

  # Add GCM variation bars if requested and available
  if (show_gcm_variation && "gcm_qlowgcm" %in% names(plot_data)) {
    p <- p + geom_segment(
      aes(xend = name, y = gcm_qlowgcm, yend = gcm_qhighgcm),
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
        na.rm = TRUE)
  }

  # Add all CU average if requested
  if (show_all_cu_avg) {
    p <- p + geom_point(aes(y = allcu_value, shape = "All Species Average"),
      fill = "gold",
      color = "black",
      size = 3,
      na.rm = TRUE)
  }

  # Add main points with continuous color scale (RdYlGn reversed so red = high risk)
  p <- p + geom_point(aes(fill = cu_value, shape = "CU Value"),
    color = "black",
    size = 4,
    stroke = 1) +
    scale_fill_distiller(
      palette = "RdYlGn",
      direction = -1,  # Reversed: red for high values (high risk)
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




# 13. MAZ boundary location plot ------------------------------------------

MAZ_boundary_highlight <- function(MAZ,
                                   MAZ_pick) {

  MAZ_i <-  filter(MAZ, MAZ_Acrony == MAZ_pick)

  p <- ggplot() +
    #annotation_map_tile(type = "cartolight") +
    geom_sf(data = MAZ, color = "black", alpha = 0.3) +
    geom_sf(data = MAZ_i, fill = "green")

  return(p)

}


# X. Testing plot functions -----------------------------------------------
# 
# cu_i <- "CK-03"
# 
# cu_i <- cu_run$FULL_CU_IN[i]
# cu_run_i <- cu_run[cu_run$FULL_CU_IN == cu_i, ]
# sp_pick <- cu_run$spp[cu_run$FULL_CU_IN == cu_i] # species abbr
# sp_pick_bcfp <- spp_lookup$spp_abr_bcfp[spp_lookup$spp_abr == sp_pick]
# 
# cu_timing_i <- cu_timing_Fr %>% filter(FULL_CU_IN == cu_i)
# 
# cu_boundary_i <- cu_boundary[cu_boundary$FULL_CU_IN == cu_i, ]
# 
# # # subset nuseds observations
# # nuseds_cu <- nuseds_Fr[nuseds_Fr$FULL_CU_IN == cu_i, ]
# #
# # subset migration path
# migr_cu <- migr_list[[cu_i]]
# 
# # stream_cu_sub <- stream_cu_picks[, colnames(stream_cu_picks) == cu_i]
# 
# # fwModels_cu <- fwModels[stream_cu_sub, ] %>%
# #   rename(keep_model_spawning = contains(paste0("model_spawning_", sp_pick_bcfp)),
# #     keep_model_rearing  = contains(paste0("model_rearing_", sp_pick_bcfp))) %>%
# #   mutate(model_rs = if_any(starts_with("keep_model"), ~ . == TRUE)) %>% # get boolean for model spawning and rearing
# #   mutate(model_rs = factor(model_rs, levels = c(TRUE, FALSE),
# #     labels = c("1-SPAWNING/REARING", "2-ACCESSIBLE"))) %>%
# #   select(-starts_with(c("model_spawning", "model_rearing", "known_rearing", "known_spawning")))
# #
# #
# # cu_timing_long_i <- cu_timing_long[cu_timing_long$FULL_CU_IN == cu_i, ]
# # cu_timing_i <- cu_timing_Fr[cu_timing_Fr$FULL_CU_IN == cu_i, ]
# #
# #
# # stream_accessible_plot(fwModels_cu,
# #   nuseds_cu,
# #   cu_boundary_i)
# #
# # stream_indicator_plot(fwModels_cu,
# #   cu_boundary_i,
# #   variable = "Tw8_9_45_3",
# #   unit_label = "Degrees C",
# #   plot_title = "August temperature 2041-2060",
# #   scico_palette = "roma",
# #   palette_direction = -1)
# #
# 
# # migr_UFR <- filter(migr_cu, watershed_group_code == "UFRA")
# # 
# # migration_path_plot(migr_cu,
# #   nuseds_cu,
# #   cu_boundary_i,
# #   colour_var = "downstream_distance",
# # )
# # # #
# # cu_timing_plot(cu_timing_long_i)
# #
# migr_timing_plot(migrT_rcps, cu_i, cu_timing_i)
# 
# migrT_cu <- migrT_rcps[["45"]][[cu_i]]
