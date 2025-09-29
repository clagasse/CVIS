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


#
###############################################################################

library(ggspatial)  # base map tiles

# 1. CU timing plot -------------------------------------------------------

cu_timing_plot <- function(data) {

  data <-  data %>%
    mutate(data, life_stage = factor(life_stage,
      levels = c("spawning", "run_timing",
        "ocean_entry",
        "freshwater_migration")))

  p <- ggplot(data, aes(x = life_stage, xend = life_stage,
    y = as.Date("2000-01-01") + start,
    yend = as.Date("2000-01-01") + end), color = "grey") +
    geom_segment(aes(y = as.Date("2000-01-01") + start, yend = as.Date("2000-01-01") + end, color = life_stage),
      size = 6, alpha = 0.6) +
    geom_point(aes(y = as.Date("2000-01-01") + peak, size = dat_qual, fill = life_stage),
      shape = 21, color = "gray30") +
    scale_color_brewer("Stage", palette = "Set2") +
    scale_fill_brewer("",   palette = "Set2") +
    scale_size_continuous(name = "Data Quality", limits = c(1, 6)) +
    # geom_segment(color="grey", linewidth = 4) +
    # geom_point( aes(y=as.Date("2000-01-01") + start, color="95th perc"), size=3 ) +
    # geom_point( aes(y=as.Date("2000-01-01") + end, color="95th perc"), size=3 ) +
    # geom_point( aes(x=life_stage, y=as.Date("2000-01-01") + peak, color="peak"), size=3 ) +
    # geom_text( aes(x=life_stage, y=as.Date("2000-01-01") + 350, label = paste("Data \n Quality:", dat_qual)), size=3) +
    scale_y_date(date_breaks = "1 month", date_labels = "%b", limits =   c(as.Date("2000-01-01"), as.Date("2000-12-31"))) +
    coord_flip() +
    xlab("Life Stage") +
    ylab("Date") +
    labs(title = paste("Life-stage timing for ", data$FULL_CU_IN),
      subtitle = paste("Ocean Entry Age: ", data$oe_age),
      size = "Data Quality") +
    guides(
      color = "none",
      fill  = "none"
    )

  return(p)

}



# 2. Stream network accessible stream plot -----------------------------------
# input subset of stream network, nuseds data, and CU boundary for a specific CU
stream_accessible_plot <- function(stream_data,
                                   nuseds_data,
                                   cu_boundary) {
  p <- ggplot() +
    geom_sf(data = st_zm(stream_data), aes(color = model_rs)) +
    geom_sf(data = cu_boundary_i, color = "black", alpha = 0.3) +
    geom_sf(data = nuseds_cu, aes(fill = SPECIES), alpha = 0.6) +
    coord_sf(xlim = st_bbox(cu_boundary_i)[c(1, 3)],
      ylim = st_bbox(cu_boundary_i)[c(2, 4)]) +
    labs(colour = "BC FishPass",
      fill = "NUSEDS sites")

  return(p)
}


# 3. fw cu boundary/stream plots --------------

stream_indicator_plot <- function(fwModels,
                                  cu_boundary,
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
                                cu_boundary) {

  p <- ggplot() +
    annotation_map_tile(type = "cartolight") +
    geom_sf(data = cu_boundary,
      fill = "grey",
      alpha = 0.1) +
    geom_sf(data = migr_path, aes(colour = downstream_distance), linewidth = 2) +
    scale_color_scico(palette = "batlow") +
    geom_sf(data = nuseds_data, aes(fill = SPECIES)) +
    labs(color = "Distance (m)", fill = "NUSEDS sites")

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
    annotation_map_tile(type = "cartolight") +
    geom_sf(data = cu_boundary, color = "black", alpha = 0.3) +
    geom_sf(data = cu_boundary_i, fill = "green") +
    labs(title = cu_pick)

}



# 8. Hydrologic Regime ----------------------------------------------------
cu_hydrologic_regime <- function(cu_boundary_i,
                                 watershed_flow_cu,
                                 stations_cu,
                                 fwModels_cu) {

  p <- ggplot() +
    geom_sf(data = cu_boundary_i, color = "black", fill = "grey") +
    geom_sf(data = watershed_flow_cu, aes(fill = regime), alpha = 0.3) +
    geom_sf(data = stations_cu, colour = "darkred", size = 2) +
    # geom_sf(data = st_zm(fwModels_cu), alpha = 0.4) +
    coord_sf(xlim = st_bbox(cu_boundary_i)[c(1, 3)],
      ylim = st_bbox(cu_boundary_i)[c(2, 4)]) +
    labs(fill = "Hydrologic regime",
      color = "Flow gauge")

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

marine_indicator_plot <- function(SST_cu_sp,
                                  MAZ,
                                  scico_palette = "roma",
                                  unit_label = "Degrees C",
                                  plot_title = "",
                                  palette_direction = -1) {

  p <- ggplot() +
    geom_sf(data = SST_cu_sp, aes(fill = mean_sst)) +
    scale_fill_scico(palette = scico_palette, direction = palette_direction) +
    geom_sf(data = MAZ, fill = NA, color = "black") +
    coord_sf(xlim = st_bbox(MAZ)[c(1, 3)],
      ylim = st_bbox(MAZ)[c(2, 4)]) +
    labs(fill = unit_label,
      title = plot_title)


}



# X. Testing plot functions -----------------------------------------------

#
# cu_i <- cu_run$FULL_CU_IN[i]
# cu_run_i <- cu_run[cu_run$FULL_CU_IN == cu_i, ]
# sp_pick <- cu_run$spp[cu_run$FULL_CU_IN == cu_i] # species abbr
# sp_pick_bcfp <- spp_lookup$spp_abr_bcfp[spp_lookup$spp_abr == sp_pick]
#
# cu_boundary_i <- cu_boundary[cu_boundary$FULL_CU_IN == cu_i, ]
#
# # subset nuseds observations
# nuseds_cu <- nuseds_Fr[nuseds_Fr$FULL_CU_IN == cu_i, ]
#
# # subset migration path
# migr_cu <- migr_list[[cu_i]]
#
# stream_cu_sub <- stream_cu_picks[, colnames(stream_cu_picks) == cu_i]
#
# fwModels_cu <- fwModels[stream_cu_sub, ] %>%
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
# stream_accessible_plot(fwModels_cu,
#   nuseds_cu,
#   cu_boundary_i)
#
# stream_indicator_plot(fwModels_cu,
#   cu_boundary_i,
#   variable = "Tw8_9_45_3",
#   unit_label = "Degrees C",
#   plot_title = "August temperature 2041-2060",
#   scico_palette = "roma",
#   palette_direction = -1)
#
# migration_path_plot(migr_cu,
#   nuseds_cu,
#   cu_boundary_i
# )
#
# cu_timing_plot(cu_timing_long_i)
#
# migr_timing_plot(migrT_rcps, cu_i, cu_timing_i)
