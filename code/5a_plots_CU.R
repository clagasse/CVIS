################################################################################
#
# 5a_plots_CU.R
#
#  Functions for plotting and mapping data within a single conservation unit
#  Focus on freshwater stream indicators

### Plots:
# 1. Stream network plot of accessible streams and NUSEDS site within CU boundary
# 2. Stream network plot of indicator values within a CU boundary
# 3. Migration path plot from river mouth to NUSEDS sites
# 4. CU life stage timing and data quality plot
# 5. Migration indicator values plot
# 6. Nearshore marine indicator plot


#
###############################################################################


# 1. Stream network accessible stream plot -----------------------------------
# input subset of stream network, nuseds data, and CU boundary for a specific CU
stream_accessible_plot <- function(stream_data,
                                   nuseds_data,
                                   cu_boundary) {
  p <- ggplot() +
    geom_sf(data = st_zm(stream_data), aes(color = model_rs)) +
    geom_sf(data = cu_boundary_i, color = "black", alpha = 0.3) +
    geom_sf(data = nuseds_cu, aes(color = SPECIES), alpha = 0.6) +
    coord_sf(xlim = st_bbox(cu_boundary_i)[c(1, 3)],
      ylim = st_bbox(cu_boundary_i)[c(2, 4)]) +
    labs(subtitle = paste(cu_run_i$CU_NAME), colour = "BC FishPass")

  return(p)
}



#--------- 2. fw cu boundary/stream plots --------------

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

  ## stream map
  p1 <- ggplot() +
    geom_sf(data = fwModels, aes(color = !!var_sym), linewidth = 1.) +
    scale_color_scico(palette = scico_palette, direction = palette_direction) +
    geom_sf(data = cu_boundary, color = "black", alpha = 0.3) +
    coord_sf(xlim = st_bbox(cu_boundary)[c(1, 3)],
      ylim = st_bbox(cu_boundary)[c(2, 4)]) +
    labs(subtitle = plot_title,
      color = unit_label)

  if (sum(!is.na(xlim)) > 0) {
    p1 <- p1 + scale_color_scico(palette = scico_palette, direction = palette_direction, limits = xlim)
  }

  if (temp_stations == TRUE) {
    p1 <- p1 +
      geom_sf(data = Tw_stations, color = "blue") +
      coord_sf(xlim = st_bbox(cu_boundary)[c(1, 3)],
        ylim = st_bbox(cu_boundary)[c(2, 4)])
  }

  ## histogram plotting
  # Set factor levels
  fwModels[[histogram_fill]] <- factor(fwModels[[histogram_fill]], levels = c("2-ACCESSIBLE", "1-SPAWNING/REARING"))

  h1 <- ggplot(fwModels) +
    geom_histogram(aes(x = !!var_sym, fill = !!hist_sym)) +
    labs(fill = "BC Fishpass") +
    scale_fill_manual(values = c("2-ACCESSIBLE" = "darkgrey", "1-SPAWNING/REARING" = "forestgreen"))

  # Extract max y from built plot
  y_pos <- max(ggplot_build(h1)$data[[1]]$count, na.rm = TRUE) * 1.05
  # add mean value line and label
  h1 <- h1 +
    geom_vline(aes(xintercept = mean(!!var_sym, na.rm = TRUE)), color = "red", linetype = "dashed") +
    annotate("text",
      x = mean(fwModels[[variable]], na.rm = TRUE),
      y = y_pos,
      label = "mean")

  if (sum(!is.na(xlim)) > 0) {
    h1 <- h1 + xlim(xlim)
  }

  p1 / h1 + plot_layout(heights = c(3, 1))
}


#-----------------3. migration path plot -------------------

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
    geom_sf(data = nuseds_data, colour = "darkgreen") +
    labs(color = "Distance (m)")

  return(p)

}



# 4. migration indicator plot ---------------------------------------------



# 5. CU timing plot -------------------------------------------------------

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
    # scale_size_continuous("Data quality") +
    # geom_segment(color="grey", linewidth = 4) +
    # geom_point( aes(y=as.Date("2000-01-01") + start, color="95th perc"), size=3 ) +
    # geom_point( aes(y=as.Date("2000-01-01") + end, color="95th perc"), size=3 ) +
    # geom_point( aes(x=life_stage, y=as.Date("2000-01-01") + peak, color="peak"), size=3 ) +
    # geom_text( aes(x=life_stage, y=as.Date("2000-01-01") + 350, label = paste("Data \n Quality:", dat_qual)), size=3) +
    scale_y_date(date_breaks = "1 month", date_labels = "%b", limits =   c(as.Date("2000-01-01"), as.Date("2000-12-31"))) +
    coord_flip() +
    xlab("Life Stage") +
    ylab("Date") +
    labs(title = paste("Life-stage timing for CU", data$FULL_CU_IN),
      subtitle = paste("Ocean Entry Age: ", data$oe_age),
      size = "Data Quality") +
    guides(
      color = "none",
      fill  = "none"
    )

  return(p)

}



# X. Testing plot functions -----------------------------------------------

cu_i <- cu_run$FULL_CU_IN[i]
cu_run_i <- cu_run[cu_run$FULL_CU_IN == cu_i, ]
sp_pick <- cu_run$spp[cu_run$FULL_CU_IN == cu_i] # species abbr
sp_pick_bcfp <- spp_lookup$spp_abr_bcfp[spp_lookup$spp_abr == sp_pick]

cu_boundary_i <- cu_boundary[cu_boundary$FULL_CU_IN == cu_i, ]

# subset nuseds observations
nuseds_cu <- nuseds_Fr[nuseds_Fr$FULL_CU_IN == cu_i, ]

# subset migration path
migr_cu <- migr_list[[cu_i]]

stream_cu_sub <- stream_cu_picks[, colnames(stream_cu_picks) == cu_i]

fwModels_cu <- fwModels[stream_cu_sub, ] %>%
  rename(keep_model_spawning = contains(paste0("model_spawning_", sp_pick_bcfp)),
    keep_model_rearing  = contains(paste0("model_rearing_", sp_pick_bcfp))) %>%
  mutate(model_rs = if_any(starts_with("keep_model"), ~ . == TRUE)) %>% # get boolean for model spawning and rearing
  mutate(model_rs = factor(model_rs, levels = c(TRUE, FALSE),
    labels = c("1-SPAWNING/REARING", "2-ACCESSIBLE"))) %>%
  select(-starts_with(c("model_spawning", "model_rearing", "known_rearing", "known_spawning")))


cu_timing_i <- cu_timing_long[cu_timing_long$FULL_CU_IN == cu_i, ]



stream_accessible_plot(fwModels_cu,
  nuseds_cu,
  cu_boundary_i)

stream_indicator_plot(fwModels_cu,
  cu_boundary_i,
  variable = "Tw8_9_45_3",
  unit_label = "Degrees C",
  plot_title = "August temperature 2041-2060",
  scico_palette = "roma",
  palette_direction = -1)

migration_path_plot(migr_cu,
  nuseds_cu,
  cu_boundary_i
)

cu_timing_plot(cu_timing_i)
