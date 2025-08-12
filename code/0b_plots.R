################################################################################
#
# 0b_plots.R
#
#  Functions for plotting and mapping
#
###############################################################################


#--------- 2. fw cu boundary/stream plots --------------


stream_indicator_plot <- function(fwModels,
                                  Tw_stations,
                                  variable = "CT_anad", 
                                  plot_title = "Indicator plot", 
                                  histogram_fill = "model_rs",
                                  xlim = NA,
                                  temp_stations = FALSE) {
  
  # Convert variable name to symbol for tidy evaluation
  var_sym <- sym(variable)
  hist_sym <- sym(histogram_fill)
  
  p1 <- ggplot() +
    geom_sf(data = fwModels, aes(color = !!var_sym), linewidth = 1.) +
    scale_color_viridis_c() + 
    geom_sf(data = cu_boundary_i, color = "black", alpha = 0.3) +
    coord_sf(xlim = st_bbox(cu_boundary_i)[c(1,3)],
             ylim = st_bbox(cu_boundary_i)[c(2,4)]) +
    labs(subtitle = plot_title)
  
  if(sum(!is.na(xlim)) > 0) p1 <- p1 + scale_color_viridis_c(limits = xlim)
  if(temp_stations == TRUE) { 
    p1 <- p1 + 
      geom_sf(data = Tw_stations, color = "blue") +
      coord_sf(xlim = st_bbox(cu_boundary_i)[c(1,3)],
               ylim = st_bbox(cu_boundary_i)[c(2,4)])
  }
  
  
  h1 <- ggplot(fwModels) +
    geom_vline(aes(xintercept = mean(!!var_sym, na.rm = T)), color = "red", linetype = "dashed") +
    labs(fill = "BC Fishpass") +
    geom_text(aes(x = mean(!!var_sym, na.rm = T), y = 20, label = "mean"), color = "red") +
    geom_histogram(aes(x = !!var_sym, fill = !!hist_sym))
  
  if(sum(!is.na(xlim)) > 0) h1 <- h1 + xlim(xlim)
  
  p1 / h1 +
    plot_layout(heights = c(3, 1))
  
}



spatial_indicator_plot <- function(data,
                                   outline = Fr_basin,
                                   sp_pick = "Chinook", 
                                   indicator_pick, 
                                   palette = "Zissou1",
                                   reversed_pal = FALSE) 
{
  
  data_sp <- filter(data, Species == sp_pick)
  
  #take column names that contain prefix with model type
  cols_sub <- names(data_sp)[str_detect(names(data_sp), indicator_pick$abbrev)]
  #take names with prefix that also contain stat suffix
  stat_col <- cols_sub[str_detect(cols_sub, paste0(stat_suffix, "$"))]
  
  data_sp <- mutate(data_sp, plot_col = .data[[stat_col]]) 
  
  
  # Generate the palette
  palette <- wes_palette("Zissou1", 100, type = "continuous")
  if (reversed_pal) {
    palette <- rev(palette)
  }
  
  p <- ggplot() +
    geom_sf(data = data_sp, aes(fill = plot_col), alpha = 0.3) +
    scale_fill_gradientn(colours = palette) +
    #geom_label(data = cu_boundary_show, aes(label = CUID), size = 2) +
    geom_sf(data = Fr_basin, colour = "black", fill = NA, alpha = 0.3) + 
    labs(subtitle = paste(sp_pick, "-", indicator_pick$name),
         legend = indicator_pick$abbrev)
  
  return(p)
  
}


#-----------------3. migration plots -------------------








#-----------------4. indicator plots--------------------

# Function to create a lollipop chart of indicator values across CUs
plot_lollipop <- function(data, indicator_pick) {
  # Define suffixes
  stat_suffix <- "wmean"
  sp_suffix <- c("wqlow_sp", "wqhigh_sp")
  gcm_range_suffix <- c("qlow_gcm", "qhigh_gcm")
  
  #take column names that contain prefix with model type
  cols_sub <- names(data)[str_detect(names(data), indicator_pick$abbrev)]
  
  #take names with prefix that also contain stat suffix
  stat_col <- cols_sub[str_detect(cols_sub, paste0(stat_suffix, "$"))]  #must end with wmean
  min_col  <- cols_sub[str_detect(cols_sub,sp_suffix[1])]
  max_col  <- cols_sub[str_detect(cols_sub,sp_suffix[2])]
  min_gcmcol <- cols_sub[str_detect(cols_sub, paste0(gcm_range_suffix[1], collapse = "|"))]
  max_gcmcol <- cols_sub[str_detect(cols_sub, paste0(gcm_range_suffix[2], collapse = "|"))]
  
  id_col <- "CU_NAME"
  sp_col <- "CU_Species"
  
  # Check if mean and ID columns exist
  if (!all(c(id_col, stat_col) %in% names(data))) {
    stop("One or more required columns are missing in the data.")
  }
  
  # Prepare data for plotting
  plot_data <- data %>%
    select(all_of(c(id_col, stat_col, sp_col))) %>%
    rename(mean = !!stat_col, id = !!id_col, sp = !!sp_col)
  
  #palette_colors <- pal_npg("nrc")(5) # 4 colors from Nature Publishing Group palette
  
  # Create a named color palette using unique categories
  unique_sp <- unique(plot_data$sp)
  palette_colors <- pal_npg("nrc")(length(unique_sp))
  names(palette_colors) <- unique_sp
  
  plot_data$color <- palette_colors[plot_data$sp]
  plot_data$label <- paste0("<span style='color:", plot_data$color, "'>", plot_data$sp, "</span>")
  
  
  # Create a unique ordering of id by sp
  ordered_ids <- plot_data %>%
    arrange(sp, id) %>%
    distinct(id) %>%
    pull(id)
  
  # Apply the ordering
  plot_data$id <- factor(plot_data$id, levels = ordered_ids)
  
  # Add min and max if available
  if (length(min_col) == 1 && length(max_col) == 1) {
    plot_data <- plot_data %>%
      mutate(
        min = data[[min_col]],
        max = data[[max_col]]
      )
    has_range <- TRUE
  } else {
    has_range <- FALSE
  }
  
  # add GCM range if available
  if (length(min_gcmcol) == 1 && length(max_gcmcol) == 1) {
    plot_data <- plot_data %>%
      mutate(
        mingcm = data[[min_gcmcol]],
        maxgcm = data[[max_gcmcol]]
      )
    has_gcm <- TRUE
  } else {
    has_gcm <- FALSE
  }
  
  # Create the lollipop chart
  p <- ggplot(plot_data, aes(x = id, color = sp)) +
    scale_fill_manual(values = palette_colors) 
  
  if (has_range) {
    p <- p + geom_segment(aes(xend = id, y = min, yend = max), 
                          color = "darkgrey",
                          linewidth = 2.5)
  }
  
  if (has_gcm) {
    p <- p + geom_segment(aes(xend = id, y = mingcm, yend = maxgcm), 
                          color = "darkred",
                          linewidth = 1)
  }
  
  p +   geom_point(aes(y = mean), size = 2.5) +
    labs(
      title = paste(indicator_pick$name),
      x = "CU",
      y = stat_col,
      color = "Species"
    ) +
    coord_flip() +
    theme_minimal() +
    theme(
      axis.text.y =element_text(size=7)
      
    )
  
}



