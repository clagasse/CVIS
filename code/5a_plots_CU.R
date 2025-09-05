################################################################################
#
# 5a_plots_CU.R
#
#  Functions for plotting and mapping data within a single conservation unit
#  Focus on freshwater stream indicators
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











