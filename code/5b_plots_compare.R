################################################################################
#
# 5b_plots_compare.R
#
#  Functions for plotting and comparing indicator values across multiple CUs

### Plots:
# 1.  Comparison of indicator standardization function and unstandardized vs standardized values
# 2.  Lollipop chart comparison of values for a single indicator across CUs
# 3.  Summary of multiple indicators for a life stage/category (using lollipop chart)
# 4.  Maps of distribution of indicator values coloured by CU boundary

#  Plot of indicator value vs latitude, elevation, glacial coverage

# 5. Comparison of indicator value between PCIC and statistical models

###############################################################################


# 1. Indicator value comparison, standardized vs raw ----------------------

# function to show how standardized values compare to raw ones across CUs
# requires standardization function and data table with raw and unstandardized outputs

plot_std_vs_raw <- function(data,
                            indicator_pick,
                            indicator_stat,
                            plot_colours = species_palette,
                            gcm_range_suffix = c("qlowgcm", "qhighgcm"),
                            include_histogram = TRUE) {

  data_sub <- subset_ind_table(data,
    indicators_choose = indicator_pick,
    get_raw = T,
    get_std = T,
    get_gcm = T)

  if (indicator_stat == "category") include_histogram <- FALSE # check if indicator is a category

  plot_data <- rename_ind_table(data_sub,
    indicator_abbrev = indicator_pick)

  # boolean for whether gcm ranges are in the data
  has_gcm <- FALSE
  if (sum(str_detect(names(plot_data), "gcm")) > 0) has_gcm <- TRUE

  p <- ggplot(plot_data) +
    labs(
      color = "Species",
      x = "Raw (unstandardized)",
      y = "Standardized"
    )

  if (has_gcm) {
    p <- p + geom_segment(
      aes(x = min_gcm, xend = max_gcm, y = std),
      color = "grey",
      linewidth = 1
    )
  }


  p <- p + geom_point(aes(x = raw, y = std, color = sp), size = 2.5) +
    scale_fill_manual(values = plot_colours)

  if (include_histogram == TRUE) {
    p2 <- ggplot(plot_data) +
      geom_histogram(aes(x = raw)) +
      labs(
        x = "Raw",
        y = "Count"
      )

    p3 <- ggplot(plot_data) +
      geom_histogram(aes(x = std)) +
      labs(
        x = "Standardized",
        y = ""
      )

    return(p / (p2 | p3))
  }

  return(p)
}


#-----------------2. lollipop chart of indicator across CUs--------------------

# Function to create a lollipop chart of indicator values across CUs
plot_lollipop <- function(data,
                          indicator_pick,
                          indicator_name = "",
                          use_standardized = FALSE,  # use raw or transformed (standardized values)
                          plot_colours = species_palette,
                          ...) {

  data_sub <- subset_ind_table(data,
    indicators_choose = indicator_pick,
    id_col = "CVIS_NAME",
    get_raw = T,
    get_std = T,
    get_gcm = T,
    get_spat = T)

  plot_data <- rename_ind_table(data_sub,
    indicator_abbrev = indicator_pick)

  # boolean for whether gcm ranges are in the data
  has_gcm <- FALSE
  if (sum(str_detect(names(plot_data), "gcm")) > 0) has_gcm <- TRUE

  # boolean for whether gcm ranges are in the data
  has_spat <- FALSE
  if (sum(str_detect(names(plot_data), "spat")) > 0) has_spat <- TRUE

  plot_data <- plot_data %>%
    mutate(id_label = paste0(
      "<span style='color:", plot_colours[plot_data$sp], "'>",
      plot_data$id, "</span>"
    ))

  p <- ggplot(plot_data, aes(x = id_label))

  # Actual plot layers
  if (has_spat) {
    p <- p + geom_segment(aes(xend = id_label, y = min_spat, yend = max_spat),
      color = "darkgrey", linewidth = 2.5)
  }
  if (has_gcm) {
    p <- p + geom_segment(aes(xend = id_label, y = min_gcm, yend = max_gcm),
      color = "darkred", linewidth = 1)
  }

  # Point layer with dynamic fill
  if (use_standardized == F) {
    p <- p + geom_point(aes(y = raw, fill = std), shape = 21, color = "black", size = 2.5) +
      scale_fill_gradient(name = "Std score", low = "lightblue", high = "darkblue")
  }
  if (use_standardized == T) {
    p <- p + geom_point(aes(y = std, fill = std), shape = 21, color = "black", size = 2.5) +
      scale_fill_gradient(name = "Std score", low = "lightblue", high = "darkblue")

  }

  # Dummy layers for line segment legend
  p <- p + geom_segment(aes(x = Inf, xend = Inf, y = Inf, yend = Inf, color = "Spatial range"),
    linewidth = 2.5, inherit.aes = FALSE) +
    geom_segment(aes(x = Inf, xend = Inf, y = Inf, yend = Inf, color = "GCM range"),
      linewidth = 1, inherit.aes = FALSE) +

    # Manual legend styling for segments
    scale_color_manual(
      values = c("Spatial range" = "darkgrey",
        "GCM range" = "darkred")
    ) +

    labs(
      subtitle = paste(indicator_name),
      fill = "Standardized",
      y = NULL,
      x = NULL
    ) +
    coord_flip() +
    theme(
      axis.text.y = element_markdown(size = 7),
      legend.position = "right"
    )

  return(p)
}


# 3. Multiple indicator plot ----------------------------------------------

# Make a multi-panel plot of indicators using the lollipop chart and patchwork

multi_indicator_plot <- function(data,
                                 indicators_choose = c("migrT", "migrQ", "migrA21", "migr_wdist"),
                                 use_standardized_all = T,
                                 title_custom = "") {

  seq_pick <- seq_along(indicators_choose)

  tt <- 1
  for (ind in indicators_choose) {

    if (tt == 1) {
      a1 <- plot_lollipop(data,
        indicator_pick = ind,
        use_standardized = use_standardized_all
      ) +
        theme(legend.position = "none",
          axis.text.x = element_text(size = 8, angle = 45)) +
        labs(title = "",
          y = ind,
          x = NULL)

      multi_p <- a1
    } else {
      a1 <- plot_lollipop(data,
        indicator_pick = ind,
        use_standardized = use_standardized_all
      ) +
        theme(legend.position = "none",
          axis.text.y = element_blank(),
          axis.text.x = element_text(size = 8, angle = 45)) +
        labs(title = "",
          y = ind,
          x = NULL)
      multi_p <- multi_p | a1
    }

    tt <- tt + 1
  }

  multi_p <- multi_p +
    # plot_layout(ncol = tt)
    theme(plot.margin = margin(0, 0, 0, 0),
      axis.text.x = element_text(size = 8, angle = 45)) +
    plot_layout(guides = "collect") +
    plot_annotation(
      title = title_custom
    )

  return(multi_p)

}



# 4. Spatial distribution of indicator values by CU boundary --------------


spatial_indicator_plot <- function(data,
                                   outline = Fr_basin,
                                   sp_pick = c("Chinook", "Coho", "Sockeye"),
                                   sp_col_name = "SPECIES_NAME",
                                   indicator_pick,
                                   indicator_name,
                                   use_standardized = T,
                                   id_col = "FULL_CU_IN",
                                   brewer_palette = "RdYlGn",
                                   palette_direction = -1) {

  data_sub <- subset_ind_table(data,
    indicators_choose = indicator_pick,
    sp_col = sp_col_name,
    id_col = "FULL_CU_IN",
    get_raw = !use_standardized,
    get_std = use_standardized,
    get_gcm = F)

  plot_data <- rename_ind_table(data_sub,
    indicator_abbrev = indicator_pick,
    single_value_col = T)

  cu_boundary_plot <- cu_boundary %>%
    left_join(select(plot_data, id, value), by = join_by(!!sym(id_col) == id)) %>%
    rename(sp_col = !!sym(sp_col_name)) %>%
    filter(!is.na(value),
      sp_col %in% sp_pick)

  p <- ggplot() +
    geom_sf(data = cu_boundary_plot, aes(fill = value), alpha = 0.3) +
    scale_fill_distiller(palette = brewer_palette, direction = palette_direction) +
    # geom_label(data = cu_boundary_show, aes(label = CUID), size = 2) +
    geom_sf(data = Fr_basin, colour = "black", fill = NA, alpha = 0.3) +
    labs(fill = indicator_pick) +
    coord_sf(datum = NA) +
    facet_grid(. ~ sp_col)


  return(p)

}


# 5. Tile plot of standardized indicator values ---------------------------


indicator_tile_plot <- function(data,
                                indicators_choose = c("migrT", "migrQ", "migrA21", "migrdist"),
                                brewer_palette = "RdYlGn",
                                palette_direction = -1,
                                plot_colours = species_palette) {


  data_sub <- subset_ind_table(data,
    indicators_choose,
    get_raw = F,
    get_std = T,
    get_gcm = F,
    id_col = "CVIS_NAME")

  # # take column names that contain prefix with model type
  cols_sub <- names(data_sub)[str_detect(names(data_sub), paste0(indicators_choose, collapse = "|"))]

  # Prepare data for plotting
  plot_data <- data_sub %>%
    # select(all_of(c(id_col, sp_col, cols_sub))) %>%
    pivot_longer(names_to = "indicator",
      values_to = "value",
      cols = cols_sub) %>%
    mutate(indicator = str_remove(indicator, "std_")) %>%
    mutate(indicator = str_remove(indicator, "_mean"))

  # add a label for coloured text using ggtext
  plot_data <- plot_data %>%
    mutate(id_label = paste0(
      "<span style='color:", plot_colours[plot_data$sp], "'>",
      plot_data$id, "</span>"
    ))


  p <- ggplot(plot_data, aes(x = indicator, y = id_label)) +
    geom_tile(aes(fill = value)) +
    geom_text(aes(label = round(value, 1)), size = 2) + # Add the text labels
    scale_fill_distiller(palette = brewer_palette, direction = palette_direction) +
    theme(
      axis.text.y = element_markdown(size = 8, hjust = 1),
      axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
      legend.position = "none",
      panel.grid = element_blank()
    ) +
    labs(y = NULL,
      x = NULL)

  p

}



# 6. Correlation analysis and plots ---------------------------------------

get_correlation_matrix <- function(data,
                                   indicators_choose = tbl_indicators$abbrev,
                                   use_standardized = F) {

  data_sub <- subset_ind_table(data,
    indicators_choose,
    get_raw = !use_standardized,
    get_std = use_standardized,
    get_gcm = F)

  # take column names that contain prefix with model type
  cols_sub <- names(data_sub)[str_detect(names(data_sub), paste0(indicators_choose, collapse = "|"))]

  # filter to numeric columns only
  numeric_cols <- cols_sub[sapply(data_sub[cols_sub], is.numeric)]

  # take just numeric indicator columns, remove sp and id
  cor_data <- data_sub %>%
    select(all_of(numeric_cols))

  cor(cor_data,  use = "pairwise.complete.obs")

}



# 7. Make all plots -------------------------------------------------------

# utility function to make all plots for an indicator, used for markdown reporting
make_indicator_plots <- function(data,
                                 ind_pick,
                                 tbl = tbl_indicators,
                                 standardized_plots = FALSE,
                                 make_spatial = TRUE,
                                 spatial_pal_dir = -1) {

  ind_row <- tbl[tbl$abbrev == ind_pick, ]

  p <- plot_lollipop(data,
    indicator_pick = ind_row$abbrev,
    indicator_name = ind_row$name,
    use_standardized = standardized_plots
  )

  prs <- plot_std_vs_raw(data,
    indicator_pick = ind_row$abbrev,
    indicator_stat = ind_row$stat)

  print(p)

  print(prs)

  if (make_spatial == TRUE) {
    pmap <- spatial_indicator_plot(data,
      cu_boundary,
      indicator_pick = ind_row$abbrev,
      indicator_name = ind_row$name,
      use_standardized = standardized_plots,
      palette_direction = spatial_pal_dir)

    print(pmap)

  }

}




# x_y indicator comparison ------------------------------------------------

xy_indicator_plot <- function(data,
                              x_pick = "lowQpdelta",
                              y_pick = "st8pdelta",
                              point_col = "Species") {

  plot_data_x <- subset_ind_table(data,
    indicators_choose = x_pick,
    id_col = "CVIS_NAME",
    sp_col = point_col,
    get_raw = T,
    get_gcm = T) %>%
    rename_ind_table(indicator_abbrev = x_pick,
      name_suffix = "x_")

  plot_data_y <- subset_ind_table(data,
    indicators_choose = y_pick,
    id_col = "CVIS_NAME",
    sp_col = point_col,
    get_raw = T,
    get_gcm = T) %>%
    rename_ind_table(indicator_abbrev = y_pick,
      name_suffix = "y_")


  plot_data <- bind_cols(plot_data_x,
    select(plot_data_y, contains("y_")))


  p <- ggplot(plot_data) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    geom_errorbar(aes(x = x_raw, y = y_raw, ymin = y_min_gcm, ymax = y_max_gcm), colour = "blue") +
    geom_errorbar(aes(x = x_raw, y = y_raw, xmin = x_min_gcm, xmax = x_max_gcm)) +
    geom_point(aes(x = x_raw, y = y_raw, col = sp), size = 3) +
    labs(subtitle = "statistical station model (st8) vs PCIC stream model",
      x = "PCIC stream model change in August flow",
      y = "Station model change in August flow")



}

# Test plots --------------------------------------------------------------

# filter_std <- all_flat_std %>%
#   mutate(prop_coverage = as.numeric(prop_coverage)) %>%
#   filter(rcp == "85",
#     period_code == 3,
#     prop_coverage > 0.2)
#
# p <- xy_indicator_plot(filter_std,
#   point_col = "prop_coverage")

# flat_std_sub <- filter(
#   all_flat_std,
#   RCP == "45", period_code == 3
# )
#
# for (i in 1:nrow(tbl_indicators)) {
#   # Construct filename
#   file_name <- paste0(paths$figures, "/ind_", tbl_indicators$abbrev[i], ".png")
#
#   png(filename = file_name, width = 800, height = 600)
#
#   p <- plot_lollipop(flat_std_sub,
#     indicator_pick = tbl_indicators$abbrev[i],
#     indicator_stat = tbl_indicators$stat[i],
#     indicator_name = tbl_indicators$name[i],
#     use_standardized = FALSE
#   )
#
#   print(p)
#
#   # Construct filename
#   file_name <- paste0(paths$figures, "/map_", tbl_indicators$abbrev[i], ".png")
#
#   png(filename = file_name, width = 800, height = 600)
#
#   p2 <- spatial_indicator_plot(flat_std_sub,
#     cu_boundary,
#     indicator_pick = tbl_indicators$abbrev[i],
#     indicator_stat = tbl_indicators$stat[i],
#     indicator_name = tbl_indicators$name[i])
#
#   print(p2)
#
#   dev.off()
# }
#
#
# p <- plot_std_vs_raw(flat_std_sub,
#   indicator_pick = tbl_indicators$abbrev[i],
#   indicator_stat = tbl_indicators$stat[i],
#   indicator_name = tbl_indicators$name[i],
#   indicator_fun = tbl_indicators$std_fun[i]
# )
#
# print(p)
#
# # make a multi-panel plot of standardized indicator values
#
#
# multi_p <- multi_indicator_plot(flat_std_sub,
#   indicators_metadata = tbl_indicators,
#   title_custom = "Migration indicators")
#
# print(multi_p)
