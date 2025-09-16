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

#
###############################################################################

# libraries for plotting
library(ggforce)   # for custom facet sizes
library(ggtext)  # for coloured text in axis labels
library(RColorBrewer)
# library(ggsci)   # colour palettes - pal_futurama

## color palette function
get_scico_palette <- function(data, column, palette_name = "berlin") {
  categories <- sort(unique(data[[column]]))
  setNames(scico(length(categories), palette = palette_name), categories)
}

# species_palette <- get_scico_palette(cu_run, "CU_Species", "batlow")


get_brewer_palette <- function(data, column, palette_name = "Set2") {
  categories <- sort(unique(data[[column]]))

  setNames(brewer.pal(length(categories), name = palette_name), categories)
  # brewer.pal(n, palette)
}

species_palette <- get_brewer_palette(cu_run, "CU_Species", "Set1")


# species_palette <- pal_futurama()(length(unique(cu_run$CU_Species)))
#
# species_palette_lookup <- tibble("sp" = unique(cu_run$CU_Species),
#                                     "col" = species_palette)

# species_colors <- c(
#   "Chinook" = "#1b9e77",
#   "Sockeye (Lake Type)" = "#d95f02",
#   "Coho" = "#7570b3",
#   "Chum" = "goldenrod",
#   "Pink" = "darkmagenta"
# )
#

# 1. Indicator value comparison, standardized vs raw ----------------------

# function to show how standardized values compare to raw ones across CUs
# requires standardization function and data table with raw and unstandardized outputs

plot_std_vs_raw <- function(data,
                            indicator_pick,
                            indicator_name,
                            indicator_stat,
                            indicator_fun,
                            plot_colours = species_palette,
                            include_histogram = TRUE) {

  sp_col <- "CU_Species"
  gcm_range_suffix <- c("qlowgcm", "qhighgcm")

  # Define suffixes
  stat_suffix <- if_else(indicator_stat %in% c("value", "category"), indicator_pick, indicator_stat)

  if (indicator_stat == "category") include_histogram <- FALSE # check if indicator is a category

  # take column names that contain prefix with model type
  cols_sub <- names(data)[str_detect(names(data), indicator_pick)]

  # get raw and standardized columns
  cols_sub_raw <- cols_sub[!str_detect(cols_sub, "std")]
  cols_sub_std <- cols_sub[str_detect(cols_sub, "std")]


  # take names with prefix that also contain stat suffix
  stat_col_raw <- cols_sub_raw[str_detect(cols_sub_raw, stat_suffix)] # must contain mean
  stat_col_std <- cols_sub_std[str_detect(cols_sub_std, stat_suffix)]

  min_gcmcol_raw <- cols_sub_raw[str_detect(cols_sub_raw, paste0(gcm_range_suffix[1], collapse = "|"))]
  max_gcmcol_raw <- cols_sub_raw[str_detect(cols_sub_raw, paste0(gcm_range_suffix[2], collapse = "|"))]
  min_gcmcol_std <- cols_sub_std[str_detect(cols_sub_std, paste0(gcm_range_suffix[1], collapse = "|"))]
  max_gcmcol_std <- cols_sub_std[str_detect(cols_sub_std, paste0(gcm_range_suffix[2], collapse = "|"))]

  # Prepare data for plotting
  plot_data <- data %>%
    select(all_of(c(stat_col_std, stat_col_raw, sp_col))) %>%
    rename(
      raw = !!stat_col_raw,
      std = !!stat_col_std,
      sp = !!sp_col
    )

  # add GCM range if available
  if (length(min_gcmcol_raw) == 1 && length(max_gcmcol_raw) == 1) {
    plot_data <- plot_data %>%
      mutate(min_gcm_raw = data[[min_gcmcol_raw]], max_gcm_raw = data[[max_gcmcol_raw]])
    has_gcm_raw <- TRUE
  } else {
    has_gcm_raw <- FALSE
  }
  # add GCM range if available
  if (length(min_gcmcol_std) == 1 && length(max_gcmcol_std) == 1) {
    plot_data <- plot_data %>%
      mutate(min_gcm_std = data[[min_gcmcol_std]], max_gcm_std = data[[max_gcmcol_std]])
    has_gcm_std <- TRUE
  } else {
    has_gcm_std <- FALSE
  }

  p <- ggplot(plot_data) +
    labs(
      # title = paste(indicator_name),
      subtitle = paste0("Standardization function: ", indicator_fun),
      color = "Species",
      x = "Raw (unstandardized)",
      y = "Standardized"
    )

  if (has_gcm_raw) {
    p <- p + geom_segment(
      aes(x = min_gcm_raw, xend = max_gcm_raw, y = std),
      color = "grey",
      linewidth = 1
    )
  }
  if (has_gcm_std) {
    p <- p + geom_segment(
      aes(x = raw, y = min_gcm_std, yend = max_gcm_std),
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
                          indicator_name,
                          indicator_stat,
                          use_standardized = FALSE,  # use raw or transformed (standardized values)
                          plot_colours = species_palette) {

  id_col <- "CVIS_NAME"
  sp_col <- "CU_Species"
  sp_suffix <- c("qlowsp", "qhighsp")
  gcm_range_suffix <- c("qlowgcm", "qhighgcm")

  # Define suffixes
  stat_suffix <- if_else(indicator_stat %in% c("value", "category"), indicator_pick, indicator_stat)

  # take column names that contain prefix with model type
  cols_sub <- names(data)[str_detect(names(data), indicator_pick)]

  if (use_standardized == TRUE) {
    cols_sub <- cols_sub[str_detect(cols_sub, "std")]  # select std columns
  }
  if (use_standardized == FALSE) cols_sub <- cols_sub[!str_detect(cols_sub, "std")]

  # take names with prefix that also contain stat suffix
  stat_col <- cols_sub[str_detect(cols_sub, stat_suffix)] # must contain mean
  min_spcol <- cols_sub[str_detect(cols_sub, sp_suffix[1])]
  max_spcol <- cols_sub[str_detect(cols_sub, sp_suffix[2])]
  min_gcmcol <- cols_sub[str_detect(cols_sub, paste0(gcm_range_suffix[1], collapse = "|"))]
  max_gcmcol <- cols_sub[str_detect(cols_sub, paste0(gcm_range_suffix[2], collapse = "|"))]


  # Prepare data for plotting
  plot_data <- data %>%
    select(all_of(c(id_col, stat_col, sp_col))) %>%
    rename(
      mean = !!stat_col,
      id = !!id_col,
      sp = !!sp_col
    )

  plot_data <- plot_data %>%
    mutate(id_label = paste0(
      "<span style='color:", plot_colours[plot_data$sp], "'>",
      plot_data$id, "</span>"
    ))

  # Add min and max if available
  if (length(min_spcol) == 1 && length(max_spcol) == 1) {
    plot_data <- plot_data %>%
      mutate(minsp = data[[min_spcol]], maxsp = data[[max_spcol]])
    has_range <- TRUE
  } else {
    has_range <- FALSE
  }

  # add GCM range if available
  if (length(min_gcmcol) == 1 && length(max_gcmcol) == 1) {
    plot_data <- plot_data %>%
      mutate(mingcm = data[[min_gcmcol]], maxgcm = data[[max_gcmcol]])
    has_gcm <- TRUE
  } else {
    has_gcm <- FALSE
  }

  p <- ggplot(plot_data, aes(x = id_label))

  # Actual plot layers
  if (has_range) {
    p <- p + geom_segment(aes(xend = id_label, y = minsp, yend = maxsp),
      color = "darkgrey", linewidth = 2.5)
  }
  if (has_gcm) {
    p <- p + geom_segment(aes(xend = id_label, y = mingcm, yend = maxgcm),
      color = "darkred", linewidth = 1)
  }

  # Point layer with dynamic fill
  p <- p + geom_point(aes(y = mean, fill = mean), shape = 21, color = "black", size = 2.5) +
    scale_fill_gradient(name = "Mean", low = "lightblue", high = "darkblue") +

    # Dummy layers for line segment legend
    geom_segment(aes(x = Inf, xend = Inf, y = Inf, yend = Inf, color = "Spatial range"),
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
      y = stat_col
    ) +
    coord_flip() +
    theme(
      axis.text.y = element_markdown(size = 7),
      legend.position = "right"
    )

  return(p)
}

data <- filter_std


# 3. Multiple indicator plot ----------------------------------------------

# Make a multi-panel plot of indicators using the lollipop chart and patchwork

multi_indicator_plot <- function(data,
                                 indicators_metadata,
                                 indicators_choose = c("migrT", "migrQ", "migrA21", "migr_wdist"),
                                 use_standardized_all = T,
                                 title_custom = "") {

  seq_pick <- seq_along(indicators_choose)
  tbl_sub <- filter(tbl_indicators, abbrev %in% indicators_choose)

  tt <- 1
  for (i in seq_pick) {

    if (tt == 1) {
      a1 <- plot_lollipop(data,
        indicator_pick = tbl_sub$abbrev[i],
        indicator_stat = tbl_sub$stat[i],
        indicator_name = tbl_sub$name[i],
        use_standardized = use_standardized_all
      ) +
        theme(legend.position = "none") +
        labs(title = "")

      multi_p <- a1
    } else {
      a1 <- plot_lollipop(data,
        indicator_pick = tbl_sub$abbrev[i],
        indicator_stat = tbl_sub$stat[i],
        indicator_name = tbl_sub$name[i],
        use_standardized = use_standardized_all
      ) +
        theme(legend.position = "none",
          axis.text.y = element_blank()) +
        labs(title = "",
          x = "")
      multi_p <- multi_p | a1
    }

    tt <- tt + 1
  }

  multi_p <- multi_p + plot_annotation(
    title = title_custom
  )

  return(multi_p)

}



# 4. Spatial distribution of indicator values by CU boundary --------------


spatial_indicator_plot <- function(data,
                                   outline = Fr_basin,
                                   sp_pick = c("Chinook", "Coho", "Sockeye (Lake Type)"),
                                   indicator_pick,
                                   indicator_name,
                                   indicator_stat = "mean",
                                   use_standardized = TRUE,
                                   brewer_palette = "RdYlGn",
                                   palette_direction = -1) {
  sp_col <- "CU_Species"

  # Define suffixes
  stat_suffix <- if_else(indicator_stat == "value", indicator_pick, indicator_stat)

  # take column names that contain prefix with model type
  cols_sub <- names(data)[str_detect(names(data), indicator_pick)]

  if (use_standardized == TRUE) {
    cols_sub <- cols_sub[str_detect(cols_sub, "std")]  # select std columns
  }
  if (use_standardized == FALSE) {
    cols_sub <- cols_sub[!str_detect(cols_sub, "std")]  # remove std columns if using raw
  }

  # cols_sub <- cols_sub[!str_detect(cols_sub, "gcm")]  # remove gcm variation columns

  # take names with prefix that also contain stat suffix
  stat_col <- cols_sub[str_detect(cols_sub, stat_suffix)] # must contain mean

  data <- mutate(data, plot_col = .data[[stat_col]],
    sp_col = .data[[sp_col]])

  data_sp <- filter(data, sp_col %in% sp_pick)

  cu_boundary_plot <- cu_boundary %>%
    left_join(select(data_sp, FULL_CU_IN, plot_col), join_by(FULL_CU_IN)) %>%
    filter(!is.na(plot_col))


  p <- ggplot() +
    geom_sf(data = cu_boundary_plot, aes(fill = plot_col), alpha = 0.3) +
    scale_fill_distiller(palette = brewer_palette, direction = palette_direction) +
    # geom_label(data = cu_boundary_show, aes(label = CUID), size = 2) +
    geom_sf(data = Fr_basin, colour = "black", fill = NA, alpha = 0.3) +
    labs(fill = indicator_pick) +
    coord_sf(datum = NA) +
    facet_grid(. ~ Species)


  return(p)

}




# 5. Tile plot of standardized indicator values ---------------------------


indicator_tile_plot <- function(data,
                                indicators_choose = c("migrT", "migrQ", "migrA21", "migr_wdist"),
                                brewer_palette = "RdYlGn",
                                palette_direction = -1,
                                plot_colours = species_palette) {

  id_col <- "CVIS_NAME"
  sp_col <- "CU_Species"

  # take column names that contain prefix with model type
  cols_sub <- names(data)[str_detect(names(data), paste0(indicators_choose, collapse = "|"))]

  cols_sub <- cols_sub[str_detect(cols_sub, "std")]  # select std columns

  cols_sub <- cols_sub[!str_detect(cols_sub, "gcm")]  # remove gcm variation columns

  # Prepare data for plotting
  plot_data <- data %>%
    select(all_of(c(id_col, sp_col, cols_sub))) %>%
    pivot_longer(names_to = "indicator",
      values_to = "value",
      cols = cols_sub) %>%
    rename(
      id = !!id_col,
      sp = !!sp_col
    ) %>%
    mutate(indicator = str_remove(indicator, "std_")) %>%
    mutate(indicator = str_remove(indicator, "_mean"))

  plot_data <- plot_data %>%
    mutate(id_label = paste0(
      "<span style='color:", plot_colours[plot_data$sp], "'>",
      plot_data$id, "</span>"
    ))


  p <- ggplot(plot_data, aes(y = id_label, x = indicator)) +
    geom_tile(aes(fill = value)) +
    geom_text(aes(label = round(value, 1)), size = 2) + # Add the text labels
    scale_fill_distiller(palette = brewer_palette, direction = palette_direction) +
    theme(
      axis.text.y = element_markdown(size = 6),
      axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
      legend.position = "none"
    ) +
    labs(y = NULL,
      x = NULL)


  # ggplot(plot_data, aes(y = id, x = indicator)) +
  #   geom_tile(aes(fill = value)) +
  #   geom_text(aes(label = round(value, 1)), size = 1.8) +
  #   scale_fill_scico(palette = scico_palette,
  #     direction = palette_direction,
  #     limits = c(0, 1)) +
  #   theme(
  #     axis.text.y = element_text(size = 6),
  #     axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
  #     legend.position = "none",
  #     # plot.margin = margin(5, 5, 5, 5)
  #   ) +
  #   labs(y = NULL,
  #     x = NULL) +
  #   # facet_grid(sp ~ ., scales = "free_y")
  #   facet_col(~sp, scales = "free_y", space = "free")

  p

}


# 6. Correlation analysis and plots ---------------------------------------

get_correlation_matrix <- function(data,
                                   indicators_choose = tbl_indicators$abbrev,
                                   use_standardized = TRUE) {
  # take column names that contain prefix with model type
  cols_sub <- names(data)[str_detect(names(data), paste0(indicators_choose, collapse = "|"))]

  if (use_standardized == TRUE) {
    cols_sub <- cols_sub[str_detect(cols_sub, "std")]  # select std columns
  }
  if (use_standardized == FALSE) {
    cols_sub <- cols_sub[!str_detect(cols_sub, "std")]  # remove std columns if using raw
  }

  cols_sub <- cols_sub[str_detect(cols_sub, "mean")]  # take only mean, get rid of gcm ranges

  cor_data <- data %>%
    select(all_of(cols_sub))

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
    indicator_stat = ind_row$stat,
    indicator_name = ind_row$name,
    use_standardized = standardized_plots
  )

  prs <- plot_std_vs_raw(data,
    indicator_pick = ind_row$abbrev,
    indicator_stat = ind_row$stat,
    indicator_name = ind_row$name,
    indicator_fun =  ind_row$std_fun)

  print(p)

  print(prs)

  if (make_spatial == TRUE) {
    pmap <- spatial_indicator_plot(data,
      cu_boundary,
      indicator_pick = ind_row$abbrev,
      indicator_stat = ind_row$stat,
      indicator_name = ind_row$name,
      use_standardized = standardized_plots,
      palette_direction = spatial_pal_dir)

    print(pmap)

  }

}


# Test plots --------------------------------------------------------------



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
