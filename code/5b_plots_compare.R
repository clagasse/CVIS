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
    scale_color_manual(values = plot_colours)

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
    get_spat = T,
    get_pop = T)

  plot_data <- rename_ind_table(data_sub,
    indicator_abbrev = indicator_pick)

  # boolean for whether gcm ranges are in the data
  has_gcm <- FALSE
  if (sum(str_detect(names(plot_data), "gcm")) > 0) has_gcm <- TRUE

  # boolean for whether spatial ranges are in the data
  has_spat <- FALSE
  if (sum(str_detect(names(plot_data), "spat")) > 0) has_spat <- TRUE

  # boolean for whether pop ranges are in the data
  has_pop <- FALSE
  if (sum(str_detect(names(plot_data), "pop")) > 0) has_pop <- TRUE

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
  if (has_pop) {
    p <- p + geom_segment(aes(xend = id_label, y = min_pop, yend = max_pop),
      color = "darkgreen", linewidth = 1)
  }

  # Point layer with dynamic fill
  if (use_standardized == F) {
    p <- p + geom_point(aes(y = raw, fill = std), shape = 21, color = "black", size = 2.5) +
      scale_fill_gradient(name = "Standardized score", low = "lightblue", high = "darkblue")
  }
  if (use_standardized == T) {
    p <- p + geom_point(aes(y = std, fill = std), shape = 21, color = "black", size = 2.5) +
      scale_fill_gradient(name = "Standardized score", low = "lightblue", high = "darkblue")
  }

  # Dummy layers for line segment legend
  p <- p + geom_segment(aes(x = Inf, xend = Inf, y = Inf, yend = Inf, color = "Spatial Q10-Q90"),
    linewidth = 2.5, inherit.aes = FALSE) +
    geom_segment(aes(x = Inf, xend = Inf, y = Inf, yend = Inf, color = "Climate models Q10-Q90"),
      linewidth = 1, inherit.aes = FALSE) +
    geom_segment(aes(x = Inf, xend = Inf, y = Inf, yend = Inf, color = "Population min-max"),
      linewidth = 1, inherit.aes = FALSE) +

    # Manual legend styling for segments
    scale_color_manual(
      values = c("Climate models Q10-Q90" = "darkred",
        "Spatial Q10-Q90" = "darkgrey",
        "Population min-max" = "darkgreen"),
      name = "Variation"
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
    facet_grid(. ~ sp_col) +
    coord_sf(datum = NA, expand = FALSE, clip = "on") +
    theme(
      plot.margin   = grid::unit(c(0, 0, 0, 0), "pt"),
      panel.spacing = grid::unit(2, "pt")
    )


  return(p)

}


# 5. Tile plot of standardized indicator values ---------------------------

indicator_tile_plot <- function(data,
                                indicators_choose = c("migrT", "migrQ", "migrA21", "migrdist"),
                                indicators_metadata = tbl_indicators,
                                brewer_palette = "RdYlGn",
                                palette_direction = -1,
                                plot_colours = species_palette,
                                ncol = NULL,
                                uniform_height = FALSE,
                                overall_score_cols = c("sumavgs"),
                                category_name = NULL) {

  data_sub <- subset_ind_table(data,
    indicators_choose,
    get_raw = F,
    get_std = T,
    get_gcm = F,
    id_col = "CVIS_NAME")

  cols_sub <- names(data_sub)[str_detect(names(data_sub), paste0(indicators_choose, collapse = "|"))]

  # Prepare data for plotting
  plot_data <- data_sub %>%
    pivot_longer(names_to = "indicator",
      values_to = "value",
      cols = cols_sub) %>%
    mutate(indicator = str_remove(indicator, "std_")) %>%
    mutate(indicator = str_remove(indicator, "_mean"))

  # Reorder indicators to put overall score columns last
  indicator_order <- unique(plot_data$indicator)
  if (!is.null(overall_score_cols) && length(overall_score_cols) > 0) {
    # Remove overall score columns from main list
    regular_indicators <- setdiff(indicator_order, overall_score_cols)
    # Add them back at the end in the specified order
    overall_cols_present <- overall_score_cols[overall_score_cols %in% indicator_order]
    indicator_order <- c(regular_indicators, overall_cols_present)
  }
  plot_data <- plot_data %>%
    mutate(indicator = factor(indicator, levels = indicator_order))

  # Calculate color scale limits for each indicator based on range across ALL species
  indicator_limits <- plot_data %>%
    group_by(indicator) %>%
    summarise(
      min_val = min(value, na.rm = TRUE),
      max_val = max(value, na.rm = TRUE),
      .groups = "drop"
    )

  # Join limits back to plot data
  plot_data <- plot_data %>%
    left_join(indicator_limits, by = "indicator")

  # Create normalized value for color mapping (0 to 1 scale)
  plot_data <- plot_data %>%
    mutate(
      value_normalized = (value - min_val) / (max_val - min_val)
    )

  # add a label for coloured text using ggtext
  plot_data <- plot_data %>%
    mutate(id_label = paste0(
      "<span style='color:", plot_colours[plot_data$sp], "'>",
      plot_data$id, "</span>"
    ))

  # Get species list and count CUs per species
  species_list <- unique(plot_data$sp)
  cu_counts <- plot_data %>%
    group_by(sp) %>%
    summarise(
      n_cus = n_distinct(id_label),
      cu_name = if (n_distinct(id_label) == 1) first(id) else NA_character_,
      .groups = "drop"
    ) %>%
    arrange(match(sp, species_list))

  # Auto-detect ncol if not specified
  if (is.null(ncol)) {
    ncol <- ifelse(length(species_list) == 5, 2, ceiling(sqrt(length(species_list))))
  }

  # Calculate relative heights based on number of CUs
  if (!uniform_height) {
    heights <- cu_counts$n_cus
    heights <- heights / min(heights)
    heights <- pmin(heights, 3)
  } else {
    heights <- rep(1, length(species_list))
  }

  # Determine which plots are in the bottom row
  n_plots <- length(species_list)
  n_rows <- ceiling(n_plots / ncol)
  bottom_row_start <- (n_rows - 1) * ncol + 1
  bottom_row_indices <- bottom_row_start:n_plots

  # Create individual plots for each species
  plot_list <- lapply(seq_along(species_list), function(idx) {
    species <- species_list[idx]
    data_sp <- plot_data %>% filter(sp == species)
    n_cus <- cu_counts$n_cus[idx]
    cu_name <- cu_counts$cu_name[idx]

    # Check if this plot is in the bottom row
    is_bottom_row <- idx %in% bottom_row_indices

    # Check if this species has only one CU
    is_single_cu <- n_cus == 1

    # Adjust text size based on number of CUs
    y_text_size <- if (n_cus > 15) 7 else if (n_cus > 10) 8 else 9
    tile_text_size <- if (n_cus > 15) 1.8 else if (n_cus > 10) 2 else 2.5

    # Create title
    plot_title <- paste0(species, " (n=", n_cus, ")")

    # Build subplot subtitle (only for single CU species)
    plot_subtitle <- if (is_single_cu) cu_name else NULL

    p <- ggplot(data_sp, aes(x = indicator, y = id_label)) +
      geom_tile(aes(fill = value_normalized), color = "white", linewidth = 0.3) +
      geom_text(aes(label = sprintf("%.1f", value)),
        size = tile_text_size,
        color = "black") +
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

    # Add subtitle for single CU species
    if (!is.null(plot_subtitle)) {
      p <- p +
        labs(subtitle = plot_subtitle) +
        theme(
          plot.subtitle = element_text(size = 8, hjust = 0, margin = margin(b = 2),
            color = plot_colours[species])
        )
    }

    # Handle y-axis text
    if (is_single_cu) {
      p <- p + theme(axis.text.y = element_blank())
    } else {
      p <- p + theme(
        axis.text.y = element_markdown(size = y_text_size, hjust = 1, margin = margin(r = 2))
      )
    }

    # Add vertical line separators before overall score columns
    if (!is.null(overall_score_cols) && length(overall_score_cols) > 0) {
      all_indicators <- levels(data_sp$indicator)

      # Find the position where the first overall score column starts
      first_overall_idx <- which(all_indicators %in% overall_score_cols)[1]

      if (!is.na(first_overall_idx)) {
        # Add separator before the first overall score column
        separator_x <- first_overall_idx - 0.5

        p <- p +
          geom_vline(xintercept = separator_x,
            color = "black",
            linewidth = 1,
            linetype = "solid")

        # If there are multiple overall score columns, add separators between them
        if (length(overall_score_cols) > 1) {
          overall_positions <- which(all_indicators %in% overall_score_cols)

          # Add separators between consecutive overall score columns
          for (i in 1:(length(overall_positions) - 1)) {
            separator_x <- overall_positions[i] + 0.5

            p <- p +
              geom_vline(xintercept = separator_x,
                color = "grey50",
                linewidth = 0.5,
                linetype = "dashed")
          }
        }
      }
    }

    # Show x-axis labels on bottom row
    if (is_bottom_row) {
      p <- p + theme(
        axis.text.x = element_text(size = 8, angle = 45, hjust = 1, vjust = 1, margin = margin(t = 2))
      )
    } else {
      p <- p + theme(
        axis.text.x = element_blank()
      )
    }

    return(p)
  })

  # Create indicator legend/key table
  indicators_in_plot <- indicator_order

  # Match with metadata
  legend_data <- indicators_metadata %>%
    filter(abbrev %in% indicators_in_plot) %>%
    select(abbrev, name) %>%
    mutate(abbrev = factor(abbrev, levels = indicators_in_plot)) %>%
    arrange(abbrev) %>%
    rename(Code = abbrev, Description = name)

  # Add overall scores if they're not in the indicators metadata
  if (!is.null(overall_score_cols)) {
    for (score_col in overall_score_cols) {
      if (!(score_col %in% legend_data$Code)) {
        overall_desc <- case_when(
          score_col == "std_sumavgs" ~ "Sum of Category Averages",
          score_col == "std_avgall" ~ "Average of All Indicators",
          score_col == "std_sumcube" ~ "Sum of Cubic Means",
          score_col == "std_cubefwR" ~ "Cubic Mean - Freshwater Rearing",
          score_col == "std_avgfwR" ~ "Average - Freshwater Rearing",
          score_col == "std_cubemigr" ~ "Cubic Mean - Migration",
          score_col == "std_avgmigr" ~ "Average - Migration",
          score_col == "std_cubedem" ~ "Cubic Mean - Demographics",
          score_col == "std_avgdem" ~ "Average - Demographics",
          score_col == "std_cubemar" ~ "Cubic Mean - Marine",
          score_col == "std_avgmar" ~ "Average - Marine",
          score_col == "std_cubegen" ~ "Cubic Mean - Genetics",
          score_col == "std_avggen" ~ "Average - Genetics",
          TRUE ~ paste("Overall Score:", score_col)
        )
        legend_data <- bind_rows(
          legend_data,
          data.frame(Code = score_col, Description = overall_desc)
        )
      }
    }
  }

  # Reorder legend data to match indicator order
  legend_data <- legend_data %>%
    mutate(Code = factor(Code, levels = indicators_in_plot)) %>%
    arrange(Code) %>%
    mutate(Code = as.character(Code))

  # Create a grob (graphical object) for the legend
  legend_grob <- tableGrob(
    legend_data,
    rows = NULL,
    theme = ttheme_minimal(
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

  # Wrap the grob in a ggplot for patchwork compatibility
  legend_plot <- ggplot() +
    annotation_custom(legend_grob) +
    theme_void() +
    labs(title = "Indicator Key") +
    theme(
      plot.title = element_text(face = "bold", size = 10, hjust = 0, margin = margin(b = 5)),
      plot.margin = margin(2, 2, 2, 2)
    )

  # Add legend to plot list
  plot_list <- c(plot_list, list(legend_plot))

  # Combine with patchwork
  n_rows <- ceiling((n_plots + 1) / ncol)

  if (!uniform_height) {
    height_matrix <- matrix(c(heights, 0.5), nrow = n_rows, ncol = ncol, byrow = TRUE)
    row_heights <- apply(height_matrix, 1, max, na.rm = TRUE)

    p <- wrap_plots(plot_list, ncol = ncol) +
      plot_layout(heights = row_heights)
  } else {
    heights_with_legend <- c(rep(1, n_plots), 0.5)
    p <- wrap_plots(plot_list, ncol = ncol) +
      plot_layout(heights = heights_with_legend)
  }

  # Add overall title and category name if provided
  if (!is.null(category_name)) {
    p <- p +
      plot_annotation(
        title = category_name,
        theme = theme(
          plot.title = element_text(face = "bold", size = 12, hjust = 0.5, margin = margin(b = 10)),
          plot.margin = margin(5, 5, 5, 5),
          plot.background = element_rect(fill = "white", color = NA)
        )
      )
  } else {
    p <- p +
      plot_annotation(
        theme = theme(
          plot.margin = margin(5, 5, 5, 5),
          plot.background = element_rect(fill = "white", color = NA)
        )
      )
  }

  return(p)
}



# 6. Correlation analysis and plots ---------------------------------------

get_correlation_matrix <- function(data,
                                   indicators_choose = tbl_indicators$abbrev,
                                   use_standardized = T) {

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

  # --- remove "std" from column names
  new_names <- numeric_cols %>%
    stringr::str_remove("(?i)^std_")
  names(cor_data) <- new_names

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


################################################################################
#
# species_category_tile_plot.R
#
# Function to visualize indicator scores by category with within-species
# vulnerability rankings. Creates separate panels for each indicator category
# plus overall scores, with each panel showing only one species at a time.
#
################################################################################

#' Create tile plots showing all indicators by category for a single species
#'
#' This function creates a multi-panel visualization where each panel represents
#' an indicator category and shows ALL indicators within that category plus the
#' category average. The final panel shows overall vulnerability scores.
#' CUs are ranked by their within-species vulnerability.
#'
#' @param data Data frame containing standardized indicator values (from combined_scores_std)
#' @param species_pick Character string of species to plot (e.g., "Chinook", "Sockeye")
#' @param cu_name_col Character string specifying column name to use for CU labels (default "CVIS_NAME")
#' @param indicators_metadata Metadata table for indicators (tbl_indicators)
#' @param species_palette Named vector of colors for species (default uses species_palette)
#' @param brewer_palette Color palette name for tiles (default "RdYlGn")
#' @param palette_direction Direction of palette, 1 or -1 (default -1)
#' @param rank_method Method for ranking: "sumavgs", "avgall", or "sumcube" (default "sumavgs")
#' @param show_values Logical, whether to show numeric values on tiles (default TRUE)
#' @param sort_by_rank Logical, whether to sort CUs by rank (default TRUE)
#'
#' @return A patchwork object containing the multi-panel plot
#'
#' @examples
#' # Plot Chinook salmon indicators using default CVIS_NAME
#' p <- species_category_tile_plot(
#'   combined_scores_std,
#'   species_pick = "Chinook",
#'   indicators_metadata = tbl_indicators,
#'   rank_method = "sumavgs"
#' )
#'
#' # Plot using FULL_CU_IN instead
#' p <- species_category_tile_plot(
#'   combined_scores_std,
#'   species_pick = "Chinook",
#'   cu_name_col = "FULL_CU_IN",
#'   indicators_metadata = tbl_indicators
#' )
#' print(p)
#'
species_category_tile_plot <- function(data,
                                       species_pick,
                                       cu_name_col = "CVIS_NAME",
                                       indicators_metadata = tbl_indicators,
                                       species_palette = NULL,
                                       brewer_palette = "RdYlGn",
                                       palette_direction = -1,
                                       rank_method = "sumavgs",
                                       show_values = TRUE,
                                       sort_by_rank = TRUE) {
  # Use default species palette if not provided
  if (is.null(species_palette)) {
    species_palette <- c(
      "Chinook" = "#E69F00",
      "Chum" = "#56B4E9",
      "Coho" = "#009E73",
      "Pink" = "#F0E442",
      "Sockeye" = "#D55E00"
    )
  }

  # Filter to selected species
  data_sp <- data %>%
    filter(SPECIES_NAME == species_pick)

  if (nrow(data_sp) == 0) {
    stop(paste("No data found for species:", species_pick))
  }

  # Check if cu_name_col exists in data
  if (!cu_name_col %in% names(data_sp)) {
    stop(paste("Column", cu_name_col, "not found in data"))
  }

  # Get species color
  species_color <- species_palette[species_pick]
  if (is.na(species_color)) {
    species_color <- "black"  # fallback color
  }

  # Determine rank column to use
  rank_col <- paste0("std_rank_", rank_method, "_within")

  if (!rank_col %in% names(data_sp)) {
    stop(paste("Rank column", rank_col, "not found in data"))
  }

  # Sort by rank if requested
  if (sort_by_rank) {
    data_sp <- data_sp %>%
      arrange(!!sym(rank_col))
  }

  # Define category groupings with ALL indicators in each category
  category_list <- list(
    list(
      name = "Freshwater Rearing",
      indicators = c(
        indicators_metadata$abbrev[indicators_metadata$type == "fwR"],
        "avgfwR"
      )
    ),
    list(
      name = "Upstream Migration",
      indicators = c(
        indicators_metadata$abbrev[indicators_metadata$type == "migr"],
        "avgmigr"
      )
    ),
    list(
      name = "Marine",
      indicators = c(
        indicators_metadata$abbrev[indicators_metadata$type == "mar"],
        "avgmar"
      )
    ),
    list(
      name = "Demographics",
      indicators = c(
        indicators_metadata$abbrev[indicators_metadata$type == "dem"],
        "avgdem"
      )
    ),
    list(
      name = "Genetics",
      indicators = c(
        indicators_metadata$abbrev[indicators_metadata$type == "gen"],
        "avggen"
      )
    ),
    list(
      name = "Overall Vulnerability",
      indicators = c("sumavgs", "avgall", "sumcube"),
      is_overall = TRUE
    )
  )

  # Create individual plots for each category
  plot_list <- list()

  for (cat_info in category_list) {
    cat_name <- cat_info$name
    indicators_use <- cat_info$indicators
    is_overall <- !is.null(cat_info$is_overall) && cat_info$is_overall

    # Build column names with std_ prefix
    # Some indicators have _mean suffix, others don't
    indicators_std <- paste0("std_", indicators_use)
    indicators_std_mean <- paste0("std_", indicators_use, "_mean")

    # Find which indicators are available in the data (try both versions)
    indicators_available <- c()
    for (i in seq_along(indicators_use)) {
      if (indicators_std[i] %in% names(data_sp)) {
        indicators_available <- c(indicators_available, indicators_std[i])
      } else if (indicators_std_mean[i] %in% names(data_sp)) {
        indicators_available <- c(indicators_available, indicators_std_mean[i])
      }
    }

    if (length(indicators_available) == 0) {
      next  # Skip this category if no indicators available
    }

    # Prepare plot data
    plot_data <- data_sp %>%
      select(!!sym(cu_name_col), all_of(indicators_available)) %>%
      pivot_longer(
        cols = all_of(indicators_available),
        names_to = "indicator",
        values_to = "value"
      ) %>%
      mutate(indicator = str_remove(indicator, "std_")) %>%
      mutate(indicator = str_remove(indicator, "_mean$"))

    # Calculate normalized values for consistent coloring across all indicators
    indicator_limits <- plot_data %>%
      group_by(indicator) %>%
      summarise(
        min_val = min(value, na.rm = TRUE),
        max_val = max(value, na.rm = TRUE),
        .groups = "drop"
      )

    plot_data <- plot_data %>%
      left_join(indicator_limits, by = "indicator") %>%
      mutate(value_norm = if_else(
        max_val > min_val,
        (value - min_val) / (max_val - min_val),
        0.5
      ))

    # Create colored labels for CU names
    plot_data <- plot_data %>%
      mutate(cu_name_label = paste0(
        "<span style='color:", species_color, "'>",
        !!sym(cu_name_col), "</span>"
      ))

    # Reorder CUs by rank (already sorted in data_sp)
    # Get the original order from data_sp
    cu_order <- data_sp %>%
      pull(!!sym(cu_name_col))

    # Create colored labels in the same order
    cu_labels_ordered <- paste0(
      "<span style='color:", species_color, "'>",
      cu_order, "</span>"
    )

    plot_data <- plot_data %>%
      mutate(cu_name_label = factor(cu_name_label,
        levels = cu_labels_ordered))

    # Reorder indicators to put category average last
    avg_indicators <- grep("^avg|^sum", plot_data$indicator, value = TRUE)
    other_indicators <- setdiff(unique(plot_data$indicator), avg_indicators)
    indicator_order <- unique(c(other_indicators, avg_indicators))

    plot_data <- plot_data %>%
      mutate(indicator = factor(indicator, levels = indicator_order))

    # Determine text size based on number of CUs
    n_cus <- length(unique(plot_data %>% pull(!!sym(cu_name_col))))
    y_text_size <- if (n_cus > 15) 6 else if (n_cus > 10) 7 else 8
    tile_text_size <- if (n_cus > 15) 1.5 else if (n_cus > 10) 2 else 2.5

    # Create the plot
    p <- ggplot(plot_data, aes(x = indicator, y = cu_name_label)) +
      geom_tile(aes(fill = value_norm), color = "white", linewidth = 0.3) +
      scale_fill_distiller(
        palette = brewer_palette,
        direction = palette_direction,
        na.value = "grey95",
        limits = c(0, 1)
      ) +
      theme_minimal(base_size = 9) +
      theme(
        axis.title = element_blank(),
        axis.text.y = element_markdown(size = y_text_size, hjust = 1),
        axis.text.x = element_text(size = 7, angle = 45, hjust = 1, vjust = 1),
        legend.position = "none",
        panel.grid = element_blank(),
        panel.border = element_rect(color = "grey70", fill = NA, linewidth = 0.5),
        plot.title = element_text(face = "bold", size = 10, hjust = 0.5),
        plot.margin = margin(2, 2, 2, 2)
      ) +
      labs(title = cat_name)

    # Add vertical separator before category average/overall scores
    if (length(avg_indicators) > 0) {
      # Find position of first average indicator
      separator_x <- match(avg_indicators[1], levels(plot_data$indicator)) - 0.5

      p <- p +
        geom_vline(xintercept = separator_x,
          color = "black",
          linewidth = 0.8,
          linetype = "solid")
    }

    # Add text labels
    if (show_values) {
      p <- p + geom_text(
        aes(label = sprintf("%.2f", value)),
        size = tile_text_size,
        color = "black"
      )
    }

    plot_list[[cat_name]] <- p
  }

  # Combine plots with patchwork
  n_plots <- length(plot_list)

  if (n_plots == 0) {
    stop("No plots generated - check that indicators are available in data")
  }

  # Arrange in 2 columns with tighter spacing
  ncol_layout <- 2

  combined_plot <- wrap_plots(plot_list, ncol = ncol_layout) +
    plot_annotation(
      # title = paste("Climate Vulnerability Indicators:", species_pick),
      theme = theme(
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        plot.margin = margin(5, 5, 5, 5)
      )
    ) +
    plot_layout(guides = "collect")

  return(combined_plot)
}


# Example usage (commented out):
#
# Single species plot
# chinook_plot <- species_category_tile_plot(
#   data = combined_scores_std %>% filter(rcp == "45", period_code == "3"),
#   species_pick = "Chinook",
#   species_palette = species_palette,
#   indicators_metadata = tbl_indicators,
#   rank_method = "sumavgs",
#   sort_by_rank = TRUE
# )
#
# print(chinook_plot)


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
