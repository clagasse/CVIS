################################################################################
#
# 4b_species_rank_comparison_analysis.R
#
# Functions for analyzing and visualizing differences between cross-species
# and within-species vulnerability rankings
#
################################################################################

library(tidyverse)
library(patchwork)
library(ggrepel)

#------------------------------------------------------------------------------
# Analysis Functions
#------------------------------------------------------------------------------

#' Compare cross-species vs within-species ranks
#'
#' @param data Data frame with both cross and within-species ranks
#' @param score_method Which aggregation method to use ("addall", "avgall", "sumavgs")
#' @param species_col Column name for species
#'
#' @return Data frame with comparison statistics
#'
compare_species_ranks <- function(data,
                                  score_method = "addall",
                                  species_col = "SPECIES_NAME") {

  cross_rank_col <- paste0("std_rank_", score_method, "_cross")
  within_rank_col <- paste0("std_rank_", score_method, "_within")
  diff_col <- paste0("rank_diff_", score_method)

  comparison <- data %>%
    group_by(.data[[species_col]]) %>%
    summarize(
      n_CUs = n(),
      mean_rank_cross = mean(.data[[cross_rank_col]], na.rm = TRUE),
      mean_rank_within = mean(.data[[within_rank_col]], na.rm = TRUE),
      mean_abs_diff = mean(abs(.data[[diff_col]]), na.rm = TRUE),
      max_abs_diff = max(abs(.data[[diff_col]]), na.rm = TRUE),
      median_abs_diff = median(abs(.data[[diff_col]]), na.rm = TRUE),
      n_large_diffs = sum(abs(.data[[diff_col]]) >= 5, na.rm = TRUE),
      pct_large_diffs = 100 * n_large_diffs / n_CUs,
      .groups = "drop"
    )

  return(comparison)
}


#' Identify CUs with concordant vs discordant rankings
#'
#' @param data Data frame with both ranking systems
#' @param score_method Aggregation method
#' @param threshold Number of rank positions defining "large" difference
#'
#' @return List with concordant and discordant CUs
#'
identify_ranking_concordance <- function(data,
                                         score_method = "addall",
                                         threshold = 5) {

  diff_col <- paste0("rank_diff_", score_method)

  concordant <- data %>%
    filter(abs(.data[[diff_col]]) < threshold) %>%
    arrange(.data[[diff_col]])

  discordant <- data %>%
    filter(abs(.data[[diff_col]]) >= threshold) %>%
    arrange(desc(abs(.data[[diff_col]])))

  # Categorize discordant cases
  higher_cross <- discordant %>%
    filter(.data[[diff_col]] < 0)  # Ranked higher in cross-species

  higher_within <- discordant %>%
    filter(.data[[diff_col]] > 0)  # Ranked higher in within-species

  return(list(
    concordant = concordant,
    discordant = discordant,
    higher_in_cross_species = higher_cross,
    higher_in_within_species = higher_within,
    n_concordant = nrow(concordant),
    n_discordant = nrow(discordant),
    pct_concordant = 100 * nrow(concordant) / nrow(data)
  ))
}


#' Calculate Spearman correlation between ranking systems
#'
#' @param data Data frame with rankings
#' @param score_method Aggregation method
#'
#' @return Correlation coefficient
#'
calculate_rank_correlation <- function(data,
                                       score_method = "addall") {

  cross_rank_col <- paste0("std_rank_", score_method, "_cross")
  within_rank_col <- paste0("std_rank_", score_method, "_within")

  cor(
    data[[cross_rank_col]],
    data[[within_rank_col]],
    method = "spearman",
    use = "complete.obs"
  )
}


#------------------------------------------------------------------------------
# Visualization Functions
#------------------------------------------------------------------------------

#' Scatter plot comparing cross-species vs within-species ranks
#'
#' @param data Data frame with rankings
#' @param score_method Aggregation method
#' @param species_col Species column name
#' @param name_col CU name column
#' @param highlight_cus Vector of CU IDs to highlight
#'
#' @return ggplot object
#'
plot_rank_comparison_scatter <- function(data,
                                         score_method = "addall",
                                         species_col = "SPECIES_NAME",
                                         name_col = "CVIS_NAME",
                                         highlight_cus = NULL) {

  cross_rank_col <- paste0("std_rank_", score_method, "_cross")
  within_rank_col <- paste0("std_rank_", score_method, "_within")

  # Calculate correlation
  cor_value <- calculate_rank_correlation(data, score_method)

  p <- ggplot(data, aes(x = .data[[within_rank_col]],
    y = .data[[cross_rank_col]],
    color = .data[[species_col]])) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed",
      color = "gray50", linewidth = 1) +
    geom_point(alpha = 0.7, size = 3) +
    labs(
      title = "Cross-Species vs Within-Species Vulnerability Rankings",
      subtitle = paste0("Spearman correlation: ", round(cor_value, 3)),
      x = "Within-Species Rank\n(higher = more vulnerable within species)",
      y = "Cross-Species Rank\n(higher = more vulnerable overall)",
      color = "Species"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      legend.position = "right"
    )

  # Highlight specific CUs if requested
  if (!is.null(highlight_cus)) {
    highlight_data <- data %>%
      filter(FULL_CU_IN %in% highlight_cus)

    p <- p +
      geom_point(data = highlight_data, size = 5, shape = 1,
        color = "black", stroke = 2) +
      ggrepel::geom_text_repel(
        data = highlight_data,
        aes(label = FULL_CU_IN),
        color = "black",
        size = 3,
        max.overlaps = 20
      )
  }

  return(p)
}


#' Plot rank differences by species
#'
#' @param data Data frame with rankings
#' @param score_method Aggregation method
#' @param species_col Species column name
#'
#' @return ggplot object
#'
plot_rank_differences_by_species <- function(data,
                                             score_method = "addall",
                                             species_col = "SPECIES_NAME") {

  diff_col <- paste0("rank_diff_", score_method)

  ggplot(data, aes(x = .data[[species_col]], y = .data[[diff_col]],
    fill = .data[[species_col]])) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_boxplot(alpha = 0.7) +
    geom_jitter(width = 0.2, alpha = 0.5, size = 2) +
    labs(
      title = "Rank Differences by Species",
      subtitle = "Positive = ranked higher (more vulnerable) in within-species comparison",
      x = "Species",
      y = "Rank Difference\n(Cross-Species Rank - Within-Species Rank)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      legend.position = "none",
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}


#' Heatmap showing rank changes for all CUs
#'
#' @param data Data frame with rankings
#' @param score_method Aggregation method
#' @param species_col Species column name
#' @param name_col CU name column
#'
#' @return ggplot object
#'
plot_rank_difference_heatmap <- function(data,
                                         score_method = "addall",
                                         species_col = "SPECIES_NAME",
                                         name_col = "CVIS_NAME") {

  diff_col <- paste0("rank_diff_", score_method)

  # Order by species and rank difference
  data_plot <- data %>%
    arrange(.data[[species_col]], .data[[diff_col]]) %>%
    mutate(cu_order = factor(.data[[name_col]], levels = .data[[name_col]]))

  ggplot(data_plot, aes(x = "Rank Difference", y = cu_order,
    fill = .data[[diff_col]])) +
    geom_tile(color = "white") +
    scale_fill_gradient2(
      low = "blue", mid = "white", high = "red",
      midpoint = 0,
      name = "Rank\nDifference"
    ) +
    facet_grid(rows = vars(.data[[species_col]]), scales = "free_y", space = "free_y") +
    labs(
      title = "Cross-Species vs Within-Species Rank Differences",
      subtitle = "Red = more vulnerable in within-species ranking",
      x = "",
      y = "Conservation Unit"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      axis.text.y = element_text(size = 6),
      axis.text.x = element_blank(),
      strip.text.y = element_text(angle = 0, face = "bold")
    )
}


#' Plot showing which CUs change priority ranking
#'
#' @param data Data frame with rankings
#' @param score_method Aggregation method
#' @param top_n Number of top-ranked CUs to compare
#' @param species_col Species column name
#'
#' @return ggplot object
#'
plot_priority_changes <- function(data,
                                  score_method = "addall",
                                  top_n = 10,
                                  species_col = "SPECIES_NAME") {

  cross_rank_col <- paste0("std_rank_", score_method, "_cross")
  within_rank_col <- paste0("std_rank_", score_method, "_within")

  # Get top N in each ranking system
  top_cross <- data %>%
    filter(.data[[cross_rank_col]] <= top_n) %>%
    mutate(rank_system = "Cross-Species Top 10")

  top_within_by_species <- data %>%
    group_by(.data[[species_col]]) %>%
    filter(.data[[within_rank_col]] <= 3) %>%  # Top 3 per species
    ungroup() %>%
    mutate(rank_system = "Within-Species Top 3")

  # Combine and identify overlap
  priority_data <- bind_rows(top_cross, top_within_by_species) %>%
    distinct(FULL_CU_IN, .keep_all = TRUE) %>%
    mutate(
      in_both = FULL_CU_IN %in% intersect(top_cross$FULL_CU_IN,
        top_within_by_species$FULL_CU_IN)
    )

  ggplot(priority_data, aes(x = .data[[within_rank_col]],
    y = .data[[cross_rank_col]],
    color = .data[[species_col]],
    shape = in_both)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
    geom_point(size = 4, alpha = 0.8) +
    ggrepel::geom_text_repel(
      aes(label = FULL_CU_IN),
      size = 3,
      max.overlaps = 30
    ) +
    scale_shape_manual(
      values = c(16, 17),
      labels = c("Priority in one system only", "Priority in both systems")
    ) +
    labs(
      title = "Priority Conservation Units: Ranking System Comparison",
      subtitle = paste0("Cross-species top ", top_n, " vs within-species top 3"),
      x = "Within-Species Rank",
      y = "Cross-Species Rank",
      color = "Species",
      shape = ""
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      legend.position = "right"
    )
}


#' Multi-panel comparison across all aggregation methods
#'
#' @param data Data frame with rankings
#' @param species_col Species column name
#'
#' @return patchwork object with multiple plots
#'
plot_multimethod_comparison <- function(data,
                                        species_col = "SPECIES_NAME") {

  methods <- c("addall", "avgall", "sumavgs")

  plots <- map(methods, function(method) {
    plot_rank_comparison_scatter(data, score_method = method, species_col = species_col) +
      labs(title = paste("Method:", method))
  })

  wrap_plots(plots, ncol = 2) +
    plot_annotation(
      title = "Ranking Comparison Across Aggregation Methods",
      theme = theme(plot.title = element_text(face = "bold", size = 16))
    )
}


#' Summary table of ranking concordance
#'
#' @param data Data frame with rankings
#' @param score_methods Vector of aggregation methods to compare
#'
#' @return Data frame with summary statistics
#'
create_concordance_table <- function(data,
                                     score_methods = c("addall", "avgall", "sumavgs")) {

  map_dfr(score_methods, function(method) {
    concordance <- identify_ranking_concordance(data, score_method = method)
    correlation <- calculate_rank_correlation(data, score_method = method)

    tibble(
      aggregation_method = method,
      n_CUs = nrow(data),
      n_concordant = concordance$n_concordant,
      n_discordant = concordance$n_discordant,
      pct_concordant = round(concordance$pct_concordant, 1),
      spearman_correlation = round(correlation, 3)
    )
  })
}


#------------------------------------------------------------------------------
# Export Functions
#------------------------------------------------------------------------------

#' Export species ranking comparison results
#'
#' @param data Data frame with rankings
#' @param output_dir Directory to save results
#' @param file_prefix Prefix for output files
#'
export_species_comparison_results <- function(data,
                                              output_dir = "outputs/species_comparison",
                                              file_prefix = "species_rank") {

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  timestamp <- format(Sys.Date(), "%Y-%m-%d")

  # Export concordance summary
  concordance_summary <- create_concordance_table(data)
  write.csv(
    concordance_summary,
    file.path(output_dir, paste0(file_prefix, "_concordance_summary_", timestamp, ".csv")),
    row.names = FALSE
  )

  # Export detailed comparison by species
  species_comparison <- compare_species_ranks(data)
  write.csv(
    species_comparison,
    file.path(output_dir, paste0(file_prefix, "_by_species_", timestamp, ".csv")),
    row.names = FALSE
  )

  # Export discordant CUs
  discordant <- identify_ranking_concordance(data, threshold = 5)
  write.csv(
    discordant$discordant,
    file.path(output_dir, paste0(file_prefix, "_discordant_CUs_", timestamp, ".csv")),
    row.names = FALSE
  )

  # Save plots
  ggsave(
    filename = file.path(output_dir, paste0(file_prefix, "_scatter_", timestamp, ".png")),
    plot = plot_rank_comparison_scatter(data),
    width = 10, height = 8, dpi = 300
  )

  ggsave(
    filename = file.path(output_dir, paste0(file_prefix, "_by_species_", timestamp, ".png")),
    plot = plot_rank_differences_by_species(data),
    width = 10, height = 6, dpi = 300
  )

  ggsave(
    filename = file.path(output_dir, paste0(file_prefix, "_heatmap_", timestamp, ".png")),
    plot = plot_rank_difference_heatmap(data),
    width = 8, height = 12, dpi = 300
  )

  ggsave(
    filename = file.path(output_dir, paste0(file_prefix, "_priorities_", timestamp, ".png")),
    plot = plot_priority_changes(data),
    width = 12, height = 10, dpi = 300
  )

  cat("\nSpecies comparison results exported to:", output_dir, "\n")
}


#------------------------------------------------------------------------------
# Example Usage
#------------------------------------------------------------------------------

# Load data with species ranks
# combined_scores_std <- read.csv("path/to/vulnerability_scores_with_species_ranks.csv")

# Filter to specific scenario
data_filtered <- combined_scores_std %>%
  filter(rcp == "45", period_code == 3)

# Compare ranking systems
species_comparison <- compare_species_ranks(data_filtered, score_method = "addall")
print(species_comparison)

# Identify concordant and discordant CUs
concordance <- identify_ranking_concordance(data_filtered, threshold = 5)
cat("Concordant CUs:", concordance$n_concordant,
  "(", round(concordance$pct_concordant, 1), "%)\n")

# Create visualizations
plot_rank_comparison_scatter(data_filtered)
plot_rank_differences_by_species(data_filtered)
plot_rank_difference_heatmap(data_filtered)
plot_priority_changes(data_filtered, top_n = 10)

# Multi-method comparison
plot_multimethod_comparison(data_filtered)

# Create summary table
concordance_table <- create_concordance_table(data_filtered)
print(concordance_table)

# Export everything
export_species_comparison_results(
  data_filtered,
  output_dir = "outputs/species_comparison",
  file_prefix = "species_rank_rcp45_midcentury"
)
