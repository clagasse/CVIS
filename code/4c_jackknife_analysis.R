################################################################################
#
# jackknife_analysis.R
#
# Jackknife (leave-one-out) sensitivity analysis for vulnerability rankings
#
# This script systematically removes one indicator at a time and recalculates
# vulnerability scores and ranks to assess:
#   1. Which indicators most strongly influence overall rankings
#   2. Whether any CU ranks are driven by a single indicator
#   3. Robustness of rankings to indicator choice
#
# Author: [Your name]
# Date: 2025-11-03
#
################################################################################

library(tidyverse)
library(here)

# If not already loaded, source your setup and utility functions
# setwd(here())
# source(file.path(here(), "code", "0_setup.R"))

#------------------------------------------------------------------------------
# Main Jackknife Function
#------------------------------------------------------------------------------

#' Perform jackknife (leave-one-out) analysis on vulnerability indicators
#'
#' @param data Data frame containing standardized indicator values
#' @param indicators_choose Character vector of indicator abbreviations to include
#' @param aggregation_method Method for combining indicators: "additive", "average", or "sumavgs"
#' @param group_cols Columns to group by when ranking (e.g., c("rcp", "period_code"))
#' @param id_col Name of the CU identifier column
#' @param name_col Name of the CU name column (optional)
#'
#' @return List containing:
#'   - full_ranks: Rankings using all indicators
#'   - jackknife_ranks: Rankings for each leave-one-out iteration
#'   - rank_changes: Summary of rank changes when each indicator is removed
#'   - correlation_matrix: Spearman correlations between full and jackknife ranks
#'   - influence_summary: Summary statistics of indicator influence
#'
jackknife_vulnerability_analysis <- function(data,
                                             indicators_choose = tbl_indicators$abbrev,
                                             aggregation_method = "additive",
                                             group_cols = c("rcp", "period_code"),
                                             id_col = "FULL_CU_IN",
                                             name_col = "CVIS_NAME") {

  cat("\n========================================\n")
  cat("Jackknife Vulnerability Analysis\n")
  cat("========================================\n\n")

  # Check which indicator columns actually exist
  std_indicators <- paste0("std_", indicators_choose)
  available_cols <- c()
  for (ind in std_indicators) {
    if (ind %in% names(data)) {
      available_cols <- c(available_cols, ind)
    } else {
      ind_mean <- paste0(ind, "_mean")
      if (ind_mean %in% names(data)) {
        available_cols <- c(available_cols, ind_mean)
      }
    }
  }

  # Map back to indicator abbreviations for analysis
  available_indicators <- indicators_choose[sapply(std_indicators, function(x) {
    x %in% names(data) || paste0(x, "_mean") %in% names(data)
  })]

  cat("Total indicators requested:", length(indicators_choose), "\n")
  cat("Indicators found in data:", length(available_indicators), "\n")
  cat("Aggregation method:", aggregation_method, "\n")
  cat("Number of CUs:", length(unique(data[[id_col]])), "\n\n")

  if (length(available_indicators) < 2) {
    stop("Need at least 2 indicators for jackknife analysis")
  }

  # Step 1: Calculate full model ranks (using all indicators)
  cat("Step 1: Calculating baseline ranks with all", length(available_indicators), "indicators...\n")
  full_ranks <- calculate_vulnerability_ranks(
    data = data,
    indicators_choose = available_indicators,
    aggregation_method = aggregation_method,
    group_cols = group_cols,
    id_col = id_col,
    name_col = name_col
  )

  # Initialize results storage
  jackknife_results <- list()
  rank_changes_list <- list()
  correlations <- numeric(length(available_indicators))
  names(correlations) <- available_indicators

  # Step 2: Iteratively remove each indicator and recalculate ranks
  cat("\nStep 2: Running leave-one-out iterations...\n")
  pb <- txtProgressBar(min = 0, max = length(available_indicators), style = 3)

  for (i in seq_along(available_indicators)) {
    # Create indicator set with one removed
    indicators_minus_one <- available_indicators[-i]
    removed_indicator <- available_indicators[i]

    # Calculate ranks without this indicator
    jackknife_ranks <- calculate_vulnerability_ranks(
      data = data,
      indicators_choose = indicators_minus_one,
      aggregation_method = aggregation_method,
      group_cols = group_cols,
      id_col = id_col,
      name_col = name_col
    )

    # Store results
    jackknife_results[[removed_indicator]] <- jackknife_ranks

    # Calculate rank changes
    rank_comparison <- compare_ranks(
      full_ranks = full_ranks,
      jackknife_ranks = jackknife_ranks,
      removed_indicator = removed_indicator,
      id_col = id_col,
      name_col = name_col,
      group_cols = group_cols
    )

    rank_changes_list[[removed_indicator]] <- rank_comparison

    # Calculate Spearman correlation
    correlations[i] <- cor(
      full_ranks$rank,
      jackknife_ranks$rank,
      method = "spearman",
      use = "complete.obs"
    )

    setTxtProgressBar(pb, i)
  }
  close(pb)

  # Step 3: Summarize results
  cat("\nStep 3: Summarizing jackknife results...\n")

  # Combine all rank changes into single data frame
  all_rank_changes <- bind_rows(rank_changes_list, .id = "removed_indicator")

  # Calculate influence metrics for each indicator
  influence_summary <- all_rank_changes %>%
    group_by(removed_indicator) %>%
    summarise(
      mean_abs_rank_change = mean(abs(rank_change), na.rm = TRUE),
      median_abs_rank_change = median(abs(rank_change), na.rm = TRUE),
      max_abs_rank_change = max(abs(rank_change), na.rm = TRUE),
      sd_rank_change = sd(rank_change, na.rm = TRUE),
      n_CUs_affected = sum(abs(rank_change) > 0, na.rm = TRUE),
      pct_CUs_affected = 100 * n_CUs_affected / n(),
      n_large_changes = sum(abs(rank_change) >= 5, na.rm = TRUE),
      spearman_correlation = correlations[removed_indicator],
      .groups = "drop"
    ) %>%
    arrange(desc(mean_abs_rank_change))

  # Step 4: Identify most influential indicators
  cat("\n========================================\n")
  cat("Most Influential Indicators (by mean absolute rank change):\n")
  cat("========================================\n")
  print(head(influence_summary, 10))

  # Return comprehensive results
  results <- list(
    full_ranks = full_ranks,
    jackknife_ranks = jackknife_results,
    rank_changes = all_rank_changes,
    influence_summary = influence_summary,
    correlations = correlations,
    metadata = list(
      n_indicators = length(available_indicators),
      indicators = available_indicators,
      aggregation_method = aggregation_method,
      n_CUs = length(unique(data[[id_col]])),
      date_run = Sys.Date()
    )
  )

  cat("\nJackknife analysis complete!\n\n")

  return(results)
}


#------------------------------------------------------------------------------
# Helper Function: Calculate Vulnerability Ranks
#------------------------------------------------------------------------------

#' Calculate vulnerability scores and ranks for a given set of indicators
#'
#' @param data Data frame with standardized indicators
#' @param indicators_choose Vector of indicator abbreviations
#' @param aggregation_method "additive", "average", or "sumavgs"
#' @param group_cols Grouping columns for ranking
#' @param id_col CU identifier column
#' @param name_col CU name column
#'
#' @return Data frame with scores and ranks
#'
calculate_vulnerability_ranks <- function(data,
                                          indicators_choose,
                                          aggregation_method = "additive",
                                          group_cols = c("rcp", "period_code"),
                                          id_col = "FULL_CU_IN",
                                          name_col = "CVIS_NAME") {
  # Prepare indicator column names (standardized)
  std_indicators <- paste0("std_", indicators_choose)

  # Find which columns actually exist
  available_cols <- c()
  for (ind in std_indicators) {
    # Try exact match first
    if (ind %in% names(data)) {
      available_cols <- c(available_cols, ind)
    } else {
      # Try with _mean suffix
      ind_mean <- paste0(ind, "_mean")
      if (ind_mean %in% names(data)) {
        available_cols <- c(available_cols, ind_mean)
      }
    }
  }

  if (length(available_cols) == 0) {
    stop("No indicator columns found in data")
  }

  # Keep ID, grouping, name, and available indicator columns
  cols_to_keep <- c(id_col, name_col, group_cols, available_cols)
  cols_to_keep <- cols_to_keep[cols_to_keep %in% names(data)]

  data_subset <- data %>%
    select(all_of(cols_to_keep))

  # Calculate composite vulnerability score
  if (aggregation_method == "additive") {
    # Sum of all standardized indicators
    data_scored <- data_subset %>%
      rowwise() %>%
      mutate(vuln_score = sum(c_across(all_of(available_cols)), na.rm = TRUE)) %>%
      ungroup()

  } else if (aggregation_method == "average") {
    # Average of all standardized indicators
    data_scored <- data_subset %>%
      rowwise() %>%
      mutate(vuln_score = mean(c_across(all_of(available_cols)), na.rm = TRUE)) %>%
      ungroup()

  } else if (aggregation_method == "sumavgs") {
    # This requires category information - simplified version here
    # You may need to adapt this based on your tbl_indicators structure
    data_scored <- data_subset %>%
      rowwise() %>%
      mutate(vuln_score = mean(c_across(all_of(available_cols)), na.rm = TRUE)) %>%
      ungroup()

    warning("sumavgs method requires category information - using average instead")
  }

  # Calculate ranks within groups
  data_ranked <- data_scored %>%
    group_by(across(all_of(group_cols))) %>%
    mutate(
      rank = rank(vuln_score, ties.method = "average", na.last = "keep"),
      rank = max(rank, na.rm = TRUE) + 1 - rank  # Reverse so high score = high rank
    ) %>%
    ungroup() %>%
    arrange(across(all_of(group_cols)), rank)

  return(data_ranked)
}


#------------------------------------------------------------------------------
# Helper Function: Compare Ranks
#------------------------------------------------------------------------------

#' Compare full model ranks to jackknife (leave-one-out) ranks
#'
#' @param full_ranks Data frame with full model ranks
#' @param jackknife_ranks Data frame with jackknife ranks
#' @param removed_indicator Name of indicator that was removed
#' @param id_col CU identifier column
#' @param name_col CU name column
#' @param group_cols Grouping columns
#'
#' @return Data frame with rank comparisons
#'
compare_ranks <- function(full_ranks,
                          jackknife_ranks,
                          removed_indicator,
                          id_col = "FULL_CU_IN",
                          name_col = "CVIS_NAME",
                          group_cols = c("rcp", "period_code")) {

  join_cols <- c(id_col, group_cols)

  rank_comparison <- full_ranks %>%
    select(all_of(c(join_cols, name_col)), full_rank = rank, full_score = vuln_score) %>%
    left_join(
      jackknife_ranks %>%
        select(all_of(join_cols), jack_rank = rank, jack_score = vuln_score),
      by = join_cols
    ) %>%
    mutate(
      rank_change = jack_rank - full_rank,
      score_change = jack_score - full_score,
      abs_rank_change = abs(rank_change)
    )

  return(rank_comparison)
}


#------------------------------------------------------------------------------
# Visualization Functions
#------------------------------------------------------------------------------

#' Plot indicator influence from jackknife analysis
#'
#' @param jackknife_results Output from jackknife_vulnerability_analysis()
#' @param metric Influence metric to plot (default: "mean_abs_rank_change")
#'
#' @return ggplot object
#'
plot_indicator_influence <- function(jackknife_results,
                                     metric = "mean_abs_rank_change") {

  plot_data <- jackknife_results$influence_summary %>%
    arrange(desc(.data[[metric]]))

  ggplot(plot_data, aes(x = reorder(removed_indicator, .data[[metric]]),
    y = .data[[metric]])) +
    geom_col(fill = "steelblue", alpha = 0.8) +
    geom_text(aes(label = round(.data[[metric]], 2)),
      hjust = -0.2, size = 3) +
    coord_flip() +
    labs(
      title = "Indicator Influence on Vulnerability Rankings",
      subtitle = "Mean absolute rank change when indicator is removed",
      x = "Indicator Removed",
      y = "Mean Absolute Rank Change"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      axis.text = element_text(size = 10)
    )
}


#' Plot rank changes for specific CUs across all jackknife iterations
#'
#' @param jackknife_results Output from jackknife_vulnerability_analysis()
#' @param cu_ids Vector of CU identifiers to highlight
#' @param id_col CU identifier column name
#'
#' @return ggplot object
#'
plot_cu_rank_stability <- function(jackknife_results,
                                   cu_ids,
                                   id_col = "FULL_CU_IN") {

  plot_data <- jackknife_results$rank_changes %>%
    filter(.data[[id_col]] %in% cu_ids)

  ggplot(plot_data, aes(x = removed_indicator, y = rank_change,
    group = .data[[id_col]], color = .data[[id_col]])) +
    geom_line(linewidth = 1, alpha = 0.7) +
    geom_point(size = 2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    labs(
      title = "Rank Stability Across Jackknife Iterations",
      subtitle = "Change in rank when each indicator is removed",
      x = "Indicator Removed",
      y = "Change in Rank",
      color = "Conservation Unit"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    )
}


#' Heatmap of rank changes across all CUs and indicators
#'
#' @param jackknife_results Output from jackknife_vulnerability_analysis()
#' @param name_col CU name column
#'
#' @return ggplot object
#'
plot_jackknife_heatmap <- function(jackknife_results,
                                   name_col = "CVIS_NAME") {

  plot_data <- jackknife_results$rank_changes %>%
    select(all_of(name_col), removed_indicator, rank_change)

  ggplot(plot_data, aes(x = removed_indicator, y = .data[[name_col]],
    fill = rank_change)) +
    geom_tile(color = "white", linewidth = 0.5) +
    scale_fill_gradient2(
      low = "blue", mid = "white", high = "red",
      midpoint = 0,
      name = "Rank\nChange"
    ) +
    labs(
      title = "Jackknife Analysis: Rank Changes Heatmap",
      subtitle = "How each CU's rank changes when indicators are removed",
      x = "Indicator Removed",
      y = "Conservation Unit"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
      axis.text.y = element_text(size = 7),
      plot.title = element_text(face = "bold")
    )
}


#------------------------------------------------------------------------------
# Summary and Export Functions
#------------------------------------------------------------------------------

#' Generate comprehensive jackknife analysis report
#'
#' @param jackknife_results Output from jackknife_vulnerability_analysis()
#' @param output_dir Directory to save results
#' @param file_prefix Prefix for output files
#'
#' @export
#'
export_jackknife_results <- function(jackknife_results,
                                     output_dir = "outputs",
                                     file_prefix = "jackknife") {

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  timestamp <- format(Sys.Date(), "%Y-%m-%d")

  # Export influence summary
  write.csv(
    jackknife_results$influence_summary,
    file.path(output_dir, paste0(file_prefix, "_influence_summary_", timestamp, ".csv")),
    row.names = FALSE
  )

  # Export detailed rank changes
  write.csv(
    jackknife_results$rank_changes,
    file.path(output_dir, paste0(file_prefix, "_rank_changes_", timestamp, ".csv")),
    row.names = FALSE
  )

  # Export full ranks for reference
  write.csv(
    jackknife_results$full_ranks,
    file.path(output_dir, paste0(file_prefix, "_full_ranks_", timestamp, ".csv")),
    row.names = FALSE
  )

  # Save plots
  ggsave(
    filename = file.path(output_dir, paste0(file_prefix, "_influence_plot_", timestamp, ".png")),
    plot = plot_indicator_influence(jackknife_results),
    width = 10, height = 8, dpi = 300
  )

  ggsave(
    filename = file.path(output_dir, paste0(file_prefix, "_heatmap_", timestamp, ".png")),
    plot = plot_jackknife_heatmap(jackknife_results),
    width = 12, height = 10, dpi = 300
  )

  cat("\nResults exported to:", output_dir, "\n")
  cat("Files created:\n")
  cat("  - Influence summary CSV\n")
  cat("  - Rank changes CSV\n")
  cat("  - Full ranks CSV\n")
  cat("  - Influence plot PNG\n")
  cat("  - Heatmap PNG\n\n")
}


#------------------------------------------------------------------------------
# Category-Specific Jackknife Analysis
#------------------------------------------------------------------------------

#' Perform jackknife analysis by indicator category
#'
#' Runs separate jackknife analyses for each indicator category (e.g., freshwater
#' rearing, migration, demographic, marine) to see how rankings within each
#' life stage category are affected by removing indicators.
#'
#' @param data Data frame containing standardized indicator values
#' @param tbl_indicators Table with indicator metadata including 'type' column
#' @param aggregation_method Method for combining indicators
#' @param group_cols Columns to group by when ranking
#' @param id_col CU identifier column
#' @param name_col CU name column
#'
#' @return List with jackknife results for each category plus overall
#'
jackknife_by_category <- function(data,
                                  tbl_indicators,
                                  aggregation_method = "additive",
                                  group_cols = c("rcp", "period_code"),
                                  id_col = "FULL_CU_IN",
                                  name_col = "CVIS_NAME") {

  cat("\n========================================\n")
  cat("Category-Specific Jackknife Analysis\n")
  cat("========================================\n\n")

  # Get unique categories
  categories <- unique(tbl_indicators$type)
  cat("Categories found:", paste(categories, collapse = ", "), "\n\n")

  # Store results for each category
  category_results <- list()

  # Run jackknife for each category
  for (cat_i in categories) {

    cat("\n--- Analyzing category:", cat_i, "---\n")

    # Get indicators for this category
    cat_indicators <- tbl_indicators$abbrev[tbl_indicators$type == cat_i]

    if (length(cat_indicators) < 2) {
      cat("Skipping category", cat_i, "- needs at least 2 indicators\n")
      next
    }

    cat("Indicators in category:", paste(cat_indicators, collapse = ", "), "\n")

    # Run jackknife for this category
    results <- jackknife_vulnerability_analysis(
      data = data,
      indicators_choose = cat_indicators,
      aggregation_method = aggregation_method,
      group_cols = group_cols,
      id_col = id_col,
      name_col = name_col
    )

    # Store results
    category_results[[cat_i]] <- results
  }

  # Also run overall analysis with all indicators
  cat("\n--- Analyzing ALL categories combined ---\n")
  overall_results <- jackknife_vulnerability_analysis(
    data = data,
    indicators_choose = tbl_indicators$abbrev,
    aggregation_method = aggregation_method,
    group_cols = group_cols,
    id_col = id_col,
    name_col = name_col
  )

  # Compile summary comparison across categories
  cat("\n========================================\n")
  cat("Summary Across Categories\n")
  cat("========================================\n\n")

  category_summary <- map_dfr(names(category_results), function(cat_name) {
    cat_res <- category_results[[cat_name]]

    tibble(
      category = cat_name,
      n_indicators = cat_res$metadata$n_indicators,
      mean_influence = mean(cat_res$influence_summary$mean_abs_rank_change),
      max_influence = max(cat_res$influence_summary$mean_abs_rank_change),
      most_influential = cat_res$influence_summary$removed_indicator[1]
    )
  })

  print(category_summary)

  # Return all results
  return(list(
    by_category = category_results,
    overall = overall_results,
    category_summary = category_summary
  ))
}


#' Plot category-specific indicator influence comparison
#'
#' @param category_results Output from jackknife_by_category()
#'
#' @return ggplot object
#'
plot_category_influence_comparison <- function(category_results) {
  # Combine influence summaries from all categories
  plot_data <- map_dfr(names(category_results$by_category), function(cat_name) {
    category_results$by_category[[cat_name]]$influence_summary %>%
      mutate(category = cat_name)
  })

  # Create faceted plot
  ggplot(plot_data, aes(x = reorder(removed_indicator, mean_abs_rank_change),
    y = mean_abs_rank_change)) +
    geom_col(aes(fill = category), alpha = 0.8) +
    geom_text(aes(label = round(mean_abs_rank_change, 2)),
      hjust = -0.2, size = 2.5) +
    coord_flip() +
    facet_wrap(~category, scales = "free_y", ncol = 2) +
    labs(
      title = "Indicator Influence by Category",
      subtitle = "Mean absolute rank change when indicator removed (within category)",
      x = "Indicator Removed",
      y = "Mean Absolute Rank Change",
      fill = "Category"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      strip.text = element_text(face = "bold", size = 11),
      legend.position = "none"
    )
}


#' Compare ranks across categories
#'
#' Shows how CU vulnerability rankings differ when considering only specific
#' life stage categories vs. all categories combined
#'
#' @param category_results Output from jackknife_by_category()
#' @param id_col CU identifier column
#'
#' @return ggplot object
#'
plot_category_rank_comparison <- function(category_results,
                                          id_col = "FULL_CU_IN") {
  # Extract ranks from each category
  rank_data <- map_dfr(names(category_results$by_category), function(cat_name) {
    category_results$by_category[[cat_name]]$full_ranks %>%
      select(all_of(id_col), rank) %>%
      mutate(category = cat_name)
  })

  # Add overall ranks
  rank_data <- bind_rows(
    rank_data,
    category_results$overall$full_ranks %>%
      select(all_of(id_col), rank) %>%
      mutate(category = "Overall")
  )

  # Calculate rank ranges for each CU
  rank_summary <- rank_data %>%
    group_by(.data[[id_col]]) %>%
    summarize(
      min_rank = min(rank, na.rm = TRUE),
      max_rank = max(rank, na.rm = TRUE),
      rank_range = max_rank - min_rank,
      mean_rank = mean(rank, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(rank_range))

  # Plot top CUs with most variable ranks across categories
  top_variable <- head(rank_summary, 20)

  rank_data_plot <- rank_data %>%
    filter(.data[[id_col]] %in% top_variable[[id_col]])

  ggplot(rank_data_plot, aes(x = category, y = rank, group = .data[[id_col]])) +
    geom_line(aes(color = .data[[id_col]]), alpha = 0.6, linewidth = 1) +
    geom_point(aes(color = .data[[id_col]]), size = 2) +
    labs(
      title = "Rank Variability Across Categories",
      subtitle = "Top 20 CUs with most variable ranks across life stage categories",
      x = "Category",
      y = "Rank (lower = more vulnerable)",
      color = "Conservation Unit"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "right",
      legend.text = element_text(size = 7)
    )
}


#' Export category-specific jackknife results
#'
#' @param category_results Output from jackknife_by_category()
#' @param output_dir Directory to save results
#' @param file_prefix Prefix for output files
#'
export_category_jackknife_results <- function(category_results,
                                              output_dir = "outputs",
                                              file_prefix = "category_jackknife") {

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  timestamp <- format(Sys.Date(), "%Y-%m-%d")

  # Export category summary
  write.csv(
    category_results$category_summary,
    file.path(output_dir, paste0(file_prefix, "_category_summary_", timestamp, ".csv")),
    row.names = FALSE
  )

  # Export influence summary for each category
  for (cat_name in names(category_results$by_category)) {
    cat_safe_name <- gsub("[^A-Za-z0-9]", "_", cat_name)

    write.csv(
      category_results$by_category[[cat_name]]$influence_summary,
      file.path(output_dir, paste0(file_prefix, "_", cat_safe_name, "_influence_", timestamp, ".csv")),
      row.names = FALSE
    )

    write.csv(
      category_results$by_category[[cat_name]]$rank_changes,
      file.path(output_dir, paste0(file_prefix, "_", cat_safe_name, "_ranks_", timestamp, ".csv")),
      row.names = FALSE
    )
  }

  # Export overall results
  write.csv(
    category_results$overall$influence_summary,
    file.path(output_dir, paste0(file_prefix, "_overall_influence_", timestamp, ".csv")),
    row.names = FALSE
  )

  # Save comparison plots
  ggsave(
    filename = file.path(output_dir, paste0(file_prefix, "_category_influence_", timestamp, ".png")),
    plot = plot_category_influence_comparison(category_results),
    width = 12, height = 10, dpi = 300
  )

  ggsave(
    filename = file.path(output_dir, paste0(file_prefix, "_rank_comparison_", timestamp, ".png")),
    plot = plot_category_rank_comparison(category_results),
    width = 12, height = 8, dpi = 300
  )

  cat("\nCategory-specific results exported to:", output_dir, "\n")
}


#------------------------------------------------------------------------------
# Example Usage
#------------------------------------------------------------------------------

# # Load your data
# source(file.path(here(), "code", "0_setup.R"))
# load(file.path(paths$indicators, "standardized_indicators.Rdata"))
#
# Filter to specific scenario (e.g., RCP 4.5, mid-century)
data_filtered <- all_flat_std %>%
  filter(rcp == "45", period_code == 3)

# ============= OVERALL JACKKNIFE ANALYSIS =============

# Run overall jackknife analysis (all indicators)
jackknife_results <- jackknife_vulnerability_analysis(
  data = data_filtered,
  indicators_choose = tbl_indicators$abbrev,
  aggregation_method = "sumavgs",
  group_cols = c("rcp", "period_code"),
  id_col = "FULL_CU_IN",
  name_col = "CVIS_NAME"
)

# View influence summary
print(jackknife_results$influence_summary)

# Create visualizations
plot_indicator_influence(jackknife_results)
plot_jackknife_heatmap(jackknife_results)

# Examine specific CUs
top_cus <- jackknife_results$full_ranks %>%
  filter(rank <= 5) %>%
  pull(FULL_CU_IN)

plot_cu_rank_stability(jackknife_results, cu_ids = top_cus)

# Export results
export_jackknife_results(
  jackknife_results,
  output_dir = file.path(paths$indicators, "jackknife_analysis"),
  file_prefix = "jackknife_rcp45_midcentury"
)

# ============= CATEGORY-SPECIFIC JACKKNIFE ANALYSIS =============

# Run category-specific jackknife analysis
# This analyzes freshwater rearing, migration, demographic, and marine separately
category_results <- jackknife_by_category(
  data = data_filtered,
  tbl_indicators = tbl_indicators,
  aggregation_method = "sumavgs",
  group_cols = c("rcp", "period_code"),
  id_col = "FULL_CU_IN",
  name_col = "CVIS_NAME"
)

# View category summary
print(category_results$category_summary)

# View most influential indicators within each category
for (cat in names(category_results$by_category)) {
  cat("\n=== Category:", cat, "===\n")
  print(head(category_results$by_category[[cat]]$influence_summary, 5))
}

# Create category comparison plots
plot_category_influence_comparison(category_results)
plot_category_rank_comparison(category_results)

# Export category-specific results
export_category_jackknife_results(
  category_results,
  output_dir = file.path(paths$indicators, "jackknife_analysis"),
  file_prefix = "category_jackknife_rcp45_midcentury"
)

# ============= COMPARE CATEGORIES =============

# Which category shows the most variable influence?
category_results$category_summary %>%
  arrange(desc(mean_influence))

# Which CUs have the most variable ranks across categories?
# (i.e., rank high in one life stage but low in another)
rank_variability <- map_dfr(names(category_results$by_category), function(cat) {
  category_results$by_category[[cat]]$full_ranks %>%
    select(FULL_CU_IN, CVIS_NAME, rank) %>%
    mutate(category = cat)
}) %>%
  group_by(FULL_CU_IN, CVIS_NAME) %>%
  summarize(
    rank_range = max(rank) - min(rank),
    mean_rank = mean(rank),
    .groups = "drop"
  ) %>%
  arrange(desc(rank_range))

print(head(rank_variability, 10))
