################################################################################
#
# Comprehensive Sensitivity Analysis: RCP and Time Period Effects
#
# This script extends 4e_rcp_period_sensitivity_v2.R to provide detailed
# analysis of how emissions scenarios (RCP 4.5 vs 8.5) and time periods
# (Period 3: mid-century vs Period 5: end-century) influence:
#   1. Raw indicator values (tw8rate, tw8proj, migrT, lowQpdelta, highQpdelta, favchange)
#   2. Standardized indicator scores
#   3. Overall rankings and category-specific rankings (fwR, migr)
#
################################################################################

library(tidyverse)
library(patchwork)
library(corrplot)
library(here)
library(gt)
library(scales)

# Source required scripts
source("0_setup.R")
source("2_fw_utils.R")
source("4_scoring_utils.R")
source("4a_CU_scoring.R")
source("4e_rcp_period_sensitivity_v2.R")

#------------------------------------------------------------------------------
# 1. INDIVIDUAL INDICATOR SENSITIVITY ANALYSIS
#------------------------------------------------------------------------------

#' Analyze how raw and standardized indicator values change across scenarios
#'
#' @param data_raw Data with raw indicator values (all_flat)
#' @param data_std Data with standardized values (all_flat_std)
#' @param indicators Vector of indicator names to analyze
#' @param periods Vector of period codes
#' @param rcps Vector of RCP scenarios
#'
#' @return List with indicator sensitivity results
#'
analyze_indicator_sensitivity <- function(data_raw,
                                          data_std,
                                          indicators = c("tw8rate", "tw8proj", "migrT",
                                            "lowQpdelta", "highQpdelta", "favchange"),
                                          periods = c(3, 5),
                                          rcps = c("45", "85")) {

  cat("\n========================================\n")
  cat("Analyzing Individual Indicator Sensitivity\n")
  cat("========================================\n\n")

  results <- list()

  for (ind in indicators) {
    cat("Analyzing:", ind, "\n")

    # Get raw and standardized columns
    raw_col <- ind
    std_col <- paste0("std_", ind)

    # Check if columns exist
    if (!raw_col %in% names(data_raw) || !std_col %in% names(data_std)) {
      cat("  Warning: Columns not found, skipping\n")
      next
    }

    # Extract data for all scenarios
    raw_data <- data_raw %>%
      filter(period_code %in% periods, rcp %in% rcps) %>%
      select(FULL_CU_IN, CVIS_NAME, SPECIES_NAME, rcp, period_code,
        raw_value = all_of(raw_col)) %>%
      mutate(scenario = paste0("RCP", rcp, "_P", period_code))

    std_data <- data_std %>%
      filter(period_code %in% periods, rcp %in% rcps) %>%
      select(FULL_CU_IN, rcp, period_code,
        std_value = all_of(std_col)) %>%
      mutate(scenario = paste0("RCP", rcp, "_P", period_code))

    # Combine raw and standardized
    combined_data <- raw_data %>%
      left_join(std_data, by = c("FULL_CU_IN", "rcp", "period_code", "scenario"))

    # Calculate summary statistics by scenario
    summary_stats <- combined_data %>%
      group_by(scenario, rcp, period_code) %>%
      summarise(
        n_CUs = n(),
        raw_mean = mean(raw_value, na.rm = TRUE),
        raw_sd = sd(raw_value, na.rm = TRUE),
        raw_min = min(raw_value, na.rm = TRUE),
        raw_max = max(raw_value, na.rm = TRUE),
        std_mean = mean(std_value, na.rm = TRUE),
        std_sd = sd(std_value, na.rm = TRUE),
        std_min = min(std_value, na.rm = TRUE),
        std_max = max(std_value, na.rm = TRUE),
        .groups = "drop"
      )

    # Calculate pairwise differences between scenarios
    scenarios <- unique(combined_data$scenario)
    pairwise_diffs <- list()

    for (i in 1:(length(scenarios) - 1)) {
      for (j in (i + 1):length(scenarios)) {
        s1 <- scenarios[i]
        s2 <- scenarios[j]

        data_s1 <- combined_data %>% filter(scenario == s1)
        data_s2 <- combined_data %>% filter(scenario == s2)

        merged <- data_s1 %>%
          select(FULL_CU_IN, CVIS_NAME, SPECIES_NAME,
            raw_s1 = raw_value, std_s1 = std_value) %>%
          inner_join(
            data_s2 %>% select(FULL_CU_IN, raw_s2 = raw_value, std_s2 = std_value),
            by = "FULL_CU_IN"
          ) %>%
          mutate(
            raw_diff = raw_s2 - raw_s1,
            std_diff = std_s2 - std_s1,
            raw_pct_change = (raw_s2 - raw_s1) / abs(raw_s1) * 100,
            abs_raw_diff = abs(raw_diff),
            abs_std_diff = abs(std_diff)
          )

        diff_summary <- tibble(
          indicator = ind,
          scenario1 = s1,
          scenario2 = s2,
          comparison_type = case_when(
            str_extract(s1, "RCP\\d+") == str_extract(s2, "RCP\\d+") ~ "Period",
            str_extract(s1, "P\\d+") == str_extract(s2, "P\\d+") ~ "RCP",
            TRUE ~ "Both"
          ),
          n_CUs = nrow(merged),
          mean_raw_diff = mean(merged$raw_diff, na.rm = TRUE),
          sd_raw_diff = sd(merged$raw_diff, na.rm = TRUE),
          mean_abs_raw_diff = mean(merged$abs_raw_diff, na.rm = TRUE),
          mean_std_diff = mean(merged$std_diff, na.rm = TRUE),
          sd_std_diff = sd(merged$std_diff, na.rm = TRUE),
          mean_abs_std_diff = mean(merged$abs_std_diff, na.rm = TRUE),
          cor_raw = cor(merged$raw_s1, merged$raw_s2, use = "complete.obs"),
          cor_std = cor(merged$std_s1, merged$std_s2, use = "complete.obs")
        )

        pairwise_diffs[[paste(s1, s2, sep = "_vs_")]] <- list(
          summary = diff_summary,
          detailed = merged
        )
      }
    }

    results[[ind]] <- list(
      summary_stats = summary_stats,
      full_data = combined_data,
      pairwise_comparisons = pairwise_diffs
    )

    cat("  Completed\n")
  }

  cat("\nIndicator sensitivity analysis complete!\n\n")
  return(results)
}


#------------------------------------------------------------------------------
# 2. ENHANCED PLOTTING FUNCTIONS
#------------------------------------------------------------------------------

#' Plot indicator values across scenarios for all CUs
#'
#' @param indicator_results Output from analyze_indicator_sensitivity()
#' @param indicator_name Name of indicator to plot
#' @param value_type "raw" or "std" for raw or standardized values
#'
plot_indicator_by_scenario <- function(indicator_results,
                                       indicator_name,
                                       value_type = "std") {

  data <- indicator_results[[indicator_name]]$full_data

  value_col <- if (value_type == "std") "std_value" else "raw_value"

  # Box plots by scenario
  p1 <- ggplot(data, aes(x = scenario, y = .data[[value_col]], fill = rcp)) +
    geom_boxplot(alpha = 0.7) +
    facet_wrap(~period_code, labeller = labeller(period_code = c("3" = "Mid-century", "5" = "End-century"))) +
    scale_fill_manual(values = c("45" = "#4575b4", "85" = "#d73027")) +
    labs(
      title = paste(indicator_name, "-", ifelse(value_type == "std", "Standardized", "Raw"), "Values"),
      subtitle = "Distribution across all CUs by scenario",
      x = "Scenario",
      y = ifelse(value_type == "std", "Standardized Value", "Raw Value"),
      fill = "RCP"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(face = "bold", size = 14)
    )

  # Violin plots
  p2 <- ggplot(data, aes(x = scenario, y = .data[[value_col]], fill = rcp)) +
    geom_violin(alpha = 0.5) +
    geom_boxplot(width = 0.2, alpha = 0.8, outlier.size = 0.5) +
    scale_fill_manual(values = c("45" = "#4575b4", "85" = "#d73027")) +
    labs(
      title = "Distribution with density",
      x = "Scenario",
      y = ifelse(value_type == "std", "Standardized Value", "Raw Value"),
      fill = "RCP"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  combined <- p1 / p2 +
    plot_layout(heights = c(2, 1)) +
    plot_annotation(
      title = paste("Sensitivity Analysis:", indicator_name),
      theme = theme(plot.title = element_text(face = "bold", size = 16))
    )

  return(combined)
}


#' Plot indicator changes between scenarios
#'
#' @param indicator_results Output from analyze_indicator_sensitivity()
#' @param indicator_name Name of indicator to plot
#' @param comparison Which comparison to plot (e.g., "RCP45_P3_vs_RCP85_P3")
#'
plot_indicator_changes <- function(indicator_results,
                                   indicator_name,
                                   comparison = NULL) {

  comparisons <- indicator_results[[indicator_name]]$pairwise_comparisons

  if (is.null(comparison)) {
    comparison <- names(comparisons)[1]
  }

  data <- comparisons[[comparison]]$detailed
  summary <- comparisons[[comparison]]$summary

  # Scatter plot of values
  p1 <- ggplot(data, aes(x = raw_s1, y = raw_s2, color = SPECIES_NAME)) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
    geom_point(alpha = 0.6, size = 2) +
    labs(
      title = paste("Raw Values:", str_replace_all(comparison, "_", " ")),
      subtitle = paste("Correlation:", round(summary$cor_raw, 3)),
      x = summary$scenario1,
      y = summary$scenario2,
      color = "Species"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")

  # Histogram of changes
  p2 <- ggplot(data, aes(x = raw_diff, fill = SPECIES_NAME)) +
    geom_histogram(alpha = 0.6, bins = 30) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
    geom_vline(xintercept = summary$mean_raw_diff, linetype = "solid", color = "red", linewidth = 1) +
    labs(
      title = "Distribution of Changes",
      subtitle = paste("Mean change:", round(summary$mean_raw_diff, 3)),
      x = "Change in Raw Value",
      y = "Count",
      fill = "Species"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")

  # Standardized values scatter
  p3 <- ggplot(data, aes(x = std_s1, y = std_s2, color = SPECIES_NAME)) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
    geom_point(alpha = 0.6, size = 2) +
    labs(
      title = "Standardized Values",
      subtitle = paste("Correlation:", round(summary$cor_std, 3)),
      x = summary$scenario1,
      y = summary$scenario2,
      color = "Species"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")

  # Standardized changes histogram
  p4 <- ggplot(data, aes(x = std_diff, fill = SPECIES_NAME)) +
    geom_histogram(alpha = 0.6, bins = 30) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
    geom_vline(xintercept = summary$mean_std_diff, linetype = "solid", color = "red", linewidth = 1) +
    labs(
      title = "Distribution of Standardized Changes",
      subtitle = paste("Mean change:", round(summary$mean_std_diff, 3)),
      x = "Change in Standardized Value",
      y = "Count",
      fill = "Species"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")

  combined <- (p1 | p2) / (p3 | p4) +
    plot_annotation(
      title = paste("Indicator Change Analysis:", indicator_name),
      theme = theme(plot.title = element_text(face = "bold", size = 16))
    )

  return(combined)
}


#' Create heatmap of CU rankings across scenarios
#'
#' @param rank_data Output from calculate_vulnerability_ranks()
#' @param score_type Which score type to plot
#' @param top_n Number of top/bottom CUs to show
#'
plot_ranking_heatmap <- function(rank_data,
                                 score_type = "std_addall",
                                 top_n = 20) {
  # Get ranks in wide format
  ranks_wide <- rank_data %>%
    filter(score_type == !!score_type) %>%
    select(FULL_CU_IN, CVIS_NAME, SPECIES_NAME, scenario_id, rank_value) %>%
    pivot_wider(names_from = scenario_id, values_from = rank_value)

  # Calculate mean rank
  rank_cols <- setdiff(names(ranks_wide), c("FULL_CU_IN", "CVIS_NAME", "SPECIES_NAME"))
  ranks_wide$mean_rank <- rowMeans(ranks_wide[, rank_cols], na.rm = TRUE)

  # Get top and bottom CUs
  top_CUs <- ranks_wide %>%
    arrange(mean_rank) %>%
    head(top_n)

  bottom_CUs <- ranks_wide %>%
    arrange(desc(mean_rank)) %>%
    head(top_n)

  selected_CUs <- bind_rows(
    top_CUs %>% mutate(group = "Most Vulnerable"),
    bottom_CUs %>% mutate(group = "Least Vulnerable")
  )

  # Prepare for plotting
  plot_data <- selected_CUs %>%
    pivot_longer(cols = all_of(rank_cols),
      names_to = "scenario",
      values_to = "rank") %>%
    mutate(
      CU_label = paste0(SPECIES_NAME, ": ", CVIS_NAME),
      CU_label = factor(CU_label, levels = unique(CU_label[order(mean_rank)]))
    )

  # Create heatmap
  p <- ggplot(plot_data, aes(x = scenario, y = CU_label, fill = rank)) +
    geom_tile(color = "white", size = 0.5) +
    geom_text(aes(label = round(rank)), size = 2.5, color = "white") +
    facet_wrap(~group, scales = "free_y", ncol = 1) +
    scale_fill_viridis_c(option = "plasma", direction = -1) +
    labs(
      title = paste("CU Rankings Across Scenarios:", score_type),
      subtitle = paste("Top and bottom", top_n, "CUs by mean rank"),
      x = "Scenario",
      y = NULL,
      fill = "Rank"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.text.y = element_text(size = 8),
      strip.text = element_text(face = "bold", size = 12),
      plot.title = element_text(face = "bold", size = 14)
    )

  return(p)
}


#' Plot rank changes between specific scenarios
#'
#' @param rank_data Output from calculate_vulnerability_ranks()
#' @param scenario1 First scenario ID
#' @param scenario2 Second scenario ID
#' @param score_type Which score type to plot
#' @param label_top_n Number of CUs to label with names
#'
plot_rank_change_scatter <- function(rank_data,
                                     scenario1,
                                     scenario2,
                                     score_type = "std_addall",
                                     label_top_n = 10) {
  # Get ranks for both scenarios
  data <- rank_data %>%
    filter(score_type == !!score_type,
      scenario_id %in% c(scenario1, scenario2)) %>%
    select(FULL_CU_IN, CVIS_NAME, SPECIES_NAME, scenario_id, rank_value) %>%
    pivot_wider(names_from = scenario_id, values_from = rank_value) %>%
    mutate(
      rank_change = .data[[scenario2]] - .data[[scenario1]],
      abs_rank_change = abs(rank_change),
      change_direction = ifelse(rank_change > 0, "Increased vulnerability", "Decreased vulnerability")
    )

  # Identify top changers to label
  top_changers <- data %>%
    arrange(desc(abs_rank_change)) %>%
    head(label_top_n)

  # Create scatter plot
  p <- ggplot(data, aes(x = .data[[scenario1]], y = .data[[scenario2]],
    color = SPECIES_NAME, size = abs_rank_change)) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50", linewidth = 1) +
    geom_point(alpha = 0.6) +
    geom_text(data = top_changers,
      aes(label = CVIS_NAME),
      size = 2.5,
      nudge_y = 1,
      check_overlap = TRUE) +
    scale_size_continuous(range = c(1, 6)) +
    labs(
      title = paste("Rank Changes:", scenario1, "vs", scenario2),
      subtitle = paste("Score type:", score_type),
      x = paste("Rank in", scenario1),
      y = paste("Rank in", scenario2),
      color = "Species",
      size = "Absolute\nRank Change"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      legend.position = "right"
    )

  return(p)
}


#' Plot indicator correlation with rank changes
#'
#' @param indicator_drivers Output from analyze_indicator_drivers()
#' @param comparison Which comparison to plot
#'
plot_indicator_driver_heatmap <- function(indicator_drivers,
                                          comparison = NULL) {

  if (!is.null(comparison)) {
    data <- indicator_drivers %>%
      filter(comparison == !!comparison)
  } else {
    data <- indicator_drivers
  }

  # Create heatmap
  p <- ggplot(data, aes(x = comparison, y = indicator, fill = cor_with_rank_change)) +
    geom_tile(color = "white") +
    geom_text(aes(label = sprintf("%.2f", cor_with_rank_change)), size = 3) +
    scale_fill_gradient2(
      low = "#2166ac",
      mid = "white",
      high = "#b2182b",
      midpoint = 0,
      limits = c(-1, 1)
    ) +
    facet_wrap(~score_type, scales = "free") +
    labs(
      title = "Indicator Correlation with Rank Changes",
      subtitle = "Spearman correlation between indicator change and rank change",
      x = "Scenario Comparison",
      y = "Indicator",
      fill = "Correlation"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.text = element_text(face = "bold"),
      plot.title = element_text(face = "bold", size = 14)
    )

  return(p)
}


#' Create comprehensive summary plot of RCP and period effects
#'
#' @param indicator_results Output from analyze_indicator_sensitivity()
#'
plot_rcp_period_summary <- function(indicator_results) {
  # Extract all pairwise comparison summaries
  all_comparisons <- list()

  for (ind in names(indicator_results)) {
    for (comp_name in names(indicator_results[[ind]]$pairwise_comparisons)) {
      summary <- indicator_results[[ind]]$pairwise_comparisons[[comp_name]]$summary
      all_comparisons[[paste(ind, comp_name, sep = "_")]] <- summary
    }
  }

  comparison_data <- bind_rows(all_comparisons)

  # Plot 1: Mean absolute standardized difference by comparison type
  p1 <- ggplot(comparison_data,
    aes(x = indicator, y = mean_abs_std_diff, fill = comparison_type)) +
    geom_col(position = "dodge") +
    labs(
      title = "Mean Absolute Change in Standardized Values",
      x = "Indicator",
      y = "Mean Absolute Difference",
      fill = "Comparison Type"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(face = "bold")
    )

  # Plot 2: Correlation by indicator and comparison type
  p2 <- ggplot(comparison_data,
    aes(x = indicator, y = cor_std, fill = comparison_type)) +
    geom_col(position = "dodge") +
    geom_hline(yintercept = 0.9, linetype = "dashed", color = "red") +
    scale_y_continuous(limits = c(0, 1)) +
    labs(
      title = "Correlation of Standardized Values Across Scenarios",
      x = "Indicator",
      y = "Spearman Correlation",
      fill = "Comparison Type"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(face = "bold")
    )

  combined <- p1 / p2 +
    plot_annotation(
      title = "RCP and Time Period Effects on Environmental Indicators",
      theme = theme(plot.title = element_text(face = "bold", size = 16))
    )

  return(combined)
}


#------------------------------------------------------------------------------
# 3. MAIN ANALYSIS WORKFLOW
#------------------------------------------------------------------------------

#' Run complete sensitivity analysis with all plots and exports
#'
run_complete_sensitivity_analysis <- function(output_dir = "outputs/sensitivity_analysis") {

  cat("\n")
  cat("================================================================================\n")
  cat("              COMPREHENSIVE SENSITIVITY ANALYSIS                                \n")
  cat("================================================================================\n")
  cat("\n")

  # Create output directory
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # 1. Run main ranking analysis
  cat("STEP 1: Calculating vulnerability rankings...\n")
  ranking_results <- run_rcp_period_sensitivity_analysis(
    data = combined_scores_std,
    score_types = c("std_addall", "std_addfwR", "std_addmigr"),
    periods_include = c(3, 5),
    rcps_include = c("45", "85"),
    output_dir = output_dir
  )

  # 2. Analyze individual indicators
  cat("\nSTEP 2: Analyzing individual indicator sensitivity...\n")
  indicator_results <- analyze_indicator_sensitivity(
    data_raw = all_flat,
    data_std = all_flat_std,
    indicators = c("tw8rate", "tw8proj", "migrT", "lowQpdelta", "highQpdelta", "favchange"),
    periods = c(3, 5),
    rcps = c("45", "85")
  )

  # 3. Create plots
  cat("\nSTEP 3: Creating comprehensive plots...\n")

  # Indicator-specific plots
  for (ind in names(indicator_results)) {
    cat("  Plotting:", ind, "\n")

    # Distribution plots
    p_dist <- plot_indicator_by_scenario(indicator_results, ind, value_type = "std")
    ggsave(
      file.path(output_dir, paste0("indicator_distribution_", ind, ".png")),
      plot = p_dist,
      width = 12, height = 10, dpi = 300
    )

    # Change plots for each pairwise comparison
    comparisons <- names(indicator_results[[ind]]$pairwise_comparisons)
    for (comp in comparisons) {
      p_change <- plot_indicator_changes(indicator_results, ind, comp)
      comp_clean <- str_replace_all(comp, "_", "-")
      ggsave(
        file.path(output_dir, paste0("indicator_changes_", ind, "_", comp_clean, ".png")),
        plot = p_change,
        width = 14, height = 10, dpi = 300
      )
    }
  }

  # Summary plots
  cat("  Creating summary plots...\n")

  p_summary <- plot_rcp_period_summary(indicator_results)
  ggsave(
    file.path(output_dir, "rcp_period_summary.png"),
    plot = p_summary,
    width = 14, height = 10, dpi = 300
  )

  # Ranking heatmaps
  for (score_type in c("std_addall", "std_addfwR", "std_addmigr")) {
    p_heatmap <- plot_ranking_heatmap(
      ranking_results$rank_data,
      score_type = score_type,
      top_n = 15
    )
    ggsave(
      file.path(output_dir, paste0("ranking_heatmap_", score_type, ".png")),
      plot = p_heatmap,
      width = 12, height = 14, dpi = 300
    )
  }

  # Rank change scatter plots for key comparisons
  scenarios <- unique(ranking_results$rank_data$scenario_id)
  key_comparisons <- list(
    c("RCP45_P3", "RCP85_P3"),  # RCP effect at mid-century
    c("RCP45_P5", "RCP85_P5"),  # RCP effect at end-century
    c("RCP45_P3", "RCP45_P5"),  # Period effect for RCP 4.5
    c("RCP85_P3", "RCP85_P5")   # Period effect for RCP 8.5
  )

  for (comp in key_comparisons) {
    for (score_type in c("std_addall", "std_addfwR", "std_addmigr")) {
      p_scatter <- plot_rank_change_scatter(
        ranking_results$rank_data,
        scenario1 = comp[1],
        scenario2 = comp[2],
        score_type = score_type,
        label_top_n = 10
      )

      comp_name <- paste(comp[1], comp[2], sep = "_vs_")
      ggsave(
        file.path(output_dir, paste0("rank_scatter_", score_type, "_", comp_name, ".png")),
        plot = p_scatter,
        width = 10, height = 8, dpi = 300
      )
    }
  }

  # Indicator driver heatmap
  p_drivers <- plot_indicator_driver_heatmap(ranking_results$indicator_drivers)
  ggsave(
    file.path(output_dir, "indicator_driver_heatmap.png"),
    plot = p_drivers,
    width = 14, height = 10, dpi = 300
  )

  # 4. Export detailed results
  cat("\nSTEP 4: Exporting detailed results...\n")

  # Export indicator sensitivity results
  for (ind in names(indicator_results)) {
    # Summary statistics
    write.csv(
      indicator_results[[ind]]$summary_stats,
      file.path(output_dir, paste0("indicator_summary_", ind, ".csv")),
      row.names = FALSE
    )

    # All pairwise comparisons
    all_comp_summaries <- bind_rows(
      lapply(indicator_results[[ind]]$pairwise_comparisons, function(x) x$summary)
    )
    write.csv(
      all_comp_summaries,
      file.path(output_dir, paste0("indicator_comparisons_", ind, ".csv")),
      row.names = FALSE
    )
  }

  # Export main ranking results (already done by run_rcp_period_sensitivity_analysis)
  export_analysis_results(ranking_results, output_dir = output_dir)

  cat("\n")
  cat("================================================================================\n")
  cat("                       ANALYSIS COMPLETE!                                       \n")
  cat("================================================================================\n")
  cat("\nAll results saved to:", output_dir, "\n\n")

  return(list(
    ranking_results = ranking_results,
    indicator_results = indicator_results,
    output_dir = output_dir
  ))
}


#------------------------------------------------------------------------------
# 4. EXECUTE ANALYSIS
#------------------------------------------------------------------------------

# Run the complete analysis
results <- run_complete_sensitivity_analysis(
  output_dir = "outputs/sensitivity_analysis_complete"
)

# Print summary
cat("\n=== ANALYSIS SUMMARY ===\n\n")
cat("Indicators analyzed:", paste(names(results$indicator_results), collapse = ", "), "\n")
cat("Score types analyzed:",
  paste(unique(results$ranking_results$rank_data$score_type), collapse = ", "), "\n")
cat("Number of CUs:", length(unique(results$ranking_results$rank_data$FULL_CU_IN)), "\n")
cat("Scenarios compared:",
  length(unique(results$ranking_results$rank_data$scenario_id)), "\n")
cat("\nAll outputs saved to:", results$output_dir, "\n")
