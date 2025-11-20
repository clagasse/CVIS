################################################################################
#
# scenario_temporal_sensitivity_analysis.R
#
# Comprehensive analysis of how CU vulnerability ranks and indicator values
# change across:
#   - Emissions scenarios (RCP 4.5 vs RCP 8.5)
#   - Time periods (mid-century vs end-century)
#   - Interactions between scenario and time
#
# Outputs:
#   - Rank correlation matrices
#   - Rank stability metrics
#   - Temporal trends in vulnerability
#   - Scenario divergence analysis
#   - Priority CU changes across scenarios
#
################################################################################

library(tidyverse)
library(patchwork)
library(corrplot)
library(here)

#------------------------------------------------------------------------------
# 1. Data Preparation Functions
#------------------------------------------------------------------------------

#' Prepare vulnerability data across scenarios and time periods
#'
#' @param data Data frame with indicator values (all_flat_std)
#' @param indicators_choose Vector of indicator abbreviations (default: environmental change indicators only)
#' @param aggregation_method Method to aggregate indicators
#' @param use_standardized Use standardized indicator values (TRUE) or raw values (FALSE)
#' @param periods_include Vector of period codes to include (default: c(3, 5) for mid and end century)
#' @param rcps_include Vector of RCP scenarios to include (default: c("45", "85"))
#'
#' @return Data frame with vulnerability scores across all scenarios
#'
prepare_scenario_data <- function(data,
                                  indicators_choose = c("favchange", "tw8proj", "tw8rate",
                                    "migrT", "migrQ", "SSTproj", "SSTrate"),
                                  aggregation_method = "additive",
                                  use_standardized = TRUE,
                                  periods_include = c(3, 5),
                                  rcps_include = c("45", "85")) {

  cat("Preparing vulnerability scores across scenarios...\n")
  cat("Using", ifelse(use_standardized, "standardized", "raw"), "indicator values\n")

  # Filter to selected periods and RCPs
  data <- data %>%
    filter(period_code %in% periods_include,
      rcp %in% rcps_include)

  # Get all unique scenario combinations
  scenarios <- data %>%
    select(rcp, period_code) %>%
    distinct() %>%
    arrange(rcp, period_code)

  cat("Found", nrow(scenarios), "scenario combinations:\n")
  print(scenarios)

  # Prepare indicator columns
  if (use_standardized) {
    indicator_cols <- paste0("std_", indicators_choose)
  } else {
    indicator_cols <- indicators_choose
  }

  # Find available columns
  available_cols <- c()
  for (ind in indicator_cols) {
    if (ind %in% names(data)) {
      available_cols <- c(available_cols, ind)
    } else {
      ind_mean <- paste0(ind, "_mean")
      if (ind_mean %in% names(data)) {
        available_cols <- c(available_cols, ind_mean)
      }
    }
  }

  if (length(available_cols) == 0) {
    stop("No indicator columns found. Check indicator names and whether use_standardized is set correctly.")
  }

  cat("Using", length(available_cols), "indicators:\n")
  cat("  ", paste(available_cols, collapse = ", "), "\n\n")

  # Calculate vulnerability scores for each scenario
  scenario_results <- list()

  for (i in 1:nrow(scenarios)) {
    rcp_i <- scenarios$rcp[i]
    period_i <- scenarios$period_code[i]

    data_i <- data %>%
      filter(rcp == rcp_i, period_code == period_i)

    # Calculate composite score
    if (aggregation_method == "additive") {
      data_i <- data_i %>%
        rowwise() %>%
        mutate(vuln_score = sum(c_across(all_of(available_cols)), na.rm = TRUE)) %>%
        ungroup()
    } else if (aggregation_method == "average") {
      data_i <- data_i %>%
        rowwise() %>%
        mutate(vuln_score = mean(c_across(all_of(available_cols)), na.rm = TRUE)) %>%
        ungroup()
    }

    # Calculate ranks
    data_i <- data_i %>%
      mutate(
        vuln_rank = rank(-vuln_score, ties.method = "average", na.last = "keep"),
        scenario_id = paste0("RCP", rcp_i, "_P", period_i)
      )

    scenario_results[[i]] <- data_i %>%
      select(FULL_CU_IN, CVIS_NAME, SPECIES_NAME, rcp, period_code,
        scenario_id, vuln_score, vuln_rank, all_of(available_cols))
  }

  # Combine all scenarios
  combined_data <- bind_rows(scenario_results)

  return(combined_data)
}


#------------------------------------------------------------------------------
# 2. Rank Correlation Analysis
#------------------------------------------------------------------------------

#' Calculate rank correlations across all scenario pairs
#'
#' @param scenario_data Output from prepare_scenario_data()
#'
#' @return List with correlation matrix and detailed comparisons
#'
calculate_scenario_rank_correlations <- function(scenario_data) {

  cat("\nCalculating rank correlations across scenarios...\n")

  # Create wide format with ranks for each scenario
  rank_wide <- scenario_data %>%
    select(FULL_CU_IN, scenario_id, vuln_rank) %>%
    pivot_wider(names_from = scenario_id, values_from = vuln_rank)

  # Calculate correlation matrix
  rank_matrix <- rank_wide %>%
    select(-FULL_CU_IN) %>%
    as.matrix()

  cor_matrix <- cor(rank_matrix, method = "spearman", use = "pairwise.complete.obs")

  # Calculate pairwise differences
  scenarios <- unique(scenario_data$scenario_id)
  pairwise_comparisons <- list()

  for (i in 1:(length(scenarios) - 1)) {
    for (j in (i + 1):length(scenarios)) {
      scenario1 <- scenarios[i]
      scenario2 <- scenarios[j]

      comparison_data <- rank_wide %>%
        select(FULL_CU_IN, all_of(c(scenario1, scenario2))) %>%
        mutate(
          rank_diff = .data[[scenario2]] - .data[[scenario1]],
          abs_rank_diff = abs(rank_diff)
        )

      pairwise_comparisons[[paste(scenario1, "vs", scenario2)]] <- list(
        scenario1 = scenario1,
        scenario2 = scenario2,
        correlation = cor_matrix[scenario1, scenario2],
        mean_abs_diff = mean(comparison_data$abs_rank_diff, na.rm = TRUE),
        max_abs_diff = max(comparison_data$abs_rank_diff, na.rm = TRUE),
        n_large_changes = sum(comparison_data$abs_rank_diff >= 5, na.rm = TRUE),
        data = comparison_data
      )
    }
  }

  return(list(
    correlation_matrix = cor_matrix,
    rank_data = rank_wide,
    pairwise_comparisons = pairwise_comparisons
  ))
}


#' Plot correlation matrix across scenarios
#'
#' @param cor_analysis Output from calculate_scenario_rank_correlations()
#'
#' @return corrplot object
#'
plot_scenario_correlation_matrix <- function(cor_analysis) {

  corrplot(cor_analysis$correlation_matrix,
    method = "color",
    type = "upper",
    addCoef.col = "black",
    number.cex = 0.8,
    tl.col = "black",
    tl.srt = 45,
    tl.cex = 0.9,
    col = colorRampPalette(c("blue", "white", "red"))(200),
    title = "Rank Correlations Across Scenarios",
    mar = c(0, 0, 2, 0)
  )
}


#------------------------------------------------------------------------------
# 3. Temporal Trends Analysis
#------------------------------------------------------------------------------

#' Analyze temporal trends in vulnerability for each CU
#'
#' @param scenario_data Output from prepare_scenario_data()
#'
#' @return Data frame with temporal trend statistics
#'
analyze_temporal_trends <- function(scenario_data) {

  cat("\nAnalyzing temporal trends...\n")

  # Calculate trends for each RCP separately
  trend_results <- scenario_data %>%
    group_by(FULL_CU_IN, CVIS_NAME, SPECIES_NAME, rcp) %>%
    arrange(period_code) %>%
    summarise(
      n_periods = n(),
      start_score = first(vuln_score),
      end_score = last(vuln_score),
      score_change = end_score - start_score,
      pct_change = 100 * (end_score - start_score) / start_score,
      start_rank = first(vuln_rank),
      end_rank = last(vuln_rank),
      rank_change = end_rank - start_rank,
      trend_direction = case_when(
        score_change > 0 ~ "Increasing",
        score_change < 0 ~ "Decreasing",
        TRUE ~ "Stable"
      ),
      .groups = "drop"
    )

  # Calculate mean trend across periods (if multiple)
  if (length(unique(scenario_data$period_code)) > 1) {
    trend_results <- trend_results %>%
      mutate(
        periods_span = max(scenario_data$period_code) - min(scenario_data$period_code),
        annual_rate = score_change / periods_span
      )
  }

  return(trend_results)
}


#' Plot temporal trends for top vulnerable CUs
#'
#' @param scenario_data Output from prepare_scenario_data()
#' @param top_n Number of CUs to highlight
#' @param by_rcp Plot separately by RCP scenario
#'
#' @return ggplot object
#'
plot_temporal_trends <- function(scenario_data,
                                 top_n = 10,
                                 by_rcp = TRUE) {
  # Identify top vulnerable CUs (based on most recent period, RCP 4.5)
  top_cus <- scenario_data %>%
    filter(rcp == "45") %>%
    group_by(FULL_CU_IN) %>%
    slice_max(period_code, n = 1) %>%
    ungroup() %>%
    slice_min(vuln_rank, n = top_n) %>%
    pull(FULL_CU_IN)

  plot_data <- scenario_data %>%
    filter(FULL_CU_IN %in% top_cus)

  if (by_rcp) {
    p <- ggplot(plot_data, aes(x = period_code, y = vuln_score,
      color = FULL_CU_IN, group = FULL_CU_IN)) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      facet_wrap(~rcp, ncol = 1) +
      labs(
        title = paste("Temporal Trends in Vulnerability (Top", top_n, "CUs)"),
        x = "Time Period",
        y = "Vulnerability Score",
        color = "Conservation Unit"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 14),
        legend.position = "right"
      )
  } else {
    p <- ggplot(plot_data, aes(x = period_code, y = vuln_score,
      color = rcp, group = interaction(FULL_CU_IN, rcp))) +
      geom_line(aes(linetype = FULL_CU_IN), linewidth = 1) +
      geom_point(size = 2) +
      labs(
        title = paste("Temporal Trends in Vulnerability (Top", top_n, "CUs)"),
        x = "Time Period",
        y = "Vulnerability Score",
        color = "RCP Scenario",
        linetype = "CU"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 14),
        legend.position = "right"
      )
  }

  return(p)
}


#------------------------------------------------------------------------------
# 4. Scenario Divergence Analysis
#------------------------------------------------------------------------------

#' Compare RCP 4.5 vs RCP 8.5 for each time period
#'
#' @param scenario_data Output from prepare_scenario_data()
#'
#' @return Data frame with scenario divergence metrics
#'
analyze_scenario_divergence <- function(scenario_data) {

  cat("\nAnalyzing scenario divergence...\n")

  # Compare RCP scenarios for each period
  divergence_results <- scenario_data %>%
    select(FULL_CU_IN, CVIS_NAME, SPECIES_NAME, rcp, period_code,
      vuln_score, vuln_rank) %>%
    pivot_wider(
      names_from = rcp,
      values_from = c(vuln_score, vuln_rank),
      names_glue = "{.value}_rcp{rcp}"
    ) %>%
    mutate(
      score_diff = vuln_score_rcp85 - vuln_score_rcp45,
      score_pct_diff = 100 * (vuln_score_rcp85 - vuln_score_rcp45) / vuln_score_rcp45,
      rank_diff = vuln_rank_rcp85 - vuln_rank_rcp45,
      abs_rank_diff = abs(rank_diff),
      higher_in_rcp85 = vuln_score_rcp85 > vuln_score_rcp45,
      divergence_magnitude = case_when(
        abs(score_diff) < 0.1 ~ "Small",
        abs(score_diff) < 0.3 ~ "Moderate",
        TRUE ~ "Large"
      )
    )

  return(divergence_results)
}


#' Plot scenario divergence over time
#'
#' @param divergence_data Output from analyze_scenario_divergence()
#'
#' @return ggplot object
#'
plot_scenario_divergence <- function(divergence_data) {

  p1 <- ggplot(divergence_data, aes(x = factor(period_code), y = score_diff,
    fill = SPECIES_NAME)) +
    geom_boxplot(alpha = 0.7) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(
      title = "Scenario Divergence Over Time",
      subtitle = "RCP 8.5 minus RCP 4.5",
      x = "Time Period",
      y = "Vulnerability Score Difference",
      fill = "Species"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold", size = 14))

  p2 <- ggplot(divergence_data, aes(x = vuln_score_rcp45, y = vuln_score_rcp85,
    color = SPECIES_NAME)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    geom_point(aes(shape = factor(period_code)), size = 3, alpha = 0.7) +
    labs(
      title = "RCP 4.5 vs RCP 8.5 Vulnerability",
      x = "Vulnerability Score (RCP 4.5)",
      y = "Vulnerability Score (RCP 8.5)",
      color = "Species",
      shape = "Period"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold", size = 14))

  return(p1 / p2)
}


#------------------------------------------------------------------------------
# 5. Priority CU Changes
#------------------------------------------------------------------------------

#' Identify CUs that change priority status across scenarios
#'
#' @param scenario_data Output from prepare_scenario_data()
#' @param top_n Number of top-ranked CUs to consider as "priority"
#'
#' @return List with changing priorities analysis
#'
identify_priority_changes <- function(scenario_data,
                                      top_n = 10) {

  cat("\nIdentifying priority CU changes...\n")

  # Get top N CUs for each scenario
  priority_by_scenario <- scenario_data %>%
    group_by(scenario_id) %>%
    slice_min(vuln_rank, n = top_n) %>%
    ungroup()

  # Count how many scenarios each CU appears in top N
  priority_frequency <- priority_by_scenario %>%
    group_by(FULL_CU_IN, CVIS_NAME, SPECIES_NAME) %>%
    summarise(
      n_scenarios_in_top = n(),
      scenarios = paste(scenario_id, collapse = ", "),
      .groups = "drop"
    ) %>%
    arrange(desc(n_scenarios_in_top))

  # Identify consistent vs variable priorities
  n_scenarios <- length(unique(scenario_data$scenario_id))

  consistent_priorities <- priority_frequency %>%
    filter(n_scenarios_in_top == n_scenarios)

  variable_priorities <- priority_frequency %>%
    filter(n_scenarios_in_top > 0 & n_scenarios_in_top < n_scenarios)

  scenario_specific <- priority_frequency %>%
    filter(n_scenarios_in_top == 1)

  return(list(
    priority_frequency = priority_frequency,
    consistent_priorities = consistent_priorities,
    variable_priorities = variable_priorities,
    scenario_specific = scenario_specific,
    n_scenarios = n_scenarios,
    top_n = top_n
  ))
}


#' Plot priority changes across scenarios
#'
#' @param priority_analysis Output from identify_priority_changes()
#'
#' @return ggplot object
#'
plot_priority_changes <- function(priority_analysis) {

  plot_data <- priority_analysis$priority_frequency %>%
    mutate(
      priority_type = case_when(
        n_scenarios_in_top == priority_analysis$n_scenarios ~ "Consistent Priority",
        n_scenarios_in_top == 1 ~ "Scenario-Specific",
        TRUE ~ "Variable Priority"
      )
    )

  ggplot(plot_data, aes(x = reorder(CVIS_NAME, n_scenarios_in_top),
    y = n_scenarios_in_top,
    fill = priority_type)) +
    geom_col() +
    geom_hline(yintercept = priority_analysis$n_scenarios / 2,
      linetype = "dashed", color = "gray50") +
    coord_flip() +
    scale_fill_manual(
      values = c("Consistent Priority" = "darkgreen",
        "Variable Priority" = "orange",
        "Scenario-Specific" = "red")
    ) +
    labs(
      title = paste("Priority CU Consistency Across Scenarios (Top",
        priority_analysis$top_n, ")"),
      subtitle = paste("Out of", priority_analysis$n_scenarios, "total scenarios"),
      x = "Conservation Unit",
      y = "Number of Scenarios Where CU is Top Priority",
      fill = "Priority Type"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      axis.text.y = element_text(size = 8)
    )
}


#------------------------------------------------------------------------------
# 6. Indicator-Specific Scenario Sensitivity
#------------------------------------------------------------------------------

#' Analyze how individual indicators vary across scenarios
#'
#' @param scenario_data Output from prepare_scenario_data()
#' @param indicators_choose Vector of indicators to analyze
#' @param use_standardized Whether the scenario_data contains standardized or raw indicators
#'
#' @return Data frame with indicator sensitivity metrics
#'
analyze_indicator_scenario_sensitivity <- function(scenario_data,
                                                   indicators_choose = c("favchange", "tw8proj", "tw8rate",
                                                     "migrT", "migrQ", "SSTproj", "SSTrate"),
                                                   use_standardized = TRUE) {

  cat("\nAnalyzing indicator-specific scenario sensitivity...\n")

  # Prepare indicator columns
  if (use_standardized) {
    indicator_cols <- paste0("std_", indicators_choose)
  } else {
    indicator_cols <- indicators_choose
  }

  available_cols <- c()

  for (ind in indicator_cols) {
    if (ind %in% names(scenario_data)) {
      available_cols <- c(available_cols, ind)
    } else {
      ind_mean <- paste0(ind, "_mean")
      if (ind_mean %in% names(scenario_data)) {
        available_cols <- c(available_cols, ind_mean)
      }
    }
  }

  if (length(available_cols) == 0) {
    cat("Warning: No indicator columns found for sensitivity analysis\n")
    return(NULL)
  }

  # Calculate coefficient of variation for each indicator across scenarios
  indicator_sensitivity <- scenario_data %>%
    select(FULL_CU_IN, CVIS_NAME, SPECIES_NAME, all_of(available_cols)) %>%
    group_by(FULL_CU_IN, CVIS_NAME, SPECIES_NAME) %>%
    summarise(
      across(
        all_of(available_cols),
        list(
          mean = ~ mean(.x, na.rm = TRUE),
          sd = ~ sd(.x, na.rm = TRUE),
          cv = ~ sd(.x, na.rm = TRUE) / abs(mean(.x, na.rm = TRUE))
        ),
        .names = "{.col}_{.fn}"
      ),
      .groups = "drop"
    )

  # Summary by indicator
  indicator_summary <- map_dfr(available_cols, function(ind) {
    cv_col <- paste0(ind, "_cv")

    tibble(
      indicator = ind,
      mean_cv = mean(indicator_sensitivity[[cv_col]], na.rm = TRUE),
      median_cv = median(indicator_sensitivity[[cv_col]], na.rm = TRUE),
      max_cv = max(indicator_sensitivity[[cv_col]], na.rm = TRUE),
      n_high_cv = sum(indicator_sensitivity[[cv_col]] > 0.2, na.rm = TRUE)
    )
  }) %>%
    arrange(desc(mean_cv))

  return(list(
    cu_level = indicator_sensitivity,
    indicator_summary = indicator_summary
  ))
}


#' Plot indicator scenario sensitivity
#'
#' @param sensitivity_analysis Output from analyze_indicator_scenario_sensitivity()
#'
#' @return ggplot object
#'
plot_indicator_sensitivity <- function(sensitivity_analysis) {

  ggplot(sensitivity_analysis$indicator_summary,
    aes(x = reorder(indicator, mean_cv), y = mean_cv)) +
    geom_col(fill = "steelblue", alpha = 0.8) +
    geom_hline(yintercept = 0.2, linetype = "dashed", color = "red") +
    coord_flip() +
    labs(
      title = "Indicator Sensitivity to Scenarios",
      subtitle = "Mean coefficient of variation across scenarios",
      x = "Indicator",
      y = "Mean CV (higher = more variable across scenarios)"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold", size = 14))
}


#------------------------------------------------------------------------------
# 7. Comprehensive Analysis Function
#------------------------------------------------------------------------------

#' Run complete scenario/temporal sensitivity analysis
#'
#' @param data Data frame with indicator values (all_flat_std)
#' @param indicators_choose Vector of indicator abbreviations (default: environmental change indicators)
#' @param aggregation_method Aggregation method
#' @param use_standardized Use standardized (TRUE) or raw (FALSE) indicator values
#' @param periods_include Vector of period codes to include (default: c(3, 5))
#' @param rcps_include Vector of RCP scenarios to include (default: c("45", "85"))
#' @param output_dir Directory to save results
#'
#' @return List with all analysis results
#'
run_scenario_temporal_analysis <- function(data,
                                           indicators_choose = c("favchange", "tw8proj", "tw8rate",
                                             "migrT", "migrQ", "SSTproj", "SSTrate"),
                                           aggregation_method = "additive",
                                           use_standardized = TRUE,
                                           periods_include = c(3, 5),
                                           rcps_include = c("45", "85"),
                                           output_dir = "outputs/scenario_analysis") {

  cat("\n========================================\n")
  cat("Scenario & Temporal Sensitivity Analysis\n")
  cat("========================================\n\n")
  cat("Indicators:", paste(indicators_choose, collapse = ", "), "\n")
  cat("Using", ifelse(use_standardized, "standardized", "raw"), "values\n")
  cat("Periods:", paste(periods_include, collapse = ", "), "\n")
  cat("RCP scenarios:", paste(rcps_include, collapse = ", "), "\n\n")

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # 1. Prepare data across scenarios
  scenario_data <- prepare_scenario_data(
    data,
    indicators_choose = indicators_choose,
    aggregation_method = aggregation_method,
    use_standardized = use_standardized,
    periods_include = periods_include,
    rcps_include = rcps_include
  )

  # 2. Rank correlations
  cat("\n--- Rank Correlation Analysis ---\n")
  rank_correlations <- calculate_scenario_rank_correlations(scenario_data)

  cat("\nRank Correlation Matrix:\n")
  print(round(rank_correlations$correlation_matrix, 3))

  # 3. Temporal trends
  cat("\n--- Temporal Trends Analysis ---\n")
  temporal_trends <- analyze_temporal_trends(scenario_data)

  trend_summary <- temporal_trends %>%
    group_by(rcp, trend_direction) %>%
    summarise(n_CUs = n(), .groups = "drop")

  cat("\nTemporal Trend Summary:\n")
  print(trend_summary)

  # 4. Scenario divergence (only if both RCPs are included)
  divergence <- NULL
  divergence_summary <- NULL

  if (length(rcps_include) >= 2) {
    cat("\n--- Scenario Divergence Analysis ---\n")
    divergence <- analyze_scenario_divergence(scenario_data)

    divergence_summary <- divergence %>%
      group_by(period_code) %>%
      summarise(
        mean_score_diff = mean(score_diff, na.rm = TRUE),
        mean_abs_rank_diff = mean(abs_rank_diff, na.rm = TRUE),
        n_large_divergence = sum(divergence_magnitude == "Large", na.rm = TRUE),
        .groups = "drop"
      )

    cat("\nScenario Divergence Summary:\n")
    print(divergence_summary)
  } else {
    cat("\n--- Skipping Scenario Divergence (only one RCP included) ---\n")
  }

  # 5. Priority changes
  cat("\n--- Priority CU Changes ---\n")
  priority_changes <- identify_priority_changes(scenario_data, top_n = 10)

  cat("\nConsistent priorities (all scenarios):",
    nrow(priority_changes$consistent_priorities), "\n")
  cat("Variable priorities:",
    nrow(priority_changes$variable_priorities), "\n")
  cat("Scenario-specific:",
    nrow(priority_changes$scenario_specific), "\n")

  # 6. Indicator sensitivity
  cat("\n--- Indicator Scenario Sensitivity ---\n")
  indicator_sensitivity <- analyze_indicator_scenario_sensitivity(
    scenario_data = scenario_data,
    indicators_choose = indicators_choose,
    use_standardized = use_standardized
  )

  if (!is.null(indicator_sensitivity)) {
    cat("\nMost scenario-sensitive indicators:\n")
    print(head(indicator_sensitivity$indicator_summary, 5))
  }

  cat("\nAnalysis complete!\n\n")

  # Return all results
  return(list(
    scenario_data = scenario_data,
    rank_correlations = rank_correlations,
    temporal_trends = temporal_trends,
    divergence = divergence,
    divergence_summary = divergence_summary,
    priority_changes = priority_changes,
    indicator_sensitivity = indicator_sensitivity,
    metadata = list(
      n_CUs = length(unique(scenario_data$FULL_CU_IN)),
      n_scenarios = length(unique(scenario_data$scenario_id)),
      indicators = indicators_choose,
      aggregation_method = aggregation_method,
      use_standardized = use_standardized,
      periods = periods_include,
      rcps = rcps_include,
      date_run = Sys.Date()
    )
  ))
}


#------------------------------------------------------------------------------
# 8. Export Functions
#------------------------------------------------------------------------------

#' Export all scenario analysis results
#'
#' @param analysis_results Output from run_scenario_temporal_analysis()
#' @param output_dir Directory to save results
#'
export_scenario_analysis_results <- function(analysis_results,
                                             output_dir = "outputs/scenario_analysis") {

  timestamp <- format(Sys.Date(), "%Y-%m-%d")

  # Export main data
  write.csv(
    analysis_results$scenario_data,
    file.path(output_dir, paste0("scenario_vulnerability_data_", timestamp, ".csv")),
    row.names = FALSE
  )

  # Export rank correlations
  write.csv(
    analysis_results$rank_correlations$correlation_matrix,
    file.path(output_dir, paste0("rank_correlation_matrix_", timestamp, ".csv"))
  )

  write.csv(
    analysis_results$rank_correlations$rank_data,
    file.path(output_dir, paste0("ranks_all_scenarios_", timestamp, ".csv")),
    row.names = FALSE
  )

  # Export temporal trends
  write.csv(
    analysis_results$temporal_trends,
    file.path(output_dir, paste0("temporal_trends_", timestamp, ".csv")),
    row.names = FALSE
  )

  # Export divergence (if available)
  if (!is.null(analysis_results$divergence)) {
    write.csv(
      analysis_results$divergence,
      file.path(output_dir, paste0("scenario_divergence_", timestamp, ".csv")),
      row.names = FALSE
    )
  }

  # Export priority changes
  write.csv(
    analysis_results$priority_changes$priority_frequency,
    file.path(output_dir, paste0("priority_frequency_", timestamp, ".csv")),
    row.names = FALSE
  )

  write.csv(
    analysis_results$priority_changes$consistent_priorities,
    file.path(output_dir, paste0("consistent_priorities_", timestamp, ".csv")),
    row.names = FALSE
  )

  # Export indicator sensitivity (if available)
  if (!is.null(analysis_results$indicator_sensitivity)) {
    write.csv(
      analysis_results$indicator_sensitivity$indicator_summary,
      file.path(output_dir, paste0("indicator_scenario_sensitivity_", timestamp, ".csv")),
      row.names = FALSE
    )
  }

  # Create plots
  png(file.path(output_dir, paste0("rank_correlation_matrix_", timestamp, ".png")),
    width = 10, height = 8, units = "in", res = 300)
  plot_scenario_correlation_matrix(analysis_results$rank_correlations)
  dev.off()

  ggsave(
    file.path(output_dir, paste0("temporal_trends_", timestamp, ".png")),
    plot = plot_temporal_trends(analysis_results$scenario_data),
    width = 12, height = 10, dpi = 300
  )

  # Only create divergence plots if data available
  if (!is.null(analysis_results$divergence)) {
    ggsave(
      file.path(output_dir, paste0("scenario_divergence_", timestamp, ".png")),
      plot = plot_scenario_divergence(analysis_results$divergence),
      width = 12, height = 10, dpi = 300
    )
  }

  ggsave(
    file.path(output_dir, paste0("priority_changes_", timestamp, ".png")),
    plot = plot_priority_changes(analysis_results$priority_changes),
    width = 10, height = 12, dpi = 300
  )

  if (!is.null(analysis_results$indicator_sensitivity)) {
    ggsave(
      file.path(output_dir, paste0("indicator_sensitivity_", timestamp, ".png")),
      plot = plot_indicator_sensitivity(analysis_results$indicator_sensitivity),
      width = 10, height = 8, dpi = 300
    )
  }

  cat("\nAll scenario analysis results exported to:", output_dir, "\n")
}


#------------------------------------------------------------------------------
# Example Usage
#------------------------------------------------------------------------------

# ============= BASIC USAGE (Environmental Change Indicators Only) =============

# Run analysis with DEFAULT settings:
# - Only environmental change indicators (not demographic/sensitivity)
# - Standardized values
# - Periods 3 and 5 only (mid-century and end-century)
# - RCP 4.5 and 8.5
scenario_results <- run_scenario_temporal_analysis(
  data = all_flat_std,
  output_dir = "outputs/scenario_analysis"
)

# View key findings
cat("\n=== Rank Correlations ===\n")
print(scenario_results$rank_correlations$correlation_matrix)

cat("\n=== Most Scenario-Sensitive Indicators ===\n")
print(scenario_results$indicator_sensitivity$indicator_summary)

cat("\n=== Consistent Priority CUs ===\n")
print(scenario_results$priority_changes$consistent_priorities)

# Create visualizations
plot_scenario_correlation_matrix(scenario_results$rank_correlations)
plot_temporal_trends(scenario_results$scenario_data, top_n = 10)
plot_scenario_divergence(scenario_results$divergence)
plot_priority_changes(scenario_results$priority_changes)
plot_indicator_sensitivity(scenario_results$indicator_sensitivity)

# Export everything
export_scenario_analysis_results(
  scenario_results,
  output_dir = "outputs/scenario_analysis"
)


# ============= USING RAW (NON-STANDARDIZED) INDICATORS =============

# Run with raw indicator values instead of standardized
scenario_results_raw <- run_scenario_temporal_analysis(
  data = all_flat_std,
  indicators_choose = c("favchange", "tw8proj", "tw8rate", "migrT", "migrQ", "SSTproj", "SSTrate"),
  use_standardized = FALSE,  # Use raw values
  output_dir = "outputs/scenario_analysis/raw_indicators"
)


# ============= CUSTOM INDICATOR SET =============

# Use all indicators (including demographic/sensitivity)
scenario_results_all <- run_scenario_temporal_analysis(
  data = all_flat_std,
  indicators_choose = tbl_indicators$abbrev,  # All indicators
  use_standardized = TRUE,
  output_dir = "outputs/scenario_analysis/all_indicators"
)

# Use only freshwater indicators
fw_indicators <- c("favchange", "tw8proj", "tw8rate")
scenario_results_fw <- run_scenario_temporal_analysis(
  data = all_flat_std,
  indicators_choose = fw_indicators,
  use_standardized = TRUE,
  output_dir = "outputs/scenario_analysis/freshwater_only"
)


# ============= SINGLE RCP OR PERIOD =============

# Analyze only RCP 8.5 across time periods
scenario_results_rcp85 <- run_scenario_temporal_analysis(
  data = all_flat_std,
  rcps_include = "85",
  periods_include = c(3, 5),
  output_dir = "outputs/scenario_analysis/rcp85_only"
)

# Analyze only mid-century across RCP scenarios
scenario_results_midcentury <- run_scenario_temporal_analysis(
  data = all_flat_std,
  rcps_include = c("45", "85"),
  periods_include = 3,
  output_dir = "outputs/scenario_analysis/midcentury_only"
)


# ============= DETAILED EXAMINATION =============

# Compare specific scenario pairs
comp_name <- "RCP45_P3 vs RCP85_P3"
comparison <- scenario_results$rank_correlations$pairwise_comparisons[[comp_name]]

cat("\n=== Comparison:", comp_name, "===\n")
cat("Correlation:", round(comparison$correlation, 3), "\n")
cat("Mean absolute rank difference:", round(comparison$mean_abs_diff, 2), "\n")
cat("CUs with large rank changes (>5):", comparison$n_large_changes, "\n")

# View CUs with largest rank changes
large_changes <- comparison$data %>%
  arrange(desc(abs_rank_diff)) %>%
  head(10)

print(large_changes)

# Examine temporal trends for specific species
chinook_trends <- scenario_results$temporal_trends %>%
  filter(SPECIES_NAME == "Chinook") %>%
  arrange(desc(abs(score_change)))

cat("\n=== Chinook with largest temporal changes ===\n")
print(head(chinook_trends, 10))


#------------------------------------------------------------------------------
# Example Usage
#------------------------------------------------------------------------------

# Run comprehensive scenario analysis
scenario_results <- run_scenario_temporal_analysis(
  data = all_flat_std,
  indicators_choose = c("favchange", "tw8proj", "tw8rate",
    "migrT", "migrQ", "SSTproj", "SSTrate"),
  aggregation_method = "additive",
  use_standardized = FALSE,
  output_dir = "outputs/scenario_analysis"
)

# Use all indicators (including demographic/sensitivity)
scenario_results_all <- run_scenario_temporal_analysis(
  data = all_flat_std,
  indicators_choose = tbl_indicators$abbrev,  # All indicators
  use_standardized = TRUE,
  output_dir = "outputs/scenario_analysis/all_indicators"
)


# View key findings
cat("\n=== Rank Correlations ===\n")
print(scenario_results$rank_correlations$correlation_matrix)

cat("\n=== Most Scenario-Sensitive Indicators ===\n")
print(scenario_results$indicator_sensitivity$indicator_summary)

cat("\n=== Consistent Priority CUs ===\n")
print(scenario_results$priority_changes$consistent_priorities)

# Create visualizations
plot_scenario_correlation_matrix(scenario_results$rank_correlations)
plot_temporal_trends(scenario_results$scenario_data, top_n = 10)
plot_scenario_divergence(scenario_results$divergence)
plot_priority_changes(scenario_results$priority_changes)
plot_indicator_sensitivity(scenario_results$indicator_sensitivity)

# Export everything
export_scenario_analysis_results(
  scenario_results,
  output_dir = "outputs/scenario_analysis"
)

# Detailed examination of specific comparisons
# Compare RCP 4.5 mid-century vs RCP 8.5 mid-century
comp_name <- "RCP45_P3 vs RCP85_P3"
comparison <- scenario_results$rank_correlations$pairwise_comparisons[[comp_name]]

cat("\n=== Comparison:", comp_name, "===\n")
cat("Correlation:", round(comparison$correlation, 3), "\n")
cat("Mean absolute rank difference:", round(comparison$mean_abs_diff, 2), "\n")
cat("CUs with large rank changes (>5):", comparison$n_large_changes, "\n")

# View CUs with largest rank changes
large_changes <- comparison$data %>%
  arrange(desc(abs_rank_diff)) %>%
  head(10)

print(large_changes)
