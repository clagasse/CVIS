# ==============================================================================
# CVIS Sensitivity and Uncertainty Plotting Library (5c_plots_sensitivity_indicators.R)
#
# Description:
#   A consolidated library of plotting functions for CVIS sensitivity and
#   uncertainty analysis.
#   - Sourcing this file defines the plotting functions for inline rendering (e.g. in 6b).
#   - Running this file directly (standalone) executes the functions and saves
#     all PNG figures to the output directory.
# ==============================================================================

library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(corrplot)
library(patchwork)
library(scico)
library(reshape2)
library(ggrepel)

# ==================== 1. Indicator-Level Plotting Functions ====================

# 1a. Indicator-Level Sensitivity (MAD Bar Chart)
plot_indicator_sensitivity <- function(ind_sens_summary, source_colors = sens_source_palette) {
  ggplot(ind_sens_summary %>% filter(SPECIES_NAME == "ALL"), 
         aes(x = reorder(indicator, mean_abs_dev, mean), y = mean_abs_dev, fill = source)) +
    geom_bar(stat = "identity", position = "dodge") +
    coord_flip() +
    facet_wrap(~category, scales = "free_y",
               labeller = labeller(category = get("cat_label_map", envir = .GlobalEnv))) +
    scale_fill_manual(values = source_colors, na.value = "grey50", name = "Source of variation") +
    labs(
      title = "Indicator Sensitivity to Climate and Downscaling Method Variation",
      subtitle = "Average absolute deviation from default across all CUs",
      x = "Indicator",
      y = "Mean Absolute Deviation"
    ) +
    theme_cvis() +
    theme(legend.position = "bottom")
}

# 1b. Indicator Directional Shift Violins
plot_indicator_directional_shifts <- function(overall_sensitivity, tbl_indicators, source_colors = sens_source_palette) {
  ind_source_levels <- c("Baseline", "GCM1", "GCM4", "GCM6", "RCP45_P5", "RCP85_P3", "RCP85_P5", "dsmethod", "stdmethod")
  ind_source_labels <- c("Default Scenario", "CanESM2 (GCM 1)", "HadGEM2 (GCM 4)", "MPI (GCM 6)", "RCP 4.5, End-century (P5)", "RCP 8.5, Mid-century", "RCP 8.5, End-century", "Downscaling Method", "Standardize Meth")

  # Fetch category name and color lookups with safe global fallbacks
  cat_names <- if (exists("cat_label_map", envir = .GlobalEnv)) {
    get("cat_label_map", envir = .GlobalEnv)
  } else {
    c("fwrs" = "Spawning & Rearing", "migr" = "Upstream Migration", "mar"  = "Nearshore Marine", "dem"  = "Demographics", "gen"  = "Genetics")
  }
  
  cat_colors <- if (exists("indicator_palette", envir = .GlobalEnv)) {
    get("indicator_palette", envir = .GlobalEnv)
  } else {
    c("Demographics" = "#9E6B7A", "Spawning & Rearing" = "#8AA382", "Upstream Migration" = "#7D8CA3", "Nearshore Marine" = "#698B93", "Genetics" = "#D9946C")
  }

  # Arrange indicators by category order and abbrev
  ordered_indicators <- tbl_indicators %>%
    arrange(factor(category, levels = names(cat_names)), abbrev) %>%
    pull(abbrev)

  ind_shift_cus <- overall_sensitivity$indicator_metrics %>%
    filter(FULL_CU_IN != "ALL") %>%
    mutate(
        val_Baseline = base_raw_mean,
        val_GCM1 = base_raw_mean + raw_dev_GCM1,
        val_GCM4 = base_raw_mean + raw_dev_GCM4,
        val_GCM6 = base_raw_mean + raw_dev_GCM6,
        val_RCP45_P5 = base_raw_mean + raw_dev_RCP45_P5,
        val_RCP85_P3 = base_raw_mean + raw_dev_RCP85_P3,
        val_RCP85_P5 = base_raw_mean + raw_dev_RCP85_P5,
        val_dsmethod = base_raw_mean + raw_dev_dsmethod,
        val_stdmethod = base_raw_mean + raw_dev_stdmethod
    ) %>%
    select(FULL_CU_IN, indicator, category, base_raw_mean, starts_with("val_")) %>%
    pivot_longer(cols = starts_with("val_"), names_to = "source", names_prefix = "val_", values_to = "val") %>%
    filter(!is.na(val)) %>%
    group_by(indicator, source) %>%
    mutate(has_variation = source == "Baseline" | any(abs(val - base_raw_mean) > 1e-10, na.rm = TRUE)) %>%
    filter(has_variation) %>%
    group_by(indicator) %>% filter(n_distinct(source) > 1) %>% ungroup() %>%
    filter(source %in% ind_source_levels) %>%
    mutate(
      source = factor(source, levels = rev(ind_source_levels), labels = rev(ind_source_labels)),
      indicator = factor(indicator, levels = intersect(ordered_indicators, unique(indicator)))
    )
  
  baseline_refs <- ind_shift_cus %>% 
    group_by(indicator) %>% 
    summarise(ref_mean = mean(base_raw_mean, na.rm = TRUE), .groups = "drop")
  
  # Format facet labels as colored HTML strings
  ind_label_units <- tbl_indicators %>% 
    mutate(
      cat_long = cat_names[category],
      color = coalesce(cat_colors[cat_long], "black"),
      facet_label = paste0("<span style='color:", color, "'><strong>", abbrev, "</strong></span><br><span style='color:", color, ";font-size:6.5pt'>(", unit_short, ")</span>")
    ) %>% 
    select(abbrev, facet_label) %>% 
    tibble::deframe()
  
  shift_colors <- c("Baseline" = "black", source_colors)
  names(shift_colors) <- sapply(names(shift_colors), function(x) {
    idx <- match(x, ind_source_levels)
    if (!is.na(idx)) ind_source_labels[idx] else x
  })

  ggplot(ind_shift_cus, aes(y = source, x = val, fill = source, color = source)) +
    geom_vline(data = baseline_refs, aes(xintercept = ref_mean), linetype = "dashed", color = "grey30", alpha = 0.6) +
    geom_violin(alpha = 0.7, scale = "width", draw_quantiles = c(0.25, 0.5, 0.75), linewidth = 0.6) +
    facet_wrap(~indicator, scales = "free_x", ncol = 5, labeller = labeller(indicator = ind_label_units)) +
    scale_fill_manual(values = shift_colors, na.value = "grey50", guide = "none") +
    scale_color_manual(values = shift_colors, na.value = "grey50", guide = "none") +
    labs(x = "Raw Indicator Value (units vary)", y = "Source of variation") +
    theme_cvis() +
    theme(
      strip.text = ggtext::element_markdown(size = 7.5, lineheight = 1.1),
      axis.text.y = element_text(size = 8),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 7)
    )
}

# 1c. Indicator XY Sensitivity Plot (Raw vs. Standardized)
plot_indicator_xy_sensitivity <- function(overall_sensitivity, source_colors) {
  ind_xy_dat <- overall_sensitivity$indicator_metrics %>%
    filter(FULL_CU_IN != "ALL") %>%
    filter(!is.na(abs_std_dev_GCM1)) %>%
    mutate(
      valraw_Baseline = base_raw_mean, valstd_Baseline = base_std_mean,
      valraw_GCM1 = base_raw_mean + raw_dev_GCM1, valstd_GCM1 = base_std_mean + std_dev_GCM1,
      valraw_GCM4 = base_raw_mean + raw_dev_GCM4, valstd_GCM4 = base_std_mean + std_dev_GCM4,
      valraw_GCM6 = base_raw_mean + raw_dev_GCM6, valstd_GCM6 = base_std_mean + std_dev_GCM6,
      valraw_dsmethod = base_raw_mean + raw_dev_dsmethod, valstd_dsmethod = base_std_mean + std_dev_dsmethod,
      valraw_stdmethod = base_raw_mean + raw_dev_stdmethod, valstd_stdmethod = base_std_mean + std_dev_stdmethod
    ) %>%
    select(FULL_CU_IN, SPECIES_NAME, category, indicator, starts_with("valraw_"), starts_with("valstd_")) %>%
    pivot_longer(cols = starts_with("valraw_") | starts_with("valstd_"), names_to = c(".value", "source"), names_sep = "_") %>%
    rename(raw = valraw, std = valstd) %>% filter(!is.na(raw))
  
  source_shapes <- c("Baseline" = 16, "GCM1" = 17, "GCM4" = 18, "GCM6" = 15, "dsmethod" = 13, "stdmethod" = 8)
  xy_colors <- c("Baseline" = "black", source_colors)

  ggplot(ind_xy_dat, aes(x = raw, y = std, color = source, shape = source)) +
    geom_point(alpha = 0.3, size = 1.2) +
    stat_ellipse(aes(group = source), level = 0.90, linetype = "dashed", linewidth = 0.4) +
    stat_summary(fun = mean, geom = "point", size = 4, alpha = 1, stroke = 1.5) +
    facet_wrap(~indicator, scales = "free", ncol = 4) +
    scale_color_manual(values = xy_colors) + 
    scale_shape_manual(values = source_shapes) +
    labs(title = "Indicator Sensitivity: Raw vs. Standardized Risk Response", 
         subtitle = "Points show CUs; Large points show mean result. Ellipses span 90% of distribution.",
         x = "Raw Indicator Value", y = "Standardized Risk Score (0-1)") +
    theme_cvis() + 
    theme(legend.position = "bottom", strip.text = element_text(size = 8))
}

# 1d. Indicator Redundancy (Pearson Correlation Pie)
plot_indicator_redundancy_corr <- function(cor_matrix_ind) {
  corrplot(cor_matrix_ind, method = "pie", type = "lower", mar = c(0, 0, 1, 0))
}

# 1e. Indicator Downscaling Method Deviations Boxplots
plot_indicator_downscaling_deviations <- function(overall_sensitivity) {
  ind_dat <- overall_sensitivity$indicator_metrics %>%
    filter(FULL_CU_IN != "ALL", indicator %in% c("flow8pdelta", "flow18pdelta", "tw8proj", "tw8rate")) %>%
    mutate(
      val_Default = base_raw_mean,
      val_Alternative = base_raw_mean + raw_dev_dsmethod
    ) %>%
    tidyr::pivot_longer(cols = c(val_Default, val_Alternative), names_to = "scenario", names_prefix = "val_", values_to = "raw_val") %>%
    mutate(
      scenario = factor(scenario, levels = c("Default", "Alternative")),
      indicator_label = case_when(
        indicator == "flow8pdelta" ~ "August Stream Flow Change (fraction)",
        indicator == "flow18pdelta" ~ "Winter Stream Flow Change (fraction)",
        indicator == "tw8proj" ~ "August Projected Stream Temp (°C)",
        indicator == "tw8rate" ~ "August Stream Temp Warming Rate (°C/decade)",
        TRUE ~ indicator
      )
    )
  
  ggplot(ind_dat, aes(x = scenario, y = raw_val)) +
    geom_violin(alpha = 0.4, fill = "grey95", color = "grey60", scale = "width", width = 0.5) +
    geom_line(aes(group = FULL_CU_IN), color = "grey70", alpha = 0.5, linewidth = 0.4) +
    geom_point(aes(color = std_dev_dsmethod), alpha = 0.85, size = 2) +
    facet_wrap(~indicator_label, scales = "free_y", ncol = 2) +
    scale_color_gradient2(
      low = "#3060AF", 
      mid = "#EBCC5A", 
      high = "#C21A1D", 
      midpoint = 0,
      name = "Change in Standardized Risk Score",
      limits = c(-1, 1),
      breaks = c(-1, -0.5, 0, 0.5, 1)
    ) +
    labs(
      x = "Downscaling Model Scenario",
      y = "Raw Indicator Value"
    ) +
    theme_cvis() +
    theme(
      strip.text = element_text(size = 9.5, face = "bold"),
      axis.title.y = element_text(size = 10),
      axis.title.x = element_text(size = 10),
      legend.position = "bottom",
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 8)
    )
}




# ==================== 2. Score-Level Plotting Functions ====================

# 2a. Overall and Category Score Deviations
plot_score_deviations <- function(deviations, source_colors = sens_source_palette, cat_label_mapping = cat_label_map) {
  dev_raw <- deviations %>%
    select(FULL_CU_IN, category, starts_with("raw_dev_")) %>%
    pivot_longer(cols = starts_with("raw_dev_"), names_to = "source_label", values_to = "raw_deviation") %>%
    mutate(
        source_label = str_remove(source_label, "raw_dev_"),
        source_type = case_when(
            str_detect(source_label, "^GCM") ~ "GCM",
            str_detect(source_label, "^RCP") ~ "Scenario",
            str_detect(source_label, "^Method") ~ "Method",
            str_detect(source_label, "^Model") ~ "dsmethod",
            str_detect(source_label, "^dsmethod") ~ "dsmethod",
            str_detect(source_label, "^stdmethod") ~ "stdmethod",
            TRUE ~ "Other"
        ),
        source = case_when(
            source_type == "Method" ~ str_remove(source_label, "^Method_"),
            TRUE ~ source_label
        ),
        source = if_else(source == "flag", "thr-exceed", source)
    )

  # Move stdmethod beside dsmethod, ordering GCMs, RCPs, dsmethod, stdmethod, thr-exceed, cube, avgall, avgcube
  source_levels <- c("GCM1", "GCM4", "GCM6", "RCP45_P5", "RCP85_P3", "RCP85_P5", "dsmethod", "stdmethod", "thr-exceed", "cube", "avgall", "avgcube")
  dev_raw <- dev_raw %>%
    mutate(source = factor(source, levels = source_levels))

  p_dev_all <- dev_raw %>%
    filter(category == "all", source != "cube") %>%
    ggplot(aes(x = source, y = raw_deviation, fill = source)) +
    geom_violin(alpha = 0.8, draw_quantiles = c(0.25, 0.5, 0.75), linewidth = 1.1, scale = "width", width = 0.8) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    scale_fill_manual(values = source_colors, na.value = "grey50", guide = "none") +
    labs(
        title = "Overall Vulnerability Score",
        x = NULL,
        y = "Score deviation"
    ) +
    theme_cvis() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  target_cats <- c("fwrs", "migr", "gen")
  p_dev_cats <- dev_raw %>%
    filter(category %in% target_cats, !source %in% c("avgall", "avgcube")) %>%
    ggplot(aes(x = source, y = raw_deviation, fill = source)) +
    geom_violin(alpha = 0.8, draw_quantiles = c(0.25, 0.5, 0.75), linewidth = 1, scale = "width", width = 0.8) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    facet_wrap(~category, scales = "free_x", labeller = labeller(category = cat_label_mapping)) +
    scale_fill_manual(values = source_colors, na.value = "grey50", guide = "none") +
    labs(
        x = "Variation Source",
        y = "Score deviation"
    ) +
    theme_cvis() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  p_dev_all / p_dev_cats + plot_layout(heights = c(1, 1.2))
}

# 2b. Mean Rank Displacement
plot_mean_rank_displacement <- function(score_mrd_global, source_colors = sens_source_palette) {
  ggplot(score_mrd_global, aes(x = reorder(source, mrd), y = mrd, fill = source)) +
    geom_bar(stat = "identity") +
    facet_wrap(~category, scales = "free_x") +
    scale_fill_manual(values = source_colors, na.value = "grey50", guide = "none") +
    coord_flip() +
    labs(
        title = "Mean Rank Displacement (Vulnerability Scores)",
        subtitle = "Average shift in regional vulnerability rank compared to baseline scenario",
        x = "Variation Source",
        y = "Displacement in Ranks"
    ) +
    theme_cvis()
}

# 2c. Jackknife Influence
plot_jackknife_influence <- function(jack_global, parent_cat_palette) {
  ggplot(jack_global, aes(x = reorder(excluded_element, mean_abs_dev), y = mean_raw_dev, color = parent_category)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_pointrange(aes(ymin = q10_raw_dev, ymax = q90_raw_dev), size = 0.6) +
    coord_flip() +
    facet_wrap(~excluded_type, scales = "free_y", ncol = 1) +
    scale_color_scico_d(palette = "roma", name = "Category") +
    labs(
        x = "Excluded Element (Indicator or Category)",
        y = "Vulnerability Score Deviation"
    ) +
    theme_cvis()
}

# 2d. Stock-Level and CU-Level Bootstrap Results Boxplots
plot_smu_bootstrap_uncertainty <- function(mc_results, species_colors = species_palette) {
  all_scores <- mc_results %>% filter(category == "all")
  
  # Main plot ordering by median score
  smu_order <- all_scores %>%
    group_by(SMU_SIMPLE) %>%
    summarise(median_score = median(score100, na.rm = TRUE)) %>%
    arrange(median_score) %>%
    pull(SMU_SIMPLE)
  
  main_data <- all_scores %>%
    mutate(SMU_SIMPLE = factor(SMU_SIMPLE, levels = smu_order))
  
  p_main <- ggplot(main_data, aes(x = score100, y = SMU_SIMPLE, fill = SPECIES_NAME)) +
    geom_boxplot(outlier.shape = NA, alpha = 0.7, color = "grey30", linewidth = 0.5) +
    scale_fill_manual(values = species_colors, limits = names(species_colors), name = "Species", drop = FALSE) +
    labs(
      x = "Vulnerability Score (0-100)",
      y = "Stock Management Unit"
    ) +
    theme_cvis() +
    theme(
      axis.text.y = element_text(size = 9, face = "bold")
    )
  
  # Sub-panes for stocks with multiple CUs
  multi_cu_smus <- all_scores %>%
    distinct(FULL_CU_IN, SMU_SIMPLE) %>%
    group_by(SMU_SIMPLE) %>%
    summarise(cu_count = n()) %>%
    filter(cu_count > 1) %>%
    pull(SMU_SIMPLE)
  
  sub_data <- all_scores %>%
    filter(SMU_SIMPLE %in% multi_cu_smus) %>%
    mutate(
      # Wrap long CU labels at ~22 chars to prevent overflow
      CVIS_LABEL_WRAP = stringr::str_wrap(CVIS_LABEL, width = 22),
      CVIS_LABEL_WRAP = reorder(CVIS_LABEL_WRAP, score100, FUN = median),
      SMU_SIMPLE = factor(SMU_SIMPLE, levels = intersect(smu_order, multi_cu_smus))
    )
  
  p_sub <- ggplot(sub_data, aes(x = score100, y = CVIS_LABEL_WRAP, fill = SPECIES_NAME)) +
    geom_boxplot(outlier.shape = NA, alpha = 0.7, color = "grey30", linewidth = 0.4, show.legend = FALSE) +
    facet_wrap(~SMU_SIMPLE, scales = "free_y", ncol = 2) +
    scale_fill_manual(values = species_colors, limits = names(species_colors), name = "Species", drop = FALSE) +
    scale_x_continuous(limits = c(0, 100), breaks = c(0, 25, 50, 75, 100)) +
    labs(
      x = "Vulnerability Score (0-100)",
      y = NULL
    ) +
    theme_cvis() +
    theme(
      strip.text = element_text(size = 7, face = "bold"),
      axis.text.y = element_text(size = 6, lineheight = 0.85),
      panel.spacing = unit(0.6, "lines")
    )
  
  p_combined <- p_main + p_sub + 
    plot_layout(widths = c(1, 1.8), guides = "collect") & 
    theme(legend.position = "bottom", legend.direction = "horizontal")
    
  return(p_combined)
}

# 2e. Rank Consistency Heatmap
plot_rank_consistency_heatmap <- function(cor_mat) {
  cor_melted <- reshape2::melt(cor_mat)
  ggplot(cor_melted, aes(Var1, Var2, fill = value)) +
    geom_tile() +
    scale_fill_scico(palette = "batlow", direction = 1, limits = c(0.7, 1)) +
    geom_text(aes(label = round(value, 2)), color = "white", size = 3) +
    labs(
        title = "Rank Consistency Heatmap (Spearman Rho)",
        subtitle = "Category: Overall Vulnerability (all)",
        x = NULL, y = NULL, fill = "Rho"
    ) +
    theme_cvis() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}


# ==================== 3. Risk Drivers & Multivariate Analysis ====================

# 3a. Factor Importance Plot
plot_factor_importance <- function(factor_importance) {
  ggplot(factor_importance, aes(x = reorder(factor, pct_variance), y = pct_variance, fill = pct_variance)) +
    geom_col(show.legend = FALSE) + coord_flip() + scale_fill_scico(palette = "batlow", direction = 1) +
    labs(title = "Factors Explaining Variation in Overall Vulnerability", x = NULL, y = "% Variance Explained") +
    theme_cvis()
}

# 3b. SMU Vulnerability Boxplot
plot_smu_vulnerability_variation <- function(analysis_combined, species_palette) {
  ggplot(analysis_combined %>% filter(!is.na(SMU_SIMPLE)),
    aes(x = reorder(SMU_SIMPLE, total_vulnerability, FUN = median), y = total_vulnerability, fill = SPECIES_NAME)) +
    geom_boxplot(alpha = 0.8, outlier.size = 1) + coord_flip() +
    scale_fill_manual(values = species_palette, name = "Species") +
    labs(title = "Variation in Vulnerability by SMU", subtitle = "Distribution of scores (baseline) across CUs", x = NULL, y = "Total Vulnerability Score") +
    theme_cvis() + theme(axis.text.y = element_text(size = 8), legend.position = "none")
}

# 3c. SMU Sensitivity Heatmap
plot_smu_sensitivity_heatmap <- function(smu_sensitivity) {
  ggplot(smu_sensitivity, aes(x = source, y = reorder(SMU_SIMPLE, mean_raw_dev), fill = mean_raw_dev)) +
    geom_tile(color = "white") + scale_fill_scico(palette = "roma", midpoint = 0, name = "Mean Deviation") +
    labs(title = "SMU Sensitivity to Climate Uncertainty", x = "Source of Variation", y = NULL) +
    theme_cvis() + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))
}

# 3d. PCA Detailed Biplot
plot_pca_detailed_biplot <- function(pca_plot_dat, pca_loadings, species_palette) {
  ggplot(pca_plot_dat, aes(x = PC1, y = PC2)) +
    geom_point(aes(color = total_vulnerability, shape = SPECIES_NAME), size = 3.5, alpha = 0.7) +
    geom_segment(data = pca_loadings, aes(x = 0, y = 0, xend = x_end, yend = y_end),
                 arrow = arrow(length = unit(0.2, "cm")), color = "grey30", alpha = 0.8) +
    geom_text(data = pca_loadings, aes(x = x_end * 1.1, y = y_end * 1.1, label = name_short),
              size = 3, fontface = "bold", color = "black") +
    scale_color_scico(palette = "lajolla", name = "Vulnerability", midpoint = 50) +
    scale_shape_manual(values = c(16, 17, 15, 18, 8), name = "Species") +
    labs(title = "Detailed CU Risk Profile PCA Biplot", x = "Principal Component 1", y = "Principal Component 2") +
    theme_cvis() + theme(legend.position = "right")
}

# 3e. Species CU Rank Bump Plot
plot_species_bump_plot <- function(overall_sensitivity, species_name) {
  dev_full <- overall_sensitivity$deviations %>%
    filter(category == "all", SPECIES_NAME == species_name) %>%
    select(FULL_CU_IN, SPECIES_NAME, CVIS_LABEL, SMU_SIMPLE, base_score, base_rank = base_rank_sp, starts_with("raw_dev_")) %>%
    pivot_longer(cols = starts_with("raw_dev_"), names_to = "source", names_prefix = "raw_dev_", values_to = "raw_dev") %>%
    filter(source %in% c("GCM1", "GCM4", "GCM6", "RCP85_P3", "RCP45_P5", "dsmethod", "Method_cube", "Method_avgcube", "Method_flag", "stdmethod")) %>%
    bind_rows(
        overall_sensitivity$deviations %>%
        filter(category == "all", SPECIES_NAME == species_name) %>%
        select(FULL_CU_IN, SPECIES_NAME, CVIS_LABEL, SMU_SIMPLE, base_score, base_rank = base_rank_sp) %>%
        mutate(source = "Baseline", raw_dev = 0)
    ) %>%
    mutate(scen_score = base_score + raw_dev) %>%
    group_by(source) %>%
    mutate(scen_rank = rank(-scen_score, ties.method = "average", na.last = "keep")) %>%
    ungroup()
  
  source_order <- c("GCM1", "GCM4", "GCM6", "Baseline", "RCP45_P5", "RCP85_P3", "dsmethod", "Method_cube", "Method_avgcube", "Method_flag", "stdmethod")
  source_labels_bump <- c("CanESM2", "HadGEM2", "MPI-ESM", "Baseline", "RCP 4.5 (P5)", "RCP 8.5 (P3)", "DS Method", "Cube-M", "Avg-Cube", "Thr-Exceed", "Std Meth")
  dev_full <- dev_full %>% mutate(source = factor(source, levels = source_order, labels = source_labels_bump))
  
  mrd_stats_sp <- overall_sensitivity$species_score_summary %>%
    filter(category == "all" & SPECIES_NAME == species_name) %>%
    mutate(mrd_label = paste0("MRD\n", round(mrd_sp, 1))) %>%
    mutate(source = factor(source, levels = source_order, labels = source_labels_bump)) %>%
    filter(!is.na(source))
  
  n_cus <- length(unique(dev_full$FULL_CU_IN))
  x_faces <- ifelse(source_labels_bump == "Baseline", "bold", "plain")
  
  # Determine label size dynamically based on number of CUs to reduce clutter
  label_size <- if (n_cus > 20) 1.3 else if (n_cus > 10) 1.4 else 1.6
  
  mrd_y <- 1 - 0.10 * (n_cus - 1)
  
  ggplot(dev_full, aes(x = source, y = scen_rank, group = FULL_CU_IN)) +
    geom_line(aes(color = SMU_SIMPLE), alpha = 0.5, linewidth = 1) +
    geom_label_repel(
      aes(label = FULL_CU_IN, color = SMU_SIMPLE),
      alpha = 1,
      size = label_size,
      fontface = "bold",
      box.padding = 0.05,
      label.padding = 0.1,
      direction = "y",
      min.segment.length = 0,
      segment.size = 0.2,
      segment.alpha = 0.4
    ) +
    geom_text(data = mrd_stats_sp, aes(x = source, y = mrd_y, label = mrd_label, group = NULL), 
              size = 3.2, fontface = "bold.italic", vjust = 0.5, color = "black") +
    scale_y_reverse(breaks = 1:n_cus, expand = expansion(mult = c(0.20, 0.1))) +
    scale_color_brewer(palette = "Set1", name = "SMU") +
    labs(title = paste0(species_name, ": Vulnerability Rank Stability"), subtitle = "Rank 1 = Highest Risk.", x = NULL, y = "In-Species Rank") +
    theme_cvis(base_size = 10) +
    theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_line(color = "grey90"),
          legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1, face = x_faces))
}


# ==================== 4. Combined Uncertainty Plotting Functions ====================

# 4a. Combined Uncertainty Spread Boxplot (from 4d)
plot_combined_uncertainty_spread <- function(all_scores, species_palette = NULL) {
  # Retrieve species_palette from argument or environment
  spec_pal <- if (!is.null(species_palette)) {
    species_palette
  } else {
    get("species_palette", envir = .GlobalEnv)
  }

  # Calculate mean score for each CU to find the sort order
  cu_order <- all_scores %>%
    group_by(CVIS_LABEL, SPECIES_NAME) %>%
    summarise(mean_score = mean(score100, na.rm = TRUE), .groups = "drop") %>%
    arrange(mean_score)
  
  cu_colors <- spec_pal[cu_order$SPECIES_NAME]
  cu_colors <- ifelse(is.na(cu_colors), "black", cu_colors)

  # Retrieve baseline overall scores from sensitivity_analysis.Rdata if available
  # Retrieve baseline overall scores from scoring_results.Rdata
  baseline_scores <- NULL
  scoring_file <- file.path(paths$output, "scoring_results.Rdata")
  if (file.exists(scoring_file)) {
    temp_env <- new.env()
    load(scoring_file, envir = temp_env)
    if (exists("scores_tidy_baseline", envir = temp_env)) {
      baseline_scores <- temp_env$scores_tidy_baseline %>%
        dplyr::filter(category == "all", method == "catavg") %>%
        dplyr::select(CVIS_LABEL, base_score = score100_all)
    } else if (exists("scores_tidy", envir = temp_env)) {
      baseline_scores <- temp_env$scores_tidy %>%
        dplyr::filter(category == "all", method == "catavg") %>%
        dplyr::select(CVIS_LABEL, base_score = score100_all)
    }
  }
  
  # Fallback if not found: calculate mean of bootstrap scores as proxy baseline
  if (is.null(baseline_scores)) {
    baseline_scores <- all_scores %>%
      group_by(CVIS_LABEL) %>%
      summarise(base_score = mean(score100, na.rm = TRUE), .groups = "drop")
  }

  # Merge baseline score into all_scores (done before converting CVIS_LABEL to factor)
  all_scores <- all_scores %>%
    left_join(baseline_scores, by = "CVIS_LABEL")

  # Reorder CVIS_LABEL factor levels by mean_score (done after left_join to preserve factor class)
  all_scores <- all_scores %>%
    mutate(CVIS_LABEL = factor(CVIS_LABEL, levels = cu_order$CVIS_LABEL))

  ggplot(all_scores, aes(x = CVIS_LABEL, y = score100)) +
    geom_boxplot(aes(fill = SPECIES_NAME), alpha = 0.6, color = "grey60", scale = "width") +
    geom_segment(aes(x = as.numeric(CVIS_LABEL) - 0.25, xend = as.numeric(CVIS_LABEL) + 0.25, 
                     y = base_score, yend = base_score), 
                 color = "black", linewidth = 1.2) +
    coord_flip() +
    scale_fill_manual(values = spec_pal, name = "Species") +
    labs(
      x = NULL,
      y = "Vulnerability Score"
    ) +
    theme_cvis() +
    theme(
      axis.text.y = element_text(size = 7, color = cu_colors, face = "bold"),
      plot.title = element_text(face = "bold", size = 14)
    )
}


# 4b. Rank Uncertainty pointrange plot (from 4d)
plot_rank_uncertainty <- function(rank_summary) {
  ggplot(rank_summary, aes(x = reorder(CVIS_LABEL, -mean_rank), y = mean_rank)) +
    geom_pointrange(aes(ymin = q5_rank, ymax = q95_rank, color = robustness_profile), size = 0.5) +
    coord_flip() +
    scale_color_manual(
      values = c(
        "Robust High" = "#d73027", 
        "Robust Low" = "#4575b4", 
        "Highly Uncertain" = "#fee090", 
        "Intermediate / Moderate" = "grey60"
      ), 
      name = "Robustness Profile"
    ) +
    labs(
      x = "Conservation Unit (CU)",
      y = "Vulnerability Rank (1 to 50)"
    ) +
    theme_cvis() +
    theme(
      axis.text.y = element_text(size = 7),
      plot.title = element_text(face = "bold", size = 14)
    )
}

# 4c. Uncertainty Variance Decomposition column chart (from 4d)
plot_uncertainty_variance_decomposition <- function(anova_unc) {
  ggplot(anova_unc, aes(x = reorder(Source_Label, pct_uncertainty_variance), y = pct_uncertainty_variance, fill = Source_Label)) +
    geom_col(show.legend = FALSE, alpha = 0.85, width = 0.6) +
    coord_flip() +
    scale_fill_brewer(palette = "Set1") +
    labs(
      title = "Decomposition of CVIS Combined Uncertainty",
      subtitle = "Relative contribution (% variance explained) of GCMs, RCPs, and downscaling methods to score variance\n(Controlled for geographical variation between CUs)",
      x = NULL,
      y = "% Uncertainty Variance Explained"
    ) +
    theme_cvis() +
    theme(
      plot.title = element_text(face = "bold", size = 14)
    )
}

# ==================== 4. Standalone Execution Block ====================
# This block runs only if the script is run directly (not sourced) and saves all plots to disk.
if (sys.nframe() == 0) {
  cat("Running standalone plot generation...\n")
  
  library(here)
  source(file.path(here(), "code", "0_setup.R"))
  
  # Directories
  ind_fig_path <- file.path(paths$figures, "indicator_uncertainty")
  sens_fig_path <- file.path(paths$figures, "sensitivity_analysis")
  uncertainty_fig_path <- file.path(paths$figures, "uncertainty_analysis")
  spec_fig_path <- file.path(sens_fig_path, "species_focus")
  
  dir.create(ind_fig_path, showWarnings = FALSE, recursive = TRUE)
  dir.create(sens_fig_path, showWarnings = FALSE, recursive = TRUE)
  dir.create(uncertainty_fig_path, showWarnings = FALSE, recursive = TRUE)
  dir.create(spec_fig_path, showWarnings = FALSE, recursive = TRUE)
  
  # Load pre-computed sensitivity and uncertainty results
  load(file.path(paths$output, "scoring_results.Rdata"))
  load(file.path(paths$output, "sensitivity_analysis.Rdata"))
  load(file.path(paths$output, "indicator_sensitivity_summary.Rdata"))
  load(file.path(paths$output, "uncertainty_analysis_results.Rdata"))
  
  # Remap "flag" to "thr-exceed" in pre-computed objects to ensure consistent labeling across plots
  if (exists("score_sens_summary")) {
    score_sens_summary <- score_sens_summary %>%
      mutate(source = if_else(source == "flag", "thr-exceed", source))
  }
  if (exists("overall_sensitivity")) {
    if (!is.null(overall_sensitivity$score_sensitivity_summary)) {
      overall_sensitivity$score_sensitivity_summary <- overall_sensitivity$score_sensitivity_summary %>%
        mutate(source = if_else(source == "flag", "thr-exceed", source))
    }
    if (!is.null(overall_sensitivity$species_score_summary)) {
      overall_sensitivity$species_score_summary <- overall_sensitivity$species_score_summary %>%
        mutate(source = if_else(source == "flag", "thr-exceed", source))
    }
    if (!is.null(overall_sensitivity$global_score_summary)) {
      overall_sensitivity$global_score_summary <- overall_sensitivity$global_score_summary %>%
        mutate(source = if_else(source == "flag", "thr-exceed", source))
    }
  }

  source_colors <- sens_source_palette
  
  # -------------------- A. Indicator Plots --------------------
  cat("Generating indicator-level plots...\n")
  
  # 1. MAD Bar Chart
  p_ind_sens <- plot_indicator_sensitivity(ind_sens_summary, source_colors)
  ggsave(file.path(ind_fig_path, "indicator_level_sensitivity.png"), p_ind_sens, width = 12, height = 8)
  
  # 2. Directional Violins
  p_shift <- plot_indicator_directional_shifts(overall_sensitivity, tbl_indicators, source_colors)
  ggsave(file.path(ind_fig_path, "indicator_directional_shifts_violin.png"), p_shift, width = 18, height = 12)
  
  # 3. XY Plot
  p_xy <- plot_indicator_xy_sensitivity(overall_sensitivity, source_colors)
  ggsave(file.path(ind_fig_path, "indicator_xy_sensitivity_gcm_model.png"), p_xy, width = 16, height = 12)
  
  # 4. Correlation Pie
  png(file.path(ind_fig_path, "indicator_redundancy_corrplot.png"), width = 1000, height = 1000, res = 120)
  plot_indicator_redundancy_corr(overall_sensitivity$correlation_indicators)
  dev.off()
  
  # 5. Downscaling Deviations by Indicator
  p_ds_ind <- plot_indicator_downscaling_deviations(overall_sensitivity)
  ggsave(file.path(ind_fig_path, "indicator_downscaling_deviations.png"), p_ds_ind, width = 10, height = 7)
  

  
  # -------------------- B. Score & Species Plots --------------------
  cat("Generating score-level and species plots...\n")
  
  # 6. Score Deviations
  dev_raw <- overall_sensitivity$deviations %>%
    select(FULL_CU_IN, category, starts_with("raw_dev_")) %>%
    pivot_longer(cols = starts_with("raw_dev_"), names_to = "source_label", values_to = "raw_deviation") %>%
    mutate(
        source_label = str_remove(source_label, "raw_dev_"),
        source_type = case_when(
            str_detect(source_label, "^GCM") ~ "GCM",
            str_detect(source_label, "^RCP") ~ "Scenario",
            str_detect(source_label, "^Method") ~ "Method",
            str_detect(source_label, "^Model") ~ "dsmethod",
            str_detect(source_label, "^dsmethod") ~ "dsmethod",
            str_detect(source_label, "^stdmethod") ~ "stdmethod",
            TRUE ~ "Other"
        ),
        source = case_when(
            source_type == "Method" ~ str_remove(source_label, "^Method_"),
            TRUE ~ source_label
        )
    )
  p_dev_combined <- plot_score_deviations(overall_sensitivity$deviations, source_colors, cat_label_map)
  ggsave(file.path(sens_fig_path, "overall_vulnerability_deviations.png"), p_dev_combined, width = 12, height = 10)
  
  # 7. MRD Bar Chart
  score_mrd_global <- score_sens_summary %>% filter(SPECIES_NAME == "ALL")
  p_mrd <- plot_mean_rank_displacement(score_mrd_global, source_colors)
  ggsave(file.path(sens_fig_path, "mean_rank_displacement.png"), p_mrd, width = 11, height = 8)
  
  # 8. Jackknife Influence
  jack_global <- overall_sensitivity$influence_summary %>%
    filter((method == "catavg" | method == "cat_avg") & SPECIES_NAME == "ALL")
  p_jack <- plot_jackknife_influence(jack_global, cat_label_map)
  ggsave(file.path(sens_fig_path, "jackknife_leverage_overall.png"), p_jack, width = 10, height = 8)
  
  # 9. Rank Consistency Heatmap
  p_cor <- plot_rank_consistency_heatmap(cor_matrices$all)
  ggsave(file.path(sens_fig_path, "rank_correlation_heat_all.png"), p_cor, width = 8, height = 7)
  
  # 10. Factor Importance
  p_importance <- plot_factor_importance(risk_drivers_analysis$factor_importance)
  ggsave(file.path(sens_fig_path, "factor_importance_vulnerability.png"), p_importance, width = 10, height = 7)
  
  # 11. SMU Vulnerability Boxplot
  p_smu_variation <- plot_smu_vulnerability_variation(risk_drivers_analysis$analysis_combined, species_palette)
  ggsave(file.path(sens_fig_path, "smu_vulnerability_variation.png"), p_smu_variation, width = 10, height = 8)
  
  # 12. SMU Sensitivity Heatmap
  p_smu_sensitivity <- plot_smu_sensitivity_heatmap(risk_drivers_analysis$smu_sensitivity)
  ggsave(file.path(sens_fig_path, "smu_climate_sensitivity.png"), p_smu_sensitivity, width = 10, height = 8)
  
  # 13. PCA Biplot
  short_name_map <- c(
    "favchange" = "ENM Fav.", "cthr" = "FW Threats", "tw8rate" = "Aug T Rate",
    "tw8proj" = "Aug T Proj", "flow8pdelta" = "Aug Flow \u0394", "flow18pdelta" = "Win Flow \u0394",
    "fwres" = "FW Residency", "migrTproj" = "Migr T Proj", "migrQpdelta" = "Migr Flow \u0394",
    "migrdist" = "Migr Dist", "SSTproj" = "SST Entry", "SSTrate" = "SST Rate",
    "CImpact" = "Mar Impacts", "CUstatus" = "WSP Status", "CUnmat" = "Abundance",
    "hetzyg" = "Heterozygosity", "genoff" = "Genomic Offset"
  )
  pca_res <- risk_drivers_analysis$pca_result
  pca_loadings <- as.data.frame(pca_res$rotation[, 1:2])
  pca_loadings$indicator <- rownames(pca_loadings)
  pca_loadings$name_short <- coalesce(short_name_map[pca_loadings$indicator], pca_loadings$indicator)
  mult <- 15 
  pca_loadings <- pca_loadings %>% mutate(x_end = PC1 * mult, y_end = PC2 * mult) %>%
    filter(abs(PC1) > 0.15 | abs(PC2) > 0.15)
  p_pca_detailed <- plot_pca_detailed_biplot(risk_drivers_analysis$pca_plot_dat, pca_loadings, species_palette)
  ggsave(file.path(sens_fig_path, "pca_detailed_biplot.png"), p_pca_detailed, width = 11, height = 8)
  
  # 14. Species Rank Bump Plots
  for (sp in c("Chinook", "Sockeye", "Coho")) {
    p <- plot_species_bump_plot(overall_sensitivity, sp)
    ggsave(file.path(spec_fig_path, paste0("rank_bump_", sp, ".png")), p, width = 12, height = 8)
  }
  
  # -------------------- C. Combined Uncertainty Plots --------------------
  cat("Generating combined uncertainty plots...\n")
  
  # 15. Uncertainty Boxplot
  all_scores <- mc_results %>% filter(category == "all")
  p_spread <- plot_combined_uncertainty_spread(all_scores, species_palette)
  ggsave(file.path(uncertainty_fig_path, "combined_uncertainty_spread.png"), p_spread, width = 12, height = 9, dpi = 300)
  
  # 16. Rank Uncertainty pointranges
  p_rank <- plot_rank_uncertainty(cu_summary)
  ggsave(file.path(uncertainty_fig_path, "rank_uncertainty.png"), p_rank, width = 12, height = 9, dpi = 300)
  
  # 17. Uncertainty Variance Decomposition
  p_var <- plot_uncertainty_variance_decomposition(anova_unc)
  ggsave(file.path(uncertainty_fig_path, "variance_decomposition.png"), p_var, width = 10, height = 6, dpi = 300)
  
  # 18. SMU Bootstrap Boxplot
  p_smu_boot <- plot_smu_bootstrap_uncertainty(mc_results, species_palette)
  ggsave(file.path(uncertainty_fig_path, "smu_bootstrap_uncertainty.png"), p_smu_boot, width = 16, height = 11, dpi = 150)
  
  cat("Standalone plot generation complete. All figures saved.\n")
}


