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
      subtitle = "Average absolute deviation from baseline across all CUs",
      x = "Indicator",
      y = "Mean Absolute Deviation"
    ) +
    theme_cvis() +
    theme(legend.position = "bottom")
}

# 1b. Indicator Directional Shift Violins
plot_indicator_directional_shifts <- function(overall_sensitivity, tbl_indicators, source_colors = sens_source_palette) {
  ind_source_levels <- c("Baseline", "GCM1", "GCM4", "GCM6", "RCP45_P5", "RCP85_P3", "RCP85_P5", "dsmethod", "stdmethod")
  ind_source_labels <- c("Baseline", "CanESM2 (GCM 1)", "HadGEM2 (GCM 4)", "MPI (GCM 6)", "RCP 4.5 (P5)", "RCP 8.5 (P3)", "RCP 8.5 (P5)", "Downscaling Method", "Standardize Meth")

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
    mutate(source = factor(source, levels = rev(ind_source_levels), labels = rev(ind_source_labels)))
  
  baseline_refs <- ind_shift_cus %>% group_by(indicator) %>% summarise(ref_mean = mean(base_raw_mean, na.rm = TRUE), .groups = "drop")
  ind_label_units <- tbl_indicators %>% mutate(facet_label = paste0(abbrev, "\n(", unit, ")")) %>% select(abbrev, facet_label) %>% tibble::deframe()
  
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
    theme(strip.text = element_text(face = "bold", size = 8.5), axis.text.y = element_text(size = 8))
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
  corrplot(cor_matrix_ind, method = "pie", type = "lower", title = "Indicator Pearson Correlation", mar = c(0, 0, 1, 0))
}

# 1e. Indicator Correlation Clusters (Heatmap)
plot_indicator_correlation_clusters <- function(cor_matrix_pearson) {
  corrplot(cor_matrix_pearson,
    method = "color", order = "hclust", col = scico(200, palette = "roma"),
    tl.col = "black", tl.srt = 45, tl.cex = 0.7, addrect = 5, rect.col = "black", rect.lwd = 2,
    mar = c(0, 0, 1, 0), title = "Indicator Correlation Clusters"
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
        )
    )

  p_dev_all <- dev_raw %>%
    filter(category == "all", source != "cube") %>%
    ggplot(aes(x = source, y = raw_deviation, fill = source)) +
    geom_violin(alpha = 0.8, draw_quantiles = c(0.25, 0.5, 0.75), linewidth = 1.1) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    scale_fill_manual(values = source_colors, na.value = "grey50", guide = "none") +
    labs(
        title = "Influence of Uncertainty on Overall Vulnerability",
        x = NULL,
        y = "Score Change (Raw Deviation)"
    ) +
    theme_cvis() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  target_cats <- c("fwrs", "migr", "gen")
  p_dev_cats <- dev_raw %>%
    filter(category %in% target_cats, !source %in% c("avgall", "avgcube")) %>%
    ggplot(aes(x = source, y = raw_deviation, fill = source)) +
    geom_violin(alpha = 0.8, draw_quantiles = c(0.25, 0.5, 0.75), linewidth = 1, scale = "width") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    facet_wrap(~category, scales = "free_x", labeller = labeller(category = cat_label_mapping)) +
    scale_fill_manual(values = source_colors, na.value = "grey50", guide = "none") +
    labs(
        title = "Category-Level Score Sensitivity",
        x = "Variation Source",
        y = "Score Change (Raw Deviation)"
    ) +
    theme_cvis() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  p_dev_all / p_dev_cats + plot_layout(heights = c(1, 1.2)) +
    plot_annotation(
        title = "Directional Influence of Uncertainty on Vulnerability Scores",
        subtitle = "Distribution of (Scenario Score - Baseline Score) across CUs; Bolder lines indicate median and quartiles",
        theme = theme(plot.title = element_text(face = "bold", size = 16))
    )
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
    scale_color_scico_d(palette = "roma", name = "Parent Category") +
    labs(
        title = "Jackknife Influence on Overall Vulnerability",
        subtitle = "Mean raw deviation in vulnerability score (0-100 scale) when removing an element.\nLines show 10th-90th percentile range across CUs. Negative = Risk Driver.",
        x = "Excluded Element (Indicator or Category)",
        y = "Raw Score Deviation (jk_score - base_score)"
    ) +
    theme_cvis()
}

# 2d. Rank Consistency Heatmap
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
    select(FULL_CU_IN, SPECIES_NAME, CVIS_NAME, SMU_SIMPLE, base_score, base_rank = base_rank_sp, starts_with("raw_dev_")) %>%
    pivot_longer(cols = starts_with("raw_dev_"), names_to = "source", names_prefix = "raw_dev_", values_to = "raw_dev") %>%
    filter(source %in% c("GCM1", "GCM4", "GCM6", "RCP85_P3", "RCP45_P5", "dsmethod", "Method_cube", "Method_avgcube", "Method_flag", "stdmethod")) %>%
    bind_rows(
        overall_sensitivity$deviations %>%
        filter(category == "all", SPECIES_NAME == species_name) %>%
        select(FULL_CU_IN, SPECIES_NAME, CVIS_NAME, SMU_SIMPLE, base_score, base_rank = base_rank_sp) %>%
        mutate(source = "Baseline", raw_dev = 0)
    ) %>%
    mutate(scen_score = base_score + raw_dev) %>%
    group_by(source) %>%
    mutate(scen_rank = rank(-scen_score, ties.method = "average", na.last = "keep")) %>%
    ungroup()
  
  source_order <- c("GCM1", "GCM4", "GCM6", "Baseline", "RCP45_P5", "RCP85_P3", "dsmethod", "Method_cube", "Method_avgcube", "Method_flag", "stdmethod")
  source_labels_bump <- c("CanESM2", "HadGEM2", "MPI-ESM", "Baseline", "RCP 4.5 (P5)", "RCP 8.5 (P3)", "DS Method", "Cube-M", "Avg-Cube", "Flag", "Std Meth")
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
    theme_minimal(base_size = 10) +
    theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_line(color = "grey90"),
          legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1, face = x_faces))
}


# ==================== 4. Combined Uncertainty Plotting Functions ====================

# 4a. Combined Uncertainty Spread Boxplot (from 4d)
plot_combined_uncertainty_spread <- function(all_scores, species_palette) {
  ggplot(all_scores, aes(x = reorder(CVIS_NAME, score100, FUN = mean), y = score100)) +
    geom_boxplot(aes(fill = SPECIES_NAME), alpha = 0.6, outlier.size = 0.8) +
    coord_flip() +
    scale_fill_manual(values = species_palette, name = "Species") +
    labs(
      title = "Combined Uncertainty in overall CVIS Vulnerability",
      subtitle = "Vulnerability scores (0-100) across 100 Monte Carlo iterations (Period 3)\nCUs sorted by mean vulnerability score; box plot shows median and IQR",
      x = "Conservation Unit (CU)",
      y = "Vulnerability Score"
    ) +
    theme_cvis() +
    theme(
      axis.text.y = element_text(size = 7),
      plot.title = element_text(face = "bold", size = 14)
    )
}

# 4b. Rank Uncertainty pointrange plot (from 4d)
plot_rank_uncertainty <- function(rank_summary) {
  ggplot(rank_summary, aes(x = reorder(CVIS_NAME, -mean_rank), y = mean_rank)) +
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
      title = "Vulnerability Rank Stability & Confidence Intervals",
      subtitle = "Mean rank and 90% uncertainty intervals across all assumptions (Period 3)\nRank 1 = Highest Risk. Sorted by mean vulnerability rank.",
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


# ==================== 4. Single-CU Sensitivity Plotting Functions ====================

plot_cu_sensitivity_scores <- function(
  overall_sensitivity,
  cu_code
) {
  # Categories mapping including all categories and overall vulnerability
  cat_labels <- c(
    "all"  = "Overall Vulnerability",
    "dem"  = "Demographics",
    "fwrs" = "Spawning & Rearing",
    "migr" = "Upstream Migration",
    "mar"  = "Nearshore Marine",
    "gen"  = "Genetics"
  )

  # 1. Prepare score deviations data
  dev_raw <- overall_sensitivity$deviations %>%
    filter(category %in% names(cat_labels), FULL_CU_IN != "ALL") %>%
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
    ) %>%
    filter(!source %in% c("cube", "flag", "cube_all")) %>%
    mutate(
      category_label = factor(cat_labels[category], levels = cat_labels)
    )

  # Factor levels for consistency
  source_levels <- c("GCM1", "GCM4", "GCM6", "RCP45_P5", "RCP85_P3", "RCP85_P5", "dsmethod", "stdmethod", "avgall", "avgcube")
  source_labels <- c("CanESM2 (GCM 1)", "HadGEM2 (GCM 4)", "MPI (GCM 6)", "RCP 4.5 (P5)", "RCP 8.5 (P3)", "RCP 8.5 (P5)", "Downscaling Method", "Standardize Meth", "Avg All Scoring", "Avg Cube Scoring")
  
  dev_raw <- dev_raw %>%
    filter(source %in% source_levels) %>%
    mutate(source = factor(source, levels = rev(source_levels), labels = rev(source_labels)))

  # Selected CU data
  dev_raw_cu <- dev_raw %>% filter(FULL_CU_IN == cu_code)
  # All other CUs
  dev_raw_others <- dev_raw %>% filter(FULL_CU_IN != cu_code)

  # Retrieve colors using sens_source_palette
  sens_palette <- if (exists("sens_source_palette")) {
    get("sens_source_palette")
  } else {
    c(
      "GCM1" = "#e31a1c", "GCM4" = "#ff7f00", "GCM6" = "#fdbf6f",
      "RCP45_P5" = "#33a02c", "RCP85_P3" = "#1f78b4", "RCP85_P5" = "#a6cee3",
      "avgcube" = "#cab2d6", "avgall" = "#fb9a99",
      "dsmethod" = "#8dd3c7", "stdmethod" = "#8c564b"
    )
  }
  
  # Retrieve indicator colors from global environment for coloring violins
  if (exists("indicator_palette", envir = .GlobalEnv)) {
    ind_colors <- get("indicator_palette", envir = .GlobalEnv)
  } else {
    ind_colors <- c(
      "Demographics" = "purple",
      "Spawning & Rearing" = "turquoise",
      "Upstream Migration" = "royalblue",
      "Nearshore Marine" = "green4",
      "Genetics" = "orange3"
    )
  }
  ind_colors["Overall Vulnerability"] <- "grey30"
  
  y_colors <- sapply(rev(source_levels), function(x) {
    if (x %in% names(sens_palette)) sens_palette[[x]] else "black"
  })
  y_colors <- unname(y_colors)

  # Plot: Score deviations faceted and colored by category
  p1 <- ggplot(dev_raw_others, aes(y = source, x = raw_deviation, fill = category_label)) +
    geom_violin(color = "grey60", alpha = 0.4, scale = "width") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
    geom_point(data = dev_raw_cu, aes(x = raw_deviation, y = source), color = "#E67E22", size = 4, shape = 18) +
    facet_wrap(~category_label, ncol = 3) +
    scale_fill_manual(values = ind_colors, guide = "none") +
    labs(
      title = paste("Vulnerability Score Shifts for CU:", cu_code),
      subtitle = "Violins show Fraser CUs distribution; Orange diamond shows selected CU",
      x = "Score Deviation (Scenario - Baseline)",
      y = "Assumption / Scenario"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", size = 12, color = "#1A365D"),
      plot.subtitle = element_text(size = 9, color = "grey40"),
      panel.grid.minor = element_blank(),
      axis.text.y = element_text(color = y_colors, face = "bold", size = 9),
      strip.text = element_text(face = "bold", size = 10, color = "#1A365D"),
      strip.background = element_blank()
    )

  return(p1)
}


plot_cu_sensitivity_indicators <- function(
  overall_sensitivity,
  tbl_indicators,
  cu_code
) {
  # Prepare indicator directional shifts data relative to baseline
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
    filter(!is.na(val))

  # Keep only indicators that have non-NA values for the selected CU
  valid_indicators <- ind_shift_cus %>%
    filter(FULL_CU_IN == cu_code, !is.na(val)) %>%
    pull(indicator) %>%
    unique()

  # Categories mapping for ordering and coloring
  cat_labels <- c(
    "dem"  = "Demographics",
    "fwrs" = "Spawning & Rearing",
    "migr" = "Upstream Migration",
    "mar"  = "Nearshore Marine",
    "gen"  = "Genetics"
  )

  # Calculate variation for each indicator for this CU
  var_indicators_df <- ind_shift_cus %>%
    filter(FULL_CU_IN == cu_code) %>%
    group_by(indicator, category) %>%
    summarise(
      val_range = max(val, na.rm = TRUE) - min(val, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(!is.na(val_range), val_range > 1e-5) %>%
    arrange(desc(val_range))

  # If there are no varying indicators, fall back to all valid indicators
  if (nrow(var_indicators_df) == 0) {
    var_indicators <- valid_indicators
  } else {
    # Take top 9 varying indicators
    if (nrow(var_indicators_df) > 9) {
      var_indicators_df <- var_indicators_df[1:9, ]
    }
    # Sort those 9 indicators by category order, then name
    category_order <- c("dem", "fwrs", "migr", "mar", "gen")
    var_indicators_df <- var_indicators_df %>%
      mutate(category_factor = factor(category, levels = category_order)) %>%
      arrange(category_factor, indicator)
    var_indicators <- var_indicators_df$indicator
  }

  ind_shift_cus <- ind_shift_cus %>%
    filter(indicator %in% var_indicators) %>%
    mutate(
      indicator = factor(indicator, levels = var_indicators),
      category_label = factor(cat_labels[category], levels = cat_labels)
    )

  # Scenario levels for indicators
  ind_source_levels <- c("Baseline", "GCM1", "GCM4", "GCM6", "RCP45_P5", "RCP85_P3", "RCP85_P5", "dsmethod", "stdmethod")
  ind_source_labels <- c("Baseline", "CanESM2 (GCM 1)", "HadGEM2 (GCM 4)", "MPI (GCM 6)", "RCP 4.5 (P5)", "RCP 8.5 (P3)", "RCP 8.5 (P5)", "Downscaling Method", "Standardize Meth")

  ind_shift_cus <- ind_shift_cus %>%
    filter(source %in% ind_source_levels) %>%
    mutate(source = factor(source, levels = rev(ind_source_levels), labels = rev(ind_source_labels)))

  ind_shift_cu <- ind_shift_cus %>% filter(FULL_CU_IN == cu_code)
  ind_shift_others <- ind_shift_cus %>% filter(FULL_CU_IN != cu_code)

  # Label map with units
  short_units <- c(
    "favchange" = "ENM Fav",
    "cthr" = "Threat",
    "tw8rate" = "°C/decade",
    "tw8proj" = "°C",
    "flow8pdelta" = "Aug Flow",
    "flow18pdelta" = "Win Flow",
    "fwres" = "days",
    "migrTproj" = "°C",
    "migrQpdelta" = "Discharge",
    "migrdist" = "km",
    "SSTproj" = "°C",
    "SSTrate" = "°C/decade",
    "CImpact" = "Threat",
    "CUstatus" = "Status",
    "CUnmat" = "spawners",
    "hetzyg" = "Heterozygosity",
    "genoff" = "Offset"
  )

  ind_label_units <- tbl_indicators %>% 
    mutate(
      unit_short = ifelse(abbrev %in% names(short_units), short_units[abbrev], unit),
      facet_label = paste0(abbrev, " (", unit_short, ")")
    ) %>% 
    select(abbrev, facet_label) %>% 
    tibble::deframe()

  # Retrieve colors using sens_source_palette
  sens_palette <- if (exists("sens_source_palette")) {
    get("sens_source_palette")
  } else {
    c(
      "GCM1" = "#e31a1c", "GCM4" = "#ff7f00", "GCM6" = "#fdbf6f",
      "RCP45_P5" = "#33a02c", "RCP85_P3" = "#1f78b4", "RCP85_P5" = "#a6cee3",
      "avgcube" = "#cab2d6", "avgall" = "#fb9a99",
      "dsmethod" = "#8dd3c7", "stdmethod" = "#8c564b"
    )
  }
  
  # Retrieve indicator colors from global environment for coloring violins
  if (exists("indicator_palette", envir = .GlobalEnv)) {
    ind_colors <- get("indicator_palette", envir = .GlobalEnv)
  } else {
    ind_colors <- c(
      "Demographics" = "purple",
      "Spawning & Rearing" = "turquoise",
      "Upstream Migration" = "royalblue",
      "Nearshore Marine" = "green4",
      "Genetics" = "orange3"
    )
  }
  
  ind_y_colors <- sapply(rev(ind_source_levels), function(x) {
    if (x == "Baseline") {
      "black"
    } else if (x %in% names(sens_palette)) {
      sens_palette[[x]]
    } else {
      "black"
    }
  })
  ind_y_colors <- unname(ind_y_colors)

  # Extract baseline value for vertical reference lines
  baseline_line_data <- ind_shift_cu %>%
    filter(source == "Baseline") %>%
    select(indicator, x_intercept = val)

  # Plot: Indicator values across scenarios
  p2 <- ggplot(ind_shift_others, aes(y = source, x = val, fill = category_label)) +
    geom_vline(data = baseline_line_data, aes(xintercept = x_intercept), linetype = "dashed", color = "grey50") +
    geom_violin(color = "#CBD5E0", alpha = 0.5, scale = "width") +
    geom_point(data = ind_shift_cu, aes(x = val, y = source), color = "#E67E22", size = 3, shape = 18) +
    facet_wrap(~indicator, scales = "free_x", ncol = 3, labeller = labeller(indicator = ind_label_units)) +
    scale_fill_manual(values = ind_colors, guide = "none") +
    labs(
      title = paste("Raw Indicator Value Sensitivity for CU:", cu_code),
      subtitle = "Violins show distributions across all CUs; Orange diamond highlights the selected CU. Dashed line indicates baseline.",
      x = "Actual Raw Indicator Value (units vary)",
      y = "Assumption / Scenario"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", size = 12, color = "#1A365D"),
      plot.subtitle = element_text(size = 9, color = "grey40"),
      strip.text = element_text(face = "bold", size = 9),
      panel.grid.minor = element_blank(),
      axis.text.y = element_text(color = ind_y_colors, face = "bold", size = 9)
    )

  return(p2)
}


plot_cu_sensitivity_summary <- function(
  overall_sensitivity,
  tbl_indicators,
  cu_code
) {
  p1 <- plot_cu_sensitivity_scores(overall_sensitivity, cu_code)
  
  # Strip some titles for combined presentation
  p1 <- p1 + labs(title = "Category Vulnerability Score Shifts", subtitle = NULL)
  
  p2 <- plot_cu_sensitivity_indicators(overall_sensitivity, tbl_indicators, cu_code)
  p2 <- p2 + labs(title = "Raw Indicator Value Sensitivity", subtitle = NULL) +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.title.y = element_blank())

  # Combine using patchwork
  p_combined <- p1 + p2 + 
    plot_layout(widths = c(1, 2.2)) +
    plot_annotation(
      title = paste("Sensitivity Analysis Plots for CU:", cu_code),
      subtitle = "Visualizing how overall vulnerability score and individual indicators shift across uncertainty assumptions relative to other Fraser CUs",
      theme = ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold", size = 14, color = "#1A365D"),
        plot.subtitle = ggplot2::element_text(size = 10, color = "#4A5568")
      )
    )

  return(p_combined)
}


# ==================== 5. Standalone Execution Block ====================
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
  
  # 5. Correlation Clusters
  png(file.path(ind_fig_path, "indicator_correlation_clusters.png"), width = 1000, height = 1000, res = 120)
  plot_indicator_correlation_clusters(risk_drivers_analysis$cor_matrix_pearson)
  dev.off()
  
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
    filter((method == "avgall" | method == "avg_all") & SPECIES_NAME == "ALL")
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
  
  cat("Standalone plot generation complete. All figures saved.\n")
}
