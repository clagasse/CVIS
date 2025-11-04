################################################################################
#
# correlation_pca_clustering_analysis.R
#
# Advanced correlation analysis for vulnerability indicators including:
#   1. Correlation matrices and significance testing
#   2. Principal Component Analysis (PCA)
#   3. Hierarchical clustering of indicators and CUs
#   4. Variance Inflation Factors (VIF) for multicollinearity
#   5. Indicator relationships with composite vulnerability scores
#
################################################################################

library(tidyverse)
library(corrplot)
library(FactoMineR)  # For PCA
library(factoextra)  # For PCA visualization
library(dendextend)  # For dendrogram visualization
library(ggdendro)    # For dendrogram ggplot
library(Hmisc)       # For correlation p-values
library(car)         # For VIF calculation
library(psych)       # For additional PCA methods
library(patchwork)   # For multi-panel plots

#------------------------------------------------------------------------------
# 1. Correlation Matrix Analysis
#------------------------------------------------------------------------------

#' Calculate correlation matrix with significance testing
#'
#' @param data Data frame with indicator values
#' @param indicators_choose Vector of indicator abbreviations
#' @param use_standardized Use standardized values (TRUE) or raw (FALSE)
#' @param method Correlation method: "pearson" or "spearman"
#' @param min_valid_ratio Minimum proportion of non-NA values required per indicator
#'
#' @return List with correlation matrix, p-values, and significant correlations
#'
calculate_correlation_matrix <- function(data,
                                         indicators_choose = tbl_indicators$abbrev,
                                         use_standardized = TRUE,
                                         method = "pearson",
                                         min_valid_ratio = 0.5) {
  
  # Prepare indicator column names
  if (use_standardized) {
    indicator_cols <- paste0("std_", indicators_choose)
  } else {
    indicator_cols <- indicators_choose
  }
  
  # Find which columns actually exist in the data
  available_cols <- c()
  for (ind in indicator_cols) {
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
  
  if (length(available_cols) < 2) {
    stop("Need at least 2 indicator columns for correlation analysis. Found: ", 
         length(available_cols))
  }
  
  cat("Found", length(available_cols), "indicator columns out of", 
      length(indicator_cols), "requested\n")
  
  # Select only numeric indicator columns
  indicator_data <- data %>%
    select(all_of(available_cols)) %>%
    select(where(is.numeric))
  
  # Check validity of each indicator
  n_total <- nrow(indicator_data)
  valid_counts <- sapply(indicator_data, function(x) sum(!is.na(x)))
  valid_ratios <- valid_counts / n_total
  
  # Keep only indicators with enough valid data
  valid_indicators <- names(valid_ratios)[valid_ratios >= min_valid_ratio]
  
  if (length(valid_indicators) < 2) {
    stop("Insufficient indicators with enough valid data. Need at least 2 with >= ",
         min_valid_ratio * 100, "% non-missing values.")
  }
  
  cat("Using", length(valid_indicators), "indicators with >=", 
      min_valid_ratio * 100, "% valid data\n")
  
  indicator_data <- indicator_data %>%
    select(all_of(valid_indicators))
  
  # Calculate correlation matrix with p-values
  cor_result <- rcorr(as.matrix(indicator_data), type = method)
  
  # Identify significant correlations (p < 0.05)
  sig_cor <- which(cor_result$P < 0.05 & cor_result$r != 1, arr.ind = TRUE)
  
  if (nrow(sig_cor) > 0) {
    sig_pairs <- data.frame(
      indicator1 = rownames(cor_result$r)[sig_cor[, 1]],
      indicator2 = colnames(cor_result$r)[sig_cor[, 2]],
      correlation = cor_result$r[sig_cor],
      p_value = cor_result$P[sig_cor]
    ) %>%
      filter(indicator1 < indicator2) %>%  # Remove duplicates
      arrange(desc(abs(correlation)))
  } else {
    sig_pairs <- data.frame(
      indicator1 = character(0),
      indicator2 = character(0),
      correlation = numeric(0),
      p_value = numeric(0)
    )
  }
  
  return(list(
    correlation_matrix = cor_result$r,
    p_values = cor_result$P,
    significant_pairs = sig_pairs,
    method = method,
    n_indicators = length(valid_indicators),
    indicators_used = valid_indicators
  ))
}


#' Plot enhanced correlation matrix
#'
#' @param cor_result Output from calculate_correlation_matrix()
#' @param p_threshold P-value threshold for significance
#'
#' @return corrplot object
#'
plot_correlation_matrix <- function(cor_result,
                                    p_threshold = 0.05) {
  
  cor_matrix <- cor_result$correlation_matrix
  p_matrix <- cor_result$p_values
  
  # Create significance matrix for plotting
  sig_matrix <- ifelse(p_matrix < p_threshold, "sig", "non-sig")
  sig_matrix[is.na(sig_matrix)] <- "non-sig"
  
  corrplot(cor_matrix,
           method = "color",
           type = "upper",
           order = "hclust",
           addCoef.col = "black",
           number.cex = 0.6,
           tl.col = "black",
           tl.srt = 45,
           tl.cex = 0.8,
           diag = FALSE,
           col = colorRampPalette(c("blue", "white", "red"))(200),
           title = paste0("Indicator Correlations (", cor_result$method, ")"),
           mar = c(0, 0, 2, 0)
  )
  
  # Add significance marks
  corrplot(cor_matrix,
           add = TRUE,
           type = "upper",
           method = "circle",
           diag = FALSE,
           tl.pos = "n",
           cl.pos = "n",
           p.mat = p_matrix,
           sig.level = p_threshold,
           insig = "label_sig",
           pch.cex = 0.8
  )
}


#' Identify indicator clusters based on correlation
#'
#' @param cor_matrix Correlation matrix
#' @param threshold Correlation threshold for grouping
#'
#' @return List of indicator clusters
#'
identify_indicator_clusters <- function(cor_matrix,
                                        threshold = 0.7) {
  
  # Convert correlation to distance
  cor_dist <- as.dist(1 - abs(cor_matrix))
  
  # Hierarchical clustering
  hc <- hclust(cor_dist, method = "complete")
  
  # Cut tree at threshold
  clusters <- cutree(hc, h = 1 - threshold)
  
  # Organize by cluster
  cluster_list <- split(names(clusters), clusters)
  
  return(list(
    clusters = cluster_list,
    n_clusters = length(cluster_list),
    dendrogram = hc
  ))
}


#------------------------------------------------------------------------------
# 2. Variance Inflation Factor (VIF) Analysis
#------------------------------------------------------------------------------

#' Calculate VIF to detect multicollinearity
#'
#' @param data Data frame with indicators
#' @param indicators_choose Vector of indicators
#' @param use_standardized Use standardized values
#' @param min_valid_ratio Minimum proportion of non-NA values required
#'
#' @return Data frame with VIF values
#'
calculate_vif <- function(data,
                          indicators_choose = tbl_indicators$abbrev,
                          use_standardized = TRUE,
                          min_valid_ratio = 0.5) {
  
  # Prepare indicator column names
  if (use_standardized) {
    indicator_cols <- paste0("std_", indicators_choose)
  } else {
    indicator_cols <- indicators_choose
  }
  
  # Find which columns actually exist
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
  
  if (length(available_cols) < 2) {
    stop("Need at least 2 indicators for VIF calculation")
  }
  
  cat("Found", length(available_cols), "indicator columns for VIF\n")
  
  # Select indicators and check validity
  indicator_data <- data %>%
    select(all_of(available_cols)) %>%
    select(where(is.numeric))
  
  # Check validity
  n_total <- nrow(indicator_data)
  valid_counts <- sapply(indicator_data, function(x) sum(!is.na(x)))
  valid_ratios <- valid_counts / n_total
  
  # Keep only indicators with enough valid data
  valid_indicators <- names(valid_ratios)[valid_ratios >= min_valid_ratio]
  
  if (length(valid_indicators) < 2) {
    stop("Insufficient indicators with enough valid data for VIF")
  }
  
  cat("Using", length(valid_indicators), "indicators with >=", 
      min_valid_ratio * 100, "% valid data\n")
  
  indicator_data <- indicator_data %>%
    select(all_of(valid_indicators)) %>%
    na.omit()
  
  if (nrow(indicator_data) < 10) {
    warning("Very few complete cases (", nrow(indicator_data), 
            ") for VIF calculation. Results may be unreliable.")
  }
  
  # Calculate VIF for each indicator
  vif_results <- data.frame(
    indicator = names(indicator_data),
    VIF = numeric(ncol(indicator_data)),
    stringsAsFactors = FALSE
  )
  
  for (i in seq_along(indicator_data)) {
    # Fit linear model with indicator as response
    formula_str <- paste(names(indicator_data)[i], "~ .")
    model <- lm(as.formula(formula_str), data = indicator_data)
    
    # Calculate VIF
    r_squared <- summary(model)$r.squared
    vif_results$VIF[i] <- 1 / (1 - r_squared)
  }
  
  vif_results <- vif_results %>%
    arrange(desc(VIF)) %>%
    mutate(
      severity = case_when(
        VIF < 5 ~ "Low",
        VIF < 10 ~ "Moderate",
        TRUE ~ "High"
      )
    )
  
  return(vif_results)
}


#' Plot VIF values
#'
#' @param vif_results Output from calculate_vif()
#'
#' @return ggplot object
#'
plot_vif <- function(vif_results) {
  
  ggplot(vif_results, aes(x = reorder(indicator, VIF), y = VIF, fill = severity)) +
    geom_col() +
    geom_hline(yintercept = c(5, 10), linetype = "dashed", color = c("orange", "red")) +
    scale_fill_manual(
      values = c("Low" = "lightgreen", "Moderate" = "orange", "High" = "red"),
      name = "Collinearity"
    ) +
    coord_flip() +
    labs(
      title = "Variance Inflation Factors (VIF)",
      subtitle = "VIF > 5 suggests moderate multicollinearity, VIF > 10 suggests high",
      x = "Indicator",
      y = "VIF"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14)
    )
}


#------------------------------------------------------------------------------
# 3. Principal Component Analysis (PCA)
#------------------------------------------------------------------------------

#' Perform PCA on vulnerability indicators
#'
#' @param data Data frame with indicators
#' @param indicators_choose Vector of indicators
#' @param use_standardized Use standardized values
#' @param scale_pca Scale variables for PCA (recommended TRUE)
#' @param min_valid_ratio Minimum proportion of non-NA values required per indicator (default 0.5)
#'
#' @return PCA results object
#'
perform_pca <- function(data,
                        indicators_choose = tbl_indicators$abbrev,
                        use_standardized = TRUE,
                        scale_pca = TRUE,
                        min_valid_ratio = 0.5) {
  
  # Prepare indicator column names
  if (use_standardized) {
    indicator_cols <- paste0("std_", indicators_choose)
  } else {
    indicator_cols <- indicators_choose
  }
  
  # Find which columns actually exist in the data
  # Check for exact matches and also columns ending with "_mean"
  available_cols <- c()
  for (ind in indicator_cols) {
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
    stop("No indicator columns found in data. Check column names.")
  }
  
  cat("Found", length(available_cols), "indicator columns out of", length(indicator_cols), "requested\n")
  
  # Select metadata and indicator columns
  metadata_cols <- c("FULL_CU_IN", "SPECIES_NAME")
  metadata_cols <- metadata_cols[metadata_cols %in% names(data)]
  
  indicator_data <- data %>%
    select(all_of(c(metadata_cols, available_cols)))
  
  # Check how many valid values each indicator has
  n_total <- nrow(indicator_data)
  valid_counts <- sapply(indicator_data[available_cols], function(x) sum(!is.na(x)))
  valid_ratios <- valid_counts / n_total
  
  cat("\nIndicator validity:\n")
  print(data.frame(
    indicator = names(valid_ratios),
    n_valid = valid_counts,
    pct_valid = round(valid_ratios * 100, 1)
  ))
  
  # Remove indicators with too many missing values
  valid_indicators <- names(valid_ratios)[valid_ratios >= min_valid_ratio]
  
  if (length(valid_indicators) < 2) {
    stop("Insufficient indicators with enough valid data. Need at least 2 indicators with >= ",
         min_valid_ratio * 100, "% non-missing values.")
  }
  
  cat("\nUsing", length(valid_indicators), "indicators with >=", min_valid_ratio * 100, "% valid data\n")
  
  # Prepare final data for PCA
  indicator_data <- indicator_data %>%
    select(all_of(c(metadata_cols, valid_indicators)))
  
  # Separate metadata from indicators
  if (length(metadata_cols) > 0) {
    metadata <- indicator_data %>%
      select(all_of(metadata_cols))
    
    pca_data <- indicator_data %>%
      select(-all_of(metadata_cols))
  } else {
    metadata <- NULL
    pca_data <- indicator_data
  }
  
  # Remove rows with any missing values
  complete_rows <- complete.cases(pca_data)
  n_complete <- sum(complete_rows)
  
  cat("Complete cases:", n_complete, "out of", n_total, "(", 
      round(n_complete/n_total * 100, 1), "%)\n")
  
  if (n_complete < 10) {
    stop("Insufficient complete cases for PCA. Only ", n_complete, " CUs have data for all indicators.")
  }
  
  pca_data <- pca_data[complete_rows, ]
  if (!is.null(metadata)) {
    metadata <- metadata[complete_rows, ]
  }
  
  # Perform PCA
  pca_result <- PCA(pca_data,
                    scale.unit = scale_pca,
                    ncp = min(10, ncol(pca_data)),
                    graph = FALSE
  )
  
  # Add metadata to results
  if (!is.null(metadata)) {
    pca_result$metadata <- metadata
  }
  
  # Calculate indicator contributions to each PC
  var_contrib <- as.data.frame(pca_result$var$contrib) %>%
    rownames_to_column("indicator") %>%
    pivot_longer(-indicator, names_to = "PC", values_to = "contribution")
  
  pca_result$var_contrib_long <- var_contrib
  pca_result$n_complete_cases <- n_complete
  pca_result$indicators_used <- valid_indicators
  
  return(pca_result)
}


#' Plot PCA scree plot (variance explained)
#'
#' @param pca_result Output from perform_pca()
#'
#' @return ggplot object
#'
plot_pca_scree <- function(pca_result) {
  
  fviz_eig(pca_result,
           addlabels = TRUE,
           ylim = c(0, 50),
           main = "Variance Explained by Principal Components",
           xlab = "Principal Component",
           ylab = "Percentage of Variance Explained"
  ) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold", size = 14))
}


#' Plot PCA biplot (CUs and indicators)
#'
#' @param pca_result Output from perform_pca()
#' @param dims PCA dimensions to plot
#' @param color_by Variable to color points by
#'
#' @return ggplot object
#'
plot_pca_biplot <- function(pca_result,
                            dims = c(1, 2),
                            color_by = "SPECIES_NAME") {
  
  # Create biplot with species coloring
  p <- fviz_pca_biplot(pca_result,
                       axes = dims,
                       geom.ind = "point",
                       geom.var = c("arrow", "text"),
                       col.ind = pca_result$metadata[[color_by]],
                       col.var = "black",
                       alpha.ind = 0.6,
                       repel = TRUE,
                       title = paste0("PCA Biplot (PC", dims[1], " vs PC", dims[2], ")"),
                       legend.title = color_by
  ) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold", size = 14))
  
  return(p)
}


#' Plot indicator contributions to principal components
#'
#' @param pca_result Output from perform_pca()
#' @param n_pcs Number of PCs to show
#' @param top_n Show only top N contributing indicators
#'
#' @return ggplot object
#'
plot_pca_contributions <- function(pca_result,
                                   n_pcs = 3,
                                   top_n = 10) {
  
  contrib_data <- pca_result$var_contrib_long %>%
    filter(PC %in% paste0("Dim.", 1:n_pcs)) %>%
    group_by(PC) %>%
    slice_max(contribution, n = top_n) %>%
    ungroup()
  
  ggplot(contrib_data, aes(x = reorder(indicator, contribution), 
                           y = contribution, fill = PC)) +
    geom_col() +
    facet_wrap(~PC, scales = "free_y", ncol = 1) +
    coord_flip() +
    labs(
      title = "Top Indicator Contributions to Principal Components",
      x = "Indicator",
      y = "Contribution (%)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      legend.position = "none"
    )
}


#' Create PCA scores for CUs
#'
#' @param pca_result Output from perform_pca()
#' @param n_components Number of PC scores to extract
#'
#' @return Data frame with CU identifiers and PC scores
#'
extract_pca_scores <- function(pca_result,
                               n_components = 5) {
  
  scores <- as.data.frame(pca_result$ind$coord[, 1:n_components])
  colnames(scores) <- paste0("PC", 1:n_components)
  
  scores_with_metadata <- bind_cols(
    pca_result$metadata,
    scores
  )
  
  return(scores_with_metadata)
}


#------------------------------------------------------------------------------
# 4. Hierarchical Clustering
#------------------------------------------------------------------------------

#' Perform hierarchical clustering on CUs based on indicators
#'
#' @param data Data frame with indicators
#' @param indicators_choose Vector of indicators
#' @param use_standardized Use standardized values
#' @param method Clustering method (e.g., "complete", "ward.D2")
#' @param min_valid_ratio Minimum proportion of non-NA values required
#'
#' @return hclust object
#'
perform_hierarchical_clustering <- function(data,
                                            indicators_choose = tbl_indicators$abbrev,
                                            use_standardized = TRUE,
                                            method = "ward.D2",
                                            min_valid_ratio = 0.5) {
  
  # Prepare indicator column names
  if (use_standardized) {
    indicator_cols <- paste0("std_", indicators_choose)
  } else {
    indicator_cols <- indicators_choose
  }
  
  # Find which columns actually exist
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
  
  if (length(available_cols) < 2) {
    stop("Need at least 2 indicators for clustering")
  }
  
  # Select data
  cluster_data <- data %>%
    select(FULL_CU_IN, all_of(available_cols))
  
  # Check validity and keep only indicators with enough valid data
  n_total <- nrow(cluster_data)
  valid_counts <- sapply(cluster_data[available_cols], function(x) sum(!is.na(x)))
  valid_ratios <- valid_counts / n_total
  valid_indicators <- names(valid_ratios)[valid_ratios >= min_valid_ratio]
  
  if (length(valid_indicators) < 2) {
    stop("Insufficient indicators with enough valid data for clustering")
  }
  
  cluster_data <- cluster_data %>%
    select(FULL_CU_IN, all_of(valid_indicators)) %>%
    na.omit() %>%
    remove_rownames() %>%
    column_to_rownames("FULL_CU_IN")
  
  if (nrow(cluster_data) < 3) {
    stop("Insufficient complete cases for clustering. Only ", nrow(cluster_data), " CUs have complete data.")
  }
  
  cat("Clustering", nrow(cluster_data), "CUs using", ncol(cluster_data), "indicators\n")
  
  # Calculate distance matrix
  dist_matrix <- dist(cluster_data, method = "euclidean")
  
  # Perform clustering
  hc <- hclust(dist_matrix, method = method)
  
  return(hc)
}


#' Plot dendrogram with species coloring
#'
#' @param hc_result Output from perform_hierarchical_clustering()
#' @param data Original data with species information
#' @param k Number of clusters to highlight
#'
#' @return ggplot object
#'
plot_dendrogram <- function(hc_result,
                            data,
                            k = 4) {
  
  # Check if clustering result is NULL
  if (is.null(hc_result)) {
    cat("Warning: Clustering result is NULL. Cannot create dendrogram.\n")
    return(NULL)
  }
  
  # Check if hc_result is an hclust object
  if (!inherits(hc_result, "hclust")) {
    cat("Warning: Input is not an hclust object. Cannot create dendrogram.\n")
    return(NULL)
  }
  
  # Create dendrogram
  dend <- as.dendrogram(hc_result)
  
  # Get species for coloring
  cu_species <- data %>%
    select(FULL_CU_IN, SPECIES_NAME) %>%
    distinct()
  
  # Get unique CU IDs from clustering (may be less than full data due to NA removal)
  cluster_labels <- labels(dend)
  
  species_colors <- setNames(
    rainbow(length(unique(cu_species$SPECIES_NAME))),
    unique(cu_species$SPECIES_NAME)
  )
  
  # Color by species - match to labels in dendrogram
  labels_colors <- cu_species$SPECIES_NAME[match(cluster_labels, cu_species$FULL_CU_IN)]
  labels_colors <- species_colors[labels_colors]
  
  # Handle any unmatched labels
  labels_colors[is.na(labels_colors)] <- "black"
  
  dend <- dend %>%
    set("labels_col", labels_colors) %>%
    set("labels_cex", 0.6)
  
  # Plot
  par(mar = c(8, 4, 2, 2))
  plot(dend,
       main = paste0("Hierarchical Clustering of Conservation Units (k=", k, ")"),
       xlab = "",
       ylab = "Height"
  )
  
  # Add rectangles for clusters
  rect.dendrogram(dend, k = k, border = 2:5)
  
  # Add legend
  legend("topright",
         legend = names(species_colors),
         col = species_colors,
         pch = 19,
         cex = 0.7,
         title = "Species"
  )
}


#' Plot dendrogram using ggplot
#'
#' @param hc_result Output from perform_hierarchical_clustering()
#' @param data Original data with species information
#'
#' @return ggplot object
#'
plot_dendrogram_ggplot <- function(hc_result,
                                   data) {
  
  # Check if clustering result is NULL
  if (is.null(hc_result)) {
    cat("Warning: Clustering result is NULL. Cannot create dendrogram.\n")
    return(NULL)
  }
  
  # Check if hc_result is an hclust object
  if (!inherits(hc_result, "hclust")) {
    cat("Warning: Input is not an hclust object. Cannot create dendrogram.\n")
    return(NULL)
  }
  
  dend_data <- dendro_data(hc_result)
  
  # Get species information
  cu_species <- data %>%
    select(FULL_CU_IN, SPECIES_NAME) %>%
    distinct()
  
  dend_data$labels <- dend_data$labels %>%
    left_join(cu_species, by = c("label" = "FULL_CU_IN"))
  
  ggplot() +
    geom_segment(data = dend_data$segments,
                 aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_text(data = dend_data$labels,
              aes(x = x, y = y, label = label, color = SPECIES_NAME),
              hjust = 1, angle = 90, size = 2.5) +
    labs(
      title = "Hierarchical Clustering Dendrogram",
      x = "",
      y = "Height",
      color = "Species"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      panel.grid = element_blank()
    )
}


#' Identify optimal number of clusters
#'
#' @param data Data frame with indicators
#' @param indicators_choose Vector of indicators
#' @param use_standardized Use standardized values
#' @param max_k Maximum number of clusters to test
#'
#' @return ggplot object showing elbow plot
#'
plot_optimal_clusters <- function(data,
                                  indicators_choose = tbl_indicators$abbrev,
                                  use_standardized = TRUE,
                                  max_k = 10) {
  
  # Prepare data (same as clustering function)
  if (use_standardized) {
    indicator_cols <- paste0("std_", indicators_choose)
  } else {
    indicator_cols <- indicators_choose
  }
  
  indicator_cols <- indicator_cols[indicator_cols %in% names(data)]
  
  cluster_data <- data %>%
    select(all_of(indicator_cols)) %>%
    na.omit()
  
  fviz_nbclust(cluster_data,
               FUN = hcut,
               method = "wss",
               k.max = max_k
  ) +
    labs(
      title = "Optimal Number of Clusters (Elbow Method)",
      x = "Number of Clusters",
      y = "Total Within-Cluster Sum of Squares"
    ) +
    theme_minimal()
}


#------------------------------------------------------------------------------
# 5. Indicator Relationships with Composite Scores
#------------------------------------------------------------------------------

#' Correlate individual indicators with composite vulnerability scores
#'
#' @param data Data frame with indicators and composite scores
#' @param indicators_choose Vector of indicators
#' @param score_cols Composite score columns to correlate with
#' @param use_standardized Use standardized values
#'
#' @return Data frame with correlations
#'
correlate_indicators_with_scores <- function(data,
                                             indicators_choose = tbl_indicators$abbrev,
                                             score_cols = c("std_addall", "std_avgall", "std_sumavgs"),
                                             use_standardized = TRUE) {
  
  # Prepare data
  if (use_standardized) {
    indicator_cols <- paste0("std_", indicators_choose)
  } else {
    indicator_cols <- indicators_choose
  }
  
  indicator_cols <- indicator_cols[indicator_cols %in% names(data)]
  score_cols <- score_cols[score_cols %in% names(data)]
  
  # Remove NA values for correlation
  data_clean <- data %>%
    select(all_of(c(indicator_cols, score_cols))) %>%
    na.omit()
  
  # Calculate correlations
  cor_results <- map_dfr(indicator_cols, function(ind) {
    map_dfr(score_cols, function(score) {
      
      # Extract numeric vectors
      x_vals <- as.numeric(data_clean[[ind]])
      y_vals <- as.numeric(data_clean[[score]])
      
      # Check if both vectors are numeric and have variance
      if (is.numeric(x_vals) && is.numeric(y_vals) && 
          length(x_vals) > 2 && sd(x_vals, na.rm = TRUE) > 0 && sd(y_vals, na.rm = TRUE) > 0) {
        
        cor_test <- cor.test(x_vals, y_vals, method = "spearman", exact = FALSE)
        
        tibble(
          indicator = ind,
          composite_score = score,
          correlation = as.numeric(cor_test$estimate),
          p_value = cor_test$p.value
        )
      } else {
        tibble(
          indicator = ind,
          composite_score = score,
          correlation = NA_real_,
          p_value = NA_real_
        )
      }
    })
  })
  
  cor_results <- cor_results %>%
    filter(!is.na(correlation)) %>%
    arrange(composite_score, desc(abs(correlation)))
  
  return(cor_results)
}


#' Plot indicator correlations with composite scores
#'
#' @param cor_results Output from correlate_indicators_with_scores()
#' @param score_type Which composite score to plot
#'
#' @return ggplot object
#'
plot_indicator_score_correlations <- function(cor_results,
                                              score_type = "std_addall") {
  
  plot_data <- cor_results %>%
    filter(composite_score == score_type) %>%
    mutate(significant = ifelse(p_value < 0.05, "Sig", "Non-sig"))
  
  ggplot(plot_data, aes(x = reorder(indicator, correlation), 
                        y = correlation, fill = significant)) +
    geom_col() +
    geom_hline(yintercept = 0, linetype = "solid") +
    scale_fill_manual(values = c("Sig" = "steelblue", "Non-sig" = "gray80")) +
    coord_flip() +
    labs(
      title = paste("Indicator Correlations with", score_type),
      x = "Indicator",
      y = "Spearman Correlation",
      fill = "Significance\n(p < 0.05)"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold", size = 14))
}


#------------------------------------------------------------------------------
# 6. Comprehensive Analysis Function
#------------------------------------------------------------------------------

#' Run complete correlation, PCA, and clustering analysis
#'
#' @param data Data frame with indicators (e.g., all_flat_std filtered by rcp and period)
#' @param indicators_choose Vector of indicators
#' @param tbl_indicators Indicator metadata table
#' @param use_standardized Use standardized indicator values
#' @param include_composite_scores Whether data includes composite scores for correlation
#' @param output_dir Directory to save results
#'
#' @return List with all analysis results
#'
run_comprehensive_analysis <- function(data,
                                       indicators_choose = tbl_indicators$abbrev,
                                       tbl_indicators = tbl_indicators,
                                       use_standardized = TRUE,
                                       include_composite_scores = FALSE,
                                       output_dir = "outputs/correlation_analysis") {
  
  cat("\n========================================\n")
  cat("Comprehensive Indicator Analysis\n")
  cat("========================================\n\n")
  cat("Number of CUs:", nrow(data), "\n")
  cat("Number of indicators:", length(indicators_choose), "\n")
  cat("Using standardized values:", use_standardized, "\n\n")
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # 1. Correlation Analysis
  cat("1. Calculating correlation matrix...\n")
  cor_results <- calculate_correlation_matrix(data, indicators_choose, use_standardized)
  
  if (!is.null(cor_results$significant_pairs) && nrow(cor_results$significant_pairs) > 0) {
    cat("   Found", nrow(cor_results$significant_pairs), "significant correlations\n")
  } else {
    cat("   No significant correlations found\n")
  }
  
  # 2. VIF Analysis
  cat("\n2. Calculating Variance Inflation Factors...\n")
  vif_results <- tryCatch({
    calculate_vif(data, indicators_choose, use_standardized)
  }, error = function(e) {
    cat("   Warning: VIF calculation failed:", e$message, "\n")
    NULL
  })
  
  if (!is.null(vif_results)) {
    high_vif <- sum(vif_results$VIF > 10, na.rm = TRUE)
    cat("   ", high_vif, "indicators with high multicollinearity (VIF > 10)\n")
  }
  
  # 3. PCA
  cat("\n3. Performing Principal Component Analysis...\n")
  pca_results <- tryCatch({
    perform_pca(data, indicators_choose, use_standardized)
  }, error = function(e) {
    cat("   Warning: PCA failed:", e$message, "\n")
    NULL
  })
  
  if (!is.null(pca_results)) {
    var_explained <- pca_results$eig[1:min(3, nrow(pca_results$eig)), 2]
    cat("   PC1 explains", round(var_explained[1], 1), "% of variance\n")
    if (length(var_explained) > 1) cat("   PC2 explains", round(var_explained[2], 1), "% of variance\n")
    if (length(var_explained) > 2) cat("   PC3 explains", round(var_explained[3], 1), "% of variance\n")
  }
  
  # 4. Hierarchical Clustering
  cat("\n4. Performing hierarchical clustering...\n")
  hc_results <- tryCatch({
    perform_hierarchical_clustering(data, indicators_choose, use_standardized)
  }, error = function(e) {
    cat("   Warning: Clustering failed:", e$message, "\n")
    NULL
  })
  
  # 5. Indicator-Score Correlations (only if composite scores available)
  score_cors <- NULL
  if (include_composite_scores) {
    cat("\n5. Correlating indicators with composite scores...\n")
    score_cors <- tryCatch({
      correlate_indicators_with_scores(data, indicators_choose, use_standardized = use_standardized)
    }, error = function(e) {
      cat("   Warning: Indicator-score correlation failed:", e$message, "\n")
      NULL
    })
  } else {
    cat("\n5. Skipping indicator-score correlations (no composite scores in data)\n")
  }
  
  cat("\nAnalysis complete!\n")
  
  # Return all results
  return(list(
    correlation = cor_results,
    vif = vif_results,
    pca = pca_results,
    clustering = hc_results,
    indicator_score_cors = score_cors,
    metadata = list(
      n_CUs = nrow(data),
      n_indicators = length(indicators_choose),
      use_standardized = use_standardized,
      date_run = Sys.Date()
    )
  ))
}


#' Export all analysis results
#'
#' @param analysis_results Output from run_comprehensive_analysis()
#' @param data Original data
#' @param output_dir Directory to save results
#'
export_analysis_results <- function(analysis_results,
                                    data,
                                    output_dir = "outputs/correlation_analysis") {
  
  timestamp <- format(Sys.Date(), "%Y-%m-%d")
  
  # Export correlation results
  if (!is.null(analysis_results$correlation) && 
      !is.null(analysis_results$correlation$significant_pairs) &&
      nrow(analysis_results$correlation$significant_pairs) > 0) {
    write.csv(
      analysis_results$correlation$significant_pairs,
      file.path(output_dir, paste0("significant_correlations_", timestamp, ".csv")),
      row.names = FALSE
    )
  }
  
  # Export VIF
  if (!is.null(analysis_results$vif)) {
    write.csv(
      analysis_results$vif,
      file.path(output_dir, paste0("vif_results_", timestamp, ".csv")),
      row.names = FALSE
    )
  }
  
  # Export PCA scores
  if (!is.null(analysis_results$pca)) {
    pca_scores <- extract_pca_scores(analysis_results$pca)
    write.csv(
      pca_scores,
      file.path(output_dir, paste0("pca_scores_", timestamp, ".csv")),
      row.names = FALSE
    )
    
    # Export PCA loadings
    pca_loadings <- as.data.frame(analysis_results$pca$var$coord)
    pca_loadings$indicator <- rownames(pca_loadings)
    write.csv(
      pca_loadings,
      file.path(output_dir, paste0("pca_loadings_", timestamp, ".csv")),
      row.names = FALSE
    )
  }
  
  # Export indicator-score correlations
  if (!is.null(analysis_results$indicator_score_cors)) {
    write.csv(
      analysis_results$indicator_score_cors,
      file.path(output_dir, paste0("indicator_score_correlations_", timestamp, ".csv")),
      row.names = FALSE
    )
  }
  
  # Save plots
  if (!is.null(analysis_results$correlation)) {
    png(file.path(output_dir, paste0("correlation_matrix_", timestamp, ".png")),
        width = 12, height = 10, units = "in", res = 300)
    plot_correlation_matrix(analysis_results$correlation)
    dev.off()
  }
  
  if (!is.null(analysis_results$vif)) {
    ggsave(
      file.path(output_dir, paste0("vif_plot_", timestamp, ".png")),
      plot = plot_vif(analysis_results$vif),
      width = 10, height = 8, dpi = 300
    )
  }
  
  if (!is.null(analysis_results$pca)) {
    ggsave(
      file.path(output_dir, paste0("pca_scree_", timestamp, ".png")),
      plot = plot_pca_scree(analysis_results$pca),
      width = 10, height = 6, dpi = 300
    )
    
    ggsave(
      file.path(output_dir, paste0("pca_biplot_PC1_PC2_", timestamp, ".png")),
      plot = plot_pca_biplot(analysis_results$pca, dims = c(1, 2)),
      width = 12, height = 10, dpi = 300
    )
    
    ggsave(
      file.path(output_dir, paste0("pca_biplot_PC2_PC3_", timestamp, ".png")),
      plot = plot_pca_biplot(analysis_results$pca, dims = c(2, 3)),
      width = 12, height = 10, dpi = 300
    )
    
    ggsave(
      file.path(output_dir, paste0("pca_contributions_", timestamp, ".png")),
      plot = plot_pca_contributions(analysis_results$pca),
      width = 10, height = 12, dpi = 300
    )
  }
  
  if (!is.null(analysis_results$clustering)) {
    # Try to create dendrogram plots, but handle gracefully if they fail
    tryCatch({
      png(file.path(output_dir, paste0("dendrogram_", timestamp, ".png")),
          width = 14, height = 10, units = "in", res = 300)
      plot_dendrogram(analysis_results$clustering, data, k = 4)
      dev.off()
    }, error = function(e) {
      cat("Warning: Could not create base dendrogram plot:", e$message, "\n")
    })
    
    tryCatch({
      dend_plot <- plot_dendrogram_ggplot(analysis_results$clustering, data)
      if (!is.null(dend_plot)) {
        ggsave(
          file.path(output_dir, paste0("dendrogram_ggplot_", timestamp, ".png")),
          plot = dend_plot,
          width = 12, height = 14, dpi = 300
        )
      }
    }, error = function(e) {
      cat("Warning: Could not create ggplot dendrogram:", e$message, "\n")
    })
  }
  
  if (!is.null(analysis_results$indicator_score_cors)) {
    ggsave(
      file.path(output_dir, paste0("indicator_score_cors_", timestamp, ".png")),
      plot = plot_indicator_score_correlations(analysis_results$indicator_score_cors),
      width = 10, height = 8, dpi = 300
    )
  }
  
  cat("\nAll available results exported to:", output_dir, "\n")
}


#------------------------------------------------------------------------------
# Example Usage
#------------------------------------------------------------------------------

# ============= ANALYSIS ON INDICATOR VALUES (all_flat_std) =============

# Filter to specific scenario (RCP 4.5, mid-century)
data_filtered <- all_flat_std %>%
  filter(rcp == "45", period_code == 3)

cat("Filtered data has", nrow(data_filtered), "CUs\n")

# Define which indicators to analyze (from your tbl_indicators)
indicators_to_analyze <- tbl_indicators$abbrev

# Run comprehensive analysis on standardized indicator values
analysis_results <- run_comprehensive_analysis(
  data = data_filtered,
  indicators_choose = indicators_to_analyze,
  tbl_indicators = tbl_indicators,
  use_standardized = TRUE,  # Use standardized values (std_*)
  include_composite_scores = FALSE,  # Set FALSE for all_flat_std without composite scores
  output_dir = "outputs/correlation_analysis/rcp45_midcentury"
)

clustering_results <- perform_hierarchical_clustering(data = data_filtered)

# View key results
cat("\n=== Top Correlated Indicators ===\n")
if (!is.null(analysis_results$correlation$significant_pairs)) {
  print(head(analysis_results$correlation$significant_pairs, 10))
}

cat("\n=== High VIF Indicators (potential multicollinearity) ===\n")
if (!is.null(analysis_results$vif)) {
  print(filter(analysis_results$vif, VIF > 5))
}

cat("\n=== PCA Variance Explained ===\n")
if (!is.null(analysis_results$pca)) {
  print(analysis_results$pca$eig[1:5, ])
}

# Create individual plots
if (!is.null(analysis_results$correlation)) {
  plot_correlation_matrix(analysis_results$correlation)
}

if (!is.null(analysis_results$vif)) {
  plot_vif(analysis_results$vif)
}

if (!is.null(analysis_results$pca)) {
  plot_pca_scree(analysis_results$pca)
  plot_pca_biplot(analysis_results$pca, dims = c(1, 2))
  plot_pca_biplot(analysis_results$pca, dims = c(2, 3))
  plot_pca_contributions(analysis_results$pca, n_pcs = 3)
}

if (!is.null(analysis_results$clustering)) {
  plot_dendrogram(analysis_results$clustering, data_filtered, k = 5)
  plot_dendrogram_ggplot(analysis_results$clustering, data_filtered)
  plot_optimal_clusters(data_filtered, indicators_to_analyze, max_k = 10)
}

# Export everything
export_analysis_results(
  analysis_results,
  data_filtered,
  output_dir = "outputs/correlation_analysis/rcp45_midcentury"
)


# ============= ANALYSIS WITH COMPOSITE SCORES (combined_scores_std) =============

# If you have the combined_scores_std object with composite vulnerability scores
# load(file.path(paths$indicators, "combined_scores.Rdata"))
# OR
# combined_scores_std <- read.csv("path/to/vulnerability_scores_with_species_ranks.csv")

# data_with_scores <- combined_scores_std %>%
#   filter(rcp == "45", period_code == 3)
#
# analysis_with_scores <- run_comprehensive_analysis(
#   data = data_with_scores,
#   indicators_choose = tbl_indicators$abbrev,
#   use_standardized = TRUE,
#   include_composite_scores = TRUE,  # Set TRUE when you have composite scores
#   output_dir = "outputs/correlation_analysis/with_scores"
# )
#
# # Now you can also see how individual indicators correlate with overall vulnerability
# if (!is.null(analysis_with_scores$indicator_score_cors)) {
#   print(analysis_with_scores$indicator_score_cors)
#   plot_indicator_score_correlations(analysis_with_scores$indicator_score_cors, "std_addall")
# }
#
# export_analysis_results(analysis_with_scores, data_with_scores)


# ============= COMPARE ACROSS SCENARIOS =============

# Run analysis for multiple scenarios and compare
# scenarios <- expand.grid(
#   rcp = c("45", "85"),
#   period = c(3, 5)  # mid-century and end-century
# )
#
# scenario_results <- list()
#
# for (i in 1:nrow(scenarios)) {
#   rcp_i <- scenarios$rcp[i]
#   period_i <- scenarios$period[i]
#
#   data_i <- all_flat_std %>%
#     filter(rcp == rcp_i, period_code == period_i)
#
#   scenario_name <- paste0("RCP", rcp_i, "_period", period_i)
#
#   scenario_results[[scenario_name]] <- run_comprehensive_analysis(
#     data = data_i,
#     indicators_choose = tbl_indicators$abbrev,
#     use_standardized = TRUE,
#     include_composite_scores = FALSE,
#     output_dir = paste0("outputs/correlation_analysis/", scenario_name)
#   )
#
#   export_analysis_results(
#     scenario_results[[scenario_name]],
#     data_i,
#     output_dir = paste0("outputs/correlation_analysis/", scenario_name)
#   )
# }
#
# # Compare correlation structures across scenarios
# for (scenario in names(scenario_results)) {
#   cat("\n===", scenario, "===\n")
#   print(head(scenario_results[[scenario]]$correlation$significant_pairs, 5))
# }