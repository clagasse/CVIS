#--------------------------------------------------------------------------
# 4d_risk_drivers_analysis.R
# Analyze multivariate risk profiles and identify dominant factors across CUs
# Includes PCA for clustering and identifying specific indicator drivers
#--------------------------------------------------------------------------

source(here::here("code", "0_setup.R"))

# Load standardized long format data from RData (output of 4a)
cat("Loading standardized indicator data...\n")
load(file.path(paths$output, "scoring_results.Rdata")) # loads all_std_long, scores_tidy

# Use centralized baseline settings from 0_setup.R
cat_rcp <- sens_rcp_base
cat_period <- sens_period_base
cat_gcm <- sens_gcm_base

# 1. Filter and Prepare Analysis Data --------------------------------------

# We want to combine the future projections for the chosen scenario
# with the static indicators (those that don't change across scenarios)
cat("Filtering target scenario and merging static fallback indicators...\n")

# Get target dynamic indicators
dynamic_dat <- all_std_long %>%
    filter(
        rcp == cat_rcp,
        period_code == cat_period,
        gcm == cat_gcm,
        dsmodel %in% dsmodel_baseline
    )

# Get static indicators (those with period_code 0 and gcm 0)
# These act as fallback for indicators without projections
static_dat <- all_std_long %>%
    filter(
        period_code == 0,
        gcm == 0,
        dsmodel %in% dsmodel_baseline
    ) %>%
    select(FULL_CU_IN, indicator, static_value = std_value)

# Build a grid of all expected indicators for each CU
expected_inds <- all_std_long %>%
    distinct(FULL_CU_IN, SPECIES_NAME, CVIS_NAME, indicator)

# Merge dynamic with static fallback
analysis_dat <- expected_inds %>%
    left_join(dynamic_dat %>% select(FULL_CU_IN, indicator, std_value, category),
        by = c("FULL_CU_IN", "indicator")
    ) %>%
    left_join(static_dat, by = c("FULL_CU_IN", "indicator")) %>%
    mutate(std_value = coalesce(std_value, static_value)) %>%
    filter(!is.na(std_value)) # remove if no data at all

# Use tbl_indicators for metadata (category, names)
if (exists("tbl_indicators")) {
    analysis_dat <- analysis_dat %>%
        select(-any_of("category")) %>%
        left_join(tbl_indicators %>% select(abbrev, category, name), by = c("indicator" = "abbrev"))
}

# 2. Red Flag Frequency & Indicator Variation ----------------------------
cat("Calculating red flag frequency and indicator variation across CUs...\n")

# Hinge weight function — same logic as 4a scoring
# t0 = 0.33 (partial flag starts), t1 = 0.66 (full flag)
hinge_weight <- function(s, t0 = 0.33, t1 = 0.66) {
    ifelse(s <= t0, 0, ifelse(s >= t1, 1, (s - t0) / (t1 - t0)))
}

# Apply hinge weight to each row, then summarise per indicator
red_flag_summary <- analysis_dat %>%
    mutate(
        hw          = hinge_weight(std_value),
        is_red_flag = std_value >= 0.66 # full red flag threshold
    ) %>%
    group_by(indicator, category, name) %>%
    summarise(
        n_CUs = n_distinct(FULL_CU_IN),
        n_red_flags = sum(is_red_flag, na.rm = TRUE), # count of full red flags
        total_hw = sum(hw, na.rm = TRUE), # soft count of partial flags
        pct_red_flag = mean(is_red_flag, na.rm = TRUE), # proportion of CUs flagged
        median_score = median(std_value, na.rm = TRUE),
        sd_score = sd(std_value, na.rm = TRUE),
        .groups = "drop"
    ) %>%
    arrange(desc(n_red_flags))

# Indicator variation summary (ordered by sd across CUs)
variation_summary <- red_flag_summary %>%
    arrange(desc(sd_score))

# 3. Multivariate PCA Preparation ----------------------------------------
cat("Preparing data for PCA...\n")

# Pivot to wide format for PCA (Indicators as columns)
pca_input_wide <- analysis_dat %>%
    group_by(FULL_CU_IN, SPECIES_NAME, CVIS_NAME, indicator) %>%
    summarise(val = mean(std_value, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = indicator, values_from = val)

# Handle Missing Data
# Only include CUs that have at least some data
pca_input_wide <- pca_input_wide %>%
    filter(rowSums(is.na(select(., -FULL_CU_IN, -SPECIES_NAME, -CVIS_NAME))) < (ncol(.) - 3) * 0.5)

# PCA matrix creation with mean imputation for remaining NAs
pca_ready <- pca_input_wide %>%
    select(-FULL_CU_IN, -SPECIES_NAME, -CVIS_NAME) %>%
    mutate(across(everything(), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

# Identify and remove zero-variance columns (which break prcomp scale=T)
zero_var <- sapply(pca_ready, function(x) var(x, na.rm = TRUE) == 0 | is.na(var(x, na.rm = TRUE)))
if (any(zero_var)) {
    cat("Removing zero-variance indicators:", paste(names(pca_ready)[zero_var], collapse = ", "), "\n")
    pca_ready <- pca_ready[, !zero_var]
}

# Run PCA
pca_res <- prcomp(pca_ready, scale. = TRUE)

# 4. Clustering -----------------------------------------------------------
cat("Performing cluster analysis...\n")

# Determine clusters (using 3 as a baseline for risk archetypes)
set.seed(123)
km_res <- kmeans(pca_ready, centers = 3, nstart = 25)

# Add results back to data
pca_output <- pca_input_wide %>%
    mutate(
        PC1 = pca_res$x[, 1],
        PC2 = pca_res$x[, 2],
        cluster = as.factor(km_res$cluster)
    )

# 5. Metadata Integration ------------------------------------------------
cat("Integrating geographical and timing metadata...\n")

# Load CU metadata (for watershed) and timing data
load(file.path(paths$CU, "cu_run.Rds")) # Loads cu_run
load(file.path(paths$CU, "cu_timing_data.Rdata")) # Loads cu_timing_Fr

# Prep timing bins (e.g., Early, Mid, Late based on run timing peak)
metadata_cu <- cu_run %>%
    select(FULL_CU_IN, FAZ, FAZ_group, DFO_AREA) %>%
    left_join(cu_timing_Fr %>% select(FULL_CU_IN, rt_peak, oe_peak), by = "FULL_CU_IN") %>%
    mutate(
        timing_group = case_when(
            rt_peak < 200 ~ "Early",
            rt_peak >= 200 & rt_peak < 250 ~ "Mid",
            rt_peak >= 250 ~ "Late",
            TRUE ~ "Unknown"
        ),
        FAZ = factor(FAZ),
        FAZ_group = factor(FAZ_group)
    )

# Join to PCA results
pca_output <- pca_output %>%
    left_join(metadata_cu, by = "FULL_CU_IN")

# 6. Visualizations -------------------------------------------------------
cat("Generating risk profile plots...\n")

# 6a. PCA Biplot Base (Loadings and Labels)
loadings <- as.data.frame(pca_res$rotation[, 1:2])
loadings$indicator <- rownames(loadings)

# Scale loadings for plotting
scale_fact <- max(abs(pca_output$PC1)) / max(abs(loadings$PC1)) * 0.7
loadings <- loadings %>%
    mutate(
        x_end = PC1 * scale_fact,
        y_end = PC2 * scale_fact
    )

p_base_pca <- ggplot(pca_output, aes(x = PC1, y = PC2)) +
    geom_text(aes(label = FULL_CU_IN), size = 2.2, vjust = 1.6, alpha = 0.5, fontface = "bold") +
    geom_segment(
        data = loadings %>% filter(abs(PC1) > 0.15 | abs(PC2) > 0.15),
        aes(x = 0, y = 0, xend = x_end, yend = y_end),
        arrow = arrow(length = unit(0.2, "cm")), color = "grey40"
    ) +
    geom_text(
        data = loadings %>% filter(abs(PC1) > 0.15 | abs(PC2) > 0.15),
        aes(x = x_end * 1.1, y = y_end * 1.1, label = indicator),
        size = 3.5, color = "black", fontface = "bold"
    ) +
    labs(
        subtitle = paste(
            "Variance Explained: PC1 =",
            round(pca_res$sdev[1]^2 / sum(pca_res$sdev^2) * 100, 1), "% | PC2 =",
            round(pca_res$sdev[2]^2 / sum(pca_res$sdev^2) * 100, 1), "%"
        ),
        x = "PC1", y = "PC2"
    ) +
    theme_cvis()

# Plot 1: Standard Clusters (with Species Shape)
p_pca_clusters <- p_base_pca +
    geom_point(aes(color = cluster, shape = SPECIES_NAME), size = 3, alpha = 0.7) +
    scale_color_brewer(palette = "Set1", name = "Risk Cluster") +
    labs(title = "CU Risk Profile PCA - Clusters")

ggsave(file.path(paths$figures, "risk_profile_pca_clusters.png"), p_pca_clusters, width = 10, height = 8)

# 6b. PCA Colored by Watershed (No Species Shape)
p_pca_ws <- p_base_pca +
    geom_point(aes(color = FAZ_group), size = 3, alpha = 0.7) +
    scale_color_scico_d(palette = "batlow", name = "FAZ Group") +
    labs(title = "CU Risk Profile PCA - Watershed (FAZ Group) Influence")

ggsave(file.path(paths$figures, "risk_profile_pca_watershed.png"), p_pca_ws, width = 11, height = 8)

# 6c. PCA Colored by Migration Timing (No Species Shape)
p_pca_time <- p_base_pca +
    geom_point(aes(color = timing_group), size = 3, alpha = 0.7) +
    scale_color_brewer(palette = "Dark2", name = "Run Timing Group") +
    labs(title = "CU Risk Profile PCA - Phenology (Timing) Influence")

ggsave(file.path(paths$figures, "risk_profile_pca_timing.png"), p_pca_time, width = 11, height = 8)

# 6d. Red Flag Frequency Bar Chart
p_drivers <- ggplot(
    red_flag_summary %>% head(15),
    aes(x = reorder(name, n_red_flags), y = n_red_flags, fill = category)
) +
    geom_col() +
    geom_text(aes(label = paste0(round(pct_red_flag * 100), "%")),
        hjust = -0.1, size = 3, fontface = "bold"
    ) +
    coord_flip(clip = "off") +
    scale_fill_scico_d(palette = "roma", name = "Category") +
    labs(
        title = "Red Flag Frequency by Indicator",
        subtitle = paste0("Number of CUs where indicator exceeds red flag threshold (score ≥ 0.66); % = share of all CUs"),
        x = NULL, y = "Count of CUs with Red Flag"
    ) +
    theme_cvis() +
    theme(plot.margin = margin(5, 40, 5, 5))

ggsave(file.path(paths$figures, "indicator_red_flag_frequency.png"), p_drivers, width = 10, height = 7)

# 6d2. Indicator Variation (SD + Median) across CUs
variation_long <- variation_summary %>%
    select(name, category, median_score, sd_score) %>%
    pivot_longer(
        cols = c(median_score, sd_score),
        names_to = "metric", values_to = "value"
    ) %>%
    mutate(metric = recode(metric,
        "median_score" = "Median Score",
        "sd_score"     = "Std. Dev. (Variation across CUs)"
    ))

p_variation <- ggplot(
    variation_long,
    aes(x = reorder(name, value), y = value, fill = category)
) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~metric, scales = "free_x") +
    coord_flip() +
    scale_fill_scico_d(palette = "roma", name = "Category") +
    labs(
        title = "Indicator Variation and Central Tendency Across CUs",
        subtitle = "Left: spread (SD) — how consistently indicators vary between CUs. Right: median exposure level.",
        x = NULL, y = NULL
    ) +
    theme_cvis()

ggsave(file.path(paths$figures, "indicator_score_variation.png"), p_variation, width = 12, height = 7)

# 6e. Risk Cluster Profiles
cluster_profiles <- pca_output %>%
    select(-any_of(c("FULL_CU_IN", "SPECIES_NAME", "CVIS_NAME", "PC1", "PC2", "FAZ", "FAZ_group", "DFO_AREA", "rt_peak", "oe_peak", "timing_group"))) %>%
    group_by(cluster) %>%
    summarise(across(everything(), mean, na.rm = TRUE)) %>%
    pivot_longer(-cluster, names_to = "indicator", values_to = "avg_std_score")

# Filter cluster profiles by top red-flagging indicators
top_inds <- red_flag_summary$indicator[seq_len(min(10, nrow(red_flag_summary)))]
cluster_profiles_sub <- cluster_profiles %>%
    filter(indicator %in% top_inds) %>%
    left_join(tbl_indicators %>% select(abbrev, name), by = c("indicator" = "abbrev"))

p_clusters <- ggplot(cluster_profiles_sub, aes(x = name, y = avg_std_score, fill = cluster)) +
    geom_col(position = "dodge") +
    scale_fill_brewer(palette = "Set1", name = "Cluster") +
    labs(
        title = "Average Risk Signatures by Cluster",
        subtitle = "Top 10 indicators driving variance",
        y = "Mean Risk Score (Standardized)", x = NULL
    ) +
    theme_cvis() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(paths$figures, "risk_cluster_signatures.png"), p_clusters, width = 11, height = 6)

# 6f. Indicator Contribution to Variance (PCA loadings)
# Showing how much each indicator contributes to the first two PCs
loading_contrib <- loadings %>%
    mutate(contrib = sqrt(PC1^2 + PC2^2)) %>%
    arrange(desc(contrib)) %>%
    head(15) %>%
    left_join(tbl_indicators %>% select(abbrev, name), by = c("indicator" = "abbrev"))

p_contrib <- ggplot(loading_contrib, aes(x = reorder(name, contrib), y = contrib, fill = contrib)) +
    geom_col() +
    coord_flip() +
    scale_fill_viridis_c(option = "magma", guide = "none") +
    labs(
        title = "Key Risk Driver Contribution",
        subtitle = "Top 15 indicators explaining difference between CUs (PC1 & PC2)",
        x = NULL, y = "Contribution Magnitude (Loading Vector Length)"
    ) +
    theme_cvis()

ggsave(file.path(paths$figures, "risk_driver_contribution_variance.png"), p_contrib, width = 9, height = 7)

# 7. Save outputs -------------------------------------------------------
risk_drivers_4d <- list(
    red_flag_summary  = red_flag_summary,
    variation_summary = variation_summary,
    pca_output        = pca_output,
    pca_result        = pca_res,
    km_result         = km_res
)

save(risk_drivers_4d, file = file.path(paths$output, "risk_drivers_4d.Rdata"))

cat("4d Analysis Complete. Figures saved to:", paths$figures, "\n")
cat("Results saved to: risk_drivers_4d.Rdata\n")
