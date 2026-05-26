# ==============================================================================
# CVIS Monte Carlo Uncertainty Analysis (4d_CU_uncertainty_analysis.R)
#
# Description:
#   Conducts a Monte Carlo uncertainty analysis by simultaneously varying multiple
#   sources of uncertainty (GCMs, RCP emissions scenario, and downscaling methods)
#   through random sampling. Focuses on Period 3 (2041-2060), keeps scoring
#   method fixed to the default (overall: catavg, category: avg), and runs
#   100 iterations to quantify combined uncertainty, rank stability, and identify
#   CUs with robust vulnerability profiles.
#
# Workflow Steps:
#   1. Load setup environment and core scoring results.
#   2. Define uncertainty sampling space (GCMs, RCPs, Downscalers).
#   3. Perform Monte Carlo loop (100 iterations):
#      - Randomly sample GCM, RCP, and Downscaling assumptions.
#      - Select correct standardized values using a key-based join.
#      - Calculate overall vulnerability scores (catavg) and ranks.
#   4. Compute summary statistics (Mean, SD, 90% Uncertainty Intervals).
#   5. Categorize CUs into Robustness Profiles (Robust High, Robust Low, Uncertain).
#   6. Run multi-way ANOVA to decompose uncertainty variance.
#   7. Save outputs (Rdata, CSVs, Figures).
#
# Inputs:
#   - output/scoring_results.Rdata
#   - Configuration tables (via 0_setup.R)
#
# Outputs:
#   - output/uncertainty_analysis_results.Rdata
#   - output/CU_robustness_summary.csv
#   - output/uncertainty_variance_decomposition.csv
#   - output/figures/uncertainty_analysis/*.png
#
# Dependencies:
#   - Requires 4a_CU_scoring.R to have been executed.
# ==============================================================================

# ==================== 1. Setup and Environment ====================
library(here)
setwd(here())
source(file.path(here(), "code", "0_setup.R"))

# Load outputs from 4a
load(file.path(paths$output, "scoring_results.Rdata")) # loads all_std_long, scores_tidy

# Create outputs directories
uncertainty_fig_path <- file.path(paths$figures, "uncertainty_analysis")
dir.create(uncertainty_fig_path, showWarnings = FALSE, recursive = TRUE)

cat("Starting Monte Carlo Uncertainty Analysis...\n")

# Clean and format variables in the input dataset to character to prevent join mismatches
all_std_clean <- all_std_long %>%
  mutate(
    indicator = as.character(indicator),
    dsmodel = as.character(dsmodel),
    gcm = as.character(gcm),
    rcp = as.character(rcp),
    period_code = as.character(period_code)
  )

# ==================== 2. Define Sampling Space ====================
N_iterations <- 100
target_period <- "3" # Only period 3 (2041-2060)

# Uncertainty Sources:
# 1. Climate Models (GCMs)
gcm_choices <- common_gcms # c("1", "4", "6") (CanESM2, HadGEM2, MPI)
# 2. Emission Scenarios (RCPs)
rcp_choices <- c("45", "85")
# 3. Spawning/Rearing Temp Downscaling
ds_temp_choices <- c("pcicgrid", "tscapes")
# 4. Spawning/Rearing Flow Downscaling
ds_flow_choices <- c("station", "streamdyn")
# 5. Nearshore Marine SST Downscaling
ds_mar_choices <- c("qdm", "bccmssc")
# 6. Standardization Method (exponential/decay vs linear)
std_method_choices <- c("exponential", "linear")

# Set seed for reproducibility
set.seed(42)

# List to hold Monte Carlo iteration outputs
mc_results_list <- vector("list", N_iterations)

# ==================== 3. Monte Carlo Loop ====================
cat("Running", N_iterations, "iterations for Period 3...\n")

for (k in 1:N_iterations) {
  # A. Randomly sample the uncertainty sources
  g_k <- sample(gcm_choices, 1)
  rcp_k <- sample(rcp_choices, 1)
  d_temp <- sample(ds_temp_choices, 1)
  d_flow <- sample(ds_flow_choices, 1)
  d_mar <- sample(ds_mar_choices, 1)
  std_method_k <- sample(std_method_choices, 1)
  
  # B. Build selection key table for this iteration
  # Note: Flow 'station' downscaler only has ensemble mean '9', so fall back to '9' if 'station' is chosen.
  g_flow <- if (d_flow == "station") "9" else g_k
  
  iter_keys <- tibble::tribble(
    ~indicator, ~dsmodel, ~gcm, ~rcp, ~period_code,
    "tw8rate",      d_temp,   g_k,  rcp_k, target_period,
    "tw8proj",      d_temp,   g_k,  rcp_k, target_period,
    "flow8pdelta",  d_flow,   g_flow, rcp_k, target_period,
    "flow18pdelta", "streamdyn", g_k, rcp_k, target_period,
    "migrTproj",    "pcicgrid",  g_k, rcp_k, target_period,
    "migrQpdelta",  "pcicgrid",  g_k, rcp_k, target_period,
    "SSTproj",      d_mar,    "9",  rcp_k, target_period,
    "SSTrate",      d_mar,    "9",  rcp_k, target_period,
    "favchange",    "ENM",    "9",  rcp_k, target_period,
    "genoff",       "observed", "9",  rcp_k, target_period,
    # Static baseline indicators (GCM 0, RCP 0, Period 0)
    "cthr",         "cthr_anad", "0", "0", "0",
    "fwres",        "observed", "0", "0", "0",
    "migrdist",     "observed", "0", "0", "0",
    "CImpact",      "CImpact",  "0", "0", "0",
    "CUstatus",     "observed", "0", "0", "0",
    "CUnmat",       "observed", "0", "0", "0",
    "hetzyg",       "observed", "0", "0", "0"
  ) %>%
    mutate(std_method = std_method_k)
  
  # C. Extract and score
  res <- all_std_clean %>%
    inner_join(iter_keys, by = c("indicator", "dsmodel", "gcm", "rcp", "period_code", "std_method")) %>%
    filter(stat == "mean")
  
  # Calculate portfolio scores using fixed catavg (overall) and avg (category)
  scores_iter <- res %>%
    group_by(FULL_CU_IN, SPECIES_NAME, CVIS_NAME, CU_COMMON_NAME, SMU_SIMPLE, std_method) %>%
    calculate_combined_scores() %>%
    filter(method == "catavg")
  
  # Scale to 0-100 and compute ranks across CUs
  scores_scaled <- scores_iter %>%
    group_by(category) %>%
    mutate(
      score100 = scale_0_100(score),
      rank_val = rank(-score, ties.method = "average", na.last = "keep")
    ) %>%
    ungroup()
  
  # Append metadata of assumptions
  scores_scaled <- scores_scaled %>%
    mutate(
      iteration = k,
      sampled_gcm = g_k,
      sampled_rcp = rcp_k,
      sampled_ds_temp = d_temp,
      sampled_ds_flow = d_flow,
      sampled_ds_mar = d_mar,
      sampled_std_method = std_method_k
    )
  
  mc_results_list[[k]] <- scores_scaled
}

mc_results <- bind_rows(mc_results_list)
cat("Monte Carlo simulation completed.\n")

# ==================== 4. Statistical Summary & Robustness ====================
cat("Summarizing uncertainty results...\n")

# Filter for overall vulnerability (category == "all")
all_scores <- mc_results %>% filter(category == "all")

# Compute summary statistics of score and rank distributions for each CU
cu_summary <- all_scores %>%
  group_by(FULL_CU_IN, SPECIES_NAME, CVIS_NAME, CU_COMMON_NAME, SMU_SIMPLE) %>%
  summarise(
    mean_score = mean(score100, na.rm = TRUE),
    sd_score = sd(score100, na.rm = TRUE),
    q5_score = quantile(score100, 0.05, na.rm = TRUE),
    q95_score = quantile(score100, 0.95, na.rm = TRUE),
    range_score = q95_score - q5_score,
    
    mean_rank = mean(rank_val, na.rm = TRUE),
    q5_rank = quantile(rank_val, 0.05, na.rm = TRUE),
    q95_rank = quantile(rank_val, 0.95, na.rm = TRUE),
    range_rank = q95_rank - q5_rank,
    .groups = "drop"
  )

# Classify CUs into Robustness Profiles:
#   - Robust High: Vulnerability score is consistently high (q5_score >= 50)
#   - Robust Low: Vulnerability score is consistently low (q95_score <= 35)
#   - Highly Uncertain: Large spread in score (range_score >= 25) or rank (range_rank >= 15)
#   - Moderate / Intermediate: All others
cu_summary <- cu_summary %>%
  mutate(
    robustness_profile = case_when(
      q5_score >= 50 ~ "Robust High",
      q95_score <= 35 ~ "Robust Low",
      range_score >= 25 | range_rank >= 15 ~ "Highly Uncertain",
      TRUE ~ "Intermediate / Moderate"
    )
  )

print(table(cu_summary$robustness_profile))

# ==================== 5. Variance Decomposition (ANOVA) ====================
cat("Performing variance decomposition using ANOVA...\n")

# Run multi-way ANOVA to partition uncertainty variance
# We control for the true spatial variation among CUs by including FULL_CU_IN
lm_fit <- lm(score100 ~ FULL_CU_IN + sampled_gcm + sampled_rcp + sampled_ds_temp + sampled_ds_flow + sampled_ds_mar + sampled_std_method, data = all_scores)
anova_res <- anova(lm_fit)
anova_df <- as.data.frame(anova_res) %>%
  rownames_to_column("Source")

# Define target uncertainty factors
unc_factors <- c("sampled_gcm", "sampled_rcp", "sampled_ds_temp", "sampled_ds_flow", "sampled_ds_mar", "sampled_std_method")

# Subset ANOVA table to target uncertainty factors and calculate relative contribution
anova_unc <- anova_df %>%
  filter(Source %in% unc_factors) %>%
  mutate(
    pct_uncertainty_variance = `Sum Sq` / sum(`Sum Sq`) * 100
  ) %>%
  mutate(
    Source_Label = case_when(
      Source == "sampled_gcm" ~ "Climate Model (GCM)",
      Source == "sampled_rcp" ~ "Emission Scenario (RCP)",
      Source == "sampled_ds_temp" ~ "Temperature Downscaling",
      Source == "sampled_ds_flow" ~ "Flow Downscaling",
      Source == "sampled_ds_mar" ~ "Marine Downscaling",
      Source == "sampled_std_method" ~ "Standardization Method",
      TRUE ~ Source
    )
  ) %>%
  arrange(desc(pct_uncertainty_variance))

print(as.data.frame(anova_unc[, c("Source_Label", "pct_uncertainty_variance")]))

# ==================== 6. Save Datasets ====================
cat("Saving data outputs...\n")

# Save R object for downstream analysis
save(mc_results, cu_summary, anova_unc, file = file.path(paths$output, "uncertainty_analysis_results.Rdata"))

# Save summary tables to CSV
write_csv(cu_summary, file.path(paths$output, "CU_robustness_summary.csv"))
write_csv(anova_unc %>% select(Source, Source_Label, df = Df, sum_sq = `Sum Sq`, mean_sq = `Mean Sq`, F_value = `F value`, p_value = `Pr(>F)`, pct_uncertainty_variance),
          file.path(paths$output, "uncertainty_variance_decomposition.csv"))

# ==================== 7. Visualizations ====================
cat("Generating professional figures...\n")

# 7a. Figure 1: Combined Uncertainty Boxplots
# Highlight robustness profile with color outline or facet
p_spread <- ggplot(all_scores, aes(x = reorder(CVIS_NAME, score100, FUN = mean), y = score100)) +
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

ggsave(file.path(uncertainty_fig_path, "combined_uncertainty_spread.png"), p_spread, width = 12, height = 9, dpi = 300)

# 7b. Figure 2: Rank Uncertainty Intervals
rank_summary <- cu_summary

p_rank <- ggplot(rank_summary, aes(x = reorder(CVIS_NAME, -mean_rank), y = mean_rank)) +
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

ggsave(file.path(uncertainty_fig_path, "rank_uncertainty.png"), p_rank, width = 12, height = 9, dpi = 300)

# 7c. Figure 3: Uncertainty Variance Decomposition
p_var <- ggplot(anova_unc, aes(x = reorder(Source_Label, pct_uncertainty_variance), y = pct_uncertainty_variance, fill = Source_Label)) +
  geom_col(show.legend = FALSE, alpha = 0.85, width = 0.6) +
  coord_flip() +
  scale_fill_brewer(palette = "Set1") +
  labs(
    title = "Decomposition of CVIS Combined Uncertainty",
    subtitle = "Relative contribution (% variance explained) of GCMs, RCPs, and downscalers to score variance\n(Controlled for geographical variation between CUs)",
    x = NULL,
    y = "% Uncertainty Variance Explained"
  ) +
  theme_cvis() +
  theme(
    plot.title = element_text(face = "bold", size = 14)
  )

ggsave(file.path(uncertainty_fig_path, "variance_decomposition.png"), p_var, width = 10, height = 6, dpi = 300)

cat("Script 4d complete. Outputs and figures successfully saved!\n")
