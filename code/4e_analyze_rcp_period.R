################################################################################
#
# analyze_rcp_period_effects.R
#
# Analyze how RCP scenarios and time periods affect environmental change 
# indicators and overall vulnerability scores for Pacific salmon CUs
#
# Environmental change indicators that vary by RCP/period:
# - tw8proj (Projected August stream temperature)
# - tw8rate (Rate of change in August temperature)
# - lowQpdelta (Proportional decrease in August flows)
# - highQpdelta (Proportional increase in Nov-Jan flows)
# - favchange (ENM change in habitat favourability)
# - migrT (Projected temperature during migration)
# - migrQ (Proportional change in flow during migration)
# - SSTproj (Projected nearshore SST)
# - SSTrate (Rate of change in nearshore SST)
#
################################################################################

library(here)
library(tidyverse)
library(patchwork)
library(gt)

# Source required files
setwd(here())
source(file.path(here(), "code", "0_setup.R"))
source(file.path(here(), "code", "4_scoring_utils.R"))
source(file.path(here(), "code", "5b_plots_compare.R"))

# Load data (assuming these are already loaded from running 4a_CU_scoring.R)
# If not, uncomment and modify paths as needed:
# load(file.path(paths$indicators, "standardized_indicators.Rdata"))
# load(file.path(paths$indicators, "vulnerability_scores_with_species_ranks.Rdata"))


# Define environmental change indicators (base names without suffixes)
env_indicators <- c("tw8proj", "tw8rate", "lowQpdelta", "highQpdelta", 
                    "favchange", "migrT", "migrQ", "SSTproj", "SSTrate")

# Create column names for mean values
env_indicators_mean <- paste0(env_indicators, "_mean")

# Define scenarios to compare
rcps <- c("45", "85")
periods <- c("3", "5")  # Period 2: 2041-2060, Period 3: 2081-2100


# 1. Summary statistics by RCP and period ---------------------------------

cat("\n=== SUMMARY STATISTICS BY RCP AND PERIOD ===\n\n")

# Calculate mean and SD for each indicator across all CUs by scenario
summary_stats <- all_flat_std %>%
  filter(period_code %in% periods) %>%
  select(FULL_CU_IN, SPECIES_NAME, rcp, period_code, all_of(env_indicators_mean)) %>%
  pivot_longer(cols = all_of(env_indicators_mean), 
               names_to = "indicator", 
               values_to = "value") %>%
  mutate(indicator = str_remove(indicator, "_mean")) %>%
  group_by(indicator, rcp, period_code) %>%
  summarise(
    n = n(),
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    min = min(value, na.rm = TRUE),
    max = max(value, na.rm = TRUE),
    q25 = quantile(value, 0.25, na.rm = TRUE),
    median = quantile(value, 0.5, na.rm = TRUE),
    q75 = quantile(value, 0.75, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(indicator, rcp, period_code)

# Print summary table
print(summary_stats)

# Export summary statistics
write.csv(summary_stats, 
          file = file.path(paths$indicators, paste0(today, "_rcp_period_summary_stats.csv")),
          row.names = FALSE)


# 2. Calculate differences between scenarios -------------------------------

cat("\n=== CALCULATING DIFFERENCES BETWEEN SCENARIOS ===\n\n")

# RCP differences (85 - 45) for each period
rcp_differences <- all_flat_std %>%
  filter(period_code %in% periods) %>%
  select(FULL_CU_IN, SPECIES_NAME, rcp, period_code, all_of(env_indicators_mean)) %>%
  pivot_longer(cols = all_of(env_indicators_mean),
               names_to = "indicator",
               values_to = "value") %>%
  mutate(indicator = str_remove(indicator, "_mean")) %>%
  pivot_wider(names_from = rcp, values_from = value, 
              names_prefix = "rcp") %>%
  mutate(rcp_diff = rcp85 - rcp45) %>%
  select(FULL_CU_IN, SPECIES_NAME, period_code, indicator, rcp_diff)

# Period differences (Period 3 - Period 2) for each RCP
period_differences <- all_flat_std %>%
  filter(period_code %in% periods) %>%
  select(FULL_CU_IN, SPECIES_NAME, rcp, period_code, all_of(env_indicators_mean)) %>%
  pivot_longer(cols = all_of(env_indicators_mean),
               names_to = "indicator",
               values_to = "value") %>%
  mutate(indicator = str_remove(indicator, "_mean")) %>%
  pivot_wider(names_from = period_code, values_from = value,
              names_prefix = "p") %>%
  mutate(period_diff = p5 - p3) %>%
  select(FULL_CU_IN, SPECIES_NAME, rcp, indicator, period_diff)

# Summarize differences
rcp_diff_summary <- rcp_differences %>%
  group_by(indicator, period_code, SPECIES_NAME) %>%
  summarise(
    mean_diff = mean(rcp_diff, na.rm = TRUE),
    sd_diff = sd(rcp_diff, na.rm = TRUE),
    .groups = "drop"
  )

period_diff_summary <- period_differences %>%
  group_by(indicator, rcp, SPECIES_NAME) %>%
  summarise(
    mean_diff = mean(period_diff, na.rm = TRUE),
    sd_diff = sd(period_diff, na.rm = TRUE),
    .groups = "drop"
  )

print("RCP Differences (RCP85 - RCP45):")
print(rcp_diff_summary)

print("\nPeriod Differences (2081-2100 minus 2041-2060):")
print(period_diff_summary)


# 3. Analyze rank changes --------------------------------------------------

cat("\n=== ANALYZING RANK CHANGES ===\n\n")

# Calculate rank correlations between scenarios
rank_correlations <- combined_scores_std %>%
  select(FULL_CU_IN, SPECIES_NAME, rcp, period_code, 
         std_rank_sumavgs_cross, std_avgall) %>%
  arrange(FULL_CU_IN, rcp, period_code)

# Compare ranks across RCPs (within same period)
rcp_rank_comparison <- rank_correlations %>%
  select(-std_avgall) %>%
  pivot_wider(names_from = rcp, values_from = std_rank_sumavgs_cross,
              names_prefix = "rank_rcp") %>%
  mutate(rank_change_rcp = rank_rcp85 - rank_rcp45,
         abs_rank_change_rcp = abs(rank_change_rcp))

# Compare ranks across periods (within same RCP)
period_rank_comparison <- rank_correlations %>%
  select(-std_avgall) %>%
  pivot_wider(names_from = period_code, values_from = std_rank_sumavgs_cross,
              names_prefix = "rank_p") %>%
  mutate(rank_change_period = rank_p5 - rank_p3,
         abs_rank_change_period = abs(rank_change_period))

# Identify CUs with largest rank changes
cat("CUs with largest rank changes between RCP45 and RCP85:\n")
top_rcp_changes <- rcp_rank_comparison %>%
  filter(period_code == "3") %>%
  arrange(desc(abs_rank_change_rcp)) %>%
  head(10) %>%
  select(FULL_CU_IN, SPECIES_NAME, rank_rcp45, rank_rcp85, rank_change_rcp)
print(top_rcp_changes)

cat("\nCUs with largest rank changes between periods:\n")
top_period_changes <- period_rank_comparison %>%
  filter(rcp == "45") %>%
  arrange(desc(abs_rank_change_period)) %>%
  head(10) %>%
  select(FULL_CU_IN, SPECIES_NAME, rank_p3, rank_p5, rank_change_period)
print(top_period_changes)

# Calculate rank correlation coefficients
rank_cor_by_period <- rcp_rank_comparison %>%
  group_by(period_code) %>%
  summarise(
    correlation = cor(rank_rcp45, rank_rcp85, use = "complete.obs"),
    .groups = "drop"
  )

rank_cor_by_rcp <- period_rank_comparison %>%
  group_by(rcp) %>%
  summarise(
    correlation = cor(rank_p3, rank_p5, use = "complete.obs"),
    .groups = "drop"
  )

cat("\nRank correlation between RCP45 and RCP85 by period:\n")
print(rank_cor_by_period)

cat("\nRank correlation between Period 2 and Period 3 by RCP:\n")
print(rank_cor_by_rcp)


# 4. Visualization: Indicator value comparisons ----------------------------

cat("\n=== CREATING VISUALIZATIONS ===\n\n")

# Plot 1: Boxplots comparing indicator values across scenarios
p1_data <- all_flat_std %>%
  filter(period_code %in% periods) %>%
  select(SPECIES_NAME, rcp, period_code, all_of(env_indicators_mean)) %>%
  pivot_longer(cols = all_of(env_indicators_mean),
               names_to = "indicator",
               values_to = "value") %>%
  mutate(indicator = str_remove(indicator, "_mean"),
         scenario = paste0("RCP", rcp, "_P", period_code))

p1 <- ggplot(p1_data, aes(x = scenario, y = value, fill = scenario)) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(~indicator, scales = "free_y", ncol = 3) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Environmental Change Indicators by RCP and Period",
       subtitle = "Distribution of values across all CUs",
       x = "Scenario",
       y = "Indicator Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

print(p1)

ggsave(filename = file.path(paths$figures, paste0(today, "_indicator_boxplots_by_scenario.png")),
       plot = p1, width = 12, height = 10, dpi = 300)


# Plot 2: Scatter plots comparing RCP45 vs RCP85 for each period
p2_data <- all_flat_std %>%
  filter(period_code %in% periods) %>%
  select(FULL_CU_IN, SPECIES_NAME, rcp, period_code, all_of(env_indicators_mean)) %>%
  pivot_longer(cols = all_of(env_indicators_mean),
               names_to = "indicator",
               values_to = "value") %>%
  mutate(indicator = str_remove(indicator, "_mean")) %>%
  pivot_wider(names_from = rcp, values_from = value,
              names_prefix = "rcp")

p2 <- ggplot(p2_data, aes(x = rcp45, y = rcp85, color = SPECIES_NAME)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
  geom_point(alpha = 0.6) +
  facet_grid(period_code ~ indicator, scales = "free",
             labeller = labeller(period_code = c("3" = "2041-2060", "5" = "2081-2100"))) +
  scale_color_manual(values = species_palette) +
  labs(title = "Comparison of Indicator Values: RCP 4.5 vs RCP 8.5",
       subtitle = "Points above line indicate higher values under RCP 8.5",
       x = "RCP 4.5",
       y = "RCP 8.5",
       color = "Species") +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p2)

ggsave(filename = file.path(paths$figures, paste0(today, "_rcp_comparison_scatter.png")),
       plot = p2, width = 14, height = 10, dpi = 300)


# Plot 3: Rank change visualization
p3_data <- rcp_rank_comparison %>%
  filter(period_code == "5") %>%
  arrange(rank_change_rcp) %>%
  mutate(FULL_CU_IN = factor(FULL_CU_IN, levels = FULL_CU_IN))

p3 <- ggplot(p3_data, aes(x = FULL_CU_IN, y = rank_change_rcp, fill = SPECIES_NAME)) +
  geom_col() +
  scale_fill_manual(values = species_palette) +
  labs(title = "Rank Changes Between RCP 4.5 and RCP 8.5",
       subtitle = "Period 5 (2081-2100) | Positive = lower priority under RCP 8.5",
       x = "Conservation Unit",
       y = "Rank Change (RCP85 - RCP45)",
       fill = "Species") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 6),
        legend.position = "bottom")

print(p3)

ggsave(filename = file.path(paths$figures, paste0(today, "_rank_changes_rcp.png")),
       plot = p3, width = 14, height = 6, dpi = 300)


# Plot 4: Overall score comparison
p4_data <- combined_scores_std %>%
  select(FULL_CU_IN, SPECIES_NAME, rcp, period_code, std_avgall) %>%
  pivot_wider(names_from = c(rcp, period_code), 
              values_from = std_avgall,
              names_sep = "_p")

p4 <- ggplot(p4_data, aes(x = `45_p3`, y = `85_p5`, color = SPECIES_NAME)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
  geom_point(size = 3, alpha = 0.7) +
  scale_color_manual(values = species_palette) +
  labs(title = "Overall Vulnerability Score Comparison",
       subtitle = "Best case (RCP 4.5, 2041-2060) vs Worst case (RCP 8.5, 2081-2100)",
       x = "RCP 4.5, 2041-2060",
       y = "RCP 8.5, 2081-2100",
       color = "Species") +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p4)

ggsave(filename = file.path(paths$figures, paste0(today, "_overall_score_comparison.png")),
       plot = p4, width = 8, height = 8, dpi = 300)


# 5. Statistical tests -----------------------------------------------------

cat("\n=== STATISTICAL TESTS ===\n\n")

# Paired t-tests for each indicator comparing RCP45 vs RCP85
rcp_tests <- lapply(env_indicators, function(ind) {
  ind_mean <- paste0(ind, "_mean")
  
  test_data <- all_flat_std %>%
    filter(period_code == "3") %>%
    select(FULL_CU_IN, rcp, all_of(ind_mean)) %>%
    pivot_wider(names_from = rcp, values_from = all_of(ind_mean)) %>%
    filter(!is.na(`45`) & !is.na(`85`))
  
  if(nrow(test_data) > 0) {
    test_result <- t.test(test_data$`85`, test_data$`45`, paired = TRUE)
    return(data.frame(
      indicator = ind,
      mean_diff = test_result$estimate,
      t_statistic = test_result$statistic,
      p_value = test_result$p.value,
      ci_lower = test_result$conf.int[1],
      ci_upper = test_result$conf.int[2]
    ))
  }
})

rcp_test_results <- bind_rows(rcp_tests)
print("Paired t-tests: RCP 8.5 vs RCP 4.5 (Period 3)")
print(rcp_test_results)

# Paired t-tests comparing periods
period_tests <- lapply(env_indicators, function(ind) {
  ind_mean <- paste0(ind, "_mean")
  
  test_data <- all_flat_std %>%
    filter(rcp == "45") %>%
    select(FULL_CU_IN, period_code, all_of(ind_mean)) %>%
    pivot_wider(names_from = period_code, values_from = all_of(ind_mean)) %>%
    filter(!is.na(`2`) & !is.na(`3`))
  
  if(nrow(test_data) > 0) {
    test_result <- t.test(test_data$`3`, test_data$`2`, paired = TRUE)
    return(data.frame(
      indicator = ind,
      mean_diff = test_result$estimate,
      t_statistic = test_result$statistic,
      p_value = test_result$p.value,
      ci_lower = test_result$conf.int[1],
      ci_upper = test_result$conf.int[2]
    ))
  }
})

period_test_results <- bind_rows(period_tests)
print("\nPaired t-tests: Period 3 vs Period 2 (RCP 4.5)")
print(period_test_results)


# 6. Species-specific analysis ---------------------------------------------

cat("\n=== SPECIES-SPECIFIC ANALYSIS ===\n\n")

# Calculate mean changes by species
species_rcp_effects <- all_flat_std %>%
  filter(period_code == "3") %>%
  select(SPECIES_NAME, rcp, all_of(env_indicators_mean)) %>%
  pivot_longer(cols = all_of(env_indicators_mean),
               names_to = "indicator",
               values_to = "value") %>%
  mutate(indicator = str_remove(indicator, "_mean")) %>%
  group_by(SPECIES_NAME, indicator, rcp) %>%
  summarise(mean_value = mean(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = rcp, values_from = mean_value) %>%
  mutate(rcp_effect = `85` - `45`)

print("Mean RCP effect by species (Period 3):")
print(species_rcp_effects)

# Visualization: Species-specific RCP effects
p5 <- ggplot(species_rcp_effects, aes(x = indicator, y = rcp_effect, fill = SPECIES_NAME)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = species_palette) +
  labs(title = "Mean RCP Effect by Species",
       subtitle = "Difference between RCP 8.5 and RCP 4.5 (Period 3: 2081-2100)",
       x = "Indicator",
       y = "Mean Difference (RCP 8.5 - RCP 4.5)",
       fill = "Species") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

print(p5)

ggsave(filename = file.path(paths$figures, paste0(today, "_species_rcp_effects.png")),
       plot = p5, width = 10, height = 6, dpi = 300)


# 7. Export results --------------------------------------------------------

cat("\n=== EXPORTING RESULTS ===\n\n")

# Create comprehensive summary report
report_list <- list(
  summary_stats = summary_stats,
  rcp_differences = rcp_diff_summary,
  period_differences = period_diff_summary,
  rcp_test_results = rcp_test_results,
  period_test_results = period_test_results,
  rank_correlations = bind_rows(
    rank_cor_by_period %>% mutate(comparison = "RCP"),
    rank_cor_by_rcp %>% mutate(comparison = "Period")
  ),
  top_rcp_rank_changes = top_rcp_changes,
  top_period_rank_changes = top_period_changes,
  species_effects = species_rcp_effects
)

# Save as RData
save(report_list, 
     file = file.path(paths$indicators, paste0(today, "_rcp_period_analysis.RData")))

# Export individual tables
write.csv(rcp_diff_summary,
          file = file.path(paths$indicators, paste0(today, "_rcp_differences.csv")),
          row.names = FALSE)

write.csv(period_diff_summary,
          file = file.path(paths$indicators, paste0(today, "_period_differences.csv")),
          row.names = FALSE)

write.csv(rcp_test_results,
          file = file.path(paths$indicators, paste0(today, "_rcp_statistical_tests.csv")),
          row.names = FALSE)

write.csv(period_test_results,
          file = file.path(paths$indicators, paste0(today, "_period_statistical_tests.csv")),
          row.names = FALSE)

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Results saved to:", paths$indicators, "\n")
cat("Figures saved to:", paths$figures, "\n")