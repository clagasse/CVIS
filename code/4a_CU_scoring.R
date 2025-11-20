################################################################################
#
# 4a_CU_scoring.R
#
# 1 - get indicator values for each CU
# 2 - apply lower thresholds and standardization function
# 3 - calculate standardized indicator values
# 4 - summarize and compare indicator values across CUs
# 5 - estimate vulnerability indices in different categories
#
###############################################################################
#----------------1. Setup and import----------------
library(here)
setwd(here())
source(file.path(here(), "code", "0_setup.R"))

# combine indicators into common table
all_flat <- cu_run %>%
  left_join(fwR_all_flat, join_by(FULL_CU_IN, CU_NAME, SPECIES_NAME), relationship = "one-to-many") %>%
  left_join(migr_all_flat, join_by(FULL_CU_IN, CU_NAME, SPECIES_NAME, rcp, period, sp_peak)) %>%
  left_join(mar_all_flat, join_by(FULL_CU_IN, CVIS_NAME, SPECIES_NAME, rcp, period_code)) %>%
  relocate(rcp, period, period_code, .after = SPECIES_NAME) %>%
  filter(period_code %in% periods_use)



#------------- 2. Calculate standardized scores-------------------------------

all_std <- select(all_flat, "FULL_CU_IN", "rcp", "period_code")

for (i in 1:nrow(tbl_indicators)) {

  std_params_i <- as.list(tbl_standardize[i, ])

  temp <- standardize_indicator(all_flat,
    indicator_pick = tbl_indicators$abbrev[i],
    std_fun = tbl_indicators$std_fun[i],
    std_params = std_params_i,
    use_gcm_range = F
  )

  all_std <- all_std %>%
    left_join(temp, join_by("FULL_CU_IN", "rcp", "period_code"))
}

## merge standardized and original indicator values for plotting and comparisons

## add std to col names for standardized values
all_flat_std <- all_std %>%
  rename_with(
    .fn = ~ paste0("std_", .x),
    .cols = matches(tbl_indicators$abbrev)
  )

all_flat_std <- all_flat %>%
  left_join(all_flat_std, join_by(FULL_CU_IN, rcp, period_code))


# 3. Species and all CU averages ------------------------------------------

# calculate species and all CU average for each indicator (for plotting) and put into unique columns
all_std_sp_avgs <- all_flat_std %>%
  group_by(SPECIES_NAME, rcp, period_code) %>%
  dplyr::summarize(across(starts_with("std_"), \(x) mean(x, na.rm = TRUE)), .groups = "drop") %>%
  mutate(FULL_CU_IN = "Species average", CU_NAME = "Species average") %>%
  # rename columns to indicate species average
  rename_with(.fn = ~ paste0("spavg_", .x), .cols = starts_with("std_"))  %>%
  # bind new columns back to CU values
  left_join(all_flat_std, join_by(SPECIES_NAME, rcp, period_code))


# calculate overall average for each indicator (for plotting)
all_std_avgs <- all_flat_std %>%
  group_by(rcp, period_code) %>%
  dplyr::summarize(across(starts_with("std_"), \(x) mean(x, na.rm = TRUE)), .groups = "drop") %>%
  mutate(FULL_CU_IN = "All CUs", CU_NAME = "All CUs", CU_Species = "All CUs")



# 4. Overall scores using additive and multiplicative methods -------------

combined_scores_std <- all_flat_std %>%
  subset_ind_table(indicators_choose = tbl_indicators$abbrev,
    get_std = T,
    get_raw = F,
    id_col = c("FULL_CU_IN", "CVIS_NAME"),
    rename_cols = F) %>%
  sum_selected_columns(match_strings = tbl_indicators$abbrev,
    new_col_name = "std_addall") %>%
  sum_selected_columns(match_strings = c("tw8rate", "tw8proj", "lowQpdelta", "highQpdelta", "ct"),
    new_col_name = "std_addfwR") %>%
  sum_selected_columns(match_strings = tbl_indicators$abbrev[tbl_indicators$type == "migr"],
    new_col_name = "std_addmigr") %>%
  sum_selected_columns(match_strings = tbl_indicators$abbrev[tbl_indicators$type == "dem"],
    new_col_name = "std_adddem") %>%
  sum_selected_columns(match_strings = tbl_indicators$abbrev[tbl_indicators$type == "mar"],
    new_col_name = "std_addmar") %>%
  multiply_selected_columns(match_strings = c("tw8rate", "tw8proj", "lowQpdelta", "highQpdelta", "ct"),
    new_col_name = "std_prodfwR") %>%
  multiply_selected_columns(match_strings = tbl_indicators$abbrev[tbl_indicators$type == "migr"],
    new_col_name = "std_prodmigr") %>%
  multiply_selected_columns(match_strings = tbl_indicators$abbrev[tbl_indicators$type == "dem"],
    new_col_name = "std_proddem") %>%
  multiply_selected_columns(match_strings = tbl_indicators$abbrev[tbl_indicators$type == "mar"],
    new_col_name = "std_prodmar") %>%
  average_selected_columns(match_strings = tbl_indicators$abbrev,
    new_col_name = "std_avgall") %>%
  average_selected_columns(match_strings = c("tw8rate", "tw8proj", "lowQpdelta", "highQpdelta", "ct"),
    new_col_name = "std_avgfwR") %>%
  average_selected_columns(match_strings = tbl_indicators$abbrev[tbl_indicators$type == "migr"],
    new_col_name = "std_avgmigr") %>%
  average_selected_columns(match_strings = tbl_indicators$abbrev[tbl_indicators$type == "dem"],
    new_col_name = "std_avgdem") %>%
  average_selected_columns(match_strings = tbl_indicators$abbrev[tbl_indicators$type == "mar"],
    new_col_name = "std_avgmar") %>%
  sum_selected_columns(match_strings = c("std_avgfwR", "std_avgmigr", "std_avgdem", "std_avgmar"),
    new_col_name = "std_sumavgs")

# 5. Calculate CROSS-SPECIES ranks (all CUs ranked together) --------------

cat("\nCalculating cross-species ranks (all CUs ranked together)...\n")

combined_scores_std <- combined_scores_std %>%
  # Cross-species ranks for overall scores
  rank_scores(
    score_col = "std_addall",
    group_col = c("rcp", "period_code"),
    rank_col = "std_rank_addall_cross",
    descending = FALSE
  ) %>%
  rank_scores(
    score_col = "std_sumavgs",
    group_col = c("rcp", "period_code"),
    rank_col = "std_rank_sumavgs_cross",
    descending = FALSE
  ) %>%
  rank_scores(
    score_col = "std_avgall",
    group_col = c("rcp", "period_code"),
    rank_col = "std_rank_avgall_cross",
    descending = FALSE
  ) %>%
  # Cross-species ranks for category-specific scores
  rank_scores(
    score_col = "std_addfwR",
    group_col = c("rcp", "period_code"),
    rank_col = "std_rank_addfwR_cross",
    descending = FALSE
  ) %>%
  rank_scores(
    score_col = "std_addmigr",
    group_col = c("rcp", "period_code"),
    rank_col = "std_rank_addmigr_cross",
    descending = FALSE
  ) %>%
  rank_scores(
    score_col = "std_adddem",
    group_col = c("rcp", "period_code"),
    rank_col = "std_rank_adddem_cross",
    descending = FALSE
  ) %>%
  rank_scores(
    score_col = "std_addmar",
    group_col = c("rcp", "period_code"),
    rank_col = "std_rank_addmar_cross",
    descending = FALSE
  )


# 6. Calculate WITHIN-SPECIES ranks (CUs ranked within species) -----------

cat("Calculating within-species ranks (CUs ranked within their species)...\n")

combined_scores_std <- combined_scores_std %>%
  # Within-species ranks for overall scores
  rank_scores(
    score_col = "std_addall",
    group_col = c("SPECIES_NAME", "rcp", "period_code"),
    rank_col = "std_rank_addall_within",
    descending = FALSE
  ) %>%
  rank_scores(
    score_col = "std_sumavgs",
    group_col = c("SPECIES_NAME", "rcp", "period_code"),
    rank_col = "std_rank_sumavgs_within",
    descending = FALSE
  ) %>%
  rank_scores(
    score_col = "std_avgall",
    group_col = c("SPECIES_NAME", "rcp", "period_code"),
    rank_col = "std_rank_avgall_within",
    descending = FALSE
  ) %>%
  # Within-species ranks for category-specific scores
  rank_scores(
    score_col = "std_addfwR",
    group_col = c("SPECIES_NAME", "rcp", "period_code"),
    rank_col = "std_rank_addfwR_within",
    descending = FALSE
  ) %>%
  rank_scores(
    score_col = "std_addmigr",
    group_col = c("SPECIES_NAME", "rcp", "period_code"),
    rank_col = "std_rank_addmigr_within",
    descending = FALSE
  ) %>%
  rank_scores(
    score_col = "std_adddem",
    group_col = c("SPECIES_NAME", "rcp", "period_code"),
    rank_col = "std_rank_adddem_within",
    descending = FALSE
  ) %>%
  rank_scores(
    score_col = "std_addmar",
    group_col = c("SPECIES_NAME", "rcp", "period_code"),
    rank_col = "std_rank_addmar_within",
    descending = FALSE
  )


# 7. Calculate species-level percentiles for each CU -----------------------

cat("Calculating percentile ranks within species...\n")

# For each CU, calculate what percentile they fall in within their species
combined_scores_std <- combined_scores_std %>%
  group_by(SPECIES_NAME, rcp, period_code) %>%
  mutate(
    # Overall percentiles
    pct_addall_within = percent_rank(std_addall) * 100,
    pct_avgall_within = percent_rank(std_avgall) * 100,
    pct_sumavgs_within = percent_rank(std_sumavgs) * 100,
    # Category percentiles
    pct_addfwR_within = percent_rank(std_addfwR) * 100,
    pct_addmigr_within = percent_rank(std_addmigr) * 100,
    pct_adddem_within = percent_rank(std_adddem) * 100,
    pct_addmar_within = percent_rank(std_addmar) * 100
  ) %>%
  ungroup()


# 8. Calculate rank differences (cross-species vs within-species) ---------

cat("Calculating rank differences between cross-species and within-species...\n")

combined_scores_std <- combined_scores_std %>%
  mutate(
    # Difference in ranks (positive = ranked higher cross-species than within-species)
    rank_diff_addall = std_rank_addall_cross - std_rank_addall_within,
    rank_diff_avgall = std_rank_avgall_cross - std_rank_avgall_within,
    rank_diff_sumavgs = std_rank_sumavgs_cross - std_rank_sumavgs_within,
    rank_diff_addfwR = std_rank_addfwR_cross - std_rank_addfwR_within,
    rank_diff_addmigr = std_rank_addmigr_cross - std_rank_addmigr_within,
    rank_diff_adddem = std_rank_adddem_cross - std_rank_adddem_within,
    rank_diff_addmar = std_rank_addmar_cross - std_rank_addmar_within
  )


# 9. Summary statistics ----------------------------------------------------

cat("\n========================================\n")
cat("Ranking Summary Statistics\n")
cat("========================================\n\n")

# Filter to one scenario for summary
summary_data <- combined_scores_std %>%
  filter(rcp == "45", period_code == 3)

# Overall summary
cat("Overall vulnerability (addall method):\n")
cat("  Cross-species rank range: 1 to", max(summary_data$std_rank_addall_cross, na.rm = TRUE), "\n")

# Species-specific summaries
species_summary <- summary_data %>%
  group_by(SPECIES_NAME) %>%
  dplyr::summarize(
    n_CUs = n(),
    mean_score = mean(std_addall, na.rm = TRUE),
    mean_rank_cross = mean(std_rank_addall_cross, na.rm = TRUE),
    mean_rank_within = mean(std_rank_addall_within, na.rm = TRUE),
    max_rank_diff = max(abs(rank_diff_addall), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_score))

cat("\nBy species:\n")
print(species_summary)

# Identify CUs with largest rank differences
cat("\nCUs with largest rank differences (cross-species vs within-species):\n")
large_diffs <- summary_data %>%
  select(FULL_CU_IN, CVIS_NAME, SPECIES_NAME,
    std_rank_addall_cross, std_rank_addall_within, rank_diff_addall) %>%
  arrange(desc(abs(rank_diff_addall))) %>%
  head(10)
print(large_diffs)


# 10. Export results -------------------------------------------------------

cat("\nExporting results...\n")

# Export full results
write.csv(
  combined_scores_std,
  file = file.path(paths$indicators, paste0(today, "_vulnerability_scores_with_species_ranks.csv")),
  row.names = FALSE
)

# Export standardized indicators
write.csv(
  all_flat_std,
  file = file.path(paths$indicators, paste0(today, "_standardized_indicators.csv")),
  row.names = FALSE
)

# Export summary by species
write.csv(
  species_summary,
  file = file.path(paths$indicators, paste0(today, "_species_ranking_summary.csv")),
  row.names = FALSE
)

cat("\nScoring complete! Files saved to:", paths$indicators, "\n\n")

# Return the results for further analysis
# combined_scores_std
