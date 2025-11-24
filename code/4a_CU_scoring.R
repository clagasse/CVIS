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

all_std <- select(all_flat, all_of(c("FULL_CU_IN", "SPECIES_NAME", "rcp", "period_code")))

for (i in 1:nrow(tbl_indicators)) {

  std_params_i <- as.list(tbl_standardize[i, ])

  temp <- standardize_indicator(all_flat,
    indicator_pick = tbl_indicators$abbrev[i],
    std_fun = tbl_indicators$std_fun[i],
    std_params = std_params_i,
    use_gcm_range = F
  )

  all_std <- all_std %>%
    left_join(temp, join_by(FULL_CU_IN, SPECIES_NAME, rcp, period_code))
}

## merge standardized and original indicator values for plotting and comparisons

## add std to col names for standardized values
all_flat_std <- all_std %>%
  rename_with(
    .fn = ~ paste0("std_", .x),
    .cols = matches(tbl_indicators$abbrev)
  )

all_flat_std <- all_flat %>%
  left_join(all_flat_std, join_by(FULL_CU_IN, SPECIES_NAME, rcp, period_code))


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
  # category scores
  # power mean scores (default = cubed)
  power_selected_columns(match_strings = tbl_indicators$abbrev[tbl_indicators$type == "fwR"],
    new_col_name = "std_cubefwR") %>%
  power_selected_columns(match_strings = tbl_indicators$abbrev[tbl_indicators$type == "migr"],
    new_col_name = "std_cubemigr") %>%
  power_selected_columns(match_strings = tbl_indicators$abbrev[tbl_indicators$type == "dem"],
    new_col_name = "std_cubedem") %>%
  power_selected_columns(match_strings = tbl_indicators$abbrev[tbl_indicators$type == "gen"],
    new_col_name = "std_cubegen") %>%
  power_selected_columns(match_strings = tbl_indicators$abbrev[tbl_indicators$type == "mar"],
    new_col_name = "std_cubemar") %>%
  # average scores
  average_selected_columns(match_strings = tbl_indicators$abbrev[tbl_indicators$type == "fwR"],
    new_col_name = "std_avgfwR") %>%
  average_selected_columns(match_strings = tbl_indicators$abbrev[tbl_indicators$type == "migr"],
    new_col_name = "std_avgmigr") %>%
  average_selected_columns(match_strings = tbl_indicators$abbrev[tbl_indicators$type == "dem"],
    new_col_name = "std_avgdem") %>%
  average_selected_columns(match_strings = tbl_indicators$abbrev[tbl_indicators$type == "mar"],
    new_col_name = "std_avgmar") %>%
  average_selected_columns(match_strings = tbl_indicators$abbrev[tbl_indicators$type == "gen"],
    new_col_name = "std_avggen") %>%
  # total scores
  # sum of averages for each category
  sum_selected_columns(match_strings = c("std_avgfwR", "std_avgmigr", "std_avgdem", "std_avgmar", "std_avggen"),
    new_col_name = "std_sumavgs") %>%
  # add all indicators individually
  average_selected_columns(match_strings = tbl_indicators$abbrev,
    new_col_name = "std_avgall") %>%
  # sum of quadratic (cubed) values
  sum_selected_columns(match_strings = c("std_cubefwR", "std_cubemigr", "std_cubedem", "std_cubemar", "std_cubegen"),
    new_col_name = "std_sumcube")



# 5. Calculate CROSS-SPECIES ranks (all CUs ranked together) --------------

cat("\nCalculating cross-species ranks (all CUs ranked together)...\n")

combined_scores_std <- combined_scores_std %>%
  # Cross-species ranks for overall scores
  rank_scores(
    score_col = "std_avgall",
    group_col = c("rcp", "period_code"),
    rank_col = "std_rank_avgall_cross",
    descending = FALSE
  ) %>%
  rank_scores(
    score_col = "std_sumavgs",
    group_col = c("rcp", "period_code"),
    rank_col = "std_rank_sumavgs_cross",
    descending = FALSE
  ) %>%
  rank_scores(
    score_col = "std_sumcube",
    group_col = c("rcp", "period_code"),
    rank_col = "std_rank_sumcube_cross",
    descending = FALSE
  ) %>%
  # Cross-species ranks for category-specific scores
  rank_scores(
    score_col = "std_avgfwR",
    group_col = c("rcp", "period_code"),
    rank_col = "std_rank_avgfwR_cross",
    descending = FALSE
  ) %>%
  rank_scores(
    score_col = "std_avgmigr",
    group_col = c("rcp", "period_code"),
    rank_col = "std_rank_avgmigr_cross",
    descending = FALSE
  ) %>%
  rank_scores(
    score_col = "std_avgdem",
    group_col = c("rcp", "period_code"),
    rank_col = "std_rank_avgdem_cross",
    descending = FALSE
  ) %>%
  rank_scores(
    score_col = "std_avgmar",
    group_col = c("rcp", "period_code"),
    rank_col = "std_rank_avgmar_cross",
    descending = FALSE
  ) %>%
  rank_scores(
    score_col = "std_avggen",
    group_col = c("rcp", "period_code"),
    rank_col = "std_rank_avggen_cross",
    descending = FALSE
  )


# 6. Calculate WITHIN-SPECIES ranks (CUs ranked within species) -----------

cat("Calculating within-species ranks (CUs ranked within their species)...\n")

combined_scores_std <- combined_scores_std %>%
  # Within-species ranks for overall scores
  rank_scores(
    score_col = "std_avgall",
    group_col = c("SPECIES_NAME", "rcp", "period_code"),
    rank_col = "std_rank_avgall_within",
    descending = FALSE
  ) %>%
  rank_scores(
    score_col = "std_sumavgs",
    group_col = c("SPECIES_NAME", "rcp", "period_code"),
    rank_col = "std_rank_sumavgs_within",
    descending = FALSE
  ) %>%
  rank_scores(
    score_col = "std_sumcube",
    group_col = c("SPECIES_NAME", "rcp", "period_code"),
    rank_col = "std_rank_sumcube_within",
    descending = FALSE
  ) %>%
  # Within-species ranks for category-specific scores
  rank_scores(
    score_col = "std_avgfwR",
    group_col = c("SPECIES_NAME", "rcp", "period_code"),
    rank_col = "std_rank_avgfwR_within",
    descending = FALSE
  ) %>%
  rank_scores(
    score_col = "std_avgmigr",
    group_col = c("SPECIES_NAME", "rcp", "period_code"),
    rank_col = "std_rank_avgmigr_within",
    descending = FALSE
  ) %>%
  rank_scores(
    score_col = "std_avgdem",
    group_col = c("SPECIES_NAME", "rcp", "period_code"),
    rank_col = "std_rank_avgdem_within",
    descending = FALSE
  ) %>%
  rank_scores(
    score_col = "std_avgmar",
    group_col = c("SPECIES_NAME", "rcp", "period_code"),
    rank_col = "std_rank_avgmar_within",
    descending = FALSE
  ) %>%
  rank_scores(
    score_col = "std_avggen",
    group_col = c("SPECIES_NAME", "rcp", "period_code"),
    rank_col = "std_rank_avggen_within",
    descending = FALSE
  )



# 7. Calculate rank differences (cross-species vs within-species) ---------

# cat("Calculating rank differences between cross-species and within-species...\n")
# 
combined_scores_std <- combined_scores_std %>%
  mutate(
    # Difference in ranks (positive = ranked higher cross-species than within-species)
    rank_diff_avgall = std_rank_avgall_cross - std_rank_avgall_within,
    rank_diff_sumavgs = std_rank_sumavgs_cross - std_rank_sumavgs_within,
    rank_diff_sumcube = std_rank_sumcube_cross - std_rank_sumcube_within,
    rank_diff_avgfwR = std_rank_avgfwR_cross - std_rank_avgfwR_within,
    rank_diff_avgmigr = std_rank_avgmigr_cross - std_rank_avgmigr_within,
    rank_diff_avgdem = std_rank_avgdem_cross - std_rank_avgdem_within,
    rank_diff_avgmar = std_rank_avgmar_cross - std_rank_avgmar_within
 )


# 8. Summary statistics ----------------------------------------------------

# Filter to one scenario for summary
summary_data <- combined_scores_std %>%
  filter(rcp == "45", period_code == 3)

# Species-specific summaries
species_summary <- summary_data %>%
  group_by(SPECIES_NAME) %>%
  dplyr::summarize(
    n_CUs = n(),
    mean_score = mean(std_avgall, na.rm = TRUE),
    mean_rank_cross = mean(std_rank_sumavgs_cross, na.rm = TRUE),
    mean_rank_within = mean(std_rank_sumavgs_within, na.rm = TRUE),
    max_rank_diff = max(abs(rank_diff_sumavgs), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_score))


# Identify CUs with largest rank differences
large_diffs <- summary_data %>%
  select(FULL_CU_IN, CVIS_NAME, SPECIES_NAME,
    std_rank_sumavgs_cross, std_rank_sumavgs_within, rank_diff_sumavgs) %>%
  arrange(desc(abs(rank_diff_sumavgs))) %>%
  head(10)




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


cu_all_raw <- get_CU_indicators(all_flat_std,
                                cu_i = cu_i,
                                use_standardized = FALSE,
                                period_pick = "3",
                                RCP_pick = "45")

cu_all_std <- get_CU_indicators(all_flat_std,
                                cu_i = cu_i,
                                use_standardized = TRUE,
                                period_pick = "3",
                                RCP_pick = "45")

cu_all <- cu_all_raw %>%
  left_join(cu_all_std, join_by(FULL_CU_IN, rcp, period_code, indicator))

write.csv(cu_all, file = "CK-09_indicators.csv")
