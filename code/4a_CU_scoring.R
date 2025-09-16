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

# # # freshwater stream indicator statistics
# load(file.path(paths$fw, "2025-09-11_fw_rearing_indicators.Rdata"))
# # migration indicators
# load(file.path(paths$fw, "2025-09-11_migr_stats.Rdata"))
# # marine indicators
# load(file.path(paths$marine, "2025-09-10_marine_stats.Rdata"))


# combine indicators into common table
all_flat <- left_join(
  fwR_all_flat,
  migr_all_flat,
  join_by(FULL_CU_IN, CU_NAME, CU_Species, FAZ, RCP, period)
) %>%
  left_join(mar_all_flat, join_by(FULL_CU_IN, CU_NAME, RCP, period_code)) %>%
  left_join(CVIS_dem, join_by(FULL_CU_IN, CU_NAME, Species_simple))

# extract indicator columns, including mean and variation
all_inds <- all_flat %>%
  select(FULL_CU_IN, CU_NAME, CVIS_NAME, CU_Species, FAZ,
    period_code, RCP, SSP, contains(tbl_indicators$abbrev))





#------------- 2. Calculate standardized scores-------------------------------

all_std <- select(all_flat, "FULL_CU_IN", "RCP", "period_code")

for (i in 1:nrow(tbl_indicators)) {

  std_params_i <- as.list(tbl_standardize[i, ])

  # args <- list(std_params_i)

  temp <- standardize_indicator(all_flat,
    indicator_pick = tbl_indicators$abbrev[i],
    std_fun = tbl_indicators$std_fun[i],
    std_params = std_params_i,
    use_gcm_range = F
  )


  all_std <- all_std %>%
    left_join(temp, join_by("FULL_CU_IN", "RCP", "period_code"))
}


#### calculate average standardized vulnerability scores across categories
# fwR_all_std <- fwR_all_std %>%
#   mutate(std_avg_wmean = rowMeans(across(starts_with("std") & ends_with("wmean")), na.rm =T),
#          std_avg_wmean_qlow_gcm = rowMeans(across(starts_with("std") & ends_with("min_gcm")), na.rm =T),
#          std_avg_wmean_qhigh_gcm = rowMeans(across(starts_with("std") & ends_with("max_gcm")), na.rm =T))

## merge standardized and original indicator values for plotting and comparisons

## add std to col names for standardized values
all_flat_std <- all_std %>%
  rename_with(
    .fn = ~ paste0("std_", .x),
    .cols = matches(tbl_indicators$abbrev)
  )

all_flat_std <- all_flat_std %>%
  left_join(all_flat, join_by(FULL_CU_IN, RCP, period_code))


# 3. Species and all CU averages ------------------------------------------

# calculate species and all CU average for each indicator (for plotting) and put into unique columns
all_std_sp_avgs <- all_flat_std %>%
  group_by(CU_Species, RCP, period_code) %>%
  summarize(across(starts_with("std_"), \(x) mean(x, na.rm = TRUE)), .groups = "drop") %>%
  mutate(FULL_CU_IN = "Species average", CU_NAME = "Species average") %>%
  # rename columns to indicate species average
  rename_with(.fn = ~ paste0("spavg_", .x), .cols = starts_with("std_"))  %>%
  # bind new columns back to CU values
  left_join(all_flat_std, join_by(CU_Species, RCP, period_code))


# calculate overall average for each indicator (for plotting)
all_std_avgs <- all_flat_std %>%
  group_by(RCP, period_code) %>%
  summarize(across(starts_with("std_"), \(x) mean(x, na.rm = TRUE)), .groups = "drop") %>%
  mutate(FULL_CU_IN = "All CUs", CU_NAME = "All CUs", CU_Species = "All CUs")

# order categories as factors
all_flat_std$CU_Species <- factor(all_flat_std$CU_Species, levels = sort(unique(all_flat_std$CU_Species)))


# 4. Overall scores using additive and multiplicative methods -------------

means_std <- all_flat_std %>%
  subset_ind_table(indicators_choose = tbl_indicators$abbrev,
    get_std = T,
    get_raw = F) %>%
  sum_selected_columns(match_strings = tbl_indicators$abbrev,
    new_col_name = "std_addall") %>%
  sum_selected_columns(match_strings = c("Tw8rate", "Tw8proj", "lowQpdelta", "highQpdelta", "ct"),
    new_col_name = "std_addfwR") %>%
  sum_selected_columns(match_strings = tbl_indicators$abbrev[tbl_indicators$type == "migr"],
    new_col_name = "std_addmigr") %>%
  multiply_selected_columns(match_strings = tbl_indicators$abbrev,
    new_col_name = "std_prodall") %>%
  multiply_selected_columns(match_strings = c("Tw8rate", "Tw8proj", "lowQpdelta", "highQpdelta", "ct"),
    new_col_name = "std_prodfwR") %>%
  multiply_selected_columns(match_strings = tbl_indicators$abbrev[tbl_indicators$type == "migr"],
    new_col_name = "std_prodmigr")



write.csv(all_flat_std, file = file.path(paths$indicators, paste0(today, "_standardized_indicators.csv")), row.names = FALSE)
