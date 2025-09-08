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

# load FW rearing indicators
fwR_all_flat <- read_csv(file.path(paths$fw, "2025-09-04_fw_rearing_stats.csv")) %>%
  mutate(RCP = as.character(RCP)) %>%
  rename(
    period_code = period,
    fw_res = Peak_Spawn_To_Ocean_Entry_Days
  ) %>%
  left_join(period_lookup, join_by(period_code)) %>%
  relocate(period, .after = period_code)


# load FW migration indicators
load(file.path(paths$fw, "2025-09-04_migr_stats.Rdata"))

# load marine indicators
mar_all_flat <- read_csv(file.path(paths$marine, "2025-09-04_marine_stats.csv"))


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
  select(FULL_CU_IN, CU_NAME, CU_Species, FAZ, period_code, RCP, SSP, contains(tbl_indicators$abbrev))




#--------------2. Functions for standardization ------
standardize_indicator <- function(data,
                                  indicator_pick,
                                  std_fun = "linear_std",
                                  gcm_range_suffix = c("qlowgcm", "qhighgcm"),
                                  use_gcm_range = TRUE) # use GCMs to include GCM quantiles in standardization ranges, set FALSE to only use mean values
{
  stat_suffix <- "mean"
  id_col <- "FULL_CU_IN"

  # data <- filter(data, period == period_pick)

  # take column names that contain prefix with model type
  cols_sub <- names(data)[str_detect(names(data), indicator_pick)]

  # take names with prefix that also contain stat suffix
  stat_col <- cols_sub[str_detect(cols_sub, paste0(stat_suffix, "$"))] # must end with mean
  min_gcmcol <- cols_sub[str_detect(cols_sub, paste0(gcm_range_suffix[1], collapse = "|"))]
  max_gcmcol <- cols_sub[str_detect(cols_sub, paste0(gcm_range_suffix[2], collapse = "|"))]

  # some indicators don't have a mean, just the raw value. in that case, use the abbreviation
  if (length(stat_col) == 0) stat_col <- cols_sub

  data <- select(data, c(FULL_CU_IN, stat_col, min_gcmcol, max_gcmcol, RCP, period_code))

  if (length(min_gcmcol) == 1 && length(max_gcmcol) == 1 && use_gcm_range == TRUE) {
    # put gcm lows and highs and mean into one column
    data_long <- pivot_longer(data,
      cols = starts_with(indicator_pick),
      names_prefix = paste0(indicator_pick, "_")
    )

    data_std <- data_long %>%
      group_by(RCP, period_code) %>%
      reframe(
        FULL_CU_IN = FULL_CU_IN,
        std = get(std_fun)(value),
        name = name
      ) %>%
      pivot_wider(
        names_from = name,
        values_from = std,
        names_prefix = paste0(indicator_pick, "_")
      )
  } else {
    data_std <- data %>%
      group_by(RCP, period_code) %>%
      reframe(
        FULL_CU_IN = FULL_CU_IN,
        !!stat_col := get(std_fun)(!!sym(stat_col))
      )
    # std_qlowgcm= get(std_fun)(!!sym(min_gcmcol)),
    # std_qhighgcm = get(std_fun)(!!sym(max_gcmcol)))
  }

  return(data_std)
}

#------------- 3. Calculate standardized scores-------------------------------

all_std <- select(all_flat, "FULL_CU_IN", "RCP", "period_code")

for (i in 1:nrow(tbl_indicators)) {
  temp <- standardize_indicator(all_flat,
    indicator_pick = tbl_indicators$abbrev[i],
    std_fun = tbl_indicators$std_fun[i],
    use_gcm_range = T
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




write.csv(all_flat_std, file = file.path(paths$indicators, paste0(today, "_standardized_indicators.csv")), row.names = FALSE)
