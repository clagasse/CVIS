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

# # freshwater stream indicator statistics
load(file.path(paths$fw, "2025-09-11_fw_rearing_indicators.Rdata"))
# migration indicators
load(file.path(paths$fw, "2025-09-11_migr_stats.Rdata"))
# marine indicators
load(file.path(paths$marine, "2025-09-10_marine_stats.Rdata"))


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


write.csv(all_flat_std, file = file.path(paths$indicators, paste0(today, "_standardized_indicators.csv")), row.names = FALSE)



# 4. Functions for reorganizing indicator data -------------------------------

get_CU_indicators <- function(data,
                              cu_i,
                              RCP_pick = "45",
                              period_pick = "3",
                              indicators_choose = tbl_indicators$abbrev,
                              use_standardized = TRUE) {
  data <- filter(data,
    RCP == RCP_pick,
    period_code == period_pick)

  sp_col <- "CU_Species"
  id_col <- "FULL_CU_IN"

  sp_pick <- data %>%
    filter(FULL_CU_IN == cu_i) %>%
    pull(sp_col) %>%
    as.character() %>%
    unique()

  # subset columns for chosen indicators
  cols_sub <- names(data)[str_detect(names(data), paste(indicators_choose, collapse = "|"))]

  data_sub <- data %>%
    select(all_of(c(id_col, sp_col, cols_sub)))

  sp_avgs <- data_sub %>%
    filter(CU_Species == sp_pick) %>%
    summarize(across(starts_with("std_"), \(x) mean(x, na.rm = TRUE)), .groups = "drop") %>%
    # mutate(FULL_CU_IN = "Species average", CU_NAME = "Species average") %>%
    # rename columns to indicate species average
    # rename_with(.fn = ~ paste0("spavg_", .x), .cols = starts_with("std_")) %>%
    pivot_longer(cols = contains("std"),
      values_to = "sp_value",
      names_prefix = "std_",
      names_sep = "_",
      names_to = c("indicator", "stat"))

  data_CU <- data_sub %>%
    filter(FULL_CU_IN == cu_i) %>%
    select(id_col, sp_col, starts_with("std_")) %>%
    pivot_longer(cols = contains("std"),
      values_to = "cu_value",
      names_prefix = "std_",
      names_sep = "_",
      names_to = c("indicator", "stat"))

  data_CU <- data_CU %>%
    left_join(sp_avgs, join_by(indicator, stat)) %>%
    mutate(stat = if_else(is.na(stat), "mean", stat))  # fill NAs with mean

  return(data_CU)
}
