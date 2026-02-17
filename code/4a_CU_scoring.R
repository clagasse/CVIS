################################################################################
#
# 4a_CU_scoring.R
#
# 1 - get indicator values for each CU (Long Format)
# 2 - standardize indicators using ensemble-based ranges
# 3 - calculate combined scores and ranks
# 4 - export long and wide format results
#
###############################################################################
#----------------1. Setup and import----------------
library(here)
setwd(here())
source(file.path(here(), "code", "0_setup.R"))

grouping_vars_pick <- c("rcp", "period_code", "dsmodel")

# Helper to find latest file by pattern
get_latest_file <- function(path, pattern) {
  files <- list.files(path, pattern = pattern, full.names = TRUE)
  if (length(files) == 0) stop("No files found matching ", pattern)
  # filter out files starting with ~ (temp files)
  files <- files[!grepl("^~", basename(files))]
  file_info <- file.info(files)
  latest_file <- rownames(file_info)[which.max(file_info$mtime)]
  cat("Loading latest file:", basename(latest_file), "\n")
  return(latest_file)
}

# --- Load Data ---

# Freshwater
fw_file <- get_latest_file(paths$fw, "fw_rearing_indicators.Rdata")
load(fw_file) # loads fw_all, ss_all

# Migration
migr_file <- get_latest_file(paths$fw, "migr_stats.Rdata")
load(migr_file) # loads migr_all, etc.

# Marine
mar_file <- get_latest_file(paths$marine, "marine_stats.Rdata") # look for .Rds
load(mar_file)

# Demographics/Genetics (cu_long) is loaded via 1a_CU_import (sourced in 0_setup)

# Standardize columns for binding
cols_keep <- c("FULL_CU_IN", "gcm", "gcm_name", "dsmodel", "rcp", "period_code", "indicator", "stat", "value", "category")

# Helper to prep dataframes
prep_df <- function(df, model_default = "none") {
  if (!"gcm_name" %in% names(df)) df$gcm_name <- NA
  if (!"dsmodel" %in% names(df)) df$dsmodel <- model_default

  df %>%
    mutate(
      FULL_CU_IN = as.character(FULL_CU_IN),
      rcp = as.character(rcp),
      period_code = as.character(period_code), # ensure consistency
      gcm = as.character(gcm),
      value = as.numeric(value)
    ) %>%
    select(any_of(cols_keep))
}

fw_long <- prep_df(fw_all)
migr_long <- prep_df(migr_all)
mar_long <- prep_df(mar_all)
cu_long_prep <- prep_df(cu_long, model_default = "observed")


# Combine all data and filter according to settings
all_long <- bind_rows(cu_long_prep, fw_long, migr_long, mar_long) %>%
  filter(!is.na(value)) %>%
  # Filter to relevant periods/rcps if needed, or keep all
  filter(period_code %in% periods_use) %>%
  #keep only selected cumulative threat type (ie. all)
  filter(!(str_detect(dsmodel, regex("cthr", ignore_case = TRUE)) & dsmodel != cthr_pick))

# Add species info
all_long <- select(cu_run, FULL_CU_IN, SPECIES_NAME, CVIS_NAME) %>%
  left_join(all_long, by = "FULL_CU_IN")



#------------- 2. Calculate standardized scores-------------------------------

cat("\nStandardizing indicators...\n")

all_std_long <- list()

# Loop through indicators in tbl_standardize
for (i in 1:nrow(tbl_standardize)) {
  ind_abbrev <- tbl_standardize$abbrev[i]
  std_params_i <- as.list(tbl_standardize[i, ])

  cat("Processing:", ind_abbrev, "\n")

  # Check if indicator exists in data
  if (nrow(filter(all_long, indicator == ind_abbrev)) == 0) {
    cat("  No data found for", ind_abbrev, "- skipping.\n")
    next
  }

  # Standardize
  std_result <- standardize_long_indicator(
    data = all_long,
    indicator_pick = ind_abbrev,
    std_fun = tbl_standardize$std_fun[i],
    std_params = std_params_i,
    calibration_gcm = "9" # Ensemble
  )

  all_std_long[[i]] <- std_result
}

all_std_long <- bind_rows(all_std_long)


# 3.  Scoring and ranks across indicators--------------------------------------


# Helper: compute standardized scores in LONG + tidy dimensions per (rcp, period_code)
# Returns: rcp, period_code, method, category, score
summarize_scores_long_tidy <- function(data) {
  data %>%
    group_by(rcp, period_code) %>%
    reframe({
      # Category averages (standardized)
      a_fwrs <- mean(std_value[category == "fwrs"], na.rm = TRUE)
      a_migr <- mean(std_value[category == "migr"], na.rm = TRUE)
      a_mar  <- mean(std_value[category == "mar"],  na.rm = TRUE)
      a_dem  <- mean(std_value[category == "dem"],  na.rm = TRUE)
      a_gen  <- mean(std_value[category == "gen"],  na.rm = TRUE)
      
      # Category power means (cube-root of mean of cubes)
      c_fwrs <- mean(std_value[category == "fwrs"]^3, na.rm = TRUE)^(1/3)
      c_migr <- mean(std_value[category == "migr"]^3, na.rm = TRUE)^(1/3)
      c_mar  <- mean(std_value[category == "mar"]^3,  na.rm = TRUE)^(1/3)
      c_dem  <- mean(std_value[category == "dem"]^3,  na.rm = TRUE)^(1/3)
      c_gen  <- mean(std_value[category == "gen"]^3,  na.rm = TRUE)^(1/3)
      
      # Overall metrics (category should be "all")
      avg_all  <- mean(std_value, na.rm = TRUE)                               # method = avgall
      cat_avgs <- mean(c(a_fwrs, a_migr, a_mar, a_dem, a_gen), na.rm = TRUE)  # method = catavg
      avg_cube <- mean(c(c_fwrs, c_migr, c_mar, c_dem, c_gen), na.rm = TRUE)  # method = avgcube
      
      tibble(
        method   = c(
          # per-category metrics
          rep("avg", 5), rep("cube", 5),
          # overall metrics (category = "all")
          "catavg", "avgall", "avgcube"
        ),
        category = c(
          # avg by category
          "fwrs","migr","mar","dem","gen",
          # cube by category
          "fwrs","migr","mar","dem","gen",
          # overall (force category = "all")
          "all", "all", "all"
        ),
        score    = c(
          a_fwrs, a_migr, a_mar, a_dem, a_gen,
          c_fwrs, c_migr, c_mar, c_dem, c_gen,
          cat_avgs, avg_all, avg_cube
        )
      )
    }) %>%
    ungroup()
}


# 1) Base scores 
scores_base <- all_std_long %>%
  filter(dsmodel %in% dsmodel_baseline) %>%
  group_by(FULL_CU_IN, SPECIES_NAME, CVIS_NAME, gcm) %>%
  group_modify(~ summarize_scores_long_tidy(.x)) %>%
  ungroup() %>%
  mutate(score = ifelse(is.nan(score), NA_real_, score))

# 2) Ranks for ALL metrics 
# Cross-species ranks (across all CUs) for each (rcp, period_code, gcm, method, category)
ranks_cross <- scores_base %>%
  group_by(rcp, period_code, gcm, method, category) %>%
  mutate(rankall = rank(score, ties.method = "average", na.last = "keep")) %>%
  ungroup() %>%
  select(FULL_CU_IN, SPECIES_NAME, CVIS_NAME, gcm, rcp, period_code, method, category, rankall)

# Within-species ranks for each (species, rcp, period_code, gcm, method, category)
ranks_within <- scores_base %>%
  group_by(SPECIES_NAME, rcp, period_code, gcm, method, category) %>%
  mutate(rankspecies = rank(score, ties.method = "average", na.last = "keep")) %>%
  ungroup() %>%
  select(FULL_CU_IN, SPECIES_NAME, CVIS_NAME, gcm, rcp, period_code, method, category, rankspecies)

# 3) Final tidy table with separate rank columns 
scores_tidy <- scores_base %>%
  left_join(ranks_cross,
            by = c("FULL_CU_IN","SPECIES_NAME","CVIS_NAME","gcm","rcp","period_code","method","category")) %>%
  left_join(ranks_within,
            by = c("FULL_CU_IN","SPECIES_NAME","CVIS_NAME","gcm","rcp","period_code","method","category")) %>%
  arrange(rcp, period_code, gcm, SPECIES_NAME, FULL_CU_IN, method, category)



# 4. Averaging scores -----------------------------------------------------

# 1) Score averages (from scores_tidy)
# scores_tidy has at least: rcp, period_code, method, category, score
score_avgs_tidy <- scores_tidy %>%
  group_by(rcp, period_code, method, category) %>%
  summarise(mean_value = mean(score, na.rm = TRUE), .groups = "drop")

# 2) Indicator averages (from all_std_long)
# all_std_long has at least: rcp, period_code, indicator, std_value
ind_avgs_tidy <- all_std_long %>%
  group_by(rcp, period_code, indicator) %>%
  summarise(mean_value = mean(std_value, na.rm = TRUE), .groups = "drop") %>%
  # Normalize to the same tidy shape: put indicator into 'category' and tag method
  mutate(
    measure_type = "indicator",
    method = "indicator",
    category = indicator
  ) %>%
  select(rcp, period_code, method, category, mean_value)


# 5. Save outputs ---------------------------------------------------------

write.csv(all_std_long, file.path(paths$output, "all_std_long.csv"))
write.csv(scores_tidy, file.path(paths$output, "overall_scores.csv"))



