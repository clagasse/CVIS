# ==============================================================================
# CVIS Scoring & Standardization Utility Functions (4_scoring_utils.R)
#
# Description:
#   Utility functions for standardizing raw indicator values, tidying/reshaping
#   indicator tables, ranking scores, scaling, and calculating aggregated
#   vulnerability/exposure portfolio scores.
#
# Functions:
#   - linear_std, invlinear_std: 0 to 1 linear scaling (with optional 95% quantiles).
#   - logarithmic_std, exponential_std, decay_std: Non-linear scaling.
#   - step_std, cat_std, enh_std: Category/discrete step scaling.
#   - simulate_range: Simulates sequences between min/max values.
#   - sum_selected_columns, multiply_selected_columns, power_selected_columns, average_selected_columns: Rowwise aggregation helpers.
#   - subset_ind_table: Subsets and pivots wide/long indicator data tables.
#   - rename_ind_table: Renames columns with standardized suffix formatting.
#   - standardize_long_indicator: Standardizes long-format data using calibration params.
#   - get_CU_indicators: Extracts CU-specific and regional baseline statistics.
#   - rank_scores: Ranks scores across CUs or subgroups.
#   - scale_0_100: Normalizes numeric vectors to 0-100 scale.
#   - hinge_weight: Soft thresholding indicator function for red flags.
#   - calculate_combined_scores: Aggregates category and overall vulnerability portfolio scores.
#
# Dependencies:
#   - dplyr, stringr, tidyr, purrr, tibble
# ==============================================================================


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


# ==================== 1. Indicator Standardization Functions ====================


# raw standardization function between 0 and 1 using min and max values
# if xmin or xmax are set, a manual min and max range are used for standardizing between 0 and 1
# if use_95 = TRUE, the min and max are set based on 95% quantile ranges to exclude outliers

linear_std <- function(x, ..., xmin = NA, xmax = NA, use_95 = F) {
  if (is.na(xmax)) {
    xmax <- max(x, na.rm = T)
    if (use_95 == TRUE) xmax <- unlist(quantile(x, na.rm = T, probs = 0.975))
  }
  if (is.na(xmin)) {
    xmin <- min(x, na.rm = T)
    if (use_95 == TRUE) xmin <- unlist(quantile(x, na.rm = T, probs = 0.025))
  }

  y <- rep(NA, length(x))

  for (i in 1:length(x)) {
    if (is.na(x[i])) next
    z <- ifelse(x[i] < xmin, xmin,
      ifelse(x[i] > xmax, xmax, x[i])
    )
    y[i] <- (z - xmin) / (xmax - xmin)
    if (xmax == xmin) y[i] <- 0.5 # set standardized value to 0.5 if xmin = xmax
  }

  return(y)
}

# inverse raw standardization function with 1 corresponding to lowest value
invlinear_std <- function(x, ..., xmin = NA, xmax = NA, use_95 = F) {
  if (is.na(xmax)) {
    xmax <- max(x, na.rm = T)
    if (use_95 == TRUE) xmax <- unlist(quantile(x, na.rm = T, probs = 0.975))
  }
  if (is.na(xmin)) {
    xmin <- min(x, na.rm = T)
    if (use_95 == TRUE) xmin <- unlist(quantile(x, na.rm = T, probs = 0.025))
  }

  y <- rep(NA, length(x))

  for (i in 1:length(x)) {
    if (is.na(x[i])) next
    z <- ifelse(x[i] < xmin, xmin, ifelse(x[i] > xmax, xmax, x[i]))
    y[i] <- 1 - (z - xmin) / (xmax - xmin)
    if (xmax == xmin) y[i] <- 0.5 # set standardized value to 0.5 if xmin = xmax
  }
  return(y)
}

## asymptotic standardization function for indicators where higher values = greater risk
logarithmic_std <- function(x, ..., lambda = 0.6, xmin = NA, xmax = NA) {
  if (is.na(xmax)) xmax <- max(x, na.rm = T)
  if (is.na(xmin)) xmin <- min(x, na.rm = T)
  y <- rep(NA, length(x))

  for (i in 1:length(x)) {
    if (is.na(x[i])) next
    z <- ifelse(x[i] < xmin, xmin, ifelse(x[i] > xmax, xmax, x[i]))
    z_std <- (z - xmin) / (xmax - xmin)
    y[i] <- 1 - exp(-lambda * z_std) + exp(-lambda) * z_std
  }
  return(y)
}

# exponential increase function where higher values = greater risk
exponential_std <- function(x, ..., lambda = 1, xmin = NA, xmax = NA, use_95 = F) {
  if (is.na(xmax)) {
    xmax <- max(x, na.rm = T)
    if (use_95 == TRUE) xmax <- unlist(quantile(x, na.rm = T, probs = 0.975))
  }
  if (is.na(xmin)) {
    xmin <- min(x, na.rm = T)
    if (use_95 == TRUE) xmin <- unlist(quantile(x, na.rm = T, probs = 0.025))
  }

  y <- rep(NA, length(x))
  for (i in 1:length(x)) {
    if (is.na(x[i])) next
    z <- ifelse(x[i] < xmin, xmin, ifelse(x[i] > xmax, xmax, x[i]))
    z_std <- (z - xmin) / (xmax - xmin)
    y[i] <- exp(lambda * z_std) / exp(lambda) - exp(-lambda) * (1 - z_std)
    if (xmax == xmin) y[i] <- 0.5 # set standardized value to 0.5 if xmin = xmax
  }

  return(y)
}

# exponential decay standardization function for indicators where higher values = lower risk
decay_std <- function(x, ..., lambda = 0.03, xmin = NA, xmax = NA, use_95 = F) {
  if (is.na(xmax)) {
    xmax <- max(x, na.rm = T)
    if (use_95 == TRUE) xmax <- unlist(quantile(x, na.rm = T, probs = 0.975))
  }
  if (is.na(xmin)) {
    xmin <- min(x, na.rm = T)
    if (use_95 == TRUE) xmin <- unlist(quantile(x, na.rm = T, probs = 0.025))
  }
  y <- rep(NA, length(x))

  for (i in 1:length(x)) {
    if (is.na(x[i])) next
    z <- ifelse(x[i] < xmin, xmin, ifelse(x[i] > xmax, xmax, x[i]))
    z_std <- (z - xmin) / (xmax - xmin)
    y[i] <- exp(-lambda * z_std) - exp(-lambda) * z_std
  }
  return(y)
}

step_std <- function(x, ..., x1 = 200, x2 = 300, x3 = NA) {
  # Standardize a score into 3 categories
  y <- rep(NA, length(x))

  nstep <- sum(!is.na(c(x1, x2, x3)))

  for (i in 1:length(x)) {
    if (is.na(x[i])) next
    if (x[i] < x1) y[i] <- 0
    if (x[i] >= x1 && x[i] < x2) y[i] <- (1 / nstep)
    if (x[i] >= x2) y[i] <- (2 / nstep)
  }

  return(y)
}


cat_std <- function(x, ..., x1 = 1, x2 = 2, x3 = 3) {
  # Standardize a score between 0 and 1 based on category values
  y <- rep(NA, length(x))

  for (i in 1:length(x)) {
    if (is.na(x[i])) next
    if (x[i] == x1) y[i] <- 0
    if (x[i] == x2) y[i] <- 0.5
    if (x[i] == x3) y[i] <- 1
  }
  return(y)
}

enh_std <- function(x, ..., z) {
  # Standardize a score between 0 and 1 based on category values
  y <- rep(NA, length(x))

  for (i in 1:length(x)) {
    if (x[i] == 0) y[i] <- 0
    if (x[i] > 0 && !grepl("Harvest", z[i])) y[i] <- 0.5
    if (x[i] > 0 && grepl("Harvest", z[i])) y[i] <- 1
  }
  return(y)
}


simulate_range <- function(x, n = 100) {
  minx <- min(x, na.rm = T)
  maxx <- max(x, na.rm = T)

  y <- seq(from = minx, to = maxx, length.out = n)
  return(y)
}


# ==================== 2. Data Reshaping & Tidying Helpers ====================

sum_selected_columns <- function(data, match_strings, new_col_name = "row_sum") {
  pattern <- paste(match_strings, collapse = "|")
  selected_cols <- names(data)[str_detect(names(data), pattern)]
  if (sum(selected_cols %in% "SPECIES_NAME") > 0) selected_cols <- selected_cols[str_detect(selected_cols, "SPECIES_NAME", negate = TRUE)]

  # Apply row-wise sum across selected columns
  data %>%
    rowwise() %>%
    mutate(
      !!new_col_name := sum(c_across(all_of(selected_cols)), na.rm = TRUE)
    ) %>%
    ungroup()
}

multiply_selected_columns <- function(data, match_strings, new_col_name = "row_product") {
  pattern <- paste(match_strings, collapse = "|")
  selected_cols <- names(data)[str_detect(names(data), pattern)]
  if (sum(selected_cols %in% "SPECIES_NAME") > 0) selected_cols <- selected_cols[str_detect(selected_cols, "SPECIES_NAME", negate = TRUE)]

  # Apply row-wise sum across selected columns
  data %>%
    rowwise() %>%
    mutate(
      !!new_col_name := prod(c_across(all_of(selected_cols)), na.rm = TRUE)
    ) %>%
    ungroup()
}

power_selected_columns <- function(data, match_strings, new_col_name = "power_mean", p = 3) {
  pattern <- paste(match_strings, collapse = "|")
  selected_cols <- names(data)[str_detect(names(data), pattern)]
  if (sum(selected_cols %in% "SPECIES_NAME") > 0) {
    selected_cols <- selected_cols[str_detect(selected_cols, "SPECIES_NAME", negate = TRUE)]
  }

  data %>%
    rowwise() %>%
    mutate(
      !!new_col_name := (mean(c_across(all_of(selected_cols))^p, na.rm = TRUE))^(1 / p)
    ) %>%
    ungroup()
}


average_selected_columns <- function(data, match_strings, new_col_name = "row_product") {
  pattern <- paste(match_strings, collapse = "|")
  selected_cols <- names(data)[str_detect(names(data), pattern)]
  if (sum(selected_cols %in% "SPECIES_NAME") > 0) selected_cols <- selected_cols[str_detect(selected_cols, "SPECIES_NAME", negate = TRUE)]

  # Apply row-wise sum across selected columns
  data %>%
    rowwise() %>%
    mutate(
      !!new_col_name := mean(c_across(all_of(selected_cols)), na.rm = TRUE)
    ) %>%
    ungroup()
}



# helper function used in other functions for subsetting indicator table
# by default will take the mean and gcm variation of unstandardized columns for the selected indicator
subset_ind_table <- function(data,
                             indicators_choose,
                             sp_col = "SPECIES_NAME",
                             stat_suffix = "mean",
                             id_col = "FULL_CU_IN",
                             rcp_col = "rcp",
                             period_col = "period_code",
                             get_raw = T, # include unstandardized columns
                             get_std = F, # include standardized columns
                             get_gcm = T, # include gcm variation
                             get_spat = F, # include spatial variation,
                             get_pop = F, # include population variation (genetic indicators)
                             rename_cols = T, # rename id and sp cols
                             gcm_range_suffix = c("qlowgcm", "qhighgcm", "qmingcm", "qmaxgcm"),
                             sp_range_suffix = c("qlowsp", "qhighsp"),
                             pop_suffix = c("popmin", "popmax")) {
  # Detect Long Format (indicator, stat, value columns)
  is_long <- "indicator" %in% names(data) && "stat" %in% names(data) && "value" %in% names(data)

  if (is_long) {
    # 1. Standard Handle for Long Format Indicators
    data_sub <- data %>%
      filter(indicator %in% indicators_choose)

    # Define keys for pivoting/joining
    keys <- c(id_col, sp_col, rcp_col, period_col)
    keys <- keys[keys %in% names(data)]

    # Pivot RAW values
    raw_wide <- NULL
    if (get_raw) {
      # Filter for main stats and ensemble gcm (9)
      raw_data <- data_sub %>%
        filter(gcm == "9" | is.na(gcm)) %>%
        filter(stat == stat_suffix) # Only get the main stat (mean)

      raw_wide <- raw_data %>%
        pivot_wider(
          id_cols = all_of(keys),
          names_from = c(indicator, stat),
          values_from = value,
          names_sep = "_"
        )
    }

    # Pivot STD values
    std_wide <- NULL
    if (get_std) {
      std_data <- data_sub %>%
        filter(gcm == "9" | is.na(gcm)) %>%
        filter(stat == stat_suffix) # Only get the main stat (mean)

      std_wide <- std_data %>%
        pivot_wider(
          id_cols = all_of(keys),
          names_from = c(indicator, stat),
          values_from = std_value,
          names_sep = "_",
          names_prefix = "std_"
        )
    }

    # Get GCM ranges from long data
    gcm_wide <- NULL
    if (get_gcm) {
      gcm_data <- data_sub %>%
        filter(stat %in% gcm_range_suffix)

      if (nrow(gcm_data) > 0) {
        gcm_wide <- gcm_data %>%
          pivot_wider(
            id_cols = all_of(keys),
            names_from = c(indicator, stat),
            values_from = if_else(get_std, std_value, value),
            names_sep = "_"
          )
      }
    }

    # Combine all
    data_out <- raw_wide
    if (!is.null(std_wide)) {
      if (is.null(data_out)) data_out <- std_wide else data_out <- left_join(data_out, std_wide, by = keys)
    }
    if (!is.null(gcm_wide)) {
      if (is.null(data_out)) data_out <- gcm_wide else data_out <- left_join(data_out, gcm_wide, by = keys)
    }

    data <- data_out
  } else {
    # 2. Handle Wide/Modified Format (like scores_long)
    # If it's a "wide" table but has multiple GCM rows, we may need to summarize GCM ranges

    # Check if multiple GCMs exist for the same CU/RCP/Period
    has_target_gcm_col <- "gcm" %in% names(data)

    if (has_target_gcm_col && get_gcm) {
      # Identify indicator/score columns
      score_cols <- names(data)[str_detect(names(data), paste0(indicators_choose, collapse = "|"))]

      # Grouping keys
      group_keys <- c(id_col, sp_col, rcp_col, period_col)
      group_keys <- group_keys[group_keys %in% names(data)]

      # Calculate GCM ranges if not present
      if (sum(str_detect(score_cols, "qlowgcm|qhighgcm")) == 0) {
        cat("Summarizing GCM variation for columns:", paste(score_cols, collapse = ", "), "\n")

        gcm_summary <- data %>%
          group_by(across(all_of(group_keys))) %>%
          summarise(
            across(all_of(score_cols),
              list(
                qlowgcm = ~ quantile(.x, 0.1, na.rm = T),
                qhighgcm = ~ quantile(.x, 0.9, na.rm = T)
              ),
              .names = "{.col}_{.fn}"
            ),
            .groups = "drop"
          )

        # Filter data for ensemble/main view (GCM 9)
        ensemble_data <- data %>% filter(gcm == "9" | is.na(gcm))
        if (nrow(ensemble_data) == 0) {
          ensemble_data <- data %>%
            group_by(across(all_of(group_keys))) %>%
            slice(1) %>%
            ungroup()
        }

        data <- left_join(ensemble_data, gcm_summary, by = group_keys)
      }
    }
  }

  # Final Selection and Column Management
  cols_sub <- names(data)[str_detect(names(data), paste0(indicators_choose, collapse = "|"))]

  if (get_raw == F) cols_sub <- cols_sub[str_detect(cols_sub, "std")]
  if (get_std == F) cols_sub <- cols_sub[!str_detect(cols_sub, "std")]

  # Filters stats
  stat_col <- cols_sub[!str_detect(cols_sub, paste0(c(gcm_range_suffix, sp_range_suffix, pop_suffix), collapse = "|"))]

  cols_out <- c(id_col, sp_col)
  if (rcp_col %in% names(data)) cols_out <- c(cols_out, rcp_col)
  if (period_col %in% names(data)) cols_out <- c(cols_out, period_col)
  cols_out <- c(cols_out, stat_col)

  if (get_gcm) cols_out <- c(cols_out, names(data)[names(data) %in% paste0(indicators_choose, "_", rep(gcm_range_suffix, each = length(indicators_choose)))])
  if (get_spat) cols_out <- c(cols_out, names(data)[names(data) %in% paste0(indicators_choose, "_", rep(sp_range_suffix, each = length(indicators_choose)))])
  if (get_pop) cols_out <- c(cols_out, names(data)[names(data) %in% paste0(indicators_choose, "_", rep(pop_suffix, each = length(indicators_choose)))])

  data_sub <- data %>% select(any_of(cols_out))

  if (rename_cols) {
    # Check if columns exist before renaming
    rename_list <- list()
    if (id_col %in% names(data_sub)) rename_list$id <- id_col
    if (sp_col %in% names(data_sub)) rename_list$sp <- sp_col
    data_sub <- data_sub %>% rename(!!!rename_list)
  }

  return(data_sub)
}


rename_ind_table <- function(data,
                             indicator_abbrev,
                             gcm_range_suffix = c("qlowgcm", "qhighgcm", "qmingcm", "qmaxgcm"),
                             sp_range_suffix = c("qlowsp", "qhighsp"),
                             pop_suffix = c("popmin", "popmax"),
                             name_suffix = "", # optional name suffix
                             single_value_col = FALSE) {
  # Identify relevant columns belonging to this indicator
  cols_sub <- names(data)[str_detect(names(data), indicator_abbrev)]

  # 1. Identify variation columns first (more specific patterns)
  min_gcmcol <- cols_sub[str_detect(cols_sub, gcm_range_suffix[1])][1]
  max_gcmcol <- cols_sub[str_detect(cols_sub, gcm_range_suffix[2])][1]

  min_spatcol <- cols_sub[str_detect(cols_sub, sp_range_suffix[1])][1]
  max_spatcol <- cols_sub[str_detect(cols_sub, sp_range_suffix[2])][1]

  min_popcol <- cols_sub[str_detect(cols_sub, pop_suffix[1])][1]
  max_popcol <- cols_sub[str_detect(cols_sub, pop_suffix[2])][1]

  variation_cols <- na.omit(c(min_gcmcol, max_gcmcol, min_spatcol, max_spatcol, min_popcol, max_popcol))

  # 2. Identify main values among the remaining columns
  remaining_cols <- setdiff(cols_sub, variation_cols)

  std_col <- remaining_cols[str_detect(remaining_cols, "std")][1]
  stat_col <- setdiff(remaining_cols, std_col)[1]

  out_data <- data

  # Rename main values
  if (!is.na(stat_col)) {
    out_data <- out_data %>%
      rename(!!paste0(name_suffix, "raw") := !!stat_col)
  }
  if (!is.na(std_col)) {
    out_data <- out_data %>%
      rename(!!paste0(name_suffix, "std") := !!std_col)
  }

  # Rename variation columns
  if (!is.na(min_gcmcol)) {
    out_data <- out_data %>%
      rename(!!paste0(name_suffix, "min_gcm") := !!min_gcmcol)
  }
  if (!is.na(max_gcmcol)) {
    out_data <- out_data %>%
      rename(!!paste0(name_suffix, "max_gcm") := !!max_gcmcol)
  }

  if (!is.na(min_spatcol)) {
    out_data <- out_data %>%
      rename(!!paste0(name_suffix, "min_spat") := !!min_spatcol)
  }
  if (!is.na(max_spatcol)) {
    out_data <- out_data %>%
      rename(!!paste0(name_suffix, "max_spat") := !!max_spatcol)
  }

  if (!is.na(min_popcol)) {
    out_data <- out_data %>%
      rename(!!paste0(name_suffix, "min_pop") := !!min_popcol)
  }
  if (!is.na(max_popcol)) {
    out_data <- out_data %>%
      rename(!!paste0(name_suffix, "max_pop") := !!max_popcol)
  }

  return(out_data)
}



# New function for standardizing long format data
standardize_long_indicator <- function(data,
                                       calibration_data = NULL,
                                       indicator_pick,
                                       target_stat = "mean",
                                       std_fun = "linear_std",
                                       std_params = NA,
                                       calibration_gcm = "9", # Ensemble mean GCM code
                                       grouping_vars, # variables to group when determining std ranges
                                       id_col = "FULL_CU_IN",
                                       species_col = "SPECIES_NAME",
                                       baseline_rcp = NA,
                                       baseline_period = NA) {
  range_type <- std_params$range_type

  # Filter for the specific indicator
  # AND specific statistics we want to standardize (target_stat + GCM variations)
  # We exclude "nsegments", "length", etc. unless they ARE the target stats.

  # Standard variations to keep if present
  # qlowgcm, qhighgcm, qmingcm, qmaxgcm
  # We assume these should be standardized using the same params as the mean.
  stats_to_std <- c(target_stat, "qlowgcm", "qhighgcm", "qmingcm", "qmaxgcm")

  data_sub <- data %>%
    filter(
      indicator == indicator_pick,
      stat %in% stats_to_std
    )

  if (nrow(data_sub) == 0) {
    warning(paste("No data found for indicator:", indicator_pick, "with stats:", paste(stats_to_std, collapse = ", ")))
    return(NULL)
  }

  # Define grouping for calibration (determining range)
  if (!is.na(range_type) && range_type == "species") {
    grouping_vars <- c(grouping_vars, species_col)
  }

  if (!is.na(baseline_rcp)) {
    grouping_vars <- setdiff(grouping_vars, "rcp")
  }
  if (!is.na(baseline_period)) {
    grouping_vars <- setdiff(grouping_vars, "period_code")
  }

  # 1. Identify Calibration Data for Range Determination
  use_custom_calib <- FALSE
  if (!is.null(calibration_data)) {
    calib_subset <- calibration_data %>%
      filter(indicator == indicator_pick, stat == target_stat)
    
    if (nrow(calib_subset) > 0) {
      use_custom_calib <- TRUE
      calibration_data_final <- calib_subset
    }
  }

  if (!use_custom_calib) {
    # Default logic (subsetting from data_sub)
    calibration_data_final <- data_sub %>%
      filter(
        gcm == calibration_gcm,
        stat == target_stat
      )

    if (!is.na(baseline_rcp)) {
      calibration_data_final <- calibration_data_final %>% filter(rcp == baseline_rcp | is.na(rcp))
    }
    if (!is.na(baseline_period)) {
      calibration_data_final <- calibration_data_final %>% filter(period_code == baseline_period | is.na(period_code))
    }

    # Check if default calibration data exists
    if (nrow(calibration_data_final) == 0) {
      warning(paste("No default calibration data (calibration criteria not met) found for:", indicator_pick, ". Using all data for range."))
      calibration_data_final <- data_sub
    }
  }

  # 2. Calculate Standardization Parameters (xmin, xmax) per group

  # Function to get params for a group
  get_group_params <- function(val, current_params) {
    # If xmin/xmax are NA, calculate them from val

    use_95 <- if (!is.null(current_params$use_95)) current_params$use_95 else TRUE

    p_out <- current_params
    val <- val[!is.na(val)]
    if (length(val) == 0) return(tibble(xmin_calib = NA_real_, xmax_calib = NA_real_))

    if (is.na(current_params$xmax)) {
      p_out$xmax <- max(val, na.rm = TRUE)
      if (use_95) p_out$xmax <- unlist(quantile(val, na.rm = T, probs = 0.975))
    }
    if (is.na(current_params$xmin)) {
      p_out$xmin <- min(val, na.rm = TRUE)
      if (use_95) p_out$xmin <- unlist(quantile(val, na.rm = T, probs = 0.025))
    }
    return(tibble(xmin_calib = p_out$xmin, xmax_calib = p_out$xmax))
  }

  # Calculate params for each group
  calibration_params <- calibration_data_final %>%
    group_by(across(all_of(grouping_vars))) %>%
    summarise(params = list(get_group_params(value, std_params)), .groups = "drop") %>%
    unnest(params)

  # Match column types to prevent dplyr join errors if custom calibration data uses different types (e.g. character vs factor)
  for (gv in grouping_vars) {
    if (gv %in% names(calibration_params) && gv %in% names(data_sub)) {
      if (is.factor(data_sub[[gv]])) {
        calibration_params[[gv]] <- factor(calibration_params[[gv]], levels = levels(data_sub[[gv]]))
      } else if (is.character(data_sub[[gv]])) {
        calibration_params[[gv]] <- as.character(calibration_params[[gv]])
      } else if (is.numeric(data_sub[[gv]])) {
        calibration_params[[gv]] <- as.numeric(calibration_params[[gv]])
      }
    }
  }

  # 3. Join Parameters back to Full Data (all GCMs, all stats)
  # Exclude 'gcm' from the join keys because the calibration range is calculated 
  # from the calibration GCM (e.g. "9") but must be applied to all GCMs (1, 4, 6, etc.).
  join_keys <- setdiff(grouping_vars, "gcm")
  calibration_params_to_join <- calibration_params %>%
    select(-any_of("gcm"))

  data_stding <- data_sub %>%
    left_join(calibration_params_to_join, by = join_keys)

  # 4. Apply Standardization
  # Let's use grouping to ensure constant params for the block passed to std_fun
  data_std <- data_stding %>%
    group_by(across(all_of(grouping_vars))) %>%
    mutate(
      std_value = {
        xmin_g <- first(xmin_calib)
        xmax_g <- first(xmax_calib)

        params_g <- std_params
        params_g$xmin <- xmin_g
        params_g$xmax <- xmax_g

        # Apply function to the vector of values in this group
        do.call(std_fun, c(list(x = value), params_g))
      }
    ) %>%
    ungroup() %>%
    select(-xmin_calib, -xmax_calib)

  return(data_std)
}


get_CU_indicators <- function(data,
                              cu_i,
                              RCP_pick = "45",
                              period_pick = "3",
                              sp_col_name = "SPECIES_NAME",
                              id_col_name = "FULL_CU_IN",
                              indicators_choose = tbl_indicators$abbrev,
                              stats_keep = c("mean", "qlowgcm", "qhighgcm"),
                              use_standardized = TRUE) {
  # Detect Long Format
  is_long <- "indicator" %in% names(data) && "stat" %in% names(data) && "value" %in% names(data)

  if (is_long) {
    # Keep requested scenario OR baseline fallback for static indicators
    data_sub <- data %>%
      filter((rcp == RCP_pick & period_code == period_pick) | (rcp == "0" & period_code == "0")) %>%
      group_by(FULL_CU_IN, indicator, stat) %>%
      mutate(has_projection = any(rcp == RCP_pick & period_code == period_pick)) %>%
      filter(!has_projection | (rcp == RCP_pick & period_code == period_pick)) %>%
      ungroup() %>%
      select(-has_projection)

    # Set value column based on use_standardized
    val_col <- if (use_standardized) "std_value" else "value"
    pivot_prefix <- if (use_standardized) "std_" else ""

    # Filter for chosen indicators and ensemble/aggregate stats
    if ("gcm" %in% names(data_sub)) {
      if ("gcm_name" %in% names(data_sub)) {
        data_sub <- data_sub %>% filter(gcm == "9" | gcm == "0" | (is.na(gcm) & is.na(gcm_name)) | gcm_name == "ensemble")
      } else {
        data_sub <- data_sub %>% filter(gcm == "9" | gcm == "0" | is.na(gcm))
      }
    }

    if (exists("tbl_standardize")) {
      if ("dsmodel" %in% names(data_sub)) {
        data_sub <- data_sub %>%
          left_join(dplyr::select(tbl_standardize, abbrev, dsmodel_baseline_ind = dsmodel_baseline), by = c("indicator" = "abbrev")) %>%
          filter(is.na(dsmodel) | dsmodel == dsmodel_baseline_ind | is.na(dsmodel_baseline_ind)) %>%
          dplyr::select(-dsmodel_baseline_ind)
      }
      if ("std_method" %in% names(data_sub)) {
        data_sub <- data_sub %>%
          left_join(dplyr::select(tbl_standardize, abbrev, std_fun), by = c("indicator" = "abbrev")) %>%
          mutate(
            std_fun_base = case_when(
              std_fun %in% c("linear_std", "invlinear_std") ~ "linear",
              TRUE ~ "exponential"
            )
          ) %>%
          filter(is.na(std_method) | std_method == std_fun_base) %>%
          dplyr::select(-std_fun, -std_fun_base)
      }
    }

    data_sub <- data_sub %>%
      filter(
        indicator %in% indicators_choose,
        stat %in% stats_keep
      ) %>%
      select(all_of(c(id_col_name, sp_col_name, "indicator", "stat", val_col))) %>%
      rename(val = !!sym(val_col))

    sp_pick <- data_sub %>%
      filter(FULL_CU_IN == cu_i) %>%
      pull(sp_col_name) %>%
      unique() %>%
      na.omit() %>%
      as.character()

    if (length(sp_pick) > 1) sp_pick <- sp_pick[1]

    # Calculate species averages
    sp_avgs <- data_sub %>%
      filter(!!sym(sp_col_name) == sp_pick) %>%
      group_by(indicator, stat) %>%
      summarise(val = mean(val, na.rm = TRUE), .groups = "drop") %>%
      rename(sp = val)

    # Calculate all CU averages
    all_cu_avgs <- data_sub %>%
      group_by(indicator, stat) %>%
      summarise(val = mean(val, na.rm = TRUE), .groups = "drop") %>%
      rename(allcu = val)

    # Get CU values
    data_CU <- data_sub %>%
      filter(FULL_CU_IN == cu_i) %>%
      select(indicator, stat, val) %>%
      rename(cu = val)

    # Join all together
    out_data <- data_CU %>%
      left_join(sp_avgs, by = c("indicator", "stat")) %>%
      left_join(all_cu_avgs, by = c("indicator", "stat")) %>%
      mutate(FULL_CU_IN = cu_i, rcp = RCP_pick, period_code = period_pick) %>%
      relocate(FULL_CU_IN, rcp, period_code)

    # Rename and pivot
    out_data <- out_data %>%
      pivot_wider(
        names_from = stat,
        values_from = c("cu", "sp", "allcu"),
        names_glue = "{.value}_{stat}"
      ) %>%
      # Rename 'mean' stats to just 'value' to match plot expectations
      rename_with(~ str_replace(., "_mean$", "_value"), ends_with("_mean"))

    return(out_data)
  }

  data_sub <- filter(
    data,
    rcp == RCP_pick,
    period_code == period_pick
  )

  sp_pick <- data_sub %>%
    filter(FULL_CU_IN == cu_i) %>%
    pull(sp_col_name) %>%
    as.character() %>%
    unique()

  # subset columns for chosen indicators
  cols_sub <- names(data_sub)[str_detect(names(data_sub), paste(indicators_choose, collapse = "|"))]

  # Adjust columns based on use_standardized
  if (use_standardized) {
    cols_sub <- cols_sub[str_detect(cols_sub, "^std_")]
    pivot_prefix <- "std_"
  } else {
    cols_sub <- cols_sub[!str_detect(cols_sub, "^std_")]
    pivot_prefix <- ""
  }

  data_sub <- data_sub %>%
    select(all_of(c(id_col_name, sp_col_name, cols_sub)))

  # Calculate species averages
  sp_avgs <- data_sub %>%
    filter(!!sym(sp_col_name) == sp_pick) %>%
    dplyr::summarize(across(
      all_of(cols_sub),
      \(x) mean(x, na.rm = TRUE)
    ), .groups = "drop") %>%
    pivot_longer(
      cols = all_of(cols_sub),
      values_to = paste0(pivot_prefix, "sp"),
      names_prefix = pivot_prefix,
      names_sep = "_",
      names_to = c("indicator", "stat")
    )

  # Calculate all CU averages
  all_cu_avgs <- data_sub %>%
    dplyr::summarize(across(all_of(cols_sub), \(x) mean(x, na.rm = TRUE)), .groups = "drop") %>%
    pivot_longer(
      cols = all_of(cols_sub),
      values_to = paste0(pivot_prefix, "allcu"),
      names_prefix = pivot_prefix,
      names_sep = "_",
      names_to = c("indicator", "stat")
    ) %>%
    mutate(stat = if_else(is.na(stat), "mean", stat))

  # Get CU values
  data_CU <- data_sub %>%
    filter(FULL_CU_IN == cu_i) %>%
    dplyr::summarize(across(all_of(cols_sub), \(x) mean(x, na.rm = TRUE)), .groups = "drop") %>%
    # select(id_col_name, sp_col_name, all_of(cols_sub)) %>%
    pivot_longer(
      cols = all_of(cols_sub),
      values_to = paste0(pivot_prefix, "cu"),
      names_prefix = pivot_prefix,
      names_sep = "_",
      names_to = c("indicator", "stat")
    )


  # Join all together
  data_CU <- data_CU %>%
    left_join(sp_avgs, join_by(indicator, stat)) %>%
    left_join(all_cu_avgs, join_by(indicator, stat)) %>%
    mutate(stat = if_else(is.na(stat), "mean", stat)) %>%
    mutate(
      FULL_CU_IN = cu_i,
      rcp = RCP_pick,
      period_code = period_pick
    ) %>%
    relocate(FULL_CU_IN, rcp, period_code) %>%
    filter(stat %in% stats_keep) %>%
    pivot_wider(
      names_from = stat,
      values_from = c(
        paste0(pivot_prefix, "cu"),
        paste0(pivot_prefix, "sp"),
        paste0(pivot_prefix, "allcu")
      )
    )

  return(data_CU)
}

# function to convert a numeric score to a rank based on values across CUs, also can rank within groups
rank_scores <- function(data, score_col, group_cols, rank_col = "score_rank", descending = TRUE) {
  data %>%
    group_by(across(all_of(group_cols))) %>%
    mutate(
      !!rank_col := rank(
        !!sym(score_col),
        ties.method = "average",
        na.last = "keep"
      )
    ) %>%
    mutate(
      !!rank_col := if (descending) max(!!sym(rank_col), na.rm = TRUE) + 1 - !!sym(rank_col) else !!sym(rank_col)
    ) %>%
    ungroup()
}




# test <- subset_ind_table(all_flat,
#   indicators_choose = c("Tw8rate", "Tw8proj", "CUstatus"))

# ==================== 3. Portfolio & Combined Score Aggregations ====================
scale_0_100 <- function(x) {
  mn <- suppressWarnings(min(x, na.rm = TRUE))
  mx <- suppressWarnings(max(x, na.rm = TRUE))
  if (!is.finite(mn) || !is.finite(mx)) {
    return(rep(NA_real_, length(x))) # all-NA or non-finite -> no scale
  }
  if (mx <= mn) {
    return(x * 100) # constant values: yield same score for all CUs
  }
  (x - mn) / (mx - mn) * 100
}


hinge_weight <- function(s, t0 = 0.33, t1 = 0.66) {
  ifelse(s <= t0, 0, ifelse(s >= t1, 1, (s - t0) / (t1 - t0)))
}

# Calculate combined/aggregated portfolio scores
calculate_combined_scores <- function(data) {
  # Keep original grouping variables
  orig_group_vars <- group_vars(data)
  
  # Ensure SPECIES_NAME is in the grouping variables if it is in the data but not already grouped
  if ("SPECIES_NAME" %in% names(data) && !("SPECIES_NAME" %in% orig_group_vars)) {
    data <- data %>% group_by(SPECIES_NAME, .add = TRUE)
    orig_group_vars <- group_vars(data)
  }
  
  # Step 1: Calculate category-level scores for each CU/scenario group
  cats_df <- data %>%
    reframe({
      # fwrs
      fwrs_vals <- std_value[category == "fwrs"]
      fwrs_all_na <- all(is.na(fwrs_vals))
      a_fwrs <- if (fwrs_all_na) NA_real_ else mean(fwrs_vals, na.rm = TRUE)
      c_fwrs <- if (fwrs_all_na) NA_real_ else mean(fwrs_vals^3, na.rm = TRUE)^(1 / 3)
      sf_fwrs <- if (fwrs_all_na) NA_real_ else sum(hinge_weight(fwrs_vals), na.rm = TRUE)
      
      # migr
      migr_vals <- std_value[category == "migr"]
      migr_all_na <- all(is.na(migr_vals))
      a_migr <- if (migr_all_na) NA_real_ else mean(migr_vals, na.rm = TRUE)
      c_migr <- if (migr_all_na) NA_real_ else mean(migr_vals^3, na.rm = TRUE)^(1 / 3)
      sf_migr <- if (migr_all_na) NA_real_ else sum(hinge_weight(migr_vals), na.rm = TRUE)
      
      # mar
      mar_vals <- std_value[category == "mar"]
      mar_all_na <- all(is.na(mar_vals))
      a_mar <- if (mar_all_na) NA_real_ else mean(mar_vals, na.rm = TRUE)
      c_mar <- if (mar_all_na) NA_real_ else mean(mar_vals^3, na.rm = TRUE)^(1 / 3)
      sf_mar <- if (mar_all_na) NA_real_ else sum(hinge_weight(mar_vals), na.rm = TRUE)
      
      # dem
      dem_vals <- std_value[category == "dem"]
      dem_all_na <- all(is.na(dem_vals))
      a_dem <- if (dem_all_na) NA_real_ else mean(dem_vals, na.rm = TRUE)
      c_dem <- if (dem_all_na) NA_real_ else mean(dem_vals^3, na.rm = TRUE)^(1 / 3)
      sf_dem <- if (dem_all_na) NA_real_ else sum(hinge_weight(dem_vals), na.rm = TRUE)
      
      # gen
      gen_vals <- std_value[category == "gen"]
      gen_all_na <- all(is.na(gen_vals))
      a_gen <- if (gen_all_na) NA_real_ else mean(gen_vals, na.rm = TRUE)
      c_gen <- if (gen_all_na) NA_real_ else mean(gen_vals^3, na.rm = TRUE)^(1 / 3)
      sf_gen <- if (gen_all_na) NA_real_ else sum(hinge_weight(gen_vals), na.rm = TRUE)
      
      tibble::tibble(
        method = c(rep("avg", 5), rep("cube", 5), rep("flag", 5)),
        category = rep(c("fwrs", "migr", "mar", "dem", "gen"), 3),
        score = c(
          a_fwrs, a_migr, a_mar, a_dem, a_gen,
          c_fwrs, c_migr, c_mar, c_dem, c_gen,
          sf_fwrs, sf_migr, sf_mar, sf_dem, sf_gen
        )
      )
    })
  
  # Step 2: Determine species-level scenario variables to group by
  scenario_vars <- intersect(names(cats_df), c("std_method", "gcm", "rcp", "period_code"))
  species_group_vars <- intersect(c("SPECIES_NAME", "method", "category", scenario_vars), names(cats_df))
  
  cats_ungrouped <- cats_df %>% ungroup()
  
  # Step 3: Calculate species average category scores
  species_avgs <- cats_ungrouped %>%
    group_by(across(all_of(species_group_vars))) %>%
    summarise(
      species_mean = mean(score, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Step 4: Impute missing category scores using species averages
  cats_imputed <- cats_ungrouped %>%
    left_join(species_avgs, by = species_group_vars) %>%
    mutate(
      score = dplyr::coalesce(score, species_mean)
    )
  
  # Step 5: Calculate overall scores from (imputed) category scores
  overall_rows <- cats_imputed %>%
    group_by(across(all_of(orig_group_vars))) %>%
    reframe({
      m_avg <- mean(score[method == "avg"], na.rm = TRUE)
      m_cube <- mean(score[method == "cube"], na.rm = TRUE)
      flag_vals <- score[method == "flag"]
      m_flag <- if (all(is.na(flag_vals))) NA_real_ else sum(flag_vals, na.rm = TRUE)
      
      tibble::tibble(
        method = c("catavg", "avgcube", "flag"),
        category = c("all", "all", "all"),
        score = c(m_avg, m_cube, m_flag)
      )
    })
  
  # Step 6: Combine category and overall rows and format output
  res_final <- bind_rows(
    cats_imputed %>% select(all_of(c(orig_group_vars, "method", "category", "score"))),
    overall_rows
  ) %>%
    ungroup() %>%
    mutate(score = ifelse(is.nan(score), NA_real_, score))
    
  return(res_final)
}

# Shared scoring utility to normalize and rank scores
scale_and_rank_scores <- function(data,
                                  scale_baseline_rcp = NA,
                                  scale_baseline_period = NA,
                                  group_vars = c("std_method", "gcm", "rcp", "period_code", "method", "category"),
                                  within_species = TRUE,
                                  rank_descending = FALSE) {
  # Cross-species scaling (0-100)
  score100_cross <- data
  if (!is.na(scale_baseline_rcp) && !is.na(scale_baseline_period)) {
    baseline_group_vars <- setdiff(group_vars, c("rcp", "period_code"))
    bounds_cross <- score100_cross %>%
      filter(rcp == scale_baseline_rcp, period_code == scale_baseline_period) %>%
      group_by(across(all_of(baseline_group_vars))) %>%
      summarise(
        mn = suppressWarnings(min(score, na.rm = TRUE)),
        mx = suppressWarnings(max(score, na.rm = TRUE)),
        .groups = "drop"
      )
    score100_cross <- score100_cross %>%
      left_join(bounds_cross, by = baseline_group_vars) %>%
      mutate(score100_all = if_else(!is.finite(mn) | !is.finite(mx), NA_real_, if_else(mx <= mn, score * 100, (score - mn) / (mx - mn) * 100))) %>%
      select(-mn, -mx)
  } else {
    score100_cross <- score100_cross %>%
      group_by(across(all_of(group_vars))) %>%
      mutate(score100_all = scale_0_100(score)) %>%
      ungroup()
  }

  # Within-species scaling (0-100)
  if (within_species) {
    score100_within <- data
    within_group_vars <- c("SPECIES_NAME", group_vars)
    if (!is.na(scale_baseline_rcp) && !is.na(scale_baseline_period)) {
      baseline_within_group_vars <- setdiff(within_group_vars, c("rcp", "period_code"))
      bounds_within <- score100_within %>%
        filter(rcp == scale_baseline_rcp, period_code == scale_baseline_period) %>%
        group_by(across(all_of(baseline_within_group_vars))) %>%
        summarise(
          mn = suppressWarnings(min(score, na.rm = TRUE)),
          mx = suppressWarnings(max(score, na.rm = TRUE)),
          .groups = "drop"
        )
      score100_within <- score100_within %>%
        left_join(bounds_within, by = baseline_within_group_vars) %>%
        mutate(score100_species = if_else(!is.finite(mn) | !is.finite(mx), NA_real_, if_else(mx <= mn, score * 100, (score - mn) / (mx - mn) * 100))) %>%
        select(-mn, -mx)
    } else {
      score100_within <- score100_within %>%
        group_by(across(all_of(within_group_vars))) %>%
        mutate(score100_species = scale_0_100(score)) %>%
        ungroup()
    }
  }

  # Cross-species ranks
  ranks_cross <- score100_cross %>%
    group_by(across(all_of(group_vars))) %>%
    mutate(rankall = if (rank_descending) {
      rank(-score, ties.method = "average", na.last = "keep")
    } else {
      rank(score, ties.method = "average", na.last = "keep")
    }) %>%
    ungroup()

  # Within-species ranks
  if (within_species) {
    ranks_within <- score100_within %>%
      group_by(across(all_of(c("SPECIES_NAME", group_vars)))) %>%
      mutate(rankspecies = if (rank_descending) {
        rank(-score, ties.method = "average", na.last = "keep")
      } else {
        rank(score, ties.method = "average", na.last = "keep")
      }) %>%
      ungroup()
  }

  # Assemble and return tidy dataframe
  out <- data %>%
    left_join(
      score100_cross %>% select(any_of(c("FULL_CU_IN", group_vars, "score100_all"))),
      by = c("FULL_CU_IN", intersect(names(data), group_vars))
    )
  if (within_species) {
    out <- out %>%
      left_join(
        score100_within %>% select(any_of(c("FULL_CU_IN", "SPECIES_NAME", group_vars, "score100_species"))),
        by = c("FULL_CU_IN", "SPECIES_NAME", intersect(names(data), group_vars))
      )
  }
  out <- out %>%
    left_join(
      ranks_cross %>% select(any_of(c("FULL_CU_IN", group_vars, "rankall"))),
      by = c("FULL_CU_IN", intersect(names(data), group_vars))
    )
  if (within_species) {
    out <- out %>%
      left_join(
        ranks_within %>% select(any_of(c("FULL_CU_IN", "SPECIES_NAME", group_vars, "rankspecies"))),
        by = c("FULL_CU_IN", "SPECIES_NAME", intersect(names(data), group_vars))
      )
  }
  return(out)
}
