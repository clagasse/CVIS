## 4_scoring_utils.R

# Functions for standardization of indices, scoring and plotting
# as well as reshaping indicator data tables

# 1. Standardization functions --------------------------------------------

# raw standardization function between 0 and 1 using min and max values
# if xmin or xmax are set, a manual min and max range are used for standardizing between 0 and 1
# if use_95 = TRUE, the min and max are set based on 95% quantile ranges to exclude outliers

linear_std <- function(x, ..., xmin = NA, xmax = NA, use_95 = T) {
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
      ifelse(x[i] > xmax, xmax, x[i]))
    y[i] <- (z - xmin) / (xmax - xmin)
    if (xmax == xmin) y[i] <- 0.5 # set standardized value to 0.5 if xmin = xmax
  }

  return(y)
}

# inverse raw standardization function with 1 corresponding to lowest value
invlinear_std <- function(x, ..., xmin = NA, xmax = NA, use_95 = T) {
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
exponential_std <- function(x, ..., lambda = 1, xmin = NA, xmax = NA, use_95 = T) {

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
decay_std <- function(x, ..., lambda = 0.03, xmin = NA, xmax = NA, use_95 = T) {

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

step_std <- function(x, ..., x1 = 200, x2 = 300, x3 = NA, x4 = NA) {
  # Standardize a score into 3 categories
  y <- rep(NA, length(x))

  nstep <- sum(!is.na(c(x1, x2, x3, x4))) + 1

  for (i in 1:length(x)) {
    if (is.na(x[i])) next
    if (x[i] < x1) y[i] <- (1 / nstep)
    if (x[i] >= x1 && x[i] < x2) y[i] <- (2 / nstep)
    if (x[i] >= x2) y[i] <- (3 / nstep)
  }

  return(y)
}


cat_std <- function(x, ..., x1 = "Green", x2 = "Amber/Green", x3 = "Amber", x4 = "Red/Amber", x5 = "Red") {
  # Standardize a score between 0 and 1 based on category values
  y <- rep(NA, length(x))

  for (i in 1:length(x)) {
    if (is.na(x[i])) next
    if (x[i] == x1) y[i] <- 0
    if (x[i] == x2) y[i] <- 0.25
    if (x[i] == x3) y[i] <- 0.5
    if (x[i] == x4) y[i] <- 0.75
    if (x[i] == x5) y[i] <- 1
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


# 2. Data tidying and reshaping -------------------------------------------

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
                             get_raw = T,  # include unstandardized columns
                             get_std = F,  # include standardized columns
                             get_gcm = T,  # include gcm variation
                             get_spat = F, # include spatial variation,
                             get_pop  = F, # include population variation (genetic indicators)
                             rename_cols = T,  # rename id and sp cols
                             gcm_range_suffix = c("qlowgcm", "qhighgcm", "qmingcm", "qmaxgcm"),
                             sp_range_suffix = c("qlowsp", "qhighsp"),
                             pop_suffix = c("popmin", "popmax")
) {
  # take column names that contain prefix with model type
  cols_sub <- names(data)[str_detect(names(data), paste0(indicators_choose, collapse = "|"))]

  # remove raw or std cols if selected
  if (get_raw == F) cols_sub <- cols_sub[str_detect(cols_sub, "std")]
  if (get_std == F) cols_sub <- cols_sub[!str_detect(cols_sub, "std")]

  if (length(indicators_choose) == 1) {
    # take names with prefix that also contain stat suffix
    stat_col <- cols_sub[str_detect(cols_sub, stat_suffix)] # contains "mean" or other suffix
    # some indicators don't have a mean, just the raw value. in that case, use the abbreviation
    if (length(stat_col) == 0) stat_col <- cols_sub
  } else {
    stat_col <- cols_sub[!str_detect(cols_sub, paste0(gcm_range_suffix, collapse = "|"))]  # remove gcm variation columns for now
    stat_col <- stat_col[!str_detect(stat_col, paste0(sp_range_suffix, collapse = "|"))] # remove spat variation columns for now
  }

  cols_out <- c(id_col, sp_col)
  # include rcp and period columns if present
  if (sum(str_detect(names(data), rcp_col)) > 0) cols_out <- c(cols_out, rcp_col)
  if (sum(str_detect(names(data), period_col)) > 0) cols_out <- c(cols_out, period_col)
  cols_out <- c(cols_out, stat_col)

  # get gcm min and max cols
  if (get_gcm == T) {
    gcm_cols   <- cols_sub[str_detect(cols_sub, paste0(gcm_range_suffix, collapse = "|"))]
    cols_out <- c(cols_out, gcm_cols)
  }

  # get spatial min and max cols
  if (get_spat == T) {
    spat_cols <- cols_sub[str_detect(cols_sub, paste0(sp_range_suffix, collapse = "|"))]
    cols_out <- c(cols_out, spat_cols)
  }

  # get spatial min and max cols
  if (get_pop == T) {
    pop_cols <- cols_sub[str_detect(cols_sub, paste0(pop_suffix, collapse = "|"))]
    cols_out <- c(cols_out, pop_cols)
  }

  data_sub <- data %>%
    select(cols_out)

  if (rename_cols == T) data_sub <- rename(data_sub,
    id = !!id_col,  sp = !!sp_col)

  return(data_sub)

}

# utility function to update indicator column names for plotting
library(dplyr)
library(stringr)

rename_ind_table <- function(data,
                             indicator_abbrev,
                             gcm_range_suffix = c("qlowgcm", "qhighgcm", "qmingcm", "qmaxgcm"),
                             sp_range_suffix = c("qlowsp", "qhighsp"),
                             pop_suffix = c("popmin", "popmax"),
                             name_suffix = "",  # optional name suffix
                             single_value_col = FALSE) {
  # Identify relevant columns
  cols_sub <- names(data)[str_detect(names(data), indicator_abbrev)]

  stat_col <- cols_sub[!str_detect(cols_sub, "std")]
  stat_col <- stat_col[!str_detect(stat_col, paste0(gcm_range_suffix, collapse = "|"))]
  stat_col <- stat_col[!str_detect(stat_col, paste0(sp_range_suffix, collapse = "|"))]
  stat_col <- stat_col[!str_detect(stat_col, paste0(pop_suffix, collapse = "|"))]

  std_col <- cols_sub[str_detect(cols_sub, "std")]

  min_gcmcol <- cols_sub[str_detect(cols_sub, gcm_range_suffix[1])]
  max_gcmcol <- cols_sub[str_detect(cols_sub, gcm_range_suffix[2])]

  min_spatcol <- cols_sub[str_detect(cols_sub, sp_range_suffix[1])]
  max_spatcol <- cols_sub[str_detect(cols_sub, sp_range_suffix[2])]

  min_popcol <- cols_sub[str_detect(cols_sub, pop_suffix[1])]
  max_popcol <- cols_sub[str_detect(cols_sub, pop_suffix[2])]

  out_data <- data

  # Rename raw and std columns with suffix
  if (length(stat_col) > 0) {
    out_data <- out_data %>%
      rename_with(~ paste0(name_suffix, "raw"), all_of(stat_col))
  }
  if (length(std_col) > 0) {
    out_data <- out_data %>%
      rename_with(~ paste0(name_suffix, "std"), all_of(std_col))
  }

  # Rename GCM range columns
  if (length(min_gcmcol) > 0) {
    out_data <- out_data %>%
      rename_with(~ paste0(name_suffix, "min_gcm"), all_of(min_gcmcol))
  }
  if (length(max_gcmcol) > 0) {
    out_data <- out_data %>%
      rename_with(~ paste0(name_suffix, "max_gcm"), all_of(max_gcmcol))
  }

  # Rename SP range columns
  if (length(min_spatcol) > 0) {
    out_data <- out_data %>%
      rename_with(~ paste0(name_suffix, "min_spat"), all_of(min_spatcol))
  }
  if (length(max_spatcol) > 0) {
    out_data <- out_data %>%
      rename_with(~ paste0(name_suffix, "max_spat"), all_of(max_spatcol))
  }

  # Rename pop range columns
  if (length(min_popcol) > 0) {
    out_data <- out_data %>%
      rename_with(~ paste0(name_suffix, "min_pop"), all_of(min_popcol))
  }
  if (length(max_popcol) > 0) {
    out_data <- out_data %>%
      rename_with(~ paste0(name_suffix, "max_pop"), all_of(max_popcol))
  }

  # Optionally collapse to single value column
  if (single_value_col) {
    value_col <- if (length(stat_col) > 0) paste0(name_suffix, "raw") else paste0(name_suffix, "std")
    out_data <- out_data %>%
      rename(value = all_of(value_col))
  }

  return(out_data)
}


standardize_indicator <- function(data,
                                  indicator_pick,
                                  std_fun = "linear_std",
                                  std_params = NA,
                                  gcm_range_suffix = c("qlowgcm", "qhighgcm"),
                                  use_gcm_range = FALSE) {
  stat_suffix <- "mean"
  id_col <- "FULL_CU_IN"
  species_col <- "SPECIES_NAME"
  range_type <- std_params$range_type

  # take column names that contain prefix with model type
  cols_sub <- names(data)[str_detect(names(data), indicator_pick)]

  # take names with prefix that also contain stat suffix
  stat_col <- cols_sub[str_detect(cols_sub, paste0(stat_suffix, "$"))] # must end with mean
  min_gcmcol <- cols_sub[str_detect(cols_sub, paste0(gcm_range_suffix[1], collapse = "|"))]
  max_gcmcol <- cols_sub[str_detect(cols_sub, paste0(gcm_range_suffix[2], collapse = "|"))]

  # some indicators don't have a mean, just the raw value. in that case, use the abbreviation
  if (length(stat_col) == 0) stat_col <- cols_sub

  data <- select(data,
    all_of(c(id_col, species_col, stat_col, min_gcmcol, max_gcmcol, "rcp", "period_code")))

  # # Select columns
  # select_cols <- c(id_col, stat_col, min_gcmcol, max_gcmcol, "rcp", "period_code")
  # if (range_type == "species") select_cols <- c(select_cols, species_col)
  # data <- select(data, all_of(select_cols))

  # Build grouping dynamically
  grouping_vars <- c("rcp", "period_code")
  if (range_type == "species") grouping_vars <- c(grouping_vars, species_col)


  if (length(min_gcmcol) == 1 && length(max_gcmcol) == 1 && use_gcm_range == TRUE) {
    # put gcm lows and highs and mean into one column
    data_long <- pivot_longer(data,
      cols = starts_with(indicator_pick),
      names_prefix = paste0(indicator_pick, "_")
    )

    data_std <- data_long %>%
      group_by(across(all_of(grouping_vars))) %>%
      reframe(
        FULL_CU_IN = FULL_CU_IN,
        std = do.call(std_fun, c(list(value), std_params)),
        name = name
      ) %>%
      pivot_wider(
        names_from = name,
        values_from = std,
        names_prefix = paste0(indicator_pick, "_")
      )
  } else {
    data_std <- data %>%
      group_by(across(all_of(grouping_vars))) %>%
      reframe(
        FULL_CU_IN = FULL_CU_IN,
        !!stat_col := do.call(std_fun, c(list(!!sym(stat_col)), std_params))
      )

    if (range_type == "all") {
      data_std <- left_join(data_std,
        select(data, all_of(c(id_col, species_col))),
        by = id_col,
        multiple = "first")
    }


  }

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

  data_sub <- filter(data,
    rcp == RCP_pick,
    period_code == period_pick)

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
    dplyr::summarize(across(all_of(cols_sub),
      \(x) mean(x, na.rm = TRUE)), .groups = "drop") %>%
    pivot_longer(cols = all_of(cols_sub),
      values_to = paste0(pivot_prefix, "sp"),
      names_prefix = pivot_prefix,
      names_sep = "_",
      names_to = c("indicator", "stat"))

  # Calculate all CU averages
  all_cu_avgs <- data_sub %>%
    dplyr::summarize(across(all_of(cols_sub), \(x) mean(x, na.rm = TRUE)), .groups = "drop") %>%
    pivot_longer(cols = all_of(cols_sub),
      values_to = paste0(pivot_prefix, "allcu"),
      names_prefix = pivot_prefix,
      names_sep = "_",
      names_to = c("indicator", "stat")) %>%
    mutate(stat = if_else(is.na(stat), "mean", stat))

  # Get CU values
  data_CU <- data_sub %>%
    filter(FULL_CU_IN == cu_i) %>%
    dplyr::summarize(across(all_of(cols_sub), \(x) mean(x, na.rm = TRUE)), .groups = "drop") %>%
    # select(id_col_name, sp_col_name, all_of(cols_sub)) %>%
    pivot_longer(cols = all_of(cols_sub),
      values_to = paste0(pivot_prefix, "cu"),
      names_prefix = pivot_prefix,
      names_sep = "_",
      names_to = c("indicator", "stat"))


  # Join all together
  data_CU <- data_CU %>%
    left_join(sp_avgs, join_by(indicator, stat)) %>%
    left_join(all_cu_avgs, join_by(indicator, stat)) %>%
    mutate(stat = if_else(is.na(stat), "mean", stat)) %>%
    mutate(FULL_CU_IN = cu_i,
      rcp = RCP_pick,
      period_code = period_pick) %>%
    relocate(FULL_CU_IN, rcp, period_code) %>%
    filter(stat %in% stats_keep) %>%
    pivot_wider(names_from = stat,
      values_from = c(paste0(pivot_prefix, "cu"),
        paste0(pivot_prefix, "sp"),
        paste0(pivot_prefix, "allcu"))
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
