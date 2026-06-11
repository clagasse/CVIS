# ==============================================================================
# CVIS Summary Table Generator (5d_table_summaries.R)
#
# Description:
#   Generates publication-grade HTML summary and sensitivity tables using the gt
#   package. This script defines functions to summarize raw and standardized indicators,
#   overall vulnerability scores, sensitivity analyses across GCMs/methods, and scenario
#   projections compared to the default baseline.
#
# Functions Defined:
#   1. generate_cvis_summary_table():
#      Builds the baseline climate and vulnerability indicators summary table.
#   2. generate_cvis_vulnerability_table():
#      Builds overall vulnerability scores and category-level summaries across CUs.
#   3. generate_cvis_sensitivity_table():
#      Builds overall vulnerability sensitivity tables across GCMs, RCPs, and methods.
#   4. generate_cvis_scenario_raw_summary_table():
#      Builds a scenario summary table showing raw indicator distributions (5%, mean, 95%)
#      across projection periods, RCPs, and downscaling methods.
#   5. generate_cvis_indicator_description_table():
#      Builds a table summarizing descriptions, categories, and ranges of CVIS indicators.
#
# Inputs:
#   - output/scoring_results.Rdata
#   - output/sensitivity_analysis.Rdata
#   - output/indicator_tables.Rdata
#
# Outputs:
#   - gt HTML tables rendered in reports, dashboards, and manuscript generators.
#
# Dependencies:
#   - gt, dplyr, tidyr, purrr, rmarkdown
# ==============================================================================

#' Generate CVIS Climate and Vulnerability Indicators Summary Table
#'
#' @description
#' Builds a publication-grade HTML summary table (`gt` format) of CVIS raw and standardized
#' indicators under the baseline scenario. It summarizes raw value distributions (5%, mean, 95% quantiles)
#' across all CUs, lists raw averages grouped by species/SMU, and maps a specific CU's raw and
#' standardized scores, shaded by risk level.
#'
#' @param all_std_long_baseline Data frame containing baseline standardized indicators at the CU level (stat == "mean").
#' @param cu_code Character string (e.g., "CK-03"). If provided, includes the raw and standardized scores for this specific CU in the table.
#' @param group_by Character. Grouping variable for baseline comparisons. Can be either "species" or "smu". Default is "species".
#' @param categories Character vector. Specific indicator category codes (e.g. c("fwrs", "migr")) to include. Default is NULL (include all).
#' @param title Character. Header title for the gt table. Default is NULL (generates default title).
#' @param subtitle Character. Subtitle for the gt table. Default is NULL (generates default subtitle).
#'
#' @return A `gt_tbl` object.
generate_cvis_summary_table <- function(
  all_std_long_baseline,
  cu_code = NULL,
  group_by = "species",
  categories = NULL,
  title = NULL,
  subtitle = NULL
) {

  # Load indicator metadata if not in active environment
  if (!exists("tbl_indicators") || !exists("tbl_standardize")) {
    load(file.path(here::here(), "output", "indicator_tables.Rdata"))
  }

  # Filter to selected categories if provided
  if (!is.null(categories)) {
    tbl_indicators_filtered <- tbl_indicators %>%
      filter(category %in% categories)
  } else {
    tbl_indicators_filtered <- tbl_indicators
  }

  # 1. Filter to CU-level means to get raw and standardized score distributions across all CUs
  cu_mean_dat <- all_std_long_baseline %>%
    filter(stat == "mean")

  # 2. Summarize raw value distribution across CUs (5% quantile, mean, 95% quantile)
  raw_summary <- cu_mean_dat %>%
    group_by(indicator) %>%
    summarise(
      raw_q05 = quantile(value, 0.05, na.rm = TRUE),
      raw_mean = mean(value, na.rm = TRUE),
      raw_q95 = quantile(value, 0.95, na.rm = TRUE),
      .groups = "drop"
    )

  # 3. Summarize standardized score distribution across CUs (mean only)
  std_summary <- cu_mean_dat %>%
    group_by(indicator) %>%
    summarise(
      std_mean = mean(std_value, na.rm = TRUE),
      .groups = "drop"
    )

  # 4. Summarize group-specific raw means
  if (group_by == "species") {
    group_names <- unique(cu_mean_dat$SPECIES_NAME)
    group_names <- group_names[!is.na(group_names)]
    group_names <- sort(as.character(group_names))

    group_stats <- cu_mean_dat %>%
      group_by(indicator, SPECIES_NAME) %>%
      summarise(
        group_raw_mean = mean(value, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      pivot_wider(
        names_from = SPECIES_NAME,
        values_from = group_raw_mean
      )

    group_std_stats_wide <- cu_mean_dat %>%
      group_by(indicator, SPECIES_NAME) %>%
      summarise(
        group_std_mean = mean(std_value, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(SPECIES_NAME = paste0("std_val_", SPECIES_NAME)) %>%
      pivot_wider(
        names_from = SPECIES_NAME,
        values_from = group_std_mean
      )
  } else {
    group_names <- unique(cu_mean_dat$SMU_SIMPLE)
    group_names <- group_names[!is.na(group_names)]
    group_names <- sort(as.character(group_names))

    group_stats <- cu_mean_dat %>%
      group_by(indicator, SMU_SIMPLE) %>%
      summarise(
        group_raw_mean = mean(value, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      pivot_wider(
        names_from = SMU_SIMPLE,
        values_from = group_raw_mean
      )

    group_std_stats_wide <- cu_mean_dat %>%
      group_by(indicator, SMU_SIMPLE) %>%
      summarise(
        group_std_mean = mean(std_value, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(SMU_SIMPLE = paste0("std_val_", SMU_SIMPLE)) %>%
      pivot_wider(
        names_from = SMU_SIMPLE,
        values_from = group_std_mean
      )
  }

  # 5. Combine with metadata from tbl_indicators
  table_data <- tbl_indicators_filtered %>%
    select(indicator = abbrev, name, category, unit, std_fun) %>%
    left_join(raw_summary, by = "indicator") %>%
    left_join(std_summary, by = "indicator") %>%
    left_join(group_stats, by = "indicator") %>%
    left_join(group_std_stats_wide, by = "indicator")

  # Add CU-specific data if cu_code is provided
  if (!is.null(cu_code)) {
    cu_dat_sub <- cu_mean_dat %>%
      filter(FULL_CU_IN == cu_code) %>%
      select(indicator, cu_raw = value, cu_std = std_value)

    table_data <- table_data %>%
      left_join(cu_dat_sub, by = "indicator")
  }

  # Map category abbreviations to pretty names
  cat_pretty <- c(
    "fwrs" = "Freshwater Spawning & Rearing",
    "migr" = "Upstream Migration",
    "mar" = "Marine",
    "dem" = "Demographics",
    "gen" = "Genetics"
  )

  # Format the standardization function name to make it prettier
  std_fun_pretty <- c(
    "linear_std" = "Linear",
    "invlinear_std" = "Inverse Linear",
    "loglinear_std" = "Log-Linear",
    "invloglinear_std" = "Inverse Log-Linear",
    "step_std" = "Step Threshold",
    "cat_std" = "Categorical Mapping",
    "enh_std" = "Enhanced Mapping"
  )

  table_data <- table_data %>%
    mutate(
      category_pretty = factor(cat_pretty[category], levels = cat_pretty),
      std_method_label = ifelse(is.na(std_fun_pretty[std_fun]), as.character(std_fun), std_fun_pretty[std_fun])
    ) %>%
    select(-category, -std_fun) %>%
    arrange(category_pretty, name)

  # Ensure all group and helper columns exist in table_data
  for (g in group_names) {
    if (!g %in% names(table_data)) {
      table_data[[g]] <- NA_real_
    }
    std_col <- paste0("std_val_", g)
    if (!std_col %in% names(table_data)) {
      table_data[[std_col]] <- NA_real_
    }
  }

  # Select columns in the desired order
  select_cols <- c(
    "indicator", "category_pretty", "name", "unit"
  )
  if (!is.null(cu_code)) {
    select_cols <- c(select_cols, "cu_raw", "cu_std")
  }
  select_cols <- c(
    select_cols,
    "raw_q05", "raw_mean", "raw_q95",
    "std_mean",
    group_names, paste0("std_val_", group_names)
  )

  table_data_select <- table_data %>%
    select(all_of(intersect(select_cols, names(table_data))))

  # 6. Generate gt table
  gt_table <- table_data_select %>%
    gt(groupname_col = "category_pretty") %>%
    tab_header(
      title = title,
      subtitle = subtitle
    ) %>%
    cols_label(
      name = "Indicator Name",
      unit = "Units",
      raw_q05 = "5%",
      raw_mean = "Mean",
      raw_q95 = "95%",
      std_mean = "Mean"
    )

  # Column labels for CU-specific columns
  if (!is.null(cu_code)) {
    gt_table <- gt_table %>%
      cols_label(
        cu_raw = "Raw",
        cu_std = "Standardized"
      )
  }

  # Spanners
  gt_table <- gt_table %>%
    tab_spanner(
      label = "Raw Values (All CUs)",
      columns = c(raw_q05, raw_mean, raw_q95)
    ) %>%
    tab_spanner(
      label = "Standardized Score (All CUs)",
      columns = c(std_mean)
    )

  if (!is.null(cu_code)) {
    gt_table <- gt_table %>%
      tab_spanner(
        label = paste("Selected CU:", cu_code),
        columns = c(cu_raw, cu_std)
      )
  }

  group_spanner_label <- if (group_by == "species") "Raw Mean by Species" else "Raw Mean by SMU"
  gt_table <- gt_table %>%
    tab_spanner(
      label = group_spanner_label,
      columns = all_of(group_names)
    )

  # Number formatting with column-expression row filters for demographics
  gt_table <- gt_table %>%
    fmt_number(
      columns = c(raw_q05, raw_mean, raw_q95, std_mean, all_of(group_names)),
      rows = !indicator %in% c("CUstatus", "CUnmat"),
      decimals = 2
    ) %>%
    fmt_integer(
      columns = c(raw_q05, raw_mean, raw_q95, all_of(group_names)),
      rows = indicator %in% c("CUstatus", "CUnmat")
    ) %>%
    fmt_number(
      columns = c(std_mean),
      rows = indicator %in% c("CUstatus", "CUnmat"),
      decimals = 2
    )

  if (!is.null(cu_code)) {
    gt_table <- gt_table %>%
      fmt_number(
        columns = c(cu_raw),
        rows = !indicator %in% c("CUstatus", "CUnmat"),
        decimals = 2
      ) %>%
      fmt_integer(
        columns = c(cu_raw),
        rows = indicator %in% c("CUstatus", "CUnmat")
      ) %>%
      fmt_number(
        columns = c(cu_std),
        decimals = 2
      )
  }

  gt_table <- gt_table %>%
    sub_missing(
      columns = everything(),
      missing_text = "-"
    ) %>%
    cols_align(
      align = "center",
      columns = intersect(c("unit", "std_method_label", "cu_raw", "cu_std"), names(table_data_select))
    ) %>%
    cols_align(
      align = "left",
      columns = name
    ) %>%
    # Hide indicator and helper standardized columns used for coloring
    cols_hide(columns = c(indicator, starts_with("std_val_"))) %>%
    # Styling
    opt_table_font(
      font = list(
        "Inter",
        "Helvetica Neue", "Arial", "sans-serif"
      )
    ) %>%
    tab_style(
      style = cell_text(weight = "bold", size = px(15), color = "#1A365D"),
      locations = cells_title(groups = "title")
    ) %>%
    tab_style(
      style = cell_text(size = px(11), style = "italic", color = "#4A5568"),
      locations = cells_title(groups = "subtitle")
    ) %>%
    tab_style(
      style = list(
        cell_fill(color = "#EBF8FF"),
        cell_text(color = "#2B6CB0", weight = "bold", size = px(11))
      ),
      locations = cells_row_groups()
    ) %>%
    tab_style(
      style = cell_borders(sides = "right", color = "#CBD5E0", weight = px(1.5)),
      locations = cells_body(columns = unit)
    ) %>%
    tab_style(
      style = cell_borders(sides = "right", color = "#CBD5E0", weight = px(1.5)),
      locations = cells_column_labels(columns = unit)
    ) %>%
    tab_style(
      style = cell_borders(sides = "right", color = "#CBD5E0", weight = px(1.5)),
      locations = cells_body(columns = std_mean)
    ) %>%
    tab_style(
      style = cell_borders(sides = "right", color = "#CBD5E0", weight = px(1.5)),
      locations = cells_column_labels(columns = std_mean)
    )

  if (!is.null(cu_code)) {
    gt_table <- gt_table %>%
      tab_style(
        style = cell_borders(sides = "right", color = "#CBD5E0", weight = px(1.5)),
        locations = cells_body(columns = cu_std)
      ) %>%
      tab_style(
        style = cell_borders(sides = "right", color = "#CBD5E0", weight = px(1.5)),
        locations = cells_column_labels(columns = cu_std)
      ) %>%
      # Highlight the Selected CU headers
      tab_style(
        style = list(
          cell_fill(color = "#EBF8FF"),
          cell_text(color = "#2B6CB0", weight = "bold")
        ),
        locations = list(
          cells_column_labels(columns = c(cu_raw, cu_std)),
          cells_column_spanners(spanners = paste("Selected CU:", cu_code))
        )
      )
  }

  gt_table <- gt_table %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(columns = intersect(c("raw_mean", "std_mean", "cu_raw", "cu_std"), names(table_data_select)))
    ) %>%
    tab_style(
      style = cell_text(weight = "bold", size = px(11), color = "#2D3748"),
      locations = cells_column_spanners(spanners = everything())
    ) %>%
    tab_style(
      style = cell_borders(sides = "bottom", color = "#E2E8F0", weight = px(1)),
      locations = cells_body()
    ) %>%
    opt_row_striping() %>%
    tab_options(
      table.font.size = 10,
      heading.title.font.size = 13,
      heading.subtitle.font.size = 11,
      row_group.font.size = 11,
      row_group.font.weight = "bold",
      column_labels.font.weight = "bold",
      column_labels.background.color = "#F7FAFC",
      row.striping.background_color = "#F8FAFC",
      table.border.top.color = "#1A365D",
      table.border.top.width = px(2),
      table.border.bottom.color = "#1A365D",
      table.border.bottom.width = px(2),
      column_labels.border.bottom.color = "#A0AEC0",
      column_labels.border.bottom.width = px(1.5),
      table.width = pct(100),
      data_row.padding = px(6)
    )

  # Color scale function
  color_fun <- scales::col_numeric(
    palette = c("#E8F3FF", "#FFFDE6", "#FFEAEA"),
    domain = c(0, 1),
    na.color = "#FFFFFF"
  )

  # Apply coloring to group columns
  for (g in group_names) {
    std_col <- paste0("std_val_", g)
    for (i in seq_len(nrow(table_data))) {
      val <- table_data[[std_col]][i]
      if (!is.na(val)) {
        bg_color <- color_fun(val)
        gt_table <- gt_table %>%
          tab_style(
            style = cell_fill(color = bg_color),
            locations = cells_body(columns = all_of(g), rows = i)
          )
      }
    }
  }

  # Apply coloring to cu_raw and cu_std if cu_code is provided
  if (!is.null(cu_code)) {
    for (i in seq_len(nrow(table_data))) {
      val <- table_data$cu_std[i]
      if (!is.na(val)) {
        bg_color <- color_fun(val)
        gt_table <- gt_table %>%
          tab_style(
            style = cell_fill(color = bg_color),
            locations = cells_body(columns = c(cu_raw, cu_std), rows = i)
          )
      }
    }
  }

  return(gt_table)
}
#' Generate CVIS Overall Vulnerability Score Summary Table
#'
#' @description
#' Creates a summary table (`gt` format) displaying overall vulnerability scores, ranks, and 
#' category-level scores (demographics, spawning & rearing, upstream migration, nearshore marine, 
#' genetics) across all CUs. Bold-highlights a selected CU and places it on the first row for comparison.
#'
#' @param scores_tidy Data frame containing overall and category vulnerability scores for all sensitivity runs.
#' @param scores_tidy_baseline Data frame containing overall and category vulnerability scores for the baseline scenario.
#' @param cu_code Character string (e.g., "CK-03"). If provided, puts this CU's row first and bold-highlights it. Default is NULL.
#' @param title Character. Header title for the gt table. Default is NULL.
#' @param subtitle Character. Subtitle for the gt table. Default is NULL.
#'
#' @return A `gt_tbl` object.
generate_cvis_vulnerability_table <- function(
  scores_tidy,
  scores_tidy_baseline,
  cu_code = NULL,
  title = NULL,
  subtitle = NULL
) {

  # 1. Reshape category scores
  cat_scores <- scores_tidy_baseline %>%
    filter(method == "avg", category %in% c("dem", "fwrs", "gen", "mar", "migr")) %>%
    select(FULL_CU_IN, SPECIES_NAME, CVIS_LABEL, CU_COMMON_NAME, SMU_SIMPLE, category, score100_all) %>%
    pivot_wider(
      names_from = category,
      values_from = score100_all
    )

  # 2. Extract baseline overall vulnerability score and ranks
  overall_scores <- scores_tidy_baseline %>%
    filter(method == "catavg", category == "all") %>%
    select(FULL_CU_IN, overall = score100_all, rankall, rankspecies)

  # 3. Extract overall vulnerability scores for specific GCMs 1, 4, and 6
  gcm_scores <- scores_tidy %>%
    filter(std_method == std_method_base, rcp == "45", period_code == "3", category == "all", method == "catavg") %>%
    filter(gcm %in% c("1", "4", "6")) %>%
    select(FULL_CU_IN, gcm, score100_all) %>%
    mutate(gcm = paste0("gcm", gcm)) %>%
    pivot_wider(
      names_from = gcm,
      values_from = score100_all
    )

  # 4. Extract different scoring methods for overall vulnerability (ensemble GCM 9)
  method_scores <- scores_tidy %>%
    filter(std_method == std_method_base, rcp == "45", period_code == "3", category == "all", gcm == "9") %>%
    filter(method %in% c("flag", "avgcube")) %>%
    select(FULL_CU_IN, method, score100_all) %>%
    pivot_wider(
      names_from = method,
      values_from = score100_all
    )

  # 5. Join all together
  vuln_table_data <- cat_scores %>%
    left_join(overall_scores, by = "FULL_CU_IN") %>%
    left_join(gcm_scores, by = "FULL_CU_IN") %>%
    left_join(method_scores, by = "FULL_CU_IN") %>%
    mutate(
      species_smu = paste0(SPECIES_NAME, " - ", SMU_SIMPLE)
    )

  # 6. If cu_code is provided, put the selected CU in the first row
  if (!is.null(cu_code)) {
    vuln_table_data <- vuln_table_data %>%
      arrange(desc(FULL_CU_IN == cu_code), SPECIES_NAME, SMU_SIMPLE, FULL_CU_IN)
  } else {
    vuln_table_data <- vuln_table_data %>%
      arrange(SPECIES_NAME, SMU_SIMPLE, FULL_CU_IN)
  }

  # Round columns to integer
  round_cols <- c("dem", "fwrs", "gen", "mar", "migr", "overall", "rankall", "rankspecies", "gcm1", "gcm4", "gcm6", "flag", "avgcube")
  for (col in round_cols) {
    if (col %in% names(vuln_table_data)) {
      vuln_table_data[[col]] <- round(vuln_table_data[[col]], 0)
    }
  }

  # Select and order columns
  vuln_table_data <- vuln_table_data %>%
    select(species_smu, FULL_CU_IN, CU_COMMON_NAME, 
           overall, rankall, rankspecies, 
           dem, fwrs, gen, mar, migr, 
           flag, avgcube,
           gcm1, gcm4, gcm6)

  # Set default titles
  if (is.null(title)) {
    title <- "Conservation Unit Climate Vulnerability Scores and Ranks"
  }
  if (is.null(subtitle)) {
    subtitle <- "Relative ranks, category vulnerability, specific GCM projections, and scoring methodology comparisons"
  }

  # 7. Build gt table
  if (!is.null(cu_code)) {
    gt_vuln_table <- vuln_table_data %>%
      gt() %>%
      cols_label(
        species_smu = "Species - SMU"
      )
  } else {
    gt_vuln_table <- vuln_table_data %>%
      gt(groupname_col = "species_smu")
  }

  gt_vuln_table <- gt_vuln_table %>%
    tab_header(
      title = title,
      subtitle = subtitle
    ) %>%
    cols_label(
      FULL_CU_IN = "CU Code",
      CU_COMMON_NAME = "CU Name",
      overall = "Overall",
      rankall = "All CUs",
      rankspecies = "Within Species",
      dem = "Demographics",
      fwrs = "Freshwater",
      gen = "Genetics",
      mar = "Marine",
      migr = "Migration",
      flag = "Red Flag",
      avgcube = "Avg Cube",
      gcm1 = "CanESM2",
      gcm4 = "HadGEM2",
      gcm6 = "MPI"
    ) %>%
    tab_spanner(
      label = "Relative Ranks",
      columns = c(rankall, rankspecies)
    ) %>%
    tab_spanner(
      label = "Category Scores (0-100)",
      columns = c(dem, fwrs, gen, mar, migr)
    ) %>%
    tab_spanner(
      label = "Sensitivity Analysis",
      columns = c(flag, avgcube, gcm1, gcm4, gcm6)
    ) %>%
    fmt_integer(
      columns = all_of(round_cols)
    ) %>%
    sub_missing(
      columns = everything(),
      missing_text = "-"
    ) %>%
    cols_align(
      align = "center",
      columns = intersect(c("species_smu", "FULL_CU_IN", "overall", "rankall", "rankspecies", "dem", "fwrs", "gen", "mar", "migr", "flag", "avgcube", "gcm1", "gcm4", "gcm6"), names(vuln_table_data))
    ) %>%
    cols_align(
      align = "left",
      columns = CU_COMMON_NAME
    ) %>%
    opt_table_font(
      font = list(
        "Inter",
        "Helvetica Neue", "Arial", "sans-serif"
      )
    ) %>%
    tab_style(
      style = cell_text(weight = "bold", size = px(15), color = "#1A365D"),
      locations = cells_title(groups = "title")
    ) %>%
    tab_style(
      style = cell_text(size = px(11), style = "italic", color = "#4A5568"),
      locations = cells_title(groups = "subtitle")
    )

  if (is.null(cu_code)) {
    gt_vuln_table <- gt_vuln_table %>%
      tab_style(
        style = list(
          cell_fill(color = "#EBF8FF"),
          cell_text(color = "#2B6CB0", weight = "bold", size = px(11))
        ),
        locations = cells_row_groups()
      )
  }

  gt_vuln_table <- gt_vuln_table %>%
    tab_style(
      style = cell_borders(sides = "right", color = "#CBD5E0", weight = px(1.5)),
      locations = cells_body(columns = CU_COMMON_NAME)
    ) %>%
    tab_style(
      style = cell_borders(sides = "right", color = "#CBD5E0", weight = px(1.5)),
      locations = cells_column_labels(columns = CU_COMMON_NAME)
    ) %>%
    tab_style(
      style = cell_borders(sides = "right", color = "#CBD5E0", weight = px(1.5)),
      locations = cells_body(columns = rankspecies)
    ) %>%
    tab_style(
      style = cell_borders(sides = "right", color = "#CBD5E0", weight = px(1.5)),
      locations = cells_column_labels(columns = rankspecies)
    ) %>%
    tab_style(
      style = cell_borders(sides = "right", color = "#CBD5E0", weight = px(1.5)),
      locations = cells_body(columns = migr)
    ) %>%
    tab_style(
      style = cell_borders(sides = "right", color = "#CBD5E0", weight = px(1.5)),
      locations = cells_column_labels(columns = migr)
    ) %>%
    tab_style(
      style = cell_borders(sides = "right", color = "#CBD5E0", weight = px(1.5)),
      locations = cells_body(columns = overall)
    ) %>%
    tab_style(
      style = cell_borders(sides = "right", color = "#CBD5E0", weight = px(1.5)),
      locations = cells_column_labels(columns = overall)
    ) %>%
    tab_style(
      style = cell_borders(sides = "right", color = "#CBD5E0", weight = px(1.5)),
      locations = cells_body(columns = avgcube)
    ) %>%
    tab_style(
      style = cell_borders(sides = "right", color = "#CBD5E0", weight = px(1.5)),
      locations = cells_column_labels(columns = avgcube)
    )

  if (!is.null(cu_code)) {
    gt_vuln_table <- gt_vuln_table %>%
      tab_style(
        style = cell_borders(sides = "right", color = "#CBD5E0", weight = px(1.5)),
        locations = cells_body(columns = species_smu)
      ) %>%
      tab_style(
        style = cell_borders(sides = "right", color = "#CBD5E0", weight = px(1.5)),
        locations = cells_column_labels(columns = species_smu)
      )
  }

  gt_vuln_table <- gt_vuln_table %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(columns = overall)
    ) %>%
    tab_style(
      style = cell_text(weight = "bold", size = px(11), color = "#2D3748"),
      locations = cells_column_spanners(spanners = everything())
    ) %>%
    tab_style(
      style = cell_borders(sides = "bottom", color = "#E2E8F0", weight = px(1)),
      locations = cells_body()
    ) %>%
    opt_row_striping() %>%
    tab_options(
      table.font.size = 10,
      heading.title.font.size = 13,
      heading.subtitle.font.size = 11,
      row_group.font.size = 11,
      row_group.font.weight = "bold",
      column_labels.font.weight = "bold",
      column_labels.background.color = "#F7FAFC",
      row.striping.background_color = "#F8FAFC",
      table.border.top.color = "#1A365D",
      table.border.top.width = px(2),
      table.border.bottom.color = "#1A365D",
      table.border.bottom.width = px(2),
      column_labels.border.bottom.color = "#A0AEC0",
      column_labels.border.bottom.width = px(1.5),
      table.width = pct(100),
      data_row.padding = px(6)
    )

  # Color scale function for vulnerability (0-100 domain)
  color_fun_vuln <- scales::col_numeric(
    palette = c("#E8F3FF", "#FFFDE6", "#FFEAEA"),
    domain = c(0, 100),
    na.color = "#FFFFFF"
  )

  # Color scale function for rankall (1-50 domain)
  color_fun_rank <- scales::col_numeric(
    palette = c("#E8F3FF", "#FFFDE6", "#FFEAEA"),
    domain = c(1, 50),
    na.color = "#FFFFFF"
  )

  # Apply cell background colors to vulnerability score columns
  score_cols <- c("dem", "fwrs", "gen", "mar", "migr", "overall", "flag", "avgcube", "gcm1", "gcm4", "gcm6")
  for (col in score_cols) {
    for (i in seq_len(nrow(vuln_table_data))) {
      val <- vuln_table_data[[col]][i]
      if (!is.na(val)) {
        bg_color <- color_fun_vuln(val)
        gt_vuln_table <- gt_vuln_table %>%
          tab_style(
            style = cell_fill(color = bg_color),
            locations = cells_body(columns = all_of(col), rows = i)
          )
      }
    }
  }

  # Apply cell background colors to rankall column
  for (i in seq_len(nrow(vuln_table_data))) {
    val <- vuln_table_data$rankall[i]
    if (!is.na(val)) {
      bg_color <- color_fun_rank(val)
      gt_vuln_table <- gt_vuln_table %>%
        tab_style(
          style = cell_fill(color = bg_color),
          locations = cells_body(columns = rankall, rows = i)
        )
    }
  }

  # Style row 1 (selected CU) as bolded and larger, keeping individual column colors
  if (!is.null(cu_code)) {
    gt_vuln_table <- gt_vuln_table %>%
      # Bold and larger text for row 1
      tab_style(
        style = cell_text(weight = "bold", size = px(12)),
        locations = cells_body(rows = 1)
      ) %>%
      # Highlight the label columns (which don't have cell colors)
      tab_style(
        style = cell_fill(color = "#EBF8FF"),
        locations = cells_body(columns = c(species_smu, FULL_CU_IN, CU_COMMON_NAME), rows = 1)
      ) %>%
      # Add a distinct border around the selected CU row to frame it
      tab_style(
        style = cell_borders(sides = c("top", "bottom"), color = "#1A365D", weight = px(2)),
        locations = cells_body(rows = 1)
      )
  }

  return(gt_vuln_table)
}
#' Generate CVIS Overall Vulnerability Score Sensitivity Analysis Table
#'
#' @description
#' Compiles overall vulnerability scores and ranks across alternative GCMs, emission/climate projection 
#' scenarios, downscaling models, and scoring/standardization methods. Shades cells by risk level and 
#' bold-highlights a selected CU.
#'
#' @param overall_sensitivity List containing consolidated sensitivity analysis outputs (e.g., deviations, indicator metrics).
#' @param cu_code Character string (e.g., "CK-03"). If provided, bold-highlights this CU's row and puts it first. Default is NULL.
#' @param title Character. Header title for the gt table. Default is NULL.
#' @param subtitle Character. Subtitle for the gt table. Default is NULL.
#'
#' @return A `gt_tbl` object.
generate_cvis_sensitivity_table <- function(
  overall_sensitivity,
  cu_code = NULL,
  title = NULL,
  subtitle = NULL
) {

  # 1. Filter to overall vulnerability (category = "all")
  sens_data <- overall_sensitivity$deviations %>%
    filter(category == "all")

  # 2. Re-arrange rows so selected CU is first, if provided
  if (!is.null(cu_code)) {
    sens_data <- sens_data %>%
      arrange(desc(FULL_CU_IN == cu_code), SPECIES_NAME, SMU_SIMPLE, FULL_CU_IN)
  } else {
    sens_data <- sens_data %>%
      arrange(SPECIES_NAME, SMU_SIMPLE, FULL_CU_IN)
  }

  # 3. Compute scores and ranks for each scenario, rounding to integer
  table_data <- sens_data %>%
    transmute(
      species_smu = paste0(SPECIES_NAME, " - ", SMU_SIMPLE),
      FULL_CU_IN,
      CU_COMMON_NAME = CVIS_LABEL,
      
      score_base = round(base_score, 0),
      rank_base = round(base_rank_all, 0),
      
      score_gcm1 = round(base_score + raw_dev_GCM1, 0),
      rank_gcm1 = round(rankall_GCM1, 0),
      
      score_gcm4 = round(base_score + raw_dev_GCM4, 0),
      rank_gcm4 = round(rankall_GCM4, 0),
      
      score_gcm6 = round(base_score + raw_dev_GCM6, 0),
      rank_gcm6 = round(rankall_GCM6, 0),
      
      score_ds = round(base_score + raw_dev_dsmethod, 0),
      rank_ds = round(rankall_dsmethod, 0),
      
      score_std = round(base_score + raw_dev_stdmethod, 0),
      rank_std = round(rankall_stdmethod, 0),
      
      score_flag = round(base_score + raw_dev_Method_flag, 0),
      rank_flag = round(rankall_Method_flag, 0),
      
      score_avgcube = round(base_score + raw_dev_Method_avgcube, 0),
      rank_avgcube = round(rankall_Method_avgcube, 0)
    )

  if (is.null(title)) {
    title <- "CVIS Overall Vulnerability Sensitivity Analysis"
  }
  if (is.null(subtitle)) {
    subtitle <- "Overall vulnerability scores and ranks across alternative climate models, downscaling methods, and scoring methods"
  }

  # 4. Generate gt table
  if (!is.null(cu_code)) {
    gt_sens_table <- table_data %>%
      gt() %>%
      cols_label(
        species_smu = "Species - SMU"
      )
  } else {
    gt_sens_table <- table_data %>%
      gt(groupname_col = "species_smu")
  }

  gt_sens_table <- gt_sens_table %>%
    tab_header(
      title = title,
      subtitle = subtitle
    ) %>%
    cols_label(
      FULL_CU_IN = "CU Code",
      CU_COMMON_NAME = "CU Name",
      
      score_base = "Score", rank_base = "Rank",
      score_gcm1 = "Score", rank_gcm1 = "Rank",
      score_gcm4 = "Score", rank_gcm4 = "Rank",
      score_gcm6 = "Score", rank_gcm6 = "Rank",
      score_ds = "Score", rank_ds = "Rank",
      score_std = "Score", rank_std = "Rank",
      score_flag = "Score", rank_flag = "Rank",
      score_avgcube = "Score", rank_avgcube = "Rank"
    ) %>%
    tab_spanner(
      label = "Baseline",
      columns = c(score_base, rank_base)
    ) %>%
    tab_spanner(
      label = "GCM 1",
      columns = c(score_gcm1, rank_gcm1)
    ) %>%
    tab_spanner(
      label = "GCM 4",
      columns = c(score_gcm4, rank_gcm4)
    ) %>%
    tab_spanner(
      label = "GCM 6",
      columns = c(score_gcm6, rank_gcm6)
    ) %>%
    tab_spanner(
      label = "Alt Downscaling Method",
      columns = c(score_ds, rank_ds)
    ) %>%
    tab_spanner(
      label = "Linear Std",
      columns = c(score_std, rank_std)
    ) %>%
    tab_spanner(
      label = "Red Flag Method",
      columns = c(score_flag, rank_flag)
    ) %>%
    tab_spanner(
      label = "Avg Cube Method",
      columns = c(score_avgcube, rank_avgcube)
    )

  # Set column alignments
  gt_sens_table <- gt_sens_table %>%
    sub_missing(
      columns = everything(),
      missing_text = "-"
    ) %>%
    cols_align(
      align = "center",
      columns = intersect(c("species_smu", "FULL_CU_IN", "score_base", "rank_base", "score_gcm1", "rank_gcm1", "score_gcm4", "rank_gcm4", "score_gcm6", "rank_gcm6", "score_ds", "rank_ds", "score_std", "rank_std", "score_flag", "rank_flag", "score_avgcube", "rank_avgcube"), names(table_data))
    ) %>%
    cols_align(
      align = "left",
      columns = CU_COMMON_NAME
    )

  # Formatting numbers
  numeric_cols <- c(
    "score_base", "rank_base",
    "score_gcm1", "rank_gcm1",
    "score_gcm4", "rank_gcm4",
    "score_gcm6", "rank_gcm6",
    "score_ds", "rank_ds",
    "score_std", "rank_std",
    "score_flag", "rank_flag",
    "score_avgcube", "rank_avgcube"
  )
  gt_sens_table <- gt_sens_table %>%
    fmt_integer(
      columns = all_of(numeric_cols)
    )

  # Basic styling
  gt_sens_table <- gt_sens_table %>%
    opt_table_font(
      font = list(
        "Inter",
        "Helvetica Neue", "Arial", "sans-serif"
      )
    ) %>%
    tab_style(
      style = cell_text(weight = "bold", size = px(15), color = "#1A365D"),
      locations = cells_title(groups = "title")
    ) %>%
    tab_style(
      style = cell_text(size = px(11), style = "italic", color = "#4A5568"),
      locations = cells_title(groups = "subtitle")
    )

  if (is.null(cu_code)) {
    gt_sens_table <- gt_sens_table %>%
      tab_style(
        style = list(
          cell_fill(color = "#EBF8FF"),
          cell_text(color = "#2B6CB0", weight = "bold", size = px(11))
        ),
        locations = cells_row_groups()
      )
  }

  # Add borders to separate spanners
  gt_sens_table <- gt_sens_table %>%
    tab_style(
      style = cell_borders(sides = "right", color = "#CBD5E0", weight = px(1.5)),
      locations = cells_body(columns = CU_COMMON_NAME)
    ) %>%
    tab_style(
      style = cell_borders(sides = "right", color = "#CBD5E0", weight = px(1.5)),
      locations = cells_column_labels(columns = CU_COMMON_NAME)
    )
  
  spanner_rights <- c("rank_base", "rank_gcm1", "rank_gcm4", "rank_gcm6", "rank_ds", "rank_std", "rank_flag", "rank_avgcube")
  for (col in spanner_rights) {
    gt_sens_table <- gt_sens_table %>%
      tab_style(
        style = cell_borders(sides = "right", color = "#CBD5E0", weight = px(1.5)),
        locations = cells_body(columns = all_of(col))
      ) %>%
      tab_style(
        style = cell_borders(sides = "right", color = "#CBD5E0", weight = px(1.5)),
        locations = cells_column_labels(columns = all_of(col))
      )
  }

  if (!is.null(cu_code)) {
    gt_sens_table <- gt_sens_table %>%
      tab_style(
        style = cell_borders(sides = "right", color = "#CBD5E0", weight = px(1.5)),
        locations = cells_body(columns = species_smu)
      ) %>%
      tab_style(
        style = cell_borders(sides = "right", color = "#CBD5E0", weight = px(1.5)),
        locations = cells_column_labels(columns = species_smu)
      )
  }

  gt_sens_table <- gt_sens_table %>%
    tab_style(
      style = cell_borders(sides = "bottom", color = "#E2E8F0", weight = px(1)),
      locations = cells_body()
    ) %>%
    opt_row_striping() %>%
    tab_options(
      table.font.size = 10,
      heading.title.font.size = 13,
      heading.subtitle.font.size = 11,
      row_group.font.size = 11,
      row_group.font.weight = "bold",
      column_labels.font.weight = "bold",
      column_labels.background.color = "#F7FAFC",
      row.striping.background_color = "#F8FAFC",
      table.border.top.color = "#1A365D",
      table.border.top.width = px(2),
      table.border.bottom.color = "#1A365D",
      table.border.bottom.width = px(2),
      column_labels.border.bottom.color = "#A0AEC0",
      column_labels.border.bottom.width = px(1.5),
      table.width = pct(100),
      data_row.padding = px(6)
    )

  # Color scale functions for scores (0-100) and ranks (1-50)
  color_fun_score <- scales::col_numeric(
    palette = c("#E8F3FF", "#FFFDE6", "#FFEAEA"),
    domain = c(0, 100),
    na.color = "#FFFFFF"
  )
  color_fun_rank <- scales::col_numeric(
    palette = c("#E8F3FF", "#FFFDE6", "#FFEAEA"),
    domain = c(1, 50),
    na.color = "#FFFFFF"
  )

  # Apply colors to scores and ranks
  score_cols <- c("score_base", "score_gcm1", "score_gcm4", "score_gcm6", "score_ds", "score_std", "score_flag", "score_avgcube")
  rank_cols <- c("rank_base", "rank_gcm1", "rank_gcm4", "rank_gcm6", "rank_ds", "rank_std", "rank_flag", "rank_avgcube")

  for (col in score_cols) {
    for (i in seq_len(nrow(table_data))) {
      val <- table_data[[col]][i]
      if (!is.na(val)) {
        bg_color <- color_fun_score(val)
        gt_sens_table <- gt_sens_table %>%
          tab_style(
            style = cell_fill(color = bg_color),
            locations = cells_body(columns = all_of(col), rows = i)
          )
      }
    }
  }

  for (col in rank_cols) {
    for (i in seq_len(nrow(table_data))) {
      val <- table_data[[col]][i]
      if (!is.na(val)) {
        bg_color <- color_fun_rank(val)
        gt_sens_table <- gt_sens_table %>%
          tab_style(
            style = cell_fill(color = bg_color),
            locations = cells_body(columns = all_of(col), rows = i)
          )
      }
    }
  }

  # Bold/larger highlights for Row 1 (selected CU)
  if (!is.null(cu_code)) {
    gt_sens_table <- gt_sens_table %>%
      tab_style(
        style = cell_text(weight = "bold", size = px(12)),
        locations = cells_body(rows = 1)
      ) %>%
      tab_style(
        style = cell_fill(color = "#EBF8FF"),
        locations = cells_body(columns = c(species_smu, FULL_CU_IN, CU_COMMON_NAME), rows = 1)
      ) %>%
      tab_style(
        style = cell_borders(sides = c("top", "bottom"), color = "#1A365D", weight = px(2)),
        locations = cells_body(rows = 1)
      )
  }

  return(gt_sens_table)
}
#' Generate CVIS Single-CU Indicator & Overall Sensitivity Profile Table
#'
#' @description
#' Creates a detailed sensitivity profile table (`gt` format) for a specific CU. Displays the 
#' single CU's baseline raw value and compares its standardized risk scores (0-100 scale) for each individual 
#' indicator and overall vulnerability across alternative GCMs, downscalers, and scoring methods.
#'
#' @param overall_sensitivity List containing consolidated sensitivity analysis outputs.
#' @param cu_code Character string (e.g., "CK-03"). The specific CU code to extract the profile for. Required.
#' @param title Character. Header title for the gt table. Default is NULL.
#' @param subtitle Character. Subtitle for the gt table. Default is NULL.
#'
#' @return A `gt_tbl` object.
#' Generate CVIS Scenario and Sensitivity Raw Indicator Value Table
#'
#' @description
#' Summarizes the 5%, mean, and 95% raw indicator values across all CUs for projection periods, 
#' emission pathways (RCPs), and downscaling methods. Compares alternative scenarios (RCP 4.5 vs 8.5, 
#' 2040s vs 2080s) and downscaling techniques directly to the default CVIS baseline scenario, which 
#' is highlighted in the table for reference.
#'
#' @param all_std_long Data frame containing standardized and raw indicator values across all scenarios, periods, GCMs, and downscaling models.
#' @param title Character. Header title for the gt table. Default is NULL.
#' @param subtitle Character. Subtitle for the gt table. Default is NULL.
#'
#' @return A `gt_tbl` object.
generate_cvis_scenario_raw_summary_table <- function(
  all_std_long,
  title = NULL,
  subtitle = NULL
) {
  # Load indicator metadata if not in active environment
  if (!exists("tbl_indicators") || !exists("tbl_standardize")) {
    load(file.path(here::here(), "output", "indicator_tables.Rdata"))
  }

  # Ensure character type for matching
  all_std_long <- all_std_long %>%
    mutate(
      rcp = as.character(rcp),
      period_code = as.character(period_code),
      gcm = as.character(gcm),
      dsmodel = as.character(dsmodel)
    )

  # Get baseline downscaling method per indicator
  base_models <- tbl_standardize %>%
    select(indicator = abbrev, dsmodel_baseline) %>%
    mutate(dsmodel_baseline = as.character(dsmodel_baseline))

  # Define the scenarios to summarize
  # We focus on stat == "mean", and for future projections we use gcm == "9" (ensemble average)
  # For static/observed indicators, we include them too
  data_filtered <- all_std_long %>%
    filter(stat == "mean") %>%
    left_join(base_models, by = "indicator") %>%
    mutate(
      # If dsmodel is missing or NA, treat it as the baseline model
      dsmodel = coalesce(dsmodel, dsmodel_baseline)
    )

  # Define scenarios
  scenarios_df <- data_filtered %>%
    mutate(
      scenario_type = case_when(
        # Default scenario: RCP 4.5, Period 3, GCM 9 (or historical 0), Default Downscaling
        rcp == "45" & period_code == "3" & gcm == "9" & dsmodel == dsmodel_baseline ~ "Default (RCP 4.5, 2041-2060)",
        
        # RCP 4.5, Period 5 (GCM 9), Default Downscaling
        rcp == "45" & period_code == "5" & gcm == "9" & dsmodel == dsmodel_baseline ~ "RCP 4.5, 2081-2100",
        
        # RCP 8.5, Period 3 (GCM 9), Default Downscaling
        rcp == "85" & period_code == "3" & gcm == "9" & dsmodel == dsmodel_baseline ~ "RCP 8.5, 2041-2060",
        
        # RCP 8.5, Period 5 (GCM 9), Default Downscaling
        rcp == "85" & period_code == "5" & gcm == "9" & dsmodel == dsmodel_baseline ~ "RCP 8.5, 2081-2100",
        
        # Alternative downscaling method under default RCP/Period
        rcp == "45" & period_code == "3" & gcm == "9" & dsmodel != dsmodel_baseline ~ paste0("Alt Downscaling: ", dsmodel),
        
        # Observed/Baseline (Period 0 / GCM 0 / RCP 0)
        (rcp == "0" | period_code == "0" | gcm == "0") ~ "Historical / Baseline",
        
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(scenario_type))

  # Calculate 5%, mean, 95% quantiles across CUs for each indicator and scenario
  summary_stats <- scenarios_df %>%
    group_by(indicator, scenario_type) %>%
    summarise(
      q05 = quantile(value, 0.05, na.rm = TRUE),
      mean_val = mean(value, na.rm = TRUE),
      q95 = quantile(value, 0.95, na.rm = TRUE),
      .groups = "drop"
    )

  # Join with tbl_indicators to get pretty names and categories
  table_data <- tbl_indicators %>%
    select(indicator = abbrev, name, category, unit) %>%
    left_join(summary_stats, by = "indicator") %>%
    filter(!is.na(scenario_type))

  # Map category abbreviations to pretty names
  cat_pretty <- c(
    "fwrs" = "Freshwater Spawning & Rearing",
    "migr" = "Upstream Migration",
    "mar" = "Marine",
    "dem" = "Demographics",
    "gen" = "Genetics"
  )

  table_data <- table_data %>%
    mutate(
      category_pretty = factor(cat_pretty[category], levels = cat_pretty),
      # Format names
      scenario_type = factor(scenario_type, levels = c(
        "Historical / Baseline",
        "Default (RCP 4.5, 2041-2060)",
        "RCP 4.5, 2081-2100",
        "RCP 8.5, 2041-2060",
        "RCP 8.5, 2081-2100",
        sort(unique(scenario_type[grepl("^Alt Downscaling", scenario_type)]))
      ))
    ) %>%
    arrange(category_pretty, name, scenario_type)

  if (is.null(title)) {
    title <- "CVIS Indicator Scenario & Sensitivity Summary"
  }
  if (is.null(subtitle)) {
    subtitle <- "Distribution of raw indicator values (5%, mean, 95% quantiles across CUs) across RCPs, projection periods, and downscaling methods"
  }

  # Build the gt table
  gt_table <- table_data %>%
    select(category_pretty, name, unit, scenario_type, q05, mean_val, q95) %>%
    gt(groupname_col = "category_pretty") %>%
    tab_header(
      title = title,
      subtitle = subtitle
    ) %>%
    cols_label(
      name = "Indicator",
      unit = "Unit",
      scenario_type = "Scenario / Method",
      q05 = "5% Quantile",
      mean_val = "Mean",
      q95 = "95% Quantile"
    ) %>%
    tab_spanner(
      label = "Raw Indicator Value Distribution Across CUs",
      columns = c(q05, mean_val, q95)
    )

  # Alignments and formatting
  gt_table <- gt_table %>%
    sub_missing(
      columns = everything(),
      missing_text = "-"
    ) %>%
    cols_align(
      align = "left",
      columns = c(name, scenario_type)
    ) %>%
    cols_align(
      align = "center",
      columns = c(unit, q05, mean_val, q95)
    ) %>%
    fmt_number(
      columns = c(q05, mean_val, q95),
      decimals = 2
    )

  # Styling to match scientific/publication grade
  gt_table <- gt_table %>%
    opt_table_font(
      font = list(
        "Inter",
        "Helvetica Neue", "Arial", "sans-serif"
      )
    ) %>%
    tab_style(
      style = cell_text(weight = "bold", size = px(15), color = "#1A365D"),
      locations = cells_title(groups = "title")
    ) %>%
    tab_style(
      style = cell_text(size = px(11), style = "italic", color = "#4A5568"),
      locations = cells_title(groups = "subtitle")
    ) %>%
    tab_style(
      style = list(
        cell_fill(color = "#EBF8FF"),
        cell_text(color = "#2B6CB0", weight = "bold", size = px(11))
      ),
      locations = cells_row_groups()
    ) %>%
    tab_style(
      style = cell_borders(sides = "bottom", color = "#E2E8F0", weight = px(1)),
      locations = cells_body()
    ) %>%
    opt_row_striping() %>%
    tab_options(
      table.font.size = 10,
      heading.title.font.size = 13,
      heading.subtitle.font.size = 11,
      row_group.font.size = 11,
      row_group.font.weight = "bold",
      column_labels.font.weight = "bold",
      column_labels.background.color = "#F7FAFC",
      row.striping.background_color = "#F8FAFC",
      table.border.top.color = "#1A365D",
      table.border.top.width = px(2),
      table.border.bottom.color = "#1A365D",
      table.border.bottom.width = px(2),
      column_labels.border.bottom.color = "#A0AEC0",
      column_labels.border.bottom.width = px(1.5),
      table.width = pct(100),
      data_row.padding = px(5)
    )

  # Highlight default scenario row slightly for visual reference
  for (i in seq_len(nrow(table_data))) {
    if (table_data$scenario_type[i] == "Default (RCP 4.5, 2041-2060)") {
      gt_table <- gt_table %>%
        tab_style(
          style = list(
            cell_fill(color = "#FFFDE6"),
            cell_text(weight = "bold")
          ),
          locations = cells_body(rows = i)
        )
    }
  }

  return(gt_table)
}


#' Generate CVIS Indicator Description Table
#'
#' @description
#' Builds a publication-grade HTML summary table (`gt` format) describing the 17 indicators
#' used in the CVIS assessment, including their abbreviation, category, type, unit, and the
#' number of Conservation Units (CUs) with data.
#'
#' @param all_std_long_baseline Data frame containing baseline standardized indicators at the CU level. If NULL, attempts to load from output.
#' @param title Character. Header title for the gt table. Default is NULL (generates default title).
#' @param subtitle Character. Subtitle for the gt table. Default is NULL (generates default subtitle).
#'
#' @return A `gt_tbl` object.
generate_cvis_indicator_description_table <- function(
  all_std_long_baseline = NULL,
  title = NULL,
  subtitle = NULL
) {
  # Load data if NULL
  if (is.null(all_std_long_baseline)) {
    load(file.path(here::here(), "output", "scoring_results.Rdata"))
  }
  
  # 1. Calculate the number of CUs with data for each indicator dynamically
  cu_counts <- all_std_long_baseline %>%
    dplyr::filter(stat == "mean", !is.na(value)) %>%
    dplyr::group_by(indicator) %>%
    dplyr::summarise(n_cus = dplyr::n_distinct(FULL_CU_IN), .groups = "drop")

  # 2. Define the static indicator parameters and classifications (Standardization Method)
  indicator_meta <- tibble::tribble(
    ~indicator, ~name, ~category, ~std_method, ~unit_label,
    "favchange", "Change in ENM Favourability", "Freshwater Spawning & Rearing", "Inverse Linear", "Favourability Score",
    "cthr", "Standardized Cumulative Threats", "Freshwater Spawning & Rearing", "Linear", "Threat Index (0-1)",
    "tw8rate", "Rate of change in August Temperature", "Freshwater Spawning & Rearing", "Linear", "°C / decade",
    "tw8proj", "Projected August Temperature", "Freshwater Spawning & Rearing", "Exponential", "°C",
    "flow8pdelta", "Proportional change in August flow", "Freshwater Spawning & Rearing", "Exponential Decay", "Proportion change",
    "flow18pdelta", "Proportional change in Nov-Jan flow", "Freshwater Spawning & Rearing", "Exponential", "Proportion change",
    "fwres", "Freshwater residency time", "Freshwater Spawning & Rearing", "Step Threshold", "Number of days",
    
    "migrTproj", "Projected migration temperature", "Upstream Migration", "Exponential", "°C",
    "migrQpdelta", "Proportional change in discharge", "Upstream Migration", "Exponential Decay", "Proportion change",
    "migrdist", "Length of upstream migration", "Upstream Migration", "Linear", "km",
    
    "SSTproj", "Projected nearshore SST", "Marine", "Exponential", "°C",
    "SSTrate", "Rate of change in nearshore SST", "Marine", "Linear", "°C / decade",
    "CImpact", "Cumulative impacts to marine habitat", "Marine", "Linear", "Threat Index (0-1)",
    
    "CUstatus", "Wild Salmon Policy CU status", "Demographics", "Categorical Mapping", "WSP status category",
    "CUnmat", "Number of mature individuals (spawners)", "Demographics", "Exponential Decay", "Number of spawners",
    
    "hetzyg", "Genetic heterozygosity", "Genetics", "Inverse Linear", "Heterozygosity",
    "genoff", "Genomic offset", "Genetics", "Linear", "Genomic offset index"
  )

  # Join metadata with CU counts
  table_data <- indicator_meta %>%
    dplyr::left_join(cu_counts, by = "indicator") %>%
    dplyr::mutate(n_cus = dplyr::coalesce(n_cus, 0L))

  # Group categories to make it clean
  category_order <- c(
    "Freshwater Spawning & Rearing",
    "Upstream Migration",
    "Marine",
    "Demographics",
    "Genetics"
  )
  
  table_data <- table_data %>%
    dplyr::mutate(category = factor(category, levels = category_order)) %>%
    dplyr::arrange(category, name)

  # Create gt table
  gt_table <- gt(table_data, groupname_col = "category") %>%
    tab_header(
      title = title,
      subtitle = subtitle
    ) %>%
    cols_label(
      name = "Indicator Name",
      indicator = "Abbreviation",
      std_method = "Standardization Method",
      unit_label = "Measurement Unit",
      n_cus = "CUs with Data"
    )

  # Alignments and styling
  gt_table <- gt_table %>%
    sub_missing(
      columns = everything(),
      missing_text = "-"
    ) %>%
    cols_align(
      align = "left",
      columns = c(name, std_method, unit_label)
    ) %>%
    cols_align(
      align = "center",
      columns = c(indicator, n_cus)
    )

  # Formatting and typography
  gt_table <- gt_table %>%
    opt_table_font(
      font = list(
        "Inter",
        "Helvetica Neue", "Arial", "sans-serif"
      )
    ) %>%
    tab_style(
      style = cell_text(weight = "bold", size = px(15), color = "black"),
      locations = cells_title(groups = "title")
    ) %>%
    tab_style(
      style = cell_text(size = px(11), style = "italic", color = "black"),
      locations = cells_title(groups = "subtitle")
    ) %>%
    tab_style(
      style = list(
        cell_text(color = "black", weight = "bold", size = px(11))
      ),
      locations = cells_row_groups()
    ) %>%
    tab_style(
      style = cell_borders(sides = "bottom", color = "#E5E7EB", weight = px(0.5)),
      locations = cells_body()
    ) %>%
    tab_options(
      table.font.size = 10,
      heading.title.font.size = 13,
      heading.subtitle.font.size = 11,
      row_group.font.size = 11,
      row_group.font.weight = "bold",
      column_labels.font.weight = "bold",
      column_labels.background.color = "white",
      table.border.top.color = "black",
      table.border.top.width = px(2),
      table.border.bottom.color = "black",
      table.border.bottom.width = px(2),
      column_labels.border.bottom.color = "black",
      column_labels.border.bottom.width = px(1.5),
      row_group.border.top.color = "#E5E7EB",
      row_group.border.top.width = px(1),
      row_group.border.bottom.color = "#E5E7EB",
      row_group.border.bottom.width = px(1),
      table.width = pct(100),
      data_row.padding = px(5)
    )

  return(gt_table)
}





