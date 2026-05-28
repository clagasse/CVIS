# ==============================================================================
# CVIS Summary Table Generator (5d_table_summaries.R)
#
# Description:
#   Defines generate_cvis_summary_table() to build the climate and vulnerability
#   indicators summary table. Used in both the manuscript figures generator
#   (6_figures_manuscript.R) and the individual CU reports (6b_S2_CU_reports.Rmd).
# ==============================================================================

generate_cvis_summary_table <- function(
  all_std_long_baseline,
  cu_code = NULL,
  group_by = "species",
  categories = NULL,
  title = NULL,
  subtitle = NULL
) {
  library(dplyr)
  library(tidyr)
  library(gt)
  library(scales)

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

  # Default titles
  if (is.null(title)) {
    if (!is.null(cu_code)) {
      title <- paste("CVIS Climate and Vulnerability Indicators for CU:", cu_code)
    } else {
      title <- "Summary of CVIS Climate and Vulnerability Indicators"
    }
  }
  if (is.null(subtitle)) {
    if (!is.null(cu_code)) {
      subtitle <- "Baseline scenario mean and variation, and comparison against all CUs and species"
    } else {
      subtitle <- "Baseline scenario mean and variation in raw units and standardized scores"
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
        google_font(name = "Inter"),
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


generate_cvis_vulnerability_table <- function(
  scores_tidy,
  scores_tidy_baseline,
  cu_code = NULL,
  title = NULL,
  subtitle = NULL
) {
  library(dplyr)
  library(tidyr)
  library(gt)
  library(scales)

  # 1. Reshape category scores
  cat_scores <- scores_tidy_baseline %>%
    filter(method == "avg", category %in% c("dem", "fwrs", "gen", "mar", "migr")) %>%
    select(FULL_CU_IN, SPECIES_NAME, CVIS_NAME, CU_COMMON_NAME, SMU_SIMPLE, category, score100_all) %>%
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
    filter(std_method == "exponential", rcp == "45", period_code == "3", category == "all", method == "catavg") %>%
    filter(gcm %in% c("1", "4", "6")) %>%
    select(FULL_CU_IN, gcm, score100_all) %>%
    mutate(gcm = paste0("gcm", gcm)) %>%
    pivot_wider(
      names_from = gcm,
      values_from = score100_all
    )

  # 4. Extract different scoring methods for overall vulnerability (ensemble GCM 9)
  method_scores <- scores_tidy %>%
    filter(std_method == "exponential", rcp == "45", period_code == "3", category == "all", gcm == "9") %>%
    filter(method %in% c("avgall", "avgcube")) %>%
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
  round_cols <- c("dem", "fwrs", "gen", "mar", "migr", "overall", "rankall", "rankspecies", "gcm1", "gcm4", "gcm6", "avgall", "avgcube")
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
           avgall, avgcube,
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
      avgall = "Avg All",
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
      columns = c(avgall, avgcube, gcm1, gcm4, gcm6)
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
      columns = intersect(c("species_smu", "FULL_CU_IN", "overall", "rankall", "rankspecies", "dem", "fwrs", "gen", "mar", "migr", "avgall", "avgcube", "gcm1", "gcm4", "gcm6"), names(vuln_table_data))
    ) %>%
    cols_align(
      align = "left",
      columns = CU_COMMON_NAME
    ) %>%
    opt_table_font(
      font = list(
        google_font(name = "Inter"),
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
  score_cols <- c("dem", "fwrs", "gen", "mar", "migr", "overall", "avgall", "avgcube", "gcm1", "gcm4", "gcm6")
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


generate_cvis_sensitivity_table <- function(
  overall_sensitivity,
  cu_code = NULL,
  title = NULL,
  subtitle = NULL
) {
  library(dplyr)
  library(tidyr)
  library(gt)
  library(scales)

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
      CU_COMMON_NAME = CVIS_NAME,
      
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
      
      score_avgall = round(base_score + raw_dev_Method_avgall, 0),
      rank_avgall = round(rankall_Method_avgall, 0),
      
      score_avgcube = round(base_score + raw_dev_Method_avgcube, 0),
      rank_avgcube = round(rankall_Method_avgcube, 0)
    )

  if (is.null(title)) {
    title <- "CVIS Overall Vulnerability Sensitivity Analysis"
  }
  if (is.null(subtitle)) {
    subtitle <- "Overall vulnerability scores and ranks across alternative climate models, downscalers, and scoring methods"
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
      score_avgall = "Score", rank_avgall = "Rank",
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
      label = "Alternative DS",
      columns = c(score_ds, rank_ds)
    ) %>%
    tab_spanner(
      label = "Linear Std",
      columns = c(score_std, rank_std)
    ) %>%
    tab_spanner(
      label = "Avg All Method",
      columns = c(score_avgall, rank_avgall)
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
      columns = intersect(c("species_smu", "FULL_CU_IN", "score_base", "rank_base", "score_gcm1", "rank_gcm1", "score_gcm4", "rank_gcm4", "score_gcm6", "rank_gcm6", "score_ds", "rank_ds", "score_std", "rank_std", "score_avgall", "rank_avgall", "score_avgcube", "rank_avgcube"), names(table_data))
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
    "score_avgall", "rank_avgall",
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
        google_font(name = "Inter"),
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
  
  spanner_rights <- c("rank_base", "rank_gcm1", "rank_gcm4", "rank_gcm6", "rank_ds", "rank_std", "rank_avgall", "rank_avgcube")
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
  score_cols <- c("score_base", "score_gcm1", "score_gcm4", "score_gcm6", "score_ds", "score_std", "score_avgall", "score_avgcube")
  rank_cols <- c("rank_base", "rank_gcm1", "rank_gcm4", "rank_gcm6", "rank_ds", "rank_std", "rank_avgall", "rank_avgcube")

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


generate_cvis_cu_sensitivity_table <- function(
  overall_sensitivity,
  cu_code,
  title = NULL,
  subtitle = NULL
) {
  library(dplyr)
  library(tidyr)
  library(gt)
  library(scales)

  # Load indicator metadata if not in active environment
  if (!exists("tbl_indicators")) {
    load(file.path(here::here(), "output", "indicator_tables.Rdata"))
  }

  # 1. Get indicator-level sensitivity metrics for this CU
  ind_cu <- overall_sensitivity$indicator_metrics %>%
    filter(FULL_CU_IN == cu_code) %>%
    filter(!is.na(base_raw_mean)) # Keep relevant indicators only

  # Map category abbreviations to pretty names
  cat_pretty <- c(
    "fwrs" = "Freshwater Spawning & Rearing",
    "migr" = "Upstream Migration",
    "mar" = "Marine",
    "dem" = "Demographics",
    "gen" = "Genetics"
  )

  # Prepare indicator rows
  ind_rows <- ind_cu %>%
    left_join(select(tbl_indicators, abbrev, name, unit), by = c("indicator" = "abbrev")) %>%
    transmute(
      row_type = "indicator",
      category_pretty = factor(cat_pretty[category], levels = cat_pretty),
      name = ifelse(is.na(name), as.character(indicator), name),
      unit = ifelse(is.na(unit) | unit == "", "-", unit),
      base_raw = base_raw_mean,
      
      score_base = round(base_std_mean * 100, 0),
      score_gcm1 = round((base_std_mean + std_dev_GCM1) * 100, 0),
      score_gcm4 = round((base_std_mean + std_dev_GCM4) * 100, 0),
      score_gcm6 = round((base_std_mean + std_dev_GCM6) * 100, 0),
      score_ds = round((base_std_mean + std_dev_dsmethod) * 100, 0),
      score_std = round((base_std_mean + std_dev_stdmethod) * 100, 0),
      score_avgall = NA_real_,
      score_avgcube = NA_real_
    ) %>%
    arrange(category_pretty, name)

  # 2. Get overall vulnerability sensitivity metrics for this CU
  ov_cu <- overall_sensitivity$deviations %>%
    filter(FULL_CU_IN == cu_code, category == "all")

  # Prepare overall vulnerability row
  ov_row <- tibble(
    row_type = "overall",
    category_pretty = factor("Overall Vulnerability", levels = c(cat_pretty, "Overall Vulnerability")),
    name = "Overall Vulnerability Score",
    unit = "Score (0-100)",
    base_raw = NA_real_,
    
    score_base = round(ov_cu$base_score, 0),
    score_gcm1 = round(ov_cu$base_score + ov_cu$raw_dev_GCM1, 0),
    score_gcm4 = round(ov_cu$base_score + ov_cu$raw_dev_GCM4, 0),
    score_gcm6 = round(ov_cu$base_score + ov_cu$raw_dev_GCM6, 0),
    score_ds = round(ov_cu$base_score + ov_cu$raw_dev_dsmethod, 0),
    score_std = round(ov_cu$base_score + ov_cu$raw_dev_stdmethod, 0),
    score_avgall = round(ov_cu$base_score + ov_cu$raw_dev_Method_avgall, 0),
    score_avgcube = round(ov_cu$base_score + ov_cu$raw_dev_Method_avgcube, 0)
  )

  # Combine them
  all_pretty_levels <- c(cat_pretty, "Overall Vulnerability")
  
  ind_rows$category_pretty <- factor(ind_rows$category_pretty, levels = all_pretty_levels)
  ov_row$category_pretty <- factor(ov_row$category_pretty, levels = all_pretty_levels)

  table_data <- bind_rows(ind_rows, ov_row)

  if (is.null(title)) {
    title <- paste("Vulnerability Sensitivity Profile for CU:", cu_code)
  }
  if (is.null(subtitle)) {
    subtitle <- "Comparison of standardized risk scores (0-100) across alternative climate models, downscalers, and scoring methods"
  }

  # 3. Generate gt table
  gt_sens_table <- table_data %>%
    gt(groupname_col = "category_pretty") %>%
    tab_header(
      title = title,
      subtitle = subtitle
    ) %>%
    cols_label(
      name = "Indicator / Overall Metric",
      unit = "Units",
      base_raw = "Base Raw Value",
      score_base = "Baseline",
      score_gcm1 = "GCM 1",
      score_gcm4 = "GCM 4",
      score_gcm6 = "GCM 6",
      score_ds = "Alt DS",
      score_std = "Linear Std",
      score_avgall = "Avg All",
      score_avgcube = "Avg Cube"
    ) %>%
    tab_spanner(
      label = "Standardized Risk Score / Overall Score (0-100)",
      columns = c(score_base, score_gcm1, score_gcm4, score_gcm6, score_ds, score_std, score_avgall, score_avgcube)
    )

  # Alignments
  gt_sens_table <- gt_sens_table %>%
    sub_missing(
      columns = everything(),
      missing_text = "-"
    ) %>%
    cols_align(
      align = "left",
      columns = name
    ) %>%
    cols_align(
      align = "center",
      columns = c(unit, base_raw, score_base, score_gcm1, score_gcm4, score_gcm6, score_ds, score_std, score_avgall, score_avgcube)
    )

  # Formats
  gt_sens_table <- gt_sens_table %>%
    fmt_number(
      columns = base_raw,
      decimals = 2
    ) %>%
    fmt_integer(
      columns = c(score_base, score_gcm1, score_gcm4, score_gcm6, score_ds, score_std, score_avgall, score_avgcube)
    )

  # Styling
  gt_sens_table <- gt_sens_table %>%
    opt_table_font(
      font = list(
        google_font(name = "Inter"),
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
    )

  # Add right border to separate descriptive columns from score columns
  gt_sens_table <- gt_sens_table %>%
    tab_style(
      style = cell_borders(sides = "right", color = "#CBD5E0", weight = px(1.5)),
      locations = cells_body(columns = base_raw)
    ) %>%
    tab_style(
      style = cell_borders(sides = "right", color = "#CBD5E0", weight = px(1.5)),
      locations = cells_column_labels(columns = base_raw)
    )

  # Color scale functions for scores (0-100)
  color_fun_score <- scales::col_numeric(
    palette = c("#E8F3FF", "#FFFDE6", "#FFEAEA"),
    domain = c(0, 100),
    na.color = "#FFFFFF"
  )

  # Apply colors to score columns
  score_cols <- c("score_base", "score_gcm1", "score_gcm4", "score_gcm6", "score_ds", "score_std", "score_avgall", "score_avgcube")
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

  # Highlight the Overall Vulnerability row specifically (bold, larger, distinct border)
  overall_row_idx <- which(table_data$row_type == "overall")
  gt_sens_table <- gt_sens_table %>%
    tab_style(
      style = cell_text(weight = "bold", size = px(12)),
      locations = cells_body(rows = overall_row_idx)
    ) %>%
    tab_style(
      style = cell_borders(sides = c("top", "bottom"), color = "#1A365D", weight = px(2)),
      locations = cells_body(rows = overall_row_idx)
    )

  # General table options
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

  # Hide row_type column
  gt_sens_table <- gt_sens_table %>%
    cols_hide(columns = row_type)

  return(gt_sens_table)
}


plot_cu_sensitivity_summary <- function(
  overall_sensitivity,
  tbl_indicators,
  cu_code
) {
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(patchwork)

  # 1. Prepare score deviations data
  dev_raw <- overall_sensitivity$deviations %>%
    filter(category == "all", FULL_CU_IN != "ALL") %>%
    select(FULL_CU_IN, category, starts_with("raw_dev_")) %>%
    pivot_longer(cols = starts_with("raw_dev_"), names_to = "source_label", values_to = "raw_deviation") %>%
    mutate(
        source_label = str_remove(source_label, "raw_dev_"),
        source_type = case_when(
            str_detect(source_label, "^GCM") ~ "GCM",
            str_detect(source_label, "^RCP") ~ "Scenario",
            str_detect(source_label, "^Method") ~ "Method",
            str_detect(source_label, "^Model") ~ "dsmethod",
            str_detect(source_label, "^dsmethod") ~ "dsmethod",
            str_detect(source_label, "^stdmethod") ~ "stdmethod",
            TRUE ~ "Other"
        ),
        source = case_when(
            source_type == "Method" ~ str_remove(source_label, "^Method_"),
            TRUE ~ source_label
        )
    ) %>%
    filter(!source %in% c("cube", "flag", "cube_all"))

  # Factor levels for consistency
  source_levels <- c("GCM1", "GCM4", "GCM6", "RCP45_P5", "RCP85_P3", "RCP85_P5", "dsmethod", "stdmethod", "avgall", "avgcube")
  source_labels <- c("CanESM2 (GCM 1)", "HadGEM2 (GCM 4)", "MPI (GCM 6)", "RCP 4.5", "RCP 8.5 (P3)", "RCP 8.5 (P5)", "Downscale Meth", "Standardize Meth", "Avg All Scoring", "Avg Cube Scoring")
  
  dev_raw <- dev_raw %>%
    filter(source %in% source_levels) %>%
    mutate(source = factor(source, levels = rev(source_levels), labels = rev(source_labels)))

  # Selected CU data
  dev_raw_cu <- dev_raw %>% filter(FULL_CU_IN == cu_code)
  # All other CUs
  dev_raw_others <- dev_raw %>% filter(FULL_CU_IN != cu_code)

  # Plot 1: Score deviations
  p1 <- ggplot(dev_raw_others, aes(y = source, x = raw_deviation)) +
    geom_violin(fill = "#EBF8FF", color = "#90CDF4", alpha = 0.5, scale = "width") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
    geom_point(data = dev_raw_cu, aes(x = raw_deviation, y = source), color = "#E67E22", size = 4, shape = 18) +
    labs(
      title = "Overall Vulnerability Score Shifts",
      subtitle = "Violins show Fraser CUs distribution; Orange diamond shows selected CU",
      x = "Score Deviation (Scenario - Baseline)",
      y = "Assumption / Scenario"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", size = 12),
      plot.subtitle = element_text(size = 9, color = "grey40"),
      panel.grid.minor = element_blank(),
      axis.text.y = element_text(size = 9)
    )

  # 2. Prepare indicator directional shifts data
  ind_shift_cus <- overall_sensitivity$indicator_metrics %>%
    filter(FULL_CU_IN != "ALL") %>%
    mutate(
        val_Baseline = base_raw_mean,
        val_GCM1 = base_raw_mean + raw_dev_GCM1,
        val_GCM4 = base_raw_mean + raw_dev_GCM4,
        val_GCM6 = base_raw_mean + raw_dev_GCM6,
        val_RCP45_P5 = base_raw_mean + raw_dev_RCP45_P5,
        val_RCP85_P3 = base_raw_mean + raw_dev_RCP85_P3,
        val_RCP85_P5 = base_raw_mean + raw_dev_RCP85_P5,
        val_dsmethod = base_raw_mean + raw_dev_dsmethod,
        val_stdmethod = base_raw_mean + raw_dev_stdmethod
    ) %>%
    select(FULL_CU_IN, indicator, category, base_raw_mean, starts_with("val_")) %>%
    pivot_longer(cols = starts_with("val_"), names_to = "source", names_prefix = "val_", values_to = "val") %>%
    filter(!is.na(val))

  # Keep only indicators that have non-NA values for the selected CU
  valid_indicators <- ind_shift_cus %>%
    filter(FULL_CU_IN == cu_code, !is.na(val)) %>%
    pull(indicator) %>%
    unique()

  # Calculate variation for each indicator for this CU
  var_indicators <- ind_shift_cus %>%
    filter(FULL_CU_IN == cu_code) %>%
    group_by(indicator) %>%
    summarise(
      val_range = max(val, na.rm = TRUE) - min(val, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(!is.na(val_range), val_range > 1e-5) %>%
    arrange(desc(val_range)) %>%
    pull(indicator)

  # If there are no varying indicators, fall back to all valid indicators
  if (length(var_indicators) == 0) {
    var_indicators <- valid_indicators
  } else if (length(var_indicators) > 9) {
    # Keep at most 9 indicators to keep the plot clean
    var_indicators <- var_indicators[1:9]
  }

  ind_shift_cus <- ind_shift_cus %>%
    filter(indicator %in% var_indicators)

  # Scenario levels for indicators
  ind_source_levels <- c("Baseline", "GCM1", "GCM4", "GCM6", "RCP45_P5", "RCP85_P3", "RCP85_P5", "dsmethod", "stdmethod")
  ind_source_labels <- c("Baseline", "CanESM2 (GCM 1)", "HadGEM2 (GCM 4)", "MPI (GCM 6)", "RCP 4.5", "RCP 8.5 (P3)", "RCP 8.5 (P5)", "Downscale Meth", "Standardize Meth")

  ind_shift_cus <- ind_shift_cus %>%
    filter(source %in% ind_source_levels) %>%
    mutate(source = factor(source, levels = rev(ind_source_levels), labels = rev(ind_source_labels)))

  ind_shift_cu <- ind_shift_cus %>% filter(FULL_CU_IN == cu_code)
  ind_shift_others <- ind_shift_cus %>% filter(FULL_CU_IN != cu_code)

  # Label map with units
  ind_label_units <- tbl_indicators %>% 
    mutate(facet_label = paste0(abbrev, " (", unit, ")")) %>% 
    select(abbrev, facet_label) %>% 
    tibble::deframe()

  # Plot 2: Indicator directional shifts
  p2 <- ggplot(ind_shift_others, aes(y = source, x = val)) +
    geom_violin(fill = "#EDF2F7", color = "#CBD5E0", alpha = 0.5, scale = "width") +
    geom_point(data = ind_shift_cu, aes(x = val, y = source), color = "#E67E22", size = 3, shape = 18) +
    facet_wrap(~indicator, scales = "free_x", ncol = 3, labeller = labeller(indicator = ind_label_units)) +
    labs(
      title = "Raw Indicator Value Shifts",
      subtitle = "Violins show Fraser CUs distribution; Orange diamond shows selected CU",
      x = "Indicator Value (units vary)",
      y = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", size = 12),
      plot.subtitle = element_text(size = 9, color = "grey40"),
      strip.text = element_text(face = "bold", size = 9),
      panel.grid.minor = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    )

  # Combine using patchwork
  p_combined <- p1 + p2 + 
    plot_layout(widths = c(1, 2.2)) +
    plot_annotation(
      title = paste("Sensitivity Analysis Plots for CU:", cu_code),
      subtitle = "Visualizing how overall vulnerability score and individual indicators shift across uncertainty assumptions relative to other Fraser CUs",
      theme = ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold", size = 14, color = "#1A365D"),
        plot.subtitle = ggplot2::element_text(size = 10, color = "#4A5568")
      )
    )

  return(p_combined)
}



