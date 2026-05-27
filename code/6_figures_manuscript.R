# ==============================================================================
# CVIS Manuscript Figures Generator (6_figures_manuscript.R)
#
# Description:
#   Generates and saves the final PNG figure outputs used in the CVIS manuscript.
#   Loads spatial datasets, model outputs, indicators, and scoring results,
#   then plots maps, lollipop plots, migration paths, and indicator tile grids.
#
# Workflow Steps:
#   1. Setup settings and verify the output directory exists.
#   2. Load freshwater, migration, marine, and scoring datasets.
#   3. Subset CU and timing data for the specified case study.
#   4. Generate Figure 2 (Freshwater indicator multipanel maps).
#   5. Generate Figure 3 (Upstream migration path).
#   6. Generate Figure 4 (Indicator lollipop plot).
#   7. Generate Figure 5 (Vulnerability spatial maps).
#   8. Generate Figure 6 (Marine adaptive zones lollipop plot).
#   9. Generate Figure 7 (Vulnerability score indicator tiles).
#
# Inputs:
#   - Processed Rds/Rdata files under paths$fw, paths$marine, and paths$output
#
# Outputs:
#   - PNG figure files saved in paths$figures/manuscript/
#
# Dependencies:
#   - Requires 0_setup.R, ggplot2, sf, dplyr
# ==============================================================================

# ==================== 1. Settings & Configurations ====================


# Settings ----------------------------------------------------------------
# set-up used in every script
library(here)
setwd(here())
source(file.path(here(), "code", "0_setup.R"))

#output directory 
output_dir <- file.path(paths$figures, "manuscript")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# select case study CUs for manuscript
casestudy_1 <- "CK-12"
casestudy_2 <- "CM-02"
casestudy_3 <- "PKO-01"


# ==================== 2. Load Processed Datasets ====================

### ----- Load frequently used data sets- ------
# load marine adaptive zone spatial object
load(file.path(paths$marine, "MAZ.Rds"))
# load watershed basins R object
load(file.path(paths$fw, "basins_shp.Rds"))
# make a Fraser basin version
Fr_basin <- filter(basins, BASIN == "FRASER")

# freshwater stream subsets by CU boundary
load(file.path(paths$fw, "fw_streampicks_tscapes.Rdata"))
# migration paths
load(file.path(paths$fw, "fw_upstream_paths.Rdata"))

# spatial models
# stream model outputs for freshwater spawning and rearing indicators
load(file.path(paths$fw, "fw_models_tscapes.Rds"))
# indicator spatial outputs
load(file.path(paths$fw, "fw_stream_indicators_sp.Rds"))
# lakes_Fr - freshwater lakes for plotting
load(file.path(paths$fw, "BC_FWA_LAKES_FR.Rds"))

# Freshwater
fw_file <- get_latest_file(paths$fw, "fw_rearing_indicators.Rdata")
load(fw_file) # loads fw_all, ss_all

# Migration
migr_file <- get_latest_file(paths$fw, "migr_stats.Rdata")
load(migr_file) # loads migr_all, etc.

# Marine
mar_file <- get_latest_file(paths$marine, "marine_stats.Rdata") # look for .Rds
load(mar_file)


#scoring data
load(file.path(paths$output, "scoring_results.Rdata")) 
# Load outputs from 4b (Indicator metrics and sensitivity analysis)
load(file.path(paths$output, "sensitivity_analysis.Rdata")) # loads overall_sensitivity



# ==================== 3. Subset Case Study Data ====================

cu_i <- casestudy_1

cu_run_i <- cu_run[cu_run$FULL_CU_IN == cu_i, ]
#sp_pick <- cu_run$spp[cu_run$FULL_CU_IN == cu_i] # species abbr
#sp_pick_bcfp <- spp_lookup$spp_abr_bcfp[spp_lookup$spp_abr == sp_pick]

cu_timing_i <- cu_timing_Fr %>% filter(FULL_CU_IN == cu_i)

cu_boundary_i <- cu_boundary[cu_boundary$FULL_CU_IN == cu_i, ]

#Lakes within CU boundary (for plotting)
temp <- unlist(st_intersects(cu_boundary_i, lakes_Fr))
lakes_cu <- lakes_Fr[temp,]

# subset nuseds observations
nuseds_cu <- nuseds_Fr[nuseds_Fr$FULL_CU_IN == cu_i, ]

# subset migration path
migr_cu <- migr_list[[cu_i]]

#subset fw models
fw_models_cu <- subset_fw_models(
  fw_models = fw_models,
  cu_i = cu_i,
  stream_cu_picks = stream_cu_picks,
  cu_run = cu_run,
  spp_lookup = spp_lookup,
  to_factor = TRUE
)

fw_sp_ind_cu <- subset_fw_models(
  fw_models = fw_sp_ind,
  cu_i = cu_i,
  stream_cu_picks = stream_cu_picks,
  cu_run = cu_run,
  spp_lookup = spp_lookup,
  to_factor = TRUE,
  filter_rs = T  #get rearing/spawning streams only for species
)

cu_timing_long_i <- cu_timing_long[cu_timing_long$FULL_CU_IN == cu_i, ]
cu_timing_i <- cu_timing_Fr[cu_timing_Fr$FULL_CU_IN == cu_i, ]



# ==================== 4. Generate & Save Figures ====================

# Figure 2 - Map of values for freshwater spawning and rearing indicators within a CU boundary

f2 <- stream_indicator_multipanel_plot(fw_sp_ind_cu,
  cu_boundary_i,
  lakes_cu,
  variables = c("favchange_chinook_85_3", "cthr_anad", "tw8proj_9_45_3", "tw8rate_9_45_3", "flow8pdelta_9_45_3", "flow18pdelta_9_45_3"),
  plot_titles = c("Change in ENM Favourability", "Cumulative Threat Score", "August Mean Temperature",
                  "Rate of Temp. Change", "Change in August Flow", "Change in Nov-Jan Flow"),
  scico_palette = "roma",
  palette_directions = c(1, -1, -1, -1, 1, -1))

#save as png
ggsave(filename = file.path(output_dir, "figure_2.png"), plot = f2, width = 10, height = 6) 

# Figure 3 - Summary of migration timing and temperatures across CUs
# Uses migration_compare_plot from 5b to show all CUs' timing and temperatures for all 365 days (months)
f3 <- migration_compare_plot(
  migr_daily_calendar,
  timing = cu_timing_Fr,
  rcp = "45",
  period_choose = c("1981-2010", "2041-2060")
)

ggsave(filename = file.path(output_dir, "figure_3.png"), plot = f3, width = 8, height = 6)

            
# Figure 4 - Lollilop plot of indicator values and standardization function for change in August flow

f4 <- plot_lollipop(all_std_long_baseline,
                    indicator_pick = "migrTproj")

ggsave(filename = file.path(output_dir, "figure_4.png"), plot = f4,
       width = 5, height = 6) 

# Figure 5 - Mapped vulnerability scores for freshwater spawning and rearing category.

f5 <- spatial_fw_rearing_indicators_plot(all_std_long_baseline,
                                         cu_boundary,
                                         outline = Fr_basin,
                                         species_pick = "Chinook")
    
ggsave(filename = file.path(output_dir, "figure_5.png"), plot = f5, width = 10, height = 9)                                
                                   
# Figure 6 - Marine adaptive zones and associated mean indicator scores for each indicator - SSTproj, SSTrate, CImpact.

f6 <- plot_maz_lollipop(maz_all)

ggsave(filename = file.path(output_dir, "figure_6.png"), plot = f6)   

# Figure 7 - INdicator tile plot of overall vulnerability scores and individual indicator scores.

f7 <- indicator_cu_tile_plot(all_std_long_baseline,
                       scores_tidy_baseline)


ggsave(filename = file.path(output_dir, "figure_7.png"), plot = f7,
       width = 8, height = 10) 


# Figure 7 - INdicator tile plot of overall vulnerability scores and individual indicator scores.

f8 <- plot_methods_compare_tile(scores_tidy_baseline)

ggsave(filename = file.path(output_dir, "figure_8.png"), plot = f8)



# Figure 9 - Violin plot of spread of indicator values for 50 CUs across different scenarios.

#see plots created in script 5c

# Figure 10 - Deviations in overall vulnerability scores across sources of variation.

# see plots created in script 5d

# Figure 11 - Species-level bump plots of change in vulnerability rank for each CU across different sources of variation.


# ==================== 5. Summary Table for Manuscript ====================


# 1. Filter to CU-level means to get raw and standardized score distributions across all CUs
cu_mean_dat <- all_std_long_baseline %>%
  filter(stat == "mean")

# 2. Summarize raw value distribution across CUs (min, q25, mean, q75, max)
raw_summary <- cu_mean_dat %>%
  group_by(indicator) %>%
  summarise(
    raw_min = min(value, na.rm = TRUE),
    raw_q25 = quantile(value, 0.25, na.rm = TRUE),
    raw_mean = mean(value, na.rm = TRUE),
    raw_q75 = quantile(value, 0.75, na.rm = TRUE),
    raw_max = max(value, na.rm = TRUE),
    .groups = "drop"
  )

# 3. Summarize standardized score distribution across CUs (mean only)
std_summary <- cu_mean_dat %>%
  group_by(indicator) %>%
  summarise(
    std_mean = mean(std_value, na.rm = TRUE),
    .groups = "drop"
  )

# 4. Summarize SMU-specific raw means (using stat == "mean")
smu_names <- unique(cu_mean_dat$SMU_SIMPLE)
smu_names <- smu_names[!is.na(smu_names)]
smu_names <- sort(as.character(smu_names))

smu_stats <- cu_mean_dat %>%
  group_by(indicator, SMU_SIMPLE) %>%
  summarise(
    smu_raw_mean = mean(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = SMU_SIMPLE,
    values_from = smu_raw_mean
  )

# Extract standardized SMU means to use for coloring
smu_std_stats_wide <- cu_mean_dat %>%
  group_by(indicator, SMU_SIMPLE) %>%
  summarise(
    smu_std_mean = mean(std_value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(SMU_SIMPLE = paste0("std_val_", SMU_SIMPLE)) %>%
  pivot_wider(
    names_from = SMU_SIMPLE,
    values_from = smu_std_mean
  )

# 5. Combine with metadata from tbl_indicators
table_data <- tbl_indicators %>%
  select(indicator = abbrev, name, category, unit, std_fun) %>%
  left_join(raw_summary, by = "indicator") %>%
  left_join(std_summary, by = "indicator") %>%
  left_join(smu_stats, by = "indicator") %>%
  left_join(smu_std_stats_wide, by = "indicator")

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

# Ensure all SMU and helper columns exist in table_data
for (smu in smu_names) {
  if (!smu %in% names(table_data)) {
    table_data[[smu]] <- NA_real_
  }
  std_col <- paste0("std_val_", smu)
  if (!std_col %in% names(table_data)) {
    table_data[[std_col]] <- NA_real_
  }
}

# 6. Generate gt table
gt_table <- table_data %>%
  select(category_pretty, name, unit, std_method_label,
         raw_min, raw_q25, raw_mean, raw_q75, raw_max,
         std_mean, 
         all_of(smu_names),
         starts_with("std_val_")) %>%
  gt(groupname_col = "category_pretty") %>%
  tab_header(
    title = "Summary of CVIS Climate and Vulnerability Indicators",
    subtitle = "Baseline scenario mean and variation in raw units and standardized scores"
  ) %>%
  cols_label(
    name = "Indicator Name",
    unit = "Units",
    std_method_label = "Std. Method",
    raw_min = "Min",
    raw_q25 = "25%",
    raw_mean = "Mean",
    raw_q75 = "75%",
    raw_max = "Max",
    std_mean = "Mean"
  ) %>%
  tab_spanner(
    label = "Raw Values (All CUs)",
    columns = c(raw_min, raw_q25, raw_mean, raw_q75, raw_max)
  ) %>%
  tab_spanner(
    label = "Standardized Score (All CUs)",
    columns = c(std_mean)
  ) %>%
  tab_spanner(
    label = "Raw Mean by SMU",
    columns = all_of(smu_names)
  ) %>%
  fmt_number(
    columns = c(raw_min, raw_q25, raw_mean, raw_q75, raw_max, std_mean, all_of(smu_names)),
    decimals = 2
  ) %>%
  sub_missing(
    columns = everything(),
    missing_text = "-"
  ) %>%
  cols_align(
    align = "center",
    columns = c(unit, std_method_label)
  ) %>%
  cols_align(
    align = "left",
    columns = name
  ) %>%
  # Hide the helper standardized columns used for coloring
  cols_hide(columns = starts_with("std_val_")) %>%
  # Apply text formatting and borders for clean layout
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
  # Row Group headers: styling them with a subtle light tint and bold font
  tab_style(
    style = list(
      cell_fill(color = "#EBF8FF"),
      cell_text(color = "#2B6CB0", weight = "bold", size = px(11))
    ),
    locations = cells_row_groups()
  ) %>%
  # Add vertical lines to delineate the logical sections of the table
  tab_style(
    style = cell_borders(sides = "right", color = "#CBD5E0", weight = px(1.5)),
    locations = cells_body(columns = std_method_label)
  ) %>%
  tab_style(
    style = cell_borders(sides = "right", color = "#CBD5E0", weight = px(1.5)),
    locations = cells_column_labels(columns = std_method_label)
  ) %>%
  tab_style(
    style = cell_borders(sides = "right", color = "#CBD5E0", weight = px(1.5)),
    locations = cells_body(columns = std_mean)
  ) %>%
  tab_style(
    style = cell_borders(sides = "right", color = "#CBD5E0", weight = px(1.5)),
    locations = cells_column_labels(columns = std_mean)
  ) %>%
  # Bold the raw mean and standardized mean column values
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = c(raw_mean, std_mean))
  ) %>%
  # Subtle styling for the table title / column names
  tab_style(
    style = cell_text(weight = "bold", size = px(11), color = "#2D3748"),
    locations = cells_column_spanners(spanners = everything())
  ) %>%
  # Style data cells with light grey horizontal borders for clear separation
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

# Color scale function for vulnerability (soft, desaturated RdYlBu style: light blue to light yellow to light red)
color_fun <- scales::col_numeric(
  palette = c("#E8F3FF", "#FFFDE6", "#FFEAEA"),
  domain = c(0, 1),
  na.color = "#FFFFFF"
)



# Apply cell background colors to SMU raw means based on their relative standardized vulnerability
for (smu in smu_names) {
  std_col <- paste0("std_val_", smu)
  for (i in seq_len(nrow(table_data))) {
    val <- table_data[[std_col]][i]
    if (!is.na(val)) {
      bg_color <- color_fun(val)
      gt_table <- gt_table %>%
        tab_style(
          style = cell_fill(color = bg_color),
          locations = cells_body(columns = all_of(smu), rows = i)
        )
    }
  }
}

# Save the table outputs
gtsave(gt_table, filename = file.path(output_dir, "table_summary.html"))
# Save as image if webshot2 is available (wrapped in tryCatch to prevent failures)
tryCatch({
  gtsave(gt_table, filename = file.path(output_dir, "table_summary.png"))
  cat("Table saved to PNG successfully!\n")
}, error = function(e) {
  cat("gtsave as PNG failed (likely webshot2/PhantomJS not installed): ", e$message, "\n")
})


# ==================== 6. Vulnerability Summary Table by Category ====================

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

# 3. Calculate min and max overall vulnerability scores across GCMs 1, 4, and 6
gcm_scores <- scores_tidy %>%
  filter(std_method == "exponential", rcp == "45", period_code == "3", category == "all", method == "catavg") %>%
  filter(gcm %in% c("1", "4", "6")) %>%
  group_by(FULL_CU_IN) %>%
  summarise(
    gcm_min = min(score100_all, na.rm = TRUE),
    gcm_max = max(score100_all, na.rm = TRUE),
    .groups = "drop"
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
    # Combine Species and SMU for grouping
    species_smu = paste0(SPECIES_NAME, " - ", SMU_SIMPLE)
  ) %>%
  arrange(SPECIES_NAME, SMU_SIMPLE, FULL_CU_IN)

# 6. Round columns to integer
round_cols <- c("dem", "fwrs", "gen", "mar", "migr", "overall", "rankall", "rankspecies", "gcm_min", "gcm_max", "avgall", "avgcube")
for (col in round_cols) {
  if (col %in% names(vuln_table_data)) {
    vuln_table_data[[col]] <- round(vuln_table_data[[col]], 0)
  }
}

# Select and order columns as requested (Ranks first, then categories, then overall, then alternatives, then GCM range)
vuln_table_data <- vuln_table_data %>%
  select(species_smu, FULL_CU_IN, CU_COMMON_NAME, 
         rankall, rankspecies, 
         dem, fwrs, gen, mar, migr, 
         overall, avgall, avgcube,
         gcm_min, gcm_max)

# 7. Build gt table
gt_vuln_table <- vuln_table_data %>%
  gt(groupname_col = "species_smu") %>%
  tab_header(
    title = "Conservation Unit Climate Vulnerability Scores and Ranks",
    subtitle = "Relative ranks, category vulnerability, GCM model ranges, and scoring methodology comparisons"
  ) %>%
  cols_label(
    FULL_CU_IN = "CU Code",
    CU_COMMON_NAME = "CU Name",
    rankall = "All CUs",
    rankspecies = "Within Species",
    dem = "Demographics",
    fwrs = "Freshwater",
    gen = "Genetics",
    mar = "Marine",
    migr = "Migration",
    overall = "Overall",
    avgall = "Avg All",
    avgcube = "Avg Cube",
    gcm_min = "Min",
    gcm_max = "Max"
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
    label = "Overall Scoring Methods (Ensemble)",
    columns = c(overall, avgall, avgcube)
  ) %>%
  tab_spanner(
    label = "GCM Range (catavg)",
    columns = c(gcm_min, gcm_max)
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
    columns = c(FULL_CU_IN, rankall, rankspecies, dem, fwrs, gen, mar, migr, overall, avgall, avgcube, gcm_min, gcm_max)
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
  ) %>%
  # Style Row Group headers
  tab_style(
    style = list(
      cell_fill(color = "#EBF8FF"),
      cell_text(color = "#2B6CB0", weight = "bold", size = px(11))
    ),
    locations = cells_row_groups()
  ) %>%
  # Vertical dividers
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
  ) %>%
  # Bold the overall vulnerability column
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = overall)
  ) %>%
  # Header label styling
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
score_cols <- c("dem", "fwrs", "gen", "mar", "migr", "overall", "avgall", "avgcube", "gcm_min", "gcm_max")
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

# Save vulnerability table outputs
gtsave(gt_vuln_table, filename = file.path(output_dir, "table_vulnerability.html"))

tryCatch({
  gtsave(gt_vuln_table, filename = file.path(output_dir, "table_vulnerability.png"))
  cat("Vulnerability Table saved to PNG successfully!\n")
}, error = function(e) {
  cat("gtsave as PNG failed: ", e$message, "\n")
})


