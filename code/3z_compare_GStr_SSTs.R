# ==============================================================================
# CVIS Georgia Strait SST Model Comparison (3z_compare_GStr_SSTs.R)
#
# Description:
#   Compares SST summaries in Georgia Strait (GStr) and other Marine Analysis
#   Zones (MAZs) across OISST, HOTSSEA, CMIP6 high-res projections, Salish
#   Sea Cast (SSC), and BCCM. Computes spring anomalies and annual summaries,
#   renders comparison reports, and calculates Z-scores.
#
# Workflow Steps:
#   1. Load setup environment and read spatial boundaries (MAZ).
#   2. Load OISST (from pacea), HOTSSEA, and CMIP6 shapefiles.
#   3. Process spatial intersections and compute spring averages.
#   4. Process ROMs (BCCM and SSC) monthly historical data and projections.
#   5. Summarize spring and annual temperatures across models.
#   6. Render markdown report to compare SST models.
#   7. Calculate Z-scores comparing CMIP6 models to OISST baseline climatology.
#
# Inputs:
#   - Spatial MAZ shapefile and standardized marine data points.
#   - Pacea OISST and HOTSSEA databases.
#
# Outputs:
#   - Rendered HTML report in paths$reports
#
# Dependencies:
#   - Requires 0_setup.R, pacea, sf, dplyr, pivot_longer, rmarkdown
# ==============================================================================

# ==================== 1. Setup & Load Datasets ====================
library(here)
setwd(here())
source(file.path(here(), "code", "0_setup.R"))
library(pacea)


# months to use for summarizing ocean entry SSTs
months_choose <- c(4, 5, 6, 7)
# Convert to two-digit strings
target_str <- paste0("SST_", sprintf("%02d", months_choose))
# Build regex pattern
pattern <- paste0(target_str, collapse = "|")  # "03|04|05"

# read marine adaptive zones
MAZ     <- st_read(file.path(paths$spatial, "MAZ", "MAZ_Final.shp"))
# read OISST data from pacea
oisst_month <- st_transform(oisst_month, crs = "EPSG:3005")

# download hotssea data  - use hotssea_all_variables()
# after downloading get hotssea SST
hotssea_SST <- hotssea_surface_temperature_mean() %>%
  pivot_longer(cols = c(`1980_1`:`2018_12`),
    names_to = c("year", "month"),
    names_sep = "_",
    values_to = "sst")

# subset to GStr MAZ (exclude Juan de Fuca, Johnstone Strait)
hotssea_SST_GStr <- hotssea_SST[MAZ[MAZ$MAZ_Acrony == "GStr", ], ]

# read CMIP6 high resolution SST
CMIP_hist <- st_read(file.path(paths$climate, "Standardized_Marine_data/Points/CMIP6_ssp245_SST_1980-2020.gdb")) %>%
  mutate(scenario = "H")
CMIP_proj <- st_read(file.path(paths$climate, "Standardized_Marine_data/Points/CMIP6_ssp245_SST_2046-2065.gdb")) %>%
  mutate(scenario = "45")
CMIP_proj85 <- st_read(file.path(paths$climate, "Standardized_Marine_data/Points/CMIP6_ssp585_SST_2046-2065.gdb")) %>%
  mutate(scenario = "85")


# Salish Sea Cast
SSC_SST_sub <- read_sf(file.path(paths$climate, "Standardized_Marine_data/Points/SSC_SST_sub.gdb"))
# BCCM
BCCM_SST_sub <- read_sf(file.path(paths$climate, "Standardized_Marine_data/Points/BCCM_SST_sub.gdb"))


# ==================== 2. Spatial Processing & Historic Climatology ====================

# match oisst points to MAZ polygons
oisst_month$MAZ_Acrony <- NA
container <- st_contains(MAZ, oisst_month)
for (i in 1:length(container)) {
  if (length(container[[i]]) > 0) {
    oisst_month$MAZ_Acrony[container[[i]]] <- as.character(MAZ$MAZ_Acrony[i])
  }
}

oisst_spring <- oisst_month %>%
  filter(month %in% months_choose, year < 2025) %>%
  group_by(MAZ_Acrony, year) %>%
  summarise(mean_sst = mean(sst, na.rm = TRUE),
    n = n()) %>%
  ungroup()  %>%
  mutate(model = "OISST") %>%
  filter(!is.na(MAZ_Acrony)) %>%
  data.table() %>%
  select(-geometry)

hotssea_SST_spring <- hotssea_SST_GStr %>%
  filter(month %in% months_choose) %>%
  group_by(year) %>%
  summarise(mean_sst = mean(sst, na.rm = TRUE),
    n = n()) %>%
  ungroup() %>%
  mutate(model = "HOTSSEA",
    MAZ_Acrony = "GStr") %>%
  filter(!is.na(MAZ_Acrony)) %>%
  data.table() %>%
  select(-geometry)



# Regex that covers SST_01..12, SST_p10_01..12, SST_p90_01..12
s_cols <- "^SST(?:_(?:p10|p90))?_\\d{2}$"

CMIP_monthly <- CMIP_hist %>%
  bind_rows(CMIP_proj, CMIP_proj85) %>%
  mutate(year = as.numeric(year)) %>%
  # keep only the needed columns
  select(year, MAZ_Acrony, scenario, matches(s_cols)) %>%
  filter(!is.na(MAZ_Acrony)) %>%
  # 1) SPATIAL AGGREGATION: collapse many rows per MAZ_Acrony-year-(scenario)
  group_by(MAZ_Acrony, year, scenario) %>%
  summarise(
    across(
      .cols = matches(s_cols),
      .fns  = ~ mean(.x, na.rm = TRUE)
    ),
    n_cells = n(),               # how many spatial rows were averaged
    .groups = "drop"
  ) %>%
  # 2) PIVOT LONGER: turn month and stat into variables
  pivot_longer(
    cols = matches(s_cols),
    names_to = c("stat", "month"),
    names_pattern = "SST(?:_(p10|p90))?_(\\d{2})",
    values_to = "value"
  ) %>%
  mutate(
    stat  = if_else(stat == "", "mean", stat),  # plain SST_* are 'mean'
    month = as.integer(month),
    model = "CMIPQDM"
  ) %>%
  arrange(MAZ_Acrony, year, month, stat)


#aggregate over periods for comparing to ROMs
CMIP_period <- CMIP_monthly %>%
  group_by(MAZ_Acrony, scenario, stat, month) %>%
  summarise(value = mean(value, na.rm = TRUE), 
            n = n(),
            .groups = "drop")


#extract spring means
CMIP_spring <- CMIP_monthly %>%
  filter(month %in% months_choose, stat == "mean") %>%
  group_by(MAZ_Acrony, year) %>%
  summarise(value = mean(value, na.rm = TRUE), 
            n = n(),
            .groups = "drop")


# combine SST values
combined_spring <-  rbind(oisst_spring, hotssea_SST_spring, CMIP_spring, ignore.attr = TRUE) %>%
  mutate(year = as.numeric(year)) %>%
  filter(year > 1980, year < 2020)

model_means_spring <- combined_spring %>%
  filter(MAZ_Acrony %in% c("WVI", "GStr", "Offshore")) %>%
  group_by(MAZ_Acrony, model) %>%
  summarise(overall_mean_sst = mean(mean_sst, na.rm = TRUE), .groups = "drop")

spring_means <- combined_spring %>%
  group_by(MAZ_Acrony) %>%
  summarize(climatology_sst = mean(mean_sst, na.rm = TRUE))

spring_anoms <- combined_spring %>%
  left_join(spring_means, by = "MAZ_Acrony") %>%
  mutate(sst_anomaly = mean_sst - climatology_sst)



# ==================== 3. Regional Ocean Modeling System (ROMS) Processing ====================

SSC_SST_long <- SSC_SST_sub %>%
  as_tibble() %>%
  select(-c("SHAPE", "FID")) %>%
  filter(MAZ_Acrony == "GStr") %>%
  pivot_longer(
    cols = c(contains("H"), contains("45"), contains("85")),
    names_to = c("scenario", "month"),
    names_pattern = "SST_([A-Za-z0-9]+)_([0-9]+)",
    values_to = "value"
  ) %>%
  group_by(scenario, month, MAZ_Acrony) %>%
  summarise(
    SST_mean    = mean(value, na.rm = TRUE),
    SST_qlowsp   = as.numeric(quantile(value, probs = qlowsp, na.rm = TRUE)),
    SST_qhighsp  = as.numeric(quantile(value, probs = qhighsp, na.rm = TRUE)),
    .groups   = "drop"
  ) %>%
  # reshape to long to get a 'stat' column
  pivot_longer(
    cols = c(SST_mean, SST_qlowsp, SST_qhighsp),
    names_to = "stat",
    values_to = "value"
  ) %>%
  mutate(
    model = "SSC",
    month = as.numeric(month)
  )

BCCM_SST_long <- BCCM_SST_sub %>%
  as_tibble() %>%
  select(-"SHAPE") %>%
  pivot_longer(
    cols = c(contains("H"), contains("45"), contains("85")),
    names_to = c("scenario", "month"),
    names_pattern = "SST_([A-Za-z0-9]+)_([0-9]+)",
    values_to = "value"
  ) %>%
  group_by(scenario, month, MAZ_Acrony) %>%
  summarise(
    SST_mean     = mean(value, na.rm = TRUE),
    SST_qlowsp   = as.numeric(quantile(value, probs = qlowsp, na.rm = TRUE)),
    SST_qhighsp  = as.numeric(quantile(value, probs = qhighsp, na.rm = TRUE)),
    .groups   = "drop"
  ) %>%
  # reshape to long to get a 'stat' column
  pivot_longer(
    cols = c(SST_mean, SST_qlowsp, SST_qhighsp),
    names_to = "stat",
    values_to = "value"
  ) %>%
  mutate(
    model = "BCCM",
    month = as.numeric(month)
  )


# Regex that covers SST_01..12, SST_p10_01..12, SST_p90_01..12
s_cols <- "^SST(?:_(?:p10|p90))?_\\d{2}$"

#get mean and gcm variation quantiles
CMIP_SST_long <- CMIP_hist %>%
  bind_rows(CMIP_proj) %>%
  bind_rows(CMIP_proj85) %>%
  mutate(year = as.numeric(year)) %>%
  as_tibble() %>%
  filter(year < 2011 | year > 2030) %>%
  group_by(scenario, MAZ_Acrony) %>%
  summarise(
    across(
      .cols = matches(s_cols),
      .fns  = ~ mean(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  ) %>%
  # 2) PIVOT LONGER: turn month and stat into variables
  pivot_longer(
    cols = matches(s_cols),
    names_to = c("stat", "month"),
    names_pattern = "SST(?:_(p10|p90))?_(\\d{2})",
    values_to = "value"
  ) %>%
  mutate(
    stat  = if_else(stat == "", "SST_mean", stat),
    stat  = if_else(stat == "p10", "SST_qlowgcm", stat),
    stat  = if_else(stat == "p90", "SST_qhighgcm", stat),
    month = as.integer(month),
    model = "CMIPQDM"
  ) %>%
  arrange(MAZ_Acrony,  month, stat)

#get spatial variation quantiles
CMIP_SST_sp <- CMIP_hist %>%
  bind_rows(CMIP_proj, CMIP_proj85) %>%
  mutate(year = as.numeric(year)) %>%
  as_tibble() %>%
  filter(year < 2011 | year > 2030) %>%
  # 1) Summarise across spatial rows: mean + spatial quantiles
  group_by(scenario, MAZ_Acrony) %>%
  summarise(
    across(
      .cols = matches(s_cols),
      .fns = list(
        qlowsp   = ~ as.numeric(quantile(.x, probs = 0.10, na.rm = TRUE)),
        qhighsp  = ~ as.numeric(quantile(.x, probs = 0.90, na.rm = TRUE))
      ),
      .names = "{.fn}_{.col}"
    ),
    .groups = "drop"
  ) %>%
  # 2) Pivot longer: combine mean, qlowsp, qhighsp for each month
  pivot_longer(
    cols = matches("^(mean|qlowsp|qhighsp)_SST"),
    names_to = c("stat", "month"),
    names_pattern = "(mean|qlowsp|qhighsp)_SST_(\\d{2})",
    values_to = "value"
  ) %>%
  # 3) Map stat names to final labels
  mutate(
    stat = case_when(
      stat == "mean"    ~ "SST_mean",
      stat == "qlowsp"  ~ "SST_qlowsp",
      stat == "qhighsp" ~ "SST_qhighsp"
    ),
    month = as.integer(month)
  ) %>%
  filter(!is.na(month))

#combine
CMIP_SST_both <- bind_rows(CMIP_SST_long, CMIP_SST_sp) %>%
  mutate(model = "CMIPQDM") %>%
  arrange(MAZ_Acrony, month, stat)

ROM_SST <- bind_rows(BCCM_SST_long, SSC_SST_long, CMIP_SST_both) %>%
  filter(MAZ_Acrony != "SFj") %>%
  pivot_wider(
    id_cols = c(MAZ_Acrony, scenario, month, model),
    names_from = stat,
    values_from = value,
    names_glue = "{stat}"
  ) 

ROM_SST_spring_summary <- data.table(ROM_SST) %>%
  filter(month %in% months_choose) %>%
  group_by(model, scenario, MAZ_Acrony) %>%
  summarise(
    across(
      .cols = contains("SST"),
      .fns  = ~ mean(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  )

ROM_SST_annual_summary <- data.table(ROM_SST) %>%
  group_by(model, scenario, MAZ_Acrony) %>%
  summarise(
    across(
      .cols = contains("SST"),
      .fns  = ~ mean(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  )

oisst_spring_summary <- oisst_spring %>%
  filter(year < 2010) %>%
  mutate(scenario = "H") %>%
  group_by(model, scenario, MAZ_Acrony) %>%
  summarize(mean_sst = mean(mean_sst, na.rm = T)) %>%
  ungroup()

oisst_annual_summary <- oisst_month %>%
  group_by(MAZ_Acrony, year) %>%
  summarise(mean_sst = mean(sst, na.rm = TRUE),
    n = n()) %>%
  ungroup()  %>%
  mutate(model = "OISST") %>%
  filter(!is.na(MAZ_Acrony)) %>%
  data.table() %>%
  select(-geometry) %>%
  filter(year < 2010) %>%
  mutate(scenario = "H") %>%
  group_by(model, scenario, MAZ_Acrony) %>%
  summarize(mean_sst = mean(mean_sst, na.rm = T)) %>%
  ungroup()

ROM_SST_spring_summary <- bind_rows(ROM_SST_spring_summary, oisst_spring_summary)

ROM_SST_annual_summary <- bind_rows(ROM_SST_annual_summary, oisst_annual_summary)

# spring_means_ROMs <- combined_spring_ROMs %>%
#   filter(Model %in% c("ROM_H")) %>%
#   group_by(Region) %>%
#   summarize(climatology_sst = mean(mean_sst, na.rm = TRUE))
#
# combined_spring_anoms <- combined_spring_ROMs %>%
#   left_join(spring_means_ROMs, by = "Region") %>%
#   mutate(sst_anomaly = mean_sst - climatology_sst)



# ==================== 4. Generate Comparative Report ====================

rmarkdown::render(
  file.path(paths$code, "markdown", "compare_model_SSTs_wCMIP6.Rmd"),
  output_file = paste(today, "SST_comparisons.html", sep = "_"),
  output_dir = paths$reports,
  output_format = "html_document")



# ==================== 5. Z-Score Calculation ====================


CMIP_spring_stats <- CMIP_spring %>%
  mutate(scenario = case_when(
    year < 2011 ~ "H",
    year > 2030 ~ "45"
  )) %>%
  group_by(scenario, MAZ_Acrony) %>%
  summarise(
    across(contains("SST"),
      list(mean_sst = ~ mean(.x, na.rm = TRUE),
        sd_sst = ~ sd(.x, na.rm = TRUE)),
      .names = "{.fn}"),
    .groups = "drop"
  )

hotssea_spring_stats <- hotssea_SST_spring %>%
  mutate(scenario = case_when(
    year < 2011 ~ "H",
    year > 2030 ~ "45"
  )) %>%
  group_by(scenario) %>%
  summarise(
    across(contains("SST"),
      list(mean_sst = ~ mean(.x, na.rm = TRUE),
        sd_sst = ~ sd(.x, na.rm = TRUE)),
      .names = "{.fn}"),
    .groups = "drop"
  )

oisst_spring_stats <- oisst_spring %>%
  mutate(scenario = case_when(
    year < 2011 ~ "H",
    year > 2030 ~ "45"
  )) %>%
  group_by(scenario, MAZ_Acrony) %>%
  summarise(
    across(contains("SST"),
      list(mean_sst = ~ mean(.x, na.rm = TRUE),
        sd_sst = ~ sd(.x, na.rm = TRUE)),
      .names = "{.fn}"),
    .groups = "drop"
  ) %>%
  filter(!is.na(scenario)) %>%
  rename(mean_sst_h = mean_sst, sd_sst_h = sd_sst)

CMIP_spring_H <- CMIP_spring_stats %>%
  filter(scenario == "H") %>%
  rename(mean_sst_h = mean_sst, sd_sst_h = sd_sst)


CMIP_spring_z <- CMIP_spring_stats %>%
  left_join(oisst_spring_stats, by = c("MAZ_Acrony")) %>%
  mutate(z_score = (mean_sst - mean_sst_h) / sd_sst_h)
