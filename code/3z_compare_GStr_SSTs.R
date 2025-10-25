
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


#----------------- Spatial Processing-------------------------

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

CMIP_spring <- CMIP_hist %>%
  bind_rows(CMIP_proj) %>%
  bind_rows(CMIP_proj85) %>%
  mutate(year = as.numeric(year)) %>%
  select(year, MAZ_Acrony, matches(pattern)) %>%
  group_by(MAZ_Acrony, year) %>%
  summarise(
    mean_sst = rowMeans(across(matches(paste0(pattern, "$"))), na.rm = TRUE) %>% mean(na.rm = TRUE),
    n = n()
  ) %>%
  ungroup() %>%
  mutate(model = "CMIPQDM") %>%
  filter(!is.na(MAZ_Acrony)) %>%
  data.table() %>%
  select(-SHAPE)




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



### ----- ROM historical summaries and projections

SSC_SST_long <- SSC_SST_sub %>%
  as_tibble() %>%
  select(-c("SHAPE", "FID")) %>%
  pivot_longer(
    cols = c(contains("H"), contains("45"), contains("85")),
    names_to = c("scenario", "month"),
    names_pattern = "SST_([A-Za-z0-9]+)_([0-9]+)",
    values_to = "value"
  ) %>%
  mutate(model = "SSC",
    month = as.numeric(month)) %>%
  filter(MAZ_Acrony == "GStr")

BCCM_SST_long <- BCCM_SST_sub %>%
  as_tibble() %>%
  select(-"SHAPE") %>%
  pivot_longer(
    cols = c(contains("H"), contains("45"), contains("85")),
    names_to = c("scenario", "month"),
    names_pattern = "SST_([A-Za-z0-9]+)_([0-9]+)",
    values_to = "value"
  ) %>%
  mutate(model = "BCCM",
    month = as.numeric(month))

# Convert to two-digit strings
target_str <- paste0("SST_", sprintf("%02d", seq(1, 12)))
# Build regex pattern
pattern <- paste0(target_str, collapse = "|")

CMIP_SST_long <- CMIP_hist %>%
  bind_rows(CMIP_proj) %>%
  bind_rows(CMIP_proj85) %>%
  mutate(year = as.numeric(year)) %>%
  as_tibble() %>%
  filter(year < 2011 | year > 2030) %>%
  group_by(scenario, SHAPE, MAZ_Acrony) %>%
  summarise(across(contains("SST"), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  select(-c("SHAPE")) %>%
  select(scenario, MAZ_Acrony, matches(pattern)) %>%
  pivot_longer(
    cols = c(contains("SST")),
    names_to = c("month"),
    names_pattern = "SST_([0-9]+)",
    values_to = "value"
  ) %>%
  mutate(model = "CMIPQDM",
    month = as.numeric(month))

ROM_SST <- bind_rows(BCCM_SST_long, SSC_SST_long, CMIP_SST_long) %>%
  filter(MAZ_Acrony != "SFj")

ROM_SST_spring_summary <- data.table(ROM_SST) %>%
  filter(month %in% months_choose) %>%
  group_by(model, scenario, MAZ_Acrony) %>%
  summarize(mean_sst = mean(value, na.rm = T),
    SST_05 = quantile(value, 0.05, na.rm = T),
    SST_95 = quantile(value, 0.95, na.rm = T),
    n = n()) %>%
  ungroup()

ROM_SST_annual_summary <- data.table(ROM_SST) %>%
  group_by(model, scenario, MAZ_Acrony) %>%
  summarize(mean_sst = mean(value, na.rm = T),
    SST_05 = quantile(value, 0.05, na.rm = T),
    SST_95 = quantile(value, 0.95, na.rm = T),
    n = n()) %>%
  ungroup()

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



#----------------- Make report------------------------

rmarkdown::render(
  file.path(paths$code, "markdown", "compare_model_SSTs_wCMIP6.Rmd"),
  output_file = paste(today, "SST_comparisons.html", sep = "_"),
  output_dir = here("output"),
  output_format = "html_document")



# Calculate z-scores ------------------------------------------------------


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
