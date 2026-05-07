### 1f_genetics_import.R

## import and process genomic offset and heterozygosity data

# Import preliminary genetics data ----------------------------------------

## Data provided by Tim Healy, not for further distribution at this time
# genetics_sk <- read_csv(file.path(paths$salmon, "Genetics", "sockeye_genomicoffsets_heterozygosity.csv"))
# genetics_ck <- read_csv(file.path(paths$salmon, "Genetics", "chinook_genomicoffsets_heterozygosity.csv"))

#read population level data
genetics_pop <- read_csv(file.path(paths$salmon, "Genetics","offset_het_imputed_dat_sockeye_coho_chinook Mar2026.csv")) %>%
  rename(
    FULL_CU_IN = CU,
    hetzyg = het
  ) %>%
  mutate(FULL_CU_IN = adjust_CU_IN(FULL_CU_IN))


# aggregate populations at the cu level by taking the average
genetics_cu <- genetics_pop %>%
  group_by(species, FULL_CU_IN) %>%
  summarize(
    n_pop_genetics = n(),
    genoff_45_mean = mean(go45, na.rm = T),
    genoff_45_popmin = min(go45, na.rm = T),
    genoff_45_popmax = max(go45, na.rm = T),
    genoff_85_mean = mean(go85, na.rm = T),
    genoff_85_popmin = min(go85, na.rm = T),
    genoff_85_popmax = max(go85, na.rm = T),
    hetzyg_mean = mean(hetzyg, na.rm = T),
    hetzyg_popmin = min(hetzyg, na.rm = T),
    hetzyg_popmax = max(hetzyg, na.rm = T)
  ) %>%
  filter(str_detect(FULL_CU_IN, "US", negate = TRUE)) 
# identify the genetics measure columns to pivot
measure_cols <- grep("^(genoff|hetzyg)", names(genetics_cu), value = TRUE)

genetics_long <- genetics_cu %>%
  pivot_longer(
    cols = all_of(measure_cols),
    names_to = c("indicator", "rcp", "stat"),
    # indicator: genoff or hetzyg
    # rcp: optional digits (e.g., 45, 85); not present for hetzyg
    # stat: mean|popmin|popmax
    names_pattern = "^(genoff|hetzyg)(?:_(\\d+))?_(mean|popmin|popmax)$",
    values_to = "value"
  ) %>%
  mutate(
    gcm         = if_else(indicator == "genoff", 9L, 0L),
    rcp         = if_else(as.character(rcp) == "", "0", rcp),
    period_code = if_else(indicator == "genoff", 3L, 0L),
    dsmodel     = "observed"
  ) %>%
  left_join(select(tbl_indicators, abbrev, category), join_by(indicator == abbrev))


min_go85 <- quantile(genetics_cu$genoff_85_mean)

ggplot() +
  geom_boxplot(data = genetics_cu, aes(y = genoff_45_mean, colour = species))


save(genetics_long, file = file.path(paths$CU, "genetics_dat.Rds"))


str_detect(genetics_cu$FULL_CU_IN, "US")
