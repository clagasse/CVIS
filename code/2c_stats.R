

library(tidyverse)
library(sf)
library(Hmisc)

# Weighted mean and quantile helpers
wmean <- function(x, w) Hmisc::wtd.mean(x, w, na.rm = TRUE)
wquant <- function(x, w, probs) Hmisc::wtd.quantile(x, w, probs = probs, na.rm = TRUE)

# CU Summary Function
summarize_cu <- function(cu_run, cu_boundary, fw_amod, stream_cu_picks, reaches_ENM_list) {
  cu_i <- cu_run$FULL_CU_IN[i]
  sp_pick <- cu_run$spp[cu_run$FULL_CU_IN == cu_i] #species abbr
  sp_pick_bcfp <- spp_lookup$spp_abr_bcfp[spp_lookup$spp_abr == sp_pick]  #BCFP species abbr (different for Chinook)
  cu_boundary_i <- cu_boundary[cu_boundary$FULL_CU_IN == cu_i,]

  amod_CU <- fw_amod[stream_cu_picks[, i], ]
  reaches_ENM_sp <- reaches_ENM_list[[sp_pick]]
  reaches_ENM_cu <- reaches_ENM_sp[lengths(st_intersects(reaches_ENM_sp, cu_boundary_i)) > 0, ]
  
  total_length <- sum(amod_CU$Shape_Length, na.rm = TRUE)
  
  data.table(
    cuid = cuid_i,
    dur_spn = cu_run$Peak_Spawn_To_Ocean_Entry_Days[i],
    length_sum = total_length,
    avg_length = mean(amod_CU$Shape_Length, na.rm = TRUE),
    avg_order = mean(amod_CU$STREAM_ORDER, na.rm = TRUE),
    n_streams = nrow(amod_CU),
    cu_area = as.numeric(st_area(cu_boundary_i)) / 1e6,
    Tw8_0_00_0 = wmean(amod_CU$Tw8_0_00_0, amod_CU$Shape_Length),
    Tw8_0_00_1 = wmean(amod_CU$Tw8_0_00_1, amod_CU$Shape_Length),
    
    sd_Tw8_0_00_1 = sqrt(wtd.var(amod_CU$Tw8_0_00_1, amod_CU$Shape_Length)),
    sd_Tw8_9_45_3 = sqrt(wtd.var(amod_CU$Tw8_9_45_3, amod_CU$Shape_Length)),
    CT_anad_mean = wmean(amod_CU$CT_anad, amod_CU$Shape_Length),
    CT_anad_025 = wquant(amod_CU$CT_anad, amod_CU$Shape_Length, 0.05),
    CT_anad_975 = wquant(amod_CU$CT_anad, amod_CU$Shape_Length, 0.95),
    SPN_ENM_fav_hist = mean(reaches_ENM_cu$Fav_f.0_00_1, na.rm = TRUE),
    SPN_ENM_fav_proj = mean(reaches_ENM_cu$Fav_f.9_45_3, na.rm = TRUE),
    SPN_ENM_fav_diff = mean(reaches_ENM_cu$Fav_f.9_45_3, na.rm = TRUE) - mean(reaches_ENM_cu$Fav_f.0_00_1, na.rm = TRUE)
  )
}


# Assume reaches_ENM_list is a named list of ENM spatial objects by species
cu_stats <- map_dfr(1:nrow(cu_run), summarize_cu,
                    cu_run = cu_run,
                    cu_boundary = cu_boundary,
                    fw_amod = fw_amod,
                    stream_cu_picks = stream_cu_picks,
                    reaches_ENM_list = list(
                      CH = reaches_ENM_CH,
                      CO = reaches_ENM_CO,
                      PK = reaches_ENM_PK,
                      CM = reaches_ENM_CM,
                      SK = reaches_ENM_SK
                    ))





# Define model groups
fwT_models <- list(
  Tw8_00_0 = grep("^Tw8.*00_0$", names(fwT_cu), value = TRUE),  # ^starts with  $ends with
  Tw8_00_1 = grep("^Tw8.*00_1$", names(fwT_cu), value = TRUE),
  Tw8_45_3 = grep("45_3", names(fwT_cu), value = TRUE),
  Tw8_45_4 = grep("45_4", names(fwT_cu), value = TRUE),
  Tw8_45_5 = grep("45_5", names(fwT_cu), value = TRUE),
  Tw8_85_3 = grep("85_3", names(fwT_cu), value = TRUE),
  Tw8_85_4 = grep("85_4", names(fwT_cu), value = TRUE),
  Tw8_85_5 = grep("85_5", names(fwT_cu), value = TRUE)
)



summarize_weighted <- function (cols, dt, weights_col) {
  weights <- dt[[weights_col]]
  
  list(
    mean = map_dbl(cols, ~ Hmisc::wtd.mean(dt[[.x]], weights, na.rm = TRUE)),
    sd = map_dbl(cols, ~  sqrt(Hmisc::wtd.var(dt[[.x]], weights, na.rm = TRUE))),
    q05 = map_dbl(cols, ~ Hmisc::wtd.quantile(dt[[.x]], weights, probs = 0.05, na.rm = TRUE)),
    q95 = map_dbl(cols, ~ Hmisc::wtd.quantile(dt[[.x]], weights, probs = 0.95, na.rm = TRUE))
  )
}


fwT_stats <- map(fwT_models, summarize_weighted, dt = fwT_cu, weights_col = "length_metre") 

# Function to summarize a group of model columns with weights
summarize_weighted_models <- function(dt, model_groups, weights_col) {
  weights <- dt[[weights_col]]
  
  map_dfr(names(model_groups), function(model_name) {
    cols <- model_groups[[model_name]]
    
    stats <- list(
      model = model_name,
      mean = map_dbl(cols, ~ Hmisc::wtd.mean(dt[[.x]], weights, na.rm = TRUE)),
      sd = map_dbl(cols, ~ Hmisc::wtd.sd(dt[[.x]], weights, na.rm = TRUE)),
      q05 = map_dbl(cols, ~ Hmisc::wtd.quantile(dt[[.x]], weights, probs = 0.05, na.rm = TRUE)),
      q95 = map_dbl(cols, ~ Hmisc::wtd.quantile(dt[[.x]], weights, probs = 0.95, na.rm = TRUE))
    )
    
    # Combine into a tidy data.table
    data.table(
      model = model_name,
      variable = cols,
      mean = stats$mean,
      sd = stats$sd,
      q05 = stats$q05,
      q95 = stats$q95
    )
  })
}

summarize_weighted_models(fwT_cu, fwT_models, "Shape_Length")



# Apply to each model
fwT_stats <- map(fwT_cu, fwT_models, summarize_weighted)



library(data.table)
library(purrr)

# Example data.table
dt <- data.table(
  model1_a = rnorm(100),
  model1_b = rnorm(100),
  model1_c = rnorm(100),
  model1_d = rnorm(100),
  model1_e = rnorm(100),
  model1_f = rnorm(100),
  model2_a = rnorm(100),
  model2_b = rnorm(100),
  model2_c = rnorm(100),
  model2_d = rnorm(100),
  model2_e = rnorm(100),
  model2_f = rnorm(100)
)

# Define model groups
model_groups <- list(
  model1 = grep("^model1_", names(dt), value = TRUE),
  model2 = grep("^model2_", names(dt), value = TRUE)
)

# Function to compute stats for a group of columns
summarize_model <- function(cols, dt) {
  list(
    mean = dt[, lapply(.SD, mean, na.rm = TRUE), .SDcols = cols],
    sd = dt[, lapply(.SD, sd, na.rm = TRUE), .SDcols = cols],
    q05 = dt[, lapply(.SD, quantile, probs = 0.05, na.rm = TRUE), .SDcols = cols],
    q95 = dt[, lapply(.SD, quantile, probs = 0.95, na.rm = TRUE), .SDcols = cols]
  )
}

# Apply to each model
model_stats <- map(model_groups, summarize_model, dt = dt)


