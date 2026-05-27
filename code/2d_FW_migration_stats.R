# ==============================================================================
# CVIS Freshwater Migration Statistics (2d_FW_migration_stats.R)
#
# Description:
#   Loads stream-level migration paths and PCIC climate grids, and calculates
#   upstream migration exposure indicators (water temperature and discharge changes)
#   for each salmon CU. Uses a dynamic temporal migration window based on run timing
#   and spawning peak dates.
#
# Workflow Steps:
#   1. Load setup environment and configuration values.
#   2. Load CU paths spatial metadata and PCIC daily netcdf databases.
#   3. Define moving timing-window exposure helper functions.
#   4. Iterate through RCP scenarios and CUs to map spatial grid cell exposure.
#   5. Aggregate daily, decadal, and GCM uncertainty statistics.
#   6. Save output datasets to processed_data/freshwater/.
#
# Inputs:
#   - processed_data/freshwater/fw_upstream_paths.Rdata (migration paths by CU)
#   - PCIC climate databases (daily netcdfs) in 0_data_climate/PCIC_averaged/combined/
#
# Outputs:
#   - processed_data/freshwater/[date]_migr_stats.Rdata
#
# Dependencies:
#   - Requires 2c_FW_upstream_paths.R to have been executed.
# ==============================================================================

# ==================== 1. Setup and Environment ====================

library(here)
setwd(here())
source(file.path(here(), "code", "0_setup.R"))

#label for period variable used as baseline when calculating difference in Q
baseline_pick <- "1981-2010"

#rcps to iterate
rcp_iter <- c("45", "85")

#name of downscale model type
dsmodel_name <- "pcicgrid"

# ==================== 2. Load Spatial Objects ====================
# load CU paths
load(file.path(paths$fw, "fw_upstream_paths.Rdata"))

# load PCIC daily outputs
PCIC_file_loc <- file.path(paths$climate, "PCIC_averaged", "combined")



# ==================== 3. Helper Functions for Indicators ====================


#function to get an index of which streams to include for each day of the year based on upstream migration distance
get_migration_index <- function(migr_cu,
                                PCIC_base, 
                                migr_s, # start date for migration window
                                migr_e, # end date for migration window
                                s_rate,  # movement rate of head of migration window
                                e_rate,  # movement rate of tail of migration window
                                migrdist,
                                intersection_df,  #mapping table between PCIC grid cell and stream index
                                win_start_dist = 50000  #minimum upstream distance to include at start of migration window
)
{
  migr_win <- matrix(NA, nrow = nrow(migr_cu), ncol = 365)  #stream/day matrix
  head_dist <- vector("numeric", 365)  #daily upstream distance to use as head of migration window
  tail_dist <- vector("numeric", 365)  #daily upstream distance to use as tail of migration window
  PCIC_ind <- matrix(NA, nrow = dim(PCIC_base)[["x"]] * dim(PCIC_base)[["y"]], ncol = 365)
  #PCIC_pp <- matrix(NA, nrow = dim(PCIC_base)[["x"]] * dim(PCIC_base)[["y"]], ncol = 365)
  
  for (day in 1:ncol(migr_win)) {
    # upper range of migration window = #days since rt_start x front movement rate
    head_dist[day] <- min((day - migr_s) * s_rate, migrdist)
    head_dist[day] <- max(head_dist[day], win_start_dist)  #remove potential negative values
    tail_dist[day] <- min((day - migr_e) * e_rate, migrdist)
    tail_dist[day] <- max(tail_dist[day], 0)
    
    # stream segments within migration window distance each day
    migr_win[, day] <- migr_cu$downstream_distance <= head_dist[day] & migr_cu$downstream_distance >= tail_dist[day]
    # turn T/F into index of streams
    migr_win_index <- which(migr_win[, day] == TRUE)
    # if day is outside the migration timing window (rt_start and sp_peak), make NA
    if (day > migr_e | day < migr_s) migr_win_index <- NULL
    
    # use mapping table to convert stream index into PCIC grid cell index
    int_day <- intersection_df$grid_id[intersection_df$line_id %in% migr_win_index]
    
    # join with prop_path value
    prop_day <- bind_cols(grid_id = int_day,
                          prop_path = intersection_df$prop_path[intersection_df$line_id %in% migr_win_index])
    
    int_day <- unique(int_day)
    
    # get T/F index for each PCIC grid cell each day
    PCIC_ind[, day] <- seq(1, dim(PCIC_base)[["x"]] * dim(PCIC_base)[["y"]]) %in% int_day
    
    #       ## Remainder of this for loop is for implementing prop_path weighting, not currently working
    #       # get average prop_path for each grid cell
    #       # prop_avg <- prop_day %>%
    #       #   group_by(grid_id) %>%
    #       #   summarize(prop_avg = mean(prop_path, na.rm = T)) %>%
    #       #   ungroup()
    #       #
    #       # #create a vector corresponding to total number of grid cells,
    #       # # fill it with prop_path value using index of intersecting cells
    #       # vec <- rep(NA, dim(PCIC_base)[["x"]] * dim(PCIC_base)[["y"]])
    #       # vec[prop_avg[[1]]] <- prop_avg[[2]]
    #       #
    #       # #get prop_paths value for each PCIC grid cell each day
    #       # PCIC_pp[,day] <- vec
  }
  
  return(PCIC_ind)
  
}


# function to produce summary across spatial dimensions but maintaining gcms, periods, and days of year for a stars attribute
summarize_attribute <- function(data,
                                 attr_name = "discharge",
                                 cu_name,
                                 rcp_pick = "45",
                                 baseline_period = baseline_pick,
                                 qlowgcm = qlgcm,
                                 qhighgcm = qhgcm) {

  ## a. SPATIAL DIMENSION -----
  
  # time x period x model array
  spatial_avg <- st_apply(
    data[attr_name],
    MARGIN = c("time", "period", "model"),
    FUN = mean,
    na.rm = TRUE
  )[[1]]
  
  # label dimensions
  dimnames(spatial_avg) <- list(
    time   = st_get_dimension_values(data, "time"),
    period = st_get_dimension_values(data, "period"),
    model  = st_get_dimension_values(data, "model")
  )
  
  # array -> long tibble: time, period, model, <attr_name>
  spatial_long <- as.data.frame(
    as.table(spatial_avg),
    stringsAsFactors = FALSE
  ) |>
    tibble::as_tibble() |>
    rlang::set_names(c("time", "period", "model", attr_name))
  
  # nest ONLY time + value, then add period/model into each nested tibble
  spatial_nested <- spatial_long |>
    dplyr::group_by(period, model) |>
    tidyr::nest(time = c(time, !!rlang::sym(attr_name))) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      FULL_CU_IN = cu_name,
      rcp        = rcp_pick,
      attr       = attr_name
    ) |>
    dplyr::relocate(FULL_CU_IN, rcp, attr, period, model, time)
  
  ## b. BASELINE VALUES (PER MODEL AND TIME/DOY) -----
  
  baseline_tbl <- spatial_nested |>
    dplyr::filter(period == baseline_period)
  
  if (nrow(baseline_tbl) == 0) {
    stop("No rows found for baseline_period = '", baseline_period,
         "'. Available periods: ",
         paste(unique(spatial_nested$period), collapse = ", "))
  }
  
  # Extract baseline data: model + time-specific values
  baseline_values <- baseline_tbl |>
    dplyr::select(model, time) |>
    tidyr::unnest(time) |>
    dplyr::select(model, time, baseline_value = !!rlang::sym(attr_name))
  
  ## c. ADD PROPORTIONAL DIFFERENCES INTO EACH NESTED DATAFRAME -----
  
  spatial_nested <- spatial_nested |>
    dplyr::mutate(
      time = purrr::pmap(
        list(time, model, period),
        \(df, m, p) {
          # Get baseline values for this model
          baseline_for_model <- baseline_values |>
            dplyr::filter(model == m) |>
            dplyr::select(time, baseline_value)
          
          # Join baseline values by time (day-of-year)
          df |>
            dplyr::left_join(baseline_for_model, by = "time") |>
            dplyr::mutate(
              period = p,
              model = m,
              pdelta = (!!rlang::sym(attr_name) - baseline_value) / baseline_value,
              .before = 1
            )
        }
      )
    )
  
  spatial_nested
}


# ==================== 4. Calculate Upstream Migration Stats ====================

cu_migr_stats <- list() # list to store cu migration characteristics
migrT_rcps <- list()  # list to store temperature results
migrQ_rcps <- list()  # list to store discharge results
#migrA21_rcps <- list()

# Select CUs/paths to use for the calendar list
calendar_cu_select <- c("PKO-01", "CK-12")

# List to accumulate daily calendar stats per RCP
migr_cal_list <- list()

#loop over rcp scenarios
for (j in 1:length(rcp_iter)) {
  
  cat(paste("Starting migration stats for RCP", rcp_iter[j]))
  
  PCIC_file_name <- paste0("daily_rcp", rcp_iter[j], ".nc")
  
  PCIC_daily <- read_mdim(file.path(PCIC_file_loc, PCIC_file_name)) 
  PCIC_daily <- mutate(PCIC_daily, waterTemperature = waterTemperature - 273.15)

  # Calculate daily calendar stats for selected CUs (all 365 days, stream order >= 8)
  rcp_cal_stats <- list()
  for (cu_name in calendar_cu_select) {
    path_cu <- migr_list[[cu_name]] %>%
      filter(stream_order >= 8) %>%
      st_transform(4269)
      
    if (nrow(path_cu) == 0) {
      warning(paste("No stream segments with order >= 8 for", cu_name))
      next
    }
    
    PCIC_cu_cal <- PCIC_daily[path_cu]
    
    t_stats <- summarize_attribute(PCIC_cu_cal, attr_name = "waterTemperature", cu_name = cu_name, rcp_pick = rcp_iter[j])
    q_stats <- summarize_attribute(PCIC_cu_cal, attr_name = "discharge", cu_name = cu_name, rcp_pick = rcp_iter[j])
    
    t_flat <- t_stats %>%
      select(-attr) %>%
      mutate(time = map(time, ~select(.x, -period, -model))) %>%
      unnest(time) %>%
      select(spatial_path = FULL_CU_IN, rcp, period, model, day_of_year = time, migrTproj = waterTemperature)
      
    q_flat <- q_stats %>%
      select(-attr) %>%
      mutate(time = map(time, ~select(.x, -period, -model))) %>%
      unnest(time) %>%
      select(spatial_path = FULL_CU_IN, rcp, period, model, day_of_year = time, migrQpdelta = pdelta)
      
    cu_flat <- left_join(t_flat, q_flat, by = c("spatial_path", "rcp", "period", "model", "day_of_year"))
    
    rcp_cal_stats[[cu_name]] <- cu_flat
  }
  
  migr_cal_list[[j]] <- bind_rows(rcp_cal_stats)

  migrT_all <- list()  # list to store temperature results
  migrQ_all <- list()  # list to store discharge results
  #migrA21_all <- list()
  
  if(j == 1) {
    # get base PCIC grid - smaller object quicker for operations
    PCIC_base <- PCIC_daily[, , , 1, 1, 1]
    
    periods    <- st_get_dimension_values(PCIC_daily, "period")
    gcm_models <- st_get_dimension_values(PCIC_daily, "model")
  }

  #loop over CUs
  for (i in 1:n.CUs) {
    cu_i <- cu_run$FULL_CU_IN[i]
    migr_cu <- migr_list[[cu_i]] %>%
      st_transform(4269)
    
    migr_cu_main <- filter(migr_cu, prop_paths == 1)   # select only paths leading to all NuSEDS sites

    # get cu timing and calculate movement rates for migration window
    cu_timing_i <- cu_timing_Fr[cu_timing_Fr$FULL_CU_IN == cu_i, ] %>%
      select(FULL_CU_IN, rt_start, rt_end, sp_start, sp_peak, sp_end) %>%
      mutate(
        rt_to_sp_s = sp_start - rt_start,  # days between spawn timing start and run timing start, used to calculate movement rate of front
        rt_to_sp_p = sp_peak - rt_end,     # days from spawn timing peak to run timing end, used to calculate movement of tail
        migr_s     = round(rt_start),           # start date for migration window used to calculate exposure - set to run timing start
        migr_e     = round(sp_peak),            # end date for migration window - set to peak spawn timing
        migr_dur   = round(migr_e - migr_s),
        migrdist   = round(sum(migr_cu$length_metre * migr_cu$prop_paths)),  # weighted average migration distance
        s_rate     = migrdist / (rt_to_sp_s),  # movement rate of front of migration window
        e_rate     = migrdist / (rt_to_sp_p),  # movement rate of tail of migration window
        
        max_dist   = max(migr_cu$downstream_distance, na.rm = T),
        main_dist  = sum(migr_cu_main$length_metre), # distance in common for all sites
        main_elev  = max(st_coordinates(migr_cu_main)[, 3]),  # highest point in common for all sites
        max_elev   = max(st_coordinates(migr_cu)[, 3]),     # highest point along all of migration path (ie. highest NUSEDS site)
        max_work   = max(migr_cu$work_upstream, na.rm = T),       #highest work of any point along path
        main_work  = max(migr_cu_main$work_upstream, na.rm =T)   #highest work for point along all migration path
        )     
    
    if (sum(!is.na(migr_cu)) == 0 |   # skip if path is empty
      is.na(cu_timing_i$migr_s) | is.na(cu_timing_i$migr_e)) { # skip if timing info missing

      cu_migr_stats[[i]]   <- NA
      migrT_all[[i]]       <- NA
      migrQ_all[[i]]       <- NA
      #migrA21_all[[i]]  <- NA

      next
    }

    ### GET MIGRATION WINDOW
    # Create a moving migration window over the cu path and input into stars array
    # get index of grid cells that intersect with each stream segment
    intersections <- st_intersects(PCIC_base, migr_cu)

    # create a mapping table between grid cell index and stream index that intersect, with prop_path
    intersection_df <- data.frame(
      grid_id = rep(seq_along(intersections), lengths(intersections)),
      line_id = unlist(intersections),
      prop_path = migr_cu$prop_paths[unlist(intersections)]
    )
    
    
    PCIC_ind <- get_migration_index(migr_cu,
                        PCIC_base, 
                        migr_s = cu_timing_i$migr_s, # start date for migration window
                        migr_e = cu_timing_i$migr_e, # end date for migration window
                        s_rate = cu_timing_i$s_rate,  # movement rate of head of migration window
                        e_rate = cu_timing_i$e_rate,  # movement rate of tail of migration window
                        migrdist = cu_timing_i$migrdist,
                        intersection_df)  #mapping table between PCIC grid cell and stream index

    daily_cells <- apply(PCIC_ind, 2, sum, na.rm = T)  # vector of number of grid cells included each day
    # reshape flattened PCIC index by grid cells into array with same dimensions as stars object
    doy_int <- array(PCIC_ind, dim = dim(PCIC_daily))

    # put the array into the stars object as a new attribute
    PCIC_daily$migration_window <- doy_int

    ### SUBSET grid to migration path and timing window -----
    PCIC_cu <- PCIC_daily[migr_cu] %>%   # subset cells that intersect with migr_cu
      filter(time >= cu_timing_i$migr_s, time <= cu_timing_i$migr_e) # filter out values outside migration window

    # new T or Q attribute with days outside of migration window turned to NA
    PCIC_cu$migrT <- PCIC_cu$waterTemperature
    PCIC_cu$migrT[!PCIC_cu$migration_window] <- NA

    PCIC_cu$migrQ <- PCIC_cu$discharge
    PCIC_cu$migrQ[!PCIC_cu$migration_window] <- NA

    ## Logical for whether cell temperature is above threshold on each day
    #PCIC_cu$win_Tthr21 <- PCIC_cu$win_T > 21

    ### NOT WORKING - prop_path weighting
    # weight the grid cells based on the proportion of NUSEDS spawning sites the stream segments leads to
    # i.e. terminal segments that only lead to one site out of many for a CU have lower weighting
    # daily_prop <- apply(PCIC_pp, 2, sum, na.rm = T)  #vector of summed weight of grid cells included each day
    # doy_prop <- array(PCIC_pp, dim = dim(PCIC_cu))
    # PCIC_cu$weighted_window  <- doy_prop
    # #calculate weighted temperature and discharge, only including days within migration window
    # PCIC_cu$weighted_T <- PCIC_cu$waterTemperature * PCIC_cu$weighted_window
    #
    # daily_prop <- daily_prop[time_vals]
    #
    # ### problem step - use st_apply to take sum of weighted_T across spatial dimensions then divide by summed weighting
    # # Get weighted average over x and y — keeping period, day of year, and model
    # PCIC_avg_pp <- st_apply(
    #   PCIC_cu[c("weighted_T")],  # variable to calc
    #   MARGIN = c("period", "model", "time"),  # dimensions to keep
    #   FUN = function(x) {
    #     sum(x, na.rm = T)
    #   }
    # )
    # PCIC_avg_pp <- st_apply(
    #   PCIC_avg_pp,  # variable to calc
    #   MARGIN ="time",  # dimensions to keep
    #   FUN = function(x, time_index) {
    #     for(i in seq_along(time_index)) x / daily_prop[i]
    #   },
    #   time_index = st_get_dimension_values(PCIC_avg_pp, "time")
    # )


    ### AGGREGATE OVER DIMENSIONS to get summary statistics------

    migrT_i <- summarize_attribute(PCIC_cu,
      attr_name = "migrT",
      cu_name = cu_i,
      rcp = rcp_iter[j])

    migrQ_i <- summarize_attribute(PCIC_cu,
      attr_name = "migrQ",
      cu_name = cu_i,
      rcp = rcp_iter[j])
    
    migr_bind <- bind_rows(migrT_i, migrQ_i)
    
    if(i == 1 & j == 1)  {
      migr_daily_all <- migr_bind
      cu_migr_timing <- cu_timing_i 
    }  else {
      migr_daily_all <- bind_rows(migr_daily_all, migr_bind)
    }
    
    cu_migr_timing[i,] <- cu_timing_i

    print(paste("Migration stats complete for:", cu_i))

  }

}

# Combine RCP iterations and format columns for consistency
migr_daily_calendar <- bind_rows(migr_cal_list) %>%
  mutate(day_of_year = as.integer(day_of_year)) %>%
  rename(gcm_name = any_of("model")) %>%
  mutate(dsmodel = dsmodel_name,
         gcm_name = gcm_name %>%
                  str_to_lower() %>%
                  str_replace("[-\\.].*$", ""))   %>% 
  left_join(select(period_lookup, period_code, period, dsmodel), 
            by = c("period", "dsmodel")) %>%
  left_join(gcm_codes, by = c("gcm_name"))

#rename and reformat columns for consistency with other workflows
migr_daily_all <- migr_daily_all %>%
  rename(gcm_name = any_of("model")) %>%
  mutate(dsmodel = dsmodel_name,
         gcm_name = gcm_name %>%
                  str_to_lower() %>%
                  str_replace("[-\\.].*$", ""))   %>% # remove dash or dot and the rest 
  left_join(select(period_lookup, period_code, period, dsmodel), #add period_code using mapping
            by = c("period", "dsmodel")) %>%
  left_join(gcm_codes, by = c("gcm_name")) 

# Calculate ensemble GCM for migr_daily_all (taking mean across other GCMs)
migr_daily_all_unnested <- migr_daily_all %>%
  mutate(time = map(time, ~select(.x, -any_of(c("period", "model"))))) %>%
  unnest(time)

migr_daily_all_ensemble <- migr_daily_all_unnested %>%
  group_by(FULL_CU_IN, rcp, attr, period, period_code, dsmodel, time) %>%
  summarise(
    migrT = if (first(attr) == "migrT") mean(migrT, na.rm = TRUE) else NA_real_,
    migrQ = if (first(attr) == "migrQ") mean(migrQ, na.rm = TRUE) else NA_real_,
    baseline_value = mean(baseline_value, na.rm = TRUE),
    pdelta = mean(pdelta, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    gcm_name = "ensemble",
    gcm = "9",
    period_inner = period,
    model_inner = "ensemble"
  )

migr_daily_all_ensemble_nested <- migr_daily_all_ensemble %>%
  group_by(FULL_CU_IN, rcp, attr, period, period_code, dsmodel, gcm_name, gcm) %>%
  nest(time_df = c(period_inner, model_inner, time, pdelta, migrT, migrQ, baseline_value)) %>%
  ungroup() %>%
  mutate(time = map2(time_df, attr, ~ {
    df <- .x %>% rename(period = period_inner, model = model_inner)
    if (.y == "migrT") {
      df %>% select(-migrQ)
    } else {
      df %>% select(-migrT)
    }
  })) %>%
  select(-time_df)

migr_daily_all <- bind_rows(migr_daily_all, migr_daily_all_ensemble_nested)

# Calculate ensemble GCM for migr_daily_calendar
migr_daily_calendar_ensemble <- migr_daily_calendar %>%
  group_by(spatial_path, rcp, period, period_code, dsmodel, day_of_year) %>%
  summarise(
    migrTproj = mean(migrTproj, na.rm = TRUE),
    migrQpdelta = mean(migrQpdelta, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    gcm_name = "ensemble",
    gcm = "9"
  )

migr_daily_calendar <- bind_rows(migr_daily_calendar, migr_daily_calendar_ensemble)


  # extract stats across doy within nested rows (excluding ensemble rows to avoid double-counting)
migr_all <- migr_daily_all %>%
  filter(gcm_name != "ensemble") %>%
  rowwise() %>%
  mutate(
    # mean of the column named by `attr` in the nested tibble
    proj = {
      tbl <- time
      col <- attr
      if (!is.null(col) && nzchar(col) && col %in% names(tbl)) {
        mean(tbl[[col]], na.rm = TRUE)
      } else {
        NA_real_
      }
    },
    # mean of pdelta if present
    pdelta = if ("pdelta" %in% names(time)) {
      mean(time$pdelta, na.rm = TRUE)
    } else {
      NA_real_
    },
    # build two long rows per input row
    stats = list(tibble(
      indicator = c(paste0(attr, "proj"), paste0(attr, "pdelta")),
      value     = c(proj, pdelta)
    ))
  ) %>%
  ungroup() %>%
  unnest(stats) %>%
  # optionally drop rows we couldn't compute
  filter(!is.na(value)) %>%
  # keep any id columns you need; drop intermediates
  select(-time, -proj, -pdelta, -attr) %>%
  mutate(stat = "mean")  # add mean stat


gcm_quantiles_wide <- migr_all %>%
  # exclude ensemble rows (by id or name)
  filter(!(gcm == 9L | str_to_lower(str_trim(gcm_name)) == "ensemble")) %>%
  group_by(FULL_CU_IN, rcp, period, period_code, dsmodel, indicator) %>%
  summarise(
    gcm_name = "ensemble",
    gcm      =  "9",
    mean     = mean(value, na.rm = TRUE),
    qlowgcm  = stats::quantile(value, probs = qlgcm, na.rm = TRUE, names = FALSE),
    qhighgcm = stats::quantile(value, probs = qhgcm, na.rm = TRUE, names = FALSE),
    .groups  = "drop"
  )

gcm_quantiles_long <- gcm_quantiles_wide %>%
  pivot_longer(
    cols      = c(mean, qlowgcm, qhighgcm),
    names_to  = "stat",
    values_to = "value"
  )

#bind back
migr_all <- bind_rows(migr_all, gcm_quantiles_long)

#add migration distance indicator
cu_migrdist <- cu_migr_timing %>%
  select(FULL_CU_IN, migrdist) %>%
  rename(value = migrdist) %>%
  mutate(indicator = "migrdist",
         dsmodel  = "observed",
         stat      = "mean",
         gcm = "0",
         rcp = "0",
         period_code = 0)

migr_all <- bind_rows(migr_all, cu_migrdist)

#add category column
migr_all <- migr_all %>%
  mutate(category = "migr")


save(migr_all, cu_migr_timing, migr_daily_all, migr_daily_calendar,
  file = file.path(paths$fw, paste0(today, "_migr_stats.Rdata")))
save(migr_all, cu_migr_timing, migr_daily_all, migr_daily_calendar,
  file = file.path(paths$fw, "migr_stats.Rdata"))










