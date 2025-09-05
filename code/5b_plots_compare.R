################################################################################
#
# 5b_plots_compare.R
#
#  Functions for plotting and comparing indicator values across multiple CUs

### Plots:
# 1.  Lollipop chart comparison of values for a single indicator across CUs
# 2.  Comparison of indicator standardization function and unstandardized vs standardized values
# 3.  Summary of multiple indicators for a life stage/category
# 4.  Maps of distribution of indicator values coloured by CU boundary

#  Plot of indicator value vs latitude, elevation, glacial coverage

# 5. Comparison of indicator value between PCIC and statistical models

#
###############################################################################

library(here)
setwd(here())
source(file.path(here(), "code", "0_setup.R"))

library(scico)

## color palette function
get_scico_palette <- function(data, column, palette_name = "berlin") {
  categories <- sort(unique(data[[column]]))
  setNames(scico(length(categories), palette = palette_name), categories)
}

species_palette <- get_scico_palette(cu_run, "CU_Species", "berlin")


#order categories as factors
all_flat_std$CU_Species <- factor(all_flat_std$CU_Species, levels = sort(unique(all_flat_std$CU_Species)))



#-----------------1. lollipop chart of indicator across CUs--------------------

# Function to create a lollipop chart of indicator values across CUs
plot_lollipop <- function(data, indicator_pick, indicator_name, indicator_stat, species_palette, gcm_range_suffix = c("qlowgcm", "qhighgcm")) {
  
  # Define suffixes
  stat_suffix <- if_else(indicator_stat == "raw", indicator_pick, indicator_stat)
  sp_suffix   <- c("qlowsp", "qhighsp")
  
  #take column names that contain prefix with model type
  cols_sub <- names(data)[str_detect(names(data), indicator_pick)]
  
  #take names with prefix that also contain stat suffix
  stat_col <- cols_sub[str_detect(cols_sub, stat_suffix)]  #must contain mean
  min_spcol  <- cols_sub[str_detect(cols_sub, sp_suffix[1])]
  max_spcol  <- cols_sub[str_detect(cols_sub, sp_suffix[2])]
  min_gcmcol <- cols_sub[str_detect(cols_sub, paste0(gcm_range_suffix[1], collapse = "|"))]
  max_gcmcol <- cols_sub[str_detect(cols_sub, paste0(gcm_range_suffix[2], collapse = "|"))]
  
  id_col <- "CU_NAME"
  sp_col <- "CU_Species"
  
  # Check if mean and ID columns exist
  if (!all(c(id_col, stat_col) %in% names(data))) {
    stop("One or more required columns are missing in the data.")
  }
  
  # Prepare data for plotting
  plot_data <- data %>%
    select(all_of(c(id_col, stat_col, sp_col))) %>%
    rename(mean = !!stat_col,
           id = !!id_col,
           sp = !!sp_col)
  
  #palette_colors <- pal_npg("nrc")(5) # 4 colors from Nature Publishing Group palette
  
  # # Create a named color palette using unique categories
  # unique_sp <- unique(plot_data$sp)
  # palette_colors <- pal_npg("nrc")(length(unique_sp))
  # names(palette_colors) <- unique_sp
  
  # plot_data$color <- species_palette[plot_data$sp]
  # plot_data$label <- paste0("<span style='color:",
  #                           plot_data$color,
  #                           "'>",
  #                           plot_data$sp,
  #                           "</span>")
  # 
  
  # Create a unique ordering of id by sp
  ordered_ids <- plot_data %>%
    arrange(sp, id) %>%
    distinct(id) %>%
    pull(id)
  
  # Apply the ordering
  plot_data$id <- factor(plot_data$id, levels = ordered_ids)
  
  # Add min and max if available
  if (length(min_spcol) == 1 && length(max_spcol) == 1) {
    plot_data <- plot_data %>%
      mutate(min = data[[min_spcol]], max = data[[max_spcol]])
    has_range <- TRUE
  } else {
    has_range <- FALSE
  }
  
  # add GCM range if available
  if (length(min_gcmcol) == 1 && length(max_gcmcol) == 1) {
    plot_data <- plot_data %>%
      mutate(mingcm = data[[min_gcmcol]], maxgcm = data[[max_gcmcol]])
    has_gcm <- TRUE
  } else {
    has_gcm <- FALSE
  }
  
  # Create the lollipop chart
  p <- ggplot(plot_data, aes(x = id, color = sp)) +
    scale_fill_manual(values = species_palette)
  
  if (has_range) {
    p <- p + geom_segment(aes(xend = id, y = min, yend = max),
                          color = "darkgrey",
                          linewidth = 2.5)
  }
  
  if (has_gcm) {
    p <- p + geom_segment(
      aes(xend = id, y = mingcm, yend = maxgcm),
      color = "darkred",
      linewidth = 1
    )
  }
  
  p +   geom_point(aes(y = mean), size = 2.5) +
    labs(
      title = paste(indicator_name),
      x = "CU",
      y = stat_col,
      color = "Species"
    ) +
    coord_flip() +
    theme_minimal() +
    theme(axis.text.y = element_text(size = 7))
  
}



# 2. Indicator value comparison, standardized vs raw ----------------------

# function to show how standardized values compare to raw ones across CUs

#requires standardization function and data table with raw and unstandardized outputs

plot_std_vs_raw <- function(data, indicator_pick,  indicator_name, indicator_stat, 
                            indicator_fun, species_palette, gcm_range_suffix = c("qlowgcm", "qhighgcm")) {
  
  sp_col <- "CU_Species"
  
  # Define suffixes
  stat_suffix <- if_else(indicator_stat == "raw", indicator_pick, indicator_stat)
  
  #take column names that contain prefix with model type
  cols_sub <- names(data)[str_detect(names(data), indicator_pick)]
  
  #get raw and standardized columns
  cols_sub_raw <- cols_sub[!str_detect(cols_sub, "std")]
  cols_sub_std <- cols_sub[str_detect(cols_sub, "std")]
  
  
  #take names with prefix that also contain stat suffix
  stat_col_raw <- cols_sub_raw[str_detect(cols_sub_raw, stat_suffix)]  #must contain mean
  stat_col_std <- cols_sub_std[str_detect(cols_sub_std, stat_suffix)]
  
  
  min_gcmcol_raw <- cols_sub_raw[str_detect(cols_sub_raw, paste0(gcm_range_suffix[1], collapse = "|"))]
  max_gcmcol_raw <- cols_sub_raw[str_detect(cols_sub_raw, paste0(gcm_range_suffix[2], collapse = "|"))]
  min_gcmcol_std <- cols_sub_std[str_detect(cols_sub_std, paste0(gcm_range_suffix[1], collapse = "|"))]
  max_gcmcol_std <- cols_sub_std[str_detect(cols_sub_std, paste0(gcm_range_suffix[2], collapse = "|"))]
  
  # Prepare data for plotting
  plot_data <- data %>%
    select(all_of(c(stat_col_std, stat_col_raw, sp_col))) %>%
    rename(raw = !!stat_col_raw,
           std = !!stat_col_std,
           sp = !!sp_col)
  
  # add GCM range if available
  if (length(min_gcmcol_raw) == 1 && length(max_gcmcol_raw) == 1) {
    plot_data <- plot_data %>%
      mutate(min_gcm_raw = data[[min_gcmcol_raw]], max_gcm_raw = data[[max_gcmcol_raw]]) %>% 
      mutate(min_gcm_std = data[[min_gcmcol_std]], max_gcm_std = data[[max_gcmcol_std]])
    has_gcm <- TRUE
  } else {
    has_gcm <- FALSE
  }
  
  p <- ggplot(plot_data) +
    labs(
      title = paste(indicator_name),
      subtitle = paste0("Standardization function: ", indicator_fun),
      color = "Species",
      x = "Raw (unstandardized)",
      y = "Standardized"
    ) 
  
  if (has_gcm) {
    p <- p + geom_segment(
      aes(x = min_gcm_raw, xend = max_gcm_raw, y = std),
      color = "grey",
      linewidth = 1
    ) +
      geom_segment(
        aes(x = raw, y = min_gcm_std, yend = max_gcm_std),
        color = "grey",
        linewidth = 1
      )
  }
  
  p <- p +  geom_point(aes(x = raw, y = std, color = sp), size = 2.5) +
    scale_fill_manual(values = species_palette) +
    theme_bw()
  
  return(p)
  
}


# ggplot() +
#   geom_histogram(data = CVIS_std, aes(x = rateT_spn_rawstd), bins = 20, fill = "purple")
# 
# ggplot() +
#   geom_line(data = CVIS_std, aes(x = rateT_spn, y = rateT_spn_rawstd)) +
#   geom_point(data = CVIS_std, aes(x = rateT_spn, y = rateT_spn_rawstd)) +
#   geom_line(data = CVIS_std, aes(x = rateT_spn, y = rateT_spn_asymp), color = "red") +
#   geom_point(data = CVIS_std, aes(x = rateT_spn, y = rateT_spn_asymp))




## test plots

flat_sub <- filter(all_flat,
                   RCP == "45", period_code == 3)

flat_std_sub <- filter(all_flat_std,
                       RCP == "45", period_code == 3)

for(i in 1:nrow(tbl_indicators)) {
  # Construct filename
  file_name <- paste0(paths$figures, "/ind_", tbl_indicators$abbrev[i], ".png")
  
  png(filename = file_name, width = 800, height = 600)
  
  p <- plot_lollipop(flat_sub, indicator_pick = tbl_indicators$abbrev[i],  indicator_stat = tbl_indicators$stat[i],
                  indicator_name = tbl_indicators$name[i], species_palette)
  
  print(p)
  
  dev.off()
  
}


p <- plot_std_vs_raw(flat_std_sub, indicator_pick = tbl_indicators$abbrev[i],  indicator_stat = tbl_indicators$stat[i],
                indicator_name = tbl_indicators$name[i], indicator_fun = tbl_indicators$std_fun[i])

print(p)
