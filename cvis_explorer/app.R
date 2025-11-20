# app.R
# Climate Vulnerability Indicator Explorer
# All-in-one Shiny app with integrated data preparation
# Version: 2024-11-20 - Complete working version with fixed CU selection

library(shiny)
library(bslib)
library(tidyverse)
library(plotly)
library(DT)
library(scales)
library(here)

#===============================================================================
# DATA PREPARATION AND LOADING
#===============================================================================

cat("Initializing CVIS Explorer...\n")

prepare_data_from_csv <- function() {
  cat("\nPreparing data from CSV files...\n")
  
  # Find most recent standardized indicators CSV
  csv_files <- list.files(
    here("processed_data"),
    pattern = "standardized_indicators\\.csv$",
    full.names = TRUE
  )
  
  if (length(csv_files) == 0) {
    stop("No standardized indicators CSV files found in processed_data/")
  }
  
  most_recent <- csv_files[which.max(file.info(csv_files)$mtime)]
  cat("Loading:", basename(most_recent), "\n")
  
  all_flat_std <- read_csv(most_recent, show_col_types = FALSE)
  
  # Debug
  cat("\nData loaded:\n")
  cat("  Rows:", nrow(all_flat_std), "\n")
  cat("  Columns:", ncol(all_flat_std), "\n")
  
  # Get standardized columns
  std_cols <- names(all_flat_std)[str_starts(names(all_flat_std), "std_")]
  cat("  Standardized columns found:", length(std_cols), "\n")
  cat("  Examples:", paste(head(std_cols, 5), collapse = ", "), "\n")
  
  # Create indicator metadata - match the ACTUAL column names
  tbl_indicators <- tribble(
    ~abbrev,            ~type,    ~name,
    "favchange_mean",   "fwR",    "ENM Change in Favourability",
    "CT_mean",          "fwR",    "Cumulative threats to freshwater habitat",
    "tw8rate_mean",     "fwR",    "Rate of change in August Temperature",
    "tw8proj_mean",     "fwR",    "Projected August Temperature",
    "lowQpdelta_mean",  "fwR",    "Change in August flow",
    "highQpdelta_mean", "fwR",    "Change in Nov-Jan flow",
    "fwres",            "fwR",    "Freshwater residency time",
    "migrT_mean",       "migr",   "Temperature during upstream migration",
    "migrQ_mean",       "migr",   "Change in discharge during migration",
    "migrdist",         "migr",   "Length of upstream migration",
    "SSTproj_mean",     "mar",    "Nearshore SST during ocean entry",
    "SSTrate_mean",     "mar",    "Rate of change in nearshore SST",
    "CImpact_mean",     "mar",    "Cumulative impacts to marine habitat",
    "CUstatus",         "dem",    "WSP status",
    "CUnmat",           "dem",    "Number of mature individuals",
    "hetzyg_mean",      "gen",    "Heterozygosity",
    "genoff_mean",      "gen",    "Genomic offset"
  )
  
  # Ensure required columns
  if (!"DFO_AREA" %in% names(all_flat_std)) {
    all_flat_std <- all_flat_std %>% mutate(DFO_AREA = "Fraser")
  }
  
  if (!"CVIS_NAME" %in% names(all_flat_std) && "CU_NAME" %in% names(all_flat_std)) {
    all_flat_std <- all_flat_std %>% mutate(CVIS_NAME = CU_NAME)
  }
  
  # Convert CUstatus from text to numeric
  if ("CUstatus" %in% names(all_flat_std)) {
    all_flat_std <- all_flat_std %>%
      mutate(CUstatus_numeric = case_when(
        str_detect(CUstatus, "^Red$") ~ 1,
        str_detect(CUstatus, "Red.*Amber|Amber.*Red") ~ 0.75,
        str_detect(CUstatus, "^Amber$") ~ 0.5,
        str_detect(CUstatus, "Amber.*Green|Green.*Amber") ~ 0.25,
        str_detect(CUstatus, "^Green$") ~ 0,
        TRUE ~ 0.5
      ))
  } else {
    all_flat_std <- all_flat_std %>% mutate(CUstatus_numeric = 0.5)
  }
  
  #=============================================================================
  # Create wide format data
  #=============================================================================
  
  indicator_data_wide <- all_flat_std %>%
    select(
      FULL_CU_IN, CU_NAME, CVIS_NAME, SPECIES_NAME, 
      DFO_AREA, CUstatus, CUstatus_numeric, rcp, period, period_code,
      starts_with("std_")
    ) %>%
    mutate(
      CU_label = paste0(SPECIES_NAME, " - ", CU_NAME),
      status_label = case_when(
        CUstatus_numeric >= 0.9 ~ "Red",
        CUstatus_numeric >= 0.65 ~ "Red/Amber",
        CUstatus_numeric >= 0.4 ~ "Amber",
        CUstatus_numeric >= 0.15 ~ "Amber/Green",
        CUstatus_numeric >= 0 ~ "Green",
        TRUE ~ "Unknown"
      )
    ) %>%
    filter(if_any(starts_with("std_"), ~!is.na(.)))
  
  cat("\nWide data:\n")
  cat("  Rows:", nrow(indicator_data_wide), "\n")
  cat("  CUs:", length(unique(indicator_data_wide$FULL_CU_IN)), "\n")
  
  #=============================================================================
  # Create long format data
  #=============================================================================
  
  # Get columns and remove std_ prefix
  std_indicator_cols <- names(indicator_data_wide)[str_starts(names(indicator_data_wide), "std_")]
  indicator_abbrevs <- str_remove(std_indicator_cols, "^std_")
  
  cat("\nIndicator matching:\n")
  cat("  Found in data:", paste(head(indicator_abbrevs, 5), collapse = ", "), "\n")
  
  # Match with metadata
  indicator_metadata <- tbl_indicators %>%
    filter(abbrev %in% indicator_abbrevs) %>%
    mutate(
      category_label = case_when(
        type == "fwR" ~ "Spawning & Rearing",
        type == "migr" ~ "Upstream Migration",
        type == "dem" ~ "Demographics",
        type == "mar" ~ "Nearshore Marine",
        type == "gen" ~ "Genetic",
        TRUE ~ "Other"
      ),
      description = name
    )
  
  cat("  Matched:", nrow(indicator_metadata), "indicators\n")
  cat("  Names:", paste(head(indicator_metadata$abbrev, 5), collapse = ", "), "\n")
  
  # Pivot to long
  indicator_data_long <- indicator_data_wide %>%
    pivot_longer(
      cols = all_of(std_indicator_cols),
      names_to = "indicator_std",
      values_to = "std_value",
      names_prefix = "std_"
    ) %>%
    left_join(
      indicator_metadata,
      by = c("indicator_std" = "abbrev")
    ) %>%
    filter(!is.na(std_value), !is.na(name)) %>%
    arrange(FULL_CU_IN, category_label, name)
  
  cat("\nLong data:\n")
  cat("  Rows:", nrow(indicator_data_long), "\n")
  
  #=============================================================================
  # Calculate context statistics
  #=============================================================================
  
  species_averages <- indicator_data_long %>%
    group_by(SPECIES_NAME, rcp, period_code, indicator_std, name, category_label) %>%
    summarize(
      species_mean = mean(std_value, na.rm = TRUE),
      species_median = median(std_value, na.rm = TRUE),
      species_sd = sd(std_value, na.rm = TRUE),
      .groups = "drop"
    )
  
  overall_averages <- indicator_data_long %>%
    group_by(rcp, period_code, indicator_std, name, category_label) %>%
    summarize(
      overall_mean = mean(std_value, na.rm = TRUE),
      overall_median = median(std_value, na.rm = TRUE),
      overall_sd = sd(std_value, na.rm = TRUE),
      overall_q25 = quantile(std_value, 0.25, na.rm = TRUE),
      overall_q75 = quantile(std_value, 0.75, na.rm = TRUE),
      .groups = "drop"
    )
  
  indicator_data_long <- indicator_data_long %>%
    left_join(species_averages, 
              by = c("SPECIES_NAME", "rcp", "period_code", 
                     "indicator_std", "name", "category_label")) %>%
    left_join(overall_averages,
              by = c("rcp", "period_code", "indicator_std", "name", "category_label"))
  
  #=============================================================================
  # Create filter choices
  #=============================================================================
  
  filter_choices <- list(
    species = sort(unique(indicator_data_wide$SPECIES_NAME)),
    dfo_area = sort(unique(indicator_data_wide$DFO_AREA)),
    status = sort(unique(indicator_data_wide$status_label)),
    rcp = sort(unique(indicator_data_wide$rcp)),
    period = sort(unique(indicator_data_wide$period_code)),
    indicators = indicator_metadata
  )
  
  cat("\n✓ Data preparation complete\n")
  cat("  CUs:", length(unique(indicator_data_wide$FULL_CU_IN)), "\n")
  cat("  Indicators:", nrow(indicator_metadata), "\n")
  cat("  Species:", paste(filter_choices$species, collapse = ", "), "\n\n")
  
  return(list(
    wide = indicator_data_wide,
    long = indicator_data_long,
    choices = filter_choices
  ))
}

# Prepare data
app_data <- prepare_data_from_csv()
data_wide <- app_data$wide
data_long <- app_data$long
filter_choices <- app_data$choices

cat("✓ App data loaded successfully\n\n")

#===============================================================================
# UTILITY FUNCTIONS
#===============================================================================

filter_data <- function(data, species = NULL, dfo_area = NULL, 
                        status = NULL, rcp = NULL) {
  if (!is.null(species) && length(species) > 0) {
    data <- data %>% filter(SPECIES_NAME %in% species)
  }
  if (!is.null(dfo_area) && length(dfo_area) > 0) {
    data <- data %>% filter(DFO_AREA %in% dfo_area)
  }
  if (!is.null(status) && length(status) > 0) {
    data <- data %>% filter(status_label %in% status)
  }
  if (!is.null(rcp)) {
    data <- data %>% filter(rcp == rcp)
  }
  return(data)
}

get_selected_indicators <- function(categories) {
  filter_choices$indicators %>%
    filter(category_label %in% categories) %>%
    pull(abbrev)
}

#===============================================================================
# PLOTTING FUNCTIONS
#===============================================================================

plot_heatmap <- function(data_long, selected_cus, selected_indicators) {
  
  plot_data <- data_long %>%
    filter(FULL_CU_IN %in% selected_cus,
           indicator_std %in% selected_indicators) %>%
    mutate(name = factor(name, levels = unique(name)))
  
  if (nrow(plot_data) == 0) {
    return(plotly_empty(type = "scatter", mode = "markers") %>%
             layout(title = "No data available for selection"))
  }
  
  plot_ly(plot_data,
          x = ~name,
          y = ~CU_label,
          z = ~std_value,
          type = "heatmap",
          colorscale = list(c(0, "green"), c(0.5, "yellow"), c(1, "red")),
          hovertemplate = paste("<b>%{y}</b><br>%{x}<br>Value: %{z:.3f}<br><extra></extra>"),
          colorbar = list(title = "Risk Score")) %>%
    layout(
      title = "Standardized Indicator Values",
      xaxis = list(title = "", tickangle = -45),
      yaxis = list(title = ""),
      margin = list(l = 200, b = 150)
    )
}

plot_radar <- function(data_long, selected_cus, selected_indicators) {
  
  plot_data <- data_long %>%
    filter(FULL_CU_IN %in% selected_cus,
           indicator_std %in% selected_indicators)
  
  if (nrow(plot_data) == 0) {
    return(plotly_empty(type = "scatter", mode = "markers") %>%
             layout(title = "No data available for selection"))
  }
  
  fig <- plot_ly(type = 'scatterpolar', mode = 'markers')
  colors <- scales::hue_pal()(length(selected_cus))
  
  for (i in seq_along(selected_cus)) {
    cu <- selected_cus[i]
    cu_data <- plot_data %>% 
      filter(FULL_CU_IN == cu) %>%
      arrange(name)
    
    fig <- fig %>% add_trace(
      r = cu_data$std_value,
      theta = cu_data$name,
      name = cu_data$CU_label[1],
      fill = 'toself',
      opacity = 0.6,
      line = list(color = colors[i]),
      fillcolor = colors[i]
    )
  }
  
  fig %>% layout(
    polar = list(
      radialaxis = list(
        visible = TRUE,
        range = c(0, 1),
        tickvals = c(0, 0.25, 0.5, 0.75, 1)
      )
    ),
    showlegend = TRUE,
    title = "Radar Chart: Indicator Values Across CUs"
  )
}

plot_grouped_bars <- function(data_long, selected_cus, selected_indicators) {
  
  plot_data <- data_long %>%
    filter(FULL_CU_IN %in% selected_cus,
           indicator_std %in% selected_indicators)
  
  if (nrow(plot_data) == 0) {
    return(plotly_empty(type = "scatter", mode = "markers") %>%
             layout(title = "No data available for selection"))
  }
  
  p <- ggplot(plot_data, aes(x = name, y = std_value, fill = CU_label)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~category_label, scales = "free_x", ncol = 2) +
    scale_fill_brewer(palette = "Set2") +
    labs(
      title = "Standardized Indicator Values by Category",
      x = "",
      y = "Standardized Value (0-1)",
      fill = "Conservation Unit"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.text = element_text(face = "bold", size = 11),
      legend.position = "bottom"
    )
  
  ggplotly(p, tooltip = c("x", "y", "fill")) %>%
    layout(legend = list(orientation = "h", y = -0.2))
}

plot_context_boxplot <- function(data_long, selected_cus, selected_indicators,
                                 context_data_long) {
  
  context <- context_data_long %>%
    filter(indicator_std %in% selected_indicators)
  
  selected <- data_long %>%
    filter(FULL_CU_IN %in% selected_cus,
           indicator_std %in% selected_indicators)
  
  if (nrow(selected) == 0) {
    return(plotly_empty(type = "scatter", mode = "markers") %>%
             layout(title = "No data available for selection"))
  }
  
  p <- ggplot() +
    geom_boxplot(data = context,
                 aes(x = name, y = std_value),
                 fill = "gray80", alpha = 0.5, outlier.shape = NA) +
    geom_segment(data = context %>% 
                   group_by(name, SPECIES_NAME) %>%
                   summarize(avg = mean(std_value, na.rm = TRUE), .groups = "drop") %>%
                   distinct(),
                 aes(x = as.numeric(factor(name)) - 0.4,
                     xend = as.numeric(factor(name)) + 0.4,
                     y = avg, yend = avg),
                 color = "blue", linewidth = 0.8, linetype = "dashed", alpha = 0.6) +
    geom_point(data = selected,
               aes(x = name, y = std_value, color = CU_label),
               size = 4, alpha = 0.9) +
    facet_wrap(~category_label, scales = "free_x", ncol = 2) +
    scale_color_brewer(palette = "Set1") +
    labs(
      title = "Selected CUs in Context of All CUs",
      subtitle = "Gray boxes show distribution, blue line is species average",
      x = "",
      y = "Standardized Value (0-1)",
      color = "Selected CU"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
      strip.text = element_text(face = "bold", size = 11),
      legend.position = "bottom"
    )
  
  ggplotly(p, tooltip = c("x", "y", "color")) %>%
    layout(legend = list(orientation = "h", y = -0.2))
}

#===============================================================================
# USER INTERFACE
#===============================================================================

ui <- page_sidebar(
  title = "Climate Vulnerability Indicator Explorer",
  theme = bs_theme(
    bootswatch = "flatly",
    primary = "#2C3E50",
    base_font = font_google("Open Sans")
  ),
  
  sidebar = sidebar(
    width = 380,
    class = "sidebar-custom",
    
    card(
      card_header(icon("filter"), "Filters"),
      selectInput(
        "filter_species",
        "Species",
        choices = filter_choices$species,
        multiple = TRUE,
        selected = NULL
      ),
      selectInput(
        "filter_dfo_area",
        "DFO Area",
        choices = filter_choices$dfo_area,
        multiple = TRUE,
        selected = NULL
      ),
      selectInput(
        "filter_status",
        "Status",
        choices = filter_choices$status,
        multiple = TRUE,
        selected = NULL
      ),
      selectInput(
        "filter_rcp",
        "RCP Scenario",
        choices = filter_choices$rcp,
        selected = filter_choices$rcp[1]
      )
    ),
    
    hr(),
    
    card(
      card_header(icon("check-square"), "Select CUs"),
      p(style = "font-size: 0.9em; color: #666;", 
        "Select 1-10 CUs to compare, or leave blank to show all filtered CUs"),
      textInput("cu_search", "Search CUs", placeholder = "Type to search..."),
      div(
        style = "max-height: 300px; overflow-y: auto;",
        uiOutput("cu_checkboxes")
      ),
      actionButton("select_all", "Select All", class = "btn-sm"),
      actionButton("select_none", "Clear All", class = "btn-sm")
    ),
    
    hr(),
    
    card(
      card_header(icon("chart-bar"), "Indicator Categories"),
      checkboxGroupInput(
        "indicator_categories",
        NULL,
        choices = c(
          "Spawning & Rearing" = "Spawning & Rearing",
          "Upstream Migration" = "Upstream Migration",
          "Nearshore Marine" = "Nearshore Marine",
          "Demographics" = "Demographics"
        ),
        selected = c("Spawning & Rearing", "Upstream Migration", 
                     "Nearshore Marine", "Demographics")
      )
    )
  ),
  
  layout_columns(
    col_widths = c(12, 12, 12),
    
    navset_card_tab(
      id = "main_tabs",
      
      nav_panel(
        "Overview Table",
        icon = icon("table"),
        card(
          card_header("Selected CUs - Summary Statistics"),
          DTOutput("summary_table")
        )
      ),
      
      nav_panel(
        "Comparison Plots",
        icon = icon("chart-line"),
        layout_columns(
          col_widths = c(12, 12),
          card(
            card_header("Visualization Type"),
            radioButtons(
              "plot_type",
              NULL,
              choices = c(
                "Heatmap" = "heatmap",
                "Radar Chart" = "radar",
                "Grouped Bars" = "bars",
                "Context Boxplot" = "boxplot"
              ),
              selected = "heatmap",
              inline = TRUE
            )
          ),
          card(
            card_header("Comparison Visualization"),
            plotlyOutput("comparison_plot", height = "600px")
          )
        )
      ),
      
      nav_panel(
        "Category Breakdown",
        icon = icon("layer-group"),
        card(
          card_header("Detailed View by Category"),
          plotlyOutput("category_plot", height = "700px")
        )
      )
    ),
    
    card(
      card_header(icon("download"), "Export Data"),
      layout_columns(
        col_widths = c(6, 6),
        downloadButton("download_data", "Download CSV", 
                       class = "btn-success btn-block"),
        downloadButton("download_plot", "Download Plot", 
                       class = "btn-info btn-block")
      )
    )
  )
)

#===============================================================================
# SERVER LOGIC
#===============================================================================

server <- function(input, output, session) {
  
  # Filtered data
  filtered_data_wide <- reactive({
    filter_data(
      data_wide,
      species = input$filter_species,
      dfo_area = input$filter_dfo_area,
      status = input$filter_status,
      rcp = input$filter_rcp
    )
  })
  
  filtered_data_long <- reactive({
    filter_data(
      data_long,
      species = input$filter_species,
      dfo_area = input$filter_dfo_area,
      status = input$filter_status,
      rcp = input$filter_rcp
    )
  })
  
  selected_indicators <- reactive({
    get_selected_indicators(input$indicator_categories)
  })
  
  # Render CU checkboxes
  output$cu_checkboxes <- renderUI({
    data_to_use <- filtered_data_wide()
    
    if (is.null(data_to_use) || nrow(data_to_use) == 0) {
      return(div(
        style = "padding: 20px; color: red;",
        "No CUs match the current filters. Try adjusting your filter selections."
      ))
    }
    
    cu_options <- data_to_use %>%
      distinct(FULL_CU_IN, CU_label, SPECIES_NAME, status_label) %>%
      arrange(SPECIES_NAME, CU_label)
    
    if (!is.null(input$cu_search) && input$cu_search != "") {
      cu_options <- cu_options %>%
        filter(str_detect(CU_label, regex(input$cu_search, ignore_case = TRUE)))
    }
    
    if (nrow(cu_options) == 0) {
      return(div(
        style = "padding: 20px;",
        "No CUs found matching your search."
      ))
    }
    
    cu_list <- cu_options %>%
      pmap(function(FULL_CU_IN, CU_label, SPECIES_NAME, status_label, ...) {
        status_class <- case_when(
          status_label == "Red" ~ "status-red",
          status_label == "Amber" ~ "status-amber",
          status_label == "Green" ~ "status-green",
          TRUE ~ "status-unknown"
        )
        
        div(
          class = "cu-checkbox",
          checkboxInput(
            inputId = paste0("cu_", FULL_CU_IN),
            label = tags$span(
              class = status_class,
              paste0(CU_label, " (", status_label, ")")
            ),
            value = FALSE
          )
        )
      })
    
    tagList(cu_list)
  })
  
  # Track selected CUs - FIXED: Combine into single reactive
  selected_cus <- reactive({
    # Force reactivity by accessing filtered data
    data_to_use <- filtered_data_wide()
    
    if (is.null(data_to_use) || nrow(data_to_use) == 0) {
      return(character(0))
    }
    
    cu_ids <- data_to_use %>%
      distinct(FULL_CU_IN) %>%
      pull(FULL_CU_IN)
    
    # Collect selected CUs - accessing each checkbox creates reactive dependencies
    selected <- character(0)
    for (cu_id in cu_ids) {
      checkbox_id <- paste0("cu_", cu_id)
      checkbox_value <- input[[checkbox_id]]
      
      if (!is.null(checkbox_value) && checkbox_value) {
        selected <- c(selected, cu_id)
      }
    }
    
    # Limit to 10
    if (length(selected) > 10) {
      showNotification("Maximum 10 CUs can be selected.", type = "warning")
      selected <- selected[1:10]
    }
    
    # If nothing selected, return all filtered CUs
    if (length(selected) == 0) {
      cat("\n=== No CUs selected, showing all", length(cu_ids), "CUs ===\n")
      return(cu_ids)
    }
    
    cat("\n=== CUs selected:", length(selected), "===\n")
    cat("Selected IDs:", paste(head(selected, 5), collapse = ", "), "\n")
    
    return(selected)
  })
  
  # Plot data
  plot_data_long <- reactive({
    req(selected_cus(), selected_indicators())
    
    filtered_data_long() %>%
      filter(
        FULL_CU_IN %in% selected_cus(),
        indicator_std %in% selected_indicators()
      )
  })
  
  observeEvent(input$select_all, {
    data_to_use <- filtered_data_wide()
    if (is.null(data_to_use)) return()
    
    cu_ids <- data_to_use %>%
      distinct(FULL_CU_IN) %>%
      pull(FULL_CU_IN) %>%
      head(10)
    
    for (cu_id in cu_ids) {
      updateCheckboxInput(session, paste0("cu_", cu_id), value = TRUE)
    }
  })
  
  observeEvent(input$select_none, {
    data_to_use <- filtered_data_wide()
    if (is.null(data_to_use)) return()
    
    cu_ids <- data_to_use %>%
      distinct(FULL_CU_IN) %>%
      pull(FULL_CU_IN)
    
    for (cu_id in cu_ids) {
      updateCheckboxInput(session, paste0("cu_", cu_id), value = FALSE)
    }
  })
  
  output$summary_table <- DT::renderDT({
    if (length(selected_indicators()) == 0) {
      return(datatable(data.frame(Message = "Please select indicator categories")))
    }
    
    n_cus <- length(selected_cus())
    
    if (n_cus > 50) {
      return(datatable(data.frame(Message = paste("Too many CUs to display:", n_cus, "- Please apply filters"))))
    }
    
    table_data <- filtered_data_wide() %>%
      filter(FULL_CU_IN %in% selected_cus()) %>%
      select(
        CU_label, SPECIES_NAME, status_label,
        all_of(paste0("std_", selected_indicators()))
      )
    
    colnames(table_data) <- c(
      "Conservation Unit", "Species", "Status",
      filter_choices$indicators %>%
        filter(abbrev %in% selected_indicators()) %>%
        pull(name)
    )
    
    datatable(
      table_data,
      options = list(
        pageLength = 20,
        scrollX = TRUE,
        dom = 'Bfrtip'
      ),
      rownames = FALSE
    ) %>%
      formatRound(columns = 4:ncol(table_data), digits = 3) %>%
      formatStyle(
        'Status',
        backgroundColor = styleEqual(
          c('Red', 'Red/Amber', 'Amber', 'Amber/Green', 'Green'),
          c('#e74c3c', '#e67e22', '#f39c12', '#2ecc71', '#27ae60')
        ),
        color = 'white'
      )
  })
  
  output$comparison_plot <- renderPlotly({
    if (length(selected_indicators()) < 1) {
      return(plotly_empty() %>% 
               layout(title = "Please select at least 1 indicator category"))
    }
    
    n_cus <- length(selected_cus())
    
    if (n_cus > 10) {
      return(plotly_empty() %>% 
               layout(title = paste("Too many CUs to display:", n_cus, "- Please select specific CUs (max 10) or apply filters")))
    }
    
    switch(input$plot_type,
           "heatmap" = plot_heatmap(plot_data_long(), selected_cus(), selected_indicators()),
           "radar" = plot_radar(plot_data_long(), selected_cus(), selected_indicators()),
           "bars" = plot_grouped_bars(plot_data_long(), selected_cus(), selected_indicators()),
           "boxplot" = plot_context_boxplot(plot_data_long(), selected_cus(), selected_indicators(), filtered_data_long())
    )
  })
  
  output$category_plot <- renderPlotly({
    if (length(selected_indicators()) < 1) {
      return(plotly_empty() %>% 
               layout(title = "Please select at least 1 indicator category"))
    }
    
    n_cus <- length(selected_cus())
    
    if (n_cus > 10) {
      return(plotly_empty() %>% 
               layout(title = paste("Too many CUs to display:", n_cus, "- Please select specific CUs (max 10) or apply filters")))
    }
    
    plot_grouped_bars(plot_data_long(), selected_cus(), selected_indicators())
  })
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("cvis_indicator_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(selected_cus(), selected_indicators())
      
      export_data <- filtered_data_wide() %>%
        filter(FULL_CU_IN %in% selected_cus()) %>%
        select(
          FULL_CU_IN, CU_NAME, SPECIES_NAME, status_label,
          all_of(paste0("std_", selected_indicators()))
        )
      
      write_csv(export_data, file)
    }
  )
  
  output$download_plot <- downloadHandler(
    filename = function() {
      paste0("cvis_plot_", Sys.Date(), ".png")
    },
    content = function(file) {
      showNotification("Plot download requires additional setup", type = "message")
    }
  )
}

#===============================================================================
# RUN APP
#===============================================================================

shinyApp(ui, server)