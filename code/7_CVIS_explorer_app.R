# ==============================================================================
# CVIS Shiny App Explorer (7_CVIS_explorer_app.R)
#
# Description:
#   Launches the local interactive Shiny Dashboard explorer app. Allows users
#   to explore climate vulnerability indicator summaries, life stage timing, WSP
#   demographics, accessible stream networks, hydrologic regimes, projected August
#   stream temperatures/flows, and nearshore sea surface temperatures (SST) for
#   any selected Conservation Unit (CU) in the Fraser Basin.
#
# Workflow Steps:
#   1. Load setup environment and Shiny libraries (shinydashboard, reactable, gt).
#   2. Load computed CVIS indicators, scores, and spatial stream paths.
#   3. Build UI with a left sidebar navigation and interactive tables/plots.
#   4. Server-side rendering: dynamically generate maps and plots for the selected CU.
#   5. Run the local Shiny web application.
#
# Inputs:
#   - Sourced: code/4_scoring_utils.R, code/5a_plots_CU.R
#   - processed_data/freshwater/fw_upstream_paths.Rdata
#   - output/all_indicators_std_long.csv / combined_scores_long.csv
#
# Outputs:
#   - Launches local interactive Shiny dashboard
#
# Dependencies:
#   - Requires 4a_CU_scoring.R to be run first. Sourced from 0a_console.R.
# ==============================================================================

library(shiny)
library(shinydashboard)
library(tidyverse)
library(sf)
library(ggplot2)
library(gt)
library(reactable)
library(htmltools)
library(scico)
library(patchwork)
library(ggspatial)

# Source existing scripts
source(file.path(here(), "code", "4_scoring_utils.R"))
source(file.path(here(), "code", "5a_plots_CU.R"))

# Ensure paths are configured
if (!exists("paths")) {
  library(here)
  source(file.path(here(), "code", "0_setup.R"))
}

# Helper to find the latest date-prefixed file matching a pattern
get_latest_file <- function(path, pattern) {
  files <- list.files(path, pattern = pattern, full.names = TRUE)
  if (length(files) == 0) {
    # try output directory as fallback
    files <- list.files(here("output"), pattern = pattern, full.names = TRUE)
  }
  if (length(files) == 0) {
    stop("No files matching pattern '", pattern, "' found in ", path)
  }
  sorted_files <- sort(files, decreasing = TRUE)
  return(sorted_files[1])
}

# Load Data
# Try to load from Rdata first (produced by 4a_CU_scoring.R)
if (!exists("all_std_long")) {
  rdata_path <- file.path(here(), "output", "scoring_results.Rdata")
  if (file.exists(rdata_path)) {
    load(rdata_path) # loads all_std_long and scores_tidy
    if (!exists("scores_long") && exists("scores_tidy")) {
      scores_long <- scores_tidy
    }
  }
}

# Fallback to output CSVs if not already in environment
if (!exists("all_std_long")) {
  data_path <- file.path(here(), "output", "all_indicators_std_long.csv")
  if (file.exists(data_path)) {
    all_std_long <- read.csv(data_path)
  } else {
    warning("all_indicators_std_long.csv not found. Please run 4a_CU_scoring.R")
  }
}

if (!exists("scores_long")) {
  data_path <- file.path(here(), "output", "combined_scores_long.csv")
  if (file.exists(data_path)) {
    scores_long <- read.csv(data_path)
  } else {
    warning("combined_scores_long.csv not found. Please run 4a_CU_scoring.R")
  }
}

if (!exists("fw_all")) {
  fw_path <- get_latest_file(paths$fw, "fw_rearing_indicators.Rdata")
  load(fw_path) # loads fw_all, ss_all
}

if (!exists("migr_all") || !exists("migr_daily_all")) {
  load(get_latest_file(paths$fw, "migr_stats.Rdata"))
}

if (!exists("migr_list")) {
  load(file.path(paths$fw, "fw_upstream_paths.Rdata"))
}

# Load Core Spatial and Reference Objects if not in environment
if (!exists("MAZ")) {
  load(file.path(paths$marine, "MAZ.Rds"))
}
if (!exists("basins")) {
  load(file.path(paths$fw, "basins_shp.Rds"))
}
if (!exists("Fr_basin") && exists("basins")) {
  Fr_basin <- filter(basins, BASIN == "FRASER")
}
if (!exists("stream_cu_picks")) {
  load(file.path(paths$fw, "fw_streampicks_tscapes.Rdata"))
}
if (!exists("fw_sp_ind")) {
  load(file.path(paths$fw, "fw_stream_indicators_sp.Rds"))
}

# Dynamic post-load safeguard for legacy column names in spatial indicators
if (exists("fw_sp_ind")) {
  names(fw_sp_ind) <- sub("^tw8_9_", "tw8proj_9_", names(fw_sp_ind))
  names(fw_sp_ind) <- sub("^lowQpdelta_9_", "flow8pdelta_9_", names(fw_sp_ind))
  names(fw_sp_ind) <- sub("^highQpdelta_9_", "flow18pdelta_9_", names(fw_sp_ind))
}

if (!exists("lakes_Fr")) {
  load(file.path(paths$fw, "BC_FWA_LAKES_FR.Rds"))
}
if (!exists("watershed_flow")) {
  load(file.path(paths$fw, "flow_gauge_data.Rdata"))
}
if (!exists("Tw_stations")) {
  load(file.path(paths$fw, "Tw_stations.Rds"))
}
if (!exists("cu_timing_long") || !exists("cu_timing_Fr")) {
  load(file.path(paths$CU, "cu_timing_data.Rdata"))
}
if (!exists("CMIP6_SST")) {
  load(file.path(paths$marine, "CMIP6_SST_periods.Rds"))
}
if (!exists("CImpact_points")) {
  load(file.path(paths$marine, "CImpact_points.Rds"))
}
if (!exists("tbl_standardize") || !exists("tbl_indicators")) {
  load(file.path(paths$params, "indicator_tables.Rdata"))
}

# Alias for compatibility if code uses old names
if (exists("all_std_long")) all_flat_std <- all_std_long
if (exists("scores_long")) combined_scores_std <- scores_long

################################################################################
# ==================== 2. User Interface (UI) Definition ====================
################################################################################

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "CVIS Explorer", titleWidth = 250),
  dashboardSidebar(
    collapsed = FALSE, # Open at startup
    sidebarMenu(
      id = "tabs",
      menuItem("About", tabName = "about", icon = icon("info-circle")),
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Demographics", tabName = "demographics", icon = icon("users")),
      menuItem("Timing", tabName = "timing", icon = icon("calendar")),
      menuItem("Spawning & Rearing",
        tabName = "spawning_menu", icon = icon("water"),
        menuSubItem("Stream Access", tabName = "spawning_access"),
        menuSubItem("Hydrology", tabName = "spawning_hydro"),
        menuSubItem("Flow", tabName = "spawning_flow"),
        menuSubItem("Temperature", tabName = "spawning_temp"),
        menuSubItem("ENM", tabName = "spawning_enm"),
        menuSubItem("Threats", tabName = "spawning_threats")
      ),
      menuItem("Migration", tabName = "migration_path", icon = icon("route")),
      menuItem("Marine",
        tabName = "marine_menu", icon = icon("ship"),
        menuSubItem("SST", tabName = "marine_sst"),
        menuSubItem("Impacts", tabName = "marine_impacts")
      )
    )
  ),
  dashboardBody(

    # Control panel at top
    fluidRow(
      style = "background-color: #ecf0f5; padding: 10px 15px; margin-bottom: 15px; border-bottom: 2px solid #d2d6de;",
      column(
        12,
        selectInput("cu_select", "Conservation Unit:",
          choices = setNames(
            cu_run$FULL_CU_IN,
            paste0(cu_run$CU_NAME, " (", cu_run$SPECIES_NAME, ")")
          ),
          selected = cu_run$FULL_CU_IN[1],
          width = "100%"
        )
      )
    ),


    # Add custom CSS for the reactable
    tags$head(
      tags$style(HTML("
        .climate-row { background-color: #fcffeb; }
        .migration-row { background-color: #eff6ff; }
      "))
    ),
    tabItems(

      # ABOUT TAB
      tabItem(
        tabName = "about",
        fluidRow(
          box(
            width = 12,
            title = "About CVIS Explorer",
            status = "info",
            solidHeader = TRUE,
            p("This application explores climate vulnerability indicator data for conservation units (CUs) in the Fraser Basin.
              Use the top drop-down menu to select a CU and then navigate using the left-hand menu to view maps and summaries of indicator
              data for the CU."),
            p(strong("Warning: Loading data for CUs with large CU boundaries may take a while (particularly Fraser Pinks).")),
            h4("Indicator Categories"),
            tags$ul(
              tags$li(strong("Freshwater Spawning and Rearing:"), " Environmental change to streams, cumulative
                        threats to stream habitats, and freshwater residency times"),
              tags$li(strong("Upstream Migration:"), " Changes to stream temperatures and flows during migration,
                        and distance to spawning sites"),
              tags$li(strong("Nearshore Marine:"), " Changes to sea surface temperature and cumulative impacts
                        to nearshore marine habitat"),
              tags$li(strong("Demographics:"), " Conservation status and recent spawner abundance"),
              tags$li(strong("Genetics:"), " Heterozygosity and genomic offset measures (not currently shown in app)")
            ),
            h4("Default Parameters"),
            p("All indicators are displayed using:"),
            tags$ul(
              tags$li(strong("RCP 4.5:"), " Representative Concentration Pathway 4.5 (moderate emissions scenario)"),
              tags$li(strong("Time Period:"), " Mid-Century (2041-2060)")
            ),
            h4("Data Quality"),
            div(
              class = "alert alert-warning",
              style = "background-color: #fff3cd; border: 1px solid #ffc107; padding: 15px; border-radius: 4px;",
              h4(icon("exclamation-triangle"), " Draft Data Disclaimer", style = "margin-top: 0;"),
              p(strong("DRAFT - Not for further distribution without permission of the authors")),
              p("This data and analysis are preliminary and subject to change. The indicators presented here
                  are under active development and have not been peer-reviewed. Results should be interpreted
                  with caution and are intended for exploratory analysis only. Please contact the authors before
                  citing or using this information in any formal capacity.")
            )
          )
        )
      ),

      # OVERVIEW TAB
      tabItem(
        tabName = "overview",
        fluidRow(
          box(
            width = 12,
            title = "All Climate Vulnerability Indicators",
            status = "primary",
            solidHeader = TRUE,
            tags$p(
              style = "margin: 5px 0 10px 0; color: #666; font-size: 12px;",
              "Summary of all climate vulnerability indicators for the selected conservation unit. Green shading indicates values below species average (lower risk), while red indicates values above species average (higher risk)."
            ),
            reactableOutput("comprehensive_indicators_table")
          )
        )
      ),

      # DEMOGRAPHICS TAB
      tabItem(
        tabName = "demographics",
        fluidRow(
          box(
            width = 12, title = "Demographic Indicators Summary",
            status = "info", solidHeader = TRUE,
            tags$p(
              style = "margin: 5px 0 10px 0; color: #666; font-size: 12px;",
              "Recent conservation status and number of mature individuals indicator values."
            ),
            reactableOutput("demographics_indicators_table")
          )
        ),
        fluidRow(
          box(
            width = 12, title = "Spawner Abundance Time Series",
            status = "primary", solidHeader = TRUE,
            tags$p(
              style = "margin: 5px 0 10px 0; color: #666; font-size: 12px;",
              "Time series of annual spawner abundance (blue line) and generational geometric average (red line). Points at bottom indicate Wild Salmon Policy status assessment (Red/Amber/Green) with shape indicating data quality confidence."
            ),
            plotOutput("abundance_plot", height = "500px")
          )
        )
      ),

      # TIMING TAB
      tabItem(
        tabName = "timing",
        fluidRow(
          box(
            width = 12, title = "Life History Timing",
            status = "success", solidHeader = TRUE,
            tags$p(
              style = "margin: 5px 0 10px 0; color: #666; font-size: 12px;",
              "Life stage timing throughout the year showing spawning, upstream migration, ocean entry, and juvenile freshwater migration periods. Shaded areas indicate periods used for calculating specific vulnerability indicators. Point size represents data quality (larger = LOWER quality)."
            ),
            plotOutput("timing_plot", height = "400px")
          )
        )
      ),

      # SPAWNING ACCESS TAB
      tabItem(
        tabName = "spawning_access",
        fluidRow(
          box(
            width = 12, title = "CU Location",
            status = "primary", solidHeader = TRUE,
            tags$p(
              style = "margin: 5px 0 10px 0; color: #666; font-size: 12px;",
              "Geographic location of the selected conservation unit (green) relative to all Fraser River basin conservation units (grey outlines)."
            ),
            plotOutput("boundary_highlight_plot", height = "400px")
          )
        ),
        fluidRow(
          box(
            width = 12, title = "Stream Statistics",
            status = "info", solidHeader = TRUE,
            tags$p(
              style = "margin: 5px 0 10px 0; color: #666; font-size: 12px;",
              "Summary statistics for accessible stream habitat within the conservation unit boundary based on BC Fishpass habitat potential models."
            ),
            gt_output("stream_stats_table")
          )
        ),
        fluidRow(
          box(
            width = 12, title = "Accessible Stream Network",
            status = "primary", solidHeader = TRUE,
            tags$p(
              style = "margin: 5px 0 10px 0; color: #666; font-size: 12px;",
              "Accessible streams only with coloring indicating BC Fishpass modelled habitat potential and NuSEDS spawning site locations within the conservation unit boundary."
            ),
            plotOutput("accessibility_plot", height = "600px")
          )
        )
      ),

      # SPAWNING HYDROLOGY TAB
      tabItem(
        tabName = "spawning_hydro",
        fluidRow(
          box(
            width = 12, title = "Hydrologic Regime Coverage",
            status = "info", solidHeader = TRUE,
            tags$p(
              style = "margin: 5px 0 10px 0; color: #666; font-size: 12px;",
              "Proportion of CU boundary classified by hydrologic regime type using method described in Ruzzante et al 2025.
              prop_coverage indicates proportion of CU boundary within gauged watersheds"
            ),
            gt_output("hydro_stats_table")
          )
        ),
        fluidRow(
          box(
            width = 12, title = "Hydrologic Regime Map",
            status = "primary", solidHeader = TRUE,
            tags$p(
              style = "margin: 5px 0 10px 0; color: #666; font-size: 12px;",
              "Map of hydrologic regime zones and flow gauge locations within the CU boundary using method described in Ruzzante et al 2025."
            ),
            plotOutput("hydro_regime_plot", height = "600px")
          )
        )
      ),

      # SPAWNING FLOW TAB
      tabItem(
        tabName = "spawning_flow",
        fluidRow(
          box(
            width = 12, title = "Flow Indicators Summary",
            status = "info", solidHeader = TRUE,
            tags$p(
              style = "margin: 5px 0 10px 0; color: #666; font-size: 12px;",
              "Projected changes in stream flow during the month of August (flow8pdelta) and Nov - Jan (flow18pdelta) for RCP 4.5, 2041-2060."
            ),
            reactableOutput("flow_indicators_table")
          )
        ),
        fluidRow(
          box(
            width = 12, title = "Change in August Flow",
            status = "warning", solidHeader = TRUE,
            tags$p(
              style = "margin: 5px 0 10px 0; color: #666; font-size: 12px;",
              "Projected proportional change in August stream flows. Negative values indicate flow reductions relative to historic period."
            ),
            plotOutput("august_flow_plot", height = "650px")
          )
        ),
        fluidRow(
          box(
            width = 12, title = "Change in Nov-Jan Flow",
            status = "info", solidHeader = TRUE,
            tags$p(
              style = "margin: 5px 0 10px 0; color: #666; font-size: 12px;",
              "Projected proportional change in winter flows (November-January). Positive values indicate flow increases relative to historic period."
            ),
            plotOutput("winter_flow_plot", height = "650px")
          )
        )
      ),

      # SPAWNING TEMP TAB
      tabItem(
        tabName = "spawning_temp",
        fluidRow(
          box(
            width = 12, title = "Temperature Indicators Summary",
            status = "info", solidHeader = TRUE,
            tags$p(
              style = "margin: 5px 0 10px 0; color: #666; font-size: 12px;",
              "Stream temperature indicators: projected August temperature (tw8proj) and rate of change in August temperature (tw8rate)."
            ),
            reactableOutput("temp_indicators_table")
          )
        ),
        fluidRow(
          box(
            width = 12, title = "Projected August Stream Temperature",
            status = "danger", solidHeader = TRUE,
            tags$p(
              style = "margin: 5px 0 10px 0; color: #666; font-size: 12px;",
              "Projected mean August stream temperature (2041-2060, RCP 4.5)"
            ),
            plotOutput("temp_plot", height = "650px")
          )
        ),
        fluidRow(
          box(
            width = 12, title = "Rate of Temperature Change",
            status = "warning", solidHeader = TRUE,
            tags$p(
              style = "margin: 5px 0 10px 0; color: #666; font-size: 12px;",
              "Rate of change in August stream temperature from baseline (1981-2000) to mid-century (2041-2060)"
            ),
            plotOutput("temp_rate_plot", height = "650px")
          )
        )
      ),

      # SPAWNING ENM TAB
      tabItem(
        tabName = "spawning_enm",
        fluidRow(
          box(
            width = 12, title = "ENM Indicators Summary",
            status = "info", solidHeader = TRUE,
            tags$p(
              style = "margin: 5px 0 10px 0; color: #666; font-size: 12px;",
              "Indicator value of changes in habitat favourability (favchange) from Environmental Niche Models."
            ),
            reactableOutput("enm_indicators_table")
          )
        ),
        fluidRow(
          box(
            width = 12, title = "Change in Favourability",
            status = "info", solidHeader = TRUE,
            tags$p(
              style = "margin: 5px 0 10px 0; color: #666; font-size: 12px;",
              "Change in habitat favourability from baseline to mid-century. Negative values indicate decreased favourability, positive values indicate improving conditions."
            ),
            plotOutput("enm_diff_plot", height = "650px")
          )
        )
      ),

      # SPAWNING THREATS TAB
      tabItem(
        tabName = "spawning_threats",
        fluidRow(
          box(
            width = 12, title = "Cumulative Threats Indicator Summary",
            status = "info", solidHeader = TRUE,
            tags$p(
              style = "margin: 5px 0 10px 0; color: #666; font-size: 12px;",
              "Indicator of cumulative threat score (CT) representing anthropogenic stressors to stream habitat."
            ),
            reactableOutput("threats_indicators_table")
          )
        ),
        fluidRow(
          box(
            width = 12, title = "Cumulative Threats to Stream Habitat",
            status = "danger", solidHeader = TRUE,
            tags$p(
              style = "margin: 5px 0 10px 0; color: #666; font-size: 12px;",
              "Cumulative threat score combining nine anthropogenic stressor categories. Higher values represent higher threats."
            ),
            plotOutput("ct_plot", height = "650px")
          )
        )
      ),

      # MIGRATION PATH & TIMING TAB
      tabItem(
        tabName = "migration_path",
        fluidRow(
          box(
            width = 12, title = "Migration Indicators Summary",
            status = "info", solidHeader = TRUE,
            tags$p(
              style = "margin: 5px 0 10px 0; color: #666; font-size: 12px;",
              "Indicators representing temperature and flow conditions and migration distance from ocean to spawning grounds."
            ),
            reactableOutput("migration_indicators_table")
          )
        ),
        fluidRow(
          box(
            width = 12, title = "Migration Route, Timing & Temperature",
            status = "primary", solidHeader = TRUE,
            tags$p(
              style = "margin: 5px 0 10px 0; color: #666; font-size: 12px;",
              "Daily projected stream temperatures during upstream migration period with migration events (left) and geographic migration path coloured by August temperature (right)."
            ),
            plotOutput("migration_path_plot", height = "550px")
          )
        )
      ),

      # MARINE SST TAB
      tabItem(
        tabName = "marine_sst",
        fluidRow(
          box(
            width = 12, title = "Sea Surface Temperature Indicators Summary",
            status = "info", solidHeader = TRUE,
            tags$p(
              style = "margin: 5px 0 10px 0; color: #666; font-size: 12px;",
              "Nearshore marine indicators related to sea surface temperature, the projected SST during the ocean entry period (SSTproj), and the rate of change in SST (SSTrate) relative to the baseline period."
            ),
            reactableOutput("sst_indicators_table")
          )
        ),
        fluidRow(
          box(
            width = 12, title = "Marine Adaptive Zone",
            status = "info", solidHeader = TRUE,
            tags$p(
              style = "margin: 5px 0 10px 0; color: #666; font-size: 12px;",
              "Marine Adaptive Zone (green) representing the nearshore rearing area used for marine indicator calculations."
            ),
            plotOutput("maz_boundary_plot", height = "400px")
          )
        ),
        fluidRow(
          box(
            width = 12, title = "Projected Sea Surface Temperature",
            status = "danger", solidHeader = TRUE,
            tags$p(
              style = "margin: 5px 0 10px 0; color: #666; font-size: 12px;",
              "Projected sea surface temperature during the period surround peak ocean entry (2041-2060, RCP 4.5)."
            ),
            plotOutput("sst_plot", height = "600px")
          )
        )
      ),

      # MARINE IMPACTS TAB
      tabItem(
        tabName = "marine_impacts",
        fluidRow(
          box(
            width = 12, title = "Marine Cumulative Impacts Indicator Summary",
            status = "info", solidHeader = TRUE,
            tags$p(
              style = "margin: 5px 0 10px 0; color: #666; font-size: 12px;",
              "Indicator for cumulative impact score (CImpact) for nearshore marine habitats within the marine adaptive zone."
            ),
            reactableOutput("impacts_indicators_table")
          )
        ),
        fluidRow(
          box(
            width = 12, title = "Cumulative Impacts on Marine Habitat",
            status = "warning", solidHeader = TRUE,
            tags$p(
              style = "margin: 5px 0 10px 0; color: #666; font-size: 12px;",
              "Cumulative impact score combining stressors including aquaculture, fishing, invasive species, pollution, and shipping. Higher values indicate a higher impact score."
            ),
            plotOutput("marine_impacts_plot", height = "600px")
          )
        )
      )
    )
  )
)

################################################################################
# ==================== 3. Helper Functions ====================
################################################################################

# Function to create a reactable for a single indicator
create_indicator_reactable <- function(df, indicator_codes) {
  # Filter to selected indicators
  tbl_view <- df %>%
    filter(`Indicator Code` %in% indicator_codes)

  if (nrow(tbl_view) == 0) {
    return(NULL)
  }

  # Precompute scaling for coloring
  diff_max <- tryCatch(
    {
      x <- tbl_view[["CU score"]] - tbl_view[["Species score"]]
      m <- max(abs(x), na.rm = TRUE)
      if (is.infinite(m) || is.na(m) || m == 0) 1 else m
    },
    error = function(e) 1
  )

  # Color function for CU score vs Species score
  cu_z_cell <- function(value, index, name) {
    sp <- tbl_view[["Species score"]][index]
    if (is.na(value) || is.na(sp)) {
      return("NA")
    }
    diff <- value - sp
    intensity <- min(1, abs(diff) / diff_max)
    alpha <- 0.15 + 0.40 * intensity
    bg <- if (diff < 0) {
      sprintf("rgba(34, 197, 94, %.3f)", alpha)
    } else if (diff > 0) {
      sprintf("rgba(239, 68, 68, %.3f)", alpha)
    } else {
      "rgba(148, 163, 184, 0.20)"
    }
    div(
      style = list(
        backgroundColor = bg,
        borderRadius = "6px",
        padding = "1px 6px",
        display = "inline-block"
      ),
      sprintf("%.3f", value)
    )
  }

  # Create reactable
  reactable(
    tbl_view,
    filterable = FALSE,
    searchable = FALSE,
    highlight = TRUE,
    bordered = TRUE,
    striped = FALSE,
    compact = TRUE,
    pagination = FALSE,
    defaultColDef = colDef(
      minWidth = 70,
      headerVAlign = "center",
      vAlign = "center"
    ),
    theme = reactableTheme(
      borderColor = "#e5e7eb",
      stripedColor = "#f9fafb",
      highlightColor = "#eef2ff"
    ),
    columnGroups = list(
      colGroup(
        name = "Standardized Scores",
        columns = intersect(c("CU score", "Species score", "All Species score"), names(tbl_view))
      ),
      colGroup(
        name = "CU Raw Values",
        columns = intersect(c("Mean", "GCM Q10", "GCM Q90"), names(tbl_view))
      ),
      colGroup(
        name = "Raw Means",
        columns = intersect(c("Species Mean", "All Species Mean"), names(tbl_view))
      )
    ),
    columns = list(
      `Indicator Type` = colDef(width = 140),
      `Indicator Code` = colDef(width = 115),
      `CU score` = colDef(width = 95, format = colFormat(digits = 3), na = "NA", cell = cu_z_cell),
      `Species score` = colDef(width = 95, format = colFormat(digits = 3), na = "NA"),
      `All Species score` = colDef(width = 115, format = colFormat(digits = 3), na = "NA"),
      `Mean` = colDef(width = 95, format = colFormat(digits = 2, separators = TRUE), na = "NA"),
      `GCM Q10` = colDef(width = 85, format = colFormat(digits = 2, separators = TRUE), na = "NA"),
      `GCM Q90` = colDef(width = 85, format = colFormat(digits = 2, separators = TRUE), na = "NA"),
      `Species Mean` = colDef(width = 95, format = colFormat(digits = 2, separators = TRUE), na = "NA"),
      `All Species Mean` = colDef(width = 95, format = colFormat(digits = 2, separators = TRUE), na = "NA")
    )
  )
}

################################################################################
# ==================== 4. Server Logic Definition ====================
################################################################################

server <- function(input, output, session) {
  # Reactive data for selected CU
  cu_data <- reactive({
    req(input$cu_select)
    all_std_long %>%
      filter(
        FULL_CU_IN == input$cu_select,
        (rcp == "45" & period_code == "3") | (rcp == "0" & period_code == "0")
      ) %>%
      group_by(FULL_CU_IN, indicator, stat) %>%
      mutate(has_projection = any(rcp == "45" & period_code == "3")) %>%
      filter(!has_projection | (rcp == "45" & period_code == "3")) %>%
      ungroup() %>%
      select(-has_projection)
  })

  # Reactive data for indicator table (formatted for display)
  indicator_table_data <- reactive({
    req(input$cu_select)

    # Get all indicator data for the selected CU (using default RCP 4.5 and period 3)
    cu_all_raw <- get_CU_indicators(all_std_long,
      cu_i = input$cu_select,
      use_standardized = FALSE,
      period_pick = "3",
      RCP_pick = "45"
    )

    cu_all_std <- get_CU_indicators(all_std_long,
      cu_i = input$cu_select,
      use_standardized = TRUE,
      period_pick = "3",
      RCP_pick = "45"
    )

    df <- cu_all_raw %>%
      left_join(cu_all_std, by = c("FULL_CU_IN", "rcp", "period_code", "indicator"), suffix = c("_raw", "_std")) %>%
      left_join(select(tbl_indicators, abbrev, long_type), by = c("indicator" = "abbrev")) %>%
      mutate(across(where(is.numeric), ~ ifelse(is.nan(.), NA_real_, .)))

    # Dynamically pick columns to be robust to missing columns
    pick_col <- function(df_in, choices) {
      out <- choices[choices %in% names(df_in)]
      if (length(out) == 0) NA_character_ else out[1]
    }

    std_cu_col <- pick_col(df, c("std_cu_value", "std_cu_mean", "cu_value_std", "cu_mean_std", "std_cu"))
    std_sp_col <- pick_col(df, c("std_sp_value", "std_sp_mean", "sp_value_std", "sp_mean_std", "std_sp"))
    std_allcu_col <- pick_col(df, c("std_allcu_value", "std_allcu_mean", "allcu_value_std", "allcu_mean_std", "std_allcu"))
    cu_col <- pick_col(df, c("cu_value_raw", "cu_mean_raw", "cu_value", "cu_mean"))
    q10_col <- pick_col(df, c("cu_qlowgcm_raw", "cu_qlowgcm", "cu_qlowgcm_std"))
    q90_col <- pick_col(df, c("cu_qhighgcm_raw", "cu_qhighgcm", "cu_qhighgcm_std"))
    sp_col <- pick_col(df, c("sp_value_raw", "sp_mean_raw", "sp_value", "sp_mean"))
    allcu_col <- pick_col(df, c("allcu_value_raw", "allcu_mean_raw", "allcu_value", "allcu_mean"))

    tbl_view <- df %>%
      transmute(
        `Indicator Type` = long_type,
        `Indicator Code` = indicator,
        `CU score` = if (is.na(std_cu_col)) NA_real_ else .[[std_cu_col]],
        `Species score` = if (is.na(std_sp_col)) NA_real_ else .[[std_sp_col]],
        `All Species score` = if (is.na(std_allcu_col)) NA_real_ else .[[std_allcu_col]],
        `Mean` = if (is.na(cu_col)) NA_real_ else .[[cu_col]],
        `GCM Q10` = if (is.na(q10_col)) NA_real_ else .[[q10_col]],
        `GCM Q90` = if (is.na(q90_col)) NA_real_ else .[[q90_col]],
        `Species Mean` = if (is.na(sp_col)) NA_real_ else .[[sp_col]],
        `All Species Mean` = if (is.na(allcu_col)) NA_real_ else .[[allcu_col]]
      )

    # Reorder columns
    display_order <- c(
      "Indicator Type", "Indicator Code",
      "CU score", "Species score", "All Species score",
      "Mean", "GCM Q10", "GCM Q90",
      "Species Mean", "All Species Mean"
    )
    tbl_view <- tbl_view[, intersect(display_order, names(tbl_view))]
    tbl_view
  })

  # Reactive timing data
  cu_timing_data <- reactive({
    req(input$cu_select)
    cu_timing_long %>%
      filter(FULL_CU_IN == input$cu_select)
  })

  # Reactive spatial data
  cu_spatial <- reactive({
    req(input$cu_select)

    cu_i <- input$cu_select
    sp_pick <- cu_run$SPECIES_NAME[cu_run$FULL_CU_IN == cu_i]
    sp_pick_bcfp <- spp_lookup$spp_abr_bcfp[spp_lookup$SPECIES_NAME == sp_pick]
    sp_pick_ENM <- str_to_lower(sp_pick)[1]

    cu_boundary_i <- cu_boundary[cu_boundary$FULL_CU_IN == cu_i, ]
    nuseds_cu <- nuseds_Fr[nuseds_Fr$FULL_CU_IN == cu_i, ]

    stream_cu_sub <- stream_cu_picks[, colnames(stream_cu_picks) == cu_i]

    fw_sp_cu <- subset_fw_models(
      fw_models = fw_sp_ind,
      cu_i = cu_i,
      stream_cu_picks = stream_cu_picks,
      cu_run = cu_run,
      spp_lookup = spp_lookup,
      to_factor = TRUE
    ) %>%
      rename(
        favchange_45 = contains(paste0("favchange_", sp_pick_ENM, "_45_3")),
        favchange_85 = contains(paste0("favchange_", sp_pick_ENM, "_85_3"))
      )

    acc_sp_cu <- fw_sp_cu %>%
      filter(model_access_salmon %in% c("OBSERVED", "INFERRED"))

    temp <- unlist(st_intersects(cu_boundary_i, watershed_flow))
    watershed_flow_cu <- watershed_flow[temp, ]

    temp <- unlist(st_intersects(cu_boundary_i, stations_flow))
    stations_cu <- stations_flow[temp, ]

    temp <- unlist(st_intersects(cu_boundary_i, lakes_Fr))
    lakes_cu <- lakes_Fr[temp, ]

    list(
      boundary = cu_boundary_i,
      nuseds = nuseds_cu,
      fw_sp = fw_sp_cu,
      acc_sp = acc_sp_cu,
      watersheds = watershed_flow_cu,
      stations = stations_cu,
      lakes = lakes_cu
    )
  })

  # OVERVIEW OUTPUTS - New comprehensive reactable
  output$comprehensive_indicators_table <- renderReactable({
    req(indicator_table_data())

    tbl_view <- indicator_table_data()

    # Precompute scaling for coloring
    diff_max <- tryCatch(
      {
        x <- tbl_view[["CU score"]] - tbl_view[["Species score"]]
        m <- max(abs(x), na.rm = TRUE)
        if (is.infinite(m) || is.na(m) || m == 0) 1 else m
      },
      error = function(e) 1
    )

    # Color function for CU score vs Species score
    cu_z_cell <- function(value, index, name) {
      sp <- tbl_view[["Species score"]][index]
      if (is.na(value) || is.na(sp)) {
        return("NA")
      }
      diff <- value - sp
      intensity <- min(1, abs(diff) / diff_max)
      alpha <- 0.15 + 0.40 * intensity
      bg <- if (diff < 0) {
        sprintf("rgba(34, 197, 94, %.3f)", alpha)
      } else if (diff > 0) {
        sprintf("rgba(239, 68, 68, %.3f)", alpha)
      } else {
        "rgba(148, 163, 184, 0.20)"
      }
      div(
        style = list(
          backgroundColor = bg,
          borderRadius = "6px",
          padding = "1px 6px",
          display = "inline-block"
        ),
        sprintf("%.3f", value)
      )
    }

    # Create reactable
    reactable(
      tbl_view,
      filterable = FALSE,
      searchable = FALSE,
      highlight = TRUE,
      bordered = TRUE,
      striped = FALSE,
      compact = TRUE,
      pagination = FALSE,
      defaultSorted = "Indicator Type",
      defaultSortOrder = "asc",
      defaultColDef = colDef(
        minWidth = 70,
        headerVAlign = "center",
        vAlign = "center"
      ),
      theme = reactableTheme(
        borderColor = "#e5e7eb",
        stripedColor = "#f9fafb",
        highlightColor = "#eef2ff"
      ),
      columnGroups = list(
        colGroup(
          name = "Standardized Scores",
          columns = intersect(c("CU score", "Species score", "All Species score"), names(tbl_view))
        ),
        colGroup(
          name = "CU Raw Values",
          columns = intersect(c("Mean", "Q10", "Q90"), names(tbl_view))
        ),
        colGroup(
          name = "Raw Means",
          columns = intersect(c("Species Mean", "All Species Mean"), names(tbl_view))
        )
      ),
      columns = list(
        `Indicator Type` = colDef(width = 100),
        `Indicator Code` = colDef(width = 115),
        `CU score` = colDef(width = 95, format = colFormat(digits = 3), na = "NA", cell = cu_z_cell),
        `Species score` = colDef(width = 95, format = colFormat(digits = 3), na = "NA"),
        `All Species score` = colDef(width = 115, format = colFormat(digits = 3), na = "NA"),
        `Mean` = colDef(width = 95, format = colFormat(digits = 2, separators = TRUE), na = "NA"),
        `Q10` = colDef(width = 85, format = colFormat(digits = 2, separators = TRUE), na = "NA"),
        `Q90` = colDef(width = 85, format = colFormat(digits = 2, separators = TRUE), na = "NA"),
        `Species Mean` = colDef(width = 95, format = colFormat(digits = 2, separators = TRUE), na = "NA"),
        `All Species Mean` = colDef(width = 95, format = colFormat(digits = 2, separators = TRUE), na = "NA")
      ),
      rowClass = function(index, name) {
        ind <- tbl_view[["Indicator Code"]][index]
        if (!is.null(ind) && ind %in% c("tw8proj", "tw8rate", "SSTproj", "SSTrate")) {
          "climate-row"
        } else if (!is.null(ind) && ind %in% c("migrT", "migrQ", "migrdist")) {
          "migration-row"
        } else {
          ""
        }
      }
    ) %>%
      htmlwidgets::prependContent(
        tags$h4(
          paste0(input$cu_select, " Indicator Summary (RCP 4.5, Mid-Century 2041-2060)"),
          style = "margin: 0 0 6px; font-size: 14px; font-weight: 600;"
        )
      )
  })

  # TIMING OUTPUT
  output$timing_plot <- renderPlot({
    req(cu_timing_data())
    cu_timing_plot(cu_timing_data(), show_indicator_periods = TRUE)
  })

  # DEMOGRAPHICS OUTPUTS
  output$demographics_indicators_table <- renderReactable({
    req(indicator_table_data())
    create_indicator_reactable(indicator_table_data(), c("CUstatus", "CUnmat"))
  })


  output$abundance_plot <- renderPlot({
    req(input$cu_select)
    abundance_status_plot(status_data, cu_i = input$cu_select)
  })

  # SPAWNING ACCESS OUTPUTS
  output$boundary_highlight_plot <- renderPlot({
    req(input$cu_select)

    cu_boundary_i <- cu_boundary %>% filter(FULL_CU_IN == input$cu_select)
    target_crs <- sf::st_crs(cu_boundary)

    # Reproject other map layers
    if (exists("bc_coast", envir = .GlobalEnv)) {
      bc_coast_proj <- sf::st_transform(get("bc_coast", envir = .GlobalEnv), target_crs)
    } else if (exists("paths") && !is.null(paths$marine) && file.exists(file.path(paths$marine, "bc_coast.Rds"))) {
      bc_coast_proj <- sf::st_transform(readRDS(file.path(paths$marine, "bc_coast.Rds")), target_crs)
    } else {
      library(pacea)
      bc_coast_proj <- sf::st_transform(pacea::bc_coast, target_crs)
    }

    if (exists("Fr_basin", envir = .GlobalEnv)) {
      Fr_basin_proj <- sf::st_transform(get("Fr_basin", envir = .GlobalEnv), target_crs)
    } else {
      if (exists("paths") && !is.null(paths$fw) && file.exists(file.path(paths$fw, "basins_shp.Rds"))) {
        load(file.path(paths$fw, "basins_shp.Rds"))
        Fr_basin_proj <- sf::st_transform(dplyr::filter(basins, BASIN == "FRASER"), target_crs)
      } else {
        Fr_basin_proj <- NULL
      }
    }

    if (exists("lakes_Fr", envir = .GlobalEnv)) {
      lakes_proj <- sf::st_transform(get("lakes_Fr", envir = .GlobalEnv), target_crs)
    } else {
      if (exists("paths") && !is.null(paths$fw) && file.exists(file.path(paths$fw, "BC_FWA_LAKES_FR.Rds"))) {
        load(file.path(paths$fw, "BC_FWA_LAKES_FR.Rds"))
        lakes_proj <- sf::st_transform(lakes_Fr, target_crs)
      } else {
        lakes_proj <- NULL
      }
    }

    bbox <- sf::st_bbox(cu_boundary)
    x_range <- bbox["xmax"] - bbox["xmin"]
    y_range <- bbox["ymax"] - bbox["ymin"]
    margin_factor <- 0.08
    xlims <- c(bbox["xmin"] - margin_factor * x_range, bbox["xmax"] + margin_factor * x_range)
    ylims <- c(bbox["ymin"] - margin_factor * y_range, bbox["ymax"] + margin_factor * y_range)

    p <- ggplot() +
      geom_sf(data = bc_coast_proj, fill = "grey90", color = "grey75", linewidth = 0.3)
    
    if (!is.null(Fr_basin_proj)) {
      p <- p + geom_sf(data = Fr_basin_proj, fill = "antiquewhite", color = "grey60", linewidth = 0.4)
    }
    
    if (!is.null(lakes_proj)) {
      p <- p + geom_sf(data = lakes_proj, fill = "aliceblue", color = "aliceblue", linewidth = 0.1)
    }

    p <- p +
      geom_sf(data = cu_boundary, color = "black", alpha = 0.3) +
      geom_sf(data = cu_boundary_i, fill = "green", alpha = 0.5) +
      coord_sf(xlim = xlims, ylim = ylims, expand = FALSE) +
      labs(title = input$cu_select) +
      theme_void() +
      theme(
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
        plot.margin = margin(5, 5, 5, 5)
      )

    print(p)
  })

  output$stream_stats_table <- render_gt({
    req(input$cu_select)

    cu_i <- input$cu_select
    fwR_cu <- fw_all %>% filter(FULL_CU_IN == cu_i)

    if (nrow(fwR_cu) > 0) {
      fwR_cu %>%
        select(c(
          total_length_acc, n_streams, proportion_rear,
          proportion_spawn, proportion_rs,
          avg_elevation, avg_lat, avg_lon
        )) %>%
        mutate(
          total_length_acc = round(total_length_acc / 1000, 1),
          proportion_rear = round(proportion_rear, 3),
          proportion_spawn = round(proportion_spawn, 3),
          proportion_rs = round(proportion_rs, 3),
          avg_elevation = round(avg_elevation, 1),
          avg_lat = round(avg_lat, 5),
          avg_lon = round(avg_lon, 5)
        ) %>%
        gt() %>%
        tab_header(title = "CU Boundary Stream Statistics") %>%
        cols_label(
          total_length_acc = "Accessible Length (km)",
          n_streams = "# Segments",
          avg_elevation = "Avg. Elevation (m)",
          proportion_rear = "Prop. Rearing",
          proportion_spawn = "Prop. Spawning",
          proportion_rs = "Prop. Rearing/Spawning"
        ) %>%
        tab_spanner(
          label = "Habitat Model",
          columns = c(proportion_rear, proportion_spawn, proportion_rs)
        ) %>%
        tab_options(
          table.font.size = px(11),
          data_row.padding = px(3)
        )
    }
  })

  output$accessibility_plot <- renderPlot({
    req(cu_spatial())

    spatial <- cu_spatial()

    p <- stream_accessible_plot(
      stream_data = spatial$fw_sp,
      nuseds_data = spatial$nuseds,
      cu_boundary = spatial$boundary,
      lakes_cu = spatial$lakes
    )

    print(p)
  })

  # SPAWNING HYDROLOGY OUTPUTS
  output$hydro_stats_table <- render_gt({
    req(input$cu_select)

    cu_i <- input$cu_select
    fwR_cu <- fwR_all[[cu_i]]

    if (!is.null(fwR_cu$streams)) {
      fwR_cu$streams %>%
        select(contains("prop_")) %>%
        mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
        gt() %>%
        tab_header(title = "Hydrologic Regime Coverage") %>%
        tab_options(
          table.font.size = px(11),
          data_row.padding = px(3),
          column_labels.font.size = px(12),
          column_labels.font.weight = "bold"
        )
    }
  })

  output$hydro_regime_plot <- renderPlot({
    req(cu_spatial())

    spatial <- cu_spatial()

    tryCatch(
      {
        p <- ggplot() +
          geom_sf(data = spatial$boundary, color = "black", fill = "grey", alpha = 0.5) +
          geom_sf(data = st_zm(spatial$fw_sp))

        # Add watersheds if available
        if (!is.null(spatial$watersheds) && nrow(spatial$watersheds) > 0) {
          p <- p + geom_sf(data = spatial$watersheds, aes(fill = regime), alpha = 0.7)
        }

        # Add stations if available
        if (!is.null(spatial$stations) && nrow(spatial$stations) > 0) {
          # Wrap station names for legend
          spatial$stations$Station.Wrapped <- str_wrap(spatial$stations$Station.Name, width = 15)
          p <- p +
            geom_sf(data = spatial$stations, aes(colour = Station.Wrapped), size = 2) +
            scale_colour_brewer(palette = "Set1")
        }

        p <- p +
          coord_sf(
            xlim = st_bbox(spatial$boundary)[c(1, 3)],
            ylim = st_bbox(spatial$boundary)[c(2, 4)]
          ) +
          labs(
            fill = "Hydrologic regime",
            color = "Flow Gauge"
          )

        print(p)
      },
      error = function(e) {
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
        text(1, 1, paste("Hydrologic data not available\n", e$message), cex = 0.8)
      }
    )
  })

  # SPAWNING FLOW OUTPUTS
  output$flow_indicators_table <- renderReactable({
    req(indicator_table_data())
    create_indicator_reactable(indicator_table_data(), c("flow8pdelta", "flow18pdelta"))
  })

  output$august_flow_plot <- renderPlot({
    req(cu_spatial())

    tryCatch(
      {
        spatial <- cu_spatial()

        stream_indicator_plot(spatial$acc_sp, spatial$boundary, spatial$lakes,
          Tw_stations = NULL,
          variable = "flow8pdelta_9_45_3",
          plot_title = "Change in August Flow - 2041-2060",
          unit_label = "Proportional Change",
          xlim = c(-1, 0),
          risk_palette = "lajolla",
          palette_direction = 1,
          temp_stations = FALSE
        )
      },
      error = function(e) {
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
        text(1, 1, paste("Error:\n", e$message), cex = 0.8)
      }
    )
  })

  output$winter_flow_plot <- renderPlot({
    req(cu_spatial())

    tryCatch(
      {
        spatial <- cu_spatial()

        stream_indicator_plot(spatial$acc_sp, spatial$boundary, spatial$lakes,
          Tw_stations = NULL,
          variable = "flow18pdelta_9_45_3",
          plot_title = "Change in Nov-Jan Flow - 2041-2060",
          unit_label = "Proportional Change",
          xlim = c(0, 1),
          risk_palette = "lajolla",
          palette_direction = -1,
          temp_stations = FALSE
        )
      },
      error = function(e) {
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
        text(1, 1, paste("Error:\n", e$message), cex = 0.8)
      }
    )
  })

  # SPAWNING TEMP OUTPUTS
  output$temp_indicators_table <- renderReactable({
    req(indicator_table_data())
    create_indicator_reactable(indicator_table_data(), c("tw8proj", "tw8rate"))
  })

  output$temp_plot <- renderPlot({
    req(cu_spatial())

    tryCatch(
      {
        spatial <- cu_spatial()

        stream_indicator_plot(spatial$acc_sp, spatial$boundary, spatial$lakes, Tw_stations,
          variable = "tw8proj_9_45_3",
          plot_title = "August Mean Temperature - 2041-2060",
          unit_label = "Temperature (°C)",
          risk_palette = "roma",
          palette_direction = -1,
          temp_stations = TRUE
        )
      },
      error = function(e) {
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
        text(1, 1, paste("Error:\n", e$message), cex = 0.8)
      }
    )
  })

  output$temp_rate_plot <- renderPlot({
    req(cu_spatial())

    tryCatch(
      {
        spatial <- cu_spatial()

        stream_indicator_plot(spatial$acc_sp, spatial$boundary, spatial$lakes, Tw_stations,
          variable = "tw8rate_9_45_3",
          plot_title = "Rate of Temperature Change - 1981-2000 to 2041-2060",
          unit_label = "°C per decade",
          risk_palette = "roma",
          palette_direction = -1,
          temp_stations = TRUE
        )
      },
      error = function(e) {
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
        text(1, 1, paste("Error:\n", e$message), cex = 0.8)
      }
    )
  })

  # SPAWNING ENM OUTPUTS
  output$enm_indicators_table <- renderReactable({
    req(indicator_table_data())
    create_indicator_reactable(indicator_table_data(), c("favchange"))
  })

  output$enm_diff_plot <- renderPlot({
    req(cu_spatial())

    tryCatch(
      {
        spatial <- cu_spatial()

        stream_indicator_plot(spatial$fw_sp, spatial$boundary, spatial$lakes,
          Tw_stations = NULL,
          variable = "favchange_45",
          plot_title = "Change in ENM Favourability - 1981-2000 to 2041-2060",
          unit_label = "Change in Favourability",
          histogram_fill = "model_access_salmon",
          xlim = c(-1, 1),
          risk_palette = "berlin",
          palette_direction = -1,
          temp_stations = FALSE
        )
      },
      error = function(e) {
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
        text(1, 1, paste("Error:\n", e$message), cex = 0.8)
      }
    )
  })

  # SPAWNING THREATS OUTPUT
  output$threats_indicators_table <- renderReactable({
    req(indicator_table_data())
    create_indicator_reactable(indicator_table_data(), c("cthr"))
  })

  output$ct_plot <- renderPlot({
    req(cu_spatial())

    tryCatch(
      {
        spatial <- cu_spatial()

        stream_indicator_plot(spatial$acc_sp, spatial$boundary, spatial$lakes,
          Tw_stations = NULL,
          variable = "cthr_anad",
          plot_title = "Cumulative Threats to Stream Habitat",
          unit_label = "Cumulative Threat Score",
          risk_palette = "lajolla",
          palette_direction = -1,
          temp_stations = FALSE
        )
      },
      error = function(e) {
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
        text(1, 1, paste("Error:\n", e$message), cex = 0.8)
      }
    )
  })

  # MIGRATION OUTPUTS
  output$migration_indicators_table <- renderReactable({
    req(indicator_table_data())
    create_indicator_reactable(indicator_table_data(), c("migrTproj", "migrQpdelta", "migrdist"))
  })

  output$migration_path_plot <- renderPlot({
    req(input$cu_select, cu_spatial(), cu_timing_data())

    spatial <- cu_spatial()
    cu_timing_i <- cu_timing_Fr %>% filter(FULL_CU_IN == input$cu_select)

    if (input$cu_select %in% names(migr_list)) {
      migr_cu <- migr_list[[input$cu_select]]

      migration_path_timing_plot(
        migr_path = migr_cu,
        nuseds_data = spatial$nuseds,
        cu_boundary = spatial$boundary,
        migr_daily_all = migr_daily_all,
        cu_i = input$cu_select,
        timing = cu_timing_i,
        rcp = "45",
        period_choose = c("1981-2010", "2041-2060")
      )
    }
  })

  # MARINE OUTPUTS
  output$sst_indicators_table <- renderReactable({
    req(indicator_table_data())
    create_indicator_reactable(indicator_table_data(), c("SSTproj", "SSTrate"))
  })

  output$maz_boundary_plot <- renderPlot({
    req(input$cu_select)
    cu_MAZ <- cvis_cu_list$MAZ[cvis_cu_list$FULL_CU_IN == input$cu_select][1]
    req(!is.na(cu_MAZ) && cu_MAZ != "")
    MAZ_boundary_highlight(MAZ, MAZ_pick = cu_MAZ)
  })

  output$sst_plot <- renderPlot({
    req(input$cu_select)

    cu_timing_i <- cu_timing_Fr %>% filter(FULL_CU_IN == input$cu_select)
    cu_MAZ <- cvis_cu_list$MAZ[cvis_cu_list$FULL_CU_IN == input$cu_select][1]

    if (nrow(cu_timing_i) > 0 && !is.na(cu_MAZ) && cu_MAZ != "") {
      months_include <- seq(
        from = cu_timing_i$ns_start_month[1],
        to = cu_timing_i$ns_end_month[1],
        by = 1
      )

      SST_cu_sp <- get_spatial_var(CMIP6_SST,
        months = months_include,
        period_pick = 3,
        rcp_pick = "45"
      )

      MAZ_cu <- MAZ %>% filter(MAZ_Acrony == cu_MAZ)

      marine_indicator_plot(SST_cu_sp,
        MAZ_sp = MAZ_cu,
        var = "SST_oe",
        unit_label = "Temperature (°C)",
        plot_title = "Projected SST - 2041-2060, RCP 4.5",
        risk_palette = "roma",
        palette_direction = -1,
        palette_limits = c(9, 16)
      )
    }
  })

  # MARINE IMPACTS OUTPUT
  output$impacts_indicators_table <- renderReactable({
    req(indicator_table_data())
    create_indicator_reactable(indicator_table_data(), c("CImpact"))
  })

  output$marine_impacts_plot <- renderPlot({
    req(input$cu_select)

    cu_MAZ <- cvis_cu_list$MAZ[cvis_cu_list$FULL_CU_IN == input$cu_select][1]
    req(!is.na(cu_MAZ) && cu_MAZ != "")
    CI_cu <- CImpact_points %>% filter(MAZ_Acrony == cu_MAZ)
    MAZ_cu <- MAZ %>% filter(MAZ_Acrony == cu_MAZ)

    marine_indicator_plot(CI_cu,
      MAZ_sp = MAZ_cu,
      var = "Cumul_Impact_ALL",
      unit_label = "Cumulative Impact Score",
      plot_title = "Cumulative Impacts on Marine Habitat",
      risk_palette = "lajolla",
      palette_direction = -1,
      palette_limits = c(NA, NA)
    )
  })
}

################################################################################
# Run App
################################################################################

shinyApp(ui = ui, server = server)
