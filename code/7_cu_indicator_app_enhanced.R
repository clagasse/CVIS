################################################################################
#
# CU Climate Vulnerability Indicators - Enhanced Interactive Dashboard
#
# An improved interactive Shiny application for exploring climate vulnerability
# indicators for Pacific Salmon Conservation Units in the Fraser River watershed
#
################################################################################


# DATA LOADING-------------------------------------------------------------



# # Load required libraries
# library(shiny)
# library(shinydashboard)
# library(shinyWidgets)
# library(here)
# library(tidyverse)
# library(sf)
# library(gt)
# library(patchwork)
# library(scico)
# library(ggspatial)
# library(DT)
#
# # Source setup and utility functions
# setwd(here())
# source(file.path(here(), "code", "0_setup.R"))
#
# # Load required data files
# load(file.path(paths$fw, "fw_streampicks_tscapes.Rdata"))
# load(file.path(paths$fw, "2025-10-03_fw_rearing_indicators.Rdata"))
# load(file.path(paths$fw, "2025-09-29_fw_upstream_paths.Rdata"))
# load(file.path(paths$fw, "2025-09-30_migr_stats.Rdata"))
# load(file.path(paths$marine, "2025-09-29_marine_stats.Rdata"))
# load(file.path(paths$fw, "fw_models_tscapes.Rds"))
# load(file.path(paths$fw, "fw_stream_indicators_sp.Rds"))
# load(file.path(paths$fw, "BC_FWA_LAKES_FR.Rds"))
#
# # Load spatial data
# basins <- st_read(file.path(paths$spatial, "BC_Basins", "BC_Basins_GoogleMapPL.shp"), quiet = TRUE) %>%
#   st_cast("POLYGON")
# st_crs(basins) <- 4269
# basins <- st_transform(basins, crs = 3005)
# Fr_basin <- filter(basins, BASIN == "FRASER")
#
# stations_stats <- read.csv(file.path(paths$climate, "Ruzzante_low_flows", "stations_performance.csv"))
# watershed_flow <- st_read(file.path(paths$climate, "Ruzzante_low_flows", "watersheds.gpkg"), quiet = TRUE) %>%
#   left_join(select(stations_stats, ID, regime), by = c("ID" = "ID")) %>%
#   mutate(regime = as.factor(regime)) %>%
#   st_transform(3005)
#
# Tw_stations <- st_read(file.path(paths$climate, "Tw_stations.gdb"), quiet = TRUE)
# stations_flow <- st_read(file.path(paths$climate, "Ruzzante_low_flows", "stations.gpkg"), quiet = TRUE) %>%
#   st_transform(3005)
#
# SST_grid <- st_read(file.path(paths$climate, "Standardized_Marine_data", "Grid", "SST_bc_coast.gdb"))
# MAZ <- st_read(file.path(paths$spatial, "MAZ", "MAZ_Final.shp"))
#
# # Load combined indicator data
# source(file.path(paths$code, "4a_CU_scoring.R"))


# UI ----------------------------------------------------------------------

ui <- dashboardPage(

  skin = "blue",

  dashboardHeader(
    title = "CU Climate Vulnerability Dashboard",
    titleWidth = 350
  ),

  dashboardSidebar(
    width = 350,
    sidebarMenu(
      id = "tabs",

      # CU Selection with enhanced styling
      div(style = "padding: 15px;",
        h4("Conservation Unit Selection", style = "color: white; margin-top: 0;"),
        selectInput("cu_select",
          NULL,
          choices = setNames(cu_run$FULL_CU_IN, cu_run$CVIS_NAME),
          selected = cu_run$FULL_CU_IN[1],
          width = "100%"),

        # Quick stats display
        uiOutput("sidebar_stats")
      ),

      hr(style = "border-color: white;"),

      # Menu items with icons
      menuItem("About", tabName = "about", icon = icon("info-circle")),
      menuItem("Overview", tabName = "overview", icon = icon("map-marked-alt")),
      menuItem("Demographics", tabName = "demographics", icon = icon("users")),
      menuItem("Life Stage Timing", tabName = "timing", icon = icon("clock")),
      menuItem("Spawning & Rearing", tabName = "spawning", icon = icon("water")),
      menuItem("Upstream Migration", tabName = "migration", icon = icon("arrow-up")),
      menuItem("Nearshore Marine", tabName = "marine", icon = icon("ship"))
    )
  ),


  # Dashboard body ----------------------------------------------------------

  dashboardBody(

    # Custom CSS for better styling
    tags$head(
      tags$style(HTML("
        .info-box {
          min-height: 90px;
        }
        .info-box-icon {
          height: 90px;
          line-height: 90px;
        }
        .info-box-content {
          padding: 5px 10px;
        }
        .box-title {
          font-size: 16px;
          font-weight: bold;
        }
        .help-icon {
          float: right;
          color: #3c8dbc;
          cursor: help;
        }
        .interpretation-box {
          background-color: #f4f4f4;
          padding: 10px;
          border-left: 4px solid #3c8dbc;
          margin: 10px 0;
        }
        .value-box-text {
          font-size: 14px;
        }
        .main-header .logo {
          font-weight: bold;
        }
        .methodology-text {
          font-size: 13px;
          line-height: 1.6;
        }
      "))
    ),

    tabItems(


      # About tab ---------------------------------------------------------------
      tabItem(tabName = "about",
        fluidRow(
          box(title = "About This Dashboard", width = 12, status = "primary", solidHeader = TRUE,
            h3("Climate Vulnerability Indicators for Pacific Salmon"),
            p("This interactive dashboard provides climate vulnerability information
                      for Pacific salmon Conservation Units (CUs) in the Fraser River watershed."),
            tags$ul(
              tags$li("Understand relative climate vulnerability across different CUs"),
              tags$li("Identify key environmental stressors for each population"),
              tags$li("Support evidence-based conservation planning"),
              tags$li("Contextualize risks and benefits of management actions")
            )
          )
        ),

        fluidRow(
          box(title = "Framework Overview", width = 6, status = "info", solidHeader = TRUE,
            h4("Indicator Categories"),
            tags$ul(
              tags$li(tags$b("Demographics:"), "Population status and abundance"),
              tags$li(tags$b("Spawning & Rearing:"), "Freshwater habitat conditions, temperature, flow, and threats"),
              tags$li(tags$b("Upstream Migration:"), "Migration pathways, temperature, and flow during migration"),
              tags$li(tags$b("Nearshore Marine:"), "Ocean entry conditions and sea surface temperature")
            ),

            h4("Emissions Scenario"),
            p("Results are shown for RCP 4.5 (moderate emissions scenario) for the mid-century
                      period (2041-2060), based on CMIP5 global climate models.")
          ),

          box(title = "How to Use This Dashboard", width = 6, status = "success", solidHeader = TRUE,
            h4("Getting Started"),
            tags$ol(
              tags$li(tags$b("Select a CU:"), "Use the dropdown menu in the sidebar"),
              tags$li(tags$b("Navigate Tabs:"), "Click on different tabs to explore various indicators"),
              tags$li(tags$b("Interpret Values:"), "Standardized scores range from 0 (low risk) to 1 (high risk)"),
              tags$li(tags$b("Compare:"), "Blue dots show CU values, horizontal lines show species averages")
            ),

            h4("Understanding Plots"),
            tags$ul(
              tags$li(tags$b("Lollipop Charts:"), "Compare CU values to species averages"),
              tags$li(tags$b("Maps:"), "Spatial distribution of indicators"),
              tags$li(tags$b("Histograms:"), "Distribution across stream segments"),
              tags$li(tags$b("Error Bars:"), "Show uncertainty from climate models or spatial variation")
            )
          )
        ),

        fluidRow(
          box(title = "Data Sources & Methods", width = 12, status = "warning", solidHeader = TRUE,
            collapsible = TRUE, collapsed = TRUE,
            h4("Key Data Sources"),
            tags$ul(
              tags$li("BCFishPass - Stream accessibility and habitat models"),
              tags$li("PCIC VIC-GL - Hydrological projections"),
              tags$li("Thermalscapes - Stream temperature projections"),
              tags$li("PSF - Life history timing data"),
              tags$li("NuSEDS - Spawning site locations"),
              tags$li("DFO - Wild Salmon Policy status assessments"),
              tags$li("SalishSeaCast - Marine projections")
            ),

            h4("References"),
            p("Wilson, S.M. and Peacock, S.J. (2025). Life history timing data.
                      Can. J. Fish. Aquat. Sci. doi:10.1139/cjfas-2024-0213"),
            p("For detailed methodology, see the CVIS Overview report.")
          )
        )
      ),

      # Overview Tab ------------------------------------------------------------

      tabItem(tabName = "overview",
        fluidRow(
          # Value boxes for key metrics
          uiOutput("overview_valueboxes")
        ),

        fluidRow(
          box(title = tagList("CU Boundary Location",
            tags$i(class = "fa fa-info-circle help-icon",
              title = "Shows the geographic location of this CU within the Fraser Basin")),
          width = 8, status = "primary", solidHeader = TRUE,
          plotOutput("boundary_plot", height = "500px")),

          box(title = "Conservation Unit Details", width = 4, status = "info", solidHeader = TRUE,
            gt_output("cu_info_table"),
            hr(),
            h4("Quick Facts"),
            uiOutput("cu_quick_facts")),

          box(title = "Indicators", width = 8, status = "primary", solidHeader = TRUE,
            plotOutput("cu_indicators_plot", height = "600px"))
        )
      ),

      # Demographics Tab --------------------------------------------------------
      tabItem(tabName = "demographics",
        fluidRow(
          infoBoxOutput("status_box", width = 4),
          infoBoxOutput("abundance_box", width = 4),
          infoBoxOutput("assessment_year_box", width = 4)
        ),


        fluidRow(
          box(title = tagList("Status and Abundance Over Time",
            tags$i(class = "fa fa-info-circle help-icon",
              title = "Wild Salmon Policy status based on recent assessments")),
          width = 12, status = "primary", solidHeader = TRUE,
          gt_output("demographics_table"))
        ),

        fluidRow(
          box(title = "Demographic Indicators", width = 6, status = "info", solidHeader = TRUE,
            gt_output("demographics_indicators_table"),
            div(class = "interpretation-box",
              h4("Interpretation Guide"),
              tags$ul(
                tags$li(tags$b("WSP Status:"), "Red = high concern, Amber = moderate, Green = healthy"),
                tags$li(tags$b("Abundance:"), "Lower values indicate higher vulnerability"),
                tags$li("Standardized scores facilitate comparison across CUs")
              )
            )
          ),

          box(title = "Demographic Indicator Values", width = 6, status = "success", solidHeader = TRUE,
            plotOutput("demographics_plot", height = "400px"),
            div(class = "interpretation-box",
              p("Blue points show this CU's standardized scores.
                          Horizontal segments compare to species average.")
            )
          )
        )
      ),

      # Timing Tab --------------------------------------------------------------

      tabItem(tabName = "timing",
        fluidRow(
          box(title = tagList("Life Stage Timing",
            tags$i(class = "fa fa-info-circle help-icon",
              title = "Timing data from Wilson and Peacock (2025)")),
          width = 12, status = "primary", solidHeader = TRUE,
          plotOutput("timing_plot", height = "500px"),
          div(class = "interpretation-box",
            tags$ul(
              tags$li(tags$b("Lines:"), "Represent the timing range (start to end) for each life stage"),
              tags$li(tags$b("Points:"), "Indicate peak timing"),
              tags$li(tags$b("Point Size:"), "Reflects data quality (larger = higher quality, scale 1-6)"),
              tags$li(tags$b("Data Quality:"), "Based on number of observations and recentness")
            )
          )
          )
        ),

        fluidRow(
          box(title = "Timing Summary", width = 12, status = "info", solidHeader = TRUE,
            collapsible = TRUE,
            uiOutput("timing_summary"))
        )
      ),

      # Spawning & Rearing Tab
      tabItem(tabName = "spawning",

        # Indicator summary boxes
        fluidRow(
          uiOutput("fwr_summary_boxes")
        ),

        # Tabbed content for better organization
        tabBox(
          title = "Spawning & Rearing Indicators",
          width = 12,

          tabPanel("Overview",
            fluidRow(
              box(title = "Indicator Definitions", width = 6, status = "info",
                gt_output("fwr_indicators_table")),
              box(title = "Indicator Values", width = 6, status = "success",
                plotOutput("fwr_indicators_plot", height = "400px"))
            )
          ),

          tabPanel("Stream Habitat",
            fluidRow(
              box(title = tagList("Stream Accessibility",
                tags$i(class = "fa fa-info-circle help-icon",
                  title = "Shows accessible streams and spawning/rearing potential from BC Fishpass model")),
              width = 7, status = "primary",
              plotOutput("accessibility_plot", height = "500px")),

              box(title = "Stream Statistics", width = 5, status = "info",
                gt_output("stream_stats_table"),
                hr(),
                gt_output("nuseds_stats_table"))
            )
          ),

          tabPanel("Hydrology",
            fluidRow(
              box(title = "Hydrologic Regime", width = 8, status = "primary",
                plotOutput("regime_plot", height = "500px")),
              box(title = "Coverage Statistics", width = 4, status = "info",
                gt_output("hydro_coverage_table"),
                div(class = "interpretation-box", style = "margin-top: 15px;",
                  h4("Regime Types"),
                  tags$ul(
                    tags$li(tags$b("Snow:"), "Spring freshet dominated"),
                    tags$li(tags$b("Rain:"), "Winter peak flows"),
                    tags$li(tags$b("Hybrid:"), "Mixed patterns"),
                    tags$li(tags$b("Glacial:"), "Summer melt dominated")
                  )
              ))
            ),

            fluidRow(
              box(title = tagList("August Flow Change",
                tags$i(class = "fa fa-info-circle help-icon",
                  title = "Projected change in August flows (2041-2060 vs historical)")),
              width = 6, status = "warning",
              plotOutput("q8_plot", height = "450px")),

              box(title = tagList("Nov-Jan Flow Change",
                tags$i(class = "fa fa-info-circle help-icon",
                  title = "Changes during incubation period may increase scour risk")),
              width = 6, status = "warning",
              plotOutput("qndj_plot", height = "450px"))
            )
          ),

          tabPanel("Temperature",
            fluidRow(
              box(title = tagList("August Temperature Projection",
                tags$i(class = "fa fa-info-circle help-icon",
                  title = "Mean August temperature for 2041-2060 period")),
              width = 6, status = "danger",
              plotOutput("temp_plot", height = "450px")),

              box(title = tagList("Temperature Rate of Change",
                tags$i(class = "fa fa-info-circle help-icon",
                  title = "Rate of warming from historical to future period")),
              width = 6, status = "danger",
              plotOutput("temp_rate_plot", height = "450px"))
            ),

            fluidRow(
              box(title = "Temperature Interpretation", width = 12, status = "info",
                collapsible = TRUE, collapsed = TRUE,
                div(class = "methodology-text",
                  p(tags$b("Critical Temperature Thresholds:")),
                  tags$ul(
                    tags$li("18-20°C: Optimal range for most salmon species"),
                    tags$li("20-23°C: Sublethal stress effects"),
                    tags$li(">23°C: Lethal temperatures for extended exposure")
                  ),
                  p(tags$b("Rate of Change:"), "Rapid warming may exceed adaptive capacity of populations")
              ))
            )
          ),

          tabPanel("Habitat Suitability",
            fluidRow(
              box(title = tagList("ENM Favourability (2041-2060)",
                tags$i(class = "fa fa-info-circle help-icon",
                  title = "Ecological Niche Model projections of habitat suitability")),
              width = 6, status = "success",
              plotOutput("enm_plot", height = "450px")),

              box(title = tagList("ENM Change",
                tags$i(class = "fa fa-info-circle help-icon",
                  title = "Change in favourability from historical to future")),
              width = 6, status = "success",
              plotOutput("enm_diff_plot", height = "450px"))
            ),

            fluidRow(
              box(title = "About ENM", width = 12, status = "info",
                collapsible = TRUE, collapsed = TRUE,
                div(class = "methodology-text",
                  p("Ecological Niche Models predict habitat suitability based on environmental conditions."),
                  p(tags$b("Favourability scores:"), "Range from 0 (unsuitable) to 1 (highly suitable)"),
                  p(tags$b("Interpretation:")),
                  tags$ul(
                    tags$li("Positive values in change plot = improving conditions"),
                    tags$li("Negative values = declining suitability"),
                    tags$li("Models based on temperature, flow, and landscape characteristics")
                  )
              ))
            )
          ),

          tabPanel("Cumulative Threats",
            fluidRow(
              box(title = tagList("Cumulative Stressors",
                tags$i(class = "fa fa-info-circle help-icon",
                  title = "Combined impacts of multiple stressors on habitat")),
              width = 12, status = "warning",
              plotOutput("ct_plot", height = "500px"))
            ),

            fluidRow(
              box(title = "Threat Categories", width = 12, status = "info",
                collapsible = TRUE, collapsed = TRUE,
                div(class = "methodology-text",
                  p("Cumulative threat scores integrate multiple stressor categories:"),
                  tags$ul(
                    tags$li("Barriers and obstructions"),
                    tags$li("Land use alterations"),
                    tags$li("Water quality degradation"),
                    tags$li("Habitat fragmentation"),
                    tags$li("Climate-related changes")
                  ),
                  p(tags$b("Higher scores"), "indicate greater cumulative impact on habitat quality")
              ))
            )
          )
        )
      ),

      # Migration Tab
      tabItem(tabName = "migration",
        fluidRow(
          uiOutput("migration_summary_boxes")
        ),

        fluidRow(
          box(title = "Migration Indicators", width = 6, status = "info", solidHeader = TRUE,
            gt_output("migr_indicators_table"),
            div(class = "interpretation-box",
              h4("Key Concerns"),
              tags$ul(
                tags$li("High temperatures during migration increase energy costs"),
                tags$li("Thermal barriers (>21°C) can block migration"),
                tags$li("Low flows may reduce passage success"),
                tags$li("Longer migration distances increase exposure")
              )
            )
          ),

          box(title = "Migration Indicator Values", width = 6, status = "success", solidHeader = TRUE,
            plotOutput("migr_indicators_plot", height = "400px"))
        ),

        fluidRow(
          box(title = tagList("Migration Path from Ocean to Spawning Grounds",
            tags$i(class = "fa fa-info-circle help-icon",
              title = "Shows the migration route and distance to spawning sites")),
          width = 12, status = "primary", solidHeader = TRUE,
          plotOutput("migration_path_plot", height = "600px"))
        ),

        fluidRow(
          box(title = tagList("Migration Timing and Temperature",
            tags$i(class = "fa fa-info-circle help-icon",
              title = "Temperature conditions during migration window")),
          width = 12, status = "warning", solidHeader = TRUE,
          plotOutput("migration_timing_plot", height = "500px"),
          div(class = "interpretation-box",
            tags$ul(
              tags$li(tags$b("Vertical lines:"), "Show run timing and spawn timing windows"),
              tags$li(tags$b("Shaded area:"), "10th-90th percentile temperature range"),
              tags$li(tags$b("Multiple lines:"), "Compare historical (1981-2010) vs future (2041-2060) periods")
            )
          )
          )
        )
      ),

      # Marine Tab
      tabItem(tabName = "marine",
        fluidRow(
          uiOutput("marine_summary_boxes")
        ),

        fluidRow(
          box(title = "Marine Indicators", width = 6, status = "info", solidHeader = TRUE,
            gt_output("marine_indicators_table"),
            div(class = "interpretation-box",
              h4("Ocean Entry Conditions"),
              p("Sea surface temperature during and around ocean entry affects:"),
              tags$ul(
                tags$li("Juvenile growth and survival"),
                tags$li("Prey availability and distribution"),
                tags$li("Predation risk"),
                tags$li("Disease susceptibility")
              )
            )
          ),

          box(title = "Marine Indicator Values", width = 6, status = "success", solidHeader = TRUE,
            plotOutput("marine_indicators_plot", height = "400px"))
        ),

        fluidRow(
          box(title = tagList("Projected Sea Surface Temperature",
            tags$i(class = "fa fa-info-circle help-icon",
              title = "SST during ocean entry period (2046-2065)")),
          width = 12, status = "primary", solidHeader = TRUE,
          plotOutput("sst_plot", height = "500px"),
          div(class = "interpretation-box",
            uiOutput("sst_interpretation"))
          )
        )
      )
    )
  )
)


# SERVER ------------------------------------------------------------------

server <- function(input, output, session) {
  # Reactive values for selected CU
  cu_data <- reactive({
    req(input$cu_select)

    cu_i <- input$cu_select
    cu_run_i <- cu_run[cu_run$FULL_CU_IN == cu_i, ]
    sp_pick <- cu_run_i$SPECIES_NAME
    sp_pick_bcfp <- spp_lookup$spp_abr_bcfp[spp_lookup$SPECIES_NAME == sp_pick]
    sp_pick_ENM <- str_to_lower(sp_pick)[1]

    # Subset spatial data
    fwR_cu <- fwR_all[[cu_i]]
    stream_cu_sub <- stream_cu_picks[, colnames(stream_cu_picks) == cu_i]
    cu_boundary_i <- cu_boundary[cu_boundary$FULL_CU_IN == cu_i, ]
    nuseds_cu <- nuseds_Fr[nuseds_Fr$FULL_CU_IN == cu_i, ]

    # Process stream data
    fw_sp_cu <- fw_sp_ind[stream_cu_sub, ] %>%
      rename(keep_habitat = contains(paste0("model_habitat_", sp_pick_bcfp)),
        favchange_45 = contains(paste0("favchange_", sp_pick_ENM, "_45_3")),
        favchange_85 = contains(paste0("favchange_", sp_pick_ENM, "_85_3")),
        fav_45 = contains(paste0("fav_", sp_pick_ENM, "_45_3")),
        fav_85 = contains(paste0("fav_", sp_pick_ENM, "_85_3"))) %>%
      mutate(model_rs = if_any(starts_with("keep_habitat"), ~ . == TRUE)) %>%
      mutate(model_rs = if_else(is.na(model_rs), FALSE, model_rs)) %>%
      mutate(model_rs = factor(model_rs, levels = c(TRUE, FALSE),
        labels = c("1-SPAWNING/REARING", "2-NOT SPAWNING/REARING"))) %>%
      select(-starts_with(c("model_spawning", "model_rearing", "known_rearing", "known_spawning")))

    # Accessible streams only
    acc_sp_cu <- fw_sp_cu %>%
      filter(model_access_salmon %in% c("OBSERVED", "INFERRED"))

    # Subset flow stations
    temp <- unlist(st_intersects(cu_boundary_i, watershed_flow))
    watershed_flow_cu <- if (length(temp) > 0) watershed_flow[temp, ] else NULL

    temp <- unlist(st_intersects(cu_boundary_i, stations_flow))
    stations_cu <- if (length(temp) > 0) stations_flow[temp, ] else NULL

    # Migration data
    migr_cu <- migr_list[[cu_i]]
    migr_flat_cu <- migr_all_flat[migr_all_flat$FULL_CU_IN == cu_i, ]

    # Lakes
    temp <- unlist(st_intersects(cu_boundary_i, lakes_Fr))
    lakes_cu <- if (length(temp) > 0) lakes_Fr[temp, ] else st_sf(geometry = st_sfc())

    # Timing data
    cu_timing_long_i <- cu_timing_long[cu_timing_long$FULL_CU_IN == cu_i, ]
    cu_timing_i <- cu_timing_Fr[cu_timing_Fr$FULL_CU_IN == cu_i, ]

    # Indicator data
    ind_cu <- get_CU_indicators(all_flat_std,
      cu_i,
      indicators_choose = tbl_indicators$abbrev,
      RCP_pick = "45",
      period_pick = "3")

    # Marine data
    cu_mar <- mar_all_flat[mar_all_flat$FULL_CU_IN == cu_i, ]

    list(
      cu_i = cu_i,
      cu_run_i = cu_run_i,
      sp_pick = sp_pick,
      sp_pick_ENM = sp_pick_ENM,
      fwR_cu = fwR_cu,
      cu_boundary_i = cu_boundary_i,
      nuseds_cu = nuseds_cu,
      fw_sp_cu = fw_sp_cu,
      acc_sp_cu = acc_sp_cu,
      watershed_flow_cu = watershed_flow_cu,
      stations_cu = stations_cu,
      migr_cu = migr_cu,
      migr_flat_cu = migr_flat_cu,
      lakes_cu = lakes_cu,
      cu_timing_long_i = cu_timing_long_i,
      cu_timing_i = cu_timing_i,
      ind_cu = ind_cu,
      cu_mar = cu_mar
    )
  })

  # Sidebar stats
  output$sidebar_stats <- renderUI({
    data <- cu_data()

    status_color <- case_when(
      data$cu_run_i$CUstatus == "Red" ~ "red",
      data$cu_run_i$CUstatus == "Amber" ~ "yellow",
      data$cu_run_i$CUstatus == "Green" ~ "green",
      TRUE ~ "gray"
    )

    div(style = "background: rgba(255,255,255,0.1); padding: 10px; border-radius: 5px; margin-top: 10px;",
      tags$table(style = "color: white; width: 100%; font-size: 12px;",
        tags$tr(
          tags$td(tags$b("Species:")),
          tags$td(data$sp_pick)
        ),
        tags$tr(
          tags$td(tags$b("Status:")),
          tags$td(span(style = paste0("color: ", status_color, "; font-weight: bold;"),
            data$cu_run_i$CUstatus))
        ),
        tags$tr(
          tags$td(tags$b("Year:")),
          tags$td(data$cu_run_i$status_year)
        )
      )
    )
  })



  #   Overview value boxes  --------


  output$overview_valueboxes <- renderUI({
    data <- cu_data()

    # Calculate some summary stats
    stream_length <- round(data$fwR_cu$streams$total_length_acc / 1000, 0)
    n_spawning_sites <- nrow(data$nuseds_cu)

    # Get migration distance if available
    if (!is.null(data$migr_flat_cu) && nrow(data$migr_flat_cu) > 0) {
      migr_dist <- round(data$migr_flat_cu$migr_wdist_mean[1] / 1000, 0)
    } else {
      migr_dist <- "N/A"
    }

    fluidRow(
      valueBox(
        value = data$sp_pick,
        subtitle = "Species",
        icon = icon("fish"),
        color = "blue"
      ),
      valueBox(
        value = paste0(stream_length, " km"),
        subtitle = "Accessible Stream Length",
        icon = icon("water"),
        color = "aqua"
      ),
      valueBox(
        value = n_spawning_sites,
        subtitle = "Spawning Sites (NuSEDS)",
        icon = icon("map-marker-alt"),
        color = "green"
      ),
      valueBox(
        value = paste0(migr_dist, " km"),
        subtitle = "Migration Distance",
        icon = icon("arrow-up"),
        color = "orange"
      )
    )
  })

  # Demographics info boxes -------------------------------------------------

  output$status_box <- renderInfoBox({
    data <- cu_data()

    status_color <- case_when(
      data$cu_run_i$CUstatus == "Red" ~ "red",
      data$cu_run_i$CUstatus %in% c("Amber", "Red/Amber") ~ "yellow",
      data$cu_run_i$CUstatus == "Green" ~ "green",
      TRUE ~ "light-blue"
    )

    infoBox(
      title = "WSP Status",
      value = data$cu_run_i$CUstatus,
      icon = icon("exclamation-triangle"),
      color = status_color
    )
  })

  output$abundance_box <- renderInfoBox({
    data <- cu_data()

    abundance <- ifelse(is.na(data$cu_run_i$CUnmat),
      "No Data",
      format(round(data$cu_run_i$CUnmat, 0), big.mark = ","))

    infoBox(
      title = "Wild Spawner Abundance",
      value = abundance,
      icon = icon("users"),
      color = "aqua"
    )
  })

  output$assessment_year_box <- renderInfoBox({
    data <- cu_data()

    infoBox(
      title = "Assessment Year",
      value = data$cu_run_i$status_year,
      icon = icon("calendar"),
      color = "blue"
    )
  })


  # FWR summary boxes -------------------------------------------------------

  output$fwr_summary_boxes <- renderUI({
    data <- cu_data()
    ind_data <- data$ind_cu %>%
      filter(indicator %in% c("tw8proj", "lowQpdelta", "ct", "favchange")) %>%
      filter(stat == "mean")

    # Get values
    temp_val <- ind_data$cu_value[ind_data$indicator == "tw8proj"]
    flow_val <- ind_data$cu_value[ind_data$indicator == "lowQpdelta"]
    ct_val <- ind_data$cu_value[ind_data$indicator == "ct"]
    enm_val <- ind_data$cu_value[ind_data$indicator == "favchange"]

    # Determine colors based on values
    temp_color <- if (temp_val > 0.7) "red" else if (temp_val > 0.4) "yellow" else "green"
    flow_color <- if (flow_val > 0.7) "red" else if (flow_val > 0.4) "yellow" else "green"
    ct_color <- if (ct_val > 0.7) "red" else if (ct_val > 0.4) "yellow" else "green"
    enm_color <- if (enm_val > 0.7) "red" else if (enm_val > 0.4) "yellow" else "green"

    fluidRow(
      infoBox(
        title = "Temperature Risk",
        value = sprintf("%.2f", temp_val),
        subtitle = "Standardized Score",
        icon = icon("thermometer-full"),
        color = temp_color,
        width = 3
      ),
      infoBox(
        title = "Flow Change Risk",
        value = sprintf("%.2f", flow_val),
        subtitle = "Standardized Score",
        icon = icon("tint"),
        color = flow_color,
        width = 3
      ),
      infoBox(
        title = "Cumulative Threats",
        value = sprintf("%.2f", ct_val),
        subtitle = "Standardized Score",
        icon = icon("exclamation-circle"),
        color = ct_color,
        width = 3
      ),
      infoBox(
        title = "Habitat Change",
        value = sprintf("%.2f", enm_val),
        subtitle = "ENM Favourability Change",
        icon = icon("leaf"),
        color = enm_color,
        width = 3
      )
    )
  })

  # Migration summary boxes
  output$migration_summary_boxes <- renderUI({
    data <- cu_data()
    ind_data <- data$ind_cu %>%
      filter(indicator %in% c("migrT", "migrA21", "migrdist")) %>%
      filter(stat == "mean")

    temp_val <- ind_data$cu_value[ind_data$indicator == "migrT"]
    barrier_val <- ind_data$cu_value[ind_data$indicator == "migrA21"]
    dist_val <- ind_data$cu_value[ind_data$indicator == "migrdist"]

    temp_color <- if (temp_val > 0.7) "red" else if (temp_val > 0.4) "yellow" else "green"
    barrier_color <- if (barrier_val > 0.7) "red" else if (barrier_val > 0.4) "yellow" else "green"
    dist_color <- if (dist_val > 0.7) "orange" else if (dist_val > 0.4) "yellow" else "aqua"

    fluidRow(
      infoBox(
        title = "Migration Temperature",
        value = sprintf("%.2f", temp_val),
        subtitle = "Standardized Score",
        icon = icon("temperature-high"),
        color = temp_color,
        width = 4
      ),
      infoBox(
        title = "Thermal Barrier Risk",
        value = sprintf("%.2f", barrier_val),
        subtitle = "Proportion >21°C",
        icon = icon("ban"),
        color = barrier_color,
        width = 4
      ),
      infoBox(
        title = "Migration Distance",
        value = sprintf("%.2f", dist_val),
        subtitle = "Relative to other CUs",
        icon = icon("route"),
        color = dist_color,
        width = 4
      )
    )
  })

  # Marine summary boxes
  output$marine_summary_boxes <- renderUI({
    data <- cu_data()
    ind_data <- data$ind_cu %>%
      filter(indicator == "SSTproj") %>%
      filter(stat == "mean")

    sst_val <- ind_data$cu_value[1]
    sst_color <- if (sst_val > 0.7) "red" else if (sst_val > 0.4) "yellow" else "green"

    fluidRow(
      infoBox(
        title = "SST Risk",
        value = sprintf("%.2f", sst_val),
        subtitle = "Standardized Score",
        icon = icon("ship"),
        color = sst_color,
        width = 4
      ),
      infoBox(
        title = "Ocean Entry Timing",
        value = ifelse(nrow(data$cu_mar) > 0,
          paste(month.name[data$cu_mar$ns_timing_start[1]], "-",
            month.name[data$cu_mar$ns_timing_end[1]]),
          "N/A"),
        subtitle = "Entry Window",
        icon = icon("calendar-alt"),
        color = "blue",
        width = 4
      ),
      infoBox(
        title = "Marine Zone",
        value = ifelse(nrow(data$cu_mar) > 0, data$cu_mar$MAZ[1], "N/A"),
        subtitle = "Adaptive Zone",
        icon = icon("water"),
        color = "aqua",
        width = 4
      )
    )
  })

  # Overview Tab Outputs
  output$boundary_plot <- renderPlot({
    data <- cu_data()
    cu_boundary_highlight(cu_boundary, cu_pick = data$cu_i)
  })

  output$cu_info_table <- render_gt({
    data <- cu_data()
    data$cu_run_i %>%
      select(FULL_CU_IN, CU_NAME, SPECIES_NAME) %>%
      gt() %>%
      tab_header(title = "Conservation Unit Information") %>%
      cols_label(
        FULL_CU_IN = "CU ID",
        CU_NAME = "CU Name",
        SPECIES_NAME = "Species"
      )
  })

  output$cu_quick_facts <- renderUI({
    data <- cu_data()

    tags$ul(
      tags$li(tags$b("CU ID:"), data$cu_i),
      tags$li(tags$b("Species:"), data$sp_pick),
      tags$li(tags$b("Region:"), "Fraser River"),
      tags$li(tags$b("Spawning Sites:"), nrow(data$nuseds_cu))
    )
  })

  output$cu_indicators_plot <- renderPlot({
    data <- cu_data()
    plot_cu_indicators_lollipop(data$ind_cu)
  })



  # Demographics tab outputs ------------------------------------------------

  output$demographics_table <- render_gt({
    data <- cu_data()
    dem_details <- data$cu_run_i %>%
      select(FULL_CU_IN, CU_NAME, CUstatus, CUnmat, status_year)

    dem_details %>%
      gt() %>%
      tab_header(title = "Status and Abundance") %>%
      cols_label(
        FULL_CU_IN = "CU ID",
        CU_NAME = "CU Name",
        CUstatus = "WSP Status",
        CUnmat = "Wild Spawner Abundance",
        status_year = "Assessment Year"
      ) %>%
      fmt_number(columns = c(CUnmat), decimals = 0)
  })

  output$demographics_indicators_table <- render_gt({
    tbl_indicators %>%
      filter(type == "dem") %>%
      select(name, abbrev) %>%
      gt() %>%
      tab_header(title = "Demographic Indicators") %>%
      cols_label(
        name = "Indicator Name",
        abbrev = "Abbreviation"
      )
  })

  output$demographics_plot <- renderPlot({
    data <- cu_data()
    plot_cu_lolli(data$ind_cu,
      indicators_choose = c("CUstatus", "CUnmat"))
  })


  # Timing Tab Outputs ------------------------------------------------------

  output$timing_plot <- renderPlot({
    data <- cu_data()
    cu_timing_plot(data$cu_timing_long_i)
  })

  output$timing_summary <- renderUI({
    data <- cu_data()
    timing <- data$cu_timing_i

    tagList(
      h4("Timing Details"),
      tags$table(style = "width: 100%;",
        tags$tr(
          tags$th("Life Stage"),
          tags$th("Start (DOY)"),
          tags$th("Peak (DOY)"),
          tags$th("End (DOY)"),
          tags$th("Data Quality")
        ),
        tags$tr(
          tags$td("Spawning"),
          tags$td(timing$sp_start),
          tags$td(timing$sp_peak),
          tags$td(timing$sp_end),
          tags$td(timing$sp_dat_qual)
        ),
        tags$tr(
          tags$td("Run Timing"),
          tags$td(timing$rt_start),
          tags$td(timing$rt_peak),
          tags$td(timing$rt_end),
          tags$td(timing$rt_dat_qual)
        ),
        tags$tr(
          tags$td("Ocean Entry"),
          tags$td(timing$oe_start),
          tags$td(timing$oe_peak),
          tags$td(timing$oe_end),
          tags$td(timing$oe_dat_qual)
        )
      ),
      p(style = "margin-top: 10px;", tags$i("DOY = Day of Year"))
    )
  })

  # Spawning & Rearing Tab Outputs ------------------------------------------

  output$fwr_indicators_table <- render_gt({
    tbl_indicators %>%
      filter(type == "fwR") %>%
      select(name, abbrev) %>%
      gt() %>%
      tab_header(title = "Freshwater Rearing Indicators") %>%
      cols_label(
        name = "Indicator Name",
        abbrev = "Abbreviation"
      )
  })

  output$fwr_indicators_plot <- renderPlot({
    data <- cu_data()
    plot_cu_lolli(data$ind_cu,
      indicators_choose = c("highQpdelta", "lowQpdelta",
        "tw8rate", "tw8proj",
        "ct", "favchange", "fwres"))
  })

  output$accessibility_plot <- renderPlot({
    data <- cu_data()
    stream_accessible_plot(data$fw_sp_cu,
      data$nuseds_cu,
      data$cu_boundary_i,
      data$lakes_cu)
  })

  output$stream_stats_table <- render_gt({
    data <- cu_data()
    data$fwR_cu$streams %>%
      select(c(total_length_acc, n_streams, proportion_rear, proportion_spawn, proportion_rs,
        avg_elevation, avg_lat, avg_lon)) %>%
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
      tab_header(title = "Stream Statistics") %>%
      cols_label(
        total_length_acc = "Length (km)",
        n_streams = "# Segments",
        proportion_rear = "Rearing",
        proportion_spawn = "Spawning",
        proportion_rs = "Rearing/Spawning",
        avg_elevation = "Elevation (m)",
        avg_lat = "Latitude",
        avg_lon = "Longitude"
      ) %>%
      tab_spanner(
        label = "Habitat Model Proportions",
        columns = c(proportion_rear, proportion_spawn, proportion_rs)
      )
  })

  output$nuseds_stats_table <- render_gt({
    data <- cu_data()
    data$fwR_cu$streams %>%
      select(contains("nuseds")) %>%
      gt() %>%
      tab_header(title = "NuSEDS Statistics")
  })

  output$regime_plot <- renderPlot({
    data <- cu_data()
    if (!is.null(data$watershed_flow_cu) && !is.null(data$stations_cu)) {
      cu_hydrologic_regime(data$cu_boundary_i,
        data$watershed_flow_cu,
        data$stations_cu,
        data$fw_sp_cu)
    } else {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5,
          label = "No hydrologic data available for this CU",
          size = 6) +
        theme_void()
    }
  })

  output$hydro_coverage_table <- render_gt({
    data <- cu_data()
    data$fwR_cu$streams %>%
      select(contains("prop_")) %>%
      mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
      gt() %>%
      tab_header(title = "Hydrologic Coverage") %>%
      cols_label(
        prop_snow = "Snow",
        prop_rain = "Rain",
        prop_hybrid = "Hybrid",
        prop_glacial = "Glacial",
        prop_coverage = "Total"
      ) %>%
      fmt_percent(columns = where(is.numeric), decimals = 1)
  })

  output$q8_plot <- renderPlot({
    data <- cu_data()
    stream_indicator_plot(data$acc_sp_cu,
      data$cu_boundary_i,
      data$lakes_cu,
      variable = "qpdelta_flow_8_45_3",
      plot_title = "Change in August flow - 2041-2060",
      unit_label = "Proportion change",
      xlim = c(-1, 0),
      scico_palette = "lapaz",
      palette_direction = -1)
  })

  output$qndj_plot <- renderPlot({
    data <- cu_data()
    stream_indicator_plot(data$acc_sp_cu,
      data$cu_boundary_i,
      data$lakes_cu,
      variable = "qpdelta_flow_18_45_3",
      plot_title = "Change in Nov-Jan flow - 2041-2060",
      unit_label = "Proportion change",
      scico_palette = "devon",
      xlim = c(0, 1),
      palette_direction = -1)
  })

  output$temp_plot <- renderPlot({
    data <- cu_data()
    stream_indicator_plot(data$acc_sp_cu,
      data$cu_boundary_i,
      data$lakes_cu,
      Tw_stations,
      variable = "tw8_9_45_3",
      temp_stations = TRUE,
      plot_title = "August Mean Temperature - 2041-2060",
      unit_label = "Degrees C",
      scico_palette = "roma",
      palette_direction = -1)
  })

  output$temp_rate_plot <- renderPlot({
    data <- cu_data()
    stream_indicator_plot(data$acc_sp_cu,
      data$cu_boundary_i,
      data$lakes_cu,
      Tw_stations,
      variable = "delta_tw8_9_45_3",
      temp_stations = TRUE,
      plot_title = "Rate of Change in August Mean Temperature",
      unit_label = "Degrees C per decade",
      scico_palette = "roma",
      palette_direction = -1)
  })

  output$enm_plot <- renderPlot({
    data <- cu_data()
    stream_indicator_plot(data$fw_sp_cu,
      data$cu_boundary_i,
      data$lakes_cu,
      variable = "fav_45",
      plot_title = "ENM favourability, 2041-2060",
      histogram_fill = "model_access_salmon",
      scico_palette = "davos",
      xlim = c(0, 1),
      palette_direction = -1)
  })

  output$enm_diff_plot <- renderPlot({
    data <- cu_data()
    stream_indicator_plot(data$fw_sp_cu,
      data$cu_boundary_i,
      data$lakes_cu,
      variable = "favchange_45",
      plot_title = "ENM favourability change, 1981-2000 to 2041-2060",
      histogram_fill = "model_access_salmon",
      scico_palette = "berlin",
      xlim = c(-1, 1),
      palette_direction = -1)
  })

  output$ct_plot <- renderPlot({
    data <- cu_data()
    stream_indicator_plot(data$acc_sp_cu,
      data$cu_boundary_i,
      data$lakes_cu,
      variable = "ct_anad",
      plot_title = "Cumulative stressors",
      unit_label = "Cumulative Threat Score",
      scico_palette = "lajolla",
      palette_direction = -1)
  })


  # Migration Tab Outputs ---------------------------------------------------

  output$migr_indicators_table <- render_gt({
    tbl_indicators %>%
      filter(type == "migr") %>%
      select(name, abbrev) %>%
      gt() %>%
      tab_header(title = "Upstream Migration Indicators") %>%
      cols_label(
        name = "Indicator Name",
        abbrev = "Abbreviation"
      )
  })

  output$migr_indicators_plot <- renderPlot({
    data <- cu_data()
    plot_cu_lolli(data$ind_cu,
      indicators_choose = c("migrT", "migrQ", "migrA21", "migrdist"))
  })

  output$migration_path_plot <- renderPlot({
    data <- cu_data()
    migration_path_plot(data$migr_cu,
      data$nuseds_cu,
      data$cu_boundary_i)
  })

  output$migration_timing_plot <- renderPlot({
    data <- cu_data()
    migr_timing_plot(migrT_rcps,
      data$cu_i,
      data$cu_timing_i,
      rcp = "45",
      period_choose = c("1981-2010", "2041-2060"))
  })


  # Marine Tab Outputs ------------------------------------------------------

  output$marine_indicators_table <- render_gt({
    tbl_indicators %>%
      filter(type == "mar") %>%
      select(name, abbrev) %>%
      gt() %>%
      tab_header(title = "Nearshore Marine Indicators") %>%
      cols_label(
        name = "Indicator Name",
        abbrev = "Abbreviation"
      )
  })

  output$marine_indicators_plot <- renderPlot({
    data <- cu_data()
    plot_cu_lolli(data$ind_cu,
      indicators_choose = c("SSTproj", "CI"))
  })

  output$sst_plot <- renderPlot({
    data <- cu_data()

    if (nrow(data$cu_mar) > 0) {
      start_month <- month.name[unique(data$cu_mar$ns_timing_start)]
      end_month <- month.name[unique(data$cu_mar$ns_timing_end)]
      MAZ_pick <- unique(data$cu_mar$MAZ)

      SST_cu_sp <- subset_and_mean_sst(
        sf_data = filter(SST_grid, MAZ_Acrony == MAZ_pick),
        timing_df = data$cu_mar,
        RCP_pick = "45"
      )

      marine_indicator_plot(
        SST_cu_sp,
        MAZ = filter(MAZ, MAZ_Acrony == MAZ_pick),
        plot_title = paste0("Projected SST, RCP 4.5, ", start_month, "-", end_month)
      )
    } else {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5,
          label = "No marine data available for this CU",
          size = 6) +
        theme_void()
    }
  })

  output$sst_interpretation <- renderUI({
    data <- cu_data()

    if (nrow(data$cu_mar) > 0) {
      start_month <- month.name[unique(data$cu_mar$ns_timing_start)]
      end_month <- month.name[unique(data$cu_mar$ns_timing_end)]

      tagList(
        p(tags$b("Ocean Entry Window:"), paste(start_month, "to", end_month)),
        p("Sea surface temperature during this period affects juvenile salmon survival,
          growth rates, and prey availability. Warmer temperatures may lead to:"),
        tags$ul(
          tags$li("Reduced prey quality and availability"),
          tags$li("Increased metabolic demands"),
          tags$li("Higher disease susceptibility"),
          tags$li("Altered predator-prey dynamics")
        )
      )
    } else {
      p("No marine timing data available for interpretation.")
    }
  })


}

################################################################################
# Run the app
################################################################################

shinyApp(ui = ui, server = server)
