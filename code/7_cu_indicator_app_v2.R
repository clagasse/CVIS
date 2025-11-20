library(shiny)
library(shinythemes)
library(sf)
library(dplyr)
library(stringr)
library(gt)
library(ggplot2)

# UI Definition
ui <- fluidPage(
  theme = shinytheme("flatly"),

  titlePanel("Climate Vulnerability Indicator Report"),

  sidebarLayout(
    sidebarPanel(
      width = 3,

      # CU Selection
      selectInput(
        "cu_select",
        "Conservation Unit (CU):",
        choices = NULL,  # Will be populated from data
        selected = NULL
      ),

      # RCP Scenario Selection
      selectInput(
        "rcp_select",
        "RCP Scenario:",
        choices = c("RCP 4.5" = "45", "RCP 8.5" = "85"),
        selected = "45"
      ),

      # Time Period Selection
      selectInput(
        "period_select",
        "Time Period:",
        choices = c("1981-2010" = "1",
          "2011-2040" = "2",
          "2041-2060" = "3",
          "2061-2080" = "4",
          "2081-2100" = "5"),
        selected = "3"
      ),

      hr(),

      # Additional info
      h5("About"),
      p("This dashboard displays climate vulnerability indicators for Fraser River salmon Conservation Units (CUs)."),
      p("Select a CU, RCP scenario, and time period to view the indicators.")
    ),

    mainPanel(
      width = 9,

      # Tabs for different sections
      tabsetPanel(
        id = "main_tabs",
        type = "tabs",

        # Overview Tab
        tabPanel(
          "Overview",
          br(),
          h3(textOutput("cu_title")),
          hr(),
          plotOutput("boundary_plot", height = "400px"),
          hr(),
          h4("Demographic Overview"),
          gt_output("demographics_table"),
          br(),
          plotOutput("demographics_plot", height = "300px")
        ),

        # Life Stage Timing Tab
        tabPanel(
          "Life Stage Timing",
          br(),
          h4("Life Stage Timing"),
          p("Timing by life history stage is taken from Wilson and Peacock (2025)."),
          p("Data quality is rated on a scale of 1 to 6 based on number of observations and their recentness, with 1 being the highest quality data."),
          plotOutput("timing_plot", height = "400px")
        ),

        # Spawning and Rearing Tab
        tabPanel(
          "Spawning & Rearing",
          br(),
          h4("Spawning and Rearing Indicators"),
          gt_output("fwr_indicators_table"),
          br(),
          plotOutput("fwr_indicators_plot", height = "400px"),
          hr(),
          h5("Stream Accessibility"),
          plotOutput("accessibility_plot", height = "500px"),
          br(),
          gt_output("stream_stats_table")
        ),

        # Hydrologic Regime Tab
        tabPanel(
          "Hydrology",
          br(),
          h4("Hydrologic Regime"),
          gt_output("hydro_regime_table"),
          br(),
          plotOutput("regime_plot", height = "500px"),
          br(),
          h5("Temperature Projections"),
          plotOutput("temperature_plot", height = "400px")
        ),

        # ENM Tab
        tabPanel(
          "Ecological Niche",
          br(),
          h4("Ecological Niche Model (ENM)"),
          plotOutput("enm_plot", height = "500px"),
          hr(),
          h5("ENM Favourability Change"),
          plotOutput("enm_diff_plot", height = "500px")
        ),

        # Cumulative Threats Tab
        tabPanel(
          "Cumulative Threats",
          br(),
          h4("Cumulative Threats"),
          plotOutput("threats_plot", height = "500px")
        ),

        # Migration Tab
        tabPanel(
          "Upstream Migration",
          br(),
          h4("Upstream Migration Indicators"),
          gt_output("migration_indicators_table"),
          br(),
          plotOutput("migration_indicators_plot", height = "400px"),
          hr(),
          h5("Migration Path"),
          plotOutput("migration_path_plot", height = "500px"),
          hr(),
          h5("Migration Timing and Temperature"),
          plotOutput("migration_timing_plot", height = "400px")
        ),

        # Marine Tab
        tabPanel(
          "Nearshore Marine",
          br(),
          h4("Nearshore Marine Indicators"),
          gt_output("marine_indicators_table"),
          br(),
          plotOutput("marine_indicators_plot", height = "400px"),
          hr(),
          h5("Sea Surface Temperature (SST)"),
          plotOutput("sst_plot", height = "500px")
        )
      )
    )
  )
)

# Server Logic
server <- function(input, output, session) {
  # Reactive values for storing data
  cu_data <- reactiveVal(NULL)

  # Initialize - Load data when app starts
  # This assumes the data objects are already loaded in the global environment
  # from running the main analysis scripts
  observe({
    # Populate CU choices from cu_run data
    if (exists("cu_run") && is.data.frame(cu_run)) {
      cu_choices <- cu_run$FULL_CU_IN
      updateSelectInput(session, "cu_select", choices = cu_choices, selected = cu_choices[1])
    }
  })

  # Reactive expression for current CU
  current_cu <- reactive({
    req(input$cu_select)
    input$cu_select
  })

  # Reactive expression for current RCP
  current_rcp <- reactive({
    req(input$rcp_select)
    input$rcp_select
  })

  # Reactive expression for current period
  current_period <- reactive({
    req(input$period_select)
    input$period_select
  })

  # Get CU-specific data
  cu_subset <- reactive({
    req(current_cu())
    cu_i <- current_cu()

    # Subset data for the selected CU
    cu_run_i <- cu_run[cu_run$FULL_CU_IN == cu_i, ]

    # Check if we got a valid row
    if (nrow(cu_run_i) == 0) {
      showNotification("CU not found in cu_run data", type = "error")
      return(NULL)
    }

    # Get species - handle as scalar
    sp_pick <- as.character(cu_run_i$SPECIES_NAME[1])

    # Check if species lookup works
    sp_match <- spp_lookup$spp_abr_bcfp[spp_lookup$SPECIES_NAME == sp_pick][1]
    if (length(sp_match) == 0) {
      showNotification(paste("Species not found in spp_lookup:", sp_pick), type = "error")
      return(NULL)
    }
    sp_pick_bcfp <- as.character(sp_match[1])

    # Get ENM species name
    sp_pick_ENM <- str_to_lower(as.character(sp_pick[1]))

    # Get spatial subsets
    fwR_cu <- fwR_all[[cu_i]]
    stream_cu_sub <- stream_cu_picks[, colnames(stream_cu_picks) == cu_i]
    cu_boundary_i <- cu_boundary[cu_boundary$FULL_CU_IN == cu_i, ]
    nuseds_cu <- nuseds_Fr[nuseds_Fr$FULL_CU_IN == cu_i, ]

    # Get indicator data with current RCP and period
    ind_cu <- get_CU_indicators(
      all_flat_std,
      cu_i,
      indicators_choose = tbl_indicators$abbrev,
      RCP_pick = current_rcp(),
      period_pick = current_period()
    )

    # Return list of subsetted data
    list(
      cu_run_i = cu_run_i,
      sp_pick = sp_pick,
      sp_pick_bcfp = sp_pick_bcfp,
      sp_pick_ENM = sp_pick_ENM,
      fwR_cu = fwR_cu,
      stream_cu_sub = stream_cu_sub,
      cu_boundary_i = cu_boundary_i,
      nuseds_cu = nuseds_cu,
      ind_cu = ind_cu
    )
  })

  # Output: CU Title
  output$cu_title <- renderText({
    req(cu_subset())
    cu_subset()$cu_run_i$CU_NAME
  })

  # Output: Boundary Plot
  output$boundary_plot <- renderPlot({
    req(cu_subset())
    cu_boundary_highlight(cu_boundary, cu_pick = current_cu())
  })

  # Output: Demographics Table
  output$demographics_table <- render_gt({
    req(cu_subset())
    cu_subset()$cu_run_i %>%
      select(FULL_CU_IN, CU_NAME, gen_length, WSP_population_status, Most_Recent_Generational_Average) %>%
      gt() %>%
      fmt_number(
        columns = c(gen_length, Most_Recent_Generational_Average),
        decimals = 2
      ) %>%
      cols_label(
        gen_length = "Generation Length",
        WSP_population_status = "WSP Status",
        Most_Recent_Generational_Average = "Recent Gen Average"
      )
  })

  # Output: Demographics Plot
  output$demographics_plot <- renderPlot({
    req(cu_subset())
    plot_cu_lolli(
      cu_subset()$ind_cu,
      indicators_choose = c("CUstatus", "CUnmat")
    )
  })

  # Output: Timing Plot
  output$timing_plot <- renderPlot({
    req(cu_subset())
    cu_i <- current_cu()
    cu_timing_long_i <- cu_timing_long[cu_timing_long$FULL_CU_IN == cu_i, ]
    cu_timing_plot(cu_timing_long_i)
  })

  # Output: FWR Indicators Table
  output$fwr_indicators_table <- render_gt({
    tbl_indicators %>%
      filter(type == "fwR") %>%
      gt()
  })

  # Output: FWR Indicators Plot
  output$fwr_indicators_plot <- renderPlot({
    req(cu_subset())
    plot_cu_lolli(
      cu_subset()$ind_cu,
      indicators_choose = c("highQpdelta", "lowQpdelta", "tw8rate",
        "tw8proj", "ct", "favchange", "fwres")
    )
  })

  # Output: Accessibility Plot
  output$accessibility_plot <- renderPlot({
    req(cu_subset())
    cu_i <- current_cu()
    cu_data <- cu_subset()

    # Get stream data with species-specific columns
    fw_sp_cu <- fw_sp_ind[cu_data$stream_cu_sub, ] %>%
      rename(keep_habitat = contains(paste0("model_habitat_", cu_data$sp_pick_bcfp)),
        favchange_45 = contains(paste0("favchange_", cu_data$sp_pick_ENM, "_45_3")),
        favchange_85 = contains(paste0("favchange_", cu_data$sp_pick_ENM, "_85_3")),
        fav_45 = contains(paste0("fav_", cu_data$sp_pick_ENM, "_45_3")),
        fav_85 = contains(paste0("fav_", cu_data$sp_pick_ENM, "_85_3"))) %>%
      mutate(model_rs = if_any(starts_with("keep_habitat"), ~ . == TRUE)) %>%
      mutate(model_rs = if_else(is.na(model_rs), FALSE, model_rs)) %>%
      mutate(model_rs = factor(model_rs, levels = c(TRUE, FALSE),
        labels = c("1-SPAWNING/REARING", "2-NOT SPAWNING/REARING")))

    # Get lakes
    temp <- unlist(st_intersects(cu_data$cu_boundary_i, lakes_Fr))
    lakes_cu <- lakes_Fr[temp, ]

    stream_accessible_plot(fw_sp_cu, cu_data$nuseds_cu, cu_data$cu_boundary_i, lakes_cu)
  })

  # Output: Stream Stats Table
  output$stream_stats_table <- render_gt({
    req(cu_subset())
    cu_subset()$fwR_cu$streams %>%
      select(c(total_length_acc, n_streams, proportion_rear, proportion_spawn,
        proportion_rs, avg_elevation, avg_lat, avg_lon)) %>%
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
      tab_header(title = md("**CU Boundary Stream Statistics**")) %>%
      cols_label(
        total_length_acc = "Accessible Stream Length (km)",
        n_streams = "Number of Stream Segments",
        proportion_rear = "Proportion Rearing",
        proportion_spawn = "Proportion Spawning",
        proportion_rs = "Proportion Rearing/Spawning",
        avg_elevation = "Avg. Elevation (m)",
        avg_lat = "Avg. Latitude",
        avg_lon = "Avg. Longitude"
      )
  })

  # Output: Hydro Regime Table
  output$hydro_regime_table <- render_gt({
    req(cu_subset())
    cu_subset()$fwR_cu$streams %>%
      select(contains("prop_")) %>%
      mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
      gt() %>%
      tab_header(title = md("**Hydrologic Regime Coverage Proportions**")) %>%
      cols_label(
        prop_snow = "Snow-Dominated",
        prop_rain = "Rain-Dominated",
        prop_hybrid = "Hybrid",
        prop_glacial = "Glacial",
        prop_coverage = "Total Coverage"
      )
  })

  # Output: Regime Plot
  output$regime_plot <- renderPlot({
    req(cu_subset())
    cu_i <- current_cu()
    cu_data <- cu_subset()

    # Get watershed flow and stations
    temp <- unlist(st_intersects(cu_data$cu_boundary_i, watershed_flow))
    watershed_flow_cu <- watershed_flow[temp, ]

    stations_flow <- st_read(file.path(paths$climate, "Ruzzante_low_flows", "stations.gpkg"),
      quiet = TRUE) %>%
      st_transform(3005)
    temp <- unlist(st_intersects(cu_data$cu_boundary_i, stations_flow))
    stations_cu <- stations_flow[temp, ]

    # Get stream data
    fw_sp_cu <- fw_sp_ind[cu_data$stream_cu_sub, ]

    cu_hydrologic_regime(cu_data$cu_boundary_i, watershed_flow_cu, stations_cu, fw_sp_cu)
  })

  # Output: Temperature Plot
  output$temperature_plot <- renderPlot({
    req(cu_subset())
    cu_i <- current_cu()
    cu_data <- cu_subset()

    # Get stream data with species-specific columns
    fw_sp_cu <- fw_sp_ind[cu_data$stream_cu_sub, ]

    # Get lakes
    temp <- unlist(st_intersects(cu_data$cu_boundary_i, lakes_Fr))
    lakes_cu <- lakes_Fr[temp, ]

    # Select temperature variable based on RCP
    temp_var <- paste0("Tw8_9_", current_rcp(), "_", current_period())

    stream_indicator_plot(
      fw_sp_cu,
      cu_data$cu_boundary_i,
      lakes_cu,
      variable = temp_var,
      plot_title = paste0("August Temperature, Period ", current_period()),
      scico_palette = "roma",
      palette_direction = -1
    )
  })

  # Output: ENM Plot
  output$enm_plot <- renderPlot({
    req(cu_subset())
    cu_i <- current_cu()
    cu_data <- cu_subset()

    # Get stream data with species-specific columns - rename without period since they cover all periods
    fw_sp_cu <- fw_sp_ind[cu_data$stream_cu_sub, ] %>%
      rename(
        fav_45 = contains(paste0("fav_", cu_data$sp_pick_ENM, "_45")),
        fav_85 = contains(paste0("fav_", cu_data$sp_pick_ENM, "_85"))
      )

    # Get lakes
    temp <- unlist(st_intersects(cu_data$cu_boundary_i, lakes_Fr))
    lakes_cu <- lakes_Fr[temp, ]

    # Select ENM variable based on RCP - use the simple name since it was renamed
    enm_var <- paste0("fav_", current_rcp())

    stream_indicator_plot(
      fw_sp_cu,
      cu_data$cu_boundary_i,
      lakes_cu,
      variable = enm_var,
      plot_title = paste0("ENM Favourability, Period ", current_period(), ", RCP ", current_rcp()),
      histogram_fill = "model_access_salmon",
      scico_palette = "davos",
      xlim = c(0, 1),
      palette_direction = -1
    )
  })

  # Output: ENM Diff Plot
  output$enm_diff_plot <- renderPlot({
    req(cu_subset())
    cu_i <- current_cu()
    cu_data <- cu_subset()

    # Get stream data with species-specific columns - rename without period
    fw_sp_cu <- fw_sp_ind[cu_data$stream_cu_sub, ] %>%
      rename(
        favchange_45 = contains(paste0("favchange_", cu_data$sp_pick_ENM, "_45")),
        favchange_85 = contains(paste0("favchange_", cu_data$sp_pick_ENM, "_85"))
      )

    # Get lakes
    temp <- unlist(st_intersects(cu_data$cu_boundary_i, lakes_Fr))
    lakes_cu <- lakes_Fr[temp, ]

    # Select ENM change variable based on RCP
    enm_change_var <- paste0("favchange_", current_rcp())

    stream_indicator_plot(
      fw_sp_cu,
      cu_data$cu_boundary_i,
      lakes_cu,
      variable = enm_change_var,
      plot_title = paste0("ENM Favourability Change, Baseline to Period ", current_period(), ", RCP ", current_rcp()),
      histogram_fill = "model_access_salmon",
      scico_palette = "berlin",
      xlim = c(-1, 1),
      palette_direction = -1
    )
  })

  # Output: Threats Plot
  output$threats_plot <- renderPlot({
    req(cu_subset())
    cu_i <- current_cu()
    cu_data <- cu_subset()

    # Get stream data - accessible only
    fw_sp_cu <- fw_sp_ind[cu_data$stream_cu_sub, ] %>%
      filter(model_access_salmon %in% c("OBSERVED", "INFERRED"))

    # Get lakes
    temp <- unlist(st_intersects(cu_data$cu_boundary_i, lakes_Fr))
    lakes_cu <- lakes_Fr[temp, ]

    stream_indicator_plot(
      fw_sp_cu,
      cu_data$cu_boundary_i,
      lakes_cu,
      variable = "ct_anad",
      plot_title = "Cumulative Stressors",
      unit_label = "Cumulative Threat Score",
      scico_palette = "lajolla",
      palette_direction = -1
    )
  })

  # Output: Migration Indicators Table
  output$migration_indicators_table <- render_gt({
    tbl_indicators %>%
      filter(type == "migr") %>%
      gt()
  })

  # Output: Migration Indicators Plot
  output$migration_indicators_plot <- renderPlot({
    req(cu_subset())
    plot_cu_lolli(
      cu_subset()$ind_cu,
      indicators_choose = c("migrT", "migrQ", "migrA21", "migrdist")
    )
  })

  # Output: Migration Path Plot
  output$migration_path_plot <- renderPlot({
    req(cu_subset())
    cu_i <- current_cu()
    cu_data <- cu_subset()

    migr_cu <- migr_list[[cu_i]]

    migration_path_plot(migr_cu, cu_data$nuseds_cu, cu_data$cu_boundary_i)
  })

  # Output: Migration Timing Plot
  output$migration_timing_plot <- renderPlot({
    req(cu_subset())
    cu_i <- current_cu()
    cu_timing_i <- cu_timing_Fr[cu_timing_Fr$FULL_CU_IN == cu_i, ]

    # Determine periods to show based on selection
    period_map <- c("1" = "1981-2010", "2" = "2011-2040", "3" = "2041-2060",
      "4" = "2061-2080", "5" = "2081-2100")
    periods <- c("1981-2010", period_map[current_period()])

    migr_timing_plot(
      migrT_rcps,
      cu_i,
      cu_timing_i,
      rcp = current_rcp(),
      period_choose = periods
    )
  })

  # Output: Marine Indicators Table
  output$marine_indicators_table <- render_gt({
    tbl_indicators %>%
      filter(type == "mar") %>%
      gt()
  })

  # Output: Marine Indicators Plot
  output$marine_indicators_plot <- renderPlot({
    req(cu_subset())
    plot_cu_lolli(
      cu_subset()$ind_cu,
      indicators_choose = c("SSTproj", "CI")
    )
  })

  # Output: SST Plot
  output$sst_plot <- renderPlot({
    req(cu_subset())
    cu_i <- current_cu()

    # Get marine data
    cu_mar <- mar_all_flat[mar_all_flat$FULL_CU_IN == cu_i, ]

    if (nrow(cu_mar) > 0) {
      start_month <- month.name[unique(cu_mar$ns_timing_start)]
      end_month <- month.name[unique(cu_mar$ns_timing_end)]
      MAZ_pick <- unique(cu_mar$MAZ)

      # Subset SST data
      SST_cu_sp <- subset_and_mean_sst(
        sf_data = filter(SST_grid, MAZ_Acrony == MAZ_pick),
        timing_df = cu_mar,
        RCP_pick = current_rcp()
      )

      marine_indicator_plot(
        SST_cu_sp,
        MAZ = filter(MAZ, MAZ_Acrony == MAZ_pick),
        plot_title = paste0("Projected SST, RCP ", current_rcp(), ", ",
          start_month, "-", end_month)
      )
    } else {
      # If no marine data, show a message
      plot.new()
      text(0.5, 0.5, "No marine data available for this CU", cex = 1.5)
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
