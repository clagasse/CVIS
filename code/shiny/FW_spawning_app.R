
library(shiny)
library(ggplot2)
library(viridis)
library(gt)
library(sf)
library(dplyr)

ui <- fluidPage(
  titlePanel("Freshwater Spawning Report"),
  sidebarLayout(
    sidebarPanel(
      selectInput("cu_input", "Select Conservation Unit (CU):", choices = unique(cu_run$FULL_CU_IN))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Maps", 
                 plotOutput("cu_acc_p"),
                 plotOutput("ct_anad_p"),
                 plotOutput("ts_hist_p"),
                 plotOutput("ts_proj_p"),
                 plotOutput("ENM_proj_p"),
                 plotOutput("ENM_diff_p")
        ),
        tabPanel("Histograms",
                 plotOutput("ct_anad_h"),
                 plotOutput("ts_hist_h"),
                 plotOutput("ts_proj_h"),
                 plotOutput("ENM_proj_h"),
                 plotOutput("ENM_diff_h")
        ),
        tabPanel("Tables",
                 gt_output("stream_stats"),
                 gt_output("cumulative_threats"),
                 gt_output("stream_temp"),
                 gt_output("aug_flows_pcic"),
                 gt_output("enm_favourability"),
                 gt_output("aug_flows_stat")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  cu_data <- reactive({
    cu_i <- input$cu_input
    list(
      cu_run_i = cu_run[cu_run$FULL_CU_IN == cu_i,],
      sp_pick = cu_run$spp[cu_run$FULL_CU_IN == cu_i],
      cu_boundary_i = cu_boundary[cu_boundary$FULL_CU_IN == cu_i,],
      nuseds_cu = nuseds_Fr[nuseds_Fr$FULL_CU_IN == cu_i,],
      fwR_cu = fwR_all[[cu_i]],
      # Add other filtered datasets as needed
    )
  })

  output$cu_acc_p <- renderPlot({
    ggplot() +
      geom_sf(data = st_zm(cu_data()$bcfpa_cu), aes(color = model_rs)) +
      geom_sf(data = cu_data()$cu_boundary_i, color = "black", alpha = 0.3) +
      geom_sf(data = cu_data()$nuseds_cu, aes(color = SPECIES), alpha = 0.6) +
      coord_sf(xlim = st_bbox(cu_data()$cu_boundary_i)[c(1,3)],
               ylim = st_bbox(cu_data()$cu_boundary_i)[c(2,4)]) +
      labs(subtitle = paste(cu_data()$cu_run_i$CU_NAME), colour = "BC FishPass")
  })

  output$stream_stats <- render_gt({
    cu_data()$fwR_cu$streams %>%
      gt() %>%
      tab_header(title = "CU Boundary Stream Statistics")
  })

  output$cumulative_threats <- render_gt({
    cu_data()$fwR_cu$CT %>%
      gt() %>%
      tab_header(title = "Cumulative Threats")
  })

  output$stream_temp <- render_gt({
    cu_data()$fwR_cu$fwT %>%
      gt() %>%
      tab_header(title = "Stream Temp")
  })

  output$aug_flows_pcic <- render_gt({
    cu_data()$fwR_cu$fwQlow %>%
      gt() %>%
      tab_header(title = "August flows - PCIC stream model")
  })

  output$enm_favourability <- render_gt({
    cu_data()$fwR_cu$ENM %>%
      gt() %>%
      tab_header(title = "ENM favourability")
  })

  output$aug_flows_stat <- render_gt({
    cu_data()$fwR_cu$wpQlow %>%
      gt() %>%
      tab_header(title = "August flows - statistical gauge model")
  })

  output$ct_anad_p <- renderPlot({
    ggplot() +
      geom_sf(data = cu_data()$fwct_cu, aes(color = CT_anad), linewidth = 1.) +
      scale_color_viridis(limits = c(0,5.5)) +
      geom_sf(data = cu_data()$cu_boundary_i, color = "black", alpha = 0.3) +
      coord_sf(xlim = st_bbox(cu_data()$cu_boundary_i)[c(1,3)],
               ylim = st_bbox(cu_data()$cu_boundary_i)[c(2,4)]) +
      labs(subtitle = paste("Cumulative stressor score (anadromous)"), color = "Threat Score")
  })

  output$ct_anad_h <- renderPlot({
    ggplot(cu_data()$fwct_cu) +
      geom_histogram(aes(x = CT_anad, fill = model_rs)) +
      geom_vline(aes(xintercept = mean(CT_anad, na.rm = T)), color = "red", linetype = "dashed") +
      labs(fill = "Modelled rearing/spawning")
  })

  output$ts_hist_p <- renderPlot({
    ggplot() +
      geom_sf(data = cu_data()$fwT_cu, aes(color = Tw8_0_00_0), linewidth = 1.) +
      scale_color_gradientn(colours = wesanderson::wes_palette("Zissou1", 100, type = "continuous"),
                            limits = c(5,25)) +
      geom_sf(data = cu_data()$cu_boundary_i, color = "black", alpha = 0.3) +
      geom_sf(data = Tw_stations, color = "forestgreen", size = 0.5) +
      coord_sf(xlim = st_bbox(cu_data()$cu_boundary_i)[c(1,3)],
               ylim = st_bbox(cu_data()$cu_boundary_i)[c(2,4)]) +
      labs(subtitle = paste("August T, Thermalscapes, 1981-2000"), color = "T (deg C)")
  })

  output$ts_hist_h <- renderPlot({
    ggplot(cu_data()$fwT_cu) +
      geom_histogram(aes(x = Tw8_0_00_0, fill = model_rs)) +
      geom_vline(aes(xintercept = mean(Tw8_0_00_0, na.rm = T)), color = "red", linetype = "dashed") +
      labs(fill = "Modelled rearing/spawning")
  })

  output$ts_proj_p <- renderPlot({
    ggplot() +
      geom_sf(data = cu_data()$fwT_cu, aes(color = Tw8_9_45_3), linewidth = 1.) +
      scale_color_gradientn(colours = wesanderson::wes_palette("Zissou1", 100, type = "continuous"),
                            limits = c(5,25)) +
      geom_sf(data = cu_data()$cu_boundary_i, color = "black", alpha = 0.3) +
      geom_sf(data = Tw_stations, color = "forestgreen", size = 0.5) +
      coord_sf(xlim = st_bbox(cu_data()$cu_boundary_i)[c(1,3)],
               ylim = st_bbox(cu_data()$cu_boundary_i)[c(2,4)]) +
      labs(subtitle = paste("August T, Thermalscapes, 2041-2060"), color = "T (deg C)")
  })

  output$ts_proj_h <- renderPlot({
    ggplot(cu_data()$fwT_cu) +
      geom_histogram(aes(x = Tw8_9_45_3)) +
      geom_vline(aes(xintercept = mean(Tw8_9_45_3, na.rm = T)), color = "red", linetype = "dashed") +
      geom_text(aes(x = mean(Tw8_9_45_3, na.rm = T), y = 20, label = "mean"), color = "red")
  })

  output$ENM_proj_p <- renderPlot({
    plot_col <- names(select(cu_data()$ENM_cu, contains("Fav_f.9_45_3")))[1]
    ggplot() +
      geom_sf(data = cu_data()$ENM_cu, aes(color = .data[[plot_col]])) +
      geom_sf(data = cu_data()$cu_boundary_i, color = "black", alpha = 0.2) +
      scale_color_viridis()
  })

  output$ENM_proj_h <- renderPlot({
    plot_col <- names(select(cu_data()$ENM_cu, contains("Fav_f.9_45_3")))[1]
    ggplot() +
      geom_histogram(data = cu_data()$ENM_cu, aes(x = .data[[plot_col]]))
  })

  output$ENM_diff_p <- renderPlot({
    plot_col <- names(select(cu_data()$ENM_cu, contains("Fav_change_9_45_5")))[1]
    ggplot() +
      geom_sf(data = cu_data()$ENM_cu, aes(color = .data[[plot_col]])) +
      geom_sf(data = cu_data()$cu_boundary_i, color = "black", alpha = 0.2) +
      scale_color_viridis()
  })

  output$ENM_diff_h <- renderPlot({
    plot_col <- names(select(cu_data()$ENM_cu, contains("Fav_change_9_45_5")))[1]
    ggplot() +
      geom_histogram(data = cu_data()$ENM_cu, aes(x = .data[[plot_col]]))
  })
}

shinyApp(ui, server)
