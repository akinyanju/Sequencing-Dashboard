# -- Render the plot dynamically --
output$species_plot <- renderPlotly({
  df <- species_plot_data()
  req(nrow(df) > 0)
  
  plot_width <- max(1000, length(unique(df$GT_QC_Sample_ID)) * 30)
  # Construct rich hover text with all details
  df$hover_text <- paste0(
    "Lab: ", df$Investigator_Folder, "<br>",
    "Project Run: ", df$Project_run_type, "<br>",
    "Sample: ", df$GT_QC_Sample_ID, "<br>",
    "Species: ", df$Species, "<br>",
    "Align_PCT: ", df$Alignment, "%"
  )
  
  plot_ly(
    data = df,
    x = ~GT_QC_Sample_ID,
    y = ~Alignment,
    color = ~Species,
    type = 'bar',
    text = ~hover_text,
    hoverinfo = "text", textposition = "none"
  ) %>%
    layout(
      hovermode = "x",  # Compare data on hover across traces
      barmode = 'stack',
      xaxis = list(title = "Sample ID", tickangle = -45),
      yaxis = list( title = "Alignment (%)", autorange = TRUE),
      legend = list(
        orientation = "h", x = 0, xanchor = "left", y = -0.3,
        font = list(size = 10), itemwidth = 100, itemsizing = "constant"
      ),
      margin = list(t = 50, b = 150, l = 80, r = 80)
    ) %>%
    event_register('plotly_click') %>%
    config(displayModeBar = TRUE, displaylogo = FALSE)
  
})

#--------Return to non-Flo mode only when switching into the Species Alignment tab--------
observeEvent(input$main_tabs, {
  req(input$main_tabs == "species_tab")
  # if we’re currently in Flo mode, reset back to Box
  if (isolate(input$plt) == "Flo") {
    updateRadioButtons(session, "plt", selected = "Box", inline   = TRUE)
  }
}, ignoreInit = TRUE)

# -------- UI Switch Between Plot and Table --------
output$species_view_ui <- renderUI({
  req(selected_species_view())
  
  # Check if filters are empty (before any selection)
  if (is.null(input$var1) || length(input$var1) == 0 ||
      is.null(input$var3) || length(input$var3) == 0 ||
      is.null(input$var6) || length(input$var6) == 0) {
    
    # 🕒 Placeholder: waiting for user input
    return(
      div(
        style = "display: flex; align-items: center; justify-content: center; height: 400px;",
        div(
          style = "background-color: #098207; border: 1px solid #ddd; border-radius: 12px; padding: 30px 40px; box-shadow: 0 4px 12px rgba(0,0,0,0.05); text-align: center; max-width: 500px;",
          tags$i(class = "fas fa-spinner fa-spin fa-2x", style = "color: #020305; margin-bottom: 15px;"),
          h3("Preparing your plots..."),
          p("Waiting for data selection. Please choose options from the left panel.",
            style = "font-size: 16px; color: #e4f5f4; margin: 0;"),
          p("Note: nothing will appear here until you select filters above.",
            style = "font-size: 14px; color: white; font-style: italic; margin: 0;")
        )
      )
    )
  }
  # At this point inputs are selected → fetch data
  data <- filtered_species_metric()
  #  No matching dataset
  if (is.null(data) || nrow(data) == 0) {
    return(
      div(
        style = "padding: 30px; font-size: 16px; color: darkred;",
        icon("exclamation-triangle", lib = "font-awesome"),
        span("No dataset match found for the current input parameters."),
        tags$br(),
        span("→→→If the plot or table fails to render, the selected combination of Lab or Run Type likely has no associated SpeciesAlignment metrics."),
        tags$br(),
        span("→→→SpeciesAlignment metrics capture was initiated in 2025; earlier datasets may not include this schema."),
        tags$br(),
        span("→→→SpeciesAlignment metrics may not be found for some apps e.g. ONT, PacBio, for now.")
      )
    )
  }
  # Render plot or table
  if (selected_species_view() == "plot") {
    div(
      style = 'width: 100%; height: 600px; overflow-x: auto;',
      shinycssloaders::withSpinner(
        plotlyOutput(ns("species_plot"), height = "600px"),
        type = 4, color = "#0a9396", proxy.height = "600px"
      )
    )
  } else {
    div(style = "width: 100%;", DT::dataTableOutput(ns("species_table")))
  }
})



