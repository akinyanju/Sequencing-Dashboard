
###########################################################################
# -------- Radio buttons for selecting plot type --------
output$plot_type <- renderUI({
  radioButtons("plt", "Plot type", choices = c("Box", 'Bar', 'Flo'),
               selected = NULL, inline = TRUE)
})
###########################################################################
# -------- Debounced version of react_input_long to avoid double re-triggering --------
react_input_long_debounced <- debounce(react_input_long, 400)
###########################################################################
# -------- dynamically render the plot area’s user interface:--------
output$plot_ui <- renderUI({
  req(input$plt)
  
  # ─── FLO MODE UI ─────────────────────────────────────────────────────────────
  # ─── FLO MODE UI ─────────────────────────────────────────────────────────────
  if (input$plt == "Flo") {
    dat_raw <- tryCatch({
      df <- filtered_by_year_cache()
      
      # If FlowcellID doesn't exist, show the placeholder
      if (!"FlowcellID" %in% names(df)) stop("FlowcellID missing")
      
      df <- df %>%
        dplyr::filter(
          Investigator_Folder %in% input$var1,
          Project_ID          %in% input$var2,
          !is.na(FlowcellID),
          FlowcellID != "NULL"
        )
      
      if (nrow(df) == 0) stop("No flowcell-matched data")
      
      df
    }, error = function(e) {
      # Return placeholder on any failure
      return(NULL)
    })
    
    # If NULL, show the placeholder
    if (is.null(dat_raw)) {
      return(
        div(
          style = "display: flex; align-items: center; justify-content: center; height: 400px;",
          div(
            style = "background-color: #098207; border: 1px solid #ddd; border-radius: 12px; padding: 30px 40px; box-shadow: 0 4px 12px rgba(0,0,0,0.05); text-align: center; max-width: 500px;",
            tags$i(class = "fas fa-spinner fa-spin fa-2x", style = "color: #020305; margin-bottom: 15px;"),
            h3("Preparing your plots..."),
            p("Waiting for data selection. Choose options from the left panel.",
              style = "font-size: 16px; color: #e4f5f4; margin: 0;"),
            p("Note: nothing will appear here if selected app/lab have no FlowcellID information.",
              style = "font-size: 14px; color: white; font-style: italic; margin: 0;")
          )
        )
      )
    }
    
    
    # Compute picker choices
    flow_opts <- sort(unique(dat_raw$FlowcellID))
    selected_flowcell <- isolate(input$flo_flowcell) %||% flow_opts
    flov <- dat_raw %>% filter(FlowcellID %in% selected_flowcell)
    
    run_opts <- sort(unique(flov$Project_run_type))
    selected_runs <- isolate(input$flo_runs) %||% run_opts
    run_filtered <- flov %>% filter(Project_run_type %in% selected_runs)
    
    metric_opts <- sort(unique(intersect(trimws(run_filtered$name), flo_only_metrics)))
    selected_metric <- isolate(input$flo_metric)
    if (is.null(selected_metric) || !(selected_metric %in% metric_opts)) {
      selected_metric <- head(metric_opts, 1)
    }
    
    # Render the three Flo pickers + plot area
    return(
      fluidPage(
        fluidRow(
          column(4,
                 pickerInput(
                   inputId  = ns("flo_flowcell"),
                   label    = "Select Flowcell",
                   choices  = flow_opts,
                   selected = selected_flowcell,
                   multiple = TRUE,
                   options  = pickerOptions(
                     actionsBox         = TRUE,
                     liveSearch         = TRUE,
                     size               = 10,
                     title              = "Select flowcells",
                     selectedTextFormat = 'count > 1',
                     countSelectedText  = "{0} of {1} flowcell(s) selected"
                   )
                 )
          ),
          column(4,
                 pickerInput(
                   inputId  = ns("flo_runs"),
                   label    = "Compare Runs",
                   choices  = run_opts,
                   selected = selected_runs,
                   multiple = TRUE,
                   options  = pickerOptions(
                     actionsBox         = TRUE,
                     liveSearch         = TRUE,
                     size               = 10,
                     title              = "Select runs",
                     selectedTextFormat = 'count > 1',
                     countSelectedText  = "{0} of {1} run(s) selected"
                   )
                 )
          ),
          column(4,
                 pickerInput(
                   inputId  = ns("flo_metric"),
                   label    = "Select Run Metric",
                   choices  = metric_opts,
                   selected = selected_metric,
                   multiple = FALSE,
                   options  = pickerOptions(
                     liveSearch = TRUE,
                     size       = 10,
                     title      = "Select a metric"
                   )
                 )
          )
        ),
        tags$hr(),
        shinycssloaders::withSpinner(
          plotlyOutput(ns("plot"), height = "600px"),
          type = 4
        )
      )
    )
  }
  
  # ─── ORIGINAL “NO DATA” PLACEHOLDER & NON-FLO LOGIC ────────────────────────────
  # Determine dat_raw for Box/Bar
  dat_raw <- NULL
  val <- tryCatch(react_input_long_debounced(), error = function(e) NULL)
  if (!is.null(val) && inherits(val, "data.frame")) {
    dat_raw <- val
  }
  
  # No data placeholder
  if (is.null(dat_raw) || nrow(dat_raw) == 0) {
    return(
      div(
        style = "display: flex; align-items: center; justify-content: center; height: 400px;",
        div(
          style = "background-color: #098207; border: 1px solid #ddd; border-radius: 12px; padding: 30px 40px; box-shadow: 0 4px 12px rgba(0,0,0,0.05); text-align: center; max-width: 500px;",
          tags$i(class = "fas fa-spinner fa-spin fa-2x", style = "color: #020305; margin-bottom: 15px;"),
          h3("Preparing your plots..."),
          p("Waiting for data selection. Please choose options from the left panel.",
            style = "font-size: 16px; color: #e4f5f4; margin: 0;"),
          p("Note: nothing will appear here if selected filters have no metrics.",
            style = "font-size: 14px; color: white; font-style: italic; margin: 0;")
        )
      )
    )
  }
  
  # Prevent mixed-metric errors
  pct_flags <- grepl("PCT", dat_raw$name)
  if (any(pct_flags) && !all(pct_flags)) {
    return(
      div(
        style = "display: flex; align-items: center; justify-content: center; height: 250px;",
        div(
          style = "background-color: #fff4f4; border: 1px solid #f5c2c7; border-radius: 10px; padding: 20px 30px; box-shadow: 0 4px 10px rgba(0,0,0,0.05); text-align: left; max-width: 500px;",
          tags$i(class = "fas fa-exclamation-circle fa-lg", style = "color: #dc3545; margin-bottom: 10px;"),
          h5(style = "color: #dc3545; margin-top: 0;", "Error: Plotting Prevented"),
          tags$ul(
            style = "font-size: 14px; color: #555; padding-left: 20px; margin: 0;",
            tags$li("You mixed 'PCT' with standard metrics."),
            tags$li("Please select only 'PCT' metrics or only standard metrics.")
          )
        )
      )
    )
  }
  
  # Default spinner + Plotly output for Box/Bar
  shinycssloaders::withSpinner(
    plotlyOutput(ns("plot"), height = "600px"),
    type = 4
  )
})


###########################################################################
# --------Reactive: Sample size grouped by folder--------
sample_size <- reactive({
  data <- react_input_long()
  if (is.null(data) || nrow(data) == 0) return(NULL)
  data %>%
    group_by(Investigator_Folder) %>%
    summarize(num = length(unique(Sample_Name)), .groups = "drop")
})
###########################################################################
# --------Reactive: Value summaries (mean and sum)--------
react_value <- reactive({
  data <- react_input_long()
  if (is.null(data) || nrow(data) == 0) return(NULL)

  data %>%
    group_by(Investigator_Folder, name) %>%
    summarize(
      value_mean = mean(value),
      value_sum = sum(value),
      .groups = "drop"
    )
})
###########################################################################
output$plot <- renderPlotly({
  req(input$plt)
  data <- react_input_long()
  if (is.null(data) || nrow(data) == 0) return(NULL)

  # --------Adjust font size based on number of levels--------
  axis_text_size <- if (length(unique(data$Investigator_Folder)) <= 25) 13 else 10
  face_wrap_strip_size <- if (length(unique(data$Project_run_type)) <= 4) 15 else 10

  # --------Theme used in Flo plot (ggplot2-based)--------
  geom_theme <- function() {
    theme_minimal(base_size = 14) +
      theme(legend.title = element_text(size = 15),
            legend.text = element_text(size = 15),
            axis.title.x = element_text(size = 15, color = "black"),
            axis.title.y = element_text(size = 15, color = "black"),
            axis.text = element_text(size = axis_text_size, color = "black"),
            legend.position = "none",
            # Transparent background with border
            panel.background = element_rect(fill = "transparent", color = "black", linewidth = 0.5),
            plot.background  = element_rect(fill = "transparent", color = "black", linewidth = 0.8),

            # Facet strip styling
            strip.background = element_rect(fill = "grey", color = "black", linewidth = 0.5),
            strip.text       = element_text(size = face_wrap_strip_size, face = "bold")
      )
  }
  
  get_tick_format <- function(is_pct, y_values) {
    if (is_pct) {
      return(NULL) 
    } else if (min(y_values, na.rm = TRUE) < 0.001) {
      return(".0e")  # scientific for small values
    } else {
      return(NULL)   # let Plotly use default (e.g., 1K, 10M)
    }
  }
  

  ####################################
  # -------- BOX PLOT --------
  ####################################
  if (input$plt == "Box") {
    # Ensure value is numeric
    data <- data %>%
      mutate(value = suppressWarnings(as.numeric(gsub(",", "", value)))) %>%
      filter(!is.na(value))

    #legend <- if (any(grepl("PCT", data$name))) "Percentage (%)" else "Read"
    is_pct <- all(grepl("PCT", data$name))
    legend <- if (is_pct) "Percentage (%, per sample)" else "Read Count (per sample)"
    tick_format <- get_tick_format(is_pct, data$value)
    
    # Now compute metric names and assign colors safely
    metric_names <- unique(data$name)
    num_colors <- max(3, length(metric_names))

    colors <- if (num_colors <= 8) {
      RColorBrewer::brewer.pal(num_colors, "Set2")
    } else {
      colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))(num_colors)
    }
    names(colors) <- metric_names

    # Get sample sizes
    sample_sizes <- data %>%
      group_by(Investigator_Folder) %>%
      summarise(num = n_distinct(Sample_Name), .groups = "drop")

    data_prepped <- data %>%
      left_join(sample_sizes, by = "Investigator_Folder") %>%
      mutate(myaxis = paste0(Investigator_Folder, " (n=", num, ")"))

    num_groups <- length(unique(data_prepped$myaxis))
    plot_width <- max(800, min(3000, num_groups * 50))

    plot_ly(
      data = data_prepped,
      x = ~myaxis,
      y = ~value,
      type = 'box',
      split = ~name,
      colors = colors,
      boxpoints = 'all',
      jitter = 0.3,
      pointpos = 0,
      marker = list(opacity = 0.6, size = 7),
      hoverinfo = "text",
      text = ~paste0(
        "<b>Metric value:</b> ", value,
        "<br><b>Metric name:</b> ", name,
        "<br><b>Lab:</b> ", Investigator_Folder,
        "<br><b>FlowcellID:</b> ", FlowcellID,
        "<br><b>Run:</b> ", Project_run_type,
        "<br><b>Sample:</b> ", Sample_Name),
      hoverlabel = list(
        bgcolor = "white",
        align = "left",
        font = list(size = 13)
      )
    ) %>%
      layout(
        hovermode = "x",
        xaxis = list(title = "", tickangle = 300, tickfont = list(size = 12)),
        yaxis = list(title = legend, zeroline = TRUE, tickformat = tick_format),
        boxmode = "group",
        legend = list(orientation = "h", x = 0.5, xanchor = "center", y = 1.05, yanchor = "bottom"),
        margin = list(t = 100, b = 150),
        hovermode = "closest",        # Ensures hover is per-point
        hoverdistance = 5,            # Keeps tooltip closer to point
        spikedistance = -1            # Prevents crosshair jumpiness
      ) %>% event_register('plotly_click') %>%
      config(displayModeBar = TRUE, displaylogo = FALSE)
    ####################################
    # -------- FLO PLOT --------
    ####################################
  } else if (input$plt == "Flo") {
    data <- filtered_by_year_cache()
    if (is.null(data) || nrow(data) == 0) return(NULL)

    req(input$flo_flowcell, input$flo_metric, input$flo_runs)

    data <- data %>%
      filter(
        FlowcellID %in% input$flo_flowcell,
        name == input$flo_metric,
        Project_run_type %in% input$flo_runs
      )

    if (nrow(data) == 0) return(div("⚠️ No data for selected metric in Flo mode"))

    # Try parsing "X +/- Y" style values first
    parsed <- strsplit(as.character(data$value), "\\s*\\+/-\\s*")
    has_error_format <- lengths(parsed) == 2

    if (any(has_error_format)) {
      data <- data[has_error_format, ]
      data$value_mean  <- suppressWarnings(as.numeric(sapply(parsed, `[`, 1)))
      data$value_error <- suppressWarnings(as.numeric(sapply(parsed, `[`, 2)))
      data <- data[!is.na(data$value_mean) & !is.na(data$value_error), ]
      if (nrow(data) == 0) return(div("⚠️ No valid ± error values"))

      # Ensure Lane is numeric for correct X-axis rendering
      data$Lane <- as.numeric(as.character(data$Lane))

      p <- ggplot(data, aes(x = Lane, y = value_mean, color = Project_run_type, group = Project_run_type)) +
        geom_line() +
        geom_point(size = 3,
                   aes(text = paste0(
                     "Lane: ", Lane, "<br>",
                     "Metric name: ", name, "<br>",
                     "Metric value: ", round(value_mean, 2), " ± ", round(value_error, 2), "<br>",
                     "Species: ", Species, "<br>",
                     "Run: ", Project_run_type, "<br>",
                     "Lab: ", Investigator_Folder
                   ))) +
        # Vertical error bars
        geom_errorbar(aes(
          ymin = value_mean - value_error,
          ymax = value_mean + value_error,
          text = paste0(
            "Lane: ", Lane, "<br>",
            "Metric name: ", name, "<br>",
            "Metric value: ", round(value_mean, 2), " ± ", round(value_error, 2), "<br>",
            "Species: ", Species, "<br>",
            "Run: ", Project_run_type, "<br>",
            "Lab: ", Investigator_Folder
          )
        ), width = 0.25, linewidth = 0.8) +

        # ⎺ Top cap
        geom_segment(
          aes(
            x = Lane - 0.1,
            xend = Lane + 0.1,
            y = value_mean + value_error,
            yend = value_mean + value_error
          ),
          linewidth = 0.8
        ) +
        # ⎽ Bottom cap
        geom_segment(
          aes(
            x = Lane - 0.1,
            xend = Lane + 0.1,
            y = value_mean - value_error,
            yend = value_mean - value_error
          ),
          linewidth = 0.8
        ) +
        facet_wrap(~FlowcellID, ncol = 2) +
        ylab(input$flo_metric) +
        xlab("Lane") +
        geom_theme()


    } else {
      # Fallback: simple numeric
      data$value_numeric <- suppressWarnings(as.numeric(as.character(data$value)))
      data <- data[!is.na(data$value_numeric), ]
      if (nrow(data) == 0) return(div("⚠️ No numeric values for plotting"))

      p <- ggplot(data, aes(x = Lane, y = value_numeric, color = Project_run_type, group = Project_run_type)) +
        geom_line() +
        geom_point(size = 3) +
        facet_wrap(~FlowcellID, ncol = 2) +
        ylab(input$flo_metric) +
        xlab("Lane") +
        geom_theme()
    }
    ggplotly(p, tooltip = "text", height = 600) %>%
      layout(hovermode = "x") %>% event_register('plotly_click') %>%
      config(displayModeBar = TRUE, displaylogo = FALSE)
    ####################################
    # -------- BAR PLOT --------
    ####################################
  } else {
    data <- react_input_long()
    if (is.null(data) || nrow(data) == 0) return(NULL)
    sample_size <- sample_size()

    # Now compute metric names and assign colors safely
    metric_names <- unique(data$name)
    num_colors <- max(3, length(metric_names))

    colors <- if (num_colors <= 8) {
      RColorBrewer::brewer.pal(num_colors, "Set2")
    } else {
      colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))(num_colors)
    }
    names(colors) <- metric_names

    df <- react_value() %>%
      left_join(sample_size) %>%
      mutate(myaxis = paste0(Investigator_Folder, " (n=", num, ")"))
    
    is_pct <- all(grepl("PCT", df$name))  # Only true if all selected metrics are PCT
    y <- if (is_pct) df$value_mean else df$value_sum
    legend <- if (is_pct) "Percentage (%, mean across samples)" else "Total Reads (summed)"

    num_bars <- length(unique(df$myaxis))
    plot_width <- max(800, min(3000, num_bars * 50))

    tick_format <- get_tick_format(is_pct, y)
    
    plot_ly(df, x = ~myaxis, y = ~y, split = ~name, colors = setNames(colors, metric_names),
            width = plot_width, height = 600, type = 'bar') %>%
      layout(margin = list(t = 100, b = 150),
             yaxis = list(title = legend, zeroline = TRUE, tickformat = tick_format),
             legend = list(orientation = "h", x = 0.5, xanchor = "center", y = 1, yanchor = "bottom"),
             xaxis = list(title = "", tickangle = 300, showticklabels = TRUE,
                          tickfont = list(size = 12)),
             barmode = 'group', autosize = FALSE,
             hovermode = "x"
      ) %>% event_register('plotly_click') %>%
      config(displayModeBar = TRUE, displaylogo = FALSE)
  }
})


############################################################################################################
