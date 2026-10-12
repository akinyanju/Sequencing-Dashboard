
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
# -------- Plot panel height: fills available viewport instead of a fixed
# 600px that leaves dead space below on tall screens (nothing to scroll to)
# or clips the plot's own rotated axis labels on short ones (see
# modules/dashboardBodyUI.R for the small script that reports window_h on
# load/resize). Read by both plotlyOutput() below (container size) and the
# ggplotly()/plot_ly() calls in output$plot (the figure's own size) so the
# two stay in sync.
###########################################################################
plot_panel_height <- reactive({
  h <- input$window_h
  if (is.null(h)) return(600)
  # Leave room for the header, tab bar, View Plot/MultiQC buttons, and the
  # filter/picker row above the plot itself.
  max(420, h - 300)
})
###########################################################################
# -------- dynamically render the plot area’s user interface:--------
output$plot_ui <- renderUI({
  req(input$plt)

  # ─── FLO MODE UI ─────────────────────────────────────────────────────────────
  # ─── FLO MODE UI ─────────────────────────────────────────────────────────────
  if (input$plt == "Flowcell") {
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
            p("Larger applications (e.g. rnaseq, wgs) can take up to a minute to populate the filters on the left — please wait rather than re-clicking.",
              style = "font-size: 14px; color: white; margin: 8px 0 0 0;"),
            p("Note: nothing will appear here if selected app/lab have no FlowcellID information.",
              style = "font-size: 14px; color: white; font-style: italic; margin: 8px 0 0 0;")
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
                   label    = "Select Run(s)",
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
          plotlyOutput(ns("plot"), height = paste0(plot_panel_height(), "px")),
          type = 4
        )
      )
    )
  }

  # ─── COMPARE RUNS MODE UI ───────────────────────────────────────────────────
  # Year choices come from available_years() — the same distinct-years result
  # the main sidebar already computes to populate release_year, re-exposed
  # from there instead of run again here. The actual row data
  # (compare_data_pool()) defaults cmp_year to input$release_year, which lets
  # it reuse filtered_by_year_cache() — data the sidebar already loaded —
  # instead of a fresh query; see dashboardServer.R for the full reasoning
  # (a prior version re-queried on every single app switch, which was slow
  # enough to look like a hang, and before that, loaded the app's entire
  # multi-year history unconditionally for every user and took the
  # production server down).
  #
  # Lab/Run/Metric choices depend on compare_data_pool(), which doesn't exist
  # until cmp_year has a value — so the very first render (before any year is
  # selected) shows Year only, with Lab/Run/Metric empty; they populate
  # moments later once this re-renders. In the common fast-path case that's
  # near-instant since no new query is involved.
  if (input$plt == "Compare Runs") {
    # Show a placeholder immediately while compare_data_pool() is (re)computing
    # for the current Application/Year selection — e.g. right after switching
    # Application while already in Compare Runs, which can involve a fresh,
    # scoped DB query. Without this, the panel goes blank for that stretch
    # since the picker layout below depends on the pool being ready.
    if (isTRUE(compare_pool_loading())) {
      return(
        div(
          style = "display: flex; align-items: center; justify-content: center; height: 400px;",
          div(
            style = "background-color: #098207; border: 1px solid #ddd; border-radius: 12px; padding: 30px 40px; box-shadow: 0 4px 12px rgba(0,0,0,0.05); text-align: center; max-width: 500px;",
            tags$i(class = "fas fa-spinner fa-spin fa-2x", style = "color: #020305; margin-bottom: 15px;"),
            h3("Preparing your plots..."),
            p("Fetching comparison data for the selected application...",
              style = "font-size: 16px; color: #e4f5f4; margin: 0;"),
            p("Larger applications (e.g. rnaseq, wgs) can take up to a minute — please wait rather than re-clicking.",
              style = "font-size: 14px; color: white; margin: 8px 0 0 0;")
          )
        )
      )
    }

    year_opts <- available_years()

    if (length(year_opts) == 0) {
      return(
        div(
          style = "display: flex; align-items: center; justify-content: center; height: 400px;",
          div(
            style = "background-color: #098207; border: 1px solid #ddd; border-radius: 12px; padding: 30px 40px; box-shadow: 0 4px 12px rgba(0,0,0,0.05); text-align: center; max-width: 500px;",
            tags$i(class = "fas fa-spinner fa-spin fa-2x", style = "color: #020305; margin-bottom: 15px;"),
            h3("Preparing your comparison..."),
            p("No delivered projects found for this application yet.",
              style = "font-size: 16px; color: #e4f5f4; margin: 0;")
          )
        )
      )
    }

    # Year — default to the SAME single year the main sidebar currently has
    # selected (falling back to the most recent available year if that one
    # isn't valid here, e.g. the sidebar's "Undelivered / Missing Date"
    # pseudo-year). This isn't just a UX choice — matching the sidebar's year
    # exactly is what lets compare_data_pool() reuse already-loaded data
    # instead of running a fresh query. Capped at 2 (also enforced
    # client-side via pickerOptions(maxOptions=2)): even with the
    # multi-year query itself narrowed down, compare_plot_data() still
    # scales with how much of the table it has to touch, and 2 years
    # covers the "compare this run against last year's" case that
    # actually gets used without inviting an open-ended multi-year fetch.
    default_year <- if (!is.null(input$release_year) && input$release_year %in% year_opts) {
      input$release_year
    } else {
      year_opts[1]
    }
    selected_years <- intersect(isolate(input$cmp_year) %||% default_year, year_opts)
    if (length(selected_years) == 0) selected_years <- default_year
    if (length(selected_years) > 2) selected_years <- head(selected_years, 2)

    # isolate(): this render must NOT re-fire just because
    # compare_data_pool() itself changed (e.g. the newly-created cmp_year
    # picker's own value round-tripping to the server the moment it's
    # selected). The cmp_year -> cmp_lab -> cmp_run -> cmp_metrics
    # observeEvent cascade in dashboardServer.R already owns patching those
    # widgets in place via updatePickerInput() for that case. Without
    # isolate() here, BOTH mechanisms fire off the same trigger: this
    # renderUI tears down and recreates the picker DOM at the same moment
    # the cascade's updatePickerInput() message tries to patch it, and the
    # client-side bootstrap-select/selectize binding throws ("Cannot read
    # properties of undefined (reading 'setValue')") on the widget mid
    # teardown, which aborts that Shiny render batch and leaves the whole
    # container empty. isolate() still reads the current pool (so an app
    # switch, which invalidates this renderUI via available_years(), picks
    # up a fresh pool) without the extra render-per-cmp_year-tick trigger.
    pool <- isolate(compare_data_pool())
    have_pool <- !is.null(pool) && nrow(pool) > 0

    if (have_pool) {
      # Lab
      if (is_genome_tech()) {
        lab_opts <- sort(unique(pool$Investigator_Folder))
      } else {
        lab_opts <- trimws(as.character(selectedGroup()))
      }
      # Default to just the first lab, not all of them — keeps the initial
      # comparison small; users can add more labs themselves if they want.
      selected_labs <- intersect(isolate(input$cmp_lab) %||% lab_opts[1], lab_opts)
      if (length(selected_labs) == 0) selected_labs <- lab_opts[1]
      by_lab <- pool %>% filter(Investigator_Folder %in% selected_labs)

      # Project Run
      run_opts <- sort(unique(by_lab$Project_run_type))
      selected_runs <- intersect(isolate(input$cmp_run) %||% head(run_opts, 3), run_opts)
      if (length(selected_runs) == 0) selected_runs <- head(run_opts, 3)
      by_run <- by_lab %>% filter(Project_run_type %in% selected_runs)

      # Metric(s) — same Standard/PCT grouping as the main sidebar's var5
      excl <- paste0("^(", paste(c(paste0("Domain", 1:4), paste0("Species", 1:5)), collapse = "|"), ")$")
      metric_opts <- sort(unique(by_run$name[!grepl(excl, by_run$name)]))
      metric_opts <- setdiff(metric_opts, flo_only_metrics)
      pct_metrics <- metric_opts[grepl("PCT", metric_opts)]
      std_metrics <- metric_opts[!grepl("PCT", metric_opts)]
      metric_groups <- list("📊 Standard Metrics" = std_metrics, "📉 PCT Metrics" = pct_metrics)

      preferred <- intersect(c("Reads_Total", "Reads_Filtered"), metric_opts)
      default_metrics <- if (length(preferred) > 0) preferred else head(metric_opts, 1)
      selected_metrics <- intersect(isolate(input$cmp_metrics) %||% default_metrics, metric_opts)
      if (length(selected_metrics) == 0) selected_metrics <- default_metrics
    } else {
      # cmp_year hasn't round-tripped to the server yet, or its scoped query
      # is still running — these fill in on the next render once it lands.
      lab_opts <- NULL; selected_labs <- NULL
      run_opts <- NULL; selected_runs <- NULL
      metric_groups <- NULL; selected_metrics <- NULL
    }

    return(
      fluidPage(
        fluidRow(
          column(3,
                 # No actionsBox (Select All/Deselect All) here: bootstrap-select's
                 # "Select All" bypasses maxOptions entirely and selects every
                 # choice, silently blowing past the 2-year cap below — and with
                 # that cap in place, bulk-selecting is never something a user
                 # actually needs on this picker.
                 pickerInput(
                   inputId = ns("cmp_year"), label = "Year",
                   choices = year_opts, selected = selected_years, multiple = TRUE,
                   options = pickerOptions(liveSearch = TRUE, size = 10,
                                            title = "Select year(s)", selectedTextFormat = "count > 1",
                                            countSelectedText = "{0} of {1} year(s)",
                                            maxOptions = 2, maxOptionsText = "No more than 2 years at once")
                 )
          ),
          column(3,
                 pickerInput(
                   inputId = ns("cmp_lab"), label = "Lab",
                   choices = lab_opts, selected = selected_labs, multiple = TRUE,
                   options = pickerOptions(actionsBox = TRUE, liveSearch = TRUE, size = 10,
                                            title = "Select lab(s)", selectedTextFormat = "count > 1",
                                            countSelectedText = "{0} of {1} lab(s)")
                 )
          ),
          column(3,
                 pickerInput(
                   inputId = ns("cmp_run"), label = "Project Run(s) to Compare",
                   choices = run_opts, selected = selected_runs, multiple = TRUE,
                   options = pickerOptions(actionsBox = TRUE, liveSearch = TRUE, size = 10,
                                            title = "Select run(s)", selectedTextFormat = "count > 1",
                                            countSelectedText = "{0} of {1} run(s)")
                 )
          ),
          column(3,
                 pickerInput(
                   inputId = ns("cmp_metrics"), label = "Metric(s)",
                   choices = metric_groups, selected = selected_metrics, multiple = TRUE,
                   options = pickerOptions(actionsBox = TRUE, liveSearch = TRUE, size = 10,
                                            title = "Select metric(s)", selectedTextFormat = "count > 1",
                                            countSelectedText = "{0} of {1} metric(s)")
                 )
          )
        ),
        # Locks the 4 pickers above for as long as the plot output is
        # recomputing, so a user who doesn't wait for one fetch to finish
        # can't queue up repeated expensive DuckDB queries (most likely by
        # toggling Year, the one that can hit the slow multi-year path) —
        # exactly the kind of pile-up that pinned a production R process at
        # 100% CPU for minutes (see compare_data_pool()'s comment in
        # dashboardServer.R). This has to be done client-side: Shiny is
        # single-threaded per session, so a long-running compare_plot_data()
        # blocks the entire reactive flush — a server-side reactiveVal
        # toggle (tried first) never reaches the browser until the fetch it
        # was supposed to guard has already finished, since nothing gets
        # flushed to the client mid-computation. shiny:recalculating /
        # shiny:recalculated fire client-side the instant an input change is
        # sent (this is the same built-in signal shinycssloaders'
        # withSpinner below already uses for its own spinner on this exact
        # output), so they aren't subject to that blocking.
        tags$script(HTML(sprintf("
          (function() {
            if (window.__gtCompareLockBound) return;
            window.__gtCompareLockBound = true;
            var pickerIds = ['#%s', '#%s', '#%s', '#%s'];
            function setLocked(locked) {
              pickerIds.forEach(function(id) {
                var $el = $(id);
                if (!$el.length) return;
                $el.prop('disabled', locked);
                if (typeof $el.selectpicker === 'function') {
                  $el.selectpicker('refresh');
                } else {
                  $el.next('button.dropdown-toggle').prop('disabled', locked);
                }
              });
            }
            $(document).on('shiny:recalculating', function(e) {
              if (e.target && e.target.id === '%s') setLocked(true);
            });
            $(document).on('shiny:recalculated', function(e) {
              if (e.target && e.target.id === '%s') setLocked(false);
            });
          })();
        ", ns('cmp_year'), ns('cmp_lab'), ns('cmp_run'), ns('cmp_metrics'), ns('plot'), ns('plot')))),
        tags$hr(),
        shinycssloaders::withSpinner(
          plotlyOutput(ns("plot"), height = paste0(plot_panel_height(), "px")),
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
          p("Larger applications (e.g. rnaseq, wgs) can take up to a minute to populate the filters on the left — please wait rather than re-clicking.",
            style = "font-size: 14px; color: white; margin: 8px 0 0 0;"),
          p("Note: nothing will appear here if selected filters have no metrics.",
            style = "font-size: 14px; color: white; font-style: italic; margin: 8px 0 0 0;")
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
    plotlyOutput(ns("plot"), height = paste0(plot_panel_height(), "px")),
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

  # Compare Runs has its own dedicated data source and its own filters
  # (cmp_year/cmp_lab/cmp_run/cmp_metrics, not var3/var5/var6) — handled
  # here, before the generic react_input_long() pull below, so an empty
  # result from the SHARED sidebar's current selection can never block
  # this unrelated branch.
  #
  # compare_plot_data() (dashboardServer.R) does the filter + dedup +
  # mean-per-run-per-metric aggregation directly in DuckDB now, rather
  # than fetching compare_data_pool()'s raw rows here and running that
  # same pipeline in R — at production scale (millions of rows) the R-side
  # version was the actual bottleneck (a single R process pinned at 100%
  # CPU for minutes), not the database query.
  if (input$plt == "Compare Runs") {
    req(input$cmp_year, input$cmp_lab, input$cmp_run, input$cmp_metrics)
    summary_df <- compare_plot_data()
    if (is.null(summary_df) || nrow(summary_df) == 0) return(NULL)

    # Percentage metrics (0-100 by construction) get a "%" suffix; everything
    # else (raw read/base counts, often in the millions) gets a short-scale
    # suffix (e.g. "40M") instead of the default "4e+07"-style notation.
    y_label_fn <- function(x) {
      if (length(x) > 0 && all(is.finite(x)) && max(abs(x)) <= 100) {
        paste0(x, "%")
      } else {
        scales::label_number(scale_cut = scales::cut_short_scale())(x)
      }
    }

    p <- ggplot(
      summary_df,
      aes(x = Project_run_type, y = value, fill = Project_run_type,
          text = paste0(
            "Run: ", Project_run_type, "<br>",
            "Metric: ", name, "<br>",
            "Mean value: ", round(value, 2)
          ))
    ) +
      geom_col() +
      facet_wrap(~name, scales = "free_y") +
      scale_y_continuous(labels = y_label_fn) +
      ylab(NULL) + xlab(NULL) +
      theme_minimal(base_size = 12) +
      theme(
        legend.position = "none",
        axis.text.x = element_text(angle = 30, hjust = 1),
        strip.text = element_text(face = "bold")
      )

    gp <- ggplotly(p, tooltip = "text", height = plot_panel_height())

    # ggplot2's theme() font sizes don't reliably carry over 1:1 through
    # ggplotly() — they came out much larger than the rest of the app's
    # ~11-12px plotly convention (e.g. the Box plot's native tickfont). Set
    # them directly on the converted plotly spec instead, in real pixels.
    # facet_wrap produces one x/y axis pair per panel (xaxis, xaxis2, ... /
    # yaxis, yaxis2, ...), so every axis matching that pattern needs it, and
    # facet strip titles become annotations rather than axis text.
    axis_names <- grep("^[xy]axis", names(gp$x$layout), value = TRUE)
    for (ax in axis_names) {
      gp$x$layout[[ax]]$tickfont <- list(size = 11)
    }
    gp$x$layout$annotations <- lapply(gp$x$layout$annotations, function(a) {
      a$font <- list(size = 12)
      a
    })

    return(
      gp %>%
        layout(hovermode = "closest") %>% event_register('plotly_click') %>%
        config(displayModeBar = TRUE, displaylogo = FALSE)
    )
  }

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
  } else if (input$plt == "Flowcell") {
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
    ggplotly(p, tooltip = "text", height = plot_panel_height()) %>%
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
            width = plot_width, height = plot_panel_height(), type = 'bar') %>%
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
