module1_Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ######################################################################################
      observeEvent(input$option, {
        req(input$option)  
        
        year_choices <- get_distinct_years()
        
        if (length(year_choices) == 0) {
          showNotification("⚠️ No valid years found in the database.", type = "warning")
        } else {
          updatePickerInput(session, "year",
                            choices = year_choices,
                            selected = head(year_choices, 2))
        }
        # Period — respect factor order (re-evaluated based on input$option)
        period_choices <- get_distinct_periods(option = input$option)
        updatePickerInput(session, "period",
                          choices = period_choices,
                          selected = period_choices)
        # PLATFORM — all selected
        platform_choices <- get_distinct_platforms()
        updatePickerInput(session, "platform",
                          choices = platform_choices,
                          selected = platform_choices)
        
      })
      ######################################################################################
      Appchannel <- reactive({
        list(input$platform,input$period,input$year)
      })
      
      observeEvent(Appchannel(), {
        platforms <- toupper(input$platform)
        years <- input$year  
        
        app_choices <- get_distinct_applications(platforms = platforms, years = years)
        updatePickerInput(session = session, inputId = "application",
                          choices = app_choices,
                          selected = app_choices)
        
        if ("ILLUMINA" %in% platforms) {
          shinyjs::enable("application_parent")
          shinyjs::enable("application")
          shinyjs::show("application")
        } else {
          shinyjs::disable("application_parent")
          shinyjs::hide("application")
          shinyjs::disable("application")
        }
      })
      ######################################################################################
      observeEvent(input$application, {
        apps <- toupper(input$application)
        platforms <- toupper(input$platform)
        
        metric_choices <- get_available_metrics(applications = apps, platforms = platforms)
        
        updatePickerInput(session = session, inputId = "metrics",
                          choices = metric_choices,
                          selected = head(metric_choices))
      })
      ####################################################################################
      #Additional plots
      channel <- reactive({
        list(input$more_plot,input$more_plots_opt, input$platform,input$period,input$year, input$application)
      })

      observeEvent(channel(), {
        req(input$more_plot)
        req(input$more_plots_opt)
        req(input$site)
        
        if (input$more_plot == "Yes") {
          shinyjs::disable("site")
          shinyjs::enable("more_plots_opt")
          shinyjs::show("more_plots_opt")
          
          platforms <- toupper(input$platform)
          years <- input$year
          apps <- input$application
          
          if (input$more_plots_opt == "Machine") {
            instruments <- get_distinct_instruments(platforms = platforms, years = years, applications = apps)
            updatePickerInput(session, "instrument", choices = sort(instruments), selected = sort(instruments))
            
            shinyjs::disable("instrument_parent")
            shinyjs::hide("instrument")
            shinyjs::disable("instrument")
            
            shinyjs::disable("lab_parent")
            shinyjs::hide("lab")
            shinyjs::disable("lab")
            
            shinyjs::show("metrics")
            
            if ("ILLUMINA" %in% platforms) {
              shinyjs::enable("application_parent")
              shinyjs::enable("application")
              shinyjs::show("application")
            }
            
          } else if (input$more_plots_opt == "Lab") {
            labs <- get_distinct_labs(platforms = platforms, years = years, applications = apps)
            updatePickerInput(session, "lab", choices = sort(labs), selected = sort(labs))
            
            shinyjs::enable("lab_parent")
            shinyjs::show("lab")
            shinyjs::enable("lab")
            
            shinyjs::show("metrics")
            
            if ("ILLUMINA" %in% platforms) {
              shinyjs::enable("application_parent")
              shinyjs::enable("application")
              shinyjs::show("application")
            }
            
          } else if (input$more_plots_opt == "Project") {
            shinyjs::hide("application")
            shinyjs::hide("metrics")
            shinyjs::disable("lab_parent")
            shinyjs::hide("lab")
            shinyjs::disable("lab")
            
            # Use a query to get distinct applications
            apps_list <- get_distinct_applications(platforms = platforms, years = years)
            updatePickerInput(session, "application", choices = sort(apps_list), selected = sort(apps_list))
          }
          
        } else if (input$more_plot == "No") {
          if ("ILLUMINA" %in% toupper(input$platform)) {
            shinyjs::enable("application_parent")
            shinyjs::enable("application")
            shinyjs::show("application")
          }
          
          shinyjs::show("metrics")
          shinyjs::enable("site")
          shinyjs::disable("more_plots_opt")
          shinyjs::hide("more_plots_opt")
          
          shinyjs::disable("instrument_parent")
          shinyjs::hide("instrument")
          shinyjs::disable("instrument")
          
          shinyjs::disable("lab_parent")
          shinyjs::hide("lab")
          shinyjs::disable("lab")
        }
      })
      ####################################################################################
      # Trigger value to force refresh
      seq_reload_trigger <- reactiveVal(runif(1))  # random token
      # Last refresh time
      last_refresh_time <- reactiveVal(Sys.time())
      ####################################################################################
      # Refresh on button click
      observeEvent(input$refetch_btn, {
        seq_reload_trigger(runif(1))               # update token
        last_refresh_time(Sys.time())              # update timestamp
      })
      ####################################################################################
      #
      retrigger_option <- function() {
        isolate({
          current_option <- input$option
          updateRadioButtons(session, "option", selected = ifelse(current_option == "Yes", "No", "Yes"))
          updateRadioButtons(session, "option", selected = current_option)
        })
      }
      
      observeEvent(input$refetch_btn, {
        if (!file.exists(path_to_duckdb)) {
          showNotification("❌ Database file missing! Inform gtdrylab@jax.org immediately", type = "error")
          return()
        }
        try({
          # Close any previous connection
          DBI::dbDisconnect(con, shutdown = TRUE)
          gc()  # Release file handles
          Sys.sleep(0.1)
        }, silent = TRUE)
        
        # Reconnect (read-only)
        con <<- DBI::dbConnect(duckdb::duckdb(), dbdir = path_to_duckdb, read_only = TRUE)
        # Force downstream update
        seq_reload_trigger(runif(1))  # This will re-trigger InputSeqMet()
        # Update timestamp
        last_refresh_time(Sys.time())
        showNotification("✅ Refreshed data from DuckDB.", type = "message")
        retrigger_option()
      })
      ####################################################################################
      # Update the dropdown menu after refetch
      observeEvent(seq_reload_trigger(), {
        # Get SQL-backed dropdown values
        year_choices     <- get_distinct_years()
        period_choices   <- get_distinct_periods(option = input$option)
        platform_choices <- get_distinct_platforms()
        
        # Use first selected values to fetch dependent dropdowns
        selected_year     <- head(year_choices, 2)
        selected_platform <- head(platform_choices)
        
        app_choices <- get_distinct_applications(
          platforms = toupper(selected_platform),
          years = selected_year
        )
        
        selected_app <- head(app_choices, 1)
        
        metric_choices <- get_available_metrics(
          applications = toupper(selected_app),
          platforms = toupper(selected_platform)
        )
        
        # YEAR
        updatePickerInput(session, "year",
                          choices = year_choices,
                          selected = selected_year)
        
        # PERIOD
        updatePickerInput(session, "period",
                          choices = period_choices,
                          selected = period_choices)
        
        # PLATFORM
        updatePickerInput(session, "platform",
                          choices = platform_choices,
                          selected = selected_platform)
        
        # APPLICATION
        updatePickerInput(session, "application",
                          choices = app_choices,
                          selected = app_choices)
        
        # Show/hide logic for ILLUMINA
        if ("ILLUMINA" %in% toupper(selected_platform)) {
          shinyjs::enable("application_parent")
          shinyjs::enable("application")
          shinyjs::show("application")
        } else {
          shinyjs::disable("application_parent")
          shinyjs::disable("application")
          shinyjs::hide("application")
        }
        
        # METRICS
        updatePickerInput(session, "metrics",
                          choices = metric_choices,
                          selected = head(metric_choices))
      })
      #####################################################################################
      output$last_refresh_text <- renderUI({
        time <- last_refresh_time()
        text <- if (!is.null(time)) {
          paste("🕒 Last fetched on", format(time, "%Y-%m-%d at %H:%M:%S"))
        } else {
          "🕒 No data fetched yet"
        }
        tags$div(style = "font-size: 16px; font-weight: bold; color: #333;", text)
      })
      ######################################################################################
       react_input_Main <- reactive({
        req(input$year, input$period, input$platform, input$application, input$metrics)
        
        df <- tryCatch({
          get_filtered_seq_metrics(
            years = input$year,
            platforms = input$platform,
            applications = input$application,
            option = input$option
          )
        }, error = function(e) {
          return(data.frame())
        })
        
        if (is.null(df) || !is.data.frame(df)) return(data.frame())
        
        # Apply missing filters (restored from old code)
        df <- df %>%
          filter(
            Year %in% input$year,
            Period %in% input$period,
            Platform %in% input$platform,
            name %in% input$metrics
          )
        
        if (all(input$platform == "ILLUMINA")) {
          df <- filter(df, Application %in% input$application)
        }
        
        # Fix quarter labels
        if (input$option == "No") {
          df$Period <- gsub("First Quarter", "Q1", df$Period)
          df$Period <- gsub("Second Quarter", "Q2", df$Period)
          df$Period <- gsub("Third Quarter", "Q3", df$Period)
          df$Period <- gsub("Fourth Quarter", "Q4", df$Period)
          
          df$year_period <- gsub("First Quarter", "Q1", df$year_period)
          df$year_period <- gsub("Second Quarter", "Q2", df$year_period)
          df$year_period <- gsub("Third Quarter", "Q3", df$year_period)
          df$year_period <- gsub("Fourth Quarter", "Q4", df$year_period)
        }
        
        df$year_period <- factor(df$year_period, levels = unique(df$year_period))
        df$Period <- factor(df$Period, levels = unique(df$Period))
        
        df
      })
      
      # 
      react_input <- reactive({
        df <- react_input_Main()
        
        if (input$more_plot == 'Yes') {
          if (input$more_plots_opt == 'Machine') {
            df <- filter(df, InstrumentID %in% input$instrument)
          } else if (input$more_plots_opt == 'Lab') {
            df <- filter(df, groupFolder %in% input$lab)
          } else if (input$more_plots_opt == 'Project') {
            # No extra filter needed; already filtered by App, Platform, Metric
          }
        }
        
        df
      }) |>
        bindCache(
          input$year,
          input$period,
          input$platform,
          input$metrics,
          input$application,
          input$option,
          input$more_plot,
          input$more_plots_opt,
          input$instrument,
          input$lab
        )
      ####################################################################################
      #output$debug_metric <- renderText({
      # str(react_input())
      #})
      #
      ####################################################################################
      #Define the summary for each plot
      react_input_grp <- reactive({
        df <- react_input()
        df$value <- as.numeric(gsub(',', '', df$value))
        
        # Filter to selected metrics only
        df <- df[df$name %in% input$metrics, ]
        df <- df %>%
          group_by(name, Platform, year_period, Application, Site, groupFolder, InstrumentID, Month, Year)
        split(df, df$name)  # now only for selected metrics
      })
      ####################################################################################
      
      getMetricData <- function(metric_name, platform) {
        grp_data <- react_input_grp()
        
        # Defensive check
        if (is.null(grp_data) || !metric_name %in% names(grp_data)) {
          warning(paste("Metric not found in input group:", metric_name))
          return(tibble())  
        }
        
        df <- grp_data[[metric_name]]
        df[df$Platform == toupper(platform), ]
      }
      ####################################################################################
      
      react_input_grp_App_Merge <- function(metric_name, platform) {
        req(input$site)
        req(input$metrics)
        
        df <- getMetricData(metric_name, platform)
        if (nrow(df) == 0) return(NULL)
        
        if (input$option %in% "Yes") {
          df |>
            group_by(year_period, Application, Year, Month) |>
            summarise(value_sum = sum(value, na.rm = TRUE), .groups = "drop") |>
            arrange(Year, Month)
        } else {
          df |>
            group_by(year_period, Application, Year) |>
            summarise(value_sum = sum(value, na.rm = TRUE), .groups = "drop") |>
            arrange(Year)
        }
      }
      ####################################################################################
      #
      react_input_grp_App_Split <- function(metric_name, platform) {
        req(input$site)
        df <- getMetricData(metric_name, platform)
        if (nrow(df) == 0) return(NULL)
        
        if (input$option %in% "Yes") {
          df |>
            group_by(year_period, Site, Application, Year, Month) |>
            summarise(value_sum = sum(value, na.rm = TRUE), .groups = "drop") |>
            arrange(Year, Month)
        } else {
          df |>
            group_by(year_period, Site, Application, Year) |>
            summarise(value_sum = sum(value, na.rm = TRUE), .groups = "drop") |>
            arrange(Year)
        }
      }
      ####################################################################################
      #
      react_input_grp_Instrument <- function(metric_name, platform) {
        req(input$site)
        df <- getMetricData(metric_name, platform)
        if (nrow(df) == 0) return(NULL)
        
        if (input$option %in% "Yes") {
          df |>
            group_by(year_period, InstrumentID, Year, Month) |>
            summarise(value_sum = sum(value, na.rm = TRUE), .groups = "drop") |>
            arrange(Year, Month)
        } else {
          df |>
            group_by(year_period, InstrumentID, Year) |>
            summarise(value_sum = sum(value, na.rm = TRUE), .groups = "drop") |>
            arrange(Year)
        }
      }
      ####################################################################################
      #
      react_input_grp_lab <- function(metric_name, platform) {
        req(input$site)
        df <- getMetricData(metric_name, platform)
        if (nrow(df) == 0) return(NULL)
        
        if (input$option %in% "Yes") {
          df |>
            group_by(year_period, groupFolder, Year, Month) |>
            summarise(value_sum = sum(value, na.rm = TRUE), .groups = "drop") |>
            arrange(Year, Month)
        } else {
          df |>
            group_by(year_period, groupFolder, Year) |>
            summarise(value_sum = sum(value, na.rm = TRUE), .groups = "drop") |>
            arrange(Year)
        }
      }
      ####################################################################################
      #
      react_input_grp_count <- function(metric_name, platform) {
        req(input$site)
        df <- getMetricData(metric_name, platform)
        if (nrow(df) == 0) return(NULL)

        if (input$option %in% "Yes") {
          df |>
            group_by(Application, year_period, Month, Year) |>
            summarise(AppCount = n(), .groups = "drop") |>
            arrange(Year, Month)
        } else {
          df |>
            group_by(Application, year_period, Year) |>
            summarise(AppCount = n(), .groups = "drop") |>
            arrange(Year)
        }
      }
      ####################################################################################
      ####################################################################################
      ####################################################################################
      ####################################################################################
      #Source the plotting function
      source(LandingPagePlottingCode, local = TRUE)
      ####################################################################################
      ####################################################################################
      ####################################################################################
      ####################################################################################
      
      react_input_long <- reactive({
        df <- react_input()
        req(input$metrics)
        
        df_filtered <- df %>%
          filter(name %in% input$metrics) %>%
          mutate(value = as.numeric(value)) %>%
          group_by(across(-c(value))) %>%   # group by everything except 'value'
          summarise(value = sum(value, na.rm = TRUE), .groups = "drop")  # or use distinct()
        
        if (nrow(df_filtered) == 0) {
          return(NULL)
        }
        
        df_wide <- df_filtered %>%
          pivot_wider(
            names_from = name,
            values_from = value,
            values_fill = list(value = 0)
          ) %>%
          relocate(any_of(input$metrics), .after = last_col()) # metrics selected appears far to the right in table column
        
        df_wide
      })
      
      ####################################################################################

      downloadfunc <- reactive({
        df <- react_input_long()
        req(df)  # Ensures download won't proceed unless df is ready
        
        dropColumns <- c("quarterly","monthly","yearly_quarterly","yearly_monthly","Period","year_period","month_name","quarter_label","month_label","year_quarter","month_num","year_month_date","year_month")
        df_cleaned <- df[, !(names(df) %in% dropColumns), drop = FALSE]
        df_cleaned
      })
      

      output$dataDownload <- downloadHandler(
        filename = function() { "gtQuarterlyMetrics.csv" },
        content = function(fname) {
          tryCatch({
            df_to_download <- downloadfunc()
            write.csv(df_to_download, fname, row.names = FALSE)
          }, error = function(e) {
            writeLines("Error generating CSV.", fname)
            message("Download error: ", e$message)
          })
        },
        contentType = "text/csv"
      )
      
      ####################################################################################
    }
  )
}