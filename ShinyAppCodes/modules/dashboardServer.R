module2_Server <- function(id, selectedGroup, authenticated_group, dashboard_ready) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    ###################################################################################
    # --------App Refresh Timestamp Block--------  
    # Displays the last time the selected app was refreshed using last_updated_app().
    output$last_update <- renderText({
      paste("Last app refreshed:", last_updated_app())
    })
    ###################################################################################
    ####################################################################################
    #-------- Admin Check Helper--------
    #Reactive function that returns TRUE if the authenticated group is "Admin".
    is_admin <- reactive({
      req(authenticated_group())
      authenticated_group() == "Admin"
    })
    ###################################################################################
    #-------- GenomeTech Group Checker--------
    #Returns TRUE if the authenticated group starts with "GenomeTechnologies".
    is_genome_tech <- reactive({
      req(authenticated_group())
      startsWith(authenticated_group(), "GenomeTechnologies")
    })
    ###################################################################################
    #-------- Session Cleanup Block--------
    #Creates a per-user environment to store session data, and ensures memory is cleared when the session ends.
    # 1. Initialize session-local environment
    session_env <- new.env(parent = emptyenv())
    session_env$session_data_cache <- reactiveVal(NULL)
    session_env$cache_time <- reactiveVal(Sys.time())
    session_env$temp_id_list <- NULL
    
    # 2. Session cleanup when browser closes or refreshes
    session$onSessionEnded(function() {
      write_log(log_Dashboard, "APP_SESSION_ENDED", "Session ended. Cleaning up memory." )
      rm(list = ls(envir = session_env), envir = session_env)
      gc(verbose = TRUE)
    })
    
    # 3. Handle logout UI reset and data cleanup
    observeEvent(session$userData$logout_trigger, {
      cat("Cleaning up session reactivity...\n")
      
      reset_inputs <- c("var1", "var2", "var3", "var5", "var6")
      for (input_id in reset_inputs) {
        updatePickerInput(session, input_id, choices = character(0), selected = NULL)
      }
      updateSelectInput(session, "app", choices = character(0), selected = NULL)
      
      if (exists("qc_metrics_fetched")) {
        try(qc_metrics_fetched(FALSE), silent = TRUE)
      }
    })
    ####################################################################################
    #-------- Dynamic Tabs Block--------
    #Renders admin-specific or user-specific tabs depending on authentication, and builds main dashboard interface including Plot, SpeciesAlignment, Summary, and Downloads.
    
    output$dynamic_tabs <- renderUI({
      req(authenticated_group())
      
      if (is_admin()) {
        ## ========== ADMIN PAGE ==========
        div(
          id = ns("admin_page"),
          h2("Admin Page"),
          br(),
          div(style = "display: flex; gap: 10px; flex-wrap: wrap;",
              actionButton(ns("view_login_logs"), "Login Logs"),
              actionButton(ns("view_seqmet_log"), "View SeqMet Log"),
              actionButton(ns("view_dashboard_log"), "View Dashboard Log"),
              actionButton(ns("view_wiki_log"), "View Wiki Log"),
              actionButton(ns("update_group_profile"), "Update Group Profile")
          ),
          br(), br(),
          
          uiOutput(ns("admin_log_ui"))
          
        )
      } else {
        ## ========== NORMAL USER PAGE ==========
        # Start building tab list
        tabs <- list(
          tabPanel("Plot",
                   uiOutput(ns("plot_ui")),
                   tags$head(tags$style(HTML(".center-warning {height: 80vh; display: flex; justify-content: center; align-items: center;
                                    color: red; font-size: 20px; font-style: italic; text-align: center; padding: 0 20px;}")))
          ),
          tabPanel(title = "SpeciesAlignment", value = "species_tab",
                   actionButton(ns("plot_species"), "Show Plot"),
                   actionButton(ns("table_species"), "Show Table"),
                   downloadButton(ns("download_species"), "Download Table"),
                   br(), br(),
                   div(style = 'width: 100%; height: 600px; overflow-x: auto; overflow-y: auto;',
                       uiOutput(ns("species_view_ui")))
          ),
          tabPanel("DownloadTable",
                   downloadButton(ns('download'), "Download below metrics",
                                  style = "color: #fff; background-color: green; border-color: Black;"),
                   br(), br(),
                   fluidRow(column(12, div(style = 'overflow-x: auto; overflow-y: auto; height:600px;',
                                           DT::dataTableOutput(ns('dataDownload')))))
          ),
          tabPanel("GeneralSummary",
                   shinycssloaders::withSpinner(
                     div(style = "max-height: 400px; overflow-y: auto;",
                         uiOutput(ns("summary_warning_ui")),
                         verbatimTextOutput(ns("summary")))
                   )
          )
        )
        tabs[[length(tabs) + 1]] <- tabPanel("Update Group Emails",
                                             br(),
                                             uiOutput(ns("user_group_email_ui"))
        )
        
        # Conditionally add GroupSummary tab for GenomeTechnologies
        if (is_genome_tech()) {
          tabs[[length(tabs) + 1]] <- tabPanel("GroupSummary",
                                               shinycssloaders::withSpinner(
                                                 div(style = "max-height: 400px; overflow-y: auto;",
                                                     uiOutput(ns("describe_warning_ui")),  # new warning UI
                                                     verbatimTextOutput(ns("describe")))
                                               )
          )
        }
        
        # Conditionally add SpeciesAlignment tab for GenomeTechnologies
        
        # if (is_genome_tech()) {
        #   tabs[[length(tabs) + 1]] <- tabPanel(title = "SpeciesAlignment", value = "species_tab",
        #                                        actionButton(ns("plot_species"), "Show Plot"),
        #                                        actionButton(ns("table_species"), "Show Table"),
        #                                        downloadButton(ns("download_species"), "Download Table"),
        #                                        br(), br(),
        #                                        div(style = 'width: 100%; height: 600px; overflow-x: auto; overflow-y: auto;',
        #                                            uiOutput(ns("species_view_ui")))
        #   )
        # }
        # Show SpeciesAlignment for all non-admins
        
        
        # Render final tabset
        div(
          id = ns("normal_tabs"),
          style = "position: relative;",
          
          # Live Refresh Button aligned to top-right
          tags$div(
            style = "position: absolute; top: -4px; right: 0; margin: 5px; display: flex; align-items: center; gap: 10px; z-index: 1000;",
            
            uiOutput(ns("last_refresh_text")),
            
            actionButton(
              ns("refresh_app_data"),
              label = tagList(
                tags$i(class = "fas fa-sync-alt", style = "margin-right: 6px;"),
                span("Refetch live data here")
              ),
              style = "background-color: green; color: white; font-weight: bold; font-size: 14px; border: 2px solid #00cc66; border-radius: 6px; box-shadow: none; padding: 4px 10px;"
            )
          ),
          
          # Tabs panel
          do.call(tabsetPanel, c(id = ns("main_tabs"), type = "tabs", tabs))
        )
      }
    })
    ####################################################################################
    # --------Hide/Show "Flo" Plot Option Block--------
    #Dynamically updates the available plot types based on group role; hides “Flo” option for non-GenomeTech users.
    
    observe({
      req(authenticated_group(), selectedGroup())  # Ensure both are available
      
      # Hide “Flo” option in “Plot type” for non-GT users
      shinyjs::delay(500, {
        if (!is_genome_tech()) {
          updateRadioButtons(session, "plt",
                             choices = c("Box", "Bar"),  # No “Flo”
                             selected = "Box", inline = TRUE)
        } else {
          # GT users still see “Box”, “Bar”, and “Flo”
          updateRadioButtons(session, "plt",
                             choices = c("Box", "Bar", "Flo"),
                             selected = "Box", inline = TRUE)
        }
      })
    })
    ####################################################################################
    #-------- App Menu Filter (User Group–Specific) Block--------
    #Filters app list to show only apps with Investigator_Folder matching user's group, except for GenomeTech.
    #Defines qc_metrics_fetched to track whether app metrics were already fetched.
    
    observe({
      req(authenticated_group())
      if (!is_admin()) {
        qc_metrics_fetched <- reactiveVal(FALSE)
        # ─────────────────────────────────────────────────────────────
        # Refresh App Data: bump `input$app` via sendInputMessage
        # ─────────────────────────────────────────────────────────────
        last_refresh_time <- reactiveVal(NULL)
        app_refresh_counter <- reactiveVal(0)
        
        observeEvent(input$refresh_app_data, {
          req(input$app)
          
          # 1. Reconnect to DuckDB (live read)
          try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE)
          con <<- DBI::dbConnect(duckdb::duckdb(), dbdir = path_to_duckdb, read_only = TRUE)
          
          # 2. Forcefully trigger downstream observers using invalidateReactive
          app_refresh_counter(app_refresh_counter() + 1)
          
          # 3. Stamp refresh time
          last_refresh_time(Sys.time())
          
          # 4. Notify user
          showNotification(
            paste0("🔄 Data refreshed at ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
            type     = "message",
            duration = 2
          )
        })
        
        ###################################################################################
        #-------- Last Refresh Timestamp Display Block --------
        #Displays the timestamp of the last successful app data refresh.
        #If no refresh has occurred yet, shows a placeholder message instead.
        #This output is typically shown beside the "Refresh" button to inform users of data freshness.
        
        output$last_refresh_text <- renderUI({
          time <- last_refresh_time()
          text <- if (!is.null(time)) {
            paste("🕒 Last fetched on", format(time, "%Y-%m-%d at %H:%M:%S"))
          } else {
            "🕒 No new data refetched yet"
          }
          tags$div(style = "font-size: 16px; font-weight: bold; color: #333;", text)
        })
        
        
        ###################################################################################
        # -------- App Access Validator Block--------
        #Returns a list of valid apps based on user group (all apps for GenomeTech, filtered for others).
        #Also sets qc_metrics_fetched(TRUE) once validation completes.
        
        valid_apps_reactive <- reactive({
          app_refresh_counter()
          req(selectedGroup(), authenticated_group())
          group <- trimws(as.character(selectedGroup()))
          
          apps_df <- if (is_genome_tech()) {
            DBI::dbGetQuery(con, "SELECT DISTINCT Application, Source FROM qc_app_index")
          } else {
            DBI::dbGetQuery(con,
                            "SELECT DISTINCT Application, Source FROM qc_app_index WHERE Investigator_Folder = ?",
                            params = list(group)
            )
          }
          
          # Store app-to-source mapping (used in downstream queries)
          session_env$app_source_table <- setNames(apps_df$Source, apps_df$Application)
          
          if (!qc_metrics_fetched()) {
            qc_metrics_fetched(TRUE)
          }
          
          apps <- unique(trimws(apps_df$Application))
          if (length(apps) == 0) return(NULL)
          return(sort(apps))
        })
        
        ###################################################################################
        #-------- App Dropdown Manager Block--------
        #Watches for changes in valid apps and updates the "Select Application" dropdown accordingly.
        #Ensures a default app is selected if none or invalid, and avoids unnecessary updates using prev_apps().
        
        # Track previously known valid app choices
        prev_apps <- reactiveVal(character(0))
        
        observe({
          #invalidateLater(1000, session)
          req(valid_apps_reactive())
          # Wait until Login is bypassed
          tab <- isolate(input$main_tab)
          if (!is.null(tab) && tab != "Login to QC metrics dashboard") return()
          
          req(valid_apps_reactive())
          
          all_apps <- sort(valid_apps_reactive())
          previous_apps <- prev_apps()
          
          # Group apps
          long_read_apps  <- c("ONT", "PacBio")
          short_read_apps <- setdiff(all_apps, long_read_apps)
          
          grouped_apps <- list(
            "📏 Short Read Apps" = sort(short_read_apps),
            "🧬 Long Read Apps"  = sort(intersect(all_apps, long_read_apps))
          )
          
          # Default selection logic
          selected_app <- if (!is.null(input$app) && input$app %in% all_apps) {
            input$app
          } else if (length(all_apps) > 0) {
            all_apps[1]
          } else {
            NULL
          }
          
          # Only update if changed
          if (!identical(all_apps, previous_apps) || is.null(input$app) || !(input$app %in% all_apps)) {
            prev_apps(all_apps)
            
            freezeReactiveValue(input, "app")
            updateSelectInput(
              session, "app",
              choices = grouped_apps,
              selected = selected_app
            )
          }
        })
        
        
        ###################################################################################
        # -------- Session Data Loader Block--------
        #Fetches app data from DuckDB and filters by group (if not GenomeTech); stores it in session-local cache to avoid global memory bloat.
        #Also computes available years and updates the release_year dropdown with appropriate default.
        
        
        app_loading <- reactiveVal(FALSE)
        app_ready   <- reactiveVal(FALSE)
        
        observeEvent(list(input$app, app_refresh_counter()), {
          req(input$app)
          app_loading(TRUE)
          app_ready(FALSE)
          
          # Route query to correct table
          source_table <- session_env$app_source_table[[input$app]]
          quoted_table <- DBI::dbQuoteIdentifier(con, source_table)
          
          sql <- sprintf("
    SELECT DISTINCT strftime('%%Y', try_cast(Release_Date AS DATE)) AS yr
    FROM %s
    WHERE Application = ?
      AND ProjStatus = 'Delivered'
      AND Release_Date IS NOT NULL
      AND TRIM(Release_Date) != ''
      AND LOWER(TRIM(Release_Date)) != 'null'
  ", quoted_table)
          
          if (!is_genome_tech()) {
            group <- trimws(as.character(selectedGroup()))
            sql <- paste0(sql, " AND Investigator_Folder = ?")
            params <- list(input$app, group)
          } else {
            params <- list(input$app)
          }
          
          sql <- paste0(sql, " ORDER BY yr DESC")
          
          delivered_years <- tryCatch({
            DBI::dbGetQuery(con, sql, params = params)$yr
          }, error = function(e) {
            write_log(log_Dashboard, "GET_YEARS_ERROR", paste0("Year fetch failed: ", e$message))
            character(0)
          })
          
          delivered_years <- na.omit(delivered_years)
          
          if (!is_genome_tech()) {
            years  <- delivered_years
            labels <- delivered_years
          } else {
            years  <- c(delivered_years, "NULL")
            labels <- c(delivered_years, "Undelivered / Missing Date")
          }
          
          default_year <- years[1]
          
          freezeReactiveValue(input, "release_year")
          updateSelectInput(
            session, "release_year",
            choices  = setNames(years, labels),
            selected = default_year
          )
          
          app_loading(FALSE)
          app_ready(TRUE)
        })
         #     
        
        ###############################################################################
        #-------- Filtered Data by Year Block--------
        #Filters session data based on release_year dropdown: either includes undelivered or only delivered projects from a specific year.
        #Stores result in filtered_by_year_cache() for downstream dropdowns and plots.
        
        # ─────────────────────────────────────────────────────────────
        # Filtered‐by‐Year + App cache via DuckDB (offload to SQL)
        # ─────────────────────────────────────────────────────────────
        filtered_by_year_cache <- reactiveVal(NULL)
        
        observeEvent(list(input$app, input$release_year), {
          req(input$app, input$release_year)
          
          # Year filtering
          if (input$release_year == "NULL") {
            where_year <- "AND (ProjStatus != 'Delivered' OR Release_Date IS NULL OR TRIM(Release_Date) = '' OR LOWER(TRIM(Release_Date)) = 'null')"
            params     <- list(input$app)
          } else {
            where_year <- "
            AND strftime('%Y', try_cast(Release_Date AS DATE)) = ? AND ProjStatus = 'Delivered'"
            params     <- list(input$app, input$release_year)
          }
          
          # Identify source table based on app
          source_table <- session_env$app_source_table[[input$app]]
          quoted_table <- DBI::dbQuoteIdentifier(con, source_table)
          
          # Query
          sql <- sprintf("
    SELECT *
      FROM %s
     WHERE Application = ?
       %s
  ", quoted_table, where_year)
          
          df_out <- tryCatch(
            DBI::dbGetQuery(con, sql, params = params),
            error = function(e) {
              write_log(log_Dashboard, "DB_FILTER_YEAR_ERROR",
                        paste0("Year filter query failed: ", e$message))
              NULL
            }
          )
          
          # Cleanup Release_Date and unify sample column
          if (!is.null(df_out) && nrow(df_out) > 0) {
            df_out$Release_Date[trimws(df_out$Release_Date) == "NULL"] <- NA
            
            if (!"Sample_Name" %in% names(df_out) && "Sample_ID" %in% names(df_out)) {
              df_out$Sample_Name <- df_out$Sample_ID
            }
          }
          
          filtered_by_year_cache(df_out)
          
          if (is.null(df_out) || nrow(df_out) == 0) {
            showNotification(
              paste("No projects available for year:", input$release_year),
              type     = "warning",
              duration = 4
            )
          }
        })
        
        
        ###########################################################P#####################################
        #-------- Populate Lab Choices Block--------
        #Normalizes and updates the Lab (var1) dropdown based on filtered data by year.
        #Ensures all empty, NA, or "NULL"-like labs are shown as "NULL" option.
        
        
        # ─────────────────────────────────────────────────────────────
        # Populate Lab (var1) via SQL push-down, with “Missing Lab” last
        # ─────────────────────────────────────────────────────────────
       
        observeEvent(list(input$app, input$release_year), {
          req(input$app, input$release_year, selectedGroup(), authenticated_group())
          
          # 1) Detect correct source table from app
          source_table <- session_env$app_source_table[[input$app]]
          quoted_table <- DBI::dbQuoteIdentifier(con, source_table)
          
          # 2) Build year predicate and parameter list
          if (input$release_year == "NULL") {
            where_year <- "
      AND (
        ProjStatus != 'Delivered'
        OR Release_Date IS NULL
        OR TRIM(Release_Date) = ''
        OR LOWER(TRIM(Release_Date)) = 'null'
      )"
            params <- list(input$app)
          } else {
            where_year <- "
              AND strftime('%Y', try_cast(Release_Date AS DATE)) = ?
              AND ProjStatus = 'Delivered'
            "
            params     <- list(input$app, input$release_year)
          }
          
          # 3) Construct SQL for dynamic source
          sql <- sprintf(
            "SELECT DISTINCT Investigator_Folder
     FROM %s
     WHERE Application = ?
       %s
     ORDER BY
       CASE
         WHEN Investigator_Folder IS NULL
           OR TRIM(Investigator_Folder) = ''
           OR LOWER(TRIM(Investigator_Folder)) IN ('null','na')
         THEN 1 ELSE 0 END,
       Investigator_Folder",
            quoted_table, where_year
          )
          
          raw <- tryCatch(
            DBI::dbGetQuery(con, sql, params = params)$Investigator_Folder,
            error = function(e) {
              write_log(log_Dashboard, "DROPDOWN_ERROR", paste0("Lab query failed: ", e$message))
              character(0)
            }
          )
          
          # 4) Normalize blanks/NA → "Missing Lab"
          labs <- ifelse(
            is.na(raw) |
              trimws(raw) == "" |
              tolower(trimws(raw)) %in% c("null", "na"),
            "Missing Lab",
            trimws(raw)
          )
          
          # 5) Restrict for non-GT users to only their lab
          if (!is_genome_tech()) {
            labs <- trimws(as.character(selectedGroup()))
          } else {
            labs <- unique(labs)
          }
          
          # 6) Update Lab (var1) dropdown
          freezeReactiveValue(input, "var1")
          updatePickerInput(session, "var1",
                            choices  = labs,
                            selected = labs[1])
          
          # 7) If search_lab exists, override selection after slight delay
          if (!is.null(session_env$search_lab) && session_env$search_lab %in% labs) {
            lab_to_select <- session_env$search_lab
            session_env$search_lab <- NULL
            
            later::later(function() {
              if (!is.null(session) && !is.null(lab_to_select)) {
                session$sendInputMessage("var1", list(value = lab_to_select))
              }
            }, delay = 0.2)
          }
          
        })
        
        ###########################################################P#####################################
        #-------- Preserve Lab Selection on Refresh Block--------
        #When "Refresh App Data" is clicked, it re-applies the selected Lab (var1) after a short delay to prevent reset.
        
        # observeEvent(input$refresh_app_data, {
        #   # Capture the input$var1 safely
        #   selected_lab <- isolate(input$var1)
        #   
        #   # Delay to ensure updatePickerInput finishes
        #   later::later(function() {
        #     if (!is.null(session) && !is.null(selected_lab)) {
        #       session$sendInputMessage("var1", list(value = selected_lab))
        #     }
        #   }, delay = 0.2)
        # })
        
        ###########################################################P#####################################
        #-------- Update Project ID on Lab Selection Block --------
        #When a Lab (var1) is selected, filters data to update Project_ID dropdown (var2) and toggles visibility if "NULL" Lab is chosen.
        # ─────────────────────────────────────────────────────────────
        # Populate Project ID (var2) via DuckDB pushdown
        # ─────────────────────────────────────────────────────────────

        observeEvent(input$var1, {
          req(input$app, input$release_year, input$var1, selectedGroup(), authenticated_group())
          
          # 1) Detect correct source table for selected app
          source_table <- session_env$app_source_table[[input$app]]
          quoted_table <- DBI::dbQuoteIdentifier(con, source_table)
          
          # 2) Build year clause
          if (input$release_year == "NULL") {
            year_sql <- "AND (ProjStatus != 'Delivered' OR Release_Date IS NULL OR TRIM(Release_Date) = '' OR LOWER(TRIM(Release_Date)) = 'null')"
            params <- list(input$app)
          } else {
            year_sql <- "
              AND strftime('%Y', try_cast(Release_Date AS DATE)) = ?
              AND ProjStatus = 'Delivered'
            "
            params <- list(input$app, input$release_year)
          }
          
          # 3) Lab clause
          if ("Missing Lab" %in% input$var1) {
            lab_sql <- "AND (Investigator_Folder IS NULL OR TRIM(Investigator_Folder) = '' OR LOWER(TRIM(Investigator_Folder)) IN ('null', 'na'))"
          } else {
            ph <- paste(rep("?", length(input$var1)), collapse = ",")
            lab_sql <- sprintf("AND Investigator_Folder IN (%s)", ph)
            params <- c(params, as.list(input$var1))
          }
          
          # 4) Final SQL using dynamic table
          sql <- sprintf(
            "SELECT DISTINCT Project_ID
     FROM %s
     WHERE Application = ?
       %s
       %s
     ORDER BY
       CASE
         WHEN Project_ID IS NULL
           OR TRIM(Project_ID) = ''
           OR LOWER(TRIM(Project_ID)) IN ('null','na')
         THEN 1 ELSE 0 END,
       Project_ID",
            quoted_table, year_sql, lab_sql
          )
          
          # 5) Execute query safely
          raw_proj <- tryCatch(
            DBI::dbGetQuery(con, sql, params = params)$Project_ID,
            error = function(e) {
              write_log(log_Dashboard, "DROPDOWN_ERROR", paste0("Project_ID query failed: ", e$message))
              character(0)
            }
          )
          
          # 6) Normalize to "Missing Project"
          projs <- ifelse(
            is.na(raw_proj) | trimws(raw_proj) == "" | tolower(trimws(raw_proj)) %in% c("null", "na"),
            "Missing Project",
            trimws(raw_proj)
          )
          projs <- unique(projs)
          projs <- c(setdiff(projs, "Missing Project"), intersect(projs, "Missing Project"))  # reorder
          
          # 7) Default: select all if multiple labs; otherwise first
          default_proj <- if (length(projs) > 1) projs else projs[1]
          
          # 8) Update UI
          freezeReactiveValue(input, "var2")
          updatePickerInput(session, "var2",
                            choices = projs,
                            selected = default_proj)
          
          # 9) Toggle visibility
          if ("Missing Lab" %in% input$var1) {
            shinyjs::enable("var2_parent"); shinyjs::show("var2")
          } else {
            shinyjs::disable("var2_parent"); shinyjs::hide("var2")
          }
        })
        
        ###########################################################P#####################################
        #-------- Update Project Run Type on Lab/Project Change Block --------
        #Updates the Project Run Type (var3) dropdown based on selected Lab (var1) and, if applicable, Project ID (var2) when "NULL" Lab is chosen.
        # ─────────────────────────────────────────────────────────────
        # Populate Project Run Type (var3) via DuckDB pushdown
        # ─────────────────────────────────────────────────────────────
       observeEvent(input$var2, {
          req(input$app, input$release_year, input$var1, input$var2)
          
          # 1) Identify the correct table
          source_table <- session_env$app_source_table[[input$app]]
          quoted_table <- DBI::dbQuoteIdentifier(con, source_table)
          
          # 2) Year clause
          if (input$release_year == "NULL") {
            year_sql <- "AND (ProjStatus != 'Delivered' OR Release_Date IS NULL OR TRIM(Release_Date) = '' OR LOWER(TRIM(Release_Date)) = 'null')"
            params <- list(input$app)
          } else {
            year_sql <- "
              AND strftime('%Y', try_cast(Release_Date AS DATE)) = ?
              AND ProjStatus = 'Delivered'
            "
            params <- list(input$app, input$release_year)
          }
          
          # 3) Lab clause
          if ("Missing Lab" %in% input$var1) {
            lab_sql <- "AND (Investigator_Folder IS NULL OR TRIM(Investigator_Folder) = '' OR LOWER(TRIM(Investigator_Folder)) IN ('null','na'))"
          } else if (length(input$var1) == 1) {
            lab_sql <- "AND Investigator_Folder = ?"
            params <- c(params, input$var1)
          } else {
            ph <- paste(rep("?", length(input$var1)), collapse = ",")
            lab_sql <- sprintf("AND Investigator_Folder IN (%s)", ph)
            params <- c(params, as.list(input$var1))
          }
          
          
          # 4) Project clause
          if ("Missing Project" %in% input$var2) {
            proj_sql <- "AND (Project_ID IS NULL OR TRIM(Project_ID) = '' OR LOWER(TRIM(Project_ID)) IN ('null','na'))"
          } else {
            if (length(input$var2) == 1) {
              proj_sql <- "AND Project_ID = ?"
              params <- c(params, input$var2)
            } else {
              ph <- paste(rep("?", length(input$var2)), collapse = ",")
              proj_sql <- sprintf("AND Project_ID IN (%s)", ph)
              params <- c(params, as.list(input$var2))
            }
          }
          
          # 5) Final query for run types
          sql <- sprintf(
            "SELECT DISTINCT Project_run_type
     FROM %s
     WHERE Application = ?
       %s %s %s
     ORDER BY
       CASE
         WHEN Project_run_type IS NULL
           OR TRIM(Project_run_type) = ''
           OR LOWER(TRIM(Project_run_type)) IN ('null','na')
         THEN 1 ELSE 0 END,
       Project_run_type",
            quoted_table, year_sql, lab_sql, proj_sql
          )
          
          run_raw <- tryCatch(
            DBI::dbGetQuery(con, sql, params = params)$Project_run_type,
            error = function(e) {
              write_log(log_Dashboard, "DROPDOWN_ERROR", paste0("Project_run_type query failed: ", e$message))
              character(0)
            }
          )
          
          # 6) Normalize
          runs <- ifelse(
            is.na(run_raw) | trimws(run_raw) == "" | tolower(trimws(run_raw)) %in% c("null", "na"),
            "Missing Run",
            trimws(run_raw)
          )
          runs <- unique(runs)
          runs <- c(setdiff(runs, "Missing Run"), intersect(runs, "Missing Run"))
          
          # 7) Push to UI
          # 7) Push to UI
          freezeReactiveValue(input, "var3")
          updatePickerInput(session, "var3",
                            choices  = runs,
                            selected = runs[1])
          
          # 8) If search_run exists, override selection after slight delay
          if (!is.null(session_env$search_run) && session_env$search_run %in% runs) {
            run_to_select <- session_env$search_run
            session_env$search_run <- NULL
            
            later::later(function() {
              if (!is.null(session) && !is.null(run_to_select)) {
                session$sendInputMessage("var3", list(value = run_to_select))
              }
            }, delay = 0.2)
          }
          
        })
        
        
        ###########################################################P#####################################
        #-------- Update Metric and Species Dropdowns on Run Type Change Block --------
        #Updates the Metric (var5) and Species (var6) menus based on filtered data for the selected Project Run Type, excluding domain/species and Flo-only metrics.
        # ─────────────────────────────────────────────────────────────
        # Populate Metrics (var5) & Species (var6) via DuckDB pushdown
        # ─────────────────────────────────────────────────────────────

        observeEvent(
          list(input$app, input$release_year, input$var1, input$var2, input$var3),
          {
            req(input$app, input$release_year, input$var1, input$var2, input$var3)
            
            # 1) Identify source table
            source_table <- session_env$app_source_table[[input$app]]
            quoted_table <- DBI::dbQuoteIdentifier(con, source_table)
            
            # 2) Year clause
            if (input$release_year == "NULL") {
              year_sql <- "AND (ProjStatus != 'Delivered' OR Release_Date IS NULL OR TRIM(Release_Date) = '' OR LOWER(TRIM(Release_Date)) = 'null')"
              params <- list(input$app)
            } else {
              year_sql <- "
              AND strftime('%Y', try_cast(Release_Date AS DATE)) = ?
              AND ProjStatus = 'Delivered'
            "
              params <- list(input$app, input$release_year)
            }
            
            # 3) Lab clause
            if ("Missing Lab" %in% input$var1) {
              lab_sql <- "AND (Investigator_Folder IS NULL OR TRIM(Investigator_Folder) = '' OR LOWER(TRIM(Investigator_Folder)) IN ('null','na'))"
            } else {
              ph <- paste(rep("?", length(input$var1)), collapse = ",")
              lab_sql <- sprintf("AND Investigator_Folder IN (%s)", ph)
              params <- c(params, as.list(input$var1))
            }
            
            # 4) Project clause
            if ("Missing Project" %in% input$var2) {
              proj_sql <- "AND (Project_ID IS NULL OR TRIM(Project_ID) = '' OR LOWER(TRIM(Project_ID)) IN ('null','na'))"
            } else {
              ph <- paste(rep("?", length(input$var2)), collapse = ",")
              proj_sql <- sprintf("AND Project_ID IN (%s)", ph)
              params <- c(params, as.list(input$var2))
            }
            
            # 5) Run clause
            if ("Missing Run" %in% input$var3) {
              run_sql <- "AND (Project_run_type IS NULL OR TRIM(Project_run_type) = '' OR LOWER(TRIM(Project_run_type)) IN ('null','na'))"
            } else {
              ph <- paste(rep("?", length(input$var3)), collapse = ",")
              run_sql <- sprintf("AND Project_run_type IN (%s)", ph)
              params <- c(params, as.list(input$var3))
            }
            
            # 6) Construct WHERE clause for both queries
            base_where <- paste(
              "FROM", quoted_table, "WHERE Application = ?",
              year_sql, lab_sql, proj_sql, run_sql
            )
            
            # ── Metrics ────────────────────────────────────────────────
            sql_metrics <- paste0("SELECT DISTINCT name ", base_where, " ORDER BY name")
            metrics_raw <- tryCatch(
              DBI::dbGetQuery(con, sql_metrics, params = params)$name,
              error = function(e) {
                write_log(log_Dashboard, "DROPDOWN_ERROR", paste0("Metrics query failed: ", e$message))
                character(0)
              }
            )
            
            excl <- paste0("^(", paste(c(paste0("Domain", 1:4), paste0("Species", 1:5)), collapse = "|"), ")$")
            filtered_metrics <- setdiff(metrics_raw[!grepl(excl, metrics_raw)], flo_only_metrics)
            
            # Split into standard vs PCT
            pct_metrics <- sort(filtered_metrics[grepl("PCT", filtered_metrics)])
            std_metrics <- sort(filtered_metrics[!grepl("PCT", filtered_metrics)])
            groups      <- list(
              "📊 Standard Metrics" = std_metrics,
              "📉 PCT Metrics"      = pct_metrics
            )
            
            all_metrics <- unique(unlist(groups))
            
            # 1. Prefer both "Reads_Total" and "Reads_Filtered" if present
            preferred <- intersect(c("Reads_Total", "Reads_Filtered"), all_metrics)
            
            if (length(preferred) >= 2) {
              default_metrics <- preferred[1:2]
            } else if (length(preferred) == 1) {
              default_metrics <- preferred
            } else if (length(all_metrics) >= 1) {
              default_metrics <- all_metrics[1]
            } else {
              default_metrics <- NULL
            }
            
            
            freezeReactiveValue(input, "var5")
            updatePickerInput(
              session, "var5",
              choices  = groups,
              selected = if (length(default_metrics)>0) default_metrics else NULL,
              options  = list(`live-search`=TRUE, size=10)
            )
            
            # ── Species ────────────────────────────────────────────────
            sql_species <- paste0("SELECT DISTINCT Species ", base_where, " ORDER BY Species")
            species_raw <- tryCatch(
              DBI::dbGetQuery(con, sql_species, params = params)$Species,
              error = function(e) {
                write_log(log_Dashboard, "DROPDOWN_ERROR", paste0("Species query failed: ", e$message))
                character(0)
              }
            )
            
            species_choices <- sort(na.omit(species_raw))
            
            freezeReactiveValue(input, "var6")
            updatePickerInput(
              session, "var6",
              choices = species_choices,
              selected = species_choices
            )
          }
        )
        
        ###########################################################P#####################################
        
 
        ###########################################################P#####################################
        # ───────────────────────────────────────────────────────────────
        # Flo Plot Dropdown Reactivity
        # ───────────────────────────────────────────────────────────────
        ###########################################################
        #-------- Flo Flowcell Population Block --------
        #When Flo plot is selected and Lab input stabilizes, update Flowcell options for Flo plot (flo_flowcell) based on filtered data.
        #Preserves previously selected flowcell if still valid.
        
        # ─────────────────────────────────────────────────────────────
        # Toggle Flo vs Standard inputs
        # ─────────────────────────────────────────────────────────────
        observe({
          req(input$plt)
          if (input$plt == "Flo") {
            shinyjs::hide("var3"); shinyjs::hide("var5"); shinyjs::hide("var6")
            shinyjs::show("flo_flowcell"); shinyjs::show("flo_runs"); shinyjs::show("flo_metric")
          } else {
            shinyjs::show("var3"); shinyjs::show("var5"); shinyjs::show("var6")
            shinyjs::hide("flo_flowcell"); shinyjs::hide("flo_runs"); shinyjs::hide("flo_metric")
          }
        })
        
        # ─────────────────────────────────────────────────────────────
        # 1) Populate Flo Flowcell
        # ─────────────────────────────────────────────────────────────
        observeEvent(
          list(input$plt, input$app, input$release_year, input$var1),
          {
            req(input$plt == "Flo", input$app, input$release_year, input$var1)
            
            # Identify correct source table
            source_table <- session_env$app_source_table[[input$app]]
            quoted_table <- DBI::dbQuoteIdentifier(con, source_table)
            
            # --- Year clause + params ---
            if (input$release_year == "NULL") {
              year_sql    <- "AND (ProjStatus != 'Delivered' OR Release_Date IS NULL OR TRIM(Release_Date) = '' OR LOWER(TRIM(Release_Date)) = 'null')"
              year_params <- list(input$app)
            } else {
              year_sql    <- "
              AND strftime('%Y', try_cast(Release_Date AS DATE)) = ?
              AND ProjStatus = 'Delivered'
            "
              year_params <- list(input$app, input$release_year)
            }
            
            # --- Lab clause + params, handling "Missing Lab" specially ---
            if ("Missing Lab" %in% input$var1) {
              real_labs  <- setdiff(input$var1, "Missing Lab")
              clauses    <- c("(Investigator_Folder IS NULL OR TRIM(Investigator_Folder) = '' OR LOWER(TRIM(Investigator_Folder)) IN ('null','na'))")
              lab_params <- list()
              
              if (length(real_labs) > 0) {
                ph <- paste(rep("?", length(real_labs)), collapse = ",")
                clauses <- c(clauses, sprintf("Investigator_Folder IN (%s)", ph))
                lab_params <- as.list(real_labs)
              }
              lab_sql <- paste0("AND (", paste(clauses, collapse = " OR "), ")")
            } else {
              ph <- paste(rep("?", length(input$var1)), collapse = ",")
              lab_sql <- sprintf("AND Investigator_Folder IN (%s)", ph)
              lab_params <- as.list(input$var1)
            }
            
            # --- Final SQL and params ---
            sql <- sprintf("
      SELECT DISTINCT FlowcellID
        FROM %s
       WHERE Application = ?
         %s
         %s
       ORDER BY FlowcellID
    ", quoted_table, year_sql, lab_sql)
            params <- c(year_params, lab_params)
            
            # --- Fetch from DuckDB ---
            flow_opts <- tryCatch(
              DBI::dbGetQuery(con, sql, params = params)$FlowcellID,
              error = function(e) {
                write_log(log_Dashboard, "DROPDOWN_ERROR", paste0("Flo Flowcell query failed: ", e$message))
                character(0)
              }
            )
            flow_opts <- sort(unique(na.omit(flow_opts)))
            
            # --- Preserve and update selection ---
            prev <- isolate(input$flo_flowcell)
            sel  <- intersect(prev, flow_opts)
            sel  <- if (length(sel) > 0) sel else flow_opts
            
            shinyjs::delay(100, {
              freezeReactiveValue(input, "flo_flowcell")
              updatePickerInput(
                session, "flo_flowcell",
                choices  = flow_opts,
                selected = sel
              )
            })
          }
        )
        
        # ─────────────────────────────────────────────────────────────
        # 2) Populate Flo Runs (with Missing‐Lab support)
        # ─────────────────────────────────────────────────────────────
        observeEvent(
          list(input$plt, input$flo_flowcell, input$var1),
          {
            req(input$plt == "Flo", input$flo_flowcell, input$var1, input$app)
            
            # Identify correct source table
            source_table <- session_env$app_source_table[[input$app]]
            quoted_table <- DBI::dbQuoteIdentifier(con, source_table)
            
            # --- Year clause + params ---
            if (input$release_year == "NULL") {
              year_sql    <- "AND (ProjStatus != 'Delivered' OR Release_Date IS NULL OR TRIM(Release_Date) = '' OR LOWER(TRIM(Release_Date)) = 'null')"
              year_params <- list(input$app)
            } else {
              year_sql    <- "
              AND strftime('%Y', try_cast(Release_Date AS DATE)) = ?
              AND ProjStatus = 'Delivered'
            "
              year_params <- list(input$app, input$release_year)
            }
            
            # --- Lab clause + params ---
            if ("Missing Lab" %in% input$var1) {
              real_labs <- setdiff(input$var1, "Missing Lab")
              clauses   <- c("(Investigator_Folder IS NULL OR TRIM(Investigator_Folder) = '' OR LOWER(TRIM(Investigator_Folder)) IN ('null','na'))")
              lab_params <- list()
              if (length(real_labs) > 0) {
                ph <- paste(rep("?", length(real_labs)), collapse = ",")
                clauses <- c(clauses, sprintf("Investigator_Folder IN (%s)", ph))
                lab_params <- as.list(real_labs)
              }
              lab_sql <- paste0("AND (", paste(clauses, collapse = " OR "), ")")
            } else {
              ph <- paste(rep("?", length(input$var1)), collapse = ",")
              lab_sql <- sprintf("AND Investigator_Folder IN (%s)", ph)
              lab_params <- as.list(input$var1)
            }
            
            # --- Flowcell clause + params ---
            fph <- paste(rep("?", length(input$flo_flowcell)), collapse = ",")
            flow_sql <- sprintf("AND FlowcellID IN (%s)", fph)
            flow_params <- as.list(input$flo_flowcell)
            
            # --- Final SQL ---
            sql <- sprintf("
      SELECT DISTINCT Project_run_type
        FROM %s
       WHERE Application = ?
         %s
         %s
         %s
       ORDER BY Project_run_type
    ", quoted_table, year_sql, lab_sql, flow_sql)
            
            params <- c(year_params, lab_params, flow_params)
            
            # --- Query DuckDB ---
            run_opts <- tryCatch(
              DBI::dbGetQuery(con, sql, params = params)$Project_run_type,
              error = function(e) {
                write_log(log_Dashboard, "DROPDOWN_ERROR", paste0("Flo Runs query failed: ", e$message))
                character(0)
              }
            )
            run_opts <- sort(unique(na.omit(run_opts)))
            
            # --- Preserve & update selection ---
            prev <- isolate(input$flo_runs)
            sel <- intersect(prev, run_opts)
            sel <- if (length(sel) > 0) sel else run_opts
            
            shinyjs::delay(100, {
              freezeReactiveValue(input, "flo_runs")
              updatePickerInput(
                session, "flo_runs",
                choices = run_opts,
                selected = sel
              )
            })
          }
        )
        
        # ─────────────────────────────────────────────────────────────
        # 3) Populate Flo Metric
        # ─────────────────────────────────────────────────────────────
        observeEvent(
          { input$plt; input$flo_runs; input$var1 },
          {
            req(input$plt == "Flo", input$flo_runs, input$flo_flowcell, input$var1, input$app)
            
            # Identify correct source table
            source_table <- session_env$app_source_table[[input$app]]
            quoted_table <- DBI::dbQuoteIdentifier(con, source_table)
            
            # --- Year clause + params ---
            if (input$release_year == "NULL") {
              year_sql    <- "AND (ProjStatus != 'Delivered' OR Release_Date IS NULL OR TRIM(Release_Date) = '' OR LOWER(TRIM(Release_Date)) = 'null')"
              year_params <- list(input$app)
            } else {
              year_sql    <- "
              AND strftime('%Y', try_cast(Release_Date AS DATE)) = ?
              AND ProjStatus = 'Delivered'
            "
              year_params <- list(input$app, input$release_year)
            }
            
            # --- Lab clause ---
            if ("Missing Lab" %in% input$var1) {
              real_labs <- setdiff(input$var1, "Missing Lab")
              clauses   <- c("(Investigator_Folder IS NULL OR TRIM(Investigator_Folder) = '' OR LOWER(TRIM(Investigator_Folder)) IN ('null','na'))")
              lab_params <- list()
              if (length(real_labs) > 0) {
                ph <- paste(rep("?", length(real_labs)), collapse = ",")
                clauses <- c(clauses, sprintf("Investigator_Folder IN (%s)", ph))
                lab_params <- as.list(real_labs)
              }
              lab_sql <- paste0("AND (", paste(clauses, collapse = " OR "), ")")
            } else {
              ph <- paste(rep("?", length(input$var1)), collapse = ",")
              lab_sql <- sprintf("AND Investigator_Folder IN (%s)", ph)
              lab_params <- as.list(input$var1)
            }
            
            # --- Flowcell clause ---
            fph         <- paste(rep("?", length(input$flo_flowcell)), collapse = ",")
            flow_sql    <- sprintf("AND FlowcellID IN (%s)", fph)
            flow_params <- as.list(input$flo_flowcell)
            
            # --- Run clause ---
            rph         <- paste(rep("?", length(input$flo_runs)), collapse = ",")
            run_sql     <- sprintf("AND Project_run_type IN (%s)", rph)
            run_params  <- as.list(input$flo_runs)
            
            # --- Final SQL ---
            sql <- sprintf("
      SELECT DISTINCT name
        FROM %s
       WHERE Application = ?
         %s
         %s
         %s
         %s
       ORDER BY name
    ", quoted_table, year_sql, lab_sql, flow_sql, run_sql)
            params <- c(year_params, lab_params, flow_params, run_params)
            
            # --- Execute query for metric names ---
            metric_opts <- tryCatch({
              DBI::dbGetQuery(con, sql, params = params)$name %>%
                trimws() %>%
                intersect(flo_only_metrics) %>%
                unique() %>%
                sort()
            }, error = function(e) {
              write_log(log_Dashboard, "DROPDOWN_ERROR", paste0("Flo metric query failed: ", e$message))
              character(0)
            })
            
            # --- Preserve & update selection ---
            prev <- isolate(input$flo_metric)
            sel  <- if (!is.null(prev) && prev %in% metric_opts) prev else head(metric_opts, 1)
            
            shinyjs::delay(100, {
              freezeReactiveValue(input, "flo_metric")
              updatePickerInput(session, "flo_metric",
                                choices  = metric_opts,
                                selected = sel)
            })
          }
        )
        
        ###########################################################
        #-------- Clear Flo Dropdowns When Empty Block --------
        #If no flowcell is selected, clear the available runs.
        #If no run is selected, clear the available Flo-compatible metrics.
        observe({
          if (is.null(input$flo_flowcell) || length(input$flo_flowcell) == 0) {
            updatePickerInput(session, "flo_runs", choices = character(0), selected = NULL)
          }
        })
        
        observe({
          if (is.null(input$flo_runs) || length(input$flo_runs) == 0) {
            updatePickerInput(session, "flo_metric", choices = character(0), selected = NULL)
          }
        })
        ###########################################################P#####################################
        #-------- Refresh App Data Block Manually --------
        #When the "Refresh App Data" button is clicked, this block:
        #1. Reconnects to the DuckDB database and fetches metrics for the selected app.
        #2. Applies group-level filtering (if user is not Genome Technologies).
        #3. Updates the session-local cache with the new filtered data.
        #4. Re-applies the current release year filter to populate filtered_by_year_cache().
        #5. Refreshes the Lab dropdown via the downstream observer on filtered_by_year_cache().
        #6. Re-queries the Application list in the database and updates the app menu accordingly.
        #7. Displays a timestamped notification to inform the user that data has been refreshed.
        

         ###################################################################################
        #-------------Expiry Observer--------------
        #This checks for stale cache every 30 seconds (adjust as needed):
        observe({
          invalidateLater(30000, session)  # Check every 30 sec
          if (!is.null(session_env$cache_time()) &&
              difftime(Sys.time(), session_env$cache_time(), units = "mins") > 60) {
            
            session_env$session_data_cache(NULL)
            session_env$cache_time(NULL)
            write_log(log_Dashboard, "APP_CACHE_CLEARED", "Session cache expired and cleared." )
          }
        })
        ###########################################################P#####################################
        ###########################################################P#####################################
        ###########################################################P#####################################
        #Global search bar
        #This block enables a unified global search feature. It supports both automatic search on typing and manual search via a button. 
        #When triggered, it searches across multiple QC metrics tables for a matching Project_run_type, updates the App and Year dropdowns accordingly, 
        #and stores matching Lab and Run values for downstream selection. It also ensures that the correct lab (var1) and run (var3) are auto-selected once their respective dropdowns are populated.
        
        session_env$search_trigger <- reactiveVal()
        
        # 1. Search via button click
        observeEvent(input$search_btn, {
          isolate({
            search_term <- trimws(input$global_search)
            if (nzchar(search_term)) {
              #Force re-trigger even if same string
              session_env$search_trigger(NULL)
              session_env$search_trigger(search_term)
            }
          })
        })
        
        # 2. Search automatically when typing (on blur or enter)
        observeEvent(input$global_search, {
          search_term <- trimws(input$global_search)
          if (nzchar(search_term)) {
            session_env$search_trigger(search_term)
          }
        })
        
        # 3. Search execution logic (debounced/centralized)
        observeEvent(session_env$search_trigger(), {
          search_input <- trimws(session_env$search_trigger())
          req(nzchar(search_input))
          
          tables_to_search <- c("qc_illumina_metrics", "qc_pacbio_metrics", "qc_ont_metrics")
          match_results <- list()
          
          total_time <- system.time({
            for (tbl in tables_to_search) {
              start_time <- Sys.time()
              all_fields <- DBI::dbListFields(con, tbl)
              if (!"project_run_type" %in% tolower(all_fields)) next
              
              sql <- glue::glue("SELECT *, '{tbl}' AS SourceTable FROM {tbl} WHERE LOWER(Project_run_type) = LOWER(?)")
              result <- DBI::dbGetQuery(con, sql, params = list(search_input))
              
              if (nrow(result) > 0) match_results[[tbl]] <- result
            }
          })
          
          if (length(match_results) == 0) {
            showNotification("No matches found. Best to search by 'Project Run Type'.", type = "error")
            return()
          }
          
          all_matches <- do.call(rbind, match_results)
          
          first_row <- head(all_matches, 1)
          
          app_found  <- first_row$Application[[1]]
          year_found <- substr(first_row$Release_Date[[1]], 1, 4)
          lab_found  <- first_row$Investigator_Folder[[1]]
          run_found  <- first_row$Project_run_type[[1]]
          
          # Update upstream App + Year
          updateSelectInput(session, "app", selected = app_found)
          updateSelectInput(session, "release_year", selected = year_found)
          
          # Save downstream targets for delayed update
          session_env$search_lab  <- lab_found
          session_env$search_run  <- run_found
          session_env$search_data <- all_matches
        })
        
        
        observeEvent(input$var1, {
          isolate({
            lab_found <- session_env$search_lab
            
            if (!is.null(lab_found) && lab_found %in% input$var1) {
              updateSelectInput(session, "var1", selected = lab_found)
              session_env$search_lab <- NULL
            }
          })
        })
        
        observeEvent(input$var3, {
          isolate({
            run_found <- session_env$search_run
            
            if (!is.null(run_found) && run_found %in% input$var3) {
              updateSelectInput(session, "var3", selected = run_found)
              session_env$search_run <- NULL
            }
          })
        })
        
        
        
        
        ###########################################################P#####################################
        ###########################################################P#####################################
        ###########################################################P#####################################
        
        ###################################################################################
        #-------- Final Reactive Input Filter Block --------
        #Filters the project-run-type-specific data based on selected metrics and species.
        #Also coerces values to numeric and ensures clean, ready-to-plot data.
        #Returns a filtered data frame that is used by all standard plots (Box, Bar).
        #    
        react_input <- reactive({
          req(input$app, input$release_year, input$var1, input$var2, input$var3, input$var5, input$var6)
          
          # Get correct source table
          source_table <- session_env$app_source_table[[input$app]]
          quoted_table <- DBI::dbQuoteIdentifier(con, source_table)
          
          # SQL clauses
          year_clause <- if (input$release_year == "NULL") {
            " AND (ProjStatus != 'Delivered' OR Release_Date IS NULL OR TRIM(Release_Date) = '' OR LOWER(TRIM(Release_Date)) = 'null')"
          } else {
            " AND strftime('%Y', try_cast(Release_Date AS DATE)) = ?
              AND ProjStatus = 'Delivered'"
          }
          
          lab_clause <- if ("Missing Lab" %in% input$var1) {
            " AND (Investigator_Folder IS NULL OR TRIM(Investigator_Folder) = '' OR LOWER(TRIM(Investigator_Folder)) IN ('null','na'))"
          } else {
            paste0(" AND Investigator_Folder IN (", paste(rep("?", length(input$var1)), collapse = ","), ")")
          }
          
          proj_clause <- if ("Missing Project" %in% input$var2) {
            " AND (Project_ID IS NULL OR TRIM(Project_ID) = '' OR LOWER(TRIM(Project_ID)) IN ('null','na'))"
          } else {
            paste0(" AND Project_ID IN (", paste(rep("?", length(input$var2)), collapse = ","), ")")
          }
          
          run_clause <- if ("Missing Run" %in% input$var3) {
            " AND (Project_run_type IS NULL OR TRIM(Project_run_type) = '' OR LOWER(TRIM(Project_run_type)) IN ('null','na'))"
          } else {
            paste0(" AND Project_run_type IN (", paste(rep("?", length(input$var3)), collapse = ","), ")")
          }
          
          metric_clause <- paste0(" AND name IN (", paste(rep("?", length(input$var5)), collapse = ","), ")")
          species_clause <- paste0(" AND Species IN (", paste(rep("?", length(input$var6)), collapse = ","), ")")
          
          # Assemble SQL query
          sql <- paste0(
            "SELECT *, CAST(value AS VARCHAR) AS raw_value FROM ",
            quoted_table,
            " WHERE Application = ?",
            year_clause,
            lab_clause,
            proj_clause,
            run_clause,
            metric_clause,
            species_clause
          )
          
          # Build params in correct order
          params <- list(input$app)
          if (input$release_year != "NULL") params <- c(params, input$release_year)
          if (!("Missing Lab" %in% input$var1))     params <- c(params, input$var1)
          if (!("Missing Project" %in% input$var2)) params <- c(params, input$var2)
          if (!("Missing Run" %in% input$var3))     params <- c(params, input$var3)
          params <- c(params, input$var5, input$var6)
          
          # Execute query
          df <- DBI::dbGetQuery(con, sql, params = params)
          
          # Convert values
          df$value <- suppressWarnings(as.numeric(gsub(",", "", df$raw_value)))
          if ("Lane" %in% colnames(df)) {
            df$Lane <- suppressWarnings(as.numeric(df$Lane))
          }
          # 🔧 Patch: create Sample_Name if only Sample_ID exists (for ONT/PacBio)
          if (!"Sample_Name" %in% names(df) && "Sample_ID" %in% names(df)) {
            df$Sample_Name <- df$Sample_ID
          }
          # 🔧 Patch missing columns for downstream compatibility
          if (!"FlowcellID" %in% names(df)) {
            df$FlowcellID <- NA_character_  # Placeholder for compatibility
          }

          df
        })
        
        
        ######################################################################################################################
        #-------- Long Format Metric Input Block --------
        #Prepares react_input() data for long-format plotting by:
        #1. Removing domain/species entries (which are not standard metrics),
        #2. Deduplicating by Sample_Name and metric name,
        #3. Converting values to numeric and removing NA.
        #This ensures clean, non-redundant input for Box/Bar plots.
        
        react_input_long <- reactive({
          df <- react_input()
          req(nrow(df) > 0)
          
          if (!"Sample_Name" %in% names(df)) {
            stop("❌ `Sample_Name` column missing from input data.")
          }
          
          df %>%
            filter(!grepl("^Domain[1-4]$|^Species[1-5]$", name)) %>%
            distinct(Sample_Name, name, .keep_all = TRUE) %>%
            mutate(value = as.numeric(gsub(",", "", value))) %>%
            filter(!is.na(value))
        })
        
        #output$debug_metric <- renderText({
        #  df <- react_input_long()
        #  cat("Returned rows:", nrow(df), "\n")
        #  str(head(df))
        #})
        ####################################################################################
        # ---- Sample count summary by folder ----
        sample_size <- reactive({
          react_input_long() |> group_by(Investigator_Folder) |>
            summarise(num = n_distinct(Sample_Name), .groups = "drop")
        })
        ####################################################################################
        # ---- Metric summary (mean, sum) by folder & metric ----
        react_value <- reactive({
          react_input_long() |> group_by(Investigator_Folder, name) |>
            summarise(across(value, list(mean = mean, sum = sum)), .groups = "drop")
        })
        ###########################################################Plotting area
        #Here is where all display code will be added.
        #Do note that the variable containing all data point is found in reac_data()
        #define plot command options
        
        source(DashboardPlottingCode, local = TRUE)
        
        ############################################################function to download data
        #-------- Download Data Function Block --------
        #Converts filtered data to wide format for CSV export, removes lane/status columns and duplicates for non-GenomeTech users

        downloadfunc <- reactive({
          # 1) grab exactly the slice we need
          df <- react_input()
          
          # 2) explicitly list the columns that should stay fixed (all except name/value)
          id_cols <- setdiff(names(df), c("name", "value", "raw_value"))
          
          # 3) pivot into wide form
          wide_data <- df |>
            tidyr::pivot_wider(
              id_cols     = id_cols,
              names_from   = name,
              values_from  = value,
              values_fn    = list,     # collect duplicates in lists, just in case
              values_fill  = NA        # fill missing combinations with NA, not error
            )
          
          # 4) drop helper/raw columns
          wide_data <- wide_data |>
            dplyr::select(-any_of(c("raw_value", "Application")))
          
          # 5) For non‐GT users, drop Lane/ProjStatus/Release_Year and keep one row per sample
          if (!is_genome_tech()) {
            wide_data <- wide_data |>
              dplyr::select(-any_of(c("Lane", "ProjStatus", "Release_Year"))) |>
              dplyr::distinct(Sample_Name, .keep_all = TRUE)
          }
          
          wide_data
        })
        
        
        # ---- Data table preview ----
        output$dataDownload <- DT::renderDataTable({
          downloadfunc()
        })
        
        # ---- Download handler ----
        output$download <- downloadHandler(
          filename = function() {
            # sanitize the app name (replace spaces/punctuation with underscores)
            app_name <- gsub("[^A-Za-z0-9]+", "_", tolower(input$app))
            paste0(app_name, "_gtQCmetrics.csv")
          },
          content = function(fname) {
            # grab the reactive table
            df <- downloadfunc()
            
            # flatten any list-columns into a single string per cell
            for (col in names(df)) {
              if (is.list(df[[col]])) {
                df[[col]] <- vapply(
                  df[[col]],
                  function(x) if (length(x)) paste(x, collapse = ";") else NA_character_,
                  FUN.VALUE = character(1)
                )
              }
            }
            
            # now safe to write
            write.csv(df, fname, row.names = FALSE)
          },
          contentType = "text/csv"
        )
        
        ####################################################################################
        #SPECIES ALIGNMENT DATA MANAGEMENT AREA
        ####################################################################################
        #-------- Filtered Species Metrics Block --------
        #Extracts species-level metrics (Species1–5) for selected labs, runs, and species choices.
        #Used for rendering stacked bar plots of species alignment in the dashboard.
        
        filtered_species_metric <- reactive({
          # start from the app+year slice
          df <- filtered_by_year_cache()
          # require all of app, year, lab, project, run, and species be chosen
          req(df, input$var1, input$var2, input$var3, input$var6)
          allowed_species_names <- paste0("Species", 1:5)
          df %>%
            filter(
              Investigator_Folder %in% input$var1,
              Project_ID          %in% input$var2,
              Project_run_type    %in% input$var3,
              name                %in% allowed_species_names,
              Species             %in% input$var6
            )
        })
        
        
        # -- Track view selection state: plot or table --
        selected_species_view <- reactiveVal("plot")
        observeEvent(input$plot_species, { selected_species_view("plot") })
        observeEvent(input$table_species, { selected_species_view("table") })
        # -- Reshape for plot --
        species_plot_data <- reactive({
          req(filtered_species_metric(), selected_species_view() == "plot")
          
          filtered_species_metric() %>%
            filter(!is.na(value), value != "n.a.") %>%
            mutate(
              Alignment = as.numeric(stringr::str_extract(value, "(?<=\\().*?(?=\\))")),
              Species   = stringr::str_trim(stringr::str_remove(value, "\\(.*?\\)"))
            ) %>%
            filter(!is.na(Alignment)) %>%
            distinct(GT_QC_Sample_ID, Species, .keep_all = TRUE)
        }) %>% debounce(500)

        ##############
        ##############
        ##############
        ##############
        source(SpeciesAlignmentPlot, local = TRUE)
        ##############
        ##############
        ##############
        ##############
        
        # -------- Prepare a reactive wide‐species dataset with one row per Sample_Name --------
        species_download_data <- reactive({
          df <- filtered_species_metric()
          
          wide_data <- df %>%
            tidyr::pivot_wider(
              names_from  = name,
              values_from = value,
              values_fn   = list
            ) %>%
            tidyr::unnest(cols = everything())
          
          # drop Application (and for non-GT users drop Lane/ProjStatus/Release_Year)
          wide_data <- wide_data %>%
            select(-any_of(c("Application",
                             if (!is_genome_tech()) c("Lane","ProjStatus","Release_Year") else NULL)))
          
          # ensure one row per Sample_Name
          wide_data %>%
            distinct(Sample_Name, .keep_all = TRUE)
        })
        
        
        # -------- Render the species table as wide, one‐row‐per‐sample --------
        output$species_table <- DT::renderDataTable({
          req(selected_species_view() == "table")
          species_download_data()
        })
        
        # -------- Download handler uses the same wide dataset --------
        output$download_species <- downloadHandler(
          filename = function() {
            # sanitize the app name (lowercase, non‐alphanumerics → “_”)
            app_name <- gsub("[^A-Za-z0-9]+", "_", tolower(input$app))
            paste0(app_name, "_SpeciesMetrics_", ".csv")
          },
          content = function(file) {
            df <- species_download_data()
            
            # 1) Drop the Application column if present
            if ("Application" %in% names(df)) {
              df <- df %>% select(-Application)
            }
            
            # 2) Collapse any list‐columns into semicolon‐separated strings
            for (col in names(df)) {
              if (is.list(df[[col]])) {
                df[[col]] <- vapply(
                  df[[col]],
                  function(x) if (length(x)) paste(x, collapse = ";") else NA_character_,
                  FUN.VALUE = character(1)
                )
              }
            }
            
            # 3) Write to CSV
            write.csv(df, file, row.names = FALSE)
          },
          contentType = "text/csv"
        )
        
        ####################################################################################
        #Summary Metrics
        ####################################################################################
        # ---- Summary of selected metrics across all labs ----
        output$summary <- renderPrint({
          tryCatch({
            df <- react_input_long()
            req(nrow(df) > 0)
            
            summary_by_name <- df %>%
              dplyr::group_by(name) %>%
              dplyr::summarise(
                Min = min(value, na.rm = TRUE),
                Q1 = quantile(value, 0.25, na.rm = TRUE),
                Median = median(value, na.rm = TRUE),
                Mean = mean(value, na.rm = TRUE),
                Q3 = quantile(value, 0.75, na.rm = TRUE),
                Max = max(value, na.rm = TRUE),
                SD = sd(value, na.rm = TRUE),
                n = dplyr::n(),
                .groups = "drop"
              )
            
            print(as.data.frame(summary_by_name), row.names = FALSE)
          }, error = function(e) {
            cat("")
          })
        })
        
        # ---- Show Group Summary tab only for Genome Tech users ----
        output$showDescribeTab <- reactive({
          is_genome_tech()
        })
        outputOptions(output, "showDescribeTab", suspendWhenHidden = FALSE)
        
        # ---- Group-wise Summary (per PI Folder) ----
        output$describe <- renderPrint({
          tryCatch({
            df <- react_input_long()
            req(all(c("Investigator_Folder", "name", "value") %in% names(df)))
            
            group_summaries <- df %>%
              dplyr::group_split(Investigator_Folder) %>%
              purrr::map_chr(function(group_df) {
                folder <- unique(group_df$Investigator_Folder)
                
                summary <- group_df %>%
                  dplyr::group_by(name) %>%
                  dplyr::summarise(
                    n = dplyr::n(),
                    Min = min(value, na.rm = TRUE),
                    Q1 = quantile(value, 0.25, na.rm = TRUE),
                    Median = median(value, na.rm = TRUE),
                    Mean = mean(value, na.rm = TRUE),
                    Q3 = quantile(value, 0.75, na.rm = TRUE),
                    Max = max(value, na.rm = TRUE),
                    SD = sd(value, na.rm = TRUE),
                    .groups = "drop"
                  )
                
                paste0(
                  "=== Investigator Folder: ", folder, " ===\n",
                  paste(capture.output(print(as.data.frame(summary), row.names = FALSE)), collapse = "\n")
                )
              })
            
            cat(paste(group_summaries, collapse = "\n-------------------------------\n"))
          }, error = function(e) {
            cat("")
          })
        })
        output$summary_warning_ui <- renderUI({
          df <- tryCatch(react_input_long(), error = function(e) NULL)
          if (is.null(df) || nrow(df) == 0 || all(is.na(df$value))) {
            div(
              style = "color: darkred; background-color: #fff4f4; border: 1px solid #f5c2c7;
       padding: 10px; margin-bottom: 10px; border-radius: 6px;",
              tags$b("⚠ No summary available."),
              tags$br(),
              "Possible reasons:",
              tags$ul(
                tags$li("You did not select any metrics."),
                tags$li("No data available for the chosen filters."),
                tags$li("Reach out to gtdrylab@jax.org for further assistance.")
              )
            )
          } else {
            NULL
          }
        })
        
        output$describe_warning_ui <- renderUI({
          df <- tryCatch(react_input_long(), error = function(e) NULL)
          if (is.null(df) || nrow(df) == 0 || all(is.na(df$value))) {
            div(
              style = "color: darkred; background-color: #fff4f4; border: 1px solid #f5c2c7;
              padding: 10px; margin-bottom: 10px; border-radius: 6px;",
              tags$b("⚠ Group summary not available."),
              tags$br(),
              "Possible reasons:",
              tags$ul(
                tags$li("You did not select any metrics."),
                tags$li("No data available for the chosen filters."),
                tags$li("Reach out to the gtdrylab@jax.org for further assistance")
              )
            )
          } else {
            NULL
          }
        })
        
        ####################################################################################
        # Queries DuckDB: from the unified_qc_view table, Caches the result in sample_counts_cache() so it can be used in: App sample counter and Menu badge percentages
        ####################################################################################
        observeEvent(app_refresh_counter(), {
          init_sample_counts()
        })
        
        ####################################################################################
        # Total samples in the database across all applications 
        ####################################################################################
        
        total_database_samples <- reactive({
          df <- sample_counts_cache()
          if (is.null(df)) return(0)
          sum(df$Sample_Count, na.rm = TRUE)
        })
        
        ####################################################################################
        # Update UI element with count of filtered samples and total
        ####################################################################################
        observe({
          req(input$app)
          req(app_ready())  # ⬅️ Only run when app is fully ready and dropdowns are populated
          req(filtered_by_year_cache(), total_database_samples() > 0)
          
          output$App_sample_counter_colored <- renderUI({
            app_sample_count <- length(unique(filtered_by_year_cache()$Sample_Name))
            total_samples <- total_database_samples()
            
            span(
              paste0(format(app_sample_count, big.mark = ","),
                     " (of ", format(total_samples, big.mark = ","), ")"),
              style = "color: #4497f2; font-weight: bold; font-size: 18px;"
            )
          })
        })
        
        ####################################################################################
        # Info Icon in Header: Percentage of each app's samples in the total database
        ####################################################################################
        output$menu_task_items <- renderUI({
          req(total_database_samples() > 0) #last_updated_app(),
          total <- total_database_samples()
          
          task_items <- {
            df <- sample_counts_cache()
            if (is.null(df) || nrow(df) == 0) return(NULL)
            total <- sum(df$Sample_Count, na.rm = TRUE)
            
            lapply(seq_len(nrow(df)), function(i) {
              app_name <- df$Application[i]
              count <- df$Sample_Count[i]
              value <- round((count / total) * 100, 2)
              shinydashboard::taskItem(value = value, color = "green", text = app_name)
            })
          }
          shinydashboard::dropdownMenu(
            type = "task",
            badgeStatus = "success",
            icon = icon("info-circle", "fa-2x", style = "color: #4497f2;"),
            headerText = "Percent of application's samples in database",
            .list = purrr::compact(task_items)
          )
        })
        
        ####################################################################################
        #This closes the check for admin. leave as is
      }
    })
    ####################################################################################
    ####################################ADMIN PAGE################################################
    source(userSelfEmailUpdate, local = TRUE)
    source(AdminPage, local = TRUE)
    #############################################################################################
    
  })
}


