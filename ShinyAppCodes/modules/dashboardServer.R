module2_Server <- function(id, selectedGroup, authenticated_group, authenticated_email, dashboard_ready, main_tab) {
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
          tabPanel(HTML('<i class="ti ti-chart-bar"></i> Plot'),
                   div(style = "padding-top: 16px;",
                       actionButton(ns("show_plot"), "View Plot",
                                    icon = icon("chart-column"),
                                    style = "background-color: #185FA5; color: white; border: none; border-radius: 8px; font-weight: 500;"),
                       actionButton(ns("show_multiqc"), "View MultiQC",
                                    icon = icon("magnifying-glass-chart"),
                                    style = "background-color: #185FA5; color: white; border: none; border-radius: 8px; font-weight: 500;"),
                   br(), br(),
                   tags$head(tags$style(HTML("
                    .scrollable-plot-container {
                    overflow-x: auto !important;
                    overflow-y: auto !important;
                    /* Fills whatever's left of the actual browser viewport
                       instead of a fixed 550px — on a tall screen a fixed
                       height left a large dead strip below the plot; on a
                       short one it could still clip. 300px accounts for the
                       header/tabs/buttons above this box; 420px is a floor
                       so it never gets uncomfortably short. Matches the
                       plot figure's own height (plot_panel_height() in
                       DashboardPlotCode.R, computed from window_h with the
                       same 300px margin), so the figure fills this box
                       without leaving it mostly empty. */
                    height: max(420px, calc(100vh - 300px));
                    border: 1px solid #ccc;
                    padding: 10px;
                    background-color: #fff;
                    box-shadow: 0 0 5px rgba(0,0,0,0.1);
                  }
                    .center-warning {
                    height: 80vh;
                    display: flex;
                    justify-content: center;
                    align-items: center;
                    color: red;
                    font-size: 20px;
                    font-style: italic;
                    text-align: center;
                    padding: 0 20px;
                  }"))),
                   div(class = "scrollable-plot-container gt-scrollbox",
                       uiOutput(ns("plot_multiqc_switch_ui"))
                   ))
          ),
          tabPanel(title = HTML('<i class="ti ti-dna"></i> Alignment'), value = "species_tab",
                   div(style = "padding-top: 16px;",
                   actionButton(ns("plot_species"), "Show Plot",
                                icon = icon("chart-column"),
                                style = "background-color: #185FA5; color: white; border: none; border-radius: 8px; font-weight: 500;"),
                   actionButton(ns("table_species"), "Show Table",
                                icon = icon("table"),
                                style = "background-color: #185FA5; color: white; border: none; border-radius: 8px; font-weight: 500;"),
                   downloadButton(ns("download_species"), "Download Table",
                                  style = "background-color: #185FA5; color: white; border: none; border-radius: 8px; font-weight: 500;"),
                   br(), br(),
                   div(style = 'width: 100%; height: max(420px, calc(100vh - 300px)); overflow-x: auto; overflow-y: auto;',
                       uiOutput(ns("species_view_ui")))
                   ) 
          ),
          tabPanel(HTML('<i class="ti ti-download"></i> Download'),
                   div(style = "padding-top: 16px;",
                       downloadButton(ns('download'), "Download Below Metrics",
                                      style = "background-color: #185FA5; color: white; border: none; border-radius: 8px; font-weight: 500;"),
                       br(), br(),
                       fluidRow(column(12, div(class = "gt-scrollbox", style = 'overflow-x: auto; overflow-y: auto; height: max(420px, calc(100vh - 300px));',
                                               DT::dataTableOutput(ns('dataDownload')))))
                   )
          ),
          tabPanel(HTML('<i class="ti ti-file-text"></i> Summary'),
                   shinycssloaders::withSpinner(
                     div(style = "max-height: 400px; overflow-y: auto;",
                         uiOutput(ns("summary_warning_ui")),
                         verbatimTextOutput(ns("summary")))
                   )
          )
        )
        tabs[[length(tabs) + 1]] <- tabPanel(HTML('<i class="ti ti-mail"></i> Update Group Emails'),
                                             br(),
                                             uiOutput(ns("user_group_email_ui"))
        )
        
        # Conditionally add GroupSummary tab for GenomeTechnologies
        if (is_genome_tech()) {
          tabs[[length(tabs) + 1]] <- tabPanel(HTML('<i class="ti ti-users"></i> GroupSummary'),
                                               shinycssloaders::withSpinner(
                                                 div(style = "max-height: 400px; overflow-y: auto;",
                                                     uiOutput(ns("describe_warning_ui")),  # new warning UI
                                                     verbatimTextOutput(ns("describe")))
                                               )
          )
        }
        # ---- Projects tab (GenomeTechnologies only) ----
        if (is_genome_tech()) {
          tabs[[length(tabs) + 1]] <- tabPanel(
            title = HTML('<i class="ti ti-microscope"></i> SecondaryAnalysisQC'),
            value = "projects_tab",
            div(
              style = "width: 100%; height: 70vh; overflow: hidden; margin-top: 10px;",
              tags$iframe(
                src = "/projects/",
                style = "width: 100%; height: 100%; border: none;",
                sandbox = paste(
                  "allow-scripts",
                  "allow-same-origin",
                  "allow-popups",
                  "allow-popups-to-escape-sandbox",
                  "allow-top-navigation-by-user-activation"
                )
              )
            )
          )
        }
        # Render final tabset
        div(
          id = ns("normal_tabs"),
          style = "position: relative;",
          # ── Pill-tab CSS ──────────────────────────────────────────────────────
          tags$head(tags$style(HTML("

            .nav-tabs { border-bottom: none !important; gap: 4px; flex-wrap: wrap; }

            .nav-tabs > li > a {
              display: inline-flex !important;
              align-items: center;
              gap: 6px;
              padding: 7px 15px !important;
              border-radius: 20px !important;
              border: 1px solid transparent !important;
              font-size: 13px;
              font-weight: 400;
              color: #5F5E5A !important;
              background: transparent !important;
              margin: 0 !important;
              transition: background 0.14s, color 0.14s, border-color 0.14s;
              line-height: 1.4;
            }

            .nav-tabs > li > a:hover {
              background: #F1EFE8 !important;
              color: #2C2C2A !important;
              border-color: transparent !important;
            }

            .nav-tabs > li.active > a,
            .nav-tabs > li.active > a:hover,
            .nav-tabs > li.active > a:focus {
              background: #E6F1FB !important;
              color: #185FA5 !important;
              border-color: #B5D4F4 !important;
              font-weight: 500;
            }
            .nav-tabs > li > a[data-value='Plot']::before                { content: '\\ea7f'; font-family: 'tabler-icons'; font-style: normal; font-size: 15px; line-height: 1; }
            .nav-tabs > li > a[data-value='Summary']::before              { content: '\\eb6d'; font-family: 'tabler-icons'; font-style: normal; font-size: 15px; line-height: 1; }
            .nav-tabs > li > a[data-value='Update Group Emails']::before  { content: '\\eb20'; font-family: 'tabler-icons'; font-style: normal; font-size: 15px; line-height: 1; }
            .nav-tabs > li > a[data-value='GroupSummary']::before         { content: '\\ec25'; font-family: 'tabler-icons'; font-style: normal; font-size: 15px; line-height: 1; }
          "))),
          # Tabs panel
          do.call(tabsetPanel, c(id = ns("main_tabs"), type = "tabs", tabs))
        )
      }
    })
    ####################################################################################
    #-------- App Menu Filter (User Group–Specific) Block--------
    #Filters app list to show only apps with Investigator_Folder matching user's group, except for GenomeTech.
    #Defines qc_metrics_fetched to track whether app metrics were already fetched.
    last_refresh_time_shared <- reactiveVal(NULL)
    
    observe({
      req(authenticated_group())
      if (!is_admin()) {
        qc_metrics_fetched <- reactiveVal(FALSE)
        # ─────────────────────────────────────────────────────────────
        # Refresh App Data: bump `input$app` via sendInputMessage
        # ─────────────────────────────────────────────────────────────
        
        app_refresh_counter <- reactiveVal(0)
        
        observeEvent(input$refresh_app_data, {
          req(input$app)
          
          # 1. Reconnect to DuckDB (live read) — recreate the pool rather than yanking
          #    the single connection object other in-flight queries may be using.
          try(pool::poolClose(con), silent = TRUE)
          con <<- pool::dbPool(duckdb::duckdb(), dbdir = path_to_duckdb, read_only = TRUE)
          
          # 2. Forcefully trigger downstream observers using invalidateReactive
          app_refresh_counter(app_refresh_counter() + 1)
          
          # 3. Stamp refresh time
          last_refresh_time_shared(Sys.time())
          
          # 4. Notify user
          showNotification(
            paste0("🔄 Data refreshed at ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
            type     = "message",
            duration = 2
          )
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

        # Real (non-"NULL"/undelivered) years with delivered data for the
        # current app — computed once below as part of populating the main
        # release_year dropdown, and re-exposed here so Compare Runs can read
        # it directly instead of re-running the same query a second time.
        available_years <- reactiveVal(character(0))

        observeEvent(list(input$app, app_refresh_counter()), {
          req(input$app)
          app_loading(TRUE)
          app_ready(FALSE)
          
          # Route query to correct table
          source_table <- session_env$app_source_table[[input$app]]
          quoted_table <- DBI::dbQuoteIdentifier(con, source_table)
          
          # EXTRACT(YEAR FROM ...) instead of strftime('%Y', ...): both still
          # have to evaluate try_cast on every Application-matching row
          # before DISTINCT can dedupe it (this query's whole job is
          # discovering which years exist, so unlike the other date filters
          # in this file there's no fixed range to compare against instead)
          # but returning an integer is cheaper than formatting a string per
          # row, and it's one less function call wrapping the cast.
          sql <- sprintf("
    SELECT DISTINCT CAST(EXTRACT(YEAR FROM try_cast(Release_Date AS DATE)) AS VARCHAR) AS yr
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

          # TEMPORARY timing instrumentation — remove once we've confirmed
          # where the 5-minute app-switch delay in Compare Runs is actually
          # going. Every app switch runs this query, so it's a prime
          # suspect: it has to try_cast every Application-matching row
          # before it can dedupe to distinct years, same class of cost as
          # the compare_data_pool() query already fixed, just not
          # resolvable with a fixed date range since discovering the years
          # IS the point of this query.
          .t0 <- Sys.time()
          delivered_years <- tryCatch({
            DBI::dbGetQuery(con, sql, params = params)$yr
          }, error = function(e) {
            write_log(log_Dashboard, "GET_YEARS_ERROR", paste0("Year fetch failed: ", e$message))
            character(0)
          })
          write_log(log_Dashboard, "TIMING_AVAILABLE_YEARS",
                     paste0("app=", input$app, " elapsed_secs=", round(as.numeric(Sys.time() - .t0, units = "secs"), 2)))
          
          delivered_years <- na.omit(delivered_years)
          available_years(delivered_years)

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
            # A raw BETWEEN on the cast date, not
            # strftime('%Y', try_cast(...)) = ?: formatting the date to a
            # string before comparing forces a full-table scan since it
            # defeats column min/max pruning, and this same query runs on
            # every single Application/Year change for every user — at a
            # production scale of millions of rows that difference is the
            # whole app's baseline responsiveness, not just Compare Runs'.
            where_year <- "
            AND try_cast(Release_Date AS DATE) BETWEEN ? AND ? AND ProjStatus = 'Delivered'"
            params     <- list(input$app, paste0(input$release_year, "-01-01"), paste0(input$release_year, "-12-31"))
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
          
          .t0 <- Sys.time()
          df_out <- tryCatch(
            DBI::dbGetQuery(con, sql, params = params),
            error = function(e) {
              write_log(log_Dashboard, "DB_FILTER_YEAR_ERROR",
                        paste0("Year filter query failed: ", e$message))
              NULL
            }
          )
          write_log(log_Dashboard, "TIMING_FILTERED_BY_YEAR_CACHE",
                     paste0("app=", input$app, " year=", input$release_year,
                            " rows=", if (is.null(df_out)) 0 else nrow(df_out),
                            " elapsed_secs=", round(as.numeric(Sys.time() - .t0, units = "secs"), 2)))

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

        ###############################################################################
        #-------- Compare Runs: scoped data pool (fast path + slow path) --------
        # ── COST INCIDENT NOTE ──────────────────────────────────────────────
        # An earlier version of this block ran unconditionally on every
        # Application change for every user (not gated on Compare Runs mode
        # at all) and pulled SELECT * for the app's ENTIRE delivered history
        # across all years in one go. That took down the production server.
        # A later version fixed the cost/gating but still ran its own fresh
        # query on every app switch — several seconds of "nothing visibly
        # happening" before anything rendered, easy to mistake for a missed
        # click. This version fixes that: the common case (Compare Runs
        # showing the SAME single year already selected in the main sidebar —
        # true right after opening Compare Runs, or after switching
        # Application, since neither touches cmp_year) reuses
        # filtered_by_year_cache(), which the main sidebar has ALREADY loaded
        # for that exact year/app combination — no new query at all. Only
        # picking a genuinely different year (or a 2nd/3rd one) falls back to
        # an actual, debounced, scoped DB query for just those (<=3) years —
        # never the app's full history — and even that only runs while
        # Compare Runs is active.

        # A multi-select pickerInput reports one input update per checkbox
        # click, not once when the user's done — so picking a 2nd and 3rd
        # year in quick succession would otherwise fire the slow-path query
        # below once per click instead of once total, each one queuing up
        # behind the last since Shiny runs a session's reactives
        # synchronously. Debouncing collapses rapid clicks into one query.
        # It's only ever consulted on the slow path (see below), so it adds
        # no delay at all to the common, fast-path case.
        cmp_year_debounced <- debounce(reactive({ input$cmp_year }), 600)

        # compare_data_pool() used to fetch full per-sample rows (value,
        # Sample_Name, ...) for every lab/run/metric in the selected years,
        # then leaned on R-side dplyr (filter/distinct/group_by/summarise)
        # to turn that into the handful of numbers the chart actually
        # needs. That's fine on a test table with a few hundred rows; on a
        # production table with millions of rows it meant pulling millions
        # of rows into a single R process and processing them there —
        # `top` on the server showed the R process pinned at 100% CPU on
        # ONE core for minutes, which is exactly what that looks like
        # (DuckDB itself is a multi-threaded columnar engine and would show
        # much higher CPU if the time were actually going into the query).
        #
        # Now this reactive ONLY answers "what labs/runs/metrics exist for
        # these years" — a small distinct-combinations table, one row per
        # unique (lab, run, metric) triple, not one row per sample. It
        # feeds plot_ui's picker-choice cascade below, which only ever
        # calls unique()/sort() on these same three columns anyway. The
        # actual chart values are a separate, small, already-aggregated
        # query — compare_plot_data() — run only once the user's full
        # Year/Lab/Run/Metric selection is in place, computed by DuckDB
        # (fast, multi-threaded, filtered to exactly what's selected)
        # instead of by R (single-threaded, on everything in the selected
        # years regardless of what's actually chosen).
        compare_data_pool <- reactive({
          # Deliberately NOT req() here: req()'s silent-error propagates up
          # and would abort output$plot_ui's ENTIRE render the moment this is
          # called before cmp_year exists yet (e.g. the very first render
          # after switching into Compare Runs) — killing the Year picker too,
          # not just leaving Lab/Run/Metric empty as intended. Returning NULL
          # plainly lets callers treat "not ready yet" the same way they
          # already treat "no data", exactly like this used to behave when it
          # was a plain reactiveVal.
          if (!isTRUE(input$plt == "Compare Runs") || is.null(input$app) ||
              is.null(input$cmp_year) || length(input$cmp_year) == 0) {
            return(NULL)
          }

          is_fast_path <- length(input$cmp_year) == 1 && identical(input$cmp_year, input$release_year)

          if (is_fast_path) {
            # The sidebar has already fetched this exact year/app combo
            # (filtered_by_year_cache()) — no new query, just collapse it
            # to distinct combinations in R. Cheap: it's a single year's
            # worth of rows, the same data Box mode already works with
            # every day at this scale.
            .t0 <- Sys.time()
            raw <- filtered_by_year_cache()
            if (is.null(raw) || nrow(raw) == 0) return(NULL)
            df_out <- unique(raw[c("Investigator_Folder", "Project_run_type", "name")])
            df_out$cmp_year <- input$cmp_year
            write_log(log_Dashboard, "TIMING_COMPARE_POOL_FAST",
                       paste0("app=", input$app, " year=", input$cmp_year,
                              " raw_rows=", nrow(raw), " pool_rows=", nrow(df_out),
                              " elapsed_secs=", round(as.numeric(Sys.time() - .t0, units = "secs"), 2)))
          } else {
            years <- head(cmp_year_debounced(), 2)
            # Same reasoning as the guard above: a debounced reactive returns
            # nothing at all until its first delay period elapses, so treat
            # that the same way — a plain "not ready yet" NULL, not a thrown
            # req() failure that would abort the caller's whole render.
            if (length(years) == 0) return(NULL)

            source_table <- session_env$app_source_table[[input$app]]
            quoted_table <- DBI::dbQuoteIdentifier(con, source_table)

            # One raw date-range comparison per selected year, bound as
            # parameters (never string-interpolated — input$cmp_year is
            # client-controllable, so these still have to go through the
            # same placeholder binding as any other user-supplied value).
            year_bounds <- paste(rep("(try_cast(Release_Date AS DATE) BETWEEN ? AND ?)", length(years)), collapse = " OR ")
            year_params <- as.vector(rbind(paste0(years, "-01-01"), paste0(years, "-12-31")))

            # DISTINCT on just 3 columns (plus a computed year) instead of
            # every raw row: one row per unique lab/run/metric combination
            # that exists in these years, not one row per sample. For a
            # table with millions of per-sample rows this is typically a
            # few thousand rows at most.
            sql <- sprintf("
    SELECT DISTINCT Investigator_Folder, Project_run_type, name,
           CAST(EXTRACT(YEAR FROM try_cast(Release_Date AS DATE)) AS VARCHAR) AS cmp_year
      FROM %s
     WHERE Application = ?
       AND ProjStatus = 'Delivered'
       AND (%s)
  ", quoted_table, year_bounds)

            .t0 <- Sys.time()
            df_out <- tryCatch(
              DBI::dbGetQuery(con, sql, params = c(list(input$app), as.list(year_params))),
              error = function(e) {
                write_log(log_Dashboard, "COMPARE_POOL_ERROR", paste0("Compare Runs pool query failed: ", e$message))
                NULL
              }
            )
            write_log(log_Dashboard, "TIMING_COMPARE_POOL_SLOW",
                       paste0("app=", input$app, " years=", paste(years, collapse=","),
                              " pool_rows=", if (is.null(df_out)) 0 else nrow(df_out),
                              " elapsed_secs=", round(as.numeric(Sys.time() - .t0, units = "secs"), 2)))
          }

          df_out
        })

        # The actual chart data: a small, targeted, DuckDB-side
        # filter+dedup+aggregate, run only once the user has a complete
        # Year/Lab/Run/Metric selection. This replaces what used to be a
        # dplyr pipeline in output$plot (DashboardPlotCode.R) operating on
        # compare_data_pool()'s old (large, per-sample-row) result — same
        # filter criteria and same dedup-before-averaging logic, just
        # computed by DuckDB against the real table instead of by R against
        # an already-fetched data frame, since it's DuckDB that's built
        # (and multi-threaded) for exactly this kind of aggregation.
        compare_plot_data <- reactive({
          if (!isTRUE(input$plt == "Compare Runs") || is.null(input$app) ||
              is.null(input$cmp_year) || is.null(input$cmp_lab) ||
              is.null(input$cmp_run) || is.null(input$cmp_metrics) ||
              length(input$cmp_year) == 0 || length(input$cmp_lab) == 0 ||
              length(input$cmp_run) == 0 || length(input$cmp_metrics) == 0) {
            return(NULL)
          }

          source_table <- session_env$app_source_table[[input$app]]
          quoted_table <- DBI::dbQuoteIdentifier(con, source_table)

          table_cols <- tryCatch(DBI::dbListFields(con, source_table), error = function(e) character(0))
          sample_col <- if ("Sample_Name" %in% table_cols) "Sample_Name"
                        else if ("Sample_ID" %in% table_cols) "Sample_ID"
                        else NA_character_
          if (is.na(sample_col)) return(NULL)
          quoted_sample_col <- DBI::dbQuoteIdentifier(con, sample_col)

          years <- head(input$cmp_year, 2)
          year_bounds <- paste(rep("(try_cast(Release_Date AS DATE) BETWEEN ? AND ?)", length(years)), collapse = " OR ")
          year_params <- as.vector(rbind(paste0(years, "-01-01"), paste0(years, "-12-31")))

          lab_ph    <- paste(rep("?", length(input$cmp_lab)), collapse = ",")
          run_ph    <- paste(rep("?", length(input$cmp_run)), collapse = ",")
          metric_ph <- paste(rep("?", length(input$cmp_metrics)), collapse = ",")

          # ROW_NUMBER() ... = 1 replicates the old
          # distinct(Sample_Name, Project_run_type, name, .keep_all = TRUE)
          # step — dedupe exact-duplicate sample/run/metric rows BEFORE
          # averaging, so a duplicated row (if the source table has one)
          # can't silently skew the mean. The Domain*/Species* exclusion
          # matches the same fixed list output$plot filtered out in R.
          sql <- sprintf("
    WITH dedup AS (
      SELECT Project_run_type, name,
             try_cast(replace(CAST(value AS VARCHAR), ',', '') AS DOUBLE) AS value,
             ROW_NUMBER() OVER (PARTITION BY %s, Project_run_type, name) AS rn
        FROM %s
       WHERE Application = ?
         AND ProjStatus = 'Delivered'
         AND (%s)
         AND Investigator_Folder IN (%s)
         AND Project_run_type IN (%s)
         AND name IN (%s)
         AND name NOT IN ('Domain1','Domain2','Domain3','Domain4','Species1','Species2','Species3','Species4','Species5')
    )
    SELECT Project_run_type, name, AVG(value) AS value
      FROM dedup
     WHERE rn = 1 AND value IS NOT NULL
     GROUP BY Project_run_type, name
  ", quoted_sample_col, quoted_table, year_bounds, lab_ph, run_ph, metric_ph)

          params <- c(
            list(input$app),
            as.list(year_params),
            as.list(input$cmp_lab),
            as.list(input$cmp_run),
            as.list(input$cmp_metrics)
          )

          .t0 <- Sys.time()
          result <- tryCatch(
            DBI::dbGetQuery(con, sql, params = params),
            error = function(e) {
              write_log(log_Dashboard, "COMPARE_PLOT_DATA_ERROR", paste0("Compare Runs plot data query failed: ", e$message))
              NULL
            }
          )
          write_log(log_Dashboard, "TIMING_COMPARE_PLOT_DATA",
                     paste0("app=", input$app, " years=", paste(years, collapse=","),
                            " labs=", length(input$cmp_lab), " runs=", length(input$cmp_run),
                            " metrics=", length(input$cmp_metrics),
                            " result_rows=", if (is.null(result)) 0 else nrow(result),
                            " elapsed_secs=", round(as.numeric(Sys.time() - .t0, units = "secs"), 2)))
          result
        })

        # Lets output$plot_ui show a "Preparing your plots..." placeholder
        # the instant Application changes while already in Compare Runs,
        # instead of a blank panel while compare_data_pool() re-queries for
        # the new app. Set synchronously/cheaply as soon as the trigger
        # fires; cleared once compare_data_pool() actually finishes.
        #
        # Deliberately keyed on input$app ONLY, not input$cmp_year: the
        # freshly-created cmp_year picker's own initial value round-tripping
        # to the server (right after first entering Compare Runs) also
        # changes input$cmp_year, and if THAT also flipped this flag on and
        # off, output$plot_ui (which depends on this flag) would render the
        # placeholder and then the real layout within the same reactive
        # flush that already replaces the picker DOM once — reintroducing
        # the exact "recreate the picker DOM while something else is mid
        # teardown" race that caused the blank-panel bug in the first place.
        # An Application change never has that problem: it doesn't coincide
        # with the pickers' own first round-trip.
        compare_pool_loading <- reactiveVal(FALSE)

        observeEvent(input$app, {
          if (isTRUE(input$plt == "Compare Runs")) compare_pool_loading(TRUE)
        }, ignoreInit = TRUE)

        observeEvent(compare_data_pool(), {
          compare_pool_loading(FALSE)
        }, ignoreNULL = FALSE)

        ###########################################################P#####################################
        #-------- Populate Lab Choices Block--------
        #Normalizes and updates the Lab (var1) dropdown based on filtered data by year.
        #Ensures all empty, NA, or "NULL"-like labs are shown as "NULL" option.
        
        
        # ─────────────────────────────────────────────────────────────
        # Populate Lab (var1) via SQL push-down, with “Missing Lab” last
        # ─────────────────────────────────────────────────────────────
       
        observeEvent(list(input$app, input$release_year), {
          req(input$app, input$release_year, selectedGroup(), authenticated_group())

          # Non-GT users are always restricted to their own lab, so the
          # distinct-Investigator_Folder scan below (a full pass over the
          # selected Application/Year on potentially huge tables like rnaseq)
          # would only be run to have its result thrown away. Skip it
          # entirely for non-GT users — this is the single biggest win
          # available for their initial Application-switch delay.
          if (!is_genome_tech()) {
            labs <- trimws(as.character(selectedGroup()))
          } else {
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
              AND try_cast(Release_Date AS DATE) BETWEEN ? AND ?
              AND ProjStatus = 'Delivered'
            "
              params     <- list(input$app, paste0(input$release_year, "-01-01"), paste0(input$release_year, "-12-31"))
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

            .t0_lab <- Sys.time()
            raw <- tryCatch(
              DBI::dbGetQuery(con, sql, params = params)$Investigator_Folder,
              error = function(e) {
                write_log(log_Dashboard, "DROPDOWN_ERROR", paste0("Lab query failed: ", e$message))
                character(0)
              }
            )
            write_log(log_Dashboard, "TIMING_DROPDOWN_LAB",
                       paste0("app=", input$app, " year=", input$release_year,
                              " elapsed_secs=", round(as.numeric(Sys.time() - .t0_lab, units = "secs"), 2)))

            # 4) Normalize blanks/NA → "Missing Lab"
            labs <- ifelse(
              is.na(raw) |
                trimws(raw) == "" |
                tolower(trimws(raw)) %in% c("null", "na"),
              "Missing Lab",
              trimws(raw)
            )
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
              AND try_cast(Release_Date AS DATE) BETWEEN ? AND ?
              AND ProjStatus = 'Delivered'
            "
            params <- list(input$app, paste0(input$release_year, "-01-01"), paste0(input$release_year, "-12-31"))
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
          .t0_projid <- Sys.time()
          raw_proj <- tryCatch(
            DBI::dbGetQuery(con, sql, params = params)$Project_ID,
            error = function(e) {
              write_log(log_Dashboard, "DROPDOWN_ERROR", paste0("Project_ID query failed: ", e$message))
              character(0)
            }
          )
          write_log(log_Dashboard, "TIMING_DROPDOWN_PROJECT_ID",
                     paste0("app=", input$app, " year=", input$release_year,
                            " elapsed_secs=", round(as.numeric(Sys.time() - .t0_projid, units = "secs"), 2)))
          
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
              AND try_cast(Release_Date AS DATE) BETWEEN ? AND ?
              AND ProjStatus = 'Delivered'
            "
            params <- list(input$app, paste0(input$release_year, "-01-01"), paste0(input$release_year, "-12-31"))
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
          
          .t0_runtype <- Sys.time()
          run_raw <- tryCatch(
            DBI::dbGetQuery(con, sql, params = params)$Project_run_type,
            error = function(e) {
              write_log(log_Dashboard, "DROPDOWN_ERROR", paste0("Project_run_type query failed: ", e$message))
              character(0)
            }
          )
          write_log(log_Dashboard, "TIMING_DROPDOWN_RUN_TYPE",
                     paste0("app=", input$app, " year=", input$release_year,
                            " elapsed_secs=", round(as.numeric(Sys.time() - .t0_runtype, units = "secs"), 2)))
          
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
              AND try_cast(Release_Date AS DATE) BETWEEN ? AND ?
              AND ProjStatus = 'Delivered'
            "
              params <- list(input$app, paste0(input$release_year, "-01-01"), paste0(input$release_year, "-12-31"))
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
            .t0_metrics <- Sys.time()
            metrics_raw <- tryCatch(
              DBI::dbGetQuery(con, sql_metrics, params = params)$name,
              error = function(e) {
                write_log(log_Dashboard, "DROPDOWN_ERROR", paste0("Metrics query failed: ", e$message))
                character(0)
              }
            )
            write_log(log_Dashboard, "TIMING_DROPDOWN_METRICS",
                       paste0("app=", input$app, " year=", input$release_year,
                              " elapsed_secs=", round(as.numeric(Sys.time() - .t0_metrics, units = "secs"), 2)))
            
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
            .t0_species <- Sys.time()
            species_raw <- tryCatch(
              DBI::dbGetQuery(con, sql_species, params = params)$Species,
              error = function(e) {
                write_log(log_Dashboard, "DROPDOWN_ERROR", paste0("Species query failed: ", e$message))
                character(0)
              }
            )
            write_log(log_Dashboard, "TIMING_DROPDOWN_SPECIES",
                       paste0("app=", input$app, " year=", input$release_year,
                              " elapsed_secs=", round(as.numeric(Sys.time() - .t0_species, units = "secs"), 2)))
            
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
        
        observe({
          req(input$plt)
          if (input$plt %in% c("Flowcell", "Compare Runs")) {
            # Compare Runs manages its own Year/Lab/Run/Metric pickers inline
            # (see plot/DashboardPlotCode.R), so the shared sidebar ones would
            # just be redundant — same treatment Flowcell already gets.
            shinyjs::hide("var3"); shinyjs::hide("var5"); shinyjs::hide("var6")
          } else {
            shinyjs::show("var3"); shinyjs::show("var5"); shinyjs::show("var6")
          }
        })
        

        
        # ─────────────────────────────────────────────────────────────
        # 1) Populate Flo Flowcell
        # ─────────────────────────────────────────────────────────────
        observeEvent(
          list(input$plt, input$app, input$release_year, input$var1),
          {
            req(input$plt == "Flowcell", input$app, input$release_year, input$var1)
            
            # Identify correct source table
            source_table <- session_env$app_source_table[[input$app]]
            quoted_table <- DBI::dbQuoteIdentifier(con, source_table)
            
            # --- Year clause + params ---
            if (input$release_year == "NULL") {
              year_sql    <- "AND (ProjStatus != 'Delivered' OR Release_Date IS NULL OR TRIM(Release_Date) = '' OR LOWER(TRIM(Release_Date)) = 'null')"
              year_params <- list(input$app)
            } else {
              year_sql    <- "
              AND try_cast(Release_Date AS DATE) BETWEEN ? AND ?
              AND ProjStatus = 'Delivered'
            "
              year_params <- list(input$app, paste0(input$release_year, "-01-01"), paste0(input$release_year, "-12-31"))
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
            req(input$plt == "Flowcell", input$flo_flowcell, input$var1, input$app)
            
            # Identify correct source table
            source_table <- session_env$app_source_table[[input$app]]
            quoted_table <- DBI::dbQuoteIdentifier(con, source_table)
            
            # --- Year clause + params ---
            if (input$release_year == "NULL") {
              year_sql    <- "AND (ProjStatus != 'Delivered' OR Release_Date IS NULL OR TRIM(Release_Date) = '' OR LOWER(TRIM(Release_Date)) = 'null')"
              year_params <- list(input$app)
            } else {
              year_sql    <- "
              AND try_cast(Release_Date AS DATE) BETWEEN ? AND ?
              AND ProjStatus = 'Delivered'
            "
              year_params <- list(input$app, paste0(input$release_year, "-01-01"), paste0(input$release_year, "-12-31"))
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
            req(input$plt == "Flowcell", input$flo_runs, input$flo_flowcell, input$var1, input$app)
            
            # Identify correct source table
            source_table <- session_env$app_source_table[[input$app]]
            quoted_table <- DBI::dbQuoteIdentifier(con, source_table)
            
            # --- Year clause + params ---
            if (input$release_year == "NULL") {
              year_sql    <- "AND (ProjStatus != 'Delivered' OR Release_Date IS NULL OR TRIM(Release_Date) = '' OR LOWER(TRIM(Release_Date)) = 'null')"
              year_params <- list(input$app)
            } else {
              year_sql    <- "
              AND try_cast(Release_Date AS DATE) BETWEEN ? AND ?
              AND ProjStatus = 'Delivered'
            "
              year_params <- list(input$app, paste0(input$release_year, "-01-01"), paste0(input$release_year, "-12-31"))
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

        # ─────────────────────────────────────────────────────────────
        # Compare Runs: cascading picker updates (Year -> Lab -> Run -> Metric)
        # Lab/Run/Metric are all derived in-memory from compare_data_pool(),
        # which holds only the (max 3) years the user actually selected in
        # cmp_year — not the app's full history — so no fresh DB query is
        # needed for this part of the cascade specifically. cmp_year itself
        # changing is what triggers compare_data_pool() to re-query, scoped
        # to whatever years are now selected (see that block above).
        # ─────────────────────────────────────────────────────────────

        # Deliberately NOT keyed on input$app OR input$plt: output$plot_ui (in
        # DashboardPlotCode.R) already recomputes this entire Year->Lab->
        # Run->Metric cascade together, consistently, from scratch whenever
        # the app switches or the user switches into Compare Runs (it
        # rebuilds the picker DOM directly, with fresh choices baked in).
        # Adding input$app here too would let this observer fire in the same
        # instant using input$cmp_year/cmp_lab's still-stale (previous-app)
        # values — a real race that clobbered plot_ui's correct choices
        # with empty ones derived from a year/lab that don't exist in the
        # new app's data. Same problem with input$plt: the moment the user
        # switches Box -> Compare Runs (or back in again), this observer
        # would fire at the exact same instant output$plot_ui rebuilds the
        # cmp_lab picker's DOM from scratch — updatePickerInput() below would
        # race that DOM replacement and could hit the widget mid-teardown,
        # throwing a client-side JS error ("Cannot read properties of
        # undefined (reading 'setValue')") that aborts the render and leaves
        # the whole panel blank. req() below still gates this to only take
        # effect while in Compare Runs; it should just only react to the
        # user's own picks, not to entering the mode.
        observeEvent(input$cmp_year, {
          req(input$plt == "Compare Runs", input$cmp_year)
          pool <- compare_data_pool()
          req(pool)

          by_year <- pool[pool$cmp_year %in% input$cmp_year, ]

          if (is_genome_tech()) {
            lab_opts <- sort(unique(by_year$Investigator_Folder))
          } else {
            lab_opts <- trimws(as.character(selectedGroup()))
          }

          # Default to just the first lab, not all of them — keeps the
          # initial comparison small; users can add more labs themselves.
          prev <- isolate(input$cmp_lab)
          sel  <- intersect(prev, lab_opts)
          sel  <- if (length(sel) > 0) sel else lab_opts[1]

          freezeReactiveValue(input, "cmp_lab")
          updatePickerInput(session, "cmp_lab", choices = lab_opts, selected = sel)
        })

        # Same reasoning as above — not keyed on input$app or input$plt.
        observeEvent(input$cmp_lab, {
          req(input$plt == "Compare Runs", input$cmp_year, input$cmp_lab)
          pool <- compare_data_pool()
          req(pool)

          by_lab <- pool[pool$cmp_year %in% input$cmp_year &
                           pool$Investigator_Folder %in% input$cmp_lab, ]
          run_opts <- sort(unique(by_lab$Project_run_type))

          prev <- isolate(input$cmp_run)
          sel  <- intersect(prev, run_opts)
          sel  <- if (length(sel) > 0) sel else head(run_opts, 3)

          freezeReactiveValue(input, "cmp_run")
          updatePickerInput(session, "cmp_run", choices = run_opts, selected = sel)
        })

        # Same reasoning as above — not keyed on input$app or input$plt.
        observeEvent(input$cmp_run, {
          req(input$plt == "Compare Runs", input$cmp_year, input$cmp_lab, input$cmp_run)
          pool <- compare_data_pool()
          req(pool)

          by_run <- pool[pool$cmp_year %in% input$cmp_year &
                           pool$Investigator_Folder %in% input$cmp_lab &
                           pool$Project_run_type %in% input$cmp_run, ]

          excl <- paste0("^(", paste(c(paste0("Domain", 1:4), paste0("Species", 1:5)), collapse = "|"), ")$")
          metric_opts <- sort(unique(by_run$name[!grepl(excl, by_run$name)]))
          metric_opts <- setdiff(metric_opts, flo_only_metrics)

          pct_metrics   <- metric_opts[grepl("PCT", metric_opts)]
          std_metrics   <- metric_opts[!grepl("PCT", metric_opts)]
          metric_groups <- list("📊 Standard Metrics" = std_metrics, "📉 PCT Metrics" = pct_metrics)

          preferred <- intersect(c("Reads_Total", "Reads_Filtered"), metric_opts)
          default_metrics <- if (length(preferred) > 0) preferred else head(metric_opts, 1)

          prev <- isolate(input$cmp_metrics)
          sel  <- intersect(prev, metric_opts)
          sel  <- if (length(sel) > 0) sel else default_metrics

          freezeReactiveValue(input, "cmp_metrics")
          updatePickerInput(session, "cmp_metrics", choices = metric_groups, selected = sel)
        })

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
        # 1. Search via button click
        observeEvent(input$search_btn, {
          isolate({
            search_term <- trimws(input$global_search)
            if (nzchar(search_term)) {
              # Use a list with random token so observer always fires,
              # even when searching the same term twice in a row
              session_env$search_trigger(list(
                term  = search_term,
                token = runif(1)
              ))
            }
          })
        })
        
        # 2. Search automatically when typing (on blur or enter)
        observeEvent(input$global_search, {
          search_term <- trimws(input$global_search)
          if (nzchar(search_term)) {
            session_env$search_trigger(list(
              term  = search_term,
              token = runif(1)
            ))
          }
        })
        
        # 3. Search execution logic
        observeEvent(session_env$search_trigger(), {
          search_input <- trimws(session_env$search_trigger()$term) 
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
          # Save downstream targets
          session_env$search_lab  <- lab_found
          session_env$search_run  <- run_found
          session_env$search_data <- all_matches
          
          # Check if app or year actually changed
          app_changed  <- !identical(isolate(input$app), app_found)
          year_changed <- !identical(isolate(input$release_year), year_found)
          
          if (app_changed || year_changed) {
            # App or year is different — update them and let the chain cascade
            updateSelectInput(session, "app", selected = app_found)
            updateSelectInput(session, "release_year", selected = year_found)
          } else {
            # Same app and year — chain won't fire, so directly update var1 and var3
            updatePickerInput(session, "var1", selected = lab_found)
            later::later(function() {
              updatePickerInput(session, "var3", selected = run_found)
              session_env$search_run <- NULL
              session_env$search_lab <- NULL
            }, delay = 0.5)
          }
        })
        
        
        observeEvent(input$var1, {
          isolate({
            lab_found <- session_env$search_lab
            if (!is.null(lab_found) && lab_found %in% input$var1) {
              # Clear all other selections, select ONLY the searched lab
              updatePickerInput(session, "var1", selected = lab_found)
              session_env$search_lab <- NULL
            }
          })
        })
        
        observeEvent(input$var3, {
          isolate({
            run_found <- session_env$search_run
            if (!is.null(run_found)) {
              runs_available <- input$var3
              if (run_found %in% runs_available) {
                # Clear all other selections, select ONLY the searched run
                updatePickerInput(session, "var3", selected = run_found)
                session_env$search_run <- NULL
              }
            }
          })
        })
        
        
        ###########################################################P#####################################
        #This block react to Plot and MultiQC buttons so that it show their respecitve content
        ###########################################################P#####################################
        # Reactive value to store which button was last clicked
        selected_plot_tab <- reactiveVal("plot")
        
        # Update selected tab when buttons are clicked
        observeEvent(input$show_plot, {
          selected_plot_tab("plot")
        })
        
        observeEvent(input$show_multiqc, {
          selected_plot_tab("multiqc")
        })
        
        # Render the appropriate UI based on the button clicked
        output$plot_multiqc_switch_ui <- renderUI({
          if (selected_plot_tab() == "plot") {
            # NOTE: do NOT wrap this uiOutput in shinycssloaders::withSpinner().
            # Tried that (to show loading feedback while plot_ui recomputes)
            # and it broke Compare Runs: plot_ui's content varies a lot
            # between renders (a placeholder div, the full picker+plot
            # layout, or nothing), unlike the single steady shape
            # withSpinner() is meant for (plotOutput/tableOutput/etc), and it
            # left the panel permanently blank after switching into Compare
            # Runs. A working blank moment beats a broken spinner.
            uiOutput(ns("plot_ui"))
          } else {
            uiOutput(ns("multiqc_ui"))
          }
        })
        ###########################################################P#####################################
        #Block that handles the content shown in multiQC tab
        ###########################################################P#####################################
        
        output$multiqc_ui <- renderUI({
          selected <- input$var3
          
          if (is.null(selected) || length(selected) == 0) {
            return(div(
              style = "background-color: #f8d7da; border-left: 5px solid #f44336; padding: 15px; border-radius: 6px; font-weight: bold; font-size: 20px; color: #a94442;",
              "Select item under project run to show MultiQC."
            ))
          }
          
          if (length(selected) > 1) {
            warning_msg <- div(
              style = "background-color: #fff3cd; border-left: 5px solid #ffcc00; padding: 10px; margin-bottom: 10px; border-radius: 6px; font-size: 20px; color: #856404; font-weight: bold;",
              "⚠️ Multiple selections detected. Showing report for the first selection only."
            )
            selected <- selected[1]
          } else {
            warning_msg <- NULL
          }

          if (basename(selected) != selected || grepl("\\.\\.", selected, fixed = TRUE)) {
            return(div(
              style = "background-color: #f8d7da; border-left: 5px solid #f44336; padding: 15px; border-radius: 6px; font-weight: bold; font-size: 20px; color: #a94442;",
              "Invalid selection."
            ))
          }

          # Handle compressed .gz file
          report_gz <- paste0(selected, "_report.html.gz")
          file_gz_path <- file.path(MULTIQC_DIR, report_gz)
          
          # Define expected uncompressed path (for iframe serving)
          report_html <- paste0(selected, "_report.html")
          file_html_path <- file.path(MULTIQC_DIR, report_html)
          # If uncompressed version doesn't exist but .gz does, decompress
          
          if (!file.exists(file_html_path) && file.exists(file_gz_path)) {
            tryCatch({
              tmp_path <- paste0(file_html_path, ".tmp")
              
              if (file.exists(tmp_path)) file.remove(tmp_path)
              if (file.exists(file_html_path)) file.remove(file_html_path)  # just in case
              
              # Manually decompress using gzfile() + writeBin() with safe variable names
              gz_in <- gzfile(file_gz_path, "rb")
              html_out <- file(tmp_path, "wb")
              writeBin(readBin(gz_in, what = "raw", n = 1e8), html_out)  # n = 100 MB buffer
              close(gz_in)
              close(html_out)
              
              file.rename(tmp_path, file_html_path)
            }, error = function(e) {
              return(div(
                style = "background-color: #f8d7da; border-left: 5px solid #f44336; padding: 15px; border-radius: 6px; font-weight: bold; font-size: 20px; color: #a94442;",
                paste("Error unzipping MultiQC report:", e$message)
              ))
            })
          }
          
          # If still not found
          if (!file.exists(file_html_path)) {
            return(div(
              style = "background-color: #f8d7da; border-left: 5px solid #f44336; padding: 15px; border-radius: 6px; font-weight: bold; font-size: 20px; color: #a94442;",
              paste("MultiQC report not available for:", selected)
            ))
          }
          
          # File is served via /multiqc_reports path
          file_url <- sprintf("/multiqc_reports/%s", report_html)

          tagList(
            warning_msg,
            div(
              style = "margin-bottom: 10px; background-color: #e3f2fd; border-left: 5px solid #2196f3; padding: 10px; font-size: 20px; border-radius: 6px; display: flex; align-items: center; justify-content: space-between; flex-wrap: wrap; gap: 10px;",
              tags$p(style = "font-weight: bold; margin: 0; color: #0d47a1;",
                     paste("Currently viewing MultiQC for:", selected)),
              tags$a(
                href = file_url,
                download = report_html,
                style = "background-color: #185FA5; border: none; font-weight: 500; color: #fff;
                         padding: 8px 16px; border-radius: 8px; text-decoration: none; font-size: 16px;
                         white-space: nowrap;",
                onmouseover = "this.style.backgroundColor='#0C447C'",
                onmouseout = "this.style.backgroundColor='#185FA5'",
                "⬇ Download MultiQC Report"
              )
            ),
            div(
              style = "width: 100%; height: 75vh; overflow: hidden; padding: 0; margin: 0;",
              tags$div(
                style = "height: 100%; overflow-y: auto; border: 1px solid #ccc; border-radius: 6px; box-shadow: 0 2px 8px rgba(0,0,0,0.1);",
                tags$iframe(
                  src = file_url,
                  style = "width: 100%; height: 100%; border: none;",
                  sandbox = "allow-scripts allow-same-origin"
                )
              )
            )
          )
        })
        
        ########################
        #Periodic Cleanup of uncompressed html multiqc files for space managment
        ########################
        clean_old_multiqc_files <- function(path = MULTIQC_DIR, age_minutes = 30) {
          files <- list.files(path, pattern = "_report\\.html$", full.names = TRUE)
          for (f in files) {
            if (difftime(Sys.time(), file.info(f)$mtime, units = "mins") > age_minutes) {
              file.remove(f)
            }
          }
        }

        # Run cleanup every hour
        later::later(function() {
          clean_old_multiqc_files()
          later::later(clean_old_multiqc_files, delay = 3600)
        }, delay = 10)


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
            " AND try_cast(Release_Date AS DATE) BETWEEN ? AND ?
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
          if (input$release_year != "NULL") params <- c(params, paste0(input$release_year, "-01-01"), paste0(input$release_year, "-12-31"))
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
          DT::datatable(downloadfunc(), options = list(scrollX = TRUE))
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
        # output$species_table <- DT::renderDataTable({
        #   req(selected_species_view() == "table")
        #   species_download_data()
        # })
        output$species_table <- DT::renderDataTable({
          req(selected_species_view() == "table")
          DT::datatable(species_download_data(), options = list(scrollX = TRUE))
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
              style = "color: #4497f2; font-weight: bold; font-size: 22px;"
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
    ####################################################################################
    # ── Navbar right items — only shown after successful login ──────────────────────
    # authenticated_group() is NULL before login; non-NULL after.
    # This output lives outside the is_admin() check so it always evaluates.
    ####################################################################################
    output$navbar_authenticated_items <- renderUI({
      req(authenticated_group())
      req(!isTRUE(authenticated_group() == "Admin"))
      req(isTRUE(main_tab() == "Login to QC metrics dashboard"))
      req(isTRUE(dashboard_ready()))
      
      div(class = "gt-navbar-right",
          
          div(class = "global-search-wrapper",
              div(class = "global-search-input-container",
                  div(class = "global-search-input",
                      textInput(ns("global_search"),
                                label       = NULL,
                                placeholder = "by 'PROJECT RUN TYPE' e.g. 23-li-004-run2",
                                width       = "100%")
                  ),
                  tags$span(class = "clear-search", HTML("&times;"))
              ),
              actionButton(ns("search_btn"),
                           label = icon("search"),
                           class = "global-search-button")
          ),
          
          # Launches the animated guided tour (see www/gt-tour.js).
          actionLink(
            ns("about_info"),
            label = tags$span(class = "gt-nav-link",
                              icon("info-circle"),
                              "Quick Tour")
          ),
          
          uiOutput(ns("menu_task_items")),
          
          tags$img(src   = "GTlogo.png",
                   height = "30px",
                   style  = "vertical-align: middle; opacity: 0.9;")
      )
    })
    ####################################################################################
    # ── Refetch bar — shown in tab row, only when logged in to QC dashboard ──────────
    ####################################################################################
    output$refetch_bar_ui <- renderUI({
      req(authenticated_group())
      req(!isTRUE(authenticated_group() == "Admin"))
      req(isTRUE(main_tab() == "Login to QC metrics dashboard"))
      req(isTRUE(dashboard_ready()))
      
      div(
        style = paste(
          "display: flex;",
          "align-items: center;",
          "gap: 8px;",
          "font-family: 'Inter', sans-serif;"
        ),
        
        # Timestamp
        uiOutput(ns("last_refresh_text_tabrow")),
        
        # Refetch button — steel blue, visible but not aggressive
        actionButton(
          ns("refresh_app_data"),
          label = tagList(
            tags$i(class = "fas fa-sync-alt", style = "margin-right: 5px;"),
            "Refetch live data here"
          ),
          style = paste(
            "background-color: #185FA5;",
            "color: white;",
            "font-weight: 500;",
            "font-size: 12px;",
            "border: none;",
            "border-radius: 8px;",
            "padding: 5px 12px;",
            "box-shadow: none;",
            "cursor: pointer;"
          )
        )
      )
    })
    ################
    output$last_refresh_text_tabrow <- renderUI({
      time <- last_refresh_time_shared()
      text <- if (!is.null(time)) {
        paste0("🕒 Last fetched: ", format(time, "%Y-%m-%d at %H:%M:%S"))
      } else {
        "🕒 No refresh yet"
      }
      tags$span(
        style = "font-size: 12px; color: #64748b; white-space: nowrap;",
        text
      )
    })
    ####################################ADMIN PAGE################################################
    source(userSelfEmailUpdate, local = TRUE)
    source(AdminPage, local = TRUE)
    #############################################################################################
    
  })
}


