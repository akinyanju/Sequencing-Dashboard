server <- function(input, output, session) {
  
  dashboard_ready <- reactiveVal(FALSE)
  ####################################################################################
  #check for regular update
  # observe({
  #   invalidateLater(10000)  # every 10 sec
  #   isolate({
  #     update_duckdb_from_csv()
  #   })
  # })
  ####################################################################################
  ###user login function. Capture login access even if cookie was activated
  log_access <- function(group, email) {
    timestamp <- Sys.time()
    log_entry <- data.frame(
      date = format(timestamp, "%Y-%m-%d"),
      time = format(timestamp, "%H:%M:%S"),
      group = group,
      email = email,
      stringsAsFactors = FALSE
    )
    if (!file.exists(log_file)) {
      write.csv(log_entry, log_file, row.names = FALSE)
    } else {
      write.table(log_entry, log_file, row.names = FALSE, col.names = FALSE, sep = ",", append = TRUE)
    }
  }
  ####################################################################################
  # ---- Reactive Polling to Auto-Update users_data including Group and Email ----
  users_data_reactive <- reactivePoll(
    intervalMillis = 10000,
    session = session,
    checkFunc = function() file.info(json_path)$mtime,
    valueFunc = function() {
      users_data <- jsonlite::fromJSON(readLines(json_path, warn = FALSE))
      valid_groups <- names(users_data)
      expanded_users_data <- users_data
      gt_groups <- grep("^GenomeTechnologies", valid_groups, value = TRUE)
      if (length(gt_groups) > 0) {
        all_gt_emails <- unique(unlist(users_data[gt_groups]))
        for (g in gt_groups) {
          expanded_users_data[[g]] <- unique(c(expanded_users_data[[g]], all_gt_emails))
        }
      }
      list(raw = users_data, valid_groups = valid_groups, expanded = expanded_users_data)
    }
  )
  ####################################################################################
  # Reactive values to store user authentication and app state
  values <- reactiveValues(
    stage = "group",
    selected_group = NULL,
    selected_email = NULL,
    authenticated = FALSE,
    authenticated_group = NULL,
    app_stage = "message",
    first_name = NULL
  )
  
  ####################################################################################
  #This is to be used only in dev mode. Always hash out. This ensures session is cleared from remembering
  #if (interactive()) {
  #  runjs("localStorage.clear(); sessionStorage.clear();")
  #}
  ####################################################################################
  #This JavaScript code is embedded to automatically restore a user's session if they have previously logged in and utlized remmeber me. 
  #Admin credentials are not allowed to be remembered
  
  runjs("
(function() {
  const auth = localStorage.getItem('authenticated');
  const timestamp = localStorage.getItem('auth_timestamp');
  const group = localStorage.getItem('selected_group');
  const email = localStorage.getItem('selected_email');

  if (auth === 'true' && timestamp && group !== 'Admin') {
    const now = new Date();
    const then = new Date(timestamp);
    const daysDiff = (now - then) / (1000 * 60 * 60 * 24);

    if (daysDiff < 30) {
      Shiny.setInputValue('restore_session', Math.random());
      Shiny.setInputValue('restored_group', group);
      Shiny.setInputValue('restored_email', email);
    } else {
      localStorage.removeItem('authenticated');
      localStorage.removeItem('auth_timestamp');
      localStorage.removeItem('selected_group');
      localStorage.removeItem('selected_email');
    }
  } else {
    // Also clear if group is Admin or anything else unexpected
    localStorage.removeItem('authenticated');
    localStorage.removeItem('auth_timestamp');
    localStorage.removeItem('selected_group');
    localStorage.removeItem('selected_email');

    if (sessionStorage.getItem('authenticated') === 'true') {
      Shiny.setInputValue('restore_session', Math.random());
      Shiny.setInputValue('restored_group', sessionStorage.getItem('selected_group'));
      Shiny.setInputValue('restored_email', sessionStorage.getItem('selected_email'));
      }
    }
  }
  )();")
  
  ####################################################################################
  #Persist email in browser storage. To restore values$selected_email in session restore, save it during login:
  
  # Restore session
  observeEvent(input$restore_session, {
    values$authenticated <- TRUE
    values$app_stage <- "plot"
    
    shinyjs::delay(500, {
      runjs("Shiny.setInputValue('restored_group', localStorage.getItem('selected_group') || sessionStorage.getItem('selected_group'));")
      runjs("Shiny.setInputValue('restored_email', sessionStorage.getItem('selected_email'));")
    })
    
  })
  ####################################################################################
  # Restore group
  observeEvent(input$restored_group, {
    values$selected_group <- input$restored_group
    values$authenticated_group <- input$restored_group
  })
  ####################################################################################
  # Restore email and log access only after both are restored
  observeEvent(input$restored_email, {
    values$selected_email <- input$restored_email
    
    first_name <- sub("\\..*", "", input$restored_email)
    first_name <- paste0(toupper(substring(first_name, 1, 1)), substring(first_name, 2))
    
    if (!is.null(values$authenticated_group)) {
      #showNotification("Welcome back! You are still authenticated.", type = "message")
      showNotification(paste0("Welcome back, ", first_name, "! You are still authenticated."),type = "message")
      log_access(values$authenticated_group, values$selected_email)
    }
  })
  ####################################################################################
  
  # ---- Manual or auto logout ----
  observeEvent(input$logout, {
    session$userData$logout_trigger <- TRUE
    values$stage <- "group"
    values$selected_group <- NULL
    values$selected_email <- NULL
    values$authenticated <- FALSE
    values$app_stage <- "message"
    runjs("sessionStorage.removeItem('authenticated');")
    showNotification("Logged out.", type = "error")
  })
  ####################################################################################
  # ---- Logout Button UI ----
  output$logout_ui <- renderUI({
    if (values$authenticated) {
      div(
        style = "position: fixed; bottom: 20px; right: 20px; z-index: 1000;",
        actionButton(
          "logout",
          label = tagList(icon("sign-out-alt"), "Logout"),
          style = "background-color:  #4497f2; color: white; border: none; font-size: 16px;",
          class = "btn btn-sm"
        )
      )
    } else {
      NULL
    }
  })
  ####################################################################################
  # ---- Dynamic UI Wrapper ----
  output$dynamic_ui_wrapper <- renderUI({
    div(
      class = "center-box",
      uiOutput("logout_ui"),
      uiOutput("header_ui"),
      uiOutput("main_ui")
    )
  })
  ####################################################################################
  # ---- Header UI (Only shows if user is not authenticated and no email edge case). Welcome message is also restricted to select group and email----
  # req(values$selected_group, values$authenticated, values$stage)
  output$header_ui <- renderUI({
    if (!values$authenticated && values$stage %in% c("group", "email")) {
      h2("Welcome to GT QC Metrics Dashboard", 
         style = "text-align: center; margin-top: 40px; margin-bottom: 20px; color: black;")
    } else {
      NULL
    }
  })
  ####################################################################################
  output$main_ui <- renderUI({
    if (!values$authenticated) {
      switch(values$stage,
             "group" = tagList(
               div(
                 class = "center-container",
                 style = "display: flex;justify-content: center;align-items: flex-start;padding-top: 150px;min-height: 100vh;",
                 div(
                   style = "width: 400px; background-color: #e6f2ff; padding: 20px; border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.1);",
                   div(class = "form-group",
                       tags$label("Select Your Group", `for` = "group"),
                       selectInput("group", label = NULL, choices = c("", setdiff(users_data_reactive()$valid_groups, c("NULL", "jax"))), width = "100%")
                   ),
                   div(class = "next-button-container",
                       actionButton("next_to_email", "Next", class = "btn btn-primary btn-block")
                   )
                 )
               )
             ),
             "email" = {
               req(values$selected_group)
               emails <- users_data_reactive()$expanded[[values$selected_group]]
               if (length(emails) == 0) {
                 tagList(
                   tags$p(
                     style = "font-size: 16px; text-align: center;",
                     tags$span("No emails found in this group. ", style = "color: brown;"),
                     tags$span("Please ask your PI to make this request to: ", style = "color: black;"),
                     tags$a("gtdrylab@jax.org", href = "mailto:gtdrylab@jax.org")
                   ),
                   actionButton("back", "Back")
                 )
               } else {
                 div(
                   class = "center-container",
                   style = "display: flex;justify-content: center;align-items: flex-start;padding-top: 150px;min-height: 100vh;",
                   div(
                     style = "width: 400px; background-color: #e6f2ff; padding: 20px; border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.1);",
                     selectInput("email", "Select Your Email", choices = c("", emails), width = "100%"),
                     div(style = "margin-top: 15px;",
                         actionButton("send_otc", "Send One-Time Code", class = "btn btn-success btn-block"),
                         tags$br(),
                         actionButton("back", "Back", class = "btn btn-secondary btn-block"),
                         tags$p(
                           em("...missing email, contact "),
                           tags$a("gtdrylab@jax.org", href = "mailto:gtdrylab@jax.org"),
                           style = "font-size: 12px; font-style: italic; color: #555; text-align: right; margin-top: 10px;"
                         )
                     )
                   )
                 )
               }
             },
             "verify" = div(
               class = "center-container",
               style = "display: flex;justify-content: center;align-items: flex-start;padding-top: 150px;min-height: 100vh;",
               div(
                 style = "width: 400px; background-color: #e6f2ff; padding: 20px; border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.1);",
                 passwordInput("entered_otc", "Enter One-Time Code", width = "100%"),
                 #Conditionally show "Remember me"
                 if (values$selected_group != "Admin") {
                   checkboxInput("remember_me", "Remember me for 30 days", value = FALSE)
                 },
                 div(style = "margin-top: 15px;",
                     actionButton("verify_otc", "Verify", class = "btn btn-success btn-block"),
                     tags$br(),
                     actionButton("resend_otc", "Resend Code", class = "btn btn-warning btn-block"),
                     tags$br(),
                     actionButton("back", "Back", class = "btn btn-secondary btn-block")
                 )
               )
             )
      )
    } else {
      if (values$app_stage == "message") {
        req(values$selected_email)
        
        # Extract and format first name from email (e.g., j.smith@ -> J)
        first_name <- sub("\\..*", "", values$selected_email)
        first_name <- paste0(toupper(substring(first_name, 1, 1)), substring(first_name, 2))
        
        tagList(
          div(
            style = "display: flex; justify-content: center; align-items: center; height: 50vh;",
            div(
              style = "text-align: center;",
              
              # Admin-specific welcome message
              if (values$authenticated_group == "Admin") {
                tagList(
                  tags$p(
                    paste0("You are now entering the Admin Page, ", first_name, "."),
                    style = "font-size: 24px; color: #B22222; font-weight: bold;"
                  ),
                  tags$p(
                    "Take caution!",
                    style = "font-size: 22px; color: #800000;"
                  )
                )
                
                # Genome Technologies message
              } else if (startsWith(values$authenticated_group, "GenomeTechnologies")) {
                tagList(
                  tags$p(
                    paste0("Dear ", first_name, ","),
                    style = "font-size: 24px; color: black; font-weight: bold;"
                  ),
                  tags$p(
                    tagList(
                      "As a ",
                      tags$span("Genome Technologies member", style = "color: #0072B2; font-weight: bold;"),
                      ", you have full access to the QC metrics in this dashboard."
                    ),
                    style = "font-size: 24px; color: black;"
                  )
                )
                
                # Default for other groups
              } else {
                tagList(
                  tags$p(
                    paste0("Dear ", first_name, ","),
                    style = "font-size: 24px; color: black; font-weight: bold;"
                  ),
                  tags$p(
                    tagList(
                      "You are restricted to see: ",
                      tags$span(values$authenticated_group, style = "color: #0072B2; font-weight: bold;"),
                      " QC metrics."
                    ),
                    style = "font-size: 24px; color: black;"
                  )
                )
              },
              
              # Continue button for all users
              actionButton("continue_to_plot", "Continue", class = "btn btn-primary")
            )
          )
        )
      } else if (values$app_stage == "plot") {
        module2_header_side_body()
      }
    }
  })
  ####################################################################################
  # ---- Navigation Events ----
  observeEvent(input$next_to_email, {
    if (input$group == "") {
      showNotification("Please select a group.", type = "error", duration = 5)
      return()
    }
    values$selected_group <- input$group
    values$stage <- "email"
  })
  
  observeEvent(input$back, {
    if (values$stage == "email") {
      values$stage <- "group"
      values$selected_group <- NULL
    } else if (values$stage == "verify") {
      values$stage <- "email"
    }
  })
  ####################################################################################
  # ---- Send One-Time Code to User ----

  send_code_to_user <- function(email) {
    issue_one_time_code(values$selected_group, email)
  }
  
  ####################################################################################
  ####################################################################################
  ####################################################################################
  ####################################################################################
  #DEV MODE CODE ONLY. IT MUST BE COMMENTED OUT WHEN SCRIPT IS IN PRODUCTION MODE.
  # —— DEV MODE: show debug code only ——————————————————————
   # observeEvent(input$send_otc, {
   #   req(input$email)
   #   if (input$email == "") {
   #     showNotification("Please select an email.", type = "error", duration = 5)
   #     return()
   #   }
   #   values$selected_email <- input$email
   # 
   #   # generate + store
   #   code <- sprintf("%06d", sample(0:999999, 1))
   #   otc_storage[[input$email]] <- list(code = code, timestamp = Sys.time())
   # 
   #   # *** LOG DEV SEND ***
   #   write_log(
   #     log_Dashboard,
   #     "OTC_DEV_SENT",
   #     paste0("Mode=DEV, Email='", input$email, "', Code='", code, "'")
   #   )
   # 
   #   # debug popup only
   #   showNotification(paste("DEBUG CODE:", code), type = "message", duration = 15)
   #   values$stage <- "verify"
   # })
   # 
   # observeEvent(input$send_otc, {
   #   req(input$email)
   #   if (input$email == "") {
   #     showNotification("Please select an email.", type = "error", duration = 5)
   #     return()
   #   }
   #   values$selected_email <- input$email
   #   code <- sprintf("%06d", sample(0:999999, 1))
   #   otc_storage[[input$email]] <- list(code = code, timestamp = Sys.time())
   #   showNotification(paste("DEBUG CODE:", code), type = "message", duration = 15) # For testing only
   #   values$stage <- "verify"
   # })
  ####################################################################################
  ####################################################################################
  ####################################################################################
  ####################################################################################
  #PRODUCTION CODE. IT MUST BE UNCOMMENTED BEFORE USE INSIDE CTGENOMTECH03. IT MUST BE COMMENTED OUT WHEN USED IN DEV MODE
  #—— PRODUCTION MODE: actually email the code —————————————————
  observeEvent(input$send_otc, {
    req(input$email)
    values$selected_email <- input$email

    if (!mailer_available()) {
      showNotification("Mailer not available.", type = "error")
      return()
    }

    # *** LOG PRODUCTION SEND ATTEMPT ***
    write_log(
      log_Dashboard,
      "OTC_PROD_SEND_ATTEMPT",
      paste0("Mode=PROD, sending to Email='", input$email, "'")
    )

    if (issue_one_time_code(values$selected_group, input$email)) {
      showNotification("One-time code sent!", type = "message", duration = 10)

      # *** LOG PRODUCTION SEND SUCCESS ***
      write_log(
        log_Dashboard,
        "OTC_PROD_SENT",
        paste0("Mode=PROD, Email='", input$email, "'")
      )

      values$stage <- "verify"
    } else {
      showNotification("Failed to send code. Contact gtdrylab@jax.org", type = "error", duration = 15)

      # *** LOG PRODUCTION SEND FAILURE ***
      write_log(
        log_Dashboard,
        "OTC_PROD_SEND_FAILED",
        paste0("Mode=PROD, Email='", input$email, "'")
      )
    }
  })


  ####################################################################################
  
  # —— PRODUCTION RESEND observer —————————————————————————
  observeEvent(input$resend_otc, {
    req(values$selected_email)
    
    if (!mailer_available()) {
      showNotification("Mailer not available.", type = "error")
      return()
    }
    
    # *** LOG PRODUCTION RESEND ATTEMPT ***
    write_log(
      log_Dashboard,
      "OTC_PROD_RESEND_ATTEMPT",
      paste0("Mode=PROD, resending to Email='", values$selected_email, "'")
    )
    
    if (issue_one_time_code(values$selected_group, values$selected_email)) {
      showNotification("A new one-time code has been sent.", type = "message", duration = 8)
      
      # *** LOG PRODUCTION RESEND SUCCESS ***
      write_log(
        log_Dashboard,
        "OTC_PROD_RESENT",
        paste0("Mode=PROD, Email='", values$selected_email, "'")
      )
    } else {
      showNotification("Mailer not available.", type = "error")
      
      # *** LOG PRODUCTION RESEND FAILURE ***
      write_log(
        log_Dashboard,
        "OTC_PROD_RESEND_FAILED",
        paste0("Mode=PROD, Email='", values$selected_email, "'")
      )
    }
  })
  
  ####################################################################################
  # ---- Verify OTC Code ----
  # how many seconds before an OTC expires?
  code_expiration_secs <- 300  # e.g. 5 minutes
  
  observeEvent(input$verify_otc, {
    req(input$entered_otc)
    entry <- otc_storage[[values$selected_email]]
    
    if (is.null(entry)) {
      showNotification("No code found. Please request again.", type = "error")
      return()
    }
    
    expired <- difftime(Sys.time(), entry$timestamp, units = "secs") > code_expiration_secs
    if (length(expired) && expired) {
      showNotification("Code expired. Please resend.", type = "error")
      return()
    }
    
    if (input$entered_otc == entry$code) {
      values$authenticated <- TRUE
      values$authenticated_group <- values$selected_group
      values$app_stage <- "message"
      
      # JS sessionStorage for current session
      runjs("sessionStorage.setItem('authenticated', 'true');")
      runjs(paste0("sessionStorage.setItem('selected_group', '", values$selected_group, "');"))
      runjs(paste0("sessionStorage.setItem('selected_email', '", values$selected_email, "');"))
      
      # Store user session to localStorage for 30 days — exclude Admin group
      if (isTRUE(input$remember_me) && values$selected_group != "Admin") {
        runjs(sprintf("
        localStorage.setItem('authenticated', 'true');
        localStorage.setItem('selected_group', '%s');
        localStorage.setItem('selected_email', '%s');
        localStorage.setItem('auth_timestamp', '%s');",
                      values$selected_group,
                      values$selected_email,
                      format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ")
        ))
      }
      
      showNotification(paste("Authentication successful!", sep = ""), type = "message")
      
      # Log access
      log_access(values$selected_group, values$selected_email)
      
    } else {
      showNotification("Invalid code. Try again.", type = "error")
    }
  })
  ####################################################################################
  # ---- Continue Button ----
  # observeEvent(input$continue_to_plot, {
  #   values$app_stage <- "plot"
  # })
  observeEvent(input$continue_to_plot, {
    values$app_stage <- "plot"
    dashboard_ready(TRUE)   # ✅ Triggers app module setup once
  })
  
  ####################################################################################
  # ---- Reactive function to get selected group usable to filter data inside module2_Server() ----
  selectedGroup <- reactive({
    values$selected_group
  })
  ####################################################################################
  # ---- Reactive function to get authenticated group usable for filtering in module2_Server ----
  authenticatedGroup <- reactive({
    values$authenticated_group
  })
  ####################################################################################
  # ---- Conditional Module Launch for Authenticated Users and then pass the select group to the module2_Server ----
  observe({
    if (values$authenticated && values$app_stage == "plot") {
      #module2_Server("Module2", selectedGroup = selectedGroup(), authenticated_group = authenticatedGroup()) 
      #module2_Server("Module2", selectedGroup = selectedGroup, authenticated_group = authenticatedGroup)
      module2_Server(
        "Module2",
        selectedGroup = selectedGroup,
        authenticated_group = authenticatedGroup,
        dashboard_ready = dashboard_ready   # ✅ New argument
      )
      
    }
  })
  ####################################################################################
  # ---- Always Load Static Module ----
  module1_Server("Module1")
  ####################################################################################
}

