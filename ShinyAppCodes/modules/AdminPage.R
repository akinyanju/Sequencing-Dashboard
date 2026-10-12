# ==== Admin Authentication UI Toggle ====
observe({
  req(authenticated_group())
  is_admin <- authenticated_group() == "Admin"
  
  shinyjs::delay(1000, {
    js_code <- if (is_admin) {
      "
      // Always hide sidebar for admin immediately
      $('body').addClass('sidebar-collapse');
      $('#sidebar').hide();
      $('#main-content').hide();
      $('a.sidebar-toggle[data-toggle=\"offcanvas\"]').hide();
      $('a.sidebar-toggle[data-toggle=\"offcanvas\"]').css('display', 'none !important');
      $('<style id=\"admin-hide-hamburger\">a.sidebar-toggle[data-toggle=\"offcanvas\"] { display: none !important; }</style>').appendTo('head');
      $('.sidebar-toggle').hide();
      $('[data-toggle=\"offcanvas\"]').hide();
      $('.navbar-toggle').hide();
      $('button.sidebar-toggle').hide();
      $('#plot_ui').hide();

      // Hide header elements irrelevant to admin
      $('.navbar-custom-menu').hide();
      $('.global-search-box').hide();
      $('#global_search').closest('div').hide();
      $('a[id*=\"quick_tour\"]').hide();
      $('.quick-tour-btn').hide();
      $('[id*=\"quick_tour\"]').hide();
      $('.navbar-header .navbar-brand span').hide();
      $('.navbar-brand').hide();
      $('.logo').hide();
      $('.main-header .logo').hide();

      // Inject persistent CSS for header — one selector per style tag to avoid line break issues
      $('<style id=\"admin-hide-brand\">.navbar-brand { display: none !important; }</style>').appendTo('head');
      $('<style id=\"admin-hide-logo\">.logo { display: none !important; }</style>').appendTo('head');
      $('<style id=\"admin-hide-menu\">.navbar-custom-menu { display: none !important; }</style>').appendTo('head');
      $('<style id=\"admin-hide-search\">.global-search-box { display: none !important; }</style>').appendTo('head');
      $('<style id=\"admin-hide-tour\">[id*=quick_tour] { display: none !important; }</style>').appendTo('head');

      // Only hide/show tabs if admin_tab exists in DOM
      var adminTabExists = $('ul.nav-tabs li a[data-value=\"admin_tab\"]').length > 0;
      if (adminTabExists) {
        $('ul.nav-tabs li a').each(function() {
          if ($(this).attr('data-value') !== 'admin_tab') {
            $(this).parent().hide();
          } else {
            $(this).parent().show();
          }
        });
      }
      "
    } else {
      "
      // Remove all persistent admin CSS
      $('#admin-hide-hamburger').remove();
      $('#admin-hide-brand').remove();
      $('#admin-hide-logo').remove();
      $('#admin-hide-menu').remove();
      $('#admin-hide-search').remove();
      $('#admin-hide-tour').remove();

      // Show all tabs and sidebar for normal users
      $('ul.nav-tabs li a').parent().show();
      $('#plot_ui').show();
      $('body').removeClass('sidebar-collapse');
      $('#sidebar').show();
      $('#main-content').show();
      $('.sidebar-toggle').show();
      $('[data-toggle=\"offcanvas\"]').show();
      $('.navbar-toggle').show();
      $('button.sidebar-toggle').show();

      // Restore header for non-admin
      $('.navbar-custom-menu').show();
      $('.global-search-box').show();
      $('#global_search').closest('div').show();
      $('a[id*=\"quick_tour\"]').show();
      $('.quick-tour-btn').show();
      $('[id*=\"quick_tour\"]').show();
      $('.navbar-header .navbar-brand span').show();
      $('.navbar-brand').show();
      $('.logo').show();
      $('.main-header .logo').show();
      "
    }
    
    shinyjs::runjs(js_code)
  })
})

# Forcefully clear admin UI when leaving Admin mode
observe({
  req(authenticated_group())
  
  if (authenticated_group() != "Admin") {
    shinyjs::runjs("$('#admin_page').remove();")  # Remove the entire admin page div if still hanging
  }
})

# -------------- LOGIN LOG PLOT -------------------
# Reactive for reading the login log
# login_log_data <- reactive({
#   req(file.exists(log_file))
#   df <- read.csv(log_file, stringsAsFactors = FALSE)
#   df$datetime <- as.POSIXct(paste(df$date, df$time), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
#   df
# })
login_log_data <- reactive({
  req(cached_log_data$login)
  cached_log_data$login
})


# Reactive value to store time filter mode
current_view <- reactiveVal("all")

# Buttons to control time granularity
observeEvent(input$view_login_daily,  { current_view("daily") })
observeEvent(input$view_login_weekly, { current_view("weekly") })
observeEvent(input$view_login_monthly,{ current_view("monthly") })
observeEvent(input$view_login_yearly, { current_view("yearly") })
observeEvent(input$view_login_all,    { current_view("all") })

# Filter and aggregate login data
filtered_login_data <- reactive({
  df <- login_log_data()
  view_mode <- current_view()
  
  df <- df %>%
    mutate(group_col = dplyr::case_when(
      view_mode == "daily" ~ as.Date(datetime),
      view_mode == "weekly" ~ lubridate::floor_date(datetime, "week"),
      view_mode == "monthly" ~ lubridate::floor_date(datetime, "month"),
      view_mode == "yearly" ~ lubridate::floor_date(datetime, "year"),
      TRUE ~ as.Date(datetime)
    )) %>%
    group_by(group_col) %>%
    summarise(
      count = n(),
      users_list = list(paste(email, group, time, sep = " | ")),  # keep as individual entries, one per user
      users_preview = {
        user_list <- paste(email, group, time, sep = " | ")
        preview <- if (length(user_list) > 3) {
          paste(paste(user_list[1:3], collapse = "<br>"), "<br><i>...and click bar for more</i>")
        } else {
          paste(user_list, collapse = "<br>")
        }
        preview
      },
      .groups = "drop"
    ) %>%
    rename(date = group_col)
})


# Plot login logs with bar and line, and hover preview
output$login_log_plot <- renderPlotly({
  df <- filtered_login_data()
  
  plot_ly(df, x = ~date, source = "login_plot") %>%
    add_bars(y = ~count, name = "User Count", marker = list(color = '#74add1'),
             hovertemplate = paste(
               "<b>Date:</b> %{x}<br>",
               "<b>Count:</b> %{y}<br>",
               "<b>Users:</b><br>%{customdata}<extra></extra>"
             ),
             customdata = df$users_preview) %>%
    add_lines(y = ~count, name = "User Trend", line = list(color = '#0570b0')) %>%
    layout(
      hovermode = "closest",
      title = "Login Logs",
      xaxis = list(title = "Date"),
      yaxis = list(title = "Number of Logins"),
      showlegend = FALSE
    )
})

render_login_log_ui <- function(ns) {
  tagList(
    div(
      style = "margin-bottom: 10px; display: flex; gap: 10px; flex-wrap: wrap; align-items: center;",
      
      # Time filter buttons
      div(
        style = "display: flex; gap: 10px; flex-wrap: wrap;",
        actionButton(ns("view_login_daily"), "By Day"),
        actionButton(ns("view_login_weekly"), "By Week"),
        actionButton(ns("view_login_monthly"), "By Month"),
        actionButton(ns("view_login_yearly"), "By Year"),
        actionButton(ns("view_login_all"), "All Time")
      ),
      
      # Refresh button floated to the right
      div(
        style = "margin-left: auto;",
        actionButton(
          ns("refresh_login_logs"),
          label = "🔄 Refresh Plot",
          style = "background-color: #185FA5; border: none; font-weight: 500; color: #fff; padding: 8px 16px; border-radius: 8px;"
        )
      )
    ),
    
    downloadButton(ns("download_login_log"), "Download Full Login Log"),
    plotlyOutput(ns("login_log_plot"), height = "500px"),
    textOutput(ns("last_login_refresh"))
  )
}

observe({
  req(authenticated_group() == "Admin")
  
  # Automatically show 'All Time' logs UI
  current_view("all")
  output$admin_log_ui <- renderUI({
    render_login_log_ui(ns)
  })
})

output$download_login_log <- downloadHandler(
  filename = function() {
    paste0("LoginLog_", Sys.Date(), ".csv")
  },
  content = function(file) {
    write.csv(login_log_data(), file, row.names = FALSE)
  }
)


# Show plot in UI
observeEvent(input$view_login_logs, {
  current_view("all")
  
  output$admin_log_ui <- renderUI({
    render_login_log_ui(ns)
  })
})



observeEvent(event_data("plotly_click", source = "login_plot"), {
  click_data <- event_data("plotly_click", source = "login_plot")
  if (is.null(click_data)) return(NULL)
  
  clicked_date <- as.Date(click_data$x)
  
  # Filter the login data for the clicked date (no sorting by datetime)
  full_list <- filtered_login_data() %>%
    filter(date == clicked_date) %>%
    pull(users_list) %>%
    unlist()
  
  # Debugging: Print the full list to check if data is correct
  #print(length(full_list))  # To check if data is being passed correctly
  #print(head(full_list))    # To check the first few items in the list
  
  showModal(modalDialog(
    title = paste("User Log -", format(clicked_date, "%b %d, %Y")),
    easyClose = TRUE,
    footer = modalButton("Close"),
    size = "l",  # large modal
    div(
      style = "max-height: 400px; overflow-y: auto; padding: 10px;",  # Ensure the div has scroll functionality
      HTML(paste(htmltools::htmlEscape(full_list), collapse = "<br/>"))  # Pass the full list of users here
    )
  ))
})

# Store the last refresh time in a reactiveVal
last_login_refresh_time <- reactiveVal(NULL)

observeEvent(input$refresh_login_logs, {
  if (file.exists(log_file)) {
    cached_log_data$login <- read.csv(log_file, stringsAsFactors = FALSE) %>%
      mutate(datetime = as.POSIXct(paste(date, time), format = "%Y-%m-%d %H:%M:%S"))
    
    last_login_refresh_time(Sys.time())  # ⬅️ update the timestamp here
    showNotification("Login log refreshed", type = "message")
  } else {
    showNotification("Login log file not found.", type = "error")
  }
})

output$last_login_refresh <- renderText({
  req(last_login_refresh_time())
  paste("Last refreshed at:", format(last_login_refresh_time(), "%Y-%m-%d %H:%M:%S"))
})



# # -------- LOG CONSOLE OUTPUT HANDLERS --------

# --- LOG TABLE RENDER HELPERS ---
render_log_table <- function(data) {
  req(data)
  data <- data[order(data$datetime, decreasing = TRUE), ]
  data[, !names(data) %in% "datetime", drop = FALSE]  # internal sort key only, not meaningful to users
}

render_log_ui <- function(ns, button_id, label, table_id, output_id) {
  tagList(
    actionButton(
      ns(button_id), label,
      style = "background-color: #185FA5; border: none; font-weight: 500; color: #fff;
               padding: 8px 16px; border-radius: 8px; transition: background-color 0.3s;
               cursor: pointer;",
      onmouseover = "this.style.backgroundColor='#0C447C'",
      onmouseout = "this.style.backgroundColor='#185FA5'"
    ),
    div(
      style = "height: 400px; overflow-y: scroll; background-color: #f9f9f9; padding: 10px;",
      tags$style(paste0("#", ns(table_id), " td { white-space: nowrap; }")),
      tableOutput(ns(output_id))
    )
  )
}

# --- SEQMET LOG ---
output$seqmet_log_console <- renderTable({
  render_log_table(cached_log_data$seqmet)
}, striped = TRUE, spacing = "s", bordered = TRUE)

observeEvent(input$view_seqmet_log, {
  cached_log_data$seqmet <- read_log_data(log_SeqMet)
  output$admin_log_ui <- renderUI({
    render_log_ui(ns, "refresh_seqmet_log", "🔁 Refresh Sequencing Log", "seqmet_log_console", "seqmet_log_console")
  })
})

observeEvent(input$refresh_seqmet_log, {
  cached_log_data$seqmet <- read_log_data(log_SeqMet)
  showNotification("Sequencing log refreshed.", type = "message")
})

# --- DASHBOARD LOG ---
output$dashboard_log_console <- renderTable({
  render_log_table(cached_log_data$dashboard)
}, striped = TRUE, spacing = "s", bordered = TRUE)


observeEvent(input$view_dashboard_log, {
  cached_log_data$dashboard <- read_log_data(log_Dashboard)
  output$admin_log_ui <- renderUI({
    render_log_ui(ns, "refresh_dashboard_log", "🔁 Refresh Dashboard Log", "dashboard_log_console", "dashboard_log_console")
  })
})

observeEvent(input$refresh_dashboard_log, {
  cached_log_data$dashboard <- read_log_data(log_Dashboard)
  showNotification("Dashboard log refreshed.", type = "message")
})

# --- WIKI LOG ---
wiki_log_data <- reactive({
  req(cached_log_data$wiki)
  cached_log_data$wiki
})

wiki_current_view <- reactiveVal("all")

observeEvent(input$view_wiki_daily,   { wiki_current_view("daily") })
observeEvent(input$view_wiki_weekly,  { wiki_current_view("weekly") })
observeEvent(input$view_wiki_monthly, { wiki_current_view("monthly") })
observeEvent(input$view_wiki_yearly,  { wiki_current_view("yearly") })
observeEvent(input$view_wiki_all,     { wiki_current_view("all") })

filtered_wiki_data <- reactive({
  df <- wiki_log_data()
  view_mode <- wiki_current_view()
  
  df %>%
    mutate(group_col = dplyr::case_when(
      view_mode == "daily"   ~ as.Date(datetime),
      view_mode == "weekly"  ~ lubridate::floor_date(datetime, "week"),
      view_mode == "monthly" ~ lubridate::floor_date(datetime, "month"),
      view_mode == "yearly"  ~ lubridate::floor_date(datetime, "year"),
      TRUE                   ~ as.Date(datetime)
    )) %>%
    group_by(group_col) %>%
    summarise(
      count = n(),
      visits_list = list(paste(Date, Time, Event, sep = " | ")),  # keep as individual entries, one per visit
      visits_preview = {
        visit_list <- paste(Date, Time, Event, sep = " | ")
        if (length(visit_list) > 3) {
          paste(paste(visit_list[1:3], collapse = "<br>"), "<br><i>...click bar for more</i>")
        } else {
          paste(visit_list, collapse = "<br>")
        }
      },
      .groups = "drop"
    ) %>%
    rename(date = group_col)
})

output$wiki_log_plot <- renderPlotly({
  df <- filtered_wiki_data()
  
  plot_ly(df, x = ~date, source = "wiki_plot") %>%
    add_bars(y = ~count, name = "Visit Count",
             marker = list(color = '#74c476'),
             hovertemplate = paste(
               "<b>Date:</b> %{x}<br>",
               "<b>Visits:</b> %{y}<br>",
               "<b>Details:</b><br>%{customdata}<extra></extra>"
             ),
             customdata = df$visits_preview) %>%
    add_lines(y = ~count, name = "Visit Trend",
              line = list(color = '#238b45')) %>%
    layout(
      hovermode = "closest",
      title = "Wiki Page Visits",
      xaxis = list(title = "Date"),
      yaxis = list(title = "Number of Visits"),
      showlegend = FALSE
    )
})

render_wiki_log_ui <- function(ns) {
  tagList(
    div(
      style = "margin-bottom: 10px; display: flex; gap: 10px; flex-wrap: wrap; align-items: center;",
      div(
        style = "display: flex; gap: 10px; flex-wrap: wrap;",
        actionButton(ns("view_wiki_daily"),   "By Day"),
        actionButton(ns("view_wiki_weekly"),  "By Week"),
        actionButton(ns("view_wiki_monthly"), "By Month"),
        actionButton(ns("view_wiki_yearly"),  "By Year"),
        actionButton(ns("view_wiki_all"),     "All Time")
      ),
      div(
        style = "margin-left: auto;",
        actionButton(
          ns("refresh_wiki_logs"),
          label = "🔄 Refresh Plot",
          style = "background-color: #185FA5; border: none; font-weight: 500; color: #fff; padding: 8px 16px; border-radius: 8px;"
        )
      )
    ),
    downloadButton(ns("download_wiki_log"), "Download Full Wiki Log"),
    plotlyOutput(ns("wiki_log_plot"), height = "500px"),
    textOutput(ns("last_wiki_refresh"))
  )
}

observeEvent(input$view_wiki_log, {
  cached_log_data$wiki <- read_log_data(log_Wiki)
  wiki_current_view("all")
  output$admin_log_ui <- renderUI({
    render_wiki_log_ui(ns)
  })
})

observeEvent(input$refresh_wiki_logs, {
  cached_log_data$wiki <- read_log_data(log_Wiki)
  last_wiki_refresh_time(Sys.time())
  showNotification("Wiki log refreshed.", type = "message")
})

last_wiki_refresh_time <- reactiveVal(NULL)

output$last_wiki_refresh <- renderText({
  req(last_wiki_refresh_time())
  paste("Last refreshed at:", format(last_wiki_refresh_time(), "%Y-%m-%d %H:%M:%S"))
})

output$download_wiki_log <- downloadHandler(
  filename = function() {
    paste0("WikiLog_", Sys.Date(), ".csv")
  },
  content = function(file) {
    write.csv(wiki_log_data(), file, row.names = FALSE)
  }
)

observeEvent(event_data("plotly_click", source = "wiki_plot"), {
  click_data <- event_data("plotly_click", source = "wiki_plot")
  if (is.null(click_data)) return(NULL)
  
  clicked_date <- as.Date(click_data$x)
  
  full_list <- filtered_wiki_data() %>%
    filter(date == clicked_date) %>%
    pull(visits_list) %>%
    unlist()
  
  showModal(modalDialog(
    title = paste("Wiki Visits -", format(clicked_date, "%b %d, %Y")),
    easyClose = TRUE,
    footer = modalButton("Close"),
    size = "l",
    div(
      style = "max-height: 400px; overflow-y: auto; padding: 10px;",
      HTML(paste(htmltools::htmlEscape(full_list), collapse = "<br/>"))
    )
  ))
})

# --- AUTO PURGE LOGS OLDER THAN 30 DAYS ---
purge_old_log_entries <- function(log_path, days = 30) {
  if (!file.exists(log_path)) return()
  
  tryCatch({
    df <- read_log_data(log_path)
    if (is.null(df) || nrow(df) == 0) return()
    
    cutoff <- Sys.time() - (days * 24 * 60 * 60)
    df_clean <- df[df$datetime >= cutoff, ]
    
    if (nrow(df_clean) < nrow(df)) {
      purged <- nrow(df) - nrow(df_clean)
      
      # Drop datetime before writing — it is a derived column, not a log column
      df_write <- df_clean[, !names(df_clean) %in% "datetime", drop = FALSE]
      write.csv(df_write, log_path, row.names = FALSE, quote = TRUE)
      
      write_log(log_Dashboard, "LOG_PURGE",
                paste0("Purged ", purged, " entries older than ", days,
                       " days from: ", basename(log_path)))
    }
  }, error = function(e) {
    write_log(log_Dashboard, "LOG_PURGE_ERROR", e$message)
  })
}

# Run purge on app startup — only for SeqMet and Dashboard logs
purge_old_log_entries(log_SeqMet)
purge_old_log_entries(log_Dashboard)

# ---------------- UPDATE GROUP/EMAIL PROFILE -------------------
# read_user_profile()/write_user_profile() (locked, atomic) are defined in input/inputFiles.R
submission_warning_message <- reactiveVal("")
profile_version <- reactiveVal(0)  # increment to force UI refresh

observeEvent(input$update_group_profile, {
  output$admin_log_ui <- renderUI({
    tagList(
      h3("Group Profile"),
      radioButtons(ns("update_type"), "What are you updating?", choices = c("Group", "Email")),
      uiOutput(ns("update_input")),
      actionButton(ns("submit_update"), "Submit"),
      br(),
      htmlOutput(ns("submission_warning")),
    )
  })
})

output$submission_warning <- renderUI({
  HTML(paste0("<div style='color:#741b47; font-size: 18px; font-style: italic;'>",
              htmltools::htmlEscape(submission_warning_message()), "</div>"))
})



output$update_input <- renderUI({
  profile_version()
  req(input$update_type)
  profile <- read_user_profile(json_path)
  group_names <- names(profile)
  
  if (input$update_type == "Group") {
    tagList(
      radioButtons(ns("group_action"), "Action", choices = c("Add Group", "Remove Group")),
      uiOutput(ns("group_action_ui"))
    )
  } else {
    tagList(
      selectInput(ns("selected_group"), "Select Group", choices = group_names),
      radioButtons(ns("email_action"), "Action", choices = c("Add Email", "Remove Email")),
      uiOutput(ns("dynamic_email_input"))
    )
  }
})

output$group_action_ui <- renderUI({
  profile_version()
  req(input$group_action)
  profile <- read_user_profile(json_path)
  group_names <- sort(names(profile))
  
  if (input$group_action == "Add Group") {
    tagList(
      textInput(ns("group_name"), "New Group Name"),
      fileInput(ns("group_list"), "Upload Group List (optional)")
    )
  } else if (input$group_action == "Remove Group") {
    tagList(
      selectInput(ns("group_name"), "Select Group(s) to Remove", choices = group_names, multiple = TRUE)
    )
  }
})

output$dynamic_email_input <- renderUI({
  profile_version() 
  req(input$selected_group, input$email_action)
  profile <- read_user_profile(json_path)
  emails_in_group <- profile[[input$selected_group]]
  
  if (input$email_action == "Add Email") {
    tagList(
      textInput(ns("email_address"), "Email Address (optional if uploading list)"),
      fileInput(ns("email_list"), "Upload Email List (optional)")
    )
  } else if (input$email_action == "Remove Email") {
    tagList(
      selectInput(ns("email_to_remove"), "Select Email(s) to Remove",
                  choices = emails_in_group, multiple = TRUE)
    )
  }
})



observeEvent(input$submit_update, {
  req(authenticated_group() == "Admin")

  if (input$update_type == "Group") {

    if (!is.null(input$group_action) && input$group_action == "Remove Group") {
      # --- REMOVE GROUP ---
      req(input$group_name)
      groups_to_remove <- input$group_name
      removed   <- c()
      skipped   <- c()
      not_found <- c()

      update_user_profile(json_path, function(profile) {
        for (grp in groups_to_remove) {
          if (tolower(grp) == "admin") {
            skipped <<- c(skipped, grp)
            next
          }
          match_idx <- which(tolower(names(profile)) == tolower(grp))
          if (length(match_idx) > 0) {
            removed_name <- names(profile)[match_idx]
            profile[[removed_name]] <- NULL
            removed <<- c(removed, removed_name)
          } else {
            not_found <<- c(not_found, grp)
          }
        }
        profile
      })

      profile_version(profile_version() + 1)
      submission_warning_message("")
      
      if (length(removed) > 0) {
        showNotification(
          paste("Removed group(s):", paste(removed, collapse = ", ")),
          type = "message"
        )
        submission_warning_message(
          paste0("✅ Successfully removed: '", paste(removed, collapse = ", "), 
                 "' — group(s) no longer exist in the profile.")
        )
        later(function() { submission_warning_message("") }, delay = 5)
      }
      if (length(skipped) > 0) {
        showNotification(
          paste("Protected group(s) not removed:", paste(skipped, collapse = ", ")),
          type = "warning"
        )
      }
      if (length(not_found) > 0) {
        submission_warning_message(
          paste0("⚠️ Group(s) not found and could not be removed: '",
                 paste(not_found, collapse = ", "), "'")
        )
        later(function() { submission_warning_message("") }, delay = 5)
      }
      return()
      
    } else {
      # --- ADD GROUP ---
      new_groups       <- c()
      duplicate_groups <- c()

      if (!is.null(input$group_name) && length(input$group_name) > 0 && all(nzchar(input$group_name))) {
        new_groups <- c(new_groups, input$group_name)
      }

      if (!is.null(input$group_list)) {
        group_file_data <- read.csv(input$group_list$datapath, header = FALSE)[[1]]
        new_groups <- c(new_groups, group_file_data)
      }

      new_groups <- unique(trimws(new_groups))

      update_user_profile(json_path, function(profile) {
        existing_groups_lower <- tolower(names(profile))
        for (grp in new_groups) {
          if (tolower(grp) %in% existing_groups_lower) {
            duplicate_groups <<- c(duplicate_groups, grp)
          } else {
            profile[[grp]] <- list()
          }
        }
        profile
      })

      profile_version(profile_version() + 1)

      added <- setdiff(new_groups, duplicate_groups)
      submission_warning_message("")
      
      if (length(added) > 0 && length(duplicate_groups) == 0) {
        showNotification(
          paste("Added group(s):", paste(added, collapse = ", ")),
          type = "message"
        )
        
      } else if (length(added) == 0 && length(duplicate_groups) > 0) {
        submission_warning_message(
          paste0("⚠️ Group(s) already exist and were not added: '",
                 paste(duplicate_groups, collapse = ", "), "'")
        )
        later(function() { submission_warning_message("") }, delay = 5)
        
      } else if (length(added) > 0 && length(duplicate_groups) > 0) {
        showNotification(
          paste("Added group(s):", paste(added, collapse = ", ")),
          type = "message"
        )
        submission_warning_message(
          paste0("⚠️ Already existed and skipped: '",
                 paste(duplicate_groups, collapse = ", "), "'")
        )
        later(function() { submission_warning_message("") }, delay = 5)
      }
    }
    
  } else if (input$update_type == "Email") {
    req(input$selected_group)
    
    if (input$email_action == "Add Email") {
      emails_to_add <- c()
      
      if (!is.null(input$email_address) && input$email_address != "") {
        emails_to_add <- c(emails_to_add, input$email_address)
      }
      
      if (!is.null(input$email_list)) {
        tryCatch({
          email_file_data <- read.csv(input$email_list$datapath, header = FALSE)[[1]]
          
          if (any(is.na(email_file_data)) || length(email_file_data) == 0) {
            stop("Uploaded email list is invalid or empty")
          }
          
          if (!all(grepl("^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$", email_file_data))) {
            stop("Email list contains invalid format")
          }
          
          emails_to_add <- c(emails_to_add, email_file_data)
        }, error = function(e) {
          showNotification(paste("Invalid file format:", e$message), type = "error")
          return()
        })
      }
      
      emails_to_add  <- unique(trimws(emails_to_add))
      new_emails     <- c()
      duplicates     <- c()

      update_user_profile(json_path, function(profile) {
        current_emails <- profile[[input$selected_group]]
        new_emails  <<- setdiff(emails_to_add, current_emails)
        duplicates  <<- intersect(emails_to_add, current_emails)
        profile[[input$selected_group]] <- unique(c(current_emails, new_emails))
        profile
      })

      profile_version(profile_version() + 1)
      
      submission_warning_message("")
      
      if (length(new_emails) > 0 && length(duplicates) == 0) {
        showNotification(
          paste("Added email(s):", paste(new_emails, collapse = ", ")),
          type = "message"
        )
        
      } else if (length(new_emails) == 0 && length(duplicates) > 0) {
        submission_warning_message(
          paste0("⚠️ Email(s) already exist in this group and were not added: '",
                 paste(duplicates, collapse = ", "), "'")
        )
        later(function() { submission_warning_message("") }, delay = 5)
        
      } else if (length(new_emails) > 0 && length(duplicates) > 0) {
        showNotification(
          paste("Added email(s):", paste(new_emails, collapse = ", ")),
          type = "message"
        )
        submission_warning_message(
          paste0("⚠️ Already existed and skipped: '",
                 paste(duplicates, collapse = ", "), "'")
        )
        later(function() { submission_warning_message("") }, delay = 5)
      }
      
    } else if (input$email_action == "Remove Email") {
      req(input$email_to_remove)
      
      emails_to_remove  <- input$email_to_remove
      actually_removed  <- c()
      not_found_emails  <- c()

      update_user_profile(json_path, function(profile) {
        current_emails <- profile[[input$selected_group]]
        actually_removed <<- intersect(emails_to_remove, current_emails)
        not_found_emails <<- setdiff(emails_to_remove, current_emails)
        profile[[input$selected_group]] <- setdiff(current_emails, emails_to_remove)
        profile
      })

      profile_version(profile_version() + 1)
      
      submission_warning_message("")
      
      if (length(actually_removed) > 0) {
        showNotification(
          paste("Removed email(s):", paste(actually_removed, collapse = ", ")),
          type = "message"
        )
        submission_warning_message(
          paste0("✅ Successfully removed: '", paste(actually_removed, collapse = ", "),
                 "' — email(s) no longer exist in this group.")
        )
        later(function() { submission_warning_message("") }, delay = 5)
      }
      if (length(not_found_emails) > 0) {
        submission_warning_message(
          paste0("⚠️ Email(s) not found in this group: '",
                 paste(not_found_emails, collapse = ", "), "'")
        )
        later(function() { submission_warning_message("") }, delay = 5)
      }
    }
  }
})