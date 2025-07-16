# ==== Admin Authentication UI Toggle ====
observe({
  req(authenticated_group())
  is_admin <- authenticated_group() == "Admin"
  
  shinyjs::delay(500, {
    js_code <- if (is_admin) {
      "
      // Hide non-admin tabs and sidebar
      $('ul.nav-tabs li a').each(function() {
        if ($(this).attr('data-value') !== 'admin_tab') {
          $(this).parent().hide();
        } else {
          $(this).parent().show();
        }
      });
      $('#plot_ui').hide();
      $('body').addClass('sidebar-collapse');
      $('#sidebar').hide();
      $('#main-content').hide();
      $('.sidebar-toggle').hide();
      "
    } else {
      "
      // Show all tabs and sidebar for normal users
      $('ul.nav-tabs li a').parent().show();
      $('#plot_ui').show();
      $('body').removeClass('sidebar-collapse');
      $('#sidebar').show();
      $('#main-content').show();
      $('.sidebar-toggle').show();
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
      users_full = {
        user_list <- paste(email, group, time, sep = " | ")
        paste(head(user_list, 20), collapse = "<br>")  # Limit to 20 users
      },
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
          style = "background-color: #ffcc00; border: 2px solid #333; font-weight: bold; color: #000; padding: 8px 16px; border-radius: 6px;"
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
    pull(users_full)
  
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
      HTML(paste(full_list, collapse = "<br/>"))  # Pass the full list of users here
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
# --- LOG INITIALIZATION ---
observe({
  if (file.exists(log_SeqMet)) {
    cached_log_data$seqmet <- read_log_data(log_SeqMet)
  }
  if (file.exists(log_Dashboard)) {
    cached_log_data$dashboard <- read_log_data(log_Dashboard)
  }
  if (file.exists(log_Wiki)) {
    cached_log_data$wiki <- read_log_data(log_Wiki)
  }
})

# --- LOG TABLE RENDER HELPERS ---
render_log_table <- function(data) {
  req(data)
  data[order(data$datetime, decreasing = TRUE), ]
}

render_log_ui <- function(ns, button_id, label, table_id, output_id) {
  tagList(
    actionButton(
      ns(button_id), label,
      style = "background-color: #ffe066; border: 2px solid #444; font-weight: bold; color: #000;
               padding: 8px 16px; border-radius: 6px; transition: background-color 0.3s;
               cursor: pointer;",
      onmouseover = "this.style.backgroundColor='#fff099'",
      onmouseout = "this.style.backgroundColor='#ffe066'"
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
  output$admin_log_ui <- renderUI({
    render_log_ui(ns, "refresh_dashboard_log", "🔁 Refresh Dashboard Log", "dashboard_log_console", "dashboard_log_console")
  })
})

observeEvent(input$refresh_dashboard_log, {
  cached_log_data$dashboard <- read_log_data(log_Dashboard)
  showNotification("Dashboard log refreshed.", type = "message")
})

# --- WIKI LOG ---
output$wiki_log_console <- renderTable({
  render_log_table(cached_log_data$wiki)
}, striped = TRUE, spacing = "s", bordered = TRUE)

observeEvent(input$view_wiki_log, {
  output$admin_log_ui <- renderUI({
    render_log_ui(ns, "refresh_wiki_log", "🔁 Refresh Wiki Log", "wiki_log_console", "wiki_log_console")
  })
})

observeEvent(input$refresh_wiki_log, {
  cached_log_data$wiki <- read_log_data(log_Wiki)
  showNotification("Wiki log refreshed.", type = "message")
})


# ---------------- UPDATE GROUP/EMAIL PROFILE -------------------
#----------- Helper Functions Start-----------------
read_user_profile <- function(path) {
  fromJSON(path)
}

write_user_profile <- function(profile, path) {
  write_json(profile, path, pretty = TRUE)
}
#----------- Helper Functions End-----------------
submission_warning_message <- reactiveVal("")


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
              submission_warning_message(), "</div>"))
})



output$update_input <- renderUI({
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
  profile <- read_user_profile(json_path)
  
  if (input$update_type == "Group") {
    new_groups <- c()
    duplicate_groups <- c()
    
    # Case-insensitive names of existing groups
    existing_groups_lower <- tolower(names(profile))
    
    if (!is.null(input$group_name) && length(input$group_name) > 0 && all(nzchar(input$group_name))) {
      new_groups <- c(new_groups, input$group_name)
    }
    
    if (!is.null(input$group_list)) {
      group_file_data <- read.csv(input$group_list$datapath, header = FALSE)[[1]]
      new_groups <- c(new_groups, group_file_data)
    }
    
    new_groups <- unique(trimws(new_groups))
    
    # Check for duplicates (case-insensitive)
    for (grp in new_groups) {
      if (tolower(grp) %in% existing_groups_lower) {
        duplicate_groups <- c(duplicate_groups, grp)
      } else {
        profile[[grp]] <- list()
      }
    }
    
    # support group removal by checking action. Code enhanced to prevent Admin group from being removed
    if (!is.null(input$group_action) && input$group_action == "Remove Group") {
      removed <- c()
      skipped <- c()
      
      for (grp in new_groups) {
        if (tolower(grp) == "admin") {
          skipped <- c(skipped, grp)
          next
        }
        match_idx <- which(tolower(names(profile)) == tolower(grp))
        if (length(match_idx) > 0) {
          removed_name <- names(profile)[match_idx]
          profile[[removed_name]] <- NULL
          removed <- c(removed, removed_name)
        }
      }
      
      write_user_profile(profile, json_path)
      
      if (length(removed) > 0) {
        showNotification(paste("Removed group(s):", paste(removed, collapse = ", ")), type = "message")
      }
      if (length(skipped) > 0) {
        showNotification(paste("Protected group(s) not removed:", paste(skipped, collapse = ", ")), type = "warning")
      }
      return()
    }
    
    write_user_profile(profile, json_path)
    

    if (length(setdiff(new_groups, duplicate_groups)) > 0) {
      showNotification("New group(s) added successfully.", type = "message")
    }
    
    if (length(duplicate_groups) > 0) {
      submission_warning_message(
        paste0("⏭ Will skip ", "'",paste(duplicate_groups, collapse = ", "), "'", " if duplicate group exit")
      )
      # Auto-clear after 4 seconds
      later(function() {
        submission_warning_message("")
      }, delay = 4)
    } else {
      submission_warning_message("")  # Clear message when no duplicates
    }
    
    
  } else if (input$update_type == "Email") {
    req(input$selected_group)
    
    if (input$email_action == "Add Email") {
      emails_to_add <- c()
      duplicates <- c()
      
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
      
      emails_to_add <- unique(trimws(emails_to_add))
      current_emails <- profile[[input$selected_group]]
      
      new_emails <- setdiff(emails_to_add, current_emails)
      duplicates <- intersect(emails_to_add, current_emails)
      
      profile[[input$selected_group]] <- unique(c(current_emails, new_emails))
      
      write_user_profile(profile, json_path)
      
      if (length(new_emails) > 0) {
        showNotification("New email(s) added successfully.", type = "message")
      }
      
      if (length(duplicates) > 0) {
        submission_warning_message(
          paste0("⏭ Will skip ", "'",paste(duplicates, collapse = ", "), "'", " if duplicate email exit")
        )
        # Auto-clear after 4 seconds
        later(function() {
          submission_warning_message("")
        }, delay = 4)
      } else {
        submission_warning_message("")  # Clear message when no duplicates
      }
      
    } else if (input$email_action == "Remove Email") {
      req(input$email_to_remove)
      profile[[input$selected_group]] <- setdiff(profile[[input$selected_group]], input$email_to_remove)
      write_user_profile(profile, json_path)
      showNotification("Email(s) removed successfully.", type = "message")
    }
  }

  
})
