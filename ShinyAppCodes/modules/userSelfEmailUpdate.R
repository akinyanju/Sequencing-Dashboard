get_expanded_profile <- function(json_path) {
  profile <- read_user_profile(json_path)
  gt_groups <- grep("^GenomeTechnologies", names(profile), value = TRUE)
  
  if (length(gt_groups) > 0) {
    all_gt_emails <- unique(unlist(profile[gt_groups]))
    for (g in gt_groups) {
      profile[[g]] <- unique(c(profile[[g]], all_gt_emails))
    }
  }
  
  profile
}

output$user_group_email_ui <- renderUI({
  req(authenticated_group())
  profile <- get_expanded_profile(json_path)
  
  visible_groups <- if (is_genome_tech()) {
    sort(setdiff(names(profile), "Admin"))  # Exclude Admin
  } else {
    group <- trimws(authenticated_group())
    if (group %in% names(profile)) group else character(0)
  }
  
  tagList(
    selectInput(ns("selected_group_user"), "Select Group to Add Email", choices = visible_groups),
    
    selectizeInput(
      ns("email_address_user"),
      label = "Enter Email Address (clear field to add new)",
      choices = NULL,
      options = list(
        create = TRUE,
        placeholder = 'Type or select from existing emails...',
        onInitialize = I('function() { this.clear(); }')  # JS-level reset
      )
    ),
    
    fileInput(ns("email_list_user"), "Upload Email List (optional)"),
    actionButton(ns("submit_email_user"), "Add Email(s)", class = "btn-success"),
    br(), br(),
    textOutput(ns("email_update_result"))
  )
})

observeEvent(input$selected_group_user, {
  req(input$selected_group_user)
  profile <- get_expanded_profile(json_path)
  existing_emails <- profile[[input$selected_group_user]]
  
  session$sendCustomMessage("resetEmailInput", list(id = ns("email_address_user")))
  
  updateSelectizeInput(
    session,
    "email_address_user",
    choices = existing_emails,
    selected = NULL,
    server = TRUE
  )
})

observeEvent(input$submit_email_user, {
  req(input$selected_group_user)
  profile <- read_user_profile(json_path)  # RAW profile for saving
  
  emails_to_add <- c()
  if (!is.null(input$email_address_user) && input$email_address_user != "") {
    emails_to_add <- c(emails_to_add, input$email_address_user)
  }
  
  if (!is.null(input$email_list_user)) {
    tryCatch({
      email_file_data <- read.csv(input$email_list_user$datapath, header = FALSE)[[1]]
      if (any(is.na(email_file_data)) || length(email_file_data) == 0) stop("Invalid file")
      if (!all(grepl("^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$", email_file_data))) stop("Invalid email format")
      emails_to_add <- c(emails_to_add, email_file_data)
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
      return()
    })
  }
  
  emails_to_add <- unique(trimws(emails_to_add))
  group <- input$selected_group_user
  current_emails <- profile[[group]]
  new_emails <- setdiff(emails_to_add, current_emails)
  duplicates <- intersect(emails_to_add, current_emails)
  
  # Add to selected group
  profile[[group]] <- unique(c(current_emails, new_emails))
  
  # Propagate if GenomeTechnologies group
  if (startsWith(group, "GenomeTechnologies")) {
    gt_groups <- grep("^GenomeTechnologies", names(profile), value = TRUE)
    all_gt_emails <- unique(unlist(profile[gt_groups]))
    for (g in gt_groups) {
      profile[[g]] <- unique(c(profile[[g]], all_gt_emails))
    }
  }
  
  write_user_profile(profile, json_path)
  
  output$email_update_result <- renderText({
    paste0(
      "Successfully added ", length(new_emails), " email(s). ",
      if (length(duplicates) > 0)
        paste0("⏭ Skipped ", length(duplicates), " duplicate(s).")
      else
        ""
    )
  })
  
  showNotification("Group email(s) updated.", type = "message")
})

