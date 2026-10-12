# Derive a PI's own email from their group name, e.g. "BillSkarnes_Lab_CT" -> "bill.skarnes@jax.org".
# Only "<FirstLast>_Lab_<site>"-style groups match; returns NA for everything else (GT groups, Admin, etc.).
derive_pi_email <- function(group) {
  base <- sub("_Lab_.*$", "", group)
  if (identical(base, group)) return(NA_character_)

  split_at <- regexpr("[a-z][A-Z]", base)
  if (split_at == -1) return(NA_character_)

  first <- substr(base, 1, split_at)
  last  <- substr(base, split_at + 1, nchar(base))

  paste0(tolower(first), ".", tolower(last), "@jax.org")
}

# Can this user manage (delete) emails for `group`? True for PI of their own lab, or anyone
# listed under the "Admin" group in the profile (covers GT members who also serve as admins).
can_manage_group_emails <- function(group, my_email, raw_profile) {
  if (is.null(my_email) || !nzchar(my_email)) return(FALSE)
  my_email <- tolower(trimws(my_email))

  admin_emails <- tolower(trimws(raw_profile[["Admin"]]))
  if (my_email %in% admin_emails) return(TRUE)

  pi_email <- derive_pi_email(group)
  !is.na(pi_email) && my_email == tolower(pi_email)
}

# Clear a selectizeInput on the client — updateSelectizeInput(selected = NULL) alone isn't
# reliably enough to blank an already-populated selectize widget in server mode.
clear_email_selectize <- function(input_id) {
  shinyjs::runjs(sprintf(
    "var el = document.getElementById('%s'); if (el && el.selectize) { el.selectize.clear(true); }",
    input_id
  ))
}

email_update_message <- reactiveVal("")

# Push the current, on-disk membership of `group_name` into the email selectize's
# suggestion list, so it never goes stale after an add/delete or a group switch.
refresh_email_choices <- function(group_name) {
  req(group_name)
  profile <- read_user_profile(json_path)
  existing_emails <- profile[[group_name]]

  updateSelectizeInput(
    session,
    "email_address_user",
    choices = existing_emails,
    selected = NULL,
    server = TRUE
  )
  clear_email_selectize(ns("email_address_user"))
}

output$user_group_email_ui <- renderUI({
  req(authenticated_group())
  profile <- read_user_profile(json_path)

  visible_groups <- if (is_genome_tech()) {
    sort(setdiff(names(profile), c("Admin", EXCLUDED_GROUPS)))
  } else {
    group <- trimws(authenticated_group())
    if (group %in% names(profile) && !(group %in% EXCLUDED_GROUPS)) group else character(0)
  }

  # Seed the email field's choices with whatever group the group <select> will
  # default to (its first option) right here, at creation time — waiting on the
  # observeEvent below to do it via updateSelectizeInput() races the client-side
  # widget's own initialization on first load and can silently lose the update.
  default_group  <- if (length(visible_groups) > 0) visible_groups[1] else NULL
  default_emails <- if (!is.null(default_group)) profile[[default_group]] else NULL

  tagList(
    selectInput(ns("selected_group_user"), "Select Group to Add Email", choices = visible_groups),

    div(
      style = "display: flex; align-items: flex-end; gap: 0; width: fit-content;",
      selectizeInput(
        ns("email_address_user"),
        label = "Enter or Select Email Address",
        choices = default_emails,
        options = list(
          create = TRUE,
          createOnBlur = TRUE,  # commit whatever's typed when focus leaves the field (e.g. clicking Add/Delete),
                                 # so a typed-but-unconfirmed email isn't silently dropped
          placeholder = 'Type or select from existing emails...',
          onInitialize = I('function() { this.clear(); }')  # JS-level reset
        )
      ),
      div(
        # class="form-group" replicates the exact wrapper Shiny puts around every
        # labeled input — same bottom margin included — so flex "align-items:
        # flex-end" lines up the button's own bottom edge with the input box's
        # bottom edge instead of offsetting it by that margin.
        class = "form-group",
        tags$label(class = "control-label", style = "visibility: hidden; display: block;", "Clear"),
        actionButton(ns("clear_email_user"), tagList(icon("eraser"), "Clear"),
                     style = "background-color: #64748b; color: white; border: none;
                              border-radius: 0 8px 8px 0; font-weight: 500; margin: 0;
                              margin-left: -1px; padding: 6px 14px; box-shadow: none;
                              transition: background-color 0.2s;",
                     onmouseover = "this.style.backgroundColor='#475569'",
                     onmouseout  = "this.style.backgroundColor='#64748b'")
      )
    ),

    fileInput(ns("email_list_user"), "Upload Email List (optional)"),

    div(
      style = "display: flex; gap: 10px;",
      actionButton(ns("submit_email_user"), "Add Email(s)",
                   style = "background-color: #185FA5; color: white; border: none; border-radius: 8px; font-weight: 500;"),
      uiOutput(ns("delete_email_button_ui"), inline = TRUE)
    ),

    br(), br(),
    textOutput(ns("email_update_result"))
  )
})

output$email_update_result <- renderText({
  email_update_message()
})

observeEvent(input$clear_email_user, {
  clear_email_selectize(ns("email_address_user"))
})

observeEvent(input$selected_group_user, {
  refresh_email_choices(input$selected_group_user)
})

observeEvent(input$submit_email_user, {
  req(input$selected_group_user)
  profile <- read_user_profile(json_path)  # RAW profile for saving

  group <- input$selected_group_user
  allowed_groups <- if (is_genome_tech()) {
    sort(setdiff(names(profile), c("Admin", EXCLUDED_GROUPS)))
  } else {
    trimws(authenticated_group())
  }
  if (group %in% EXCLUDED_GROUPS || !(group %in% allowed_groups)) {
    showNotification("You are not authorized to update this group.", type = "error")
    return()
  }

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
  new_emails <- c()
  duplicates <- c()

  update_user_profile(json_path, function(profile) {
    current_emails <- profile[[group]]
    new_emails <<- setdiff(emails_to_add, current_emails)
    duplicates <<- intersect(emails_to_add, current_emails)

    # Add to the selected group ONLY — an email belonging to a sibling group
    # (e.g. another GenomeTechnologies_* group) is still new to this one.
    profile[[group]] <- unique(c(current_emails, new_emails))
    profile
  })

  email_update_message(paste0(
    "Successfully added ", length(new_emails), " email(s). ",
    if (length(duplicates) > 0)
      paste0("⏭ Skipped ", length(duplicates), " duplicate(s).")
    else
      ""
  ))
  later(function() { email_update_message("") }, delay = 5)

  profile_version(profile_version() + 1)  # refresh delete-button visibility below
  refresh_email_choices(group)
  showNotification("Group email(s) updated.", type = "message")
})

# -------- Delete Email (PI of the group, or Admin-listed GT members, only) --------
# Reuses the same "Enter or Select Email Address" field above — whatever email is
# currently entered/selected there is the one that gets deleted.

output$delete_email_button_ui <- renderUI({
  profile_version()
  req(input$selected_group_user)

  group <- input$selected_group_user
  raw_profile <- read_user_profile(json_path)

  if (!can_manage_group_emails(group, authenticated_email(), raw_profile)) {
    return(NULL)
  }

  actionButton(ns("delete_email_user"), tagList(icon("trash"), "Delete Email(s)"),
               style = "background-color: #b3261e; color: #ffffff; border: none; border-radius: 8px; font-weight: 500;")
})

observeEvent(input$delete_email_user, {
  req(input$selected_group_user)

  group <- input$selected_group_user
  raw_profile <- read_user_profile(json_path)

  if (!can_manage_group_emails(group, authenticated_email(), raw_profile)) {
    showNotification("You are not authorized to delete emails from this group.", type = "error")
    return()
  }

  email_to_remove <- trimws(input$email_address_user)
  if (is.null(email_to_remove) || email_to_remove == "") {
    showNotification("Enter or select an email to delete.", type = "warning")
    return()
  }

  if (!(email_to_remove %in% raw_profile[[group]])) {
    showNotification(paste0("'", email_to_remove, "' is not in ", group, "."), type = "warning")
    return()
  }

  update_user_profile(json_path, function(profile) {
    # Remove from the selected group ONLY — sibling groups (e.g. other
    # GenomeTechnologies_* groups) keep their own independent membership.
    profile[[group]] <- setdiff(profile[[group]], email_to_remove)
    profile
  })

  profile_version(profile_version() + 1)
  refresh_email_choices(group)
  showNotification(paste("Removed", email_to_remove, "from", group), type = "message")
})

