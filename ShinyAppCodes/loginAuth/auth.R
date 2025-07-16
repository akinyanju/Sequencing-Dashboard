# -------- User groups & expansion --------
valid_groups <- names(users_data)
write_log(
  log_Dashboard,
  "GROUPS_INIT",
  paste0("Found ", length(valid_groups), " valid groups")
)

# Expand GenomeTechnologies users across all subgroups
expanded_users_data <- users_data
gt_groups <- grep("^GenomeTechnologies", valid_groups, value = TRUE)
write_log(
  log_Dashboard,
  "GT_GROUPS_FOUND",
  paste0("GenomeTechnologies groups: ", paste(gt_groups, collapse = ", "))
)

if (length(gt_groups) > 0) {
  all_gt_emails <- unique(unlist(users_data[gt_groups]))
  write_log(
    log_Dashboard,
    "GT_EMAILS_AGGREGATED",
    paste0("Aggregated ", length(all_gt_emails), " unique GT emails")
  )
  
  for (g in gt_groups) {
    before <- length(expanded_users_data[[g]])
    expanded_users_data[[g]] <- unique(c(expanded_users_data[[g]], all_gt_emails))
    after <- length(expanded_users_data[[g]])
    write_log(
      log_Dashboard,
      "GT_GROUP_EXPANDED",
      paste0("Group '", g, "': users before=", before, ", after=", after)
    )
  }
}

# … your existing group‐expansion and mailer‐check code …

# -------- One-time code store & mailer check --------
otc_storage         <- new.env()
code_expiration_secs <- 5 * 60  # 5 minutes

write_log(
  log_Dashboard,
  "OTC_INIT",
  paste0("Initialized OTC storage; codes expire in ", code_expiration_secs, " seconds")
)

mailer_available <- function() {
  avail <- nzchar(Sys.which("mail")) || nzchar(Sys.which("sendmail"))
  write_log(
    log_Dashboard,
    "MAILER_CHECK",
    paste0("System mailer available? ", avail)
  )
  avail
}

send_via_system_mailer <- function(to_email, code) {
  subject <- "GT QC Dashboard - One-Time Code"
  message <- paste0("Your one-time code is: ", code, "\n\nThis code will expire in 5 minutes.")
  sender  <- "gtdrylab@jax.org"
  cmd     <- sprintf('echo -e "%s" | mail -s "%s" -r "%s" "%s"',
                     message, subject, sender, to_email)
  
  write_log(log_Dashboard, "SEND_OTC_ATTEMPT", paste0("Sending OTC to ", to_email))
  ok <- tryCatch({
    system(cmd, intern = FALSE, ignore.stderr = TRUE)
    TRUE
  }, error = function(e) {
    write_log(log_Dashboard, "SEND_OTC_ERROR",
              paste0("Failed to send OTC to ", to_email, ": ", e$message))
    FALSE
  })
  if (ok) {
    write_log(log_Dashboard, "SEND_OTC_OK", paste0("Successfully sent OTC to ", to_email))
  }
  ok
}

# ------------------------------------------------------------
# Issue and log a one-time code
# ------------------------------------------------------------
issue_one_time_code <- function(group_name, to_email) {
  code   <- sprintf("%06d", sample(0:999999, 1))
  now    <- Sys.time()
  expiry <- now + code_expiration_secs
  
  otc_storage[[to_email]] <- list(
    code       = code,
    issued_at  = now,
    expires_at = expiry,
    group      = group_name
  )
  
  write_log(
    log_Dashboard, "OTC_GENERATED",
    paste0(
      "Group='", group_name,
      "', Email='", to_email,
      "', Code='", code,
      "', ExpiresAt='", format(expiry, "%Y-%m-%d %H:%M:%S"), "'"
    )
  )
  
  sent <- send_via_system_mailer(to_email, code)
  write_log(
    log_Dashboard,
    if (sent) "OTC_SENT" else "OTC_SEND_FAILED",
    paste0("Sent to '", to_email, "'? ", sent)
  )
  invisible(sent)
}
