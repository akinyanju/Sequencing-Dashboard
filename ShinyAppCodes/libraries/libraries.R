# ------------------------------------------------------------
# Logging setup. If some package do not auto install, which i have encountered, then manually launch R using sudo e.g. sudo R then install the package.
# ------------------------------------------------------------
#base_path <- file.path("/Users/lawalr/Dropbox/My-script/AssociateCompSci/RshinyModules")

base_path <-"/srv/shiny-server/"
log_library    <- file.path(base_path, "log", "missing_libraries_log.csv")

# Ensure the log directory exists
dir.create(dirname(log_library), recursive = TRUE, showWarnings = FALSE)

# A utility to append timestamped messages to a CSV log
write_log <- function(log_path, message) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  entry     <- data.frame(Time = timestamp, Message = message, stringsAsFactors = FALSE)
  if (!file.exists(log_path)) {
    write.table(entry, file = log_path,
                sep = ",", row.names = FALSE, col.names = TRUE, quote = TRUE)
  } else {
    write.table(entry, file = log_path,
                sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE, quote = TRUE)
  }
}

# ------------------------------------------------------------
# List of required packages
# ------------------------------------------------------------
required_packages <- c(
  "echarts4r", "shinydashboard", "shinydashboardPlus", "shiny", "shinyjs",
  "shinyauthr", "shinymanager", "shinyWidgets", "dplyr", "ggplot2", "ggbeeswarm",
  "scales", "reshape2", "psych", "forcats", "jsonlite", "ggforce", "plotly",
  "keyring", "shinycssloaders", "tidyr", "glue", "cachem", "tools", "stringr",
  "future", "promises", "tictoc", "pryr", "fs", "future.apply", "duckdb", "DBI",
  "purrr", "magrittr","later"
)

# -------------Ensure a user‐writable library exists-------------
#This code ensure there’s a per‐user library and prepend it to .libPaths(). 
#Then all of install.packages() calls (which default to lib = .libPaths()[1]) will put packages there. It avoid writable issue

user_lib <- Sys.getenv("R_LIBS_USER", unset = "~/.R/library")
user_lib <- path.expand(user_lib)
if (!dir.exists(user_lib)) dir.create(user_lib, recursive = TRUE, showWarnings = FALSE)

# Prepend it so install.packages() and library() look there first
.libPaths(c(user_lib, .libPaths()))

# ------------------------------------------------------------
# Install & load loop with sudo fallback
# ------------------------------------------------------------
for (pkg in required_packages) {
  
  # 1) Check if already installed
  if (!requireNamespace(pkg, quietly = TRUE)) {
    write_log(log_library, paste0("Package missing, attempting normal install: ", pkg))
    
    # 2) Try a normal install
    install_ok <- TRUE
    tryCatch({
      install.packages(pkg, dependencies = TRUE)
      write_log(log_library, paste0("✔ Successfully installed package: ", pkg))
    }, error = function(e) {
      write_log(log_library, paste0("⚠️ Normal install failed for ", pkg, " — ", e$message))
      install_ok <<- FALSE
    })
    
    # 3) If normal install failed, try sudo install
    if (!install_ok) {
      write_log(log_library, paste0("🔄 Attempting sudo install for: ", pkg))
      sudo_cmd <- sprintf(
        "install.packages('%s', repos='https://cloud.r-project.org', dependencies=TRUE)",
        pkg
      )
      
      # run via sudo Rscript and capture the exit code
      exit_status <- system2(
        "sudo",
        args   = c("Rscript", "-e", shQuote(sudo_cmd)),
        stdout = NULL,    # let output go to console or log
        stderr = NULL     # likewise
      )
      
      # coerce to single TRUE/FALSE
      sudo_ok <- identical(exit_status, 0L)
      
      if (sudo_ok) {
        write_log(log_library, paste0("✅ Sudo install succeeded for: ", pkg))
      } else {
        write_log(log_library, paste0("❌ Sudo install also failed for: ", pkg,
                                      " (exit status: ", paste(exit_status, collapse="/"), ")"))
      }
    }
  }
  
  # 4) Verify installation succeeded
  if (!requireNamespace(pkg, quietly = TRUE)) {
    write_log(log_library, paste0("⚠️ Package still missing after attempts: ", pkg))
    next
  }
  
  # 5) Attempt to load the package
  tryCatch({
    library(pkg, character.only = TRUE)
  }, error = function(e) {
    write_log(log_library, paste0("⚠️ Failed to load package: ", pkg, " — ", e$message))
  })
}

