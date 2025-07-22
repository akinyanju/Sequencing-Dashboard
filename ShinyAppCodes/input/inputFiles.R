# ─────────────────────────────────────────────────────────────
# inputFile.R
# 
# Purpose:
# - Initializes paths and reads user profiles, logs, and metrics data.
# - Connects to the DuckDB database containing QC metrics in `qc_illumina_metrics`.
# - Defines helper functions to:
#     • Fetch app-specific metrics from DuckDB (getMetricsFromDB)
#     • Populate dropdown filters (labs, years, flowcells, metrics, species)
#     • Log user activity and track app usage
#     • Monitor and reload SequencingMetrics.csv reactively
#     • Count unique samples per application
# - Provides global access to cached data (sample counts, logs) for the dashboard.
# - Supports efficient, reactive UI rendering across all user sessions.
# ─────────────────────────────────────────────────────────────

# #--------Code to source--------
DashboardPlottingCode <- file.path(base_path, "plot/DashboardPlotCode.R")
LandingPagePlottingCode <- file.path(base_path, "plot/landingPagePlotCode.R")
SpeciesAlignmentPlot <- file.path(base_path, "plot/SpeciesAlignmentPlot.R")
AdminPage <- file.path(base_path, "modules/AdminPage.R")
userSelfEmailUpdate <- file.path(base_path, "modules/userSelfEmailUpdate.R")

#--------Ensure the log directory exists--------
log_dir <- file.path(base_path, "log")
if (!dir.exists(log_dir)) {
  dir.create(log_dir, recursive = TRUE)
}

# -------- Paths to JSON and duckDB --------
json_path <- file.path(base_path, ".usersProfile.json")
users_data <- jsonlite::fromJSON(readLines(json_path, warn = FALSE))

#Updated DuckDB path
path_to_duckdb <- file.path(dir_InputFile, "duckDB/GTdashboardMetrics.duckdb")
log_Dashboard <- file.path(base_path, "log/DashboardMetrics_log.csv")
log_SeqMet <- file.path(base_path, "log/SequencingMetrics_log.csv")
log_file <- file.path(base_path, "log/access_log.csv")
log_Wiki <- file.path(base_path, "log/DashboardMetrics_log.csv")

# -------- Log Function --------
write_log <- function(log_path, event, details) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%OS")
  log_entry <- data.frame(
    Date = format(Sys.time(), "%Y-%m-%d"),
    Time = format(Sys.time(), "%H:%M:%OS"),
    Event = event,
    Details = details,
    stringsAsFactors = FALSE
  )
  
  tryCatch({
    dir.create(dirname(log_path), recursive = TRUE, showWarnings = FALSE)
    if (!file.exists(log_path)) {
      write.table(log_entry, file = log_path, row.names = FALSE, col.names = TRUE,
                  sep = ",", quote = TRUE)
    } else {
      write.table(log_entry, file = log_path, row.names = FALSE, col.names = FALSE,
                  sep = ",", quote = TRUE, append = TRUE)
    }
  }, error = function(e) {
    message("⚠️ Failed to write log: ", log_path, " → ", e$message)
  })
}

# -------- Throttled Logging --------
last_file_log <- new.env(parent = emptyenv())

log_throttle <- function(log_path, file_name, event, details, interval_secs = 300) {
  now <- Sys.time()
  last_time <- last_file_log[[file_name]]
  if (is.null(last_time) || difftime(now, last_time, units = "secs") > interval_secs) {
    write_log(log_path, event, details)
    last_file_log[[file_name]] <- now
  }
}

# -------- Connect to duckDB --------
write_log(log_Dashboard, "DB_CONNECT_START", paste0("Connecting to DuckDB at ", path_to_duckdb))
con <- DBI::dbConnect(duckdb::duckdb(), dbdir = path_to_duckdb, read_only = TRUE)
write_log(log_Dashboard, "DB_CONNECT_OK", "Primary DuckDB connection established")

onStop(function() {
  write_log(log_Dashboard, "DB_DISCONNECT", "Shutting down primary DuckDB connection")
  DBI::dbDisconnect(con, shutdown = TRUE)
})
# ─────────────────────────────────────────────────────────────
# Helper: Auto crawl new labs and update the json group information. Function is called in server.R module
# ─────────────────────────────────────────────────────────────
sync_json_with_duckdb_labs <- function(con, json_path) {
  users_data <- jsonlite::fromJSON(readLines(json_path, warn = FALSE))
  labs_from_db <- tryCatch({
    DBI::dbGetQuery(con, "
      SELECT DISTINCT Investigator_Folder
      FROM unified_qc_view
      WHERE Investigator_Folder IS NOT NULL
    ")$Investigator_Folder
  }, error = function(e) {
    write_log(log_Dashboard, "json_path", "⚠️ Failed to query labs from unified_qc_view")
    character(0)
  })
  
  labs_from_db <- unique(trimws(labs_from_db))
  
  new_labs <- setdiff(labs_from_db, names(users_data))
  if (length(new_labs) > 0) {
    for (lab in new_labs) {
      users_data[[lab]] <- character(0)  # or insert a default email if needed
    }
    jsonlite::write_json(users_data, json_path, pretty = TRUE, auto_unbox = TRUE)
  } else {
  }
}

# ─────────────────────────────────────────────────────────────
# Helper: Fetch distinct values for any field with arbitrary filters
# ─────────────────────────────────────────────────────────────
get_dropdown_choices <- function(field, filters, order = TRUE) {
  # Build SELECT DISTINCT <field> FROM qc_illumina_metrics
  sql <- sprintf("SELECT DISTINCT %s FROM qc_illumina_metrics", DBI::dbQuoteIdentifier(con, field))
  params <- list()
  
  # Add WHERE clauses
  if (length(filters) > 0) {
    where  <- paste0(names(filters), " = ?", collapse = " AND ")
    sql    <- paste(sql, "WHERE", where)
    params <- unname(filters)
  }
  # Optional ORDER BY
  if (order) {
    sql <- paste(sql, sprintf("ORDER BY %s", DBI::dbQuoteIdentifier(con, field)))
  }
  
  # Run query
  tryCatch({
    res <- DBI::dbGetQuery(con, sql, params = params)[[field]]
    # Normalize missing values if needed
    res[is.na(res) | trimws(res) == ""] <- "NULL"
    unique(res)
  }, error = function(e) {
    write_log(log_Dashboard, "DROPDOWN_ERROR",
              paste0("get_dropdown_choices(", field, "): ", e$message))
    character(0)
  })
}

# -------- access apps and group names from the summary table "qc_app_index" --------
get_valid_apps_fast <- function(group_name) {
  write_log(log_Dashboard, "GET_APPS_START", paste0("Querying unified_qc_view for group '", group_name, "'"))
  
  con_tmp <- DBI::dbConnect(duckdb::duckdb(), dbdir = path_to_duckdb, read_only = TRUE)
  on.exit({
    write_log(log_Dashboard, "GET_APPS_DISCONNECT", "Shutting down temporary DuckDB connection")
    DBI::dbDisconnect(con_tmp, shutdown = TRUE)
  }, add = TRUE)
  
  result <- tryCatch({
    DBI::dbGetQuery(con_tmp, sprintf("
      SELECT DISTINCT Application, Source
      FROM unified_qc_view
      WHERE Investigator_Folder = '%s'
    ", group_name))
  }, error = function(e) {
    write_log(log_Dashboard, "GET_APPS_ERROR", paste0("Error querying unified_qc_view: ", e$message))
    data.frame(Application = character(), Source = character())
  })
  
  result$Application <- trimws(result$Application)
  result$Source <- trimws(result$Source)
  result <- unique(result)
  
  write_log(log_Dashboard, "GET_APPS_OK", paste0("Found ", nrow(result), " apps for group '", group_name, "'"))
  result
}

# Initialize
all_apps <- sort(DBI::dbGetQuery(con, "SELECT DISTINCT Application FROM qc_illumina_metrics")$Application)
write_log(log_Dashboard, "INIT", paste("Loaded GTdashboardMetrics.duckdb with", length(all_apps), "apps."))

# ─────────────────────────────────────────────────────────────
# get_total_sample_counts()
# Purpose:
# Queries the DuckDB database to count unique samples per application.
# - Aggregates distinct `Sample_Name` entries grouped by `Application`.
# - Used for summarizing total sample coverage across all apps.
# - Returns a data frame with columns: Application, Sample_Count.

get_total_sample_counts <- function() {
  total_df <- data.frame(Application = character(), Sample_Count = numeric(), stringsAsFactors = FALSE)
  
  # Use a fresh read-only connection locally
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = path_to_duckdb, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  
  get_column_name <- function(table) {
    cols <- tryCatch(DBI::dbListFields(con, table), error = function(e) character(0))
    if ("Sample_Name" %in% cols) "Sample_Name"
    else if ("Sample_ID" %in% cols) "Sample_ID"
    else NA
  }
  
  tables <- c("qc_illumina_metrics", "qc_pacbio_metrics", "qc_ont_metrics")
  
  for (tbl in tables) {
    col_name <- get_column_name(tbl)
    
    if (is.na(col_name)) {
      write_log(log_Dashboard, "SAMPLE_COUNT_SKIP", paste("Skipping", tbl, "- no Sample_Name or Sample_ID"))
      next
    }
    
    query <- sprintf("
      SELECT Application, COUNT(DISTINCT TRIM(%s)) AS Sample_Count
      FROM %s
      WHERE Application IS NOT NULL
        AND TRIM(%s) IS NOT NULL
        AND TRIM(%s) != ''
        AND LOWER(TRIM(%s)) NOT IN ('na', 'null')
      GROUP BY Application
    ", col_name, tbl, col_name, col_name, col_name)
    
    tryCatch({
      df <- DBI::dbGetQuery(con, query)
      write_log(log_Dashboard, "SAMPLE_COUNT_DEBUG", paste("From", tbl, "→", paste(capture.output(print(df)), collapse = "\n")))
      total_df <- rbind(total_df, df)
    }, error = function(e) {
      write_log(log_Dashboard, paste0("SAMPLE_COUNT_ERROR_", tbl), e$message)
    })
  }
  
  # Aggregate duplicates
  total_df <- total_df %>%
    dplyr::group_by(Application) %>%
    dplyr::summarise(Sample_Count = sum(Sample_Count, na.rm = TRUE), .groups = "drop")
  
  write_log(log_Dashboard, "TOTAL_SAMPLES_OK",
            paste0("Final sample count total: ", sum(total_df$Sample_Count)))
  
  return(as.data.frame(total_df))
}
# ─────────────────────────────────────────────────────────────
# init_sample_counts()
# Purpose:
# Initializes or refreshes the global `sample_counts_cache()` reactive
# by calling `get_total_sample_counts()`.
# - Stores the resulting data for cross-session access within the Shiny app.

sample_counts_cache <- reactiveVal(NULL)
init_sample_counts <- function() {
  df <- get_total_sample_counts()
  sample_counts_cache(df)
}
# ─────────────────────────────────────────────────────────────
# # -------- LandingPage --------
# ─────────────────────────────────────────────────────────────
# Purpose:
# Retrieves all distinct, non-null years from the `sequencing_metrics` table
# where the month is valid (between 1 and 12).
# - Used to populate the "Year" dropdown menu in the UI.
# - Ensures only valid calendar years are included.

get_distinct_years <- function() {
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = path_to_duckdb, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  
  query <- "
    SELECT DISTINCT Year
    FROM sequencing_metrics
    WHERE Year IS NOT NULL
      AND Month BETWEEN 1 AND 12
  "
  
  years <- tryCatch({
    result <- DBI::dbGetQuery(con, query)
    as.integer(result$Year)
  }, error = function(e) {
    write_log(log_SeqMet, "YEAR_DROPDOWN_ERROR", e$message)
    integer(0)
  })
  
  sort(years, decreasing = TRUE)
}
# ─────────────────────────────────────────────────────────────
# Purpose:
# Retrieves a list of distinct time periods from the 'sequencing_metrics' DuckDB table
# to populate a dropdown in Shiny for monthly or quarterly views.
#
# - If `option == "Yes"`, returns full month names (e.g., "January", "February") for months 1–12.
# - If `option != "Yes"`, returns quarter labels (e.g., "First Quarter", "Second Quarter") based on month grouping.
#
# Includes error handling and logs issues using `write_log()` if the query fails.

#####
get_distinct_periods <- function(option = "Yes") {
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = path_to_duckdb, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  
  if (option == "Yes") {
    # Monthly mode
    query <- "
      SELECT DISTINCT Month
      FROM sequencing_metrics
      WHERE Month BETWEEN 1 AND 12
    "
    
    result <- tryCatch({
      DBI::dbGetQuery(con, query)$Month
    }, error = function(e) {
      write_log(log_SeqMet, "PERIOD_DROPDOWN_ERROR_MONTH", e$message)
      numeric(0)
    })
    
    result <- sort(result)
    month.name[result[!is.na(result) & result >= 1 & result <= 12]]
    
  } else {
    # Quarterly mode — compute quarter labels from Month
    query <- "
      SELECT DISTINCT Month
      FROM sequencing_metrics
      WHERE Month BETWEEN 1 AND 12
    "
    
    months <- tryCatch({
      DBI::dbGetQuery(con, query)$Month
    }, error = function(e) {
      write_log(log_SeqMet, "PERIOD_DROPDOWN_ERROR_QUARTER", e$message)
      numeric(0)
    })
    
    quarters <- dplyr::case_when(
      months %in% 1:3   ~ "First Quarter",
      months %in% 4:6   ~ "Second Quarter",
      months %in% 7:9   ~ "Third Quarter",
      months %in% 10:12 ~ "Fourth Quarter",
      TRUE              ~ NA_character_
    )
    
    # Return ordered quarter labels
    quarter_levels <- c("First Quarter", "Second Quarter", "Third Quarter", "Fourth Quarter")
    unique(quarter_levels[quarter_levels %in% quarters])
  }
}
# ─────────────────────────────────────────────────────────────

###
get_distinct_platforms <- function() {
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = path_to_duckdb, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  
  query <- "
    SELECT DISTINCT UPPER(Platform) AS Platform
    FROM sequencing_metrics
    WHERE Platform IS NOT NULL
  "
  
  platforms <- tryCatch({
    DBI::dbGetQuery(con, query)$Platform
  }, error = function(e) {
    write_log(log_SeqMet, "PLATFORM_DROPDOWN_ERROR", e$message)
    character(0)
  })
  
  sort(trimws(platforms))
}
# ─────────────────────────────────────────────────────────────
# Purpose:
# Retrieves a sorted list of unique sequencing platforms from the 'sequencing_metrics' DuckDB table.
#
# - Converts all platform names to uppercase for consistency.
# - Excludes NULL values.
# - Trims whitespace and returns alphabetically sorted results.
# - Logs an error using `write_log()` if the query fails.
get_distinct_applications <- function(platforms = NULL, years = NULL) {
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = path_to_duckdb, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  
  base_sql <- "
    SELECT DISTINCT UPPER(Application) AS Application
    FROM sequencing_metrics
    WHERE Application IS NOT NULL
  "
  
  filters <- c()
  if (!is.null(platforms)) filters <- c(filters, "UPPER(Platform) IN ({platforms*})")
  if (!is.null(years))     filters <- c(filters, "Year IN ({years*})")
  
  final_sql <- paste(base_sql, if (length(filters)) paste("AND", paste(filters, collapse = " AND ")) else "")
  
  apps <- tryCatch({
    DBI::dbGetQuery(
      con,
      glue::glue_sql(final_sql, platforms = platforms, years = years, .con = con)
    )$Application
  }, error = function(e) {
    write_log(log_SeqMet, "APPLICATION_DROPDOWN_ERROR", e$message)
    character(0)
  })
  
  sort(trimws(apps))
}
# ─────────────────────────────────────────────────────────────
# Purpose:
# Returns a subset of known metric column names ("Reads", "Bases", "Bytes") available in the
# 'sequencing_metrics' DuckDB table, based on optional filters.
#
# - Accepts optional `applications` and `platforms` filters (character vectors).
# - Dynamically builds a filtered SQL query with safe parameter binding using glue_sql.
# - Fetches the first row of data (LIMIT 1) for efficiency, then extracts column names.
# - Intersects available columns with known metric names to determine which are present.
# - Ensures safe disconnection from DuckDB and logs any errors encountered.

get_available_metrics <- function(applications = NULL, platforms = NULL) {
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = path_to_duckdb, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  
  # Pivotable metric columns
  all_possible_metrics <- c("Reads", "Bases", "Bytes")
  
  # Prepare SQL with filters
  filters <- c()
  if (!is.null(applications)) filters <- c(filters, "UPPER(Application) IN ({applications*})")
  if (!is.null(platforms))    filters <- c(filters, "UPPER(Platform) IN ({platforms*})")
  
  base_sql <- "SELECT * FROM sequencing_metrics"
  full_sql <- paste(base_sql,
                    if (length(filters)) paste("WHERE", paste(filters, collapse = " AND ")) else "",
                    "LIMIT 1")
  
  result <- tryCatch({
    df <- DBI::dbGetQuery(
      con,
      glue::glue_sql(full_sql, applications = applications, platforms = platforms, .con = con)
    )
    names(df)
  }, error = function(e) {
    write_log(log_SeqMet, "AVAILABLE_METRICS_ERROR", e$message)
    character(0)
  })
  
  intersect(all_possible_metrics, result)
}
# ─────────────────────────────────────────────────────────────
# Purpose:
# Returns a sorted list of distinct application names (in uppercase) from the 'sequencing_metrics' DuckDB table.
#
# - Accepts optional filters: `platforms` (character vector) and `years` (numeric vector).
# - Immediately returns an empty character vector if either filter is explicitly empty.
# - Dynamically constructs an SQL query using safe quoted values for platforms and years.
# - Connects to DuckDB in read-only mode and ensures disconnection on exit.
# - Returns sorted, unique application values.

get_distinct_applications <- function(platforms = NULL, years = NULL) {
  if (!is.null(platforms) && length(platforms) == 0) return(character(0))
  if (!is.null(years) && length(years) == 0) return(character(0))
  
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = path_to_duckdb, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  
  query <- "SELECT DISTINCT UPPER(Application) AS Application FROM sequencing_metrics WHERE 1=1"
  if (!is.null(platforms)) {
    query <- paste0(query, " AND UPPER(Platform) IN (", paste(shQuote(toupper(platforms)), collapse = ","), ")")
  }
  if (!is.null(years)) {
    query <- paste0(query, " AND Year IN (", paste(years, collapse = ","), ")")
  }
  
  df <- DBI::dbGetQuery(con, query)
  sort(unique(df$Application))
}
# ─────────────────────────────────────────────────────────────
# Purpose:
# Retrieves a sorted list of unique `InstrumentID` values from the `sequencing_metrics` DuckDB table,
# filtered optionally by Platform(s), Year(s), and Application(s).
#
# - Accepts optional vectors: `platforms`, `years`, and `applications`.
# - If any filter is provided as an empty vector, it returns an empty result early.
# - Builds a SQL query dynamically based on non-null filters.
# - Converts all string filters to uppercase for consistent matching.
# - Executes the query and returns sorted unique Instrument IDs.

get_distinct_instruments <- function(platforms = NULL, years = NULL, applications = NULL) {
  if (!is.null(platforms) && length(platforms) == 0) return(character(0))
  if (!is.null(years) && length(years) == 0) return(character(0))
  if (!is.null(applications) && length(applications) == 0) return(character(0))
  
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = path_to_duckdb, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  
  query <- "SELECT DISTINCT InstrumentID FROM sequencing_metrics WHERE 1=1"
  if (!is.null(platforms)) {
    query <- paste0(query, " AND UPPER(Platform) IN (", paste(shQuote(toupper(platforms)), collapse = ","), ")")
  }
  if (!is.null(years)) {
    query <- paste0(query, " AND Year IN (", paste(years, collapse = ","), ")")
  }
  if (!is.null(applications)) {
    query <- paste0(query, " AND UPPER(Application) IN (", paste(shQuote(toupper(applications)), collapse = ","), ")")
  }
  
  df <- DBI::dbGetQuery(con, query)
  sort(unique(df$InstrumentID))
}
# ─────────────────────────────────────────────────────────────
# Purpose:
# Retrieves a sorted list of unique lab identifiers (`groupFolder`) from the `sequencing_metrics` DuckDB table,
# filtered optionally by Platform(s), Year(s), and Application(s).
#
# - Accepts optional vectors: `platforms`, `years`, and `applications`.
# - Returns early with an empty character vector if any filter is an empty vector.
# - Dynamically builds a SQL query with applicable filters, using UPPER() for consistent case-insensitive matching.
# - Executes the query and returns a sorted list of unique lab group names.

get_distinct_labs <- function(platforms = NULL, years = NULL, applications = NULL) {
  if (!is.null(platforms) && length(platforms) == 0) return(character(0))
  if (!is.null(years) && length(years) == 0) return(character(0))
  if (!is.null(applications) && length(applications) == 0) return(character(0))
  
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = path_to_duckdb, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  
  query <- "SELECT DISTINCT groupFolder FROM sequencing_metrics WHERE 1=1"
  if (!is.null(platforms)) {
    query <- paste0(query, " AND UPPER(Platform) IN (", paste(shQuote(toupper(platforms)), collapse = ","), ")")
  }
  if (!is.null(years)) {
    query <- paste0(query, " AND Year IN (", paste(years, collapse = ","), ")")
  }
  if (!is.null(applications)) {
    query <- paste0(query, " AND UPPER(Application) IN (", paste(shQuote(toupper(applications)), collapse = ","), ")")
  }
  
  df <- DBI::dbGetQuery(con, query)
  sort(unique(df$groupFolder))
}
# ─────────────────────────────────────────────────────────────
# Purpose:
# Retrieves and filters sequencing metrics from the DuckDB table `sequencing_metrics` 
# based on optional parameters including Year, Month, Platform, Application, InstrumentID, and Lab.
#
# - Filters are dynamically applied in SQL with case normalization.
# - Handles both monthly and quarterly period formatting using the `option` parameter ("Yes" for monthly).
# - Ensures column presence and returns an empty structured DataFrame if no results found.
# - Adds derived fields for time-based grouping (month name, quarter label, year-month, etc.).
# - Returns pivoted long-form data for metrics ("Reads", "Bases", "Bytes") suitable for plotting or analysis.

#########
get_filtered_seq_metrics <- function(
    years = NULL,
    months = NULL,
    platforms = NULL,
    applications = NULL,
    instruments = NULL,
    labs = NULL,
    option = "Yes"
) {
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = path_to_duckdb, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  
  # Normalize filters to uppercase
  platforms    <- toupper(platforms)
  applications <- toupper(applications)
  
  # Base SQL
  base_sql <- "
    SELECT 
      Month,
      Year,
      Project,
      Site,
      InstrumentID,
      RunFolder,
      groupFolder,
      UPPER(Application) AS Application,
      FullPath,
      UPPER(Platform) AS Platform,
      Reads,
      Bases,
      Bytes,
      DeliveryDirectory,
      SampleSize,
      PolymeraseReadLength_bp_Mean
    FROM sequencing_metrics
    WHERE Month BETWEEN 1 AND 12
      AND Year IS NOT NULL
  "
  
  # Dynamic WHERE clauses
  where_clauses <- c()
  if (!is.null(years))        where_clauses <- c(where_clauses, "Year IN ({years*})")
  if (!is.null(months))       where_clauses <- c(where_clauses, "Month IN ({months*})")
  if (!is.null(platforms))    where_clauses <- c(where_clauses, "UPPER(Platform) IN ({platforms*})")
  if (!is.null(applications)) where_clauses <- c(where_clauses, "UPPER(Application) IN ({applications*})")
  if (!is.null(instruments))  where_clauses <- c(where_clauses, "InstrumentID IN ({instruments*})")
  if (!is.null(labs))         where_clauses <- c(where_clauses, "groupFolder IN ({labs*})")
  
  # Combine full query
  full_sql <- paste(
    base_sql,
    if (length(where_clauses)) paste("AND", paste(where_clauses, collapse = " AND ")) else ""
  )
  
  # Run query
  df <- DBI::dbGetQuery(
    con,
    glue::glue_sql(
      full_sql,
      years = years,
      months = months,
      platforms = platforms,
      applications = applications,
      instruments = instruments,
      labs = labs,
      .con = con
    )
  )
  
  # Early return if no data
  if (nrow(df) == 0 || any(c("Reads", "Bases", "Bytes") %in% colnames(df) == FALSE)) {
    return(data.frame(
      Year = integer(),
      Month = integer(),
      Platform = character(),
      Application = character(),
      Period = factor(levels = month.name),
      year_period = factor(levels = character()),
      name = character(),
      value = numeric(),
      Site = character(),
      groupFolder = character(),
      InstrumentID = character(),
      RunFolder = character(),
      Project = character(),
      FullPath = character(),
      DeliveryDirectory = character(),
      SampleSize = integer(),
      PolymeraseReadLength_bp_Mean = numeric()
    ))
  }
  
  # === Format and enrich ===
  df$Month <- as.integer(df$Month)
  df$month_name <- factor(month.name[df$Month], levels = month.name)
  df <- df[order(df$Year, df$Month), ]
  
  df$quarter_label <- dplyr::case_when(
    df$Month %in% 1:3   ~ "First Quarter",
    df$Month %in% 4:6   ~ "Second Quarter",
    df$Month %in% 7:9   ~ "Third Quarter",
    df$Month %in% 10:12 ~ "Fourth Quarter",
    TRUE ~ "Unassigned"
  )
  
  df$month_label <- df$month_name
  df$year_quarter <- paste0(df$Year, " - ", df$quarter_label)
  df$month_num <- match(df$month_name, month.name)
  df$year_month_date <- as.Date(paste(df$Year, df$month_num, 1, sep = "-"))
  df$year_month <- format(df$year_month_date, "%Y - %b")
  
  # Derived periods
  if (option == "Yes") {
    df$Period <- factor(df$month_label, levels = month.name)
    df$year_period <- factor(df$year_month, levels = unique(df$year_month))
  } else {
    df$Period <- factor(df$quarter_label, levels = c("First Quarter", "Second Quarter", "Third Quarter", "Fourth Quarter"))
    df$year_period <- factor(df$year_quarter, levels = unique(df$year_quarter))
  }
  
  # Pivot to long
  df_long <- tidyr::pivot_longer(df, cols = c(Reads, Bases, Bytes), names_to = "name", values_to = "value")
  return(df_long)
}
# ─────────────────────────────────────────────────────────────
#seqInputMetricdata_path <- file.path(dir_InputFile, "SequencingMetrics.csv")
# ─────────────────────────────────────────────────────────────
# -------- Log Reader Utility --------
read_log_data <- function(log_file) {
  req(file.exists(log_file))
  log_throttle(log_Dashboard, basename(log_file), "[LOG READ]", paste("Reading log:", basename(log_file)))
  df <- tryCatch({
    read.csv(log_file, stringsAsFactors = FALSE)
  }, error = function(e) {
    write_log(log_Dashboard, "[LOG READ ERROR]", paste("Failed to read:", basename(log_file), "→", e$message))
    return(data.frame(Date = character(), Time = character(), Event = character(), Details = character(), stringsAsFactors = FALSE))
  })
  df$datetime <- as.POSIXct(paste(df$Date, df$Time), format = "%Y-%m-%d %H:%M:%S")
  df
}

# -------- Cached Logs for Admin Panel --------
cached_log_data <- reactiveValues(
  seqmet = NULL,
  dashboard = NULL,
  wiki = NULL,
  login = NULL
)

observe({
  if (file.exists(log_SeqMet)) cached_log_data$seqmet <- read_log_data(log_SeqMet)
  if (file.exists(log_Dashboard)) cached_log_data$dashboard <- read_log_data(log_Dashboard)
  if (file.exists(log_Wiki)) cached_log_data$wiki <- read_log_data(log_Wiki)
  if (file.exists(log_file)) {
    cached_log_data$login <- read.csv(log_file, stringsAsFactors = FALSE) %>%
      mutate(datetime = as.POSIXct(paste(date, time), format = "%Y-%m-%d %H:%M:%S"))
  }
})

# ######################################################################
#Must be defined. Variable is used in server and plotting
flo_only_metrics <- c(
  "Cluster_PF_PCT", "Reads_Cluster_number_Mb", "Reads_Cluster_number_PF_Mb",
  "Q30_or_higher_PCT", "Yield_Gb", "Aligned_PhiX_PCT", "Error_rate_PhiX_alignment"
)
# ######################################################################

