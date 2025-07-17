# System Overview

The purpose of this system is to collect sequencing and quality control (QC) metrics, both historically and in real time, for the Genome Technologies (GT) production environment at JAX. The collected metrics are stored in a local DuckDB database (GTdashboardMetrics.duckdb, version 1.2.2), which is then pushed to a designated destination server. From there, the data is queried and sliced according to user interactions with the GT Dashboard, enabling dynamic filtering and visualization of relevant metrics.

The codebase supporting this system is primarily written in Bash and R, with some auxiliary logic implemented in Python. The system is designed for internal use by JAX staff and integrates tightly with production pipelines to provide reliable, up-to-date metrics for monitoring sequencing operations and QC performance.

> **CRITICALLY IMPORTANT:**
> Do **NOT** change permissions for `/srv/shiny-server/.usersProfile.json` and `/srv/shiny-server/log`.
> These files/folders must be owned by `shiny:shiny` set with `chown shiny:shiny` and have permissions `chmod 755` so the Shiny app can read, write, and execute as needed. Server will **crash** if this is violated.

---

## Code Locations

* On **Elion2**:
  `/gt/research_development/qifa/elion/software/qifa-ops/0.1.0/dashboardCodes`

* On **ctgenometch03**:
  `/srv/shiny-server/`

---

## Metrics Locations

* On **Elion2**:
  `/gt/data/seqdma/GTwebMetricsTables`
  *(Note: Some files/folders may be hidden; use `ls -la` to view)*

* On **ctgenometch03**:
  `/srv/shiny-server/.InputDatabase`
  *(Note: several files may be hidden; use `ls -la` to view)*

---

# Code Flow Overview - 
###### [Double click chart, then click 'Raw' to view. Use slider]

This flowchart shows how the main scripts and modules interact in the Genome Technologies sequencing and QC metrics system. *Raw flowchart is located at images/flowchart.txt for future edit and was plotted at https://www.plantuml.com*.


<div style="overflow-x: auto;">
  <img src="images/flowchart.svg" alt="Flowchart" style="min-width:1200px;">
</div>


## File-by-File Description

### Data Collection Scripts

* **crawlerSeqMetrics.sh**

  * Wrapper script that launches `gatherSequencingMetrics.sh` every 10 minutes. Time can be modified. 
  * Must be executed as `svc-gt-delivery` user.

* **crawlerQCmetricsScript.sh**

  * Wrapper script that launches `duckDBgatherwebQCmetrics.sh` every hour. Time can be modified.

* **gatherSequencingMetrics.sh**

  * Scans QC directories to collect metadata and sequencing metrics (Reads, Bases, Bytes) after delivery folder permissions is validated.
  * **Admin note:**  If GT acquire new sequencer, the name must be hardcoded inside this script. Find InstrumentName and update under relevant function.
  * It output SequencingMetrics.csv that later gets imported into duckdb, handled by next script.

* **duckDBgatherwebQCmetrics.sh**

  * Ingests `SequencingMetrics.csv` into duckdb
  * Search and gather QC metrics within the archival/current QC directories.
  * Import gathered QC metrics into `GTdashboardMetrics.duckdb`, ensuring no duplicates are added.
  * Automatically manages DuckDB locks during import to prevent conflicts.
  * Pushes DB to destination server only if new records are detected.

* **update\_projstatus.sh**

  * Script used to update the project status (Delivered or Undelivered) in the DuckDB database based on processing or delivery results. do **update_projstatus.sh --help** for options

---

### Shiny Application Core

* **app.R**

  * Entry point for launching the dashboard app. It sourced all modules.

* **server.R**

  * Hosts and integrates all server modules.
  * Manages login logic and authenticated session flow.
  * If working in development mode on your personal computer, search for "DEV MODE: show debug code only" and uncomment that block. Then, search for 'PRODUCTION MODE: actually email the code' and comment that block. **Never forget to reverse these changes when code is moved to proudction server**. You are okay if your computer has the ability to send otc. In that case, no need to uncomment DEV MODE block. 

* **UI.R**

  * Integrates all UI modules into a cohesive layout.

* **Libraries.R**

  * Loads required R libraries.
  * Attempts auto-installation if packages are missing (may fail on restricted systems). If that happens, manual installation will be required. First review the /log/missing_libraries_log.csv.

* **configPaths.R**

  * Sets environment paths and configuration variables for all modules. If working in dev mode in personal computer, change the location to code and data locations

* **inputFile.R**

  * Defines global variables and reactive input handlers shared across modules. Critical functions are handled here.

* **auth.R**

  * Handles login authentication and One time code (otc) email handling.

---

### Log Files located at /srv/shiny-server/log

* **access\_log.csv**

  * Logs user logins and OTC usage.

* **DashboardMetrics\_log.csv**

  * Logs all activity within the QC metrics dashboard page.

* **SequencingMetrics\_log.csv**

  * Logs activity related to the sequencing metrics dashboard.

* **missing\_libraries\_log.csv**

  * Logs issues related to missing or failed package loads.

* **/var/log/shiny-server/**

  * General system logs for the Shiny server.
  * Use `ls -lhrt` to find latest entries.

* **wiki logs/**

  * To be implemented. At the moment, this is pointing to DashboardMetrics logs.

---

### Sequencing Data (Landing Page)

* **landingPageUI.R**

  * Defines `module1_UI`: the sidebar and main panel layout for the sequencing dashboard.
  * Includes dropdown filters, tooltips, quick tour, and download button.

* **landingPageServer.R**

  * Defines `module1_Server`: handles reactive filtering, database queries, summary metrics, and download handling.
  * Updates filters based on selected platform and available metrics.

* **landingPagePlotCode.R**

  * Handles rendering of interactive Plotly charts by metric and platform.
  * Supports grouping by lab, machine, project, or site.
  * Includes dynamic axis, hover, and responsive layouts.

---

### QC Metrics Page

* **dashboardSideBarUI.R**

  * Defines `module2_sidebar_UI`: sidebar with filters (App, Year, Lab, Metrics, Species).
  * Supports Flo/Box/Bar plot selection and live sample count.

* **dashboardHeaderUI.R**

  * Defines `module2_header_side_body`: main layout with header, filters, and dynamic dashboard body.
  * Includes search bar, GT branding, and "Quick Tour" guide.

* **dashboardBodyUI.R**

  * Handles all tab panels and serves as the container for all rendered plots and data tables.

* **dashboardServer.R**

  * Hosts `module2_Server`: manages user roles, session state, tab rendering, and DuckDB querying.
  * Provides cache cleanup, live counts, summary tables, and admin tools.

* **DashboardPlotCode.R**

  * Renders Box, Bar, and Flo plots using Plotly or ggplot2.
  * Handles metric type checks, axis formatting, and real-time plot switching.

* **SpeciesAlignmentPlot.R**

  * Renders stacked bar charts of species-level alignment per sample.
  * Supports tabular toggle, warning handling, and plot fallback if data is missing.

---

### Admin & User Tools

* **AdminPage.R**

  * Provides admin-specific views for session logs, group management, and user email updates.
  * Uses modals, filters, and log consoles for review and intervention.

* **userSelfEmailUpdate.R**

  * Enables non-admin users to update their email/group info via file upload or direct input.
  * Ensures group JSON is synced and validated.

* **restartAppAfterCodeUpdate.sh**

  * Utility script to restart the Shiny app after updating source code.

# FAQ + Troubleshooting Guide

### General Troubleshooting Philosophy

Admins should always check the relevant log files before jumping into troubleshooting. Not all issues affect the whole system — ensure you're diagnosing the correct module where the issue originates:

* For sequencing page: check `SequencingMetrics_log.csv`
* For Dashboard QC page: check `DashboardMetrics_log.csv`
* For login issues: check `access_log.csv`
* For package/load errors: check `missing_libraries_log.csv`

### Common Questions

**Why do I see “No Data” on the dashboard?**

* The data may not have been imported correctly.
* Possible reasons:

  * `GTdashboardMetrics.duckdb` wasn't updated. Click refetch button.
  * Filters (Year, App, Platform) selected have no matching data
  * `SequencingMetrics.csv` was missing or malformed. Therefore, did not get imported into the database

**Why does login sometimes fail or not recognize my email?**

* OTC code may have expired or been reused.
* Admins should check `access_log.csv` for clues.

**Why is my project/sample missing from the QC dashboard?**

* QC pipeline may have failed or skipped samples. 
    * check the following log paths;
        * /gt/data/seqdma/GTwebMetricsTables/.slurmlog
        * /gt/data/seqdma/GTwebMetricsTables/.logs
        * And for sequencing Metrics csv file, check 
            * /gt/data/seqdma/GTwebMetricsTables/SeqMetrics/.slurmlogSeqMet
* `project_ID` or `Sample_Name` casing mismatch.
* Admins should check which table (Illumina (qc_illumina_metrics), PacBio (qc_pacbio_metrics), ONT (qc_ont_metrics)) should have the data, and inspect the relevant logs. This tables can for instance be inspected using 
    ***duckdb /gt/data/seqdma/GTwebMetricsTables/GTdashboardMetrics.duckdb "SELECT * FROM qc_illumina_metrics WHERE project_ID = 'GT24-RobsonP-94' LIMIT 10;" | less -S***


**I updated a dropdown or hit Refetch, but nothing changes.**

* UI may be cached or slow to react. Wait or close the browser tab and re-enter the url. This way, linux server memory can be reset.
* Retry after 5–10 seconds.
* Check if DuckDB was updated. It is possible the data is not even imported. Use below to manually check that the project run is present 
    ***duckdb /gt/data/seqdma/GTwebMetricsTables/GTdashboardMetrics.duckdb "SELECT * FROM qc_illumina_metrics WHERE Project_run_type = 'GT24-RobsonP-94-run2' LIMIT 10;" | less -S***
* See direcotry `/gt/data/seqdma/GTwebMetricsTables/.last_import_push` for last import time or check your email.

**Why does the Download Button return an HTML file instead of CSV?**

When clicking the download button, receiving a .html file (often displaying “Error generating CSV.”) typically means a silent error occurred in the server code behind the download button. To troubleshoot:

* Step-by-step Diagnostics
  * Identify Page Context
    **If the issue occurs on the Sequence Data Generated page:**
      * Check react_input_long() inside landingPageServer.R.
        Example for debugging:
        output$debug_metric <- renderText({
``            ``str(react_input_long())
        })

        Ensure this returns a non-empty tibble/dataframe.
      * If react_input_long() returns data correctly, then:
      Add similar debugging to downloadfunc():
      output$debug_metric <- renderText({
``            ``str(downloadfunc())
        })


        Verify that downloadfunc() outputs a valid dataframe.
        
    **If the issue occurs on the Login QC Metrics page:**
    * The download logic is handled directly via downloadfunc() inside dashboardServer.R.

      **Follow the same checks:**
      1.  Confirm data is returned from any pre-download processing functions.
        Use renderText() or temporary UI outputs to inspect reactivity.
      2.  If Both Functions Return Data but Download Still Fails:
        Ensure write.csv() is not writing a NULL or empty dataframe.
      3.  Check for missing required input filters upstream (e.g., req() blocking silently).
        Wrap write.csv() in a tryCatch() to catch and display internal errors.
      4.  Check Column Handling
        If columns in the drop list (dropColumns) do not exist in your dataframe, that’s normal and safe.
        However, confirm that pivoting and filtering logic is producing expected column names before attempting download.

        Example Debug Snippet

      output$debug_metric <- renderText({
        df <- react_input_long()
        if (is.null(df)) return("react_input_long() returned NULL.")
        paste("Rows:", nrow(df), "Cols:", ncol(df))
      })
    **Reminder**
    If data shows properly on the screen but fails to download, the issue is almost always in:

      1.  The download reactive (wrong or filtered-out dataset).
      2.  Filters excluding all rows.
      3.  Pivot errors (incorrect reshape).
      4.  Writing an empty or NULL dataframe.

# Environment Requirements

* R ≥ 4.2.0
* DuckDB ≥ 1.2.2
* Email client configured (`mail` or `ssmtp`) for pipeline reports

#### © GTdrylab July 2025







