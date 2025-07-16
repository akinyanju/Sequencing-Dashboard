module1_UI <- function(id) {
  ns <- NS(id)
  ####################################################################################
  tooltip_script <- tags$script(HTML("$(function () { $('[data-toggle=\"tooltip\"]').tooltip(); })"))
  info_icon <- function(text) {
    tags$i(class = "fa fa-info-circle", style = "color:#007bff; margin-left:6px; cursor:pointer;",
           `data-toggle` = "tooltip", `data-placement` = "right", title = text)
  }
  #
  popover_script <- tags$script(HTML("$(function () {
    $('[data-toggle=\"popover\"]').popover();})"))
  #
  popover_styles <- tags$head(
    tags$style(HTML("
    .popover {
      max-width: 500px !important;
      min-width: 400px !important;
      max-height: 300px !important;
      overflow-y: auto !important;
      background-color: #e6f3ff !important;  /* Faint sky blue */
      color: #003366 !important;             /* Deep blue text */
      font-size: 16px;                       /* 🔠 Increase base font size */
      border: 1px solid #b3d9ff;
      z-index: 1060 !important;
    }
    .popover-body {
      white-space: normal !important;
      color: #003366 !important;
      font-size: 16px !important;            /* 🔠 Increase body font size */
    }
    .popover-header {
      font-weight: bold;
      font-size: 18px !important;            /* 🔠 Larger header */
      background-color: #d0eaff !important;
      color: #003366 !important;
    }
  "))
  )
  
  

  ####################################################################################
  tagList(
    shinyjs::useShinyjs(),
    tooltip_script,
    popover_script,
    popover_styles,
    sidebarLayout(
      sidebarPanel(width = 2,
                   radioButtons(ns("option"),
                                label = tagList("View by month", info_icon("Group metrics by month or quarterly")),
                                choices = c('Yes', 'No'), selected = "No", inline = TRUE),
                   
                   pickerInput(inputId = ns("year"),
                               label = tagList("Year", info_icon("Select year(s) of metrics to visualize")),
                               choices = NULL, selected = NULL, multiple = TRUE,
                               options = pickerOptions(actionsBox = TRUE, title = "Search year", liveSearch = TRUE,
                                                       liveSearchPlaceholder = "Select year", liveSearchStyle = "contains",
                                                       selectedTextFormat = 'count > 1',
                                                       countSelectedText = "{0} of {1} date selected"),
                               choicesOpt = list(style = rep("color: black; background: white", 500))),
                   
                   pickerInput(inputId = ns("period"),
                               label = tagList("Period", info_icon("Monthly or quarterly grouping")),
                               choices = NULL, selected = NULL, multiple = TRUE,
                               options = pickerOptions(actionsBox = TRUE, title = "monthly or quarterly", liveSearch = TRUE,
                                                       liveSearchPlaceholder = "Select quarter/month", liveSearchStyle = "contains",
                                                       selectedTextFormat = 'count > 1',
                                                       countSelectedText = "{0} of {1} period selected"),
                               choicesOpt = list(style = rep("color: black; background: white", 500))),
                   
                   pickerInput(inputId = ns("platform"),
                               label = tagList("Platform(s)", info_icon("Choose sequencing platform(s). Default: All")),
                               choices = NULL, selected = NULL, multiple = TRUE,
                               options = pickerOptions(actionsBox = TRUE, title = "Search platform", liveSearch = TRUE,
                                                       liveSearchPlaceholder = "Select platform", liveSearchStyle = "contains",
                                                       selectedTextFormat = 'count > 1',
                                                       countSelectedText = "{0} of {1} platform(s)"),
                               choicesOpt = list(style = rep("color: black; background: white", 500))),
                   
                   div(id = ns("application_parent"),
                       pickerInput(inputId = ns("application"),
                                   label = tagList("Application", info_icon("Application is activated only for Illumina. No application is defined for ONT and PAcBio")),
                                   choices = NULL, selected = NULL, multiple = TRUE,
                                   options = pickerOptions(actionsBox = TRUE, title = "Select app", liveSearch = TRUE,
                                                           liveSearchPlaceholder = "Search keyword", liveSearchStyle = "contains",
                                                           selectedTextFormat = 'count > 1',
                                                           countSelectedText = "{0} of {1} app selected"),
                                   choicesOpt = list(style = rep("color: black; background: white", 500)))
                   ),
                   
                   pickerInput(inputId = ns("metrics"),
                               label = tagList("Metrics(s)", info_icon("Select metrics to visualize. Default: Bases")),
                               choices = NULL, selected = NULL, multiple = TRUE,
                               options = pickerOptions(actionsBox = TRUE, title = "Search metrics", liveSearch = TRUE,
                                                       liveSearchPlaceholder = "Select metrics", liveSearchStyle = "contains",
                                                       selectedTextFormat = 'count > 1',
                                                       countSelectedText = "{0} of {1} metric"),
                               choicesOpt = list(style = rep("color: black; background: white", 500))),
                   
                   radioButtons(ns("site"),
                                label = tagList("Site", info_icon("Merge: combine all sites; Split: view by site")),
                                choices = c("Merge", 'Split'), selected = NULL, inline = TRUE),
                   
                   radioButtons(ns("more_plot"),
                                label = tagList("Additional plots", info_icon("Show extra plots by group")),
                                choices = c("Yes", 'No'), selected = "No", inline = TRUE),
                   
                   radioButtons(ns("more_plots_opt"),
                                label = tagList("Plots", info_icon("Choose additional plot type")),
                                choices = c("Machine", 'Lab', 'Project'), selected = NULL, inline = TRUE),
                   
                   div(id = ns("instrument_parent"),
                       pickerInput(inputId = ns("instrument"),
                                   label = tagList("Instrument", info_icon("Select instrument(s)")),
                                   choices = NULL, selected = NULL, multiple = TRUE,
                                   options = pickerOptions(actionsBox = TRUE, title = "Search instrument", liveSearch = TRUE,
                                                           liveSearchPlaceholder = "Select instrument", liveSearchStyle = "contains",
                                                           selectedTextFormat = 'count > 1',
                                                           countSelectedText = "{0} of {1} instrument(s)"),
                                   choicesOpt = list(style = rep("color: black; background: white", 500)))
                   ),
                   
                   div(id = ns("lab_parent"),
                       pickerInput(inputId = ns("lab"),
                                   label = tagList("Select lab", info_icon("Choose lab(s) to include")),
                                   choices = NULL, selected = NULL, multiple = TRUE,
                                   options = pickerOptions(actionsBox = TRUE, title = "Search lab", liveSearch = TRUE,
                                                           liveSearchPlaceholder = "Select lab", liveSearchStyle = "contains",
                                                           selectedTextFormat = 'count > 1',
                                                           countSelectedText = "{0} of {1} lab(s)"),
                                   choicesOpt = list(style = rep("color: black; background: white", 500)))
                   ),
                   
                   tags$head(tags$style(HTML("body {background-color: white;color: #F5F5F5;}
                   .form-group label {color: black !important;}
                   .skin-blue .sidebar a {color: #ccff00;}"))
                   )
      ),
      mainPanel(
        # Top information pane
        div(
          style = "display: flex; justify-content: space-between; align-items: center; 
           background-color: #f5f5f5; padding: 10px; margin-bottom: 15px; 
           border-bottom: 1px solid #ccc; border-radius: 5px;",
          
          # ℹ️ About icon (left-aligned)
          div(
            actionLink(
              ns("about_info"),
              label = "Quick Tour",
              icon = icon("info-circle"),
              `data-toggle` = "popover",
              `data-trigger` = "focus",
              `data-html` = "true",
              `data-placement` = "bottom", 
              title = "About Sequencing Metric Page",
              `data-content` = paste0(
                "<p>This dashboard displays sequencing metrics generated over time. It can be grouped by platform, application, and time.</p>",
                "<p>You can filter data by year, platform, application, site, lab and other variables using the controls on the left panel.</p>",
                "<p><strong>Each plot is interactive:</strong></p>",
                "<ul>",
                "<li>Hover over bars or boxes to see detailed metric values.</li>",
                "<li>Click legend items to hide or isolate specific metrics.</li>",
                "<li>Use the top-right toolbar to zoom, reset, or download the plot as an image.</li>",
                "</ul>",
                "<p>These features allow you to explore trends, spot anomalies, and analyze sequencing performance over time.</p>"
              ),
              style = "font-size: 18px; color: #007bff; cursor: pointer;"
            )
          ),
          
          # Right-aligned: Last fetched time + refetch button
          div(
            uiOutput(ns("last_refresh_text")),
            actionButton(
              ns("refetch_btn"),
              label = "🔄 Refresh Metrics",
              class = "btn btn-success btn-sm",
              style = "font-size: 16px; font-weight: bold;"
            ),
            style = "display: flex; gap: 10px; align-items: center;"
          )
        ),
        shinycssloaders::withSpinner(uiOutput((ns("plot_1")))),
        textOutput(ns("cache_status")),
        verbatimTextOutput(ns("debug_metric")),
        htmlOutput(ns("warnmsg"))
      )
    ),
    
    # Floating Download Button
    div(id = "downloadButtonContainer",
        downloadButton(ns('dataDownload'), "Download selected metrics",
                       style = "color: #fff; background-color: green; border-color: Black; position: fixed; bottom: 60px; left: 20px; z-index: 1000;
                                padding: 10px 20px; border-radius: 5px; box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);")
    )
  )
}
