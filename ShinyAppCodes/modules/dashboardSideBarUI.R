
module2_sidebar_UI <- function(id, is_admin = reactive(FALSE)) {
  ns <- NS(id)
  
  tooltip_script <- tags$script(HTML("$(function () { $('[data-toggle=\"tooltip\"]').tooltip(); })"))
  
  info_icon <- function(text) {
    tags$i(class = "fa fa-info-circle", style = "color:#007bff; margin-left:6px; cursor:pointer;",
           `data-toggle` = "tooltip", `data-placement` = "right", title = text)
  }
  
  tagList(
    shinyjs::useShinyjs(),
    tooltip_script,
    shinydashboard::sidebarMenu(
      fluidRow(column(12,
                      
                      # Select Application
                      selectInput(
                        inputId = ns("app"),
                        label = tagList("Select Application:", info_icon("<App: Library>\natacseq: ATAC, 10X ATAC\n\nbasic: Amplicon,10X Multiome,Other\n\nwgs: wgs, ChIA-PET\n\nrnaseq: mRNA,Total-RNA\n\nrnaseqR2: 10X GEX, 10X scRNA")),
                        choices = NULL, selected = NULL
                      ),
                      # Select Year
                      selectInput(
                        inputId = ns("release_year"),
                        label = tagList("Select Year:", info_icon("Filter data by release year (\"NULL\" = undelivered/missing date). \n\nLoading time is high for year with large data")),
                        choices = NULL, selected = NULL
                      ),
                      
                      # Select Lab
                      pickerInput(
                        inputId = ns("var1"),
                        label = tagList("Select Lab", info_icon("Non-GT, you are restricted to your group. \n\nGT users, select one or more labs. If NULL is selected, addtional menu will be created")),
                        choices = NULL, selected = NULL, multiple = TRUE,
                        options = pickerOptions(actionsBox = TRUE, title = "Search group", liveSearch = TRUE,
                                                liveSearchPlaceholder = "Search keyword", liveSearchStyle = "contains",
                                                selectedTextFormat = 'count > 1',
                                                countSelectedText = "{0} of {1} labs selected", dropupAuto = FALSE),
                        choicesOpt = list(style = rep("color: black; background: white", 500))
                      ),
                      
                      # Project ID
                      pickerInput(
                        inputId = ns("var2"),
                        label = tagList("Project ID: for 'NULL' use only", info_icon("Filter by Project ID. Takes the stage of Lab")),
                        choices = NULL, selected = NULL, multiple = TRUE,
                        options = pickerOptions(actionsBox = TRUE, title = "Search id type", liveSearch = TRUE,
                                                liveSearchPlaceholder = "Search keyword", liveSearchStyle = "contains",
                                                selectedTextFormat = 'count > 1',
                                                countSelectedText = "{0} of {1} ID selected", dropupAuto = FALSE),
                        choicesOpt = list(style = rep("color: black; background: white", 500))
                      ),
                      
                      # Select Project Run
                      pickerInput(
                        inputId = ns("var3"),
                        label = tagList("Select Project Run", info_icon("Choose specific project run(s)")),
                        choices = NULL, selected = NULL, multiple = TRUE,
                        options = pickerOptions(actionsBox = TRUE, title = "Search project run", liveSearch = TRUE,
                                                liveSearchPlaceholder = "Search keyword", liveSearchStyle = "contains",
                                                selectedTextFormat = 'count > 1',
                                                countSelectedText = "{0} of {1} runs selected", dropupAuto = FALSE),
                        choicesOpt = list(style = rep("color: black; background: white", 500))
                      ),
                      
                      # Select Metrics
                      pickerInput(
                        inputId = ns("var5"),
                        label = tagList("Select Metric(s)", info_icon("Choose performance metrics to visualize. You cannot combine PCT (%) metrics with non-PCT")),
                        choices = NULL, selected = NULL, multiple = TRUE,
                        options = pickerOptions(actionsBox = TRUE, title = "Search metrics", liveSearch = TRUE,
                                                liveSearchPlaceholder = "Search keyword", liveSearchStyle = "contains",
                                                selectedTextFormat = 'count > 1',
                                                countSelectedText = "{0} of {1} metrics selected", dropupAuto = FALSE),
                        choicesOpt = list(style = rep("color: black; background: white", 500))
                      ),
                      
                      # Select Species
                      pickerInput(
                        inputId = ns("var6"),
                        label = tagList("Select Species", info_icon("Filter by species (e.g., human, mouse)")),
                        choices = NULL, selected = NULL, multiple = TRUE,
                        options = pickerOptions(actionsBox = TRUE, title = "Search Species", liveSearch = TRUE,
                                                liveSearchPlaceholder = "Search keyword", liveSearchStyle = "contains",
                                                selectedTextFormat = 'count > 1',
                                                countSelectedText = "{0} of {1} species selected", dropupAuto = FALSE),
                        choicesOpt = list(style = rep("color: black; background: white; font-weight: bold;", 500))
                      ),
                      
                      # Plot Type Radio Buttons
                      radioButtons(
                        inputId = ns("plt"),
                        label = tagList("Plot type", info_icon("Choose how to display the data. GT users will see additioal Flo button.")),
                        choices = c("Box", "Bar", "Flo"), selected = NULL, inline = TRUE
                      ),
                      
                      # Sample Size
                      h5(
                        tags$b(span("Application sample size in database:", style = "color: black;")),
                        withSpinner(
                          uiOutput(ns("App_sample_counter_colored")),
                          type = 3, color = "#4497f2", color.background = "#f4f6f6"
                        )
                      ),
                      
                      # Styling and Footer
                      tags$head(tags$style(HTML("body {background-color: white;color: black; }"))),
                      tags$head(tags$style(HTML(".content-wrapper, .right-side {min-height: 100vh !important;overflow-y: auto;}"))),
                      tags$head(tags$style(HTML("body, .content-wrapper {background-color: #e6f2ff !important; } "))),
                      tags$head(tags$style(HTML(".form-group {margin-bottom: -5px !important;margin-top: -5px !important;}"))),
                      tags$style(HTML('.sidebar-menu li a { font-size: 15px; }')),
                      
                      tags$footer(
                        strong("Managed by: GTdrylab team"),
                        align = "left",
                        style = "position: fixed; bottom: 40px; width: 100%; height: 30px;
                   font-size: 16px; color: #4497f2; background-color: transparent; z-index: 100;")
      ))
    )
  )
}

