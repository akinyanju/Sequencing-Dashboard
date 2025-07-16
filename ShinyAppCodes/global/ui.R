ui <- function(request) {
  fluidPage(
    tags$head(tags$style(HTML(" .modebar {left: 0 !important;right: auto !important;top: 0;}")), #move the plotly buttons to the left
              tags$style(type = 'text/css',
                         ".nav-tabs {font-size: 13px; background-color: #e6f2ff; border-color: #cccccc;} ",
                         ".tab-pane { padding: 20px; background-color: white; border-radius: 8px; box-shadow: 0 0 10px rgba(0,0,0,0.1);} "
              ), #vavigation tab colors and the dashboard background color
              tags$style(HTML("html, body {overflow: hidden; }.center-container {overflow: hidden !important; }")), #control scroller
              tags$style(HTML(".shiny-notification {position: fixed; top: 100px; right: 20px; left: auto; bottom: auto; z-index: 9999;font-size: 20px;
                      background-color: black; color: white; border-left: 5px solid #FF8C00; box-shadow: 0 4px 8px rgba(0, 0, 0, 0.3);}")),
              tags$style(HTML(".info-icon {color: #007bff; font-size: 1.1em;margin-left: 5px;cursor: help;}.info-icon:hover {color: #0056b3;}")), #controls the admin icon
              tags$style(HTML("#admin_tab {background: url('mechanical_background.gif') no-repeat center center fixed;background-size: cover;color: white;} "))
    ),
    tabsetPanel(id = "main_tab",
                tabPanel("Sequence Data Generated",
                         # Scroll only vertically, hide horizontal scrolling
                         div(style = "max-height: calc(100vh - 130px); overflow-y: auto; overflow-x: hidden; padding-right: 10px;",
                             module1_UI("Module1")
                         )
                ),
                tabPanel("Login to QC metrics dashboard",
                         uiOutput("dynamic_ui_wrapper")
                )
    )
  )
}
