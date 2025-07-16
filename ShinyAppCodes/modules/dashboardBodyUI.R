module2_body_UI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
        column(12,
               verbatimTextOutput(ns("debug_metric")), #used for debugging in server
               uiOutput(ns("dynamic_tabs"))  # dynamically load tabs based on authentication
        )
      )
    )
  )
}