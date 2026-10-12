module2_header_side_body <- function(id = "Module2", is_genome_tech = FALSE) {
  ns <- NS(id)
  
  tagList(
    shinyjs::useShinyjs(),
    tags$head(
      tags$script(HTML("$(function () { $('[data-toggle=\"popover\"]').popover(); });")),
      tags$style(HTML("
        
        /* ── Hide the shinydashboard inner header entirely ── */
        /* The gt-navbar in ui.R is the only navbar we show   */
        .main-header {
          display: none !important;
        }

        /* Remove the top margin/padding body adds to compensate for hidden header */
        .skin-blue .wrapper,
        .skin-blue .content-wrapper,
        .skin-blue .right-side,
        .skin-blue .main-sidebar {
          margin-top: 0 !important;
          padding-top: 0 !important;
        }

        /* ── Popover styling ── */
        .popover {
          max-width: 500px !important;
          min-width: 400px !important;
          max-height: 300px !important;
          overflow-y: auto !important;
          background-color: #e6f3ff !important;
          color: #003366 !important;
          font-size: 16px;
          border: 1px solid #b3d9ff;
          z-index: 1060 !important;
        }
        .popover-body {
          white-space: normal !important;
          color: #003366 !important;
          font-size: 16px !important;
        }
        .popover-header {
          font-weight: bold;
          font-size: 18px !important;
          background-color: #d0eaff !important;
          color: #003366 !important;
        }

        /* ── Dashboard sidebar theme ── */
        .main-sidebar,
        .skin-blue .main-sidebar,
        .skin-blue .left-side {
          background-color: #ffffff !important;
          border-right: 0.5px solid #e2e8f0 !important;
        }
        .skin-blue .sidebar a,
        .skin-blue .sidebar-menu > li > a {
          color: #1e293b !important;
          font-family: 'Inter', sans-serif !important;
          font-size: 13px !important;
        }
        .sidebar .bootstrap-select > .btn,
        .sidebar .bootstrap-select > button {
          background-color: #E3EDF8 !important;
          border: 0.5px solid #B5D4F4 !important;
          border-radius: 8px !important;
          font-size: 12px !important;
          color: #1e293b !important;
          box-shadow: none !important;
          font-family: 'Inter', sans-serif !important;
        }
        .sidebar .bootstrap-select > .btn:hover {
          border-color: #378ADD !important;
          background-color: #f0f7ff !important;
        }
        .sidebar select.form-control {
          background-color: #E3EDF8 !important;
          border: 0.5px solid #B5D4F4 !important;
          border-radius: 8px !important;
          font-size: 12px !important;
          color: #1e293b !important;
          font-family: 'Inter', sans-serif !important;
        }

        /* ── Dashboard body theme ── */
        .content-wrapper,
        .right-side,
        .skin-blue .content-wrapper,
        .skin-blue .right-side {
          background-color: #f0f4f8 !important;
          min-height: 100vh !important;
          overflow-y: auto !important;
        }

        /* ── Inner tabs ── */
        .nav-tabs > li > a {
          font-family: 'Inter', sans-serif !important;
          font-size: 13px !important;
          color: #64748b !important;
          border: none !important;
          border-bottom: 2px solid transparent !important;
          border-radius: 0 !important;
          background: transparent !important;
        }
        .nav-tabs > li.active > a,
        .nav-tabs > li.active > a:focus,
        .nav-tabs > li.active > a:hover {
          color: #185FA5 !important;
          border-bottom: 2px solid #185FA5 !important;
          background: transparent !important;
          font-weight: 500 !important;
        }
        .nav-tabs { border-bottom: 0.5px solid #e2e8f0 !important; }

        /* ── Button variants ── */
        .btn-gt-viewplot {
          background-color: #185FA5 !important; color: #E6F1FB !important;
          border: none !important; border-radius: 8px !important;
          font-size: 12px !important; font-weight: 500 !important;
          padding: 6px 14px !important; font-family: 'Inter', sans-serif !important;
        }
        .btn-gt-viewplot:hover { background-color: #0C447C !important; }

        .btn-gt-viewqc {
          background-color: #0F6E56 !important; color: #E1F5EE !important;
          border: none !important; border-radius: 8px !important;
          font-size: 12px !important; font-weight: 500 !important;
          padding: 6px 14px !important; font-family: 'Inter', sans-serif !important;
        }
        .btn-gt-viewqc:hover { background-color: #085041 !important; }

        /* ── Notification close button ── */
        .shiny-notification-close {
          color: #ffffff !important;
          opacity: 0.85;
          font-weight: bold;
        }
        .shiny-notification-close:hover {
          color: #f87171 !important;
          opacity: 1;
        }

        /* ── Radio button pills ── */
        .radio-inline.active-pill {
          background: #E6F1FB !important;
          border: 1px solid #378ADD !important;
          color: #185FA5 !important;
          font-weight: 500 !important;
        }
        .radio-inline.inactive-pill {
          background: #f8fafc !important;
          border: 1px solid #64748b !important;
          color: #475569 !important;
        }
      "))
    ),
    
    shinydashboard::dashboardPage(
      # Minimal header — hidden via CSS, required by dashboardPage()
      shinydashboard::dashboardHeader(title = "GT QC Dashboard"),
      
      sidebar = shinydashboard::dashboardSidebar(
        module2_sidebar_UI("Module2", is_genome_tech = is_genome_tech)
      ),
      
      body = shinydashboard::dashboardBody(
        module2_body_UI("Module2")
      )
    )
  )
}