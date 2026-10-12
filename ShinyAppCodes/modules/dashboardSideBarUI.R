module2_sidebar_UI <- function(id, is_genome_tech = FALSE) {
  ns <- NS(id)
  
  tooltip_script <- tags$script(HTML(
    "$(function () { $('[data-toggle=\"tooltip\"]').tooltip(); })"
  ))
  
  info_icon <- function(text) {
    tags$i(
      class = "fa fa-info-circle",
      style = "color:#378ADD; margin-left:5px; cursor:pointer;",
      `data-toggle` = "tooltip", `data-placement` = "right", title = text
    )
  }
  
  tagList(
    shinyjs::useShinyjs(),
    tooltip_script,
    
    # ── Force sidebar gap removal post-render ──────────────────────────
    tags$script(HTML("
      $(document).ready(function() {
        function fixSidebarGap() {
          $('.main-sidebar .sidebar, .sidebar-menu, .main-sidebar').css('padding-top', '0');
          $('.main-sidebar .row').css('margin-top', '0');
          $('.main-sidebar .col-sm-12').css('padding-top', '0');
        }
        fixSidebarGap();
        setTimeout(fixSidebarGap, 300);
        setTimeout(fixSidebarGap, 800);
      });
    ")),
    
    # ── Sidebar CSS ────────────────────────────────────────────────────
    tags$style(HTML("
      /* Zero out all top spacing */
      .main-sidebar .sidebar,
      .main-sidebar .sidebar-menu,
      .main-sidebar ul.sidebar-menu,
      .main-sidebar > .sidebar,
      .skin-blue .sidebar,
      .sidebar,
      .sidebar-menu {
        padding-top: 0 !important;
        margin-top:  0 !important;
      }

      /* Collapse row/column gutters */
      .main-sidebar .row        { margin-top: 0 !important; margin-bottom: 0 !important; }
      .main-sidebar .col-sm-12  { padding-top: 0 !important; padding-bottom: 0 !important; }

      /* Zero first list item */
      .main-sidebar ul.sidebar-menu > li,
      .main-sidebar .sidebar-menu > li:first-child {
        padding-top: 0 !important;
        margin-top:  0 !important;
      }

      /* Form element spacing */
      .main-sidebar .form-group,
      .main-sidebar .shiny-input-container {
        margin-top:    0   !important;
        margin-bottom: 2px !important;
        padding-top:   0   !important;
      }

      /* Labels */
      .main-sidebar .control-label {
        margin-top:    0   !important;
        margin-bottom: 1px !important;
        padding-top:   0   !important;
        font-size:   12px  !important;
        font-weight: 500   !important;
        color:       #185FA5 !important;
        font-family: 'Inter', sans-serif !important;
      }

      /* Dropdowns */
      .main-sidebar .selectize-input,
      .main-sidebar .bootstrap-select > .btn {
        padding-top:    4px !important;
        padding-bottom: 4px !important;
        min-height:    30px !important;
        font-size:     12px !important;
      }

      /* Radio buttons */
      .main-sidebar .radio-inline {
        margin-top:    2px !important;
        margin-bottom: 2px !important;
        padding-top:   0   !important;
      }

      /* Compare Runs pill — a bit of extra visual pull so users notice a
         feature they might otherwise skip past. The icon/badge (added via
         choiceNames in the R code) work in every browser; the tinted
         background/border below is a progressive enhancement for browsers
         that support :has() and simply has no effect elsewhere. */
      .main-sidebar .radio-inline:has(input[value=\"Compare Runs\"]) {
        background: linear-gradient(135deg, #eaf6f1 0%, #e6f1fb 100%) !important;
        border: 1px solid #0F6E56 !important;
      }
      .compare-runs-badge {
        display: inline-block;
        margin-left: 5px;
        padding: 0 5px;
        font-size: 9px;
        font-weight: 700;
        letter-spacing: 0.03em;
        line-height: 14px;
        color: #ffffff;
        background: #F59E0B;
        border-radius: 8px;
        vertical-align: middle;
      }

      /* Content wrapper */
      .content-wrapper, .right-side {
        min-height: 100vh !important;
        overflow-y: auto  !important;
      }
    ")),
    
    shinydashboard::sidebarMenu(
      div(style = "padding-top:0; margin-top:0;",
          
          # ── QC Filters heading ─────────────────────────────────────
          tags$div(
            style = paste(
              "font-size: 13px; font-weight: 700; color: #0C447C;",
              "text-transform: uppercase; letter-spacing: 0.1em;",
              "padding: 0 0 6px 16px; margin: 0 0 8px 0;",
              "border-bottom: 1.5px solid #B5D4F4;",
              "font-family: 'Inter', sans-serif;"
            ),
            "QC Filters"
          ),
          
          # Select Application
          selectInput(
            ns("app"),
            label = tagList("Select Application:", info_icon(
              "<App: Library>\natacseq: ATAC, 10X ATAC\n\nbasic: Amplicon, 10X Multiome, Other\n\nwgs: WGS, ChIA-PET\n\nrnaseq: mRNA, Total-RNA\n\nrnaseqR2: 10X GEX, 10X scRNA"
            )),
            choices = NULL
          ),
          
          # Select Year
          selectInput(
            ns("release_year"),
            label = tagList("Select Year:", info_icon(
              "Filter data by release year (\"NULL\" = undelivered/missing date)."
            )),
            choices = NULL
          ),
          
          # Select Lab
          pickerInput(
            ns("var1"),
            label = tagList("Select Lab", info_icon(
              "Non-GT: restricted to your group. GT users: select one or more labs."
            )),
            choices = NULL, multiple = TRUE,
            options = pickerOptions(
              actionsBox = TRUE, liveSearch = TRUE,
              liveSearchPlaceholder = "Search keyword", liveSearchStyle = "contains",
              selectedTextFormat = "count > 1",
              countSelectedText = "{0} of {1} labs selected", dropupAuto = FALSE
            ),
            choicesOpt = list(style = rep("color:black; background:white", 500))
          ),
          
          # Project ID (NULL use only)
          pickerInput(
            ns("var2"),
            label = tagList("Project ID: for 'NULL' use only", info_icon("Filter by Project ID.")),
            choices = NULL, multiple = TRUE,
            options = pickerOptions(
              actionsBox = TRUE, liveSearch = TRUE,
              liveSearchPlaceholder = "Search keyword", liveSearchStyle = "contains",
              selectedTextFormat = "count > 1",
              countSelectedText = "{0} of {1} IDs selected", dropupAuto = FALSE
            ),
            choicesOpt = list(style = rep("color:black; background:white", 500))
          ),
          
          # Select Project Run
          pickerInput(
            ns("var3"),
            label = tagList("Select Project Run", info_icon("Choose specific project run(s).")),
            choices = NULL, multiple = TRUE,
            options = pickerOptions(
              actionsBox = TRUE, liveSearch = TRUE,
              liveSearchPlaceholder = "Search keyword", liveSearchStyle = "contains",
              selectedTextFormat = "count > 1",
              countSelectedText = "{0} of {1} runs selected", dropupAuto = FALSE
            ),
            choicesOpt = list(style = rep("color:black; background:white", 500))
          ),
          # Note: Select Flowcell / Select Run Type / Select Flo Metric render
          # inside the Flowcell plot panel itself (plot/DashboardPlotCode.R),
          # not here, to avoid duplicate-ID pickers when Flowcell mode is active.
          # Select Metric(s)
          pickerInput(
            ns("var5"),
            label = tagList("Select Metric(s)", info_icon("Choose performance metrics to visualize.")),
            choices = NULL, multiple = TRUE,
            options = pickerOptions(
              actionsBox = TRUE, liveSearch = TRUE,
              liveSearchPlaceholder = "Search keyword", liveSearchStyle = "contains",
              selectedTextFormat = "count > 1",
              countSelectedText = "{0} of {1} metrics selected", dropupAuto = FALSE
            ),
            choicesOpt = list(style = rep("color:black; background:white", 500))
          ),
          
          # Select Species
          pickerInput(
            ns("var6"),
            label = tagList("Select Species", info_icon("Filter by species (e.g., human, mouse).")),
            choices = NULL, multiple = TRUE,
            options = pickerOptions(
              actionsBox = TRUE, liveSearch = TRUE,
              liveSearchPlaceholder = "Search keyword", liveSearchStyle = "contains",
              selectedTextFormat = "count > 1",
              countSelectedText = "{0} of {1} species selected", dropupAuto = FALSE
            ),
            choicesOpt = list(style = rep("color:black; background:white; font-weight:bold;", 500))
          ),
          
          # Plot Type
          # Flowcell (GT-only) is included/excluded right here at render time,
          # not patched in afterwards via updateRadioButtons() — this input is
          # rendered once app_stage becomes "plot", which happens on its own
          # timeline (the user clicking Continue) well after authenticated_group
          # is first set at login. An observer keyed off authenticated_group
          # alone fires before this element exists in the DOM, so its
          # updateRadioButtons() call is a silent no-op and never retries.
          radioButtons(
            ns("plt"),
            label = tagList("Plot type", info_icon(
              "Box: box-and-whisker plot of the selected metric(s) for the current year.\n\nCompare Runs: put specific project runs side-by-side — across labs, or even across up to 2 different years — with its own Year/Lab/Run/Metric pickers.\n\nFlowcell: per-flowcell view (GenomeTechnologies members only)."
            )),
            choiceNames = c(
              list(
                "Box",
                tagList(
                  "Compare Runs",
                  tags$span("New", class = "compare-runs-badge")
                )
              ),
              if (is_genome_tech) list("Flowcell")
            ),
            choiceValues = c("Box", "Compare Runs", if (is_genome_tech) "Flowcell"),
            selected = NULL, inline = TRUE
          ),
          
          # ── Sample count badge ─────────────────────────────────────
          div(
            style = paste(
              "background: linear-gradient(135deg, #E6F1FB 0%, #dbeeff 100%);",
              "border: 1.5px solid #378ADD; border-radius: 10px;",
              "padding: 10px 14px; margin-top: 10px;",
              "box-shadow: 0 2px 6px rgba(56,138,221,0.15);"
            ),
            tags$div(
              style = paste(
                "font-size: 13px; font-weight: 500; color: #185FA5;",
                "margin-bottom: 4px; font-family: 'Inter', sans-serif; line-height: 1.4;"
              ),
              tags$span(style = "font-weight:700;", "Application sample count"),
              tags$br(),
              tags$span(style = "font-size:12px; color:#64748b; font-weight:400;",
                        "vs. total in database")
            ),
            withSpinner(
              uiOutput(ns("App_sample_counter_colored")),
              type = 3, color = "#378ADD", color.background = "#dbeeff"
            )
          ),
          
          # ── Footer ────────────────────────────────────────────────
          div(
            style = paste(
              "position:fixed; bottom:0; width:220px;",
              "padding: 10px 14px;",
              "background-color: #0C447C;",
              "border-top: 1px solid #185FA5;",
              "font-family: 'Inter', sans-serif;",
              "font-size: 12px; font-weight: 500;",
              "color: #E6F1FB; text-align: center;",
              "letter-spacing: 0.03em;"
            ),
            "Managed by: GTdrylab team"
          )
      )
    )
  )
}