module2_header_side_body <- function(id = "Module2") {
  ns <- NS(id)
  
  tagList(
    shinyjs::useShinyjs(),
    #Popover styling moved here
    tags$head(
      tags$script(HTML("$(function () { $('[data-toggle=\"popover\"]').popover(); });")),
      tags$style(HTML("
          .popover { max-width: 500px !important; min-width: 400px !important; max-height: 300px !important; overflow-y: auto !important; background-color: #e6f3ff !important; color: #003366 !important; 
            font-size: 16px; border: 1px solid #b3d9ff;  z-index: 1060 !important; }
          .popover-body { white-space: normal !important; color: #003366 !important; font-size: 16px !important; }
          .popover-header { font-weight: bold; font-size: 18px !important; background-color: #d0eaff !important; color: #003366 !important; } ")) 
      ),
    
    shinydashboard::dashboardPage(
      header = shinydashboard::dashboardHeader(
        title = "GT QC Dashboard",
        # 🔍 Global Search Bar - Centered with Sky Blue Border
        tags$li(
          class = "dropdown",
          style = "flex-grow: 1; display: flex; align-items: center; justify-content: center; padding: 5px 20px;",
          div(
            style = "width: 420px;",
            tags$style(HTML("
              .global-search-input-container {position: relative;flex-grow: 1;}
              .global-search-input input {border: 2px solid #87ceeb !important; border-radius: 6px;padding: 6px 10px;font-size: 15px;width: 100%;padding-right: 30px;}
              .global-search-wrapper { display: flex; align-items: center;gap: 5px;}
              .global-search-button {height: 38px; font-size: 18px; padding: 0 12px; border-radius: 6px; background-color: #007bff; color: white; border: none; cursor: pointer;}
              .global-search-button:hover {background-color: #0056b3; }
              .clear-search {position: absolute; right: 8px; top: 50%; transform: translateY(-50%); cursor: pointer; color: #cc0000; font-size: 20px; font-weight: 900; font-weight: bold; user-select: none; z-index: 5; }
              .clear-search:hover { color: #ff0000;  } ")),
            tags$script(HTML(sprintf("$(document).on('click','.clear-search',function(){var ns='%s';var inputId=ns+'global_search';$('#'+inputId).val('');Shiny.setInputValue(inputId,'',{priority:'event'});});", 
                                     ns("")))),
            
            div(class = "global-search-wrapper",
                div(class = "global-search-input-container",
                    div(class = "global-search-input",
                        textInput(ns("global_search"),
                                  label = NULL,
                                  placeholder = "🔍 by 'PROJECT RUN TYPE' e.g. 23-li-004-run2",
                                  width = "100%"
                        ),
                        tags$span(class = "clear-search", HTML("&times;"))
                    )
                ),
                actionButton(ns("search_btn"), label = icon("search"), class = "global-search-button")
            )
          )
        ),
        
        # Quick Tour First (aligned + larger font)
        tags$li(
          class = "dropdown",
          style = "display: flex; align-items: center; padding: 5px 10px;",
          actionLink(
            ns("about_info"),
            label = tags$span("Quick Tour", style = "font-size: 17px; font-weight: bold;"),
            icon = icon("info-circle", class = "fa-lg"),
            `data-toggle` = "popover",
            `data-trigger` = "focus",
            `data-html` = "true",
            `data-placement` = "bottom",
            title = "About The QC Metrics Login Page",
            `data-content` = paste0(
              "<h5><strong>📊 Overview</strong></h5>",
              "<p>This dashboard displays both current and historical sequencing QC metrics from <strong>2020</strong> onward. It allows exploration by application, lab, project, and flowcell.</p>",
              
              "<h5><strong>🔍 How to Use</strong></h5>",
              "<ul>",
              "<li>Select an <strong>Application</strong> that correspond to your library to load its available QC metrics. Reference below:.<br><br>
                  &nbsp;&nbsp;<strong>App: Library</strong><br>
                  &nbsp;&nbsp;atacseq: ATAC, 10X ATAC<br>
                  &nbsp;&nbsp;basic: Amplicon, 10X Multiome, Other<br>
                  &nbsp;&nbsp;wgs: WGS, ChIA-PET<br>
                  &nbsp;&nbsp;rnaseq: mRNA, Total-RNA<br>
                  &nbsp;&nbsp;rnaseqR2: 10X GEX, 10X scRNA
              </li><br>",
              "<li>Use the sidebar filters to narrow down by <strong>Year</strong>, <strong>Lab</strong>, <strong>Project Run</strong>, <strong>Flowcell</strong>, <strong>Metric</strong>, and <strong>Species</strong>.</li>",
              "<li><strong>GT members</strong> see metrics from all labs. Other users see only their lab’s data.</li>",
              "</ul>",
              
              "<h5><strong>📁 Summary & Download Tabs</strong></h5>",
              "<ul>",
              "<li><strong>GeneralSummary</strong> provides a summary (min, max, mean, SD) of selected metrics.</li>",
              "<li><strong>DownloadTable</strong> allows downloading the filtered dataset as a table.</li>",
              "</ul>",
              
              "<h5><strong>🧬 Species Alignment Tab</strong></h5>",
              "<p>Displays species-level alignment results (e.g., human, mouse, rat), available for supported applications starting in <strong>2025</strong>.</p>",
              
              "<h5><strong>🧪 GT Member Exclusive Notes</strong></h5>",
              "<ul>",
              "<li>When the <strong>Select Year</strong> dropdown shows <em>Undelivered / Missing</em>, it indicates incomplete metadata — either <strong>Release Date</strong> or <strong>Lab</strong> is missing.</li>",
              "<li>Only GT users can view and investigate these undelivered projects.</li>",
              "<li><strong>Flo Plot</strong>: A comparative visualization tool available to GT members that enables exploring patterns between <strong>Flowcells</strong>, <strong>Run Types</strong>, and <strong>Labs</strong>. Ideal for identifying batch-level trends or anomalies.</li>",
              "</ul>",
              
              "<h5><strong>📈 Plot Interactivity</strong></h5>",
              "<ul>",
              "<li><strong>Hover</strong> over bars/boxes to view metric values.</li>",
              "<li><strong>Click</strong> legend items to hide/show specific metrics.</li>",
              "<li><strong>Toolbar</strong>: Use top-right buttons to zoom, reset, or export the plot.</li>",
              "</ul>",
              
              "<h5><strong>📩 Need Help?</strong></h5>",
              "<p>Contact us at <a href='mailto:gtdrylab@jax.org'>gtdrylab@jax.org</a> with questions or feedback.</p>"
            )
            
          )
        ),
        
        # 📊 Sample count badge — second position
        tags$li(
          class = "dropdown",
          style = "display: flex; align-items: center; padding: 5px 10px;",
          uiOutput(ns("menu_task_items"))
        ),
        
        # 🧬 Logo — last on far right
        tags$li(
          class = "dropdown",
          style = "display: flex; align-items: center; padding: 5px 10px;",
          tags$img(src = "GTlogo.png", height = "36px", style = "vertical-align: middle;")
        ),
        
        # ⬇️ Global style override for dropdown (if needed)
        tags$li(class = "dropdown",
                tags$style(HTML("
                  .navbar-custom-menu .dropdown-menu {
                    right: 0;
                    left: auto;
                    transform: translateX(-150px);
                    max-height: 300px;
                    overflow-y: auto;
                    overflow-x: hidden;
                    width: 400px !important;
                  }
                  .navbar-custom-menu .dropdown-menu .header {
                    font-size: 16px;
                    font-weight: bold;
                  }
                  .navbar-custom-menu .dropdown-menu .message-item {
                    font-size: 12px;
                    line-height: 1.2;
                    padding: 5px;
                  }
                "))
        )
      ),
      
      sidebar = shinydashboard::dashboardSidebar(
        module2_sidebar_UI("Module2")
      ),
      
      body = shinydashboard::dashboardBody(
        module2_body_UI("Module2")
      )
    )
  )
}

