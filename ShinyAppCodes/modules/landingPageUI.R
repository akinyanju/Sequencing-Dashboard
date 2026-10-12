module1_UI <- function(id) {
  ns <- NS(id)
  
  # ── Font Awesome 4.7 ─────────────────────────────────────────────────────
  fa_link <- tags$head(
    tags$link(rel = "stylesheet",
              href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css")
  )
  
  # ── Tooltip script ────────────────────────────────────────────────────────
  tooltip_script <- tags$script(HTML("$(function () { $('[data-toggle=\"tooltip\"]').tooltip(); })"))
  
  info_icon <- function(text) {
    tags$i(class = "fa fa-info-circle",
           style = "color:#378ADD; margin-left:5px; cursor:pointer;",
           `data-toggle` = "tooltip", `data-placement` = "right", title = text)
  }
  
  # ── Sidebar and main panel styles ─────────────────────────────────────────
  sidebar_styles <- tags$style(HTML("
    /* Sidebar panel */
    .well {
      background-color: #ffffff !important;
      border: none !important;
      border-right: 0.5px solid #e2e8f0 !important;
      border-radius: 0 !important;
      box-shadow: none !important;
      padding: 4px 14px 16px 10px !important;
      min-height: calc(100vh - 100px);
    }
    .col-sm-2 {
      padding-left: 12px !important;
      padding-right: 0 !important;
    }
    /* Nuclear override — force font on everything except icons */
    .well *:not(i):not(.fa):not([class*='fa-']):not(.glyphicon):not([class*='glyphicon-']) {
      font-family: 'Inter', sans-serif !important;
    }
    /* Restore icon fonts inside picker dropdowns */
    .well .bootstrap-select .dropdown-menu li a span.glyphicon,
    .well .bootstrap-select .dropdown-menu li a::before,
    .well .bs-checkbox i,
    .well .check-mark,
    .bootstrap-select .dropdown-menu span.check-mark,
    .bootstrap-select .dropdown-menu .selected span.check-mark {
      font-family: 'Glyphicons Halflings', 'FontAwesome' !important;
    }
    /* All labels inside sidebar */
    .well .form-group > label,
    .well .control-label,
    .well .shiny-input-container > label,
    .well .shiny-input-container label,
    .well label {
      font-size: 12px !important;
      color: #185FA5 !important;
      font-weight: 500 !important;
      margin-bottom: 3px !important;
      font-family: 'Inter', sans-serif !important;
    }
    /* Picker buttons */
    .well .bootstrap-select > .btn,
    .well .bootstrap-select > button {
      background-color: #E3EDF8 !important;
      border: 0.5px solid #B5D4F4 !important;
      border-radius: 8px !important;
      font-size: 12px !important;
      color: #1e293b !important;
      box-shadow: none !important;
      padding: 5px 10px !important;
      font-family: 'Inter', sans-serif !important;
    }
    .well .bootstrap-select > .btn:hover,
    .well .bootstrap-select.open > .btn {
      border-color: #378ADD !important;
      background-color: #f0f7ff !important;
    }
    
    /* RADIO PILL BASE */
    .well .radio-inline,
    .radio-inline {
      display: inline-block;
      margin-right: 0;
      padding: 4px 12px !important;
      border-radius: 20px !important;
      border: 1px solid #64748b !important;
      background: #f8fafc !important;
      color: #475569 !important;
      cursor: pointer !important;
      font-size: 12px !important;
      transition: all 0.15s !important;
    }

  /* Hide real radio button */
    .well .radio-inline input[type='radio'],
    .radio-inline input[type='radio'] {
      position: absolute !important;
      opacity: 0 !important;
    }

    /* ACTIVE state */
    .well .radio-inline.active-pill,
    .radio-inline.active-pill {
      background: #E6F1FB !important;
      border: 1px solid #378ADD !important;
      color: #185FA5 !important;
      font-weight: 500 !important;
    }

    /* Spacing between form groups */
    .well .form-group {
      margin-bottom: 6px !important;
      margin-top: 0 !important;
      padding-bottom: 0 !important;
    }
    .well .shiny-input-container {
      margin-bottom: 6px !important;
      margin-top: 0 !important;
      padding-bottom: 0 !important;
    }
    /* FILTERS heading */
    span.gt-sb-head {
      margin-top: 0 !important;
      padding-top: 0 !important;
      margin-bottom: 8px !important;
    }
    .well > .shiny-input-container,
    .well > span + .shiny-input-container,
    .well > span + div {
      margin-top: 0 !important;
      padding-top: 0 !important;
    }
    .gt-sb-head {
      font-size: 11px !important;
      font-weight: 500 !important;
      color: #64748b !important;
      text-transform: uppercase;
      letter-spacing: 0.07em;
      padding-bottom: 6px;
      border-bottom: 0.5px solid #e2e8f0;
      margin-bottom: 8px;
      display: block;
      font-family: 'Inter', sans-serif !important;
    }
    /* Main panel background */
    .col-sm-10 {
      background-color: #f0f4f8 !important;
      padding: 12px 16px !important;
    }
    /* Topbar */
    .gt-landing-topbar {
      background: #ffffff;
      border: 0.5px solid #e2e8f0;
      border-radius: 10px;
      padding: 10px 16px;
      display: flex;
      justify-content: space-between;
      align-items: center;
      margin-bottom: 10px;
    }
    .gt-landing-topbar .gt-fetch-time {
      font-size: 12px;
      color: #64748b;
      display: flex;
      align-items: center;
      gap: 6px;
    }
    /* Plot card */
    .gt-plot-wrap {
      background: #ffffff;
      border: 0.5px solid #e2e8f0;
      border-radius: 10px;
      padding: 14px;
      margin-bottom: 10px;
    }
    /* Download button */
    #downloadButtonContainer .btn {
      background-color: #185FA5 !important;
      border-color: #0C447C !important;
      color: #E6F1FB !important;
      border-radius: 8px !important;
      font-size: 13px !important;
      font-weight: 500 !important;
      box-shadow: 0 2px 6px rgba(12,68,124,0.2) !important;
      font-family: 'Inter', sans-serif !important;
    }
    #downloadButtonContainer .btn:hover {
      background-color: #0C447C !important;
    }
    /* Force radio groups into single line */
    .well .shiny-input-container .shiny-options-group {
      display: flex !important;
      flex-wrap: nowrap !important;
      gap: 6px;
    }

    /* Prevent pills from breaking */
    .well .radio-inline {
      white-space: nowrap;
    }
  "))
  
  # ── Radio pill JS — handles active state styling ──────────────────────────
  radio_pill_script <- tags$script(HTML("
    function updateRadioPills() {
      $('.radio-inline').each(function() {
        var radio = $(this).find('input[type=radio]');
    
        if (radio.is(':checked')) {
          $(this).addClass('active-pill').removeClass('inactive-pill');
        } else {
          $(this).removeClass('active-pill').addClass('inactive-pill');
        }
      });
    }

    $(document).ready(function() {
      setTimeout(updateRadioPills, 200);
    });

    $(document).on('change', 'input[type=radio]', function() {
      updateRadioPills();
    });

    $(document).on('shiny:idle', function() {
      updateRadioPills();
    });
  "))
  
  # ─────────────────────────────────────────────────────────────────────────
  tagList(
    shinyjs::useShinyjs(),
    fa_link,
    tooltip_script,
    sidebar_styles,
    radio_pill_script,
    
    sidebarLayout(
      # ── Sidebar ──────────────────────────────────────────────────────────
      sidebarPanel(width = 2,
                   
                   tags$span(class = "gt-sb-head", "Filters"),
                   
                   radioButtons(ns("option"),
                                label = tagList("View by month", info_icon("Group metrics by month or quarterly")),
                                choices = c('Yes', 'No'), selected = "No", inline = TRUE),
                   
                   pickerInput(inputId = ns("year"),
                               label = tagList("Year", info_icon("Select year(s) of metrics to visualize")),
                               choices = NULL, selected = NULL, multiple = TRUE,
                               options = pickerOptions(actionsBox = TRUE, title = "Search year",
                                                       liveSearch = TRUE, liveSearchPlaceholder = "Select year",
                                                       liveSearchStyle = "contains", selectedTextFormat = 'count > 1',
                                                       countSelectedText = "{0} of {1} date selected"),
                               choicesOpt = list(style = rep("color: black; background: white", 500))),
                   
                   pickerInput(inputId = ns("period"),
                               label = tagList("Period", info_icon("Monthly or quarterly grouping")),
                               choices = NULL, selected = NULL, multiple = TRUE,
                               options = pickerOptions(actionsBox = TRUE, title = "monthly or quarterly",
                                                       liveSearch = TRUE, liveSearchPlaceholder = "Select quarter/month",
                                                       liveSearchStyle = "contains", selectedTextFormat = 'count > 1',
                                                       countSelectedText = "{0} of {1} period selected"),
                               choicesOpt = list(style = rep("color: black; background: white", 500))),
                   
                   pickerInput(inputId = ns("platform"),
                               label = tagList("Platform(s)", info_icon("Choose sequencing platform(s). Default: All")),
                               choices = NULL, selected = NULL, multiple = TRUE,
                               options = pickerOptions(actionsBox = TRUE, title = "Search platform",
                                                       liveSearch = TRUE, liveSearchPlaceholder = "Select platform",
                                                       liveSearchStyle = "contains", selectedTextFormat = 'count > 1',
                                                       countSelectedText = "{0} of {1} platform(s)"),
                               choicesOpt = list(style = rep("color: black; background: white", 500))),
                   
                   div(id = ns("application_parent"),
                       pickerInput(inputId = ns("application"),
                                   label = tagList("Application", info_icon("Application is activated only for Illumina. No application is defined for ONT and PacBio")),
                                   choices = NULL, selected = NULL, multiple = TRUE,
                                   options = pickerOptions(actionsBox = TRUE, title = "Select app",
                                                           liveSearch = TRUE, liveSearchPlaceholder = "Search keyword",
                                                           liveSearchStyle = "contains", selectedTextFormat = 'count > 1',
                                                           countSelectedText = "{0} of {1} app selected"),
                                   choicesOpt = list(style = rep("color: black; background: white", 500)))
                   ),
                   
                   pickerInput(inputId = ns("metrics"),
                               label = tagList("Metrics(s)", info_icon("Select metrics to visualize. Default: Bases")),
                               choices = NULL, selected = NULL, multiple = TRUE,
                               options = pickerOptions(actionsBox = TRUE, title = "Search metrics",
                                                       liveSearch = TRUE, liveSearchPlaceholder = "Select metrics",
                                                       liveSearchStyle = "contains", selectedTextFormat = 'count > 1',
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
                                   options = pickerOptions(actionsBox = TRUE, title = "Search instrument",
                                                           liveSearch = TRUE, liveSearchPlaceholder = "Select instrument",
                                                           liveSearchStyle = "contains", selectedTextFormat = 'count > 1',
                                                           countSelectedText = "{0} of {1} instrument(s)"),
                                   choicesOpt = list(style = rep("color: black; background: white", 500)))
                   ),
                   
                   div(id = ns("lab_parent"),
                       pickerInput(inputId = ns("lab"),
                                   label = tagList("Select lab", info_icon("Choose lab(s) to include")),
                                   choices = NULL, selected = NULL, multiple = TRUE,
                                   options = pickerOptions(actionsBox = TRUE, title = "Search lab",
                                                           liveSearch = TRUE, liveSearchPlaceholder = "Select lab",
                                                           liveSearchStyle = "contains", selectedTextFormat = 'count > 1',
                                                           countSelectedText = "{0} of {1} lab(s)"),
                                   choicesOpt = list(style = rep("color: black; background: white", 500)))
                   )
                   
      ), # end sidebarPanel
      
      # ── Main panel ────────────────────────────────────────────────────────
      mainPanel(width = 10,
                
                div(class = "gt-landing-topbar",
                    div(
                      # Launches the animated guided tour (see www/gt-tour.js).
                      actionLink(
                        ns("about_info"),
                        label = tagList(icon("info-circle"), "Quick Tour"),
                        style = "font-size: 15px; color: #185FA5; cursor: pointer; text-decoration: none;"
                      )
                    ),
                    div(style = "display: flex; gap: 10px; align-items: center;",
                        uiOutput(ns("last_refresh_text")),
                        actionButton(
                          ns("refetch_btn"),
                          label = tagList(icon("refresh"), "Refresh Metrics"),
                          class = "btn btn-gt-refresh"
                        )
                    )
                ),
                
                uiOutput(ns("stat_cards")),
                
                shinycssloaders::withSpinner(uiOutput(ns("plot_1"))),
                
                textOutput(ns("cache_status")),
                verbatimTextOutput(ns("debug_metric")),
                htmlOutput(ns("warnmsg"))
                
      ) # end mainPanel
    ), # end sidebarLayout
    
    div(id = "downloadButtonContainer",
        downloadButton(ns('dataDownload'), "Download selected metrics",
                       style = "position: fixed; bottom: 60px; left: 20px; z-index: 1000;")
    ),
    
    # ── Floating "scroll to next plot" indicator ────────────────────────────
    # ── Floating up/down "scroll to plot" navigation ─────────────────────────
    tags$div(
      id = "scroll-plot-nav",
      style = "position: fixed; bottom: 30px; right: 30px; z-index: 1000;
                display: flex; flex-direction: column; gap: 10px;",
      
      tags$div(
        id = "scroll-prev-plot-btn",
        title = "Scroll to previous plot",
        style = "width: 46px; height: 46px; border-radius: 50%;
                  background-color: #185FA5; color: white;
                  display: flex; align-items: center; justify-content: center;
                  box-shadow: 0 2px 10px rgba(0,0,0,0.25); cursor: pointer;",
        tags$i(class = "fa fa-chevron-up", style = "font-size: 18px;")
      ),
      
      tags$div(
        id = "scroll-next-plot-btn",
        title = "Scroll to next plot",
        style = "width: 46px; height: 46px; border-radius: 50%;
                  background-color: #185FA5; color: white;
                  display: flex; align-items: center; justify-content: center;
                  box-shadow: 0 2px 10px rgba(0,0,0,0.25); cursor: pointer;
                  animation: gt-bounce 1.8s infinite;",
        tags$i(class = "fa fa-chevron-down", style = "font-size: 18px;")
      )
    ),
    
    tags$style(HTML("
      @keyframes gt-bounce {
        0%, 100% { transform: translateY(0); }
        50%      { transform: translateY(6px); }
      }
      #scroll-prev-plot-btn:hover,
      #scroll-next-plot-btn:hover { background-color: #0C447C; }
    ")),
    
    tags$script(HTML("
      $(document).ready(function() {

        function gtGetPlots() {
          return $('.gt-plot-card-anchor');
        }
        // Returns each card's top position RELATIVE TO THE SCROLL CONTAINER (not the page)
        function gtGetCardPositions() {
          var container = gtGetScrollContainer();
          var containerScrollTop = container.scrollTop();
          var containerOffsetTop = container.offset().top;

          var plots = gtGetPlots();
          var positions = [];
          plots.each(function() {
            var relativeTop = $(this).offset().top - containerOffsetTop + containerScrollTop;
            positions.push({ el: $(this), top: relativeTop });
          });
          return positions;
        }

        function gtIsAtBottom() {
          var container = gtGetScrollContainer();
          if (container.is(window)) {
            return ($(document).height() - $(window).height() - $(window).scrollTop()) < 30;
          }
          var el = container[0];
          return (el.scrollHeight - el.clientHeight - el.scrollTop) < 30;
        }

        function gtIsAtTop() {
          var container = gtGetScrollContainer();
          var scrollTop = container.is(window) ? $(window).scrollTop() : container.scrollTop();
          return scrollTop < 30;
        }

        function gtFindNextPlot() {
          var cards = gtGetCardPositions();
          if (cards.length === 0) return null;

          var container = gtGetScrollContainer();
          var currentScroll = container.is(window) ? $(window).scrollTop() : container.scrollTop();
          
          var containerHeight = container.is(window) ? $(window).height() : container.height();
          var threshold = currentScroll + containerHeight * 0.4;


          for (var i = 0; i < cards.length; i++) {
            if (cards[i].top > threshold) {
              return cards[i].el;
            }
          }
          return null;
        }

        function gtFindPrevPlot() {
          var cards = gtGetCardPositions();
          if (cards.length === 0) return null;

          var container = gtGetScrollContainer();
          var currentScroll = container.is(window)
            ? $(window).scrollTop()
            : container.scrollTop();

          var containerHeight = container.is(window) ? $(window).height() : container.height();
          var threshold = currentScroll - containerHeight * 0.2;


          // Walk BACKWARDS to find the first card above current view
          for (var i = cards.length - 1; i >= 0; i--) {
            if (cards[i].top < threshold - 10) {
              return cards[i].el;
            }
          }

          return null;
        }

        function gtSetVisible(selector, visible) {
          var el = $(selector);
          var isVisible = el.is(':visible');
          if (visible && !isVisible) {
            el.stop(true, true).fadeIn(150);
          } else if (!visible && isVisible) {
            el.stop(true, true).fadeOut(150);
          }
        }
        
        function gtUpdateScrollBtnVisibility() {
          var hasNext = !gtIsAtBottom();
          var hasPrev = !gtIsAtTop();
          gtSetVisible('#scroll-next-plot-btn', hasNext);
          gtSetVisible('#scroll-prev-plot-btn', hasPrev);
        }
        
        $(document).on('click', '#scroll-next-plot-btn', function() {
          var container = gtGetScrollContainer();
          var next = gtFindNextPlot();
          console.log('[gt-click-next] currentScroll=',
            container.is(window) ? $(window).scrollTop() : container.scrollTop(),
            'nextCardFound=', !!next,
            'nextOffsetTop=', next ? next.offset().top : null);
          if (!next) return;

          var containerOffsetTop = container.is(window) ? 0 : container.offset().top;
          var containerScrollTop = container.is(window) ? $(window).scrollTop() : container.scrollTop();
          var targetTop = next.offset().top - containerOffsetTop + containerScrollTop - 20;

          console.log('[gt-click-next] targetTop=', targetTop);

          if (container.is(window)) {
            $('html, body').stop().animate({ scrollTop: targetTop }, {duration: 350, easing: 'swing',step: function() {
              gtUpdateScrollBtnVisibility();}, complete: function() {gtUpdateScrollBtnVisibility();}});
          } else {
            container.stop().animate({ scrollTop: targetTop }, {duration: 350, easing: 'swing',step: function() {
              gtUpdateScrollBtnVisibility();}, complete: function() {gtUpdateScrollBtnVisibility();}});
          }
        });

        $(document).on('click', '#scroll-prev-plot-btn', function() {
          var container = gtGetScrollContainer();
          var prev = gtFindPrevPlot();
          console.log('[gt-click-prev] currentScroll=',
            container.is(window) ? $(window).scrollTop() : container.scrollTop(),
            'prevCardFound=', !!prev,
            'prevOffsetTop=', prev ? prev.offset().top : null);
          if (!prev) {
            if (container.is(window)) {
              $('html, body').stop().animate({ scrollTop: 0 }, {duration: 350, easing: 'swing', step: function() {
                gtUpdateScrollBtnVisibility();},complete: function() {gtUpdateScrollBtnVisibility();}});
            } else {
              container.stop().animate({ scrollTop: 0 }, {duration: 350, easing: 'swing', step: function() {
                gtUpdateScrollBtnVisibility();},complete: function() {gtUpdateScrollBtnVisibility();}});
            }
            return;
          }

          var containerOffsetTop = container.is(window) ? 0 : container.offset().top;
          var containerScrollTop = container.is(window) ? $(window).scrollTop() : container.scrollTop();
          var targetTop = prev.offset().top - containerOffsetTop + containerScrollTop - 20;

          console.log('[gt-click-prev] targetTop=', targetTop);

          if (container.is(window)) {
            $('html, body').stop().animate({ scrollTop: targetTop }, 400, gtUpdateScrollBtnVisibility);
          } else {
            container.stop().animate({ scrollTop: targetTop }, 400, gtUpdateScrollBtnVisibility);
          }
        });

        
        function gtGetScrollContainer() {
          var el = $('.tab-pane.active div[style*=\"overflow-y: auto\"]').first();
          if (el.length === 0) {
            console.log('[gt-container] FALLBACK to window — selector matched nothing');
            return $(window);
          }
          console.log('[gt-container] found element:', el[0],
            'scrollHeight=', el[0].scrollHeight,
            'clientHeight=', el[0].clientHeight,
            'currentScrollTop=', el[0].scrollTop);
          return el;
        }
        
        $(document).on('scroll', function() {
          gtUpdateScrollBtnVisibility();
        });


        $(document).on('shiny:value', function(e) {
          if (e.name && e.name.indexOf('plot_1') === 0) {
            setTimeout(gtUpdateScrollBtnVisibility, 400);
          }
        });

        $(document).on('shiny:idle', function() {
          setTimeout(gtUpdateScrollBtnVisibility, 400);
        });

        setTimeout(gtUpdateScrollBtnVisibility, 1200);

        $(document).on('keydown', function(e) {
          if (e.key === 'ArrowDown') {
            $('#scroll-next-plot-btn').click();
          }
          if (e.key === 'ArrowUp') {
            $('#scroll-prev-plot-btn').click();
          }
        });

      });
    "))

      ) # end tagList
}