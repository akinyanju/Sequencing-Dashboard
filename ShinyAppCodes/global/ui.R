ui <- function(request) {
  fluidPage(
    tags$head(
      
      # ── Plotly toolbar position ──────────────────────────────────────────
      tags$style(HTML(".modebar {left: 0 !important; right: auto !important; top: 0;}")),
      
      # ── Google Font: Inter ───────────────────────────────────────────────
      tags$link(rel = "stylesheet",
                href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600&display=swap"),
      
      tags$link(rel = "stylesheet",
                href = "https://cdn.jsdelivr.net/npm/@tabler/icons-webfont@latest/dist/tabler-icons.min.css"),

      # ── Guided tour (driver.js) — powers the "Quick Tour" links on both
      # the landing page and the QC dashboard; see www/gt-tour.js ─────────
      tags$link(rel = "stylesheet",
                href = "https://cdnjs.cloudflare.com/ajax/libs/driver.js/1.8.0/driver.min.css"),
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/driver.js/1.8.0/driver.js.iife.min.js"),
      tags$script(src = "gt-tour.js"),

      # ── "Select all in this group" for grouped metric pickers ────────────
      tags$script(src = "gt-picker-group-select.js"),

      # ── Global base ─────────────────────────────────────────────────────
      tags$style(HTML("
        html, body {
          overflow: hidden;
          font-family: 'Inter', sans-serif;
          background-color: #f0f4f8;
          color: #1e293b;
        }
        .center-container { overflow: hidden !important; }
        .no-scroll { overflow: hidden !important; position: fixed !important; width: 100% !important; }
      ")),
      
      # ── Top-level tab navigation ─────────────────────────────────────────
      tags$style(HTML("
        .nav-tabs {
          font-size: 15px;
          background-color: #ffffff;
          border-bottom: 0.5px solid #e2e8f0 !important;
          padding: 0 16px;
          margin-bottom: 0;
        }
        .nav-tabs > li > a {
          color: #64748b;
          border: none !important;
          border-bottom: 2px solid transparent !important;
          border-radius: 0 !important;
          padding: 11px 20px;
          background: transparent !important;
          font-size: 15px;
          transition: color 0.15s;
        }
        .nav-tabs > li > a:hover {
          color: #185FA5;
          background: transparent !important;
          border-color: transparent !important;
        }
        .nav-tabs > li.active > a,
        .nav-tabs > li.active > a:focus,
        .nav-tabs > li.active > a:hover {
          color: #185FA5 !important;
          border-bottom: 2px solid #185FA5 !important;
          background: transparent !important;
          font-weight: 500;
        }
      ")),
      
      # ── Tab pane ────────────────────────────────────────────────────────
      tags$style(HTML("
        .tab-content { background-color: #f0f4f8; }
        .tab-pane {
          padding: 0;
          background-color: #f0f4f8;
          border-radius: 0;
          box-shadow: none;
        }
      ")),
      
      # ── Navbar ──────────────────────────────────────────────────────────
      tags$style(HTML("
        .gt-navbar {
          background-color: #0C447C;
          height: 50px;
          display: flex;
          align-items: center;
          padding: 0 16px;
          position: sticky;
          top: 0;
          z-index: 1000;
          gap: 0;
        }
        
        /* FIX DROPDOWN POSITION IN CUSTOM NAVBAR */
        .gt-navbar .dropdown-menu {
          right: -40px !important;
          left: auto !important;

          min-width: 320px;
          max-width: 380px;

          max-height: 300px;        /* LIMIT HEIGHT */
          overflow-y: auto;         /*  SCROLL ENABLED */
          overflow-x: hidden;

          word-wrap: break-word;

          border-radius: 8px;
          padding-top: 8px;
          box-shadow: 0 8px 20px rgba(0,0,0,0.2);
        }
        
        /* Brand — always visible, left side */
        .gt-navbar-brand {
          color: #E6F1FB;
          font-size: 16px;
          font-weight: 500;
          display: flex;
          align-items: center;
          gap: 8px;
          text-decoration: none;
          white-space: nowrap;
          flex-shrink: 0;
        }
        .gt-navbar-brand i { font-size: 18px; color: #85B7EB; }

        /* Spacer pushes right group to the far right */
        .gt-navbar-spacer { flex-grow: 1; }

        /* Right group: search + quick tour + badge + logo — all together */
        .gt-navbar-right {
          display: flex;
          align-items: center;
          gap: 10px;
          flex-shrink: 0;
        }

        /* Search wrapper inside right group */
        .gt-navbar-right .global-search-wrapper {
          display: flex;
          align-items: center;
          gap: 6px;
        }
        .gt-navbar-right .global-search-input-container {
          position: relative;
        }
        .gt-navbar-right .global-search-input .form-group {
          margin-bottom: 0 !important;
        }
        .gt-navbar-right .global-search-input input {
          border: 1px solid rgba(255,255,255,0.25) !important;
          border-radius: 8px !important;
          padding: 0 30px 0 12px !important;
          font-size: 13px !important;
          width: 300px !important;
          background: rgba(255,255,255,0.12) !important;
          color: #E6F1FB !important;
          font-family: 'Inter', sans-serif !important;
          height: 34px !important;
          line-height: 34px !important;
          box-sizing: border-box !important;
          margin: 0 !important;
          display: block !important;
        }
        .gt-navbar-right .global-search-input input::placeholder {
          color: #B5D4F4 !important;
          line-height: normal !important;
        }
        .gt-navbar-right .global-search-input input:focus {
          outline: none !important;
          border-color: rgba(255,255,255,0.5) !important;
          background: rgba(255,255,255,0.2) !important;
        }
        .clear-search {
          position: absolute;
          right: 8px;
          top: 50%;
          transform: translateY(-50%);
          cursor: pointer;
          color: #B5D4F4;
          font-size: 16px;
          font-weight: bold;
          user-select: none;
          z-index: 5;
          line-height: 1;
        }
        .clear-search:hover { color: #ffffff; }

        /* Search button — exact same height as input, no extra Shiny margins */
        .global-search-button.btn {
          height: 34px !important;
          width: 36px !important;
          padding: 0 !important;
          margin: 0 !important;
          border-radius: 8px !important;
          background-color: #378ADD !important;
          color: white !important;
          border: none !important;
          cursor: pointer !important;
          display: inline-flex !important;
          align-items: center !important;
          justify-content: center !important;
          font-size: 14px !important;
          line-height: 1 !important;
          vertical-align: middle !important;
          flex-shrink: 0;
        }
        .global-search-button.btn:hover { background-color: #185FA5 !important; }

        /* Quick Tour link */
        .gt-nav-link {
          color: #B5D4F4 !important;
          font-size: 13px;
          font-weight: 500;
          font-family: 'Inter', sans-serif;
          display: inline-flex;
          align-items: center;
          gap: 5px;
          cursor: pointer;
          background: none;
          border: none;
          padding: 4px 8px;
          border-radius: 6px;
          white-space: nowrap;
          text-decoration: none !important;
          vertical-align: middle;
        }
        .gt-nav-link:hover {
          background: rgba(255,255,255,0.1);
          color: #ffffff !important;
        }
        .gt-navbar-right img {
          opacity: 0.9;
          vertical-align: middle;
        }

        /* Shiny wraps uiOutput in a div — make it inline so it doesn't break flex */
        .gt-navbar-right .shiny-html-output {
          display: inline-flex !important;
          align-items: center !important;
        }

      ")),
      
      # ── Notifications ────────────────────────────────────────────────────
      tags$style(HTML("
        .shiny-notification {
          position: fixed; top: 60px; right: 20px; left: auto; bottom: auto;
          z-index: 9999; font-size: 15px; font-family: 'Inter', sans-serif;
          background-color: #0C447C; color: #E6F1FB;
          border-left: 4px solid #5DCAA5; border-radius: 8px;
          box-shadow: 0 4px 12px rgba(0,0,0,0.15);
          padding: 12px 16px; min-width: 280px;
        }
        .shiny-notification-error   { background-color: #1e293b; border-left-color: #D85A30; }
        .shiny-notification-warning { background-color: #1e293b; border-left-color: #EF9F27; }
        .shiny-notification-message { background-color: #0C447C; border-left-color: #5DCAA5; }

        /* Close button X — white on dark backgrounds */
        .shiny-notification-close {
          color: #E6F1FB !important;
          opacity: 0.8;
        }
        .shiny-notification-close:hover {
          color: #f87171 !important;
          opacity: 1;
        }
        /* Close button X — visible on dark notification backgrounds */
      .shiny-notification-close {
        color: #ffffff !important;
        opacity: 0.85;
        font-weight: bold;
      }
      .shiny-notification-close:hover {
        color: #f87171 !important;
        opacity: 1;
      }
      ")),
      
      # ── Info icon ────────────────────────────────────────────────────────
      tags$style(HTML("
        .info-icon { color: #378ADD; font-size: 1.0em; margin-left: 4px; cursor: help; }
        .info-icon:hover { color: #185FA5; }
      ")),
      
      # ── Admin tab background ─────────────────────────────────────────────
      tags$style(HTML("
        #admin_tab {
          background: url('mechanical_background.gif') no-repeat center center fixed;
          background-size: cover; color: white;
        }
      ")),
      
      # ── Stat cards ───────────────────────────────────────────────────────
      tags$style(HTML("
        .gt-stat-card {
          background: #ffffff; border: 0.5px solid #e2e8f0;
          border-radius: 10px; padding: 6px 20px; border-left: 3px solid #378ADD;
        }
        .gt-stat-card.green  { border-left-color: #1D9E75; }
        .gt-stat-card.amber  { border-left-color: #EF9F27; }
        .gt-stat-card.purple { border-left-color: #7F77DD; }
        .gt-stat-card.teal   { border-left-color: #0E9494; }
        .gt-stat-label { font-size: 12px; color: #64748b; text-transform: uppercase; letter-spacing: 0.05em; margin-bottom: 3px; }
        .gt-stat-value { font-size: 26px; font-weight: 700; color: #1e293b; }
        .gt-stat-sub   { font-size: 13px; color: #94a3b8; margin-top: 2px; }
        .gt-stat-row   { display: flex; gap: 8px; margin-bottom: 10px; flex-wrap: nowrap; }
        .gt-stat-card  { flex: 1 1 0; min-width: 0; }
        .gt-stat-card.wide { flex: 1.6 1 0; }
      ")),
      
      # ── Sidebar styling ──────────────────────────────────────────────────
      tags$style(HTML("
        .gt-sidebar { background: #ffffff; border-right: 0.5px solid #e2e8f0; padding: 16px 14px; }
        .gt-sidebar-head { font-size: 10px; font-weight: 500; color: #64748b; text-transform: uppercase; letter-spacing: 0.07em; padding-bottom: 8px; border-bottom: 0.5px solid #e2e8f0; margin-bottom: 12px; }
        .gt-sidebar .form-group label, .gt-sidebar .control-label { font-size: 11px !important; color: #64748b !important; font-weight: 400 !important; }
        .gt-sidebar .selectize-input, .gt-sidebar .bootstrap-select .btn { background: #f8fafc !important; border: 0.5px solid #e2e8f0 !important; border-radius: 8px !important; font-size: 12px !important; color: #1e293b !important; box-shadow: none !important; }
        .gt-sidebar .radio label, .gt-sidebar .checkbox label { font-size: 12px !important; color: #1e293b !important; }
      ")),
      
      # ── Plot cards ───────────────────────────────────────────────────────
      tags$style(HTML("
        .gt-plot-card { background: #ffffff; border: 0.5px solid #e2e8f0; border-radius: 10px; padding: 14px; margin-bottom: 10px; }
        .gt-topbar { background: #ffffff; border: 0.5px solid #e2e8f0; border-radius: 10px; padding: 10px 16px; display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px; }
      ")),
      
      # ── Button styles ────────────────────────────────────────────────────
      tags$style(HTML("
        .btn-gt-refresh { background-color: #185FA5 !important; color: #ffffff !important; border: none !important; border-radius: 8px !important; font-size: 14px !important; font-weight: 500 !important; padding: 8px 16px !important; }
        .btn-gt-refresh:hover { background-color: #0C447C !important; }
        .btn-gt-download { background-color: #185FA5 !important; color: #E6F1FB !important; border: none !important; border-radius: 8px !important; font-size: 12px !important; font-weight: 500 !important; padding: 7px 14px !important; }
        .btn-gt-download:hover { background-color: #0C447C !important; }
      ")),
      
      # ── iFrame scroll handler ────────────────────────────────────────────
      tags$script(HTML("
        document.addEventListener('DOMContentLoaded', function() {
          const iframeObserver = new MutationObserver(() => {
            const iframe = document.querySelector('iframe');
            if (iframe && !iframe.hasListenerAttached) {
              iframe.hasListenerAttached = true;
              iframe.addEventListener('mouseenter', function() {
                document.documentElement.classList.add('no-scroll');
                document.body.classList.add('no-scroll');
              });
              iframe.addEventListener('mouseleave', function() {
                document.documentElement.classList.remove('no-scroll');
                document.body.classList.remove('no-scroll');
              });
            }
          });
          iframeObserver.observe(document.body, { childList: true, subtree: true });
        });
      ")),
      
      tags$style(HTML("
        .radio-inline.active-pill { background: #E6F1FB !important; border: 1px solid #378ADD !important; color: #185FA5 !important; font-weight: 500 !important; }
        .radio-inline.inactive-pill { background: #f8fafc !important; border: 1px solid #64748b !important; color: #475569 !important; }

        /* 'Pointer' treatment for the header's task/notification dropdown
           (shinydashboard::dropdownMenu). The arrow is appended to the dropdown
           li wrapper, NOT the .dropdown-menu itself, so it doesn't depend on
           (or break) the menu's own overflow/scroll clipping for its long item list. */
        .tasks-menu .dropdown-menu { margin-top: 14px !important; }
        .dropdown-pointer-arrow {
          position: absolute;
          width: 0;
          height: 0;
          border-width: 0 14px 14px;
          border-style: solid;
          border-color: transparent transparent #4497f2 transparent;
        }
      ")),
      
      # ── Clear-search JS ──────────────────────────────────────────────────
      tags$script(HTML(
        "$(document).on('click', '.clear-search', function() {
           var inputId = 'Module2-global_search';
           $('#' + inputId).val('');
           Shiny.setInputValue(inputId, '', {priority: 'event'});
         });"
      )),
      # ── Task/notification dropdown arrow (shinydashboard::dropdownMenu) ──
      # The "Quick Tour" links used to be a manually-toggled Bootstrap popover
      # driven from here; that's been replaced by the driver.js tour in
      # www/gt-tour.js, so only the notification-dropdown arrow logic remains.
      tags$script(HTML(
        "// Same pointer treatment for the task/notification dropdown: add a small arrow
         // pinned under its toggle, recomputed each time it opens. Dropdowns (unlike
         // popovers) show instantly with no fade transition, so 'shown.bs.dropdown'
         // fires synchronously — no flicker risk here.
         $(document).on('shown.bs.dropdown', function (e) {
           var $li = $(e.target);
           var $toggle = $li.children('a.dropdown-toggle').first();
           var $menu = $li.children('.dropdown-menu').first();
           if (!$toggle.length || !$menu.length) return;
           var tRect = $toggle[0].getBoundingClientRect();
           var mRect = $menu[0].getBoundingClientRect();
           var liRect = $li[0].getBoundingClientRect();
           var arrowLeft = (tRect.left + tRect.width / 2) - liRect.left - 14;
           var arrowTop = mRect.top - liRect.top - 14;
           var $arrow = $li.children('.dropdown-pointer-arrow');
           if (!$arrow.length) {
             $arrow = $('<div class=\"dropdown-pointer-arrow\"></div>').appendTo($li);
           }
           $arrow.css({ left: arrowLeft + 'px', top: arrowTop + 'px' });
         });
         // The arrow lives outside .dropdown-menu (so it isn't clipped by the menu's
         // own scroll container), which means the CSS that hides .dropdown-menu on
         // close doesn't hide it too — remove it explicitly when the dropdown closes.
         $(document).on('hidden.bs.dropdown', function (e) {
           $(e.target).children('.dropdown-pointer-arrow').remove();
         });"
      ))
    ), # end tags$head
    
    # ── Single navy navbar ───────────────────────────────────────────────
    div(class = "gt-navbar",
        
        # Brand — always visible
        tags$span(class = "gt-navbar-brand",
                  tags$i(class = "fa fa-dna", `aria-hidden` = "true"),
                  "GT QC Dashboard"
        ),
        
        # Spacer
        div(class = "gt-navbar-spacer"),
        
        # Right group — only rendered after login (server controls this)
        uiOutput("Module2-navbar_authenticated_items")
    ),
    
    # ── Main tabset ──────────────────────────────────────────────────────
    div(style = "position: relative;",
        
        tabsetPanel(id = "main_tab",
                    tabPanel("Sequence Data Generated",
                             div(style = "max-height: calc(100vh - 100px); overflow-y: auto; overflow-x: hidden; padding-right: 10px;",
                                 module1_UI("Module1")
                             )
                    ),
                    tabPanel("Login to QC metrics dashboard",
                             uiOutput("dynamic_ui_wrapper")
                    )
        ),
        
        # Refetch bar floats to the right of the tab row
        div(
          style = "position: absolute; top: 8px; right: 8px; z-index: 500;",
          uiOutput("Module2-refetch_bar_ui")
        )
    )
    
  )
}