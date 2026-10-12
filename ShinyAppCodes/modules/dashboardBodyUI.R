module2_body_UI <- function(id) {
  ns <- NS(id)
  tagList(
    # ── Report viewport height so plot panels can size to it ──────────────
    # Sent as a plain (hardcoded-namespace) input rather than through ns()
    # because this script isn't itself part of a renderUI — it's static
    # markup emitted once, so it can't call the ns() closure. "Module2" is
    # the module's actual id everywhere else it's instantiated
    # (module2_sidebar_UI("Module2") etc.), so this reliably lands as
    # input$window_h inside module2_Server.
    tags$script(HTML("
      (function() {
        function reportHeight() {
          Shiny.setInputValue('Module2-window_h', window.innerHeight, {priority: 'event'});
        }
        var t;
        function debounced() { clearTimeout(t); t = setTimeout(reportHeight, 200); }
        $(document).ready(reportHeight);
        $(document).on('shiny:connected', reportHeight);
        $(window).on('resize', debounced);
      })();
    ")),

    # ── Always-visible scroll indicators INSIDE scrollable panels only ──────
    # Deliberately nothing here for the page's own scrollbar — only the Plot
    # panel (.scrollable-plot-container, a fixed-height box the plotly
    # figure sits in) and the Download tab's data table, which each scroll
    # INSIDE their own box (overflow-x/y: auto). Any element tagged
    # class=\"gt-scrollbox\" gets a custom track+thumb, covering both axes
    # since a wide bar chart or wide table can need horizontal scrolling as
    # well as vertical. The container's OWN native scrollbar is hidden
    # (scrollbar-width/::-webkit-scrollbar) so this custom one — which
    # actually tracks scroll position, not just a static decoration — is the
    # only one visible; leaving both on screen at once looked like two
    # separate, conflicting scrollbars.
    tags$style(HTML("
      .gt-scrollbox, .dataTables_scrollBody { position: relative; }
      .gt-scrollbox, .dataTables_scrollBody { scrollbar-width: none; }
      .gt-scrollbox::-webkit-scrollbar, .dataTables_scrollBody::-webkit-scrollbar { display: none; }
      .gt-scrollbox-vtrack {
        position: absolute; top: 4px; right: 4px; width: 10px;
        background: rgba(24, 95, 165, 0.12); border-radius: 5px;
        z-index: 500; pointer-events: auto; display: none; cursor: pointer;
      }
      .gt-scrollbox-vthumb {
        position: absolute; left: 0; width: 100%; min-height: 24px;
        background: #6FA3D8; border-radius: 5px; cursor: pointer;
      }
      .gt-scrollbox-vthumb:hover, .gt-scrollbox-vthumb.gt-dragging { background: #4A85C4; }
      .gt-scrollbox-htrack {
        position: absolute; left: 4px; bottom: 4px; height: 10px;
        background: rgba(24, 95, 165, 0.12); border-radius: 5px;
        z-index: 500; pointer-events: auto; display: none; cursor: pointer;
      }
      .gt-scrollbox-hthumb {
        position: absolute; top: 0; height: 100%; min-width: 24px;
        background: #6FA3D8; border-radius: 5px; cursor: pointer;
      }
      .gt-scrollbox-hthumb:hover, .gt-scrollbox-hthumb.gt-dragging { background: #4A85C4; }
    ")),
    tags$script(HTML("
      (function() {
        // Makes the custom indicator an actual scrollbar, not just a
        // read-only decoration: drag the thumb to scroll, or click
        // elsewhere on the track to jump toward that point — the two
        // interactions people expect from any real scrollbar.
        function makeDraggable(el, track, thumb, axis) {
          function metrics() {
            return {
              trackSize: axis === 'v' ? track.clientHeight : track.clientWidth,
              thumbSize: axis === 'v' ? thumb.clientHeight : thumb.clientWidth,
              maxScroll: axis === 'v' ? (el.scrollHeight - el.clientHeight) : (el.scrollWidth - el.clientWidth)
            };
          }
          function setScroll(v) {
            if (axis === 'v') el.scrollTop = v; else el.scrollLeft = v;
          }
          function currentScroll() {
            return axis === 'v' ? el.scrollTop : el.scrollLeft;
          }
          function pointerPos(e) {
            var t = (e.touches && e.touches[0]) || e;
            return axis === 'v' ? t.clientY : t.clientX;
          }

          // Mouse, touch AND pointer listeners together: different input
          // paths (trackpad vs. touch vs. pen, and some automated/remote
          // input methods) don't all reliably generate every event type, so
          // covering all three is what makes this work as broadly as an
          // actual native scrollbar would.
          var dragging = false;
          thumb.addEventListener('mousedown', startDrag);
          thumb.addEventListener('touchstart', startDrag, { passive: false });
          thumb.addEventListener('pointerdown', startDrag);

          function startDrag(e) {
            // A single real click/tap fires more than one of these event
            // types (e.g. pointerdown AND mousedown) — without this guard
            // each would start its own independent drag, doubling up the
            // scroll-per-pixel rate.
            if (dragging) return;
            dragging = true;
            e.preventDefault();
            e.stopPropagation();
            var m = metrics();
            var startPos = pointerPos(e);
            var startScroll = currentScroll();
            var maxThumbPos = Math.max(1, m.trackSize - m.thumbSize);
            thumb.classList.add('gt-dragging');
            document.body.style.userSelect = 'none';

            function onMove(ev) {
              var delta = pointerPos(ev) - startPos;
              var scrollDelta = (delta / maxThumbPos) * m.maxScroll;
              setScroll(Math.min(m.maxScroll, Math.max(0, startScroll + scrollDelta)));
            }
            function onEnd() {
              dragging = false;
              thumb.classList.remove('gt-dragging');
              document.body.style.userSelect = '';
              document.removeEventListener('mousemove', onMove);
              document.removeEventListener('mouseup', onEnd);
              document.removeEventListener('touchmove', onMove);
              document.removeEventListener('touchend', onEnd);
              document.removeEventListener('pointermove', onMove);
              document.removeEventListener('pointerup', onEnd);
            }
            document.addEventListener('mousemove', onMove);
            document.addEventListener('mouseup', onEnd);
            document.addEventListener('touchmove', onMove, { passive: false });
            document.addEventListener('touchend', onEnd);
            document.addEventListener('pointermove', onMove);
            document.addEventListener('pointerup', onEnd);
          }

          // Click on the track itself (not the thumb) — jump so the thumb
          // is centered under the click, like a native scrollbar's track.
          track.addEventListener('mousedown', function(e) {
            if (e.target === thumb) return;
            var rect = track.getBoundingClientRect();
            var m = metrics();
            var clickPos = pointerPos(e) - (axis === 'v' ? rect.top : rect.left);
            var maxThumbPos = Math.max(1, m.trackSize - m.thumbSize);
            var targetThumbPos = Math.min(maxThumbPos, Math.max(0, clickPos - m.thumbSize / 2));
            setScroll((targetThumbPos / maxThumbPos) * m.maxScroll);
          });
        }

        function ensureIndicators(el) {
          if (el._gtScroll) return el._gtScroll;
          var vtrack = document.createElement('div'); vtrack.className = 'gt-scrollbox-vtrack';
          var vthumb = document.createElement('div'); vthumb.className = 'gt-scrollbox-vthumb';
          vtrack.appendChild(vthumb);
          var htrack = document.createElement('div'); htrack.className = 'gt-scrollbox-htrack';
          var hthumb = document.createElement('div'); hthumb.className = 'gt-scrollbox-hthumb';
          htrack.appendChild(hthumb);
          el.appendChild(vtrack);
          el.appendChild(htrack);
          // Direct listener: 'scroll' doesn't reliably bubble to a
          // delegated document-level handler across browsers.
          el.addEventListener('scroll', function() { updateBox(el); });
          makeDraggable(el, vtrack, vthumb, 'v');
          makeDraggable(el, htrack, hthumb, 'h');
          // Stored directly on the element (not re-looked-up via
          // querySelector) because el can itself contain ANOTHER
          // .gt-scrollbox/.dataTables_scrollBody nested inside it (e.g. the
          // Plot panel's own wrapper around a DT table elsewhere) —
          // el.querySelector('.gt-scrollbox-vtrack') would happily match
          // that nested box's track instead of this element's own, since it
          // searches all descendants, not just direct children.
          el._gtScroll = { vtrack: vtrack, vthumb: vthumb, htrack: htrack, hthumb: hthumb };
          return el._gtScroll;
        }

        function updateBox(el) {
          var refs = ensureIndicators(el);
          var vtrack = refs.vtrack, vthumb = refs.vthumb, htrack = refs.htrack, hthumb = refs.hthumb;
          if (!vtrack || !htrack) return;

          var vOverflow = (el.scrollHeight - el.clientHeight) > 4;
          var hOverflow = (el.scrollWidth - el.clientWidth) > 4;

          vtrack.style.display = vOverflow ? 'block' : 'none';
          htrack.style.display = hOverflow ? 'block' : 'none';
          vtrack.style.bottom = hOverflow ? '14px' : '4px';
          htrack.style.right = vOverflow ? '14px' : '4px';

          if (vOverflow) {
            var vTrackH = vtrack.clientHeight;
            var vThumbH = Math.max(24, vTrackH * (el.clientHeight / el.scrollHeight));
            var vMax = el.scrollHeight - el.clientHeight;
            vthumb.style.height = vThumbH + 'px';
            vthumb.style.top = (el.scrollTop / vMax) * (vTrackH - vThumbH) + 'px';
          }
          if (hOverflow) {
            var hTrackW = htrack.clientWidth;
            var hThumbW = Math.max(24, hTrackW * (el.clientWidth / el.scrollWidth));
            var hMax = el.scrollWidth - el.clientWidth;
            hthumb.style.width = hThumbW + 'px';
            hthumb.style.left = (el.scrollLeft / hMax) * (hTrackW - hThumbW) + 'px';
          }
        }

        function updateAll() {
          // .dataTables_scrollBody: DT's own internal scrolling div when a
          // table uses scrollX/scrollY (dataDownload, species_table, etc.)
          // — the horizontal (and, if scrollY is set, vertical) scrolling
          // happens THERE, inside DT's own markup, not in the .gt-scrollbox
          // div that merely wraps the whole DT widget.
          document.querySelectorAll('.gt-scrollbox, .dataTables_scrollBody').forEach(updateBox);
        }

        $(document).on('shiny:value shiny:visualchange', function() { setTimeout(updateAll, 200); });
        $(window).on('resize', updateAll);
        setInterval(updateAll, 800);
        updateAll();
      })();
    ")),

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