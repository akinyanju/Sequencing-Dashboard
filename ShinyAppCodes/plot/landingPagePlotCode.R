
##################################################################
#This part of the code create a list of data to plot, for input$platform herewith refers as pf and input$metrics refers as metrics
output$plot_1 <- renderUI({
  ns <- session$ns

  # Show spinner if plots aren't ready OR no selection made
  if (!plots_ready() ||
      is.null(input$metrics) || length(input$metrics) == 0 ||
      is.null(input$platform) || length(input$platform) == 0) {
    
    return(
      div(
        style = "display: flex; align-items: center; justify-content: center; height: 400px;",
        div(
          style = "background-color: #098207; border: 1px solid #ddd; border-radius: 12px; padding: 30px 40px; box-shadow: 0 4px 12px rgba(0,0,0,0.05); text-align: center; max-width: 500px;",
          tags$i(class = "fas fa-spinner fa-spin fa-2x", style = "color: #020305; margin-bottom: 15px;"),
          h2("Preparing your plots..."),
          p("Loading data selections... Please wait or choose metrics and platform.",
            style = "font-size: 16px; color: #e4f5f4; margin: 0;")
        )
      )
    )
  }
  
  selected_metrics <- input$metrics
  if (input$more_plot == "Yes" && input$more_plots_opt == "Project") {
    selected_metrics <- selected_metrics[1]
  }
  
  
  plot_output_list <- lapply(input$platform, function(pf) {
    lapply(selected_metrics, function(metric) {
      plotlyOutput(ns(paste0("plot_1_", pf, "_", metric)), height = "500px")
    })
  }) |> unlist(recursive = FALSE)
  
  do.call(tagList, plot_output_list)
})
##################################################################
#Observer will keep eyes on users selection choices and reactive accrodingly. if users select one platform and a metric, then just a plot will show and so on
#Let say user select ILLUMINA, ONT, then Reads, Bases, and Bytes, that would be 2 x 3 plots. This will be held in do.call roll for pf and metrics, gets looped and show
#the corresponding plot, one at a time.

#--------- Reactive value to track if plot is completed. Otherwise, show that plot is loading---------
plots_ready <- reactiveVal(FALSE)
#--------- Trigger render when inputs change ---------
observe({
  req(input$metrics, input$platform)
  
  plots_ready(FALSE)  # Reset every time input changes
  # Collect all unique year_periods across selected metrics
  all_periods <- sort(unique(unlist(lapply(input$metrics, function(metric) {
    df <- react_input_grp()[[metric]]
    df$year_period
  }))))
  
  for (pf in input$platform) {
    for (metric in input$metrics) {
      local({
        pf_local <- pf
        metric_local <- metric
        output_name <- paste0("plot_1_", pf_local, "_", metric_local)
        
        output[[output_name]] <- renderPlotly({
          req(input$option, input$more_plot, input$more_plots_opt)
          
          xlegend <- if (input$option == "Yes") "Months" else "Quarters"
          ylegend <- if (metric_local == "Bases") paste0(metric_local, " (Gb)") else paste0(metric_local, " (G)")
          MetricsUnit <- if (metric_local == "Bases") " (Gb)" else " (G)"
          ##################################################################
          #Filter to include both the selected metrics and platform data
          df_metric <- react_input_grp()[[metric_local]]
          req(!is.null(df_metric))
          df_filtered <- df_metric[df_metric$Platform == pf_local, ]
          req(nrow(df_filtered) > 0)
          ##################################################################
          # Updated safe getAggregated so that if plot is or not required to plot platform and metrics, it will react accordinly
          getAggregated <- function(func_name, platform) {
            func <- match.fun(func_name)
            if ("platform" %in% names(formals(func))) {
              func(metric_local, platform)
            } else {
              func(metric_local)
            }
          }
          ##################################################################
          title_text <- paste(pf_local, "-", metric_local)
          
          ##################################################################
          plotMachine <- function() {
            getAggregated("react_input_grp_Instrument", pf_local) %>%
              mutate(InstrumentID = ifelse(is.na(InstrumentID) | InstrumentID == "NULL", "Unknown Machine", InstrumentID)) %>%
              plot_ly(x = ~factor(year_period, levels = all_periods), y = ~value_sum / 1e9,
                      type = 'bar', split = ~InstrumentID,
                      hoverinfo = 'text',
                      text = ~paste0('</br> Machine: ', InstrumentID,
                                     '</br> Value: ', round(value_sum / 1e9, 2), MetricsUnit,
                                     '</br> Period: ', year_period),
                      textposition = "none") %>%
              layout(
                hovermode = "x",
                showlegend = TRUE, legend = list(orientation = 'v', x = 1.05, y = 1, xanchor = "left"),margin = list(r = 150),
                yaxis = list(title = ylegend),
                xaxis = list(title = xlegend, tickangle = 300),
                barmode = 'stack',
                annotations = list(list(text = title_text,x = 0,y = 1.05,  xref = "paper",yref = "paper",showarrow = FALSE,font = list(size = 18, family = "Arial", color = "black"), xanchor = "left")
                )
              ) %>% config(responsive = TRUE)
          }
          
          
          plotLab <- function() {
            getAggregated("react_input_grp_lab", pf_local) %>%
              plot_ly(x = ~groupFolder, y = ~value_sum / 1e9,
                      type = 'bar', split = ~year_period,
                      hoverinfo = 'text',
                      text = ~paste0('</br> Group: ', groupFolder,
                                     '</br> Value: ', round(value_sum / 1e9, 2), MetricsUnit,
                                     '</br> Period: ', year_period),
                      textposition = "none") %>%
              layout(
                hovermode = "x",
                showlegend = TRUE, legend = list(orientation = 'v', x = 1.05, y = 1, xanchor = "left"),margin = list(r = 150),
                yaxis = list(title = ylegend),
                xaxis = list(title = "Group(s)", tickangle = 300,
                             categoryorder = "array", categoryarray = ~ reorder(year_period, value_sum)),
                barmode = 'stack',
                annotations = list(list(text = title_text,x = 0,y = 1.05,  xref = "paper",yref = "paper",showarrow = FALSE,font = list(size = 18, family = "Arial", color = "black"), xanchor = "left"))
              )  %>% config(responsive = TRUE)
          }
          
          plotProject <- function() {
            getAggregated("react_input_grp_count", pf_local) %>%
              plot_ly(x = ~factor(year_period, levels = all_periods), y = ~AppCount,
                      type = 'bar', split = ~Application,
                      hoverinfo = 'text',
                      text = ~paste0('</br> App: ', Application,
                                     '</br> Value: ', AppCount,
                                     '</br> Period: ', year_period),
                      textposition = "none") %>%
              layout(
                hovermode = "x",
                showlegend = TRUE, legend = list(orientation = 'v', x = 1.05, y = 1, xanchor = "left"),margin = list(r = 150),
                yaxis = list(title = "Project count"),
                xaxis = list(title = xlegend, tickangle = 300,
                             categoryorder = "array", categoryarray = ~ reorder(year_period, AppCount)),
                barmode = 'stack',
                annotations = list(list(text = paste(pf_local),x = 0,y = 1.05,  xref = "paper",yref = "paper",showarrow = FALSE,font = list(size = 18, family = "Arial", color = "black"), xanchor = "left"))
              )  %>% config(responsive = TRUE)
          }
          
          plotMerge <- function() {
            getAggregated("react_input_grp_App_Merge", pf_local) %>%
              plot_ly(x = ~factor(year_period, levels = all_periods), y = ~value_sum / 1e9,
                      type = 'bar', split = ~Application,
                      hoverinfo = 'text',
                      text = ~paste0('</br> App: ', Application,
                                     '</br> Value: ', round(value_sum / 1e9, 2), MetricsUnit,
                                     '</br> Period: ', year_period),
                      textposition = "none") %>%
              layout(
                hovermode = "x",
                showlegend = TRUE, legend = list(orientation = 'v', x = 1.05, y = 1, xanchor = "left"),margin = list(r = 150),
                yaxis = list(title = ylegend),
                xaxis = list(title = xlegend, tickangle = 300,
                             categoryorder = "array", categoryarray = ~ reorder(year_period, value_sum)),
                barmode = 'stack',
                annotations = list(list(text = title_text,x = 0,y = 1.05,  xref = "paper",yref = "paper",showarrow = FALSE,font = list(size = 18, family = "Arial", color = "black"), xanchor = "left"))
              )  %>% config(responsive = TRUE)
          }
          
          plotSplit <- function() {
            df_split <- getAggregated("react_input_grp_App_Split", pf_local)
            CT <- df_split %>%
              filter(Site %in% c("Farmington", "CT")) %>%
              plot_ly(x = ~factor(year_period, levels = all_periods), y = ~value_sum / 1e9,
                      type = 'bar', split = ~Application,
                      hoverinfo = 'text',
                      text = ~paste0('</br> App: ', Application,
                                     '</br> Value: ', round(value_sum / 1e9, 2), MetricsUnit,
                                     '</br> Period: ', year_period),
                      textposition = "none") %>%
              add_trace(y=~Application, showlegend = F) %>%
              layout(
                hovermode = "x",
                xaxis = list(title = "Farmington", tickangle = 300, showticklabels = TRUE,
                             tickfont = list(size = 12),
                             categoryorder = "array", categoryarray = ~ reorder(year_period, value_sum)),showlegend = F
                )  %>% config(responsive = TRUE)
            
            BH <- df_split %>%
              filter(Site %in% c("Bar Harbor", "BH")) %>%
              plot_ly(x = ~factor(year_period, levels = all_periods), y = ~value_sum / 1e9,
                      type = 'bar', split = ~Application,
                      hoverinfo = 'text',
                      text = ~paste0('</br> App: ', Application,
                                     '</br> Value: ', round(value_sum / 1e9, 2), MetricsUnit,
                                     '</br> Period: ', year_period),
                      textposition = "none")%>%
              add_trace(y=~Application, showlegend = F) %>%
              layout(
                hovermode = "x",
                xaxis = list(title = "Bar Harbor", tickangle = 300, showticklabels = TRUE, tickfont = list(size = 12),
                             categoryorder = "array", categoryarray = ~ reorder(year_period, value_sum)),
                showlegend = F
                )  %>% config(responsive = TRUE)
            
            
            subplot(CT, BH, titleX = TRUE, shareY = TRUE) %>%
              layout(
                hovermode = "x",
                yaxis = list(title = ylegend), barmode = 'stack',
                title = list(text = paste(pf_local, "-", metric_local), x = 0),
                margin = list(t = 130, r = 180)
                )
          }
          
          if (input$more_plot == 'Yes' && input$more_plots_opt == 'Machine') {
            plotMachine()
          } else if (input$more_plot == 'Yes' && input$more_plots_opt == 'Lab') {
            plotLab()
          } else if (input$more_plot == 'Yes' && input$more_plots_opt == 'Project') {
            # Only render for the first selected metric
            if (metric_local == input$metrics[1]) {
              plotProject()
            } else {
              return(NULL)
            }
          } else {
            req(input$site)
            if (input$site == "Merge") {
              plotMerge()
            } else if (input$site == "Split") {
              plotSplit()
            }
          }
        })
      })
    }
  }
  #-------- small delay to ensure rendering is stable --------
  shinyjs::delay(500, plots_ready(TRUE))  
  
})

