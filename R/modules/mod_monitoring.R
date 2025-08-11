#' Monitoring Dashboard Module for TDMx
#' 
#' Real-time monitoring dashboard for system health and errors
#' 
#' @description
#' Shiny module providing:
#' - Real-time system health status
#' - Error tracking and trends
#' - Performance metrics
#' - Alert management

library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(dplyr)

#' Monitoring Dashboard UI
#' 
#' @param id Module namespace ID
#' @export
mod_monitoring_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Custom CSS for status indicators
    tags$style(HTML("
      .status-healthy { color: #28a745; font-weight: bold; }
      .status-warning { color: #ffc107; font-weight: bold; }
      .status-degraded { color: #fd7e14; font-weight: bold; }
      .status-critical { color: #dc3545; font-weight: bold; }
      .status-unknown { color: #6c757d; font-weight: bold; }
      
      .health-card {
        border-radius: 8px;
        padding: 15px;
        margin-bottom: 15px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }
      
      .health-card.healthy { border-left: 4px solid #28a745; }
      .health-card.warning { border-left: 4px solid #ffc107; }
      .health-card.degraded { border-left: 4px solid #fd7e14; }
      .health-card.critical { border-left: 4px solid #dc3545; }
      
      .metric-box {
        text-align: center;
        padding: 10px;
        background: #f8f9fa;
        border-radius: 4px;
        margin: 5px;
      }
      
      .metric-value {
        font-size: 24px;
        font-weight: bold;
        color: #333;
      }
      
      .metric-label {
        font-size: 12px;
        color: #666;
        text-transform: uppercase;
      }
    ")),
    
    fluidRow(
      # Overall Status Box
      column(12,
        div(class = "health-card",
          id = ns("overall_status_card"),
          fluidRow(
            column(3,
              h3("System Status"),
              uiOutput(ns("overall_status"))
            ),
            column(3,
              div(class = "metric-box",
                div(class = "metric-value", textOutput(ns("uptime"))),
                div(class = "metric-label", "Uptime")
              )
            ),
            column(3,
              div(class = "metric-box",
                div(class = "metric-value", textOutput(ns("error_rate"))),
                div(class = "metric-label", "Error Rate")
              )
            ),
            column(3,
              div(class = "metric-box",
                div(class = "metric-value", textOutput(ns("last_check"))),
                div(class = "metric-label", "Last Check")
              )
            )
          )
        )
      )
    ),
    
    fluidRow(
      # Health Checks Panel
      column(6,
        box(
          title = "Component Health",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          height = "500px",
          
          actionButton(ns("refresh_health"), "Refresh", 
                      icon = icon("sync"),
                      class = "btn-sm pull-right"),
          
          br(), br(),
          
          DT::dataTableOutput(ns("health_table"))
        )
      ),
      
      # Error Trends Panel
      column(6,
        box(
          title = "Error Trends (24h)",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          height = "500px",
          
          plotlyOutput(ns("error_trends_plot"), height = "450px")
        )
      )
    ),
    
    fluidRow(
      # Recent Errors Panel
      column(12,
        box(
          title = "Recent Errors",
          status = "warning",
          solidHeader = TRUE,
          width = 12,
          collapsible = TRUE,
          collapsed = FALSE,
          
          div(style = "margin-bottom: 10px;",
            selectInput(ns("error_severity_filter"), 
                       "Filter by Severity:",
                       choices = c("All" = "all", 
                                  "Critical" = "CRITICAL",
                                  "Error" = "ERROR", 
                                  "Warning" = "WARNING",
                                  "Info" = "INFO"),
                       selected = "all",
                       width = "200px",
                       inline = TRUE),
            
            actionButton(ns("clear_errors"), "Clear Old Errors",
                        icon = icon("trash"),
                        class = "btn-warning btn-sm pull-right")
          ),
          
          DT::dataTableOutput(ns("errors_table"))
        )
      )
    ),
    
    fluidRow(
      # Performance Metrics
      column(6,
        box(
          title = "Performance Metrics",
          status = "info",
          solidHeader = TRUE,
          width = 12,
          
          plotlyOutput(ns("performance_plot"), height = "300px")
        )
      ),
      
      # Alert History
      column(6,
        box(
          title = "Alert History",
          status = "danger",
          solidHeader = TRUE,
          width = 12,
          
          DT::dataTableOutput(ns("alerts_table"))
        )
      )
    ),
    
    # Auto-refresh
    div(style = "margin: 20px;",
      checkboxInput(ns("auto_refresh"), "Auto-refresh (30s)", value = TRUE)
    )
  )
}

#' Monitoring Dashboard Server
#' 
#' @param id Module namespace ID
#' @param session Shiny session
#' @export
mod_monitoring_server <- function(id, session = shiny::getDefaultReactiveDomain()) {
  moduleServer(id, function(input, output, session) {
    
    # Source required modules
    source("R/error_monitor.R", local = TRUE)
    source("R/health_checks.R", local = TRUE)
    
    # Reactive values
    values <- reactiveValues(
      health_status = NULL,
      error_stats = NULL,
      last_refresh = NULL,
      alerts = list()
    )
    
    # Auto-refresh timer
    refresh_timer <- reactiveTimer(30000)  # 30 seconds
    
    # Refresh data
    refresh_data <- function() {
      # Run health checks
      values$health_status <- run_health_checks(verbose = FALSE)
      
      # Get error statistics
      values$error_stats <- get_error_stats(time_window = hours(24))
      
      # Update last refresh time
      values$last_refresh <- Sys.time()
    }
    
    # Initial load
    observe({
      refresh_data()
    })
    
    # Auto-refresh
    observe({
      refresh_timer()
      if (input$auto_refresh) {
        refresh_data()
      }
    })
    
    # Manual refresh
    observeEvent(input$refresh_health, {
      refresh_data()
      showNotification("Health checks refreshed", type = "success", duration = 2)
    })
    
    # Overall status display
    output$overall_status <- renderUI({
      status <- values$health_status$overall_status %||% "unknown"
      
      status_text <- switch(
        status,
        healthy = "All Systems Operational",
        warning = "Minor Issues Detected",
        degraded = "Degraded Performance",
        critical = "Critical Issues",
        unknown = "Status Unknown"
      )
      
      icon_name <- switch(
        status,
        healthy = "check-circle",
        warning = "exclamation-triangle",
        degraded = "exclamation-circle",
        critical = "times-circle",
        unknown = "question-circle"
      )
      
      tags$div(
        class = paste0("status-", status),
        icon(icon_name),
        status_text
      )
    })
    
    # Update overall status card color
    observe({
      status <- values$health_status$overall_status %||% "unknown"
      
      shinyjs::removeClass(
        id = "overall_status_card",
        class = c("healthy", "warning", "degraded", "critical")
      )
      
      shinyjs::addClass(
        id = "overall_status_card",
        class = status
      )
    })
    
    # Uptime display
    output$uptime <- renderText({
      if (!is.null(values$error_stats$uptime)) {
        hours <- as.numeric(values$error_stats$uptime)
        if (hours < 24) {
          sprintf("%.1fh", hours)
        } else {
          sprintf("%.1fd", hours / 24)
        }
      } else {
        "N/A"
      }
    })
    
    # Error rate display
    output$error_rate <- renderText({
      if (!is.null(values$error_stats$error_rate)) {
        sprintf("%.2f/min", values$error_stats$error_rate)
      } else {
        "0.00/min"
      }
    })
    
    # Last check display
    output$last_check <- renderText({
      if (!is.null(values$last_refresh)) {
        format(values$last_refresh, "%H:%M:%S")
      } else {
        "Never"
      }
    })
    
    # Health status table
    output$health_table <- DT::renderDataTable({
      req(values$health_status)
      
      # Convert checks to data frame
      checks_df <- do.call(rbind, lapply(names(values$health_status$checks), function(name) {
        check <- values$health_status$checks[[name]]
        data.frame(
          Component = check$component,
          Status = check$status,
          Message = check$message,
          stringsAsFactors = FALSE
        )
      }))
      
      # Format status column with colors
      checks_df$Status <- sapply(checks_df$Status, function(s) {
        color <- switch(
          s,
          healthy = "#28a745",
          warning = "#ffc107",
          degraded = "#fd7e14",
          critical = "#dc3545",
          "#6c757d"
        )
        
        icon <- switch(
          s,
          healthy = "✓",
          warning = "⚠",
          degraded = "⚡",
          critical = "✗",
          "?"
        )
        
        sprintf('<span style="color: %s; font-weight: bold;">%s %s</span>', 
                color, icon, toupper(s))
      })
      
      DT::datatable(
        checks_df,
        escape = FALSE,
        options = list(
          pageLength = 10,
          dom = 't',
          ordering = FALSE,
          columnDefs = list(
            list(width = '100px', targets = 0),
            list(width = '120px', targets = 1)
          )
        ),
        rownames = FALSE
      )
    })
    
    # Error trends plot
    output$error_trends_plot <- renderPlotly({
      req(values$error_stats)
      
      # Get error trends from monitor
      trends <- get_error_trends(hours = 24)
      
      if (nrow(trends) == 0) {
        # No errors - show empty plot
        plot_ly() %>%
          layout(
            title = "No errors in the last 24 hours",
            xaxis = list(title = "Time"),
            yaxis = list(title = "Error Count")
          )
      } else {
        # Create stacked area chart
        plot_ly(trends, 
                x = ~hour, 
                y = ~count, 
                color = ~severity,
                type = 'scatter',
                mode = 'lines',
                stackgroup = 'one',
                fillcolor = ~severity,
                colors = c(
                  "CRITICAL" = "#dc3545",
                  "ERROR" = "#fd7e14",
                  "WARNING" = "#ffc107",
                  "INFO" = "#17a2b8"
                )) %>%
          layout(
            title = "",
            xaxis = list(title = ""),
            yaxis = list(title = "Errors per Hour"),
            hovermode = 'x unified',
            showlegend = TRUE,
            legend = list(orientation = "h", y = -0.2)
          )
      }
    })
    
    # Recent errors table
    output$errors_table <- DT::renderDataTable({
      req(values$error_stats)
      
      errors <- values$error_stats$recent_errors
      
      if (length(errors) == 0) {
        return(data.frame(Message = "No recent errors", stringsAsFactors = FALSE))
      }
      
      # Filter by severity if selected
      if (input$error_severity_filter != "all") {
        errors <- Filter(function(e) e$severity == input$error_severity_filter, errors)
      }
      
      if (length(errors) == 0) {
        return(data.frame(Message = "No errors matching filter", stringsAsFactors = FALSE))
      }
      
      # Convert to data frame
      errors_df <- do.call(rbind, lapply(errors, function(e) {
        data.frame(
          Time = format(e$timestamp, "%H:%M:%S"),
          Severity = e$severity,
          Type = e$error_type,
          Module = e$module %||% "Unknown",
          Message = substr(e$message, 1, 100),
          User = e$user %||% "System",
          stringsAsFactors = FALSE
        )
      }))
      
      # Format severity with colors
      errors_df$Severity <- sapply(errors_df$Severity, function(s) {
        color <- switch(
          s,
          "CRITICAL" = "#dc3545",
          "ERROR" = "#fd7e14",
          "WARNING" = "#ffc107",
          "INFO" = "#17a2b8",
          "#6c757d"
        )
        sprintf('<span style="color: %s; font-weight: bold;">%s</span>', color, s)
      })
      
      DT::datatable(
        errors_df,
        escape = FALSE,
        options = list(
          pageLength = 10,
          order = list(list(0, 'desc')),
          columnDefs = list(
            list(width = '80px', targets = 0),
            list(width = '100px', targets = 1)
          )
        ),
        rownames = FALSE
      )
    })
    
    # Performance metrics plot
    output$performance_plot <- renderPlotly({
      # Get health history
      history <- get_health_history(limit = 20)
      
      if (nrow(history) == 0) {
        plot_ly() %>%
          layout(title = "No performance data available")
      } else {
        # Create line plot of response times
        plot_ly(history, 
                x = ~timestamp, 
                y = ~duration_ms,
                type = 'scatter',
                mode = 'lines+markers',
                name = 'Response Time',
                line = list(color = '#007bff')) %>%
          layout(
            title = "",
            xaxis = list(title = ""),
            yaxis = list(title = "Response Time (ms)"),
            hovermode = 'x',
            showlegend = FALSE
          )
      }
    })
    
    # Alerts table
    output$alerts_table <- DT::renderDataTable({
      # This would normally fetch from the error monitor
      # For now, create sample data
      
      alerts_df <- data.frame(
        Time = character(),
        Level = character(),
        Subject = character(),
        stringsAsFactors = FALSE
      )
      
      if (length(values$alerts) > 0) {
        alerts_df <- do.call(rbind, lapply(values$alerts, function(a) {
          data.frame(
            Time = format(a$timestamp, "%Y-%m-%d %H:%M"),
            Level = a$level,
            Subject = a$subject,
            stringsAsFactors = FALSE
          )
        }))
      }
      
      if (nrow(alerts_df) == 0) {
        alerts_df <- data.frame(
          Message = "No alerts in the last 24 hours",
          stringsAsFactors = FALSE
        )
      }
      
      DT::datatable(
        alerts_df,
        options = list(
          pageLength = 5,
          dom = 't',
          ordering = FALSE
        ),
        rownames = FALSE
      )
    })
    
    # Clear old errors
    observeEvent(input$clear_errors, {
      showModal(modalDialog(
        title = "Clear Old Errors",
        "This will remove all error records older than 24 hours. Continue?",
        footer = tagList(
          modalButton("Cancel"),
          actionButton(session$ns("confirm_clear"), "Clear", class = "btn-warning")
        )
      ))
    })
    
    observeEvent(input$confirm_clear, {
      clear_error_history(older_than = hours(24))
      refresh_data()
      removeModal()
      showNotification("Old errors cleared", type = "success")
    })
    
    # Return reactive values for use in other modules
    return(list(
      health_status = reactive(values$health_status),
      error_stats = reactive(values$error_stats)
    ))
  })
}