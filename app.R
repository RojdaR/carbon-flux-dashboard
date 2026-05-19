# =============================================================================
# app.R — Carbon Flux Interactive Dashboard
# Author: Rojda Guler Aslan Sungur, Ph.D.
# =============================================================================

library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(plotly)
library(DT)
library(scales)

# ── SAMPLE DATA (replace with your actual data path) ─────────────────────────

# Generate synthetic demo data if no real data file is present
generate_demo_data <- function(n_years = 3, sites = c("Iowa", "Illinois", "Florida")) {
  start_date <- as.POSIXct("2021-01-01")
  timestamps <- seq(start_date, by = "30 min", length.out = n_years * 365 * 48)

  map_dfr(sites, function(site) {
    set.seed(which(sites == site))
    n <- length(timestamps)
    doy  <- yday(timestamps)
    hour <- hour(timestamps)

    # Synthetic seasonal + diurnal patterns
    season_amp <- ifelse(site == "Florida", 3, 8)
    Rg    <- pmax(0, 800 * sin(pi * hour / 12) * (1 + 0.3 * sin(2 * pi * doy / 365)) + rnorm(n, 0, 30))
    Tair  <- 15 + season_amp * sin(2 * pi * (doy - 80) / 365) + 5 * sin(pi * hour / 12) + rnorm(n, 0, 1.5)
    NEE   <- -(Rg * 0.015) * (1 + 0.2 * sin(2*pi*(doy-120)/365)) + 1.5 + rnorm(n, 0, 0.5)
    LE    <- Rg * 0.4 + rnorm(n, 0, 15)
    H     <- Rg * 0.25 + rnorm(n, 0, 10)

    tibble(
      timestamp = timestamps,
      site      = site,
      NEE       = round(NEE, 3),
      LE        = round(pmax(0, LE), 1),
      H         = round(H, 1),
      Tair      = round(Tair, 1),
      Rg        = round(pmax(0, Rg), 1)
    )
  })
}

# Load data (use your own CSV if available)
flux_data <- if (file.exists("data/multisite_flux.csv")) {
  read_csv("data/multisite_flux.csv")
} else {
  message("Using synthetic demo data. Replace with real data in data/multisite_flux.csv")
  generate_demo_data()
}

flux_data <- flux_data %>%
  mutate(
    date   = as.Date(timestamp),
    month  = floor_date(timestamp, "month"),
    year   = year(timestamp),
    season = case_when(
      month(timestamp) %in% c(12, 1, 2)  ~ "Winter",
      month(timestamp) %in% c(3, 4, 5)   ~ "Spring",
      month(timestamp) %in% c(6, 7, 8)   ~ "Summer",
      TRUE                                ~ "Autumn"
    )
  )

site_colors <- c("Iowa"     = "#1B7837",
                 "Illinois" = "#762A83",
                 "Florida"  = "#E08214")

variables   <- c("NEE"  = "Net Ecosystem Exchange (NEE)",
                 "LE"   = "Latent Energy (LE)",
                 "H"    = "Sensible Heat (H)",
                 "Tair" = "Air Temperature",
                 "Rg"   = "Global Radiation")

y_labels <- c(
  NEE  = "NEE (μmol m⁻² s⁻¹)",
  LE   = "LE (W m⁻²)",
  H    = "H (W m⁻²)",
  Tair = "Tₐᴵʳ (°C)",
  Rg   = "Rg (W m⁻²)"
)

# ── UI ────────────────────────────────────────────────────────────────────────

ui <- dashboardPage(
  skin = "green",

  dashboardHeader(
    title = "Carbon Flux Explorer"
  ),

  dashboardSidebar(
    sidebarMenu(
      menuItem("Time Series",   tabName = "timeseries",   icon = icon("chart-line")),
      menuItem("Seasonal View", tabName = "seasonal",     icon = icon("leaf")),
      menuItem("Site Summary",  tabName = "summary",      icon = icon("table"))
    ),

    br(),
    h5("  Filters", style = "color: #ccc; padding-left: 15px;"),

    checkboxGroupInput("sites", "Sites:",
                       choices  = unique(flux_data$site),
                       selected = unique(flux_data$site)),

    selectInput("variable", "Variable:",
                choices  = variables,
                selected = "NEE"),

    sliderInput("date_range", "Date Range:",
                min   = min(flux_data$date),
                max   = max(flux_data$date),
                value = c(min(flux_data$date), max(flux_data$date)),
                timeFormat = "%b %Y"),

    selectInput("aggregation", "Aggregation:",
                choices = c("Daily" = "day", "Monthly" = "month"),
                selected = "day")
  ),

  dashboardBody(
    tags$head(tags$style(HTML(
      ".content-wrapper { background-color: #f9f9f9; }
       .box { border-radius: 6px; }
       .info-box { border-radius: 6px; }"
    ))),

    tabItems(

      # ── TAB 1: TIME SERIES ────────────────────────────────────────────────
      tabItem(tabName = "timeseries",

        fluidRow(
          valueBoxOutput("box_sites",    width = 4),
          valueBoxOutput("box_records",  width = 4),
          valueBoxOutput("box_coverage", width = 4)
        ),

        fluidRow(
          box(
            title = "Multi-Site Time Series", width = 12,
            status = "success", solidHeader = TRUE,
            plotlyOutput("plot_timeseries", height = "380px"),
            downloadButton("dl_ts", "Download Plot", class = "btn-sm btn-default",
                           style = "margin-top: 8px;")
          )
        ),

        fluidRow(
          box(
            title = "Diurnal Pattern (Mean by Hour)", width = 6,
            status = "success", solidHeader = FALSE,
            plotlyOutput("plot_diurnal", height = "300px")
          ),
          box(
            title = "Distribution by Site", width = 6,
            status = "success", solidHeader = FALSE,
            plotlyOutput("plot_violin", height = "300px")
          )
        )
      ),

      # ── TAB 2: SEASONAL VIEW ──────────────────────────────────────────────
      tabItem(tabName = "seasonal",

        fluidRow(
          box(
            title = "Seasonal Means by Site", width = 12,
            status = "primary", solidHeader = TRUE,
            plotlyOutput("plot_seasonal", height = "400px")
          )
        ),

        fluidRow(
          box(
            title = "Monthly Heatmap", width = 12,
            status = "primary", solidHeader = FALSE,
            plotlyOutput("plot_heatmap", height = "350px")
          )
        )
      ),

      # ── TAB 3: SUMMARY TABLE ──────────────────────────────────────────────
      tabItem(tabName = "summary",

        fluidRow(
          box(
            title = "Annual Summary by Site", width = 12,
            status = "warning", solidHeader = TRUE,
            DTOutput("tbl_summary")
          )
        )
      )
    )
  )
)

# ── SERVER ────────────────────────────────────────────────────────────────────

server <- function(input, output, session) {

  # Reactive filtered data
  df_filtered <- reactive({
    req(input$sites, input$variable)
    flux_data %>%
      filter(
        site %in% input$sites,
        date >= input$date_range[1],
        date <= input$date_range[2]
      )
  })

  df_agg <- reactive({
    df_filtered() %>%
      mutate(period = floor_date(timestamp, input$aggregation)) %>%
      group_by(period, site) %>%
      summarise(value = mean(.data[[input$variable]], na.rm = TRUE), .groups = "drop")
  })

  # Value boxes
  output$box_sites <- renderValueBox({
    valueBox(length(input$sites), "Sites Selected", icon = icon("map-marker"),
             color = "green")
  })

  output$box_records <- renderValueBox({
    valueBox(format(nrow(df_filtered()), big.mark = ","),
             "Records", icon = icon("database"), color = "blue")
  })

  output$box_coverage <- renderValueBox({
    pct <- df_filtered() %>%
      summarise(cov = round(100 * mean(!is.na(.data[[input$variable]])), 1)) %>%
      pull(cov)
    valueBox(paste0(pct, "%"), "Data Coverage", icon = icon("check-circle"),
             color = if (pct > 90) "green" else if (pct > 70) "yellow" else "red")
  })

  # Time series plot
  output$plot_timeseries <- renderPlotly({
    p <- ggplot(df_agg(), aes(x = period, y = value, color = site)) +
      geom_line(alpha = 0.8, linewidth = 0.7) +
      scale_color_manual(values = site_colors) +
      labs(x = NULL, y = y_labels[input$variable], color = "Site") +
      theme_minimal(base_size = 11)
    ggplotly(p) %>% layout(hovermode = "x unified")
  })

  # Diurnal plot
  output$plot_diurnal <- renderPlotly({
    df_d <- df_filtered() %>%
      mutate(hour = hour(timestamp)) %>%
      group_by(site, hour) %>%
      summarise(mean_val = mean(.data[[input$variable]], na.rm = TRUE), .groups = "drop")

    p <- ggplot(df_d, aes(x = hour, y = mean_val, color = site)) +
      geom_line(linewidth = 1.1) +
      scale_color_manual(values = site_colors) +
      labs(x = "Hour of Day", y = y_labels[input$variable], color = "Site") +
      theme_minimal(base_size = 11)
    ggplotly(p)
  })

  # Violin plot
  output$plot_violin <- renderPlotly({
    df_v <- df_filtered() %>%
      filter(!is.na(.data[[input$variable]]))

    p <- ggplot(df_v, aes(x = site, y = .data[[input$variable]], fill = site)) +
      geom_violin(alpha = 0.7, trim = TRUE) +
      geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +
      scale_fill_manual(values = site_colors) +
      labs(x = NULL, y = y_labels[input$variable]) +
      theme_minimal(base_size = 11) +
      theme(legend.position = "none")
    ggplotly(p)
  })

  # Seasonal plot
  output$plot_seasonal <- renderPlotly({
    df_s <- df_filtered() %>%
      mutate(season = factor(season, levels = c("Spring", "Summer", "Autumn", "Winter"))) %>%
      group_by(site, season) %>%
      summarise(mean_val = mean(.data[[input$variable]], na.rm = TRUE),
                se_val   = sd(.data[[input$variable]], na.rm = TRUE) / sqrt(n()),
                .groups  = "drop")

    p <- ggplot(df_s, aes(x = season, y = mean_val, fill = site)) +
      geom_col(position = position_dodge(0.8), width = 0.7, alpha = 0.85) +
      geom_errorbar(aes(ymin = mean_val - se_val, ymax = mean_val + se_val),
                    position = position_dodge(0.8), width = 0.25) +
      scale_fill_manual(values = site_colors) +
      labs(x = NULL, y = y_labels[input$variable], fill = "Site") +
      theme_minimal(base_size = 11)
    ggplotly(p)
  })

  # Heatmap
  output$plot_heatmap <- renderPlotly({
    df_h <- df_filtered() %>%
      mutate(
        yr  = year(timestamp),
        mon = month(timestamp, label = TRUE)
      ) %>%
      group_by(site, yr, mon) %>%
      summarise(mean_val = mean(.data[[input$variable]], na.rm = TRUE), .groups = "drop") %>%
      filter(!is.na(mean_val)) %>%
      group_by(yr, mon) %>%
      summarise(mean_val = mean(mean_val), .groups = "drop")

    p <- ggplot(df_h, aes(x = mon, y = factor(yr), fill = mean_val)) +
      geom_tile(color = "white", linewidth = 0.5) +
      scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#D6604D",
                           midpoint = 0, name = y_labels[input$variable]) +
      labs(x = NULL, y = "Year") +
      theme_minimal(base_size = 11)
    ggplotly(p)
  })

  # Summary table
  output$tbl_summary <- renderDT({
    df_filtered() %>%
      group_by(Site = site, Year = year(timestamp)) %>%
      summarise(
        `Mean NEE`   = round(mean(NEE,  na.rm = TRUE), 3),
        `Mean LE`    = round(mean(LE,   na.rm = TRUE), 1),
        `Mean H`     = round(mean(H,    na.rm = TRUE), 1),
        `Mean Tair`  = round(mean(Tair, na.rm = TRUE), 1),
        `Records`    = n(),
        `Coverage %` = round(100 * mean(!is.na(NEE)), 1),
        .groups      = "drop"
      ) %>%
      datatable(
        options = list(pageLength = 15, scrollX = TRUE),
        rownames = FALSE
      ) %>%
      formatStyle("Mean NEE",
                  background = styleColorBar(range(.$`Mean NEE`, na.rm = TRUE), "#90EE90"))
  })
}

# ── LAUNCH ────────────────────────────────────────────────────────────────────

shinyApp(ui = ui, server = server)
