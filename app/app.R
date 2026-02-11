# -------------------------
# Libraries
# -------------------------

library(shiny)
library(dplyr)
library(readr)
library(ggplot2)
library(plotly)
library(bslib)
library(scales)

# -------------------------
# Load Data
# -------------------------

sales_data <- read_csv("../data/cleaned_global_sales.csv")

# -------------------------
# UI
# -------------------------

ui <- navbarPage(
  
  title = "🌍 Global Sales Dashboard",
  
  theme = bs_theme(
    version = 5,
    bootswatch = "darkly"
  ),
  
  # -------------------------
  # TAB 1 — Overview
  # -------------------------
  
  tabPanel(
    "Overview",
    
    br(),
    
    fluidRow(
      column(
        width = 4,
        selectInput(
          "region_filter",
          "Select Region:",
          choices = c("All", sort(unique(sales_data$region))),
          selected = "All"
        )
      ),
      
      column(
        width = 4,
        selectInput(
          "year_filter",
          "Select Year:",
          choices = c("All", sort(unique(sales_data$order_year))),
          selected = "All"
        )
      )
    ),
    
    br(),
    
    fluidRow(
      column(
        width = 6,
        wellPanel(
          h4("Total Sales"),
          h2(textOutput("total_sales_kpi"))
        )
      ),
      
      column(
        width = 6,
        wellPanel(
          h4("Total Profit"),
          h2(textOutput("total_profit_kpi"))
        )
      )
    ),
    
    br(),
    
    fluidRow(
      column(
        width = 12,
        h4("Sales by Category"),
        plotlyOutput("category_plot")
      )
    )
  ),
  
  # -------------------------
  # TAB 2 — Trends
  # -------------------------
  
  tabPanel(
    "Trends",
    
    br(),
    
    fluidRow(
      column(
        width = 12,
        h4("Sales Trend Over Time"),
        plotlyOutput("trend_plot")
      )
    )
  ),
  
  # -------------------------
  # TAB 3 — Region Comparison
  # -------------------------
  
  tabPanel(
    "Region Comparison",
    
    br(),
    
    fluidRow(
      column(
        width = 12,
        h4("Sales by Region"),
        plotlyOutput("region_plot")
      )
    )
  )
)

# -------------------------
# Server
# -------------------------

server <- function(input, output) {
  
  # -------------------------
  # Multi-Dimensional Filtering
  # -------------------------
  
  filtered_data <- reactive({
    
    data <- sales_data
    
    # Region filter
    if (input$region_filter != "All") {
      data <- data |> filter(region == input$region_filter)
    }
    
    # Year filter
    if (input$year_filter != "All") {
      data <- data |> filter(order_year == input$year_filter)
    }
    
    data
  })
  
  # -------------------------
  # KPIs
  # -------------------------
  
  output$total_sales_kpi <- renderText({
    total_sales <- sum(filtered_data()$sales, na.rm = TRUE)
    paste0("$", comma(round(total_sales, 2)))
  })
  
  output$total_profit_kpi <- renderText({
    total_profit <- sum(filtered_data()$profit, na.rm = TRUE)
    paste0("$", comma(round(total_profit, 2)))
  })
  
  # -------------------------
  # Category Chart
  # -------------------------
  
  output$category_plot <- renderPlotly({
    
    category_summary <- filtered_data() |>
      group_by(category) |>
      summarise(total_sales = sum(sales, na.rm = TRUE),
                .groups = "drop")
    
    p <- ggplot(category_summary,
                aes(x = reorder(category, total_sales),
                    y = total_sales,
                    text = paste("Category:", category,
                                 "<br>Sales:", comma(total_sales)))) +
      geom_col(fill = "#2E86C1") +
      coord_flip() +
      labs(x = NULL, y = "Total Sales") +
      theme_minimal(base_size = 14)
    
    ggplotly(p, tooltip = "text")
  })
  
  # -------------------------
  # Trend Chart
  # -------------------------
  
  output$trend_plot <- renderPlotly({
    
    trend_data <- filtered_data() |>
      group_by(order_year) |>
      summarise(total_sales = sum(sales, na.rm = TRUE),
                .groups = "drop")
    
    p <- ggplot(trend_data,
                aes(x = order_year,
                    y = total_sales,
                    text = paste("Year:", order_year,
                                 "<br>Sales:", comma(total_sales)))) +
      geom_line(color = "#00BFC4", linewidth = 1.2) +
      geom_point(color = "#00BFC4", size = 3) +
      labs(x = "Year", y = "Total Sales") +
      theme_minimal(base_size = 14)
    
    ggplotly(p, tooltip = "text")
  })
  
  # -------------------------
  # Region Comparison (Full Dataset)
  # -------------------------
  
  output$region_plot <- renderPlotly({
    
    region_summary <- sales_data |>
      group_by(region) |>
      summarise(total_sales = sum(sales, na.rm = TRUE),
                .groups = "drop")
    
    p <- ggplot(region_summary,
                aes(x = reorder(region, total_sales),
                    y = total_sales,
                    text = paste("Region:", region,
                                 "<br>Sales:", comma(total_sales)))) +
      geom_col(fill = "#F8766D") +
      coord_flip() +
      labs(x = NULL, y = "Total Sales") +
      theme_minimal(base_size = 14)
    
    ggplotly(p, tooltip = "text")
  })
  
}

# -------------------------
# Run App
# -------------------------

shinyApp(ui = ui, server = server)
