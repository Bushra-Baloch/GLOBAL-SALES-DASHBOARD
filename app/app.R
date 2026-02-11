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
  
  # Add smooth fade animation
  header = tags$head(
    tags$style(HTML("
      .well {
        animation: fadeIn 0.8s ease-in-out;
      }
      @keyframes fadeIn {
        from { opacity: 0; transform: translateY(10px); }
        to { opacity: 1; transform: translateY(0); }
      }
    "))
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
        width = 4,
        wellPanel(
          h4("Total Sales"),
          h2(textOutput("total_sales_kpi"))
        )
      ),
      
      column(
        width = 4,
        wellPanel(
          h4("Total Profit"),
          h2(textOutput("total_profit_kpi"))
        )
      ),
      
      column(
        width = 4,
        wellPanel(
          h4("Profit Margin"),
          h2(textOutput("profit_margin_kpi"))
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
  ),
  
  # -------------------------
  # TAB 4 — Top Products
  # -------------------------
  
  tabPanel(
    "Top Products",
    
    br(),
    
    fluidRow(
      column(
        width = 12,
        h4("Top 10 Products by Sales"),
        plotlyOutput("top_products_plot")
      )
    )
  )
)

# -------------------------
# Server
# -------------------------

server <- function(input, output) {
  
  # Multi-dimensional filtering
  filtered_data <- reactive({
    
    data <- sales_data
    
    if (input$region_filter != "All") {
      data <- data |> filter(region == input$region_filter)
    }
    
    if (input$year_filter != "All") {
      data <- data |> filter(order_year == input$year_filter)
    }
    
    data
  })
  
  # KPIs
  output$total_sales_kpi <- renderText({
    paste0("$", comma(sum(filtered_data()$sales, na.rm = TRUE)))
  })
  
  output$total_profit_kpi <- renderText({
    paste0("$", comma(sum(filtered_data()$profit, na.rm = TRUE)))
  })
  
  output$profit_margin_kpi <- renderText({
    
    total_sales  <- sum(filtered_data()$sales, na.rm = TRUE)
    total_profit <- sum(filtered_data()$profit, na.rm = TRUE)
    
    margin <- ifelse(total_sales == 0, 0,
                     (total_profit / total_sales) * 100)
    
    paste0(round(margin, 2), "%")
  })
  
  # Category Plot
  output$category_plot <- renderPlotly({
    
    df <- filtered_data() |>
      group_by(category) |>
      summarise(total_sales = sum(sales), .groups = "drop")
    
    p <- ggplot(df,
                aes(x = reorder(category, total_sales),
                    y = total_sales,
                    text = paste(category,
                                 "<br>Sales:", comma(total_sales)))) +
      geom_col(fill = "#2E86C1") +
      coord_flip() +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  # Trend Plot
  output$trend_plot <- renderPlotly({
    
    df <- filtered_data() |>
      group_by(order_year) |>
      summarise(total_sales = sum(sales), .groups = "drop")
    
    p <- ggplot(df,
                aes(order_year, total_sales,
                    text = paste("Year:", order_year,
                                 "<br>Sales:", comma(total_sales)))) +
      geom_line(color = "#00BFC4", linewidth = 1.2) +
      geom_point(color = "#00BFC4", size = 3) +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  # Region Comparison
  output$region_plot <- renderPlotly({
    
    df <- sales_data |>
      group_by(region) |>
      summarise(total_sales = sum(sales), .groups = "drop")
    
    p <- ggplot(df,
                aes(reorder(region, total_sales),
                    total_sales,
                    text = paste(region,
                                 "<br>Sales:", comma(total_sales)))) +
      geom_col(fill = "#F8766D") +
      coord_flip() +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  # Top 10 Products
  output$top_products_plot <- renderPlotly({
    
    df <- filtered_data() |>
      group_by(product_name) |>
      summarise(total_sales = sum(sales), .groups = "drop") |>
      arrange(desc(total_sales)) |>
      slice_head(n = 10)
    
    p <- ggplot(df,
                aes(reorder(product_name, total_sales),
                    total_sales,
                    text = paste(product_name,
                                 "<br>Sales:", comma(total_sales)))) +
      geom_col(fill = "#F4A261") +
      coord_flip() +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
}

# Run App
shinyApp(ui = ui, server = server)

