# ===============================
# Libraries
# ===============================

library(shiny)
library(dplyr)
library(readr)
library(ggplot2)
library(plotly)
library(bslib)
library(scales)

# ===============================
# Load Data
# ===============================

sales_data <- read_csv(
  "../data/cleaned_global_sales.csv",
  show_col_types = FALSE
)

# ===============================
# UI
# ===============================

ui <- page_sidebar(
  
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#1F2D3D"
  ),
  
  # ---------- Sidebar ----------
  
  sidebar = sidebar(
    width = 260,
    
    h5("Filters", style = "font-weight:600; margin-top:10px;"),
    
    selectInput(
      "region_filter",
      "Region",
      choices = c("All", sort(unique(sales_data$region))),
      selected = "All"
    ),
    
    selectInput(
      "year_filter",
      "Year",
      choices = c("All", sort(unique(sales_data$order_year))),
      selected = "All"
    ),
    
    hr(),
    p("Global Sales Dashboard",
      style = "font-size:13px; color:gray;")
  ),
  
  # ---------- KPI Row ----------
  
  layout_column_wrap(
    width = 1/4,
    
    value_box(
      "Total Sales",
      textOutput("total_sales_kpi"),
      showcase = icon("dollar-sign"),
      theme = "primary"
    ),
    
    value_box(
      "Total Profit",
      textOutput("total_profit_kpi"),
      showcase = icon("chart-line"),
      theme = "success"
    ),
    
    value_box(
      "Profit Margin",
      textOutput("profit_margin_kpi"),
      showcase = icon("percent"),
      theme = "warning"
    ),
    
    value_box(
      "YoY Growth",
      uiOutput("yoy_growth_kpi"),
      showcase = icon("arrow-trend-up"),
      theme = "info"
    )
  ),
  
  # ---------- Charts ----------
  
  card(
    card_header("Sales by Category"),
    card_body(plotlyOutput("category_plot", height = "400px"))
  ),
  
  card(
    card_header("Sales Trend"),
    card_body(plotlyOutput("trend_plot", height = "400px"))
  ),
  
  card(
    card_header("Sales by Region"),
    card_body(plotlyOutput("region_plot", height = "400px"))
  ),
  
  card(
    card_header("Top 10 Products"),
    card_body(plotlyOutput("top_products_plot", height = "400px"))
  )
)

# ===============================
# Server
# ===============================

server <- function(input, output) {
  
  # -------- Reactive Filtering --------
  
  filtered_data <- reactive({
    data <- sales_data
    
    if (input$region_filter != "All") {
      data <- data |> filter(region == input$region_filter)
    }
    
    if (input$year_filter != "All") {
      data <- data |> filter(order_year == as.numeric(input$year_filter))
    }
    
    data
  })
  
  # -------- KPIs --------
  
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
  
  # -------- YoY Growth --------
  
  output$yoy_growth_kpi <- renderUI({
    
    if (input$year_filter == "All") {
      return(span("Select Year", style = "color:gray;"))
    }
    
    selected_year <- as.numeric(input$year_filter)
    previous_year <- selected_year - 1
    
    current_sales <- sales_data |>
      filter(order_year == selected_year) |>
      summarise(total = sum(sales)) |>
      pull(total)
    
    previous_sales <- sales_data |>
      filter(order_year == previous_year) |>
      summarise(total = sum(sales)) |>
      pull(total)
    
    if (is.na(previous_sales) || previous_sales == 0) {
      return(span("N/A", style = "color:gray;"))
    }
    
    growth <- ((current_sales - previous_sales) / previous_sales) * 100
    
    arrow <- ifelse(growth >= 0, "▲", "▼")
    color <- ifelse(growth >= 0, "#18BC9C", "#E74C3C")
    
    span(
      paste0(arrow, " ", round(growth, 2), "%"),
      style = paste0("color:", color, "; font-weight:600;")
    )
  })
  
  # -------- Category Plot --------
  
  output$category_plot <- renderPlotly({
    
    df <- filtered_data() |>
      group_by(category) |>
      summarise(total_sales = sum(sales), .groups = "drop") |>
      arrange(desc(total_sales))
    
    p <- ggplot(df,
                aes(reorder(category, total_sales),
                    total_sales)) +
      geom_col(fill = "#2C3E50", width = 0.6) +
      coord_flip() +
      theme_minimal() +
      theme(panel.grid = element_blank())
    
    ggplotly(p)
  })
  
  # -------- Trend Plot --------
  
  output$trend_plot <- renderPlotly({
    
    df <- filtered_data() |>
      group_by(order_year) |>
      summarise(total_sales = sum(sales), .groups = "drop")
    
    p <- ggplot(df,
                aes(order_year, total_sales)) +
      geom_line(color = "#0072B2", linewidth = 1.2) +
      geom_point(color = "#0072B2", size = 3) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # -------- Region Plot --------
  
  output$region_plot <- renderPlotly({
    
    df <- sales_data |>
      group_by(region) |>
      summarise(total_sales = sum(sales), .groups = "drop")
    
    p <- ggplot(df,
                aes(reorder(region, total_sales),
                    total_sales)) +
      geom_col(fill = "#E74C3C") +
      coord_flip() +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # -------- Top Products --------
  
  output$top_products_plot <- renderPlotly({
    
    df <- filtered_data() |>
      group_by(product_name) |>
      summarise(total_sales = sum(sales), .groups = "drop") |>
      arrange(desc(total_sales)) |>
      slice_head(n = 10)
    
    p <- ggplot(df,
                aes(reorder(product_name, total_sales),
                    total_sales)) +
      geom_col(fill = "#F4A261") +
      coord_flip() +
      theme_minimal()
    
    ggplotly(p)
  })
}

shinyApp(ui = ui, server = server)
