
# -------------------------
# Libraries
# -------------------------

library(shiny)
library(dplyr)
library(readr)
library(ggplot2)
library(plotly)

# -------------------------
# Load Data
# -------------------------

sales_data <- read_csv("../data/cleaned_global_sales.csv")

# -------------------------
# UI
# -------------------------
ui <- navbarPage(
  
  title = "🌍 Global Sales Dashboard",
  
  theme = bslib::bs_theme(
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
  
  # Reactive filtered data
  filtered_data <- reactive({
    
    if (input$region_filter == "All") {
      sales_data
    } else {
      sales_data |> 
        filter(region == input$region_filter)
    }
    
  })
  
  # -------------------------
  # KPIs
  # -------------------------
  
  output$total_sales_kpi <- renderText({
    
    total_sales <- sum(filtered_data()$sales, na.rm = TRUE)
    
    paste0("$", format(round(total_sales, 2), big.mark = ","))
    
  })
  
  output$total_profit_kpi <- renderText({
    
    total_profit <- sum(filtered_data()$profit, na.rm = TRUE)
    
    paste0("$", format(round(total_profit, 2), big.mark = ","))
    
  })
  
  # -------------------------
  # Category Plot
  # -------------------------
  
  output$category_plot <- renderPlotly({
    
    category_summary <- filtered_data() |>
      group_by(category) |>
      summarise(total_sales = sum(sales, na.rm = TRUE))
    
    p <- ggplot(category_summary,
                aes(x = reorder(category, total_sales),
                    y = total_sales,
                    text = paste("Category:", category,
                                 "<br>Sales:", scales::comma(total_sales)))) +
      geom_col(fill = "#2E86C1") +
      coord_flip() +
      labs(
        x = "",
        y = "Total Sales"
      ) +
      theme_minimal(base_size = 14)
    
    ggplotly(p, tooltip = "text")
    
  })
  
}

# -------------------------
# Run App
# -------------------------

shinyApp(ui = ui, server = server)
