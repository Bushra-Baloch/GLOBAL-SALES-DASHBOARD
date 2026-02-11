library(shiny)
# Load cleaned dataset
sales_data <- read_csv("../data/cleaned_global_sales.csv")


# -------------------------
# UI (User Interface)
# -------------------------

ui <- fluidPage(
  
  titlePanel("Global Sales Dashboard"),
  
  br(),
  
  # Region Filter
  selectInput(
    inputId = "region_filter",
    label = "Select Region:",
    choices = c("All", unique(sales_data$region)),
    selected = "All"
  ),
  
  br(),
  
  fluidRow(
    
    column(
      width = 6,
      h3("Total Global Sales"),
      verbatimTextOutput("total_sales_kpi")
    ),
    
    column(
      width = 6,
      h3("Total Global Profit"),
      verbatimTextOutput("total_profit_kpi")
    )
    
  )
)


# -------------------------
# Server (Logic)
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
  
  # Calculate KPIs from filtered data
  output$total_sales_kpi <- renderText({
    
    total_sales <- sum(filtered_data()$sales, na.rm = TRUE)
    
    paste("$", format(round(total_sales, 2), big.mark = ","), sep = "")
    
  })
  
  output$total_profit_kpi <- renderText({
    
    total_profit <- sum(filtered_data()$profit, na.rm = TRUE)
    
    paste("$", format(round(total_profit, 2), big.mark = ","), sep = "")
    
  })
  
}




# -------------------------
# Run the App
# -------------------------

shinyApp(ui = ui, server = server)
