library(shiny)
# Load cleaned dataset
sales_data <- read_csv("../data/cleaned_global_sales.csv")


# -------------------------
# UI (User Interface)
# -------------------------

ui <- fluidPage(
  
  titlePanel("Global Sales Dashboard"),
  
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
  
  # Calculate KPIs
  total_sales  <- sum(sales_data$sales, na.rm = TRUE)
  total_profit <- sum(sales_data$profit, na.rm = TRUE)
  
  # Send Sales to UI
  output$total_sales_kpi <- renderText({
    paste("$", format(round(total_sales, 2), big.mark = ","), sep = "")
  })
  
  # Send Profit to UI
  output$total_profit_kpi <- renderText({
    paste("$", format(round(total_profit, 2), big.mark = ","), sep = "")
  })
  
}



# -------------------------
# Run the App
# -------------------------

shinyApp(ui = ui, server = server)
