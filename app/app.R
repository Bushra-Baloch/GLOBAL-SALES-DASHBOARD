library(shiny)
# Load cleaned dataset
sales_data <- read_csv("../data/cleaned_global_sales.csv")


# -------------------------
# UI (User Interface)
# -------------------------

ui <- fluidPage(
  
  titlePanel("Global Sales Dashboard"),
  
  br(),
  
  h3("Key Performance Indicator"),
  
  verbatimTextOutput("total_sales_kpi")
)


# -------------------------
# Server (Logic)
# -------------------------

server <- function(input, output) {
  
  # Calculate Total Sales
  total_sales <- sum(sales_data$sales, na.rm = TRUE)
  
  # Send result to UI
  output$total_sales_kpi <- renderText({
    paste("Total Global Sales:", round(total_sales, 2))
  })
  
}


# -------------------------
# Run the App
# -------------------------

shinyApp(ui = ui, server = server)
