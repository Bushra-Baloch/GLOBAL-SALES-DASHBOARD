# Load required libraries
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)


# Load dataset
sales_data <- read_csv("data/global_sales.csv")

# View first few rows
head(sales_data)
# Check structure of dataset
str(sales_data)

# Check column names
colnames(sales_data)

# Summary statistics
summary(sales_data)
# Clean column names (make lowercase and replace spaces with underscores)

colnames(sales_data) <- colnames(sales_data) |>
  tolower() |>
  gsub(" ", "_", x = _)

# Check updated names
colnames(sales_data)
sales_data$order_date <- as.Date(sales_data$order_date)
sales_data$ship_date  <- as.Date(sales_data$ship_date)

sales_data$order_year  <- as.numeric(format(sales_data$order_date, "%Y"))
sales_data$order_month <- as.numeric(format(sales_data$order_date, "%m"))


# Convert to proper Date format
sales_data$order_date <- as.Date(sales_data$order_date, format = "%m/%d/%Y")
sales_data$ship_date  <- as.Date(sales_data$ship_date, format = "%m/%d/%Y")
# Check missing values in each column
colSums(is.na(sales_data))
str(sales_data)



# Extract Year and Month
sales_data <- sales_data |>
  mutate(
    order_year  = year(order_date),
    order_month = month(order_date, label = TRUE)
  )


str(sales_data)
summary(sales_data)

# Save cleaned dataset
write_csv(sales_data, "data/cleaned_global_sales.csv")




