# Load libraries
library(dplyr)
library(readr)

# Load cleaned dataset
sales_data <- read_csv("data/cleaned_global_sales.csv")

# -----------------------------
# KPI: Total Sales & Profit
# -----------------------------

total_sales <- sum(sales_data$sales, na.rm = TRUE)
total_profit <- sum(sales_data$profit, na.rm = TRUE)

total_sales
total_profit

# -----------------------------
# Sales by Region
# -----------------------------

sales_by_region <- sales_data |>
  group_by(region) |>
  summarise(
    total_sales = sum(sales, na.rm = TRUE),
    total_profit = sum(profit, na.rm = TRUE)
  ) |>
  arrange(desc(total_sales))

sales_by_region

# -----------------------------
# Sales by Category
# -----------------------------

sales_by_category <- sales_data |>
  group_by(category) |>
  summarise(
    total_sales = sum(sales, na.rm = TRUE),
    total_profit = sum(profit, na.rm = TRUE)
  ) |>
  arrange(desc(total_sales))

sales_by_category

# -----------------------------
# Sales Trend by Year
# -----------------------------

sales_by_year <- sales_data |>
  group_by(order_year) |>
  summarise(
    total_sales = sum(sales, na.rm = TRUE),
    total_profit = sum(profit, na.rm = TRUE)
  ) |>
  arrange(order_year)

sales_by_year
