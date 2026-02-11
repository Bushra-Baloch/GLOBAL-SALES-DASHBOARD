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

