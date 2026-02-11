# Clean column names (make lowercase and replace spaces with underscores)

colnames(sales_data) <- colnames(sales_data) |>
  tolower() |>
  gsub(" ", "_", x = _)

# Check updated names
colnames(sales_data)

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




