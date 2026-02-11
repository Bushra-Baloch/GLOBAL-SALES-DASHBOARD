library(plotly)

library(dplyr)
library(readr)
library(ggplot2)

sales_data <- read_csv("data/cleaned_global_sales.csv")

sales_by_region <- sales_data |>
  group_by(region) |>
  summarise(total_sales = sum(sales, na.rm = TRUE))

sales_by_category <- sales_data |>
  group_by(category) |>
  summarise(total_sales = sum(sales, na.rm = TRUE))

sales_by_year <- sales_data |>
  group_by(order_year) |>
  summarise(total_sales = sum(sales, na.rm = TRUE))

region_plot <- ggplot(sales_by_region, 
                      aes(x = reorder(region, total_sales), 
                          y = total_sales)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Total Sales by Region",
    x = "Region",
    y = "Total Sales"
  ) +
  theme_minimal()

region_plot


region_plot <- ggplot(sales_by_region, 
                      aes(x = reorder(region, total_sales), 
                          y = total_sales)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Total Sales by Region",
    x = "Region",
    y = "Total Sales"
  ) +
  theme_minimal()

region_plot

region_plot <- ggplot(sales_by_region, 
                      aes(x = reorder(region, total_sales), 
                          y = total_sales)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Total Sales by Region",
    x = "Region",
    y = "Total Sales"
  ) +
  theme_minimal()

region_plot

ggsave("category_sales.png", 
       plot = category_plot, 
       width = 8, 
       height = 5)

trend_plot <- ggplot(sales_by_year, 
                     aes(x = order_year, 
                         y = total_sales)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Sales Trend by Year",
    x = "Year",
    y = "Total Sales"
  ) +
  theme_minimal()

trend_plot
ggsave("sales_trend.png", 
       plot = trend_plot, 
       width = 8, 
       height = 5)

ggsave("outputs/region_sales.png", plot = region_plot, width = 8, height = 5)

region_interactive <- ggplotly(region_plot)

region_interactive

htmlwidgets::saveWidget(region_interactive, 
                        "outputs/region_sales_interactive.html")

category_interactive <- ggplotly(category_plot)

category_interactive

htmlwidgets::saveWidget(category_interactive, 
                        "outputs/category_sales_interactive.html")
trend_interactive <- ggplotly(trend_plot)

trend_interactive

htmlwidgets::saveWidget(trend_interactive, 
                        "outputs/sales_trend_interactive.html")
