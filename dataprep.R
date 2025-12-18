# 1. SETUP AND DATA LOADING
library(fpp3)      
library(readr)    

# Load data
calendar <- read_csv("calendar_afcs2025.csv")
prices   <- read_csv("sell_prices_afcs2025.csv")
sales    <- read_csv("sales_train_validation_afcs2025.csv")

# 2. DATA PREPARATION

# Prepare Calendar: sort by date and add 'd' column
calendar <- calendar |>
  arrange(date) |>                  
  mutate(d = paste0("d_", row_number())) 

# Filter Sales for store TX_3 and clean item_id
sales_fixed <- sales |>
  mutate(
    store_id = "TX_3", 
    item_id = sub("_TX_3_.*", "", id) 
  )

# Pivot data to long-format (time series friendly)
sales_long <- sales_fixed |>
  pivot_longer(
    cols = starts_with("d_"), 
    names_to = "d", 
    values_to = "sales"
  )

# Join with calendar data
full_data <- sales_long |>
  left_join(calendar, by = "d")

# Join with price data
full_data <- full_data |>
  left_join(prices, by = c("store_id", "item_id", "wm_yr_wk"))

# Format date column correctly
full_data <- full_data |>
  mutate(date = mdy(date))

# Convert to tsibble (Time Series Tibble)
# Key is the product (item_id), Index is the date
ts_data <- full_data |>
  as_tsibble(key = item_id, index = date)

# Quick check of the data structure
glimpse(ts_data)

# 3. AGGREGATION TO TOTAL SALES (TX3)

# Aggregate sales for all products in store TX3 per day.
total_sales_tx3 <- ts_data |>
  index_by(date) |>
  summarise(total_sales = sum(sales, na.rm = TRUE)) |>
  fill_gaps(total_sales = 0) 


# 4. EXPLORATORY DATA ANALYSIS (EDA)

# A. General Trend (Moving Averages)
total_sales_tx3 |>
  mutate(
    MA_7 = slider::slide_dbl(total_sales, mean, .before = 6, .complete = TRUE),
    MA_28 = slider::slide_dbl(total_sales, mean, .before = 27, .complete = TRUE)
  ) |>
  autoplot(total_sales, color="gray80") + # Raw data in light gray
  geom_line(aes(y = MA_7), color = "steelblue", linewidth = 0.8) +
  geom_line(aes(y = MA_28), color = "darkred", linewidth = 1) +
  labs(title = "Daily Sales with Moving Averages",
       subtitle = "Blue = 7-day average, Red = 28-day (long term trend)",
       y = "Total Sales", x = "Date") +
  theme_minimal()

# B. Seasonality (Boxplots)
# Day of the week pattern
total_sales_tx3 |>
  mutate(day_name = wday(date, label = TRUE, week_start = 1)) |> 
  ggplot(aes(x = day_name, y = total_sales)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7) +
  labs(title = "Seasonal Pattern: Sales Distribution per Weekday",
       x = "Day", y = "Total Sales") +
  theme_minimal()

# Month of the year pattern
total_sales_tx3 |>
  mutate(month_name = month(date, label = TRUE)) |>
  ggplot(aes(x = month_name, y = total_sales)) +
  geom_boxplot(fill = "orange", alpha = 0.7) +
  labs(title = "Seasonal Pattern: Sales Distribution per Month",
       x = "Month", y = "Total Sales") +
  theme_minimal()

# Shows how the weekly pattern behaves over the years
total_sales_tx3 |>
  gg_season(total_sales, period = "week") +
  labs(title = "Seasonal Plot: Weekly pattern over time",
       y = "Sales") +
  theme_minimal()

# Shows how sales evolve per month over the years (Subseries)
total_sales_tx3 |>
  gg_subseries(total_sales, period = "month") +
  labs(title = "Subseries Plot: Monthly evolution",
       y = "Sales") +
  theme_minimal()


# D. Autocorrelation (ACF)
total_sales_tx3 |>
  ACF(total_sales, lag_max = 35) |>
  autoplot() +
  labs(title = "Autocorrelation Plot (ACF)",
       subtitle = "Peaks at 7, 14, 21 indicate a very strong weekly pattern") +
  theme_minimal()


# 5. SUMMARY STATISTICS
summary_stats <- total_sales_tx3 |>
  as_tibble() |>
  summarise(
    Mean_Sales = mean(total_sales),
    Median_Sales = median(total_sales),
    Min_Sales = min(total_sales),
    Max_Sales = max(total_sales),
    SD_Sales = sd(total_sales),
    Total_Days = n()
  )

print(summary_stats)