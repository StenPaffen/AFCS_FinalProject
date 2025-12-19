
library(fpp3)
library(lubridate)
library(readr)
options(readr.show_col_types = FALSE)
options(repr.plot.width = 12, repr.plot.height = 6)
# options(repr.plot.width = NULL, repr.plot.height = NULL)

# Load the CSVs
calendar_raw <- read_csv("calendar_afcs2025.csv", show_col_types = FALSE)
prices_raw   <- read_csv("sell_prices_afcs2025.csv", show_col_types = FALSE)
train_raw    <- read_csv("sales_train_validation_afcs2025.csv", show_col_types = FALSE)
val_raw      <- read_csv("sales_test_validation_afcs2025.csv", show_col_types = FALSE)

# Clean Calendar
calendar <- calendar_raw |>
  mutate(date = mdy(date)) |>
  arrange(date) |>
  mutate(d = paste0("d_", row_number())) |>
  select(d, date, wm_yr_wk, snap_TX, event_name_1, event_name_2)


# Function for to combine sales series with calendar and price series
process_data <- function(raw_df, calendar_df, prices_df) {
  
  df_long <- raw_df |>
    mutate(item_id = sub("_TX_3_.*", "", id), 
           store_id = "TX_3") |>
    select(-id) |>
    pivot_longer(
      cols = starts_with("d_"), 
      names_to = "d", 
      values_to = "sales"
    )
  
  df_joined <- df_long |>
    left_join(calendar_df, by = "d") |>
    left_join(prices_df, by = c("store_id", "item_id", "wm_yr_wk"))
  
  df_ts <- df_joined |>
    as_tsibble(key = item_id, index = date) |>
    mutate(
      is_event = if_else(!is.na(event_name_1), 1, 0)
    )
  
  
  return(df_ts)
}

train_ts <- process_data(train_raw, calendar, prices_raw)
val_ts <- process_data(val_raw, calendar, prices_raw)

ts_data <- train_ts

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
  theme( axis.text.x = element_text(angle = 90, hjust = 10, size = 4) )


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

# 6.correlation price and sales
price_impact <- ts_data |>
  as_tibble() |>
  group_by(item_id) |>
  summarise(correlation = cor(sales, sell_price, use = "complete.obs"))

price_impact |>
  ggplot(aes(x = correlation)) +
  geom_histogram(fill = "steelblue", bins = 30) +
  labs(title = "Distribution price-sales correlations per product",
       x = "Coorelation coefficient", y = "Number of products")

#7 difference in ACF for products
# 2 random products and their ACF
ts_data |>
  filter(item_id %in% sample(unique(ts_data$item_id), 9)) |>
  ACF(sales, lag_max = 21) |>
  autoplot() +
  facet_wrap(~item_id, scales = "free_y") +
  labs(title = "Different ACF for 9 random products")
       

