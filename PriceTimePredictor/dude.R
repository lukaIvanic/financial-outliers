library(dplyr)
library(ggplot2)
library(lubridate)

data <- read.csv("BTCUSDT1h.csv")
data$datetime <- as.POSIXct(data$datetime_str, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
data$hour_of_day <- (as.numeric(strftime(data$datetime, "%H")) + (23)) %% 24
data$day <- as.Date(data$datetime)


first_10_rows <- head(data, 240)
data_2020 <- data %>%
  filter(year(datetime) == 2020)
data_2021 <- data %>%
  filter(year(datetime) == 2021)
data_2022 <- data %>%
  filter(year(datetime) == 2022)
data_2023 <- data %>%
  filter(year(datetime) == 2023)
data_2024 <- data %>%
  filter(year(datetime) == 2024)



daily_analysis <- data_2024 %>%
  group_by(day) %>%
  summarise(
    price_0 = first(open[hour_of_day == 1]),
    price_4 = first(close[hour_of_day == 5]),
    price_23 = first(close[hour_of_day == 22]),
    .groups = "drop"
  ) %>%
  mutate(
    p40 = price_4 - price_0,
    upUntil4 = ((p40) / price_0) * 100,
    p234 = (price_23 - price_4),
    continuation = ((p234) / price_4) * 100
  ) %>%
  filter(
    !is.na(upUntil4),                
    is.finite(upUntil4),             
    !is.na(continuation),            
    is.finite(continuation)     
  )



print(first_10_rows)
print(daily_analysis)

print(range(daily_analysis$upUntil4, na.rm = TRUE))


daily_analysis <- daily_analysis %>%
  mutate(
    upUntil4_bin = cut(upUntil4, breaks = seq(floor(-5), ceiling(5), by = 1), include.lowest = TRUE)
  ) %>%
  filter(!is.na(upUntil4_bin))



binned_analysis <- daily_analysis %>%
  group_by(upUntil4_bin) %>%
  summarise(
    mean_continuation = mean(continuation, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(binned_analysis, aes(x = upUntil4_bin, y = mean_continuation, fill = mean_continuation)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(
    title = "Average Continuation vs UpUntil4 Bins",
    x = "UpUntil4 Bins (Price Change 0th to 4th Hour) [%]",
    y = "Mean Continuation (Price Change 4th to 23rd Hour) [%]",
    fill = "Mean Continuation"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )

