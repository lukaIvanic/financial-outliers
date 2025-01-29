library(dplyr)
library(ggplot2)

data <- read.csv("BTCUSDT1h.csv")
data$volatility <- (data$high - data$low) / data$open
data$datetime <- as.POSIXct(data$datetime_str, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
data$hour_of_day <- as.numeric(strftime(data$datetime, "%H"))

lower_limit <- quantile(data$volatility, 0.05, na.rm = TRUE)
upper_limit <- quantile(data$volatility, 0.95, na.rm = TRUE)

filtered_data <- data %>%
  filter(volatility >= lower_limit & volatility <= upper_limit)

ggplot(filtered_data, aes(x = factor(hour_of_day), y = volatility, fill = factor(hour_of_day))) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_viridis_d() +
  labs(
    title = "Bitcoin Volatility by Hour of Day (Filtered Outliers)",
    x = "Hour of Day (0-23)",
    y = "Volatility",
    fill = "Hour of Day"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )
