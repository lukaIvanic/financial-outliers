library(dplyr)
library(ggplot2)

data <- read.csv("BTCUSDT1h.csv")
data$volatility <- (data$high - data$low) / data$open

data$datetime <- as.POSIXct(data$datetime_str, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
data$hour_of_day <- as.numeric(strftime(data$datetime, "%H"))


hourly_volatility <- data %>%
  group_by(hour_of_day) %>%
  summarise(mean_volatility = mean(volatility, na.rm = TRUE))

ggplot(hourly_volatility, aes(x=factor(hour_of_day), y=mean_volatility, fill=mean_volatility)) +
  geom_bar(stat="identity") +
  scale_fill_gradient(low="blue", high="red") +
  labs(title="Average Bitcoin Volatility by Hour of Day",
       x="Hour of Day (0-23)",
       y="Mean Volatility") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))
