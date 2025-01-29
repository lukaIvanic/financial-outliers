library(forecast)
library(ggplot2)
library(tseries)
library(lubridate)

data <- read.csv("BTCUSDT1h.csv")

head(data)
str(data)

data <- data[order(data$datetime), ]
#btc_ts <- ts(data$close, frequency=24, start=c(year(min(data$datetime)), yday(min(data$datetime))))
data$datetime <- ymd_hms(data$datetime_str)
desired_year <- 2024
desired_month <- 1  # January

# Filter data for January 2024
data_jan2024 <- subset(data, year(datetime) == desired_year & month(datetime) == desired_month)

# Verify the number of observations
nrow(data_jan2024)
close_prices <- data_jan2024$close

# Create the time series object
# Since the data is hourly, the frequency is 24 (hours per day)
btc_ts <- ts(close_prices, frequency = 24, start = c(desired_year, 1))

plot(btc_ts, main = "Bitcoin Closing Prices - January 2024", ylab = "Close Price", xlab = "Hour")


print(btc_ts)

autoplot(btc_ts) +
  ggtitle("Bitcoin Close Price Time Series") +
  xlab("Time") +
  ylab("Close Price (USDT)") +
  theme_minimal()


decomposed <- stl(btc_ts, s.window="periodic")
autoplot(decomposed) +
  ggtitle("STL Decomposition of Bitcoin Close Price")


adf_test <- adf.test(btc_ts, alternative="stationary")

print(adf_test)



btc_diff <- diff(btc_ts, differences=1)

# Plot the differenced series
autoplot(btc_diff) +
  ggtitle("First Differenced Bitcoin Close Price") +
  xlab("Time") +
  ylab("Differenced Close Price (USDT)") +
  theme_minimal()

# Perform ADF test on differenced data
adf_test_diff <- adf.test(btc_diff, alternative="stationary")

print(adf_test_diff)



auto_model <- auto.arima(btc_ts, seasonal=TRUE, stepwise=FALSE, approximation=FALSE)

# Summary of the model
summary(auto_model)

ggAcf(btc_ts) + ggtitle("ACF of Bitcoin Close Price")
ggPacf(btc_ts) + ggtitle("PACF of Bitcoin Close Price")



manual_model <- arima(btc_ts, order=c(1,1,1), seasonal=list(order=c(1,1,1), period=24))

# Summary of the manual model
summary(manual_model)



checkresiduals(auto_model)


autoplot(auto_model$residuals) +
  ggtitle("Residuals of ARIMA Model") +
  xlab("Time") +
  ylab("Residuals") +
  theme_minimal()

# Histogram of residuals
ggplot(data.frame(residuals=auto_model$residuals), aes(x=residuals)) +
  geom_histogram(binwidth=... , fill="blue", color="black", alpha=0.7) +
  ggtitle("Histogram of Residuals") +
  xlab("Residuals") +
  ylab("Frequency") +
  theme_minimal()

# ACF of residuals
ggAcf(auto_model$residuals) + ggtitle("ACF of Residuals")





shapiro.test(auto_model$residuals)

Box.test(auto_model$residuals, lag=24, type="Ljung-Box")



forecast_horizon <- 24
btc_forecast <- forecast(auto_model, h=forecast_horizon)

# Plot the forecast
autoplot(btc_forecast) +
  ggtitle("Bitcoin Price Forecast") +
  xlab("Time") +
  ylab("Close Price (USDT)") +
  theme_minimal()



print(btc_forecast)




# Forecasted mean
forecast_mean <- btc_forecast$mean

# Lower and upper confidence intervals
forecast_lower <- btc_forecast$lower
forecast_upper <- btc_forecast$upper

# Combine into a data frame
forecast_df <- data.frame(
  Time = seq(from=last(data$datetime) + hours(1), by="hour", length.out=forecast_horizon),
  Forecast = as.numeric(forecast_mean),
  Lower_80 = as.numeric(forecast_lower[,1]),
  Upper_80 = as.numeric(forecast_upper[,1]),
  Lower_95 = as.numeric(forecast_lower[,2]),
  Upper_95 = as.numeric(forecast_upper[,2])
)

# View the forecast data frame
head(forecast_df)




# Forecasted mean
forecast_mean <- btc_forecast$mean

# Lower and upper confidence intervals
forecast_lower <- btc_forecast$lower
forecast_upper <- btc_forecast$upper

# Combine into a data frame
forecast_df <- data.frame(
  Time = seq(from=last(data$datetime) + hours(1), by="hour", length.out=forecast_horizon),
  Forecast = as.numeric(forecast_mean),
  Lower_80 = as.numeric(forecast_lower[,1]),
  Upper_80 = as.numeric(forecast_upper[,1]),
  Lower_95 = as.numeric(forecast_lower[,2]),
  Upper_95 = as.numeric(forecast_upper[,2])
)

# View the forecast data frame
head(forecast_df)

