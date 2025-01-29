library(quantmod)
library(tseries)


indf_data <- getSymbols(Symbols = "TSLA", src = "yahoo", from = Sys.Date() - 2017, 
                        to = Sys.Date(), auto.assign = FALSE)


indf_data <- na.omit(indf_data)

View(indf_data)

chart_Series(indf_data, col = "black")


add_SMA(n = 100, on = 1, col = "red")
add_SMA(n = 20, on = 1, col = "black")

add_RSI(n = 14, maType = "SMA")


add_BBands(n = 20, maType = "SMA", sd = 1, on = -1)
add_MACD(fast = 12, slow = 25, signal = 9, maType = "SMA", histogram = TRUE)


library(funModeling)
library(tidyverse)
library(Hmisc)

indf_log <- log(indf_data)
head(indf_log, n = 10)

glimpse(indf_data)
df_status(indf_data)

describe(indf_data)



acf_log <- acf(indf_log, lag.max = 320)

diff.acf <- acf(indf_log)


models <- lm(TSLA.Open ~ TSLA.Adjusted, data = indf_data)
ggplot(indf_data, aes(x = TSLA.Adjusted, y = TSLA.Open)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "TSLA.Adjusted", y = "TSLA.Open", title = "Linear Regression")


library(ggplot2)
cor_matrix <- cor(indf_data)
cor_data <- reshape2::melt(cor_matrix)
ggplot(cor_data, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "Correlation Matrix Heatmap")


library(caTools)
library(forecast)

UP_UNTIL = 1000

train_data <- indf_log[1:1000, "TSLA.Close"]  
set.seed(123)
arima_model <- auto.arima(train_data, stationary = TRUE, ic = c("aicc", "aic", "bic"), 
                          trace = TRUE)
summary(arima_model)

checkresiduals(arima_model)


arima <- arima(train_data, order = c(0, 0, 5))
summary(arima)


close_prices <- Cl(indf_data)


forecast1 <- forecast(arima, h = 370)
plot(forecast1)




train_datas <- indf_log[1:1000, "TSLA.Close"]
arima <- arima(train_datas, order = c(0, 0, 5))
forecast_ori <- forecast(arima, h = 370)
a <- ts(train_datas)
forecast_ori %>% autoplot() + autolayer(a)


str(indf_data)
summary(indf_data)





arima <- arima(train_data, order = c(0, 0, 5))
forecast1 <- forecast(arima, h = 370)
forecasted_values <- forecast1$mean

actual_values <- coredata(indf_log[1001:1370, "TSLA.Close"])
errors <- actual_values - forecasted_values
mse <- mean(errors^2)
rmse <- sqrt(mse)  
mae <- mean(abs(errors))
mape <- mean(abs(errors/actual_values)) * 100

cat("Mean Squared Error (MSE):", mse, "\n")
cat("Root Mean Squared Error (RMSE):", rmse, "\n")
cat("Mean Absolute Error (MAE):", mae, "\n")
cat("Mean Absolute Percentage Error (MAPE):", mape, "%\n")




