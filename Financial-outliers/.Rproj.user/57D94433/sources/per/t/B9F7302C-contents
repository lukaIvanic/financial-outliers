
showPrice <- function(close, open, low, high, priceRange, datetime) {
  x_vals <- datetime
  y_vals <- close

  
  
  return(list(x_values = x_vals, y_values = y_vals))
}

averageVolatilityThroughDay <- function(datetime_str, open, high, low) {
  
  # 1) Create a temporary data frame from the input vectors
  data <- data.frame(
    datetime_str = datetime_str,
    open         = open,
    high         = high,
    low          = low,
    stringsAsFactors = FALSE
  )
  
  # 2) Convert datetime_str to POSIXct and extract hour of day
  data$datetime     <- as.POSIXct(data$datetime_str, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  data$hour_of_day  <- as.numeric(strftime(data$datetime, "%H"))
  
  # 3) Calculate volatility
  data$volatility <- (data$high - data$low) / data$open
  
  # 4) Group by hour_of_day and compute mean volatility
  #    Then arrange so hours go in ascending order (0 ... 23)
  hourly_volatility <- data %>%
    group_by(hour_of_day) %>%
    summarise(mean_volatility = mean(volatility, na.rm = TRUE), .groups = "drop") %>%
    arrange(hour_of_day)
  
  # 5) Return the two result vectors in a list
  return(list(
    x_values = hourly_volatility$hour_of_day,
    y_values = hourly_volatility$mean_volatility
  ))
}


averageContinuationFromStartOfDay <- function(datetime_str, open, high, low, close, hour_of_day) {
  
  # 1) Create a data frame from the input vectors
  data <- data.frame(
    datetime_str = datetime_str,
    open         = open,
    high         = high,
    low          = low,
    close        = close,
    hour_of_day  = hour_of_day,
    stringsAsFactors = FALSE
  )
  
  # 2) Convert to POSIXct and extract the day (Date) for grouping
  data$datetime <- as.POSIXct(data$datetime_str, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  
  # 3) Summarize open/close at specific hours: 0, 4, 23
  daily_analysis <- data %>%
    summarise(
      price_0  = first(open[hour_of_day == 0]),
      price_4  = first(close[hour_of_day == 4]),
      price_23 = first(close[hour_of_day == 23]),
      .groups  = "drop"
    ) %>%
    mutate(
      upUntil4    = (price_4  - price_0)  / price_0  * 100,  # percent change from hour 0 to hour 4
      continuation = (price_23 - price_4) / price_4  * 100   # percent change from hour 4 to hour 23
    ) %>%
    # 4) Filter out non-finite or missing values
    filter(
      !is.na(upUntil4),
      is.finite(upUntil4),
      !is.na(continuation),
      is.finite(continuation)
    )
  
  print(length(daily_analysis$day))
  print(length(daily_analysis$continuation))
  
  
  # 5) Return the 'day' vector as x_values and 'continuation' as y_values 
  #    (adjust as needed if you prefer upUntil4 or something else)
  return(list(
    x_values = daily_analysis$day,
    y_values = daily_analysis$continuation
  ))
}

# columns: openTimeMs, open, high, low, close, volume, datetime_str
getAnalysis <- function(dataframe){
  
  openTimeMs <- dataframe$openTimeMs
  open <- dataframe$open
  high <- dataframe$high
  low <- dataframe$low
  close <- dataframe$close
  volume <- dataframe$volume
  datetime <- dataframe$datetime

  
  priceRange <- high - low
  
  priceChange <- close - open
  
  avgVolume <- mean(volume)
  
  dayOfWeek <- weekdays(datetime)
  hourOfDay <- as.numeric(strftime(datetime, "%H"))

  #res <- showPrice(close, open, low, high, priceRange, datetime)
  #res <- averageVolatilityThroughDay(dataframe$datetime_str, open, high, low) 
  res <- averageContinuationFromStartOfDay(dataframe$datetime_str, open, high, low, close, hourOfDay) 
  return(res)
}


