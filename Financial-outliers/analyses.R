detect_top_recent <- function(close, open, high, period = 100) {
  # Check if all input vectors have the same length
  if (!(length(close) == length(open) && length(open) == length(high))) {
    stop("All input vectors (close, open, high) must have the same length.")
  }
  
  n <- length(close)
  
  # Initialize the is_top_recent logical vector
  is_top_recent <- rep(FALSE, n)
  
  # Compute is_top_recent for each row
  for (i in (period + 1):n) {
    # Calculate the maximum high in the previous 'period' rows
    previous_highs <- high[(i - period):(i - 1)]
    max_previous_high <- max(previous_highs, na.rm = TRUE)
    
    # Determine if the current close is greater than the max of previous highs
    is_top_recent[i] <- close[i] > max_previous_high
  }
  
  # Compute priceChange = (close - open) / open
  priceChange <- (close - open) / open
  
  # Compute next row's priceChange
  next_priceChange <- c(priceChange[-1], NA)  # The last row has no "next" row
  
  # Assign categories based on is_top_recent
  categories <- ifelse(is_top_recent, "is_top_recent", "not is_top_recent")
  
  # Create a data frame to facilitate aggregation
  df <- data.frame(
    category = categories,
    next_priceChange = next_priceChange
  )
  
  # Remove the last row where next_priceChange is NA
  df <- df[!is.na(df$next_priceChange), ]
  
  
  
  # Calculate the average priceChange for each category
  avg_priceChange <- aggregate(next_priceChange ~ category, data = df, FUN = mean)
  
  # Ensure the categories are ordered as "is_top_recent" and "not is_top_recent"
  avg_priceChange <- avg_priceChange[match(c("is_top_recent", "not is_top_recent"), avg_priceChange$category), ]
  
  # Handle cases where a category might be missing (e.g., no "is_top_recent")
  avg_priceChange$next_priceChange[is.na(avg_priceChange$next_priceChange)] <- 0
  
  # Prepare x_values and y_values
  x_vals <- avg_priceChange$category
  y_vals <- avg_priceChange$next_priceChange
  
  return(list(x_values = x_vals, y_values = y_vals))
}
detect_bottom_recent <- function(close, open, low, period = 100) {
  # Check if all input vectors have the same length
  if (!(length(close) == length(open) && length(open) == length(low))) {
    stop("All input vectors (close, open, low) must have the same length.")
  }
  
  n <- length(close)
  
  # Initialize the is_bottom_recent logical vector
  is_bottom_recent <- rep(FALSE, n)
  
  # Compute is_bottom_recent for each row
  for (i in (period + 1):n) {
    # Calculate the minimum low in the previous 'period' rows
    previous_lows <- low[(i - period):(i - 1)]
    min_previous_low <- min(previous_lows, na.rm = TRUE)
    
    # Determine if the current close is less than the min of previous lows
    is_bottom_recent[i] <- close[i] < min_previous_low
  }
  
  # Compute priceChange = (close - open) / open
  priceChange <- (close - open) / open
  
  # Compute next row's priceChange
  next_priceChange <- c(priceChange[-1], NA)  # The last row has no "next" row
  
  # Assign categories based on is_bottom_recent
  categories <- ifelse(is_bottom_recent, "is_bottom_recent", "not is_bottom_recent")
  
  # Create a data frame to facilitate aggregation
  df <- data.frame(
    category = categories,
    next_priceChange = next_priceChange
  )
  
  # Remove the last row where next_priceChange is NA
  df <- df[!is.na(df$next_priceChange), ]
  
  # Calculate the average priceChange for each category
  avg_priceChange <- aggregate(next_priceChange ~ category, data = df, FUN = mean)
  
  # Ensure the categories are ordered as "is_bottom_recent" and "not is_bottom_recent"
  avg_priceChange <- avg_priceChange[match(c("is_bottom_recent", "not is_bottom_recent"), avg_priceChange$category), ]
  
  # Handle cases where a category might be missing (e.g., no "is_bottom_recent")
  avg_priceChange$next_priceChange[is.na(avg_priceChange$next_priceChange)] <- 0
  
  # Prepare x_values and y_values
  x_vals <- avg_priceChange$category
  y_vals <- avg_priceChange$next_priceChange
  
  return(list(x_values = x_vals, y_values = y_vals))
}



# Function to check if last 2 candles had similar open & close with increased volume
check_stable_high_volume <- function(open, close, volume, factor = 1.5, lookback = 10) {
  n <- length(open)
  result <- sapply(1:n, function(i) {
    if (i < 2 || i <= lookback) {
      return(FALSE)
    } else {
      avg_volume <- mean(volume[(i - lookback):(i - 1)], na.rm = TRUE)
      high_vol <- volume[i - 1] > factor * avg_volume && volume[i] > factor * avg_volume
      stable_price <- abs(open[i] - close[i]) / open[i] < 0.005 &&
        abs(open[i - 1] - close[i - 1]) / open[i - 1] < 0.005
      return(high_vol && stable_price)
    }
  })
  return(result)
}

detect_double_high_volume_vectorized <- function(close, open, volume, period = 10) {
  if (!(length(close) == length(open) && length(open) == length(volume))) {
    stop("All input vectors (close, open, volume) must have the same length.")
  }
  
  n <- length(close)
  
  # Calculate rolling average volume
  library(zoo)  # For rollapply
  
  rolling_avg_volume <- rollapply(volume, width = period, FUN = mean, align = "right", fill = NA, na.rm = TRUE)
  
  # Identify high-volume candles
  high_volume <- volume >= 1.5 * rolling_avg_volume
  
  # Identify two consecutive high-volume candles
  double_high_volume <- high_volume & lag(high_volume, 1, na.pad = FALSE)
  
  # Determine candle directions
  is_bull <- close > open
  is_bear <- close < open
  
  # Classify patterns
  double_bull_high_volume <- double_high_volume & is_bull & lag(is_bull, 1, na.pad = FALSE)
  double_bear_high_volume <- double_high_volume & is_bear & lag(is_bear, 1, na.pad = FALSE)
  
  # Assign categories
  category <- ifelse(double_bull_high_volume, "double_bull_high_volume",
                     ifelse(double_bear_high_volume, "double_bear_high_volume", "not_double_high_volume"))
  
  # Compute priceChange and next_priceChange
  priceChange <- (close - open) / open
  next_priceChange <- c(priceChange[-1], NA)
  
  # Create data frame for aggregation
  df <- data.frame(
    category = category,
    next_priceChange = next_priceChange
  )
  
  # Remove rows with NA next_priceChange
  df <- df[!is.na(df$next_priceChange), ]
  
  # Aggregate average priceChange
  avg_priceChange <- aggregate(next_priceChange ~ category, data = df, FUN = mean)
  
  # Ensure all categories are present
  desired_order <- c("double_bull_high_volume", "double_bear_high_volume", "not_double_high_volume")
  avg_priceChange <- avg_priceChange[match(desired_order, avg_priceChange$category), ]
  avg_priceChange$next_priceChange[is.na(avg_priceChange$next_priceChange)] <- 0
  
  # Prepare output
  x_vals <- avg_priceChange$category
  y_vals <- avg_priceChange$next_priceChange
  
  return(list(x_values = x_vals, y_values = y_vals))
}

# Function to analyze how the first n minutes of market impact the next n minutes
impact_first_n_minutes <- function(datetime, close, n = 30, markets = c("HK", "LN", "NY")) {
  data <- data.frame(datetime = as.POSIXct(datetime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
                     close = close)
  data$hour <- as.numeric(format(data$datetime, "%H"))
  data$minute <- as.numeric(format(data$datetime, "%M"))
  
  market_hours <- list(
    HK = c(1, 9),   # 1 AM UTC (HK open)
    LN = c(8, 16),  # 8 AM UTC (London open)
    NY = c(13, 21)  # 1 PM UTC (New York open)
  )
  
  results <- lapply(markets, function(market) {
    start_hour <- market_hours[[market]][1]
    end_hour <- market_hours[[market]][2]
    
    subset <- data %>% filter(hour >= start_hour & hour < end_hour)
    
    subset$period <- floor((subset$minute + 60 * (subset$hour - start_hour)) / n)
    period_change <- subset %>% group_by(period) %>% summarise(
      price_change = last(close) / first(close) - 1, .groups = "drop"
    )
    return(list(market = market, impact = period_change))
  })
  return(results)
}

# Function to compare price movement with liquidity quartiles
compare_liquidity_quartiles <- function(close, liquidity_index) {
  quartiles <- quantile(liquidity_index, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
  liquidity_category <- cut(liquidity_index, breaks = c(-Inf, quartiles, Inf), labels = c("Low", "Mid-Low", "Mid-High", "High"))
  
  price_change <- c(NA, diff(close))
  
  result <- data.frame(
    liquidity_category = liquidity_category,
    price_change = price_change
  ) %>% group_by(liquidity_category) %>%
    summarise(avg_price_change = mean(price_change, na.rm = TRUE), .groups = "drop")
  
  return(result)
}



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

averageContinuationNYSession <- function(datetime_str, open, high, low, close, hour_of_day) {
  
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
  
  # 2) Convert to POSIXct and group by date (day)
  data$datetime <- as.POSIXct(
    data$datetime_str, 
    format = "%Y-%m-%d %H:%M:%S", 
    tz = "UTC"
  )
  
  library(dplyr)
  
  daily_analysis <- data %>%
    # Group by the date (based on datetime)
    group_by(day = as.Date(datetime, tz = "UTC")) %>%
    # 3) Summarize open/close at hours 9, 10, and 17
    summarise(
      price_9  = first(open[hour_of_day == 9]),       # open at hour 9
      price_10 = first(close[hour_of_day == 10]),     # close at hour 10
      price_17 = first(close[hour_of_day == 17]),     # close at hour 17
      .groups  = "drop"
    ) %>%
    # 4) Calculate the percentage change for the next 7 hours (from hour 10 to hour 17)
    mutate(
      next_7_hours = (price_17 - price_10) / price_10 * 100   # percent change
    ) %>%
    # 5) Filter out non-finite or missing values
    filter(
      !is.na(next_7_hours),
      is.finite(next_7_hours)
    )
  
  # 6) Return the 'day' vector as x_values and 'next_7_hours' as y_values
  return(list(
    x_values = daily_analysis$day,
    y_values = daily_analysis$next_7_hours
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

  res <- showPrice(close, open, low, high, priceRange, datetime)
  #res <- averageVolatilityThroughDay(dataframe$datetime_str, open, high, low) 
  #res <- averageContinuationFromStartOfDay(dataframe$datetime_str, open, high, low, close, hourOfDay) 
  #res <- detect_top_recent(close, open, high)
  #res <- detect_bottom_recent(close, open, low)
  #res <- detect_double_high_volume_vectorized(close, open, volume)
  #res <- averageContinuationNYSession(dataframe$datetime_str, open, high, low, close, hourOfDay)
  
  return(res)
}


