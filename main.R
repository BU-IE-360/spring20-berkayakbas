source("api_connection.R")

# Requirements
require("forecast")
require("ggplot2")
require("xts")

make_xts <- function(product1) {
  data_seq <- seq(from = first(product1$event_date),
                  last(product1$event_date),
                  by = "days")
  
  data_seq1 <-
    seq(
      from = first(product1[product1$sold_count != 0, ]$event_date),
      last(product1$event_date),
      by = "days"
    )
  
  x1 <- (length(data_seq) - length(data_seq1))
  product1 <- product1[(x1 + 1):length(data_seq),]
  product1$event_date <- NULL
  product1$product_content_id <- NULL
  product1 <- sapply(product1, as.numeric)
  product1 <- xts(product1, order.by = data_seq1)
  return(product1)
}

# Her bir method için;
# input product datası,
# outputs
# 1. point forecast (h=1) + plot
# 2. AIC, BIC accuracy, chechresiduals ()
# 3. data.table ile AIC, BIC ve point forecast'in return edilmesi

#   3.2 Linear Regression @BERKAY
#   3.5 Arima @BBG


get_product_forecasts <- function (product) {
  method_names = c(
    "Naive",
    "Mean",
    "HW_Additive",
    "HW_Multiplicative",
    'Linear Regression',
    'Exponential Smoothing',
    'Auto Arima'
  )
  forecasts <- vector("numeric", length(method_names))
  
  # 1. Naive
  forecasts[1] <- as.numeric(naive(product$sold_count, h = 1)$mean)
  
  # 2. Mean Forecast
  forecasts[2] <- as.numeric(meanf(product$sold_count, h = 1)$mean)
  
  # 3. Holt Winters Additive
  forecasts[3] <-
    as.numeric(forecast(HoltWinters(
      ts(product$sold_count , frequency = 7)[, ], seasonal = "additive"
    ), h = 1)$mean)
  
  # 4. Holt Winters Multiplicative
  if (sum(ts(product$sold_count , frequency = 7)[, 1] == 0) == 0) {
    forecasts[4] <-
      as.numeric(forecast(HoltWinters(
        ts(product$sold_count , frequency = 7)[, ], seasonal = "multiplicative"
      ), h = 1)$mean)
  }
  else {
    forecasts[4] <- NA
  }
  
  # Stepwise Regression - Forward
  # null=lm(sold_count ~ 1, data=product)
  # forward_lr = step(null, scope=list(lower=null, upper=full), direction="forward")
  #
  # Stepwise Regression - Backward
  # full=lm(sold_count ~ ., data=product)
  # backward_lr = step(full, scope=list(lower=null, upper=full), direction="backward")
  
  # Linear Regression
  # lr_model=lm(sold_count ~ ,filtered_product)
  forecasts[5] <- NA
  
  # Exponential Smoothing
  forecasts[6] <- as.numeric(ses(product$sold_count, h = 1)$mean)
  
  # Auto Arima
  forecasts[7] <-
    as.numeric(forecast(auto.arima(product$sold_count), h = 1)$mean)
  
  forecasts <- as.data.frame(forecasts)
  row.names(forecasts) <- method_names
  return(forecasts)
}

product_ids = unique(data$product_content_id)
product_id = product_ids[2]
product_data = data[product_content_id == product_id]
product_data = make_xts(product_data)
get_product_forecasts(product_data)