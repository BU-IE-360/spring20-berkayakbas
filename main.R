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
  # 0. Initialization of vectors
  method_names = c(
    "Naive",
    "Mean",
    "HW_Additive",
    "HW_Multiplicative",
    'Linear Regression',
    'Exponential Smoothing',
    'Auto Arima',
    'TBATS'
  )
  
  forecast <- vector("numeric", length(method_names))
  accuracy_R2<-vector("numeric", length(method_names))
  accuracy_ME <- vector("numeric", length(method_names))
  accuracy_RMSE <- vector("numeric", length(method_names))
  accuracy_MAE <- vector("numeric", length(method_names))
  accuracy_MAPE <- vector("numeric", length(method_names))
  accuracy_MASE <- vector("numeric", length(method_names))
  accuracy_ACF1 <- vector("numeric", length(method_names))
  
  # 1. Naive
  index = 1
  print(index)
  model = naive(product$sold_count, h = 2)
  forecast[index] <- as.numeric(model$mean[2])
  accuracy = accuracy(model)
  accuracy_ME[index] <- accuracy[, 'ME']
  accuracy_RMSE[index] <- accuracy[, 'RMSE']
  accuracy_MAE[index] <- accuracy[, 'MAE']
  accuracy_MAPE[index] <- accuracy[, 'MAPE']
  accuracy_MASE[index] <- accuracy[, 'MASE']
  accuracy_ACF1[index] <- accuracy[, 'ACF1']
  
  # 2. Mean Forecast
  index = 2
  print(index)
  model = meanf(product$sold_count, h = 2)
  forecast[index] <- as.numeric(model$mean[2])
  accuracy = accuracy(model)
  accuracy_ME[index] <- accuracy[, 'ME']
  accuracy_RMSE[index] <- accuracy[, 'RMSE']
  accuracy_MAE[index] <- accuracy[, 'MAE']
  accuracy_MAPE[index] <- accuracy[, 'MAPE']
  accuracy_MASE[index] <- accuracy[, 'MASE']
  accuracy_ACF1[index] <- NA
  
  # 3. Holt Winters Additive
  index = 3
  print(index)
  model = HoltWinters(ts(product$sold_count , frequency = 7)[, ], seasonal = "additive")
  forecast[index] <-as.numeric(forecast(model, h = 2)$mean[2])
  accuracy = accuracy(forecast(model, h=2))
  accuracy_ME[index] <- accuracy[, 'ME']
  accuracy_RMSE[index] <- accuracy[, 'RMSE']
  accuracy_MAE[index] <- accuracy[, 'MAE']
  accuracy_MAPE[index] <- accuracy[, 'MAPE']
  accuracy_MASE[index] <- accuracy[, 'MASE']
  accuracy_ACF1[index] <- accuracy[, 'ACF1']
  
  # 4. Holt Winters Multiplicative
  if (sum(ts(product$sold_count , frequency = 7)[, 1] == 0) == 0) {
    forecast[4] <-
      as.numeric(forecast(HoltWinters(
        ts(product$sold_count , frequency = 7)[, ], seasonal = "multiplicative"
      ), h = 2)$mean[2])
  }
  else {
    forecast[4] <- NA
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
  index= 5
  lm_model_1<-lm(sold_count~ visit_count+favored_count+basket_count
                 , data=product)
  newdata_f<-product[(nrow(product)-1):nrow(product), c("visit_count","favored_count",
                                                                       "basket_count")]
  index(newdata_f)<-(index(newdata_f)+2)
  preds<-predict(lm_model_1, newdata = newdata_f)
  
  
  forecast[index] <- preds[2]
  accuracy_R2[index]<-summary(lm_model_1)$r.squared
  
  # 6. Exponential Smoothing
  index = 6
  print(index)
  model = ses(product$sold_count, h = 2)
  forecast[index] <- as.numeric(forecast(model, h = 2)$mean[2])
  accuracy = accuracy(model)
  accuracy_ME[index] <- accuracy[, 'ME']
  accuracy_RMSE[index] <- accuracy[, 'RMSE']
  accuracy_MAE[index] <- accuracy[, 'MAE']
  accuracy_MAPE[index] <- accuracy[, 'MAPE']
  accuracy_MASE[index] <- accuracy[, 'MASE']
  accuracy_ACF1[index] <- accuracy[, 'ACF1']
  
  # 7. Auto Arima
  index = 7
  print(index)
  model = auto.arima(product$sold_count)
  forecast[index] <- as.numeric(forecast(model, h = 2)$mean[2])
  accuracy = accuracy(model)
  accuracy_ME[index] <- accuracy[, 'ME']
  accuracy_RMSE[index] <- accuracy[, 'RMSE']
  accuracy_MAE[index] <- accuracy[, 'MAE']
  accuracy_MAPE[index] <- accuracy[, 'MAPE']
  accuracy_MASE[index] <- accuracy[, 'MASE']
  accuracy_ACF1[index] <- accuracy[, 'ACF1']
  
  # 8. TBATS
  index = 8
  print(index)
  model = tbats(ts(product$sold_count))
  forecast[index] <- as.numeric(forecast(model, h = 2)$mean[2])
  accuracy = accuracy(model)
  accuracy_ME[index] <- accuracy[, 'ME']
  accuracy_RMSE[index] <- accuracy[, 'RMSE']
  accuracy_MAE[index] <- accuracy[, 'MAE']
  accuracy_MAPE[index] <- accuracy[, 'MAPE']
  accuracy_MASE[index] <- accuracy[, 'MASE']
  accuracy_ACF1[index] <- accuracy[, 'ACF1']
  
  columns = cbind(forecast, accuracy_R2, accuracy_ME, accuracy_RMSE, accuracy_MAE, accuracy_MAPE, accuracy_MASE, accuracy_ACF1)
  results <- as.data.frame(columns)
  row.names(results) <- method_names
  return(results)
}

product_ids = unique(data$product_content_id)
product_id = product_ids[1]
product_data = data[product_content_id == product_id]
product_data = make_xts(product_data)
get_product_forecasts(product_data)


