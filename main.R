source("api_connection.R")

# Requirements
require("forecast")
require("ggplot2")
require("xts")
require("greybox")

mape <- function(actual,preds){
  mape <- mean(abs((actual - preds)/actual))*100
  return (mape)
}

make_xts <- function(product_data) {
  # Imporevement of data for regression
  product_data[,lag1_sold_count:=shift(sold_count,1)]
  product_data[,lag7_sold_count:=shift(sold_count,7)]
  product_data[,lag28_sold_count:=shift(sold_count,28)]
  product_data[,lag365_sold_count:=shift(sold_count,365)]
  
  # Firstly fills NA prices to last observation, then first price is set for the prior NA prices.
  product_data[price == -1]$price = c(NA)
  product_data$price = na.locf(product_data$price, na.rm = FALSE)
  product_data$price = na.locf(product_data$price, na.rm = FALSE, fromLast = TRUE)
  
  data_seq <- seq(from = first(product_data$event_date),
                  last(product_data$event_date),
                  by = "days")
  
  data_seq1 <-
    seq(
      from = first(product_data[product_data$sold_count != 0, ]$event_date),
      last(product_data$event_date),
      by = "days"
    )
  
  x1 <- (length(data_seq) - length(data_seq1))
  product_data <- product_data[(x1 + 1):length(data_seq),]
  product_data$event_date <- NULL
  product_data$product_content_id <- NULL
  product_data <- sapply(product_data, as.numeric)
  product_data[is.na(product_data)] <- 0
  product_data <- xts(product_data, order.by = data_seq1)
  return(product_data)
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
    'Naive',
    'Mean',
    'HW_Additive',
    'HW_Multiplicative',
    'Exponential Smoothing',
    'Auto Arima',
    'TBATS',
    'Linear Regression',
    'Stepwise Backward',
    'Stepwise Forward'
  )
  testplots<-as.list(rep(NA, length(method_names)))
  forecast <- rep(NA, length(method_names))
  accuracy_ADJ_R2<- rep(NA, length(method_names))
  accuracy_ME <- rep(NA, length(method_names))
  accuracy_RMSE <- rep(NA, length(method_names))
  accuracy_MAE <- rep(NA, length(method_names))
  accuracy_MAPE <- rep(NA, length(method_names))
  accuracy_MASE <- rep(NA, length(method_names))
  accuracy_ACF1 <- rep(NA, length(method_names))
  
  #set test metrics
  accuracy_test_ADJ_R2<- rep(NA, length(method_names))
  accuracy_test_ME <- rep(NA, length(method_names))
  accuracy_test_RMSE <- rep(NA, length(method_names))
  accuracy_test_MAE <- rep(NA, length(method_names))
  accuracy_test_MAPE <- rep(NA, length(method_names))
  accuracy_test_MASE <- rep(NA, length(method_names))
  
  
  #get train and test subsets of data
  train_data<-product[1:(nrow(product)-2),]
  test_data<-product[(nrow(product)-1):nrow(product),]
  testing_for_newdata<-product[(nrow(train_data)-1):(nrow(product)-2),]
  index(testing_for_newdata)<-index(testing_for_newdata)+2
  # 1. Naive
  index = 1
  model = naive(product$sold_count, h = 2)
  forecast[index] <- as.numeric(model$mean[2])
  accuracy = accuracy(model)
  accuracy_ME[index] <- accuracy[, 'ME']
  accuracy_RMSE[index] <- accuracy[, 'RMSE']
  accuracy_MAE[index] <- accuracy[, 'MAE']
  accuracy_MAPE[index] <- accuracy[, 'MAPE']
  accuracy_MASE[index] <- accuracy[, 'MASE']
  accuracy_ACF1[index] <- accuracy[, 'ACF1']
  
  #1.1 Naive Testing
  model=naive(train_data$sold_count, h = nrow(test_data))
  accuracy=accuracy(model, test_data$sold_count)
  accuracy_test_ME[index] <- accuracy["Test set", 'ME']
  accuracy_test_RMSE[index] <- accuracy["Test set", 'RMSE']
  accuracy_test_MAE[index] <- accuracy["Test set", 'MAE']
  accuracy_test_MAPE[index] <- accuracy["Test set", 'MAPE']
  accuracy_test_MASE[index] <- accuracy["Test set", 'MASE']
  
  preds_1<- xts(model$mean, order.by= seq(first(index(test_data)),last(index(test_data)), by="days"))
  testplots[[index]]<-plot(c(train_data$sold_count,test_data$sold_count), main = paste0(method_names[index]))
  lines(preds_1, col = "red")
  
  # 2. Mean Forecast
  index = 2
  model = meanf(product$sold_count, h = 2)
  forecast[index] <- as.numeric(model$mean[2])
  accuracy = accuracy(model)
  accuracy_ME[index] <- accuracy[, 'ME']
  accuracy_RMSE[index] <- accuracy[, 'RMSE']
  accuracy_MAE[index] <- accuracy[, 'MAE']
  accuracy_MAPE[index] <- accuracy[, 'MAPE']
  accuracy_MASE[index] <- accuracy[, 'MASE']
  accuracy_ACF1[index] <- NA
  
  #2.1. Mean Testing
  model=meanf(train_data$sold_count, h = nrow(test_data))
  accuracy=accuracy(model, test_data$sold_count)
  accuracy_test_ME[index] <- accuracy["Test set", 'ME']
  accuracy_test_RMSE[index] <- accuracy["Test set", 'RMSE']
  accuracy_test_MAE[index] <- accuracy["Test set", 'MAE']
  accuracy_test_MAPE[index] <- accuracy["Test set", 'MAPE']
  accuracy_test_MASE[index] <- accuracy["Test set", 'MASE']
  
  preds_1<- xts(model$mean, order.by= seq(first(index(test_data)),last(index(test_data)), by="days"))
  testplots[[index]]<-plot(c(train_data$sold_count,test_data$sold_count), main = paste0(method_names[index]))
  lines(preds_1, col = "red")
  
  # 3. Holt Winters Additive
  index = 3
  model = HoltWinters(ts(product$sold_count , frequency = 7)[, ], seasonal = "additive")
  forecast[index] <-as.numeric(forecast(model, h = 2)$mean[2])
  accuracy = accuracy(forecast(model, h=2))
  accuracy_ME[index] <- accuracy[, 'ME']
  accuracy_RMSE[index] <- accuracy[, 'RMSE']
  accuracy_MAE[index] <- accuracy[, 'MAE']
  accuracy_MAPE[index] <- accuracy[, 'MAPE']
  accuracy_MASE[index] <- accuracy[, 'MASE']
  accuracy_ACF1[index] <- accuracy[, 'ACF1']
  
  #3.1 HW Add Testing
  
  model=HoltWinters(ts(train_data$sold_count , frequency = 7)[, ], seasonal = "additive")
  accuracy = accuracy(forecast(model, h=nrow(test_data)), test_data$sold_count)
  accuracy_test_ME[index] <- accuracy["Test set", 'ME']
  accuracy_test_RMSE[index] <- accuracy["Test set", 'RMSE']
  accuracy_test_MAE[index] <- accuracy["Test set", 'MAE']
  accuracy_test_MAPE[index] <- accuracy["Test set", 'MAPE']
  accuracy_test_MASE[index] <- accuracy["Test set", 'MASE']
  
  preds_1<- xts(forecast(model, h=nrow(test_data))$mean, order.by= seq(first(index(test_data)),last(index(test_data)), by="days"))
  testplots[[index]]<-plot(c(train_data$sold_count,test_data$sold_count), main = paste0(method_names[index]))
  lines(preds_1, col = "red")
  
  # 4. Holt Winters Multiplicative
  index=4
  if (sum(ts(product$sold_count , frequency = 7)[, 1] == 0) == 0) {
      
    model = HoltWinters(ts(product$sold_count , frequency = 7)[, ], seasonal = "multiplicative")
    forecast[index] <-as.numeric(forecast(model, h = 2)$mean[2])
    accuracy = accuracy(forecast(model, h=2))
    accuracy_ME[index] <- accuracy[, 'ME']
    accuracy_RMSE[index] <- accuracy[, 'RMSE']
    accuracy_MAE[index] <- accuracy[, 'MAE']
    accuracy_MAPE[index] <- accuracy[, 'MAPE']
    accuracy_MASE[index] <- accuracy[, 'MASE']
    accuracy_ACF1[index] <- accuracy[, 'ACF1']
    
    #HW Mult testing
    model=HoltWinters(ts(train_data$sold_count , frequency = 7)[, ], seasonal = "multiplicative")
    accuracy = accuracy(forecast(model, h=nrow(test_data)), test_data$sold_count)
    accuracy_test_ME[index] <- accuracy["Test set", 'ME']
    accuracy_test_RMSE[index] <- accuracy["Test set", 'RMSE']
    accuracy_test_MAE[index] <- accuracy["Test set", 'MAE']
    accuracy_test_MAPE[index] <- accuracy["Test set", 'MAPE']
    accuracy_test_MASE[index] <- accuracy["Test set", 'MASE']
    preds_1<- xts(forecast(model, h=nrow(test_data))$mean, order.by= seq(first(index(test_data)),last(index(test_data)), by="days"))
    testplots[[index]]<-plot(c(train_data$sold_count,test_data$sold_count), main = paste0(method_names[index]))
    lines(preds_1, col = "red")
  }
  else {
    forecast[4] <- NA
  }
  
  # 5. Exponential Smoothing
  index = 5
  model = ses(product$sold_count, h = 2)
  forecast[index] <- as.numeric(forecast(model, h = 2)$mean[2])
  accuracy = accuracy(model)
  accuracy_ME[index] <- accuracy[, 'ME']
  accuracy_RMSE[index] <- accuracy[, 'RMSE']
  accuracy_MAE[index] <- accuracy[, 'MAE']
  accuracy_MAPE[index] <- accuracy[, 'MAPE']
  accuracy_MASE[index] <- accuracy[, 'MASE']
  accuracy_ACF1[index] <- accuracy[, 'ACF1']
  
  #5.1 Exp Smt Testing
  
  model=ses(train_data$sold_count, h = nrow(test_data))
  accuracy=accuracy(model, test_data$sold_count)
  accuracy_test_ME[index] <- accuracy["Test set", 'ME']
  accuracy_test_RMSE[index] <- accuracy["Test set", 'RMSE']
  accuracy_test_MAE[index] <- accuracy["Test set", 'MAE']
  accuracy_test_MAPE[index] <- accuracy["Test set", 'MAPE']
  accuracy_test_MASE[index] <- accuracy["Test set", 'MASE']
  
  preds_1<- xts(model$mean, order.by= seq(first(index(test_data)),last(index(test_data)), by="days"))
  testplots[[index]]<-plot(c(train_data$sold_count,test_data$sold_count), main = paste0(method_names[index]))
  lines(preds_1, col = "red")
  
  # 6. Auto Arima
  index = 6
  model = auto.arima(product$sold_count)
  forecast[index] <- as.numeric(forecast(model, h = 2)$mean[2])
  accuracy = accuracy(model)
  accuracy_ME[index] <- accuracy[, 'ME']
  accuracy_RMSE[index] <- accuracy[, 'RMSE']
  accuracy_MAE[index] <- accuracy[, 'MAE']
  accuracy_MAPE[index] <- accuracy[, 'MAPE']
  accuracy_MASE[index] <- accuracy[, 'MASE']
  accuracy_ACF1[index] <- accuracy[, 'ACF1']
  
  # 6.1 Auto Arima Testing
  
  model=auto.arima(train_data$sold_count)
  accuracy=accuracy(forecast(model, h = nrow(test_data)), test_data$sold_count)
  accuracy_test_ME[index] <- accuracy["Test set", 'ME']
  accuracy_test_RMSE[index] <- accuracy["Test set", 'RMSE']
  accuracy_test_MAE[index] <- accuracy["Test set", 'MAE']
  accuracy_test_MAPE[index] <- accuracy["Test set", 'MAPE']
  accuracy_test_MASE[index] <- accuracy["Test set", 'MASE']
  preds_1<- xts(forecast(model, h = nrow(test_data))$mean, order.by= seq(first(index(test_data)),last(index(test_data)), by="days"))
  testplots[[index]]<-plot(c(train_data$sold_count,test_data$sold_count), main = paste0(method_names[index]))
  lines(preds_1, col = "red")
  # 7. TBATS
  index = 7
  model = tbats(ts(product$sold_count))
  forecast[index] <- as.numeric(forecast(model, h = 2)$mean[2])
  accuracy = accuracy(model)
  accuracy_ME[index] <- accuracy[, 'ME']
  accuracy_RMSE[index] <- accuracy[, 'RMSE']
  accuracy_MAE[index] <- accuracy[, 'MAE']
  accuracy_MAPE[index] <- accuracy[, 'MAPE']
  accuracy_MASE[index] <- accuracy[, 'MASE']
  accuracy_ACF1[index] <- accuracy[, 'ACF1']
  
  #7.1 TBATS Testing
  
  model=tbats(ts(train_data$sold_count))
  accuracy=accuracy(forecast(model, h = nrow(test_data)), test_data$sold_count)
  accuracy_test_ME[index] <- accuracy["Test set", 'ME']
  accuracy_test_RMSE[index] <- accuracy["Test set", 'RMSE']
  accuracy_test_MAE[index] <- accuracy["Test set", 'MAE']
  accuracy_test_MAPE[index] <- accuracy["Test set", 'MAPE']
  accuracy_test_MASE[index] <- accuracy["Test set", 'MASE']
  preds_1<- xts(forecast(model, h = nrow(test_data))$mean, order.by= seq(first(index(test_data)),last(index(test_data)), by="days"))
  testplots[[index]]<-plot(c(train_data$sold_count,test_data$sold_count), main = paste0(method_names[index]))
  lines(preds_1, col = "red")
  
  # 8. Linear Regression
  index= 8
  lm_model_1<-lm(sold_count~ visit_count+favored_count+basket_count, data=product)
  newdata_f<-product[(nrow(product)-1):nrow(product), c("visit_count","favored_count", "basket_count")]
  index(newdata_f)<-(index(newdata_f)+2)
  preds<-predict(lm_model_1, newdata = newdata_f)
  forecast[index] <- preds[2]
  accuracy_ADJ_R2[index]<-summary(lm_model_1)$adj.r.squared
  accuracy_MAE[index] <-MAE(lm_model_1$fitted.values,product$sold_count)
  accuracy_RMSE[index] <- sqrt(MSE(lm_model_1$fitted.values,product$sold_count))
  accuracy_MAPE[index] <- mape(product_data$sold_count, lm_model_1$fitted.values)
  
  # 8.1 Linear Regression Testing
  lm_model_1<-lm(sold_count~ visit_count+favored_count+basket_count, data=train_data)
  preds<-predict(lm_model_1, newdata = testing_for_newdata)
  accuracy_test_ADJ_R2[index]<-summary(lm_model_1)$adj.r.squared
  accuracy_test_MAE[index] <-MAE(preds,test_data$sold_count)
  accuracy_test_RMSE[index] <- sqrt(MSE(preds,test_data$sold_count))
  accuracy_test_MAPE[index] <- mape(test_data$sold_count, preds)
  
  preds_1<- xts(preds, order.by= seq(first(index(test_data)),last(index(test_data)), by="days"))
  testplots[[index]]<-plot(c(train_data$sold_count,test_data$sold_count), main = paste0(method_names[index]))
  lines(preds_1, col = "red")
  
  # Stepwise Regression
  null=lm(sold_count ~ 1, data=product)
  full=lm(sold_count ~ ., data=product)
  backward_lr = step(full, scope=list(lower=null, upper=full), direction="backward", trace = 0)
  forward_lr = step(null, scope=list(lower=null, upper=full), direction="forward", trace = 0)
  
  null_test=lm(sold_count ~ 1, data=train_data)
  full_test=lm(sold_count ~ ., data=train_data)
  backward_lr_test = step(full, scope=list(lower=null, upper=full), direction="backward", trace = 0)
  forward_lr_test = step(null, scope=list(lower=null, upper=full), direction="forward", trace = 0)
  
  # 9. Stepwise Regression - Backward
  index = 9
  newdata_f<-product[(nrow(product)-1):nrow(product),]
  index(newdata_f)<-(index(newdata_f)+2)
  preds<-predict(backward_lr, newdata = newdata_f)
  forecast[index] <- preds[2]
  accuracy_ADJ_R2[index]<-summary(backward_lr)$adj.r.squared
  accuracy_MAE[index] <-MAE(backward_lr$fitted.values,product$sold_count)
  accuracy_RMSE[index] <- sqrt(MSE(backward_lr$fitted.values,product$sold_count))
  accuracy_MAPE[index] <- mape(product_data$sold_count, backward_lr$fitted.values)
  
  #9.1 Step Backward -Test
  preds<-predict(backward_lr_test, newdata = testing_for_newdata)
  accuracy_test_ADJ_R2[index]<-summary(backward_lr_test)$adj.r.squared
  accuracy_test_MAE[index] <-MAE(preds,test_data$sold_count)
  accuracy_test_RMSE[index] <- sqrt(MSE(preds,test_data$sold_count))
  accuracy_test_MAPE[index] <- mape(test_data$sold_count, preds)
  
  preds_1<- xts(preds, order.by= seq(first(index(test_data)),last(index(test_data)), by="days"))
  testplots[[index]]<-plot(c(train_data$sold_count,test_data$sold_count), main = paste0(method_names[index]))
  lines(preds_1, col = "red")
  # 10. Stepwise Regression - Forward
  index = 10
  newdata_f<-product[(nrow(product)-1):nrow(product),]
  index(newdata_f)<-(index(newdata_f)+2)
  preds<-predict(forward_lr, newdata = newdata_f)
  forecast[index] <- preds[2]
  accuracy_ADJ_R2[index]<-summary(forward_lr)$adj.r.squared
  accuracy_MAE[index] <-MAE(forward_lr$fitted.values,product$sold_count)
  accuracy_RMSE[index] <- sqrt(MSE(forward_lr$fitted.values,product$sold_count))
  accuracy_MAPE[index] <- mape(product_data$sold_count, forward_lr$fitted.values)
  
  #10.1 Step Forward Test
  preds<-predict(forward_lr_test, newdata = testing_for_newdata)
  accuracy_test_ADJ_R2[index]<-summary(forward_lr_test)$adj.r.squared
  accuracy_test_MAE[index] <-MAE(preds,test_data$sold_count)
  accuracy_test_RMSE[index] <- sqrt(MSE(preds,test_data$sold_count))
  accuracy_test_MAPE[index] <- mape(test_data$sold_count, preds)
  
  preds_1<- xts(preds, order.by= seq(first(index(test_data)),last(index(test_data)), by="days"))
  testplots[[index]]<-plot(c(train_data$sold_count,test_data$sold_count), main = paste0(method_names[index]))
  lines(preds_1, col = "red")
  
  columns = cbind(forecast, accuracy_ADJ_R2, accuracy_ME, accuracy_RMSE, accuracy_MAE, accuracy_MAPE, accuracy_MASE, 
                  accuracy_ACF1,accuracy_test_ADJ_R2, accuracy_test_ME, accuracy_test_RMSE,
                  accuracy_test_MAE,accuracy_test_MAPE,accuracy_test_MASE)
  results <- as.data.frame(columns)
  row.names(results) <- method_names
  return(c(list(results),testplots))
}

# API Connection
# If data is updated in last 60 minutes, it uses offline data.
if (exists("data_last_updated_at")) {
  is_data_old = difftime(Sys.time(), data_last_updated_at) > 60 * 60
} else {is_data_old = TRUE}
if (is_data_old) {
  token = get_token(username = username,password = password,url = subm_url)
  data = get_data(token = token, url = subm_url)
}
product_names = data.table(read.csv("API Products.csv", sep = ';'))


# Initialize predictions
predictions=unique(data[,list(product_content_id)])
predictions[,forecast:=NA]
print(paste0("Last event date: ", tail(data, 1)$event_date))

# Product Analysis
product_ids = unique(data$product_content_id)
product_id = product_ids[1]
product_data = data[product_content_id == product_id]
product_data = make_xts(product_data)
product_data = tail(product_data, 135)
get_product_forecasts(product_data)
print(paste0("Product ID:", product_id))
print(paste0("Product Name: ", product_names[product_content_id == product_id]$bottom_hierarchy))

# Set Product Prediction
predictions[product_content_id == product_id]$forecast = 0
predictions

# Send Submission
#send_submission(predictions, token, url=subm_url, submit_now=F)




#-----------------BEGIN-----------------------
#Time series Cross Validation Denemeleri
#autoarima_forecast <- function(x, h){forecast(auto.arima(x), h=h)}

#e1<- tsCV(product_data$sold_count, ses, h=2)

#e1[3:nrow(e1),]=e1[1:(nrow(e1)-2),]
#e1[1:2,]=NA
#mape(product_data[3:nrow(e1)]$sold_count,(product_data[3:nrow(e1)]$sold_count+e1[3:nrow(e1),2]))
#accuracy((ts(product_data[3:nrow(e1)]$sold_count+e1[3:nrow(e1),2])), product_data[3:nrow(e1)]$sold_count)

#-----------------END--------------------------

