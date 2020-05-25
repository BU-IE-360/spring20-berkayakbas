# 0. Initialization of vectors
method_names = c(
  'Naive',
  'Mean',
  'HW Additive',
  'HW Multiplicative',
  'Exponential Smoothing',
  'Auto Arima',
  'TBATS',
  'Linear Regression',
  'Stepwise Backward',
  'Stepwise Forward',
  'Auto Arima (lambda=auto)',
  'Neural Network (lambda=auto)',
  'Exponential Smoothing (lambda=auto)'
)
low_80 <- rep(NA, length(method_names))
low_95 <- rep(NA, length(method_names))
forecast <- rep(NA, length(method_names))
high_80 <- rep(NA, length(method_names))
high_95 <- rep(NA, length(method_names))
ADJ_R2<- rep(NA, length(method_names))
ME <- rep(NA, length(method_names))
RMSE <- rep(NA, length(method_names))
MAE <- rep(NA, length(method_names))
MAPE <- rep(NA, length(method_names))
MASE <- rep(NA, length(method_names))
ACF1 <- rep(NA, length(method_names))

#set test metrics
test_ADJ_R2<- rep(NA, length(method_names))
test_ME <- rep(NA, length(method_names))
test_RMSE <- rep(NA, length(method_names))
test_MAE <- rep(NA, length(method_names))
test_MAPE <- rep(NA, length(method_names))
test_MASE <- rep(NA, length(method_names))


#get train and test subsets of data
if(test_period==1){
  train_data_xts<-product_data_xts[1:(nrow(product_data_xts)*0.8),]
  test_data_xts<-product_data_xts[(nrow(train_data_xts)+1):nrow(product_data_xts),]
  testing_for_newdata_xts<-product_data_xts[(nrow(train_data_xts)-nrow(test_data_xts)+1):(nrow(train_data_xts)),]
  index(testing_for_newdata_xts)<-index(testing_for_newdata_xts)+2
} else{
  train_data_xts<-product_data_xts[1:(nrow(product_data_xts)-2),]
  test_data_xts<-product_data_xts[(nrow(product_data_xts)-1):nrow(product_data_xts),]
  testing_for_newdata_xts<-product_data_xts[(nrow(train_data_xts)-1):(nrow(train_data_xts)),]
  index(testing_for_newdata_xts)<-index(testing_for_newdata_xts)+2
}
# 1. Naive
index = 1
model = naive(product_data_xts$sold_count, h = 2)
forecast[index] <- as.numeric(model$mean[2])
accuracy = accuracy(model)
ME[index] <- accuracy[, 'ME']
RMSE[index] <- accuracy[, 'RMSE']
MAE[index] <- accuracy[, 'MAE']
MAPE[index] <- accuracy[, 'MAPE']
MASE[index] <- accuracy[, 'MASE']
ACF1[index] <- accuracy[, 'ACF1']

#1.1 Naive Testing
model=naive(train_data_xts$sold_count, h = nrow(test_data_xts))
accuracy=accuracy(model, test_data_xts$sold_count)
test_ME[index] <- accuracy["Test set", 'ME']
test_RMSE[index] <- accuracy["Test set", 'RMSE']
test_MAE[index] <- accuracy["Test set", 'MAE']
test_MAPE[index] <- accuracy["Test set", 'MAPE']
test_MASE[index] <- accuracy["Test set", 'MASE']

preds_1<- xts(model$mean, order.by= seq(first(index(test_data_xts)),last(index(test_data_xts)), by="days"))
print(plot(c(train_data_xts$sold_count,test_data_xts$sold_count), main = paste0(method_names[index])))
print(lines(preds_1, col = "red"))

# 2. Mean Forecast
index = 2
model = meanf(product_data_xts$sold_count, h = 2)
forecast[index] <- as.numeric(model$mean[2])
accuracy = accuracy(model)
ME[index] <- accuracy[, 'ME']
RMSE[index] <- accuracy[, 'RMSE']
MAE[index] <- accuracy[, 'MAE']
MAPE[index] <- accuracy[, 'MAPE']
MASE[index] <- accuracy[, 'MASE']
ACF1[index] <- NA

#2.1. Mean Testing
model=meanf(train_data_xts$sold_count, h = nrow(test_data_xts))
accuracy=accuracy(model, test_data_xts$sold_count)
test_ME[index] <- accuracy["Test set", 'ME']
test_RMSE[index] <- accuracy["Test set", 'RMSE']
test_MAE[index] <- accuracy["Test set", 'MAE']
test_MAPE[index] <- accuracy["Test set", 'MAPE']
test_MASE[index] <- accuracy["Test set", 'MASE']

preds_1<- xts(model$mean, order.by= seq(first(index(test_data_xts)),last(index(test_data_xts)), by="days"))
print(plot(c(train_data_xts$sold_count,test_data_xts$sold_count), main = paste0(method_names[index])))
print(lines(preds_1, col = "red"))

# 3. Holt Winters Additive
index = 3
model = HoltWinters(ts(product_data_xts$sold_count , frequency = 7)[, ], seasonal = "additive")
fr = forecast(model, h = 2)
low_80[index] <- fr$lower[2, 1]
low_95[index] <- fr$lower[2, 2]
forecast[index] <- fr$mean[2]
high_80[index] <- fr$upper[2, 1]
high_95[index] <- fr$upper[2, 2]
accuracy = accuracy(forecast(model, h=2))
ME[index] <- accuracy[, 'ME']
RMSE[index] <- accuracy[, 'RMSE']
MAE[index] <- accuracy[, 'MAE']
MAPE[index] <- accuracy[, 'MAPE']
MASE[index] <- accuracy[, 'MASE']
ACF1[index] <- accuracy[, 'ACF1']

#3.1 HW Add Testing

model=HoltWinters(ts(train_data_xts$sold_count , frequency = 7)[, ], seasonal = "additive")
accuracy = accuracy(forecast(model, h=nrow(test_data_xts)), test_data_xts$sold_count)
test_ME[index] <- accuracy["Test set", 'ME']
test_RMSE[index] <- accuracy["Test set", 'RMSE']
test_MAE[index] <- accuracy["Test set", 'MAE']
test_MAPE[index] <- accuracy["Test set", 'MAPE']
test_MASE[index] <- accuracy["Test set", 'MASE']

preds_1<- xts(forecast(model, h=nrow(test_data_xts))$mean, order.by= seq(first(index(test_data_xts)),last(index(test_data_xts)), by="days"))
print(plot(c(train_data_xts$sold_count,test_data_xts$sold_count), main = paste0(method_names[index])))
print(lines(preds_1, col = "red"))

# 4. Holt Winters Multiplicative
index=4
if (sum(ts(product_data_xts$sold_count , frequency = 7)[, 1] == 0) == 0) {
  
  model = HoltWinters(ts(product_data_xts$sold_count , frequency = 7)[, ], seasonal = "multiplicative")
  fr = forecast(model, h = 2)
  low_80[index] <- fr$lower[2, 1]
  low_95[index] <- fr$lower[2, 2]
  forecast[index] <- fr$mean[2]
  high_80[index] <- fr$upper[2, 1]
  high_95[index] <- fr$upper[2, 2]
  accuracy = accuracy(forecast(model, h=2))
  ME[index] <- accuracy[, 'ME']
  RMSE[index] <- accuracy[, 'RMSE']
  MAE[index] <- accuracy[, 'MAE']
  MAPE[index] <- accuracy[, 'MAPE']
  MASE[index] <- accuracy[, 'MASE']
  ACF1[index] <- accuracy[, 'ACF1']
  
  #HW Mult testing
  model=HoltWinters(ts(train_data_xts$sold_count , frequency = 7)[, ], seasonal = "multiplicative")
  accuracy = accuracy(forecast(model, h=nrow(test_data_xts)), test_data_xts$sold_count)
  test_ME[index] <- accuracy["Test set", 'ME']
  test_RMSE[index] <- accuracy["Test set", 'RMSE']
  test_MAE[index] <- accuracy["Test set", 'MAE']
  test_MAPE[index] <- accuracy["Test set", 'MAPE']
  test_MASE[index] <- accuracy["Test set", 'MASE']
  preds_1<- xts(forecast(model, h=nrow(test_data_xts))$mean, order.by= seq(first(index(test_data_xts)),last(index(test_data_xts)), by="days"))
  print(plot(c(train_data_xts$sold_count,test_data_xts$sold_count), main = paste0(method_names[index])))
  print(lines(preds_1, col = "red"))
}else {
  forecast[4] <- NA
}

# 5. Exponential Smoothing
index = 5
model = ses(product_data_xts$sold_count, h = 2)
fr = forecast(model, h = 2)
low_80[index] <- fr$lower[2, 1]
low_95[index] <- fr$lower[2, 2]
forecast[index] <- fr$mean[2]
high_80[index] <- fr$upper[2, 1]
high_95[index] <- fr$upper[2, 2]
accuracy = accuracy(model)
ME[index] <- accuracy[, 'ME']
RMSE[index] <- accuracy[, 'RMSE']
MAE[index] <- accuracy[, 'MAE']
MAPE[index] <- accuracy[, 'MAPE']
MASE[index] <- accuracy[, 'MASE']
ACF1[index] <- accuracy[, 'ACF1']

#5.1 Exp Smt Testing
model=ses(train_data_xts$sold_count, h = nrow(test_data_xts))
accuracy=accuracy(model, test_data_xts$sold_count)
test_ME[index] <- accuracy["Test set", 'ME']
test_RMSE[index] <- accuracy["Test set", 'RMSE']
test_MAE[index] <- accuracy["Test set", 'MAE']
test_MAPE[index] <- accuracy["Test set", 'MAPE']
test_MASE[index] <- accuracy["Test set", 'MASE']

preds_1<- xts(model$mean, order.by= seq(first(index(test_data_xts)),last(index(test_data_xts)), by="days"))
print(plot(c(train_data_xts$sold_count,test_data_xts$sold_count), main = paste0(method_names[index])))
print(lines(preds_1, col = "red"))

# 6. Auto Arima
index = 6
model = auto.arima(product_data_xts$sold_count)
fr = forecast(model, h = 2)
low_80[index] <- fr$lower[2, 1]
low_95[index] <- fr$lower[2, 2]
forecast[index] <- fr$mean[2]
high_80[index] <- fr$upper[2, 1]
high_95[index] <- fr$upper[2, 2]
accuracy = accuracy(model)
ME[index] <- accuracy[, 'ME']
RMSE[index] <- accuracy[, 'RMSE']
MAE[index] <- accuracy[, 'MAE']
MAPE[index] <- accuracy[, 'MAPE']
MASE[index] <- accuracy[, 'MASE']
ACF1[index] <- accuracy[, 'ACF1']

# 6.1 Auto Arima Testing
model=auto.arima(train_data_xts$sold_count)
accuracy=accuracy(forecast(model, h = nrow(test_data_xts)), test_data_xts$sold_count)
test_ME[index] <- accuracy["Test set", 'ME']
test_RMSE[index] <- accuracy["Test set", 'RMSE']
test_MAE[index] <- accuracy["Test set", 'MAE']
test_MAPE[index] <- accuracy["Test set", 'MAPE']
test_MASE[index] <- accuracy["Test set", 'MASE']
preds_1<- xts(forecast(model, h = nrow(test_data_xts))$mean, order.by= seq(first(index(test_data_xts)),last(index(test_data_xts)), by="days"))
print(plot(c(train_data_xts$sold_count,test_data_xts$sold_count), main = paste0(method_names[index])))
print(lines(preds_1, col = "red"))

# 7. TBATS
index = 7
model = tbats(ts(product_data_xts$sold_count), use.parallel = parallel_processing_on)
fr = forecast(model, h = 2)
low_80[index] <- fr$lower[2, 1]
low_95[index] <- fr$lower[2, 2]
forecast[index] <- fr$mean[2]
high_80[index] <- fr$upper[2, 1]
high_95[index] <- fr$upper[2, 2]
accuracy = accuracy(model)
ME[index] <- accuracy[, 'ME']
RMSE[index] <- accuracy[, 'RMSE']
MAE[index] <- accuracy[, 'MAE']
MAPE[index] <- accuracy[, 'MAPE']
MASE[index] <- accuracy[, 'MASE']
ACF1[index] <- accuracy[, 'ACF1']

#7.1 TBATS Testing
model=tbats(ts(train_data_xts$sold_count), use.parallel = parallel_processing_on)
accuracy=accuracy(forecast(model, h = nrow(test_data_xts)), test_data_xts$sold_count)
test_ME[index] <- accuracy["Test set", 'ME']
test_RMSE[index] <- accuracy["Test set", 'RMSE']
test_MAE[index] <- accuracy["Test set", 'MAE']
test_MAPE[index] <- accuracy["Test set", 'MAPE']
test_MASE[index] <- accuracy["Test set", 'MASE']
preds_1<- xts(forecast(model, h = nrow(test_data_xts))$mean, order.by= seq(first(index(test_data_xts)),last(index(test_data_xts)), by="days"))
print(plot(c(train_data_xts$sold_count,test_data_xts$sold_count), main = paste0(method_names[index])))
print(lines(preds_1, col = "red"))

# 8. Linear Regression
index= 8
lm_model_1<-lm(sold_count~ visit_count+favored_count+basket_count, data=product_data_xts)
newdata_f<-product_data_xts[(nrow(product_data_xts)-1):nrow(product_data_xts), c("visit_count","favored_count", "basket_count")]
index(newdata_f)<-(index(newdata_f)+2)
preds<-predict(lm_model_1, newdata = newdata_f)
forecast[index] <- preds[2]
ADJ_R2[index]<-summary(lm_model_1)$adj.r.squared
MAE[index] <-MAE(lm_model_1$fitted.values,product_data_xts$sold_count)
RMSE[index] <- sqrt(MSE(lm_model_1$fitted.values,product_data_xts$sold_count))
MAPE[index] <- MAPE(product_data_xts$sold_count, lm_model_1$fitted.values)

# 8.1 Linear Regression Testing
lm_model_1<-lm(sold_count~ visit_count+favored_count+basket_count, data=train_data_xts)
preds<-predict(lm_model_1, newdata = testing_for_newdata_xts)
test_ADJ_R2[index]<-summary(lm_model_1)$adj.r.squared
test_MAE[index] <-MAE(preds,test_data_xts$sold_count)
test_RMSE[index] <- sqrt(MSE(preds,test_data_xts$sold_count))
test_MAPE[index] <- MAPE(test_data_xts$sold_count, preds)

preds_1<- xts(preds, order.by= seq(first(index(test_data_xts)),last(index(test_data_xts)), by="days"))
print(plot(c(train_data_xts$sold_count,test_data_xts$sold_count), main = paste0(method_names[index])))
print(lines(preds_1, col = "red"))

# Stepwise Regression
product_data_regression_xts = product_data_xts
product_data_regression_xts$is_after_corona = as.factor(product_data_regression_xts$is_after_corona)
train_data_regression_xts = train_data_xts
train_data_regression_xts$is_after_corona = as.factor(train_data_regression_xts$is_after_corona)

null=lm(sold_count ~ 1, data=product_data_regression_xts)
full=lm(sold_count ~ ., data=product_data_regression_xts)
backward_lr = step(full, scope=list(lower=null, upper=full), direction="backward", trace = 0)
forward_lr = step(null, scope=list(lower=null, upper=full), direction="forward", trace = 0)

null_test=lm(sold_count ~ 1, data=train_data_regression_xts)
full_test=lm(sold_count ~ ., data=train_data_regression_xts)
backward_lr_test = step(full, scope=list(lower=null, upper=full), direction="backward", trace = 0)
forward_lr_test = step(null, scope=list(lower=null, upper=full), direction="forward", trace = 0)

# 9. Stepwise Regression - Backward
index = 9
newdata_f<-product_data_regression_xts[(nrow(product_data_regression_xts)-1):nrow(product_data_regression_xts),]
index(newdata_f)<-(index(newdata_f)+2)
preds<-predict(backward_lr, newdata = newdata_f)
forecast[index] <- preds[2]
ADJ_R2[index]<-summary(backward_lr)$adj.r.squared
MAE[index] <-MAE(backward_lr$fitted.values,product_data_regression_xts$sold_count)
RMSE[index] <- sqrt(MSE(backward_lr$fitted.values,product_data_regression_xts$sold_count))
MAPE[index] <- MAPE(product_data_regression_xts$sold_count, backward_lr$fitted.values)

#9.1 Step Backward -Test
preds<-predict(backward_lr_test, newdata = testing_for_newdata_xts)
test_ADJ_R2[index]<-summary(backward_lr_test)$adj.r.squared
test_MAE[index] <-MAE(preds,test_data_xts$sold_count)
test_RMSE[index] <- sqrt(MSE(preds,test_data_xts$sold_count))
test_MAPE[index] <- MAPE(test_data_xts$sold_count, preds)

preds_1<- xts(preds, order.by= seq(first(index(test_data_xts)),last(index(test_data_xts)), by="days"))
print(plot(c(train_data_regression_xts$sold_count,test_data_xts$sold_count), main = paste0(method_names[index])))
print(lines(preds_1, col = "red"))

# 10. Stepwise Regression - Forward
index = 10
newdata_f<-product_data_regression_xts[(nrow(product_data_regression_xts)-1):nrow(product_data_regression_xts),]
index(newdata_f)<-(index(newdata_f)+2)
preds<-predict(forward_lr, newdata = newdata_f)
forecast[index] <- preds[2]
ADJ_R2[index]<-summary(forward_lr)$adj.r.squared
MAE[index] <-MAE(forward_lr$fitted.values,product_data_regression_xts$sold_count)
RMSE[index] <- sqrt(MSE(forward_lr$fitted.values,product_data_regression_xts$sold_count))
MAPE[index] <- MAPE(product_data_regression_xts$sold_count, forward_lr$fitted.values)

#10.1 Step Forward Test
preds<-predict(forward_lr_test, newdata = testing_for_newdata_xts)
test_ADJ_R2[index]<-summary(forward_lr_test)$adj.r.squared
test_MAE[index] <-MAE(preds,test_data_xts$sold_count)
test_RMSE[index] <- sqrt(MSE(preds,test_data_xts$sold_count))
test_MAPE[index] <- MAPE(test_data_xts$sold_count, preds)

preds_1<- xts(preds, order.by= seq(first(index(test_data_xts)),last(index(test_data_xts)), by="days"))
print(plot(c(train_data_regression_xts$sold_count,test_data_xts$sold_count), main = paste0(method_names[index])))
print(lines(preds_1, col = "red"))

# 11. Auto Arima with Auto Transformation
index = 11
model = auto.arima(product_data_xts$sold_count, lambda = 'auto')
fr = forecast(model, h = 2)
low_80[index] <- fr$lower[2, 1]
low_95[index] <- fr$lower[2, 2]
forecast[index] <- fr$mean[2]
high_80[index] <- fr$upper[2, 1]
high_95[index] <- fr$upper[2, 2]
accuracy = accuracy(model)
ME[index] <- accuracy[, 'ME']
RMSE[index] <- accuracy[, 'RMSE']
MAE[index] <- accuracy[, 'MAE']
MAPE[index] <- accuracy[, 'MAPE']
MASE[index] <- accuracy[, 'MASE']
ACF1[index] <- accuracy[, 'ACF1']

# 11.1 Auto Arima with Auto Transformation Testing
model=auto.arima(train_data_xts$sold_count, lambda = 'auto')
accuracy=accuracy(forecast(model, h = nrow(test_data_xts)), test_data_xts$sold_count)
test_ME[index] <- accuracy["Test set", 'ME']
test_RMSE[index] <- accuracy["Test set", 'RMSE']
test_MAE[index] <- accuracy["Test set", 'MAE']
test_MAPE[index] <- accuracy["Test set", 'MAPE']
test_MASE[index] <- accuracy["Test set", 'MASE']
preds_1<- xts(forecast(model, h = nrow(test_data_xts))$mean, order.by= seq(first(index(test_data_xts)),last(index(test_data_xts)), by="days"))
print(plot(c(train_data_xts$sold_count,test_data_xts$sold_count), main = paste0(method_names[index])))
print(lines(preds_1, col = "red"))

# 12. Neural Network
index = 12
model = nnetar(product_data_xts$sold_count, lambda = 'auto')
fr = forecast(model, h = 2)
forecast[index] <- fr$mean[2]
accuracy = accuracy(model)
ME[index] <- accuracy[, 'ME']
RMSE[index] <- accuracy[, 'RMSE']
MAE[index] <- accuracy[, 'MAE']
MAPE[index] <- accuracy[, 'MAPE']
MASE[index] <- accuracy[, 'MASE']
ACF1[index] <- accuracy[, 'ACF1']

# 12.1 Neural Network Testing
model= nnetar(train_data_xts$sold_count, lambda = 'auto')
accuracy=accuracy(forecast(model, h = nrow(test_data_xts)), test_data_xts$sold_count)
test_ME[index] <- accuracy["Test set", 'ME']
test_RMSE[index] <- accuracy["Test set", 'RMSE']
test_MAE[index] <- accuracy["Test set", 'MAE']
test_MAPE[index] <- accuracy["Test set", 'MAPE']
test_MASE[index] <- accuracy["Test set", 'MASE']
preds_1<- xts(forecast(model, h = nrow(test_data_xts))$mean, order.by= seq(first(index(test_data_xts)),last(index(test_data_xts)), by="days"))
print(plot(c(train_data_xts$sold_count,test_data_xts$sold_count), main = paste0(method_names[index])))
print(lines(preds_1, col = "red"))

# 13. Exponential Smoothing - Auto Transformation
index = 13
model = ses(product_data_xts$sold_count, h = 2, lambda = 'auto')
fr = forecast(model, h = 2)
low_80[index] <- fr$lower[2, 1]
low_95[index] <- fr$lower[2, 2]
forecast[index] <- fr$mean[2]
high_80[index] <- fr$upper[2, 1]
high_95[index] <- fr$upper[2, 2]
accuracy = accuracy(model)
ME[index] <- accuracy[, 'ME']
RMSE[index] <- accuracy[, 'RMSE']
MAE[index] <- accuracy[, 'MAE']
MAPE[index] <- accuracy[, 'MAPE']
MASE[index] <- accuracy[, 'MASE']
ACF1[index] <- accuracy[, 'ACF1']

# 13.1 Exponential Smoothing - Auto Transformation Testing
model=ses(train_data_xts$sold_count, h = nrow(test_data_xts), lambda = 'auto')
accuracy=accuracy(model, test_data_xts$sold_count)
test_ME[index] <- accuracy["Test set", 'ME']
test_RMSE[index] <- accuracy["Test set", 'RMSE']
test_MAE[index] <- accuracy["Test set", 'MAE']
test_MAPE[index] <- accuracy["Test set", 'MAPE']
test_MASE[index] <- accuracy["Test set", 'MASE']
preds_1<- xts(model$mean, order.by= seq(first(index(test_data_xts)),last(index(test_data_xts)), by="days"))
print(plot(c(train_data_xts$sold_count,test_data_xts$sold_count), main = paste0(method_names[index])))
print(lines(preds_1, col = "red"))

columns = cbind(low_95, low_80, forecast, high_80, high_95, ADJ_R2,MAE, MAPE, MASE, test_ADJ_R2, test_MAE, test_MAPE, test_MASE)
# columns = cbind(forecast, ADJ_R2, ME, RMSE, MAE, MAPE, MASE, 
#                 ACF1,test_ADJ_R2, test_ME, test_RMSE,
#                 test_MAE,test_MAPE,test_MASE)

results <- as.data.frame(columns)
row.names(results) <- method_names
print("Product Forecasts Done!")