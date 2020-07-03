# install the required packages first
require("jsonlite")
require("httr")
require("data.table")
require("forecast")
require("ggplot2")
require("xts")
require("greybox")
require("foreach")
require("doParallel")

# Credentials
subm_url = 'http://167.172.183.67'
username = "username"
password = "password"
submit_now = FALSE

# Inputs
forecasted_product_number = 1
filter_last_n_days = as.numeric(as.Date(Sys.time()) - as.Date('2020-01-01'))
test_period = 2
parallel_processing_on = TRUE

get_token <- function(username, password, url_site){
  
  post_body = list(username=username,password=password)
  post_url_string = paste0(url_site,'/token/')
  result = POST(post_url_string, body = post_body)
  # error handling (wrong credentials)
  if(result$status_code==400){
    print('Check your credentials')
    return(0)
  }
  else if (result$status_code==429){
    print('Too many requests! Token cannot received!')
  }
  else if (result$status_code==201){
    output = content(result)
    token = output$key
    print('Token received successfully.')
  }
  
  return(token)
}

get_data <- function(start_date='2015-03-20', token, url_site){
  post_body = list(start_date=start_date,username=username,password=password)
  post_url_string = paste0(url_site,'/dataset/')
  
  header = add_headers(c(Authorization=paste('Token',token,sep=' ')))
  result = GET(post_url_string, header, body = post_body)
  output = content(result)
  data = data.table::rbindlist(output)
  data[,event_date:=as.Date(event_date)]
  data = data[order(product_content_id,event_date)]
  print("Data is loaded to the environment successfully.")
  csv_name = 'API DATA.csv'
  write.csv2(data, file = csv_name)
  data_last_updated_at <<- Sys.time()
  return(data)
}

send_submission <- function(predictions, token, url_site, submit_now=F){
  
  format_check=check_format(predictions)
  if(!format_check){
    return(FALSE)
  }
  
  post_string="list("
  for(i in 1:nrow(predictions)){
    post_string=sprintf("%s'%s'=%s",post_string,predictions$product_content_id[i],predictions$forecast[i])
    if(i<nrow(predictions)){
      post_string=sprintf("%s,",post_string)
    } else {
      post_string=sprintf("%s)",post_string)
    }
  }
  
  submission = eval(parse(text=post_string))
  json_body = jsonlite::toJSON(submission, auto_unbox = TRUE)
  submission=list(submission=json_body)
  
  print(submission)
  # {"31515569":2.4,"32939029":2.4,"4066298":2.4,"6676673":2.4,"7061886":2.4,"85004":2.4} 
  
  if(!submit_now){
    print("You did not submit.")
    return(FALSE)      
  }
  
  
  header = add_headers(c(Authorization=paste('Token',token,sep=' ')))
  post_url_string = paste0(url_site,'/submission/')
  result = POST(post_url_string, header, body=submission)
  
  if (result$status_code==201){
    print("Successfully submitted. Below you can see the details of your submission")
  } else {
    print("Could not submit. Please check the error message below, contact the assistant if needed.")
  }
  
  print(content(result))
  
}

check_format <- function(predictions){
  
  if(is.data.frame(predictions) | is.data.frame(predictions)){
    if(all(c('product_content_id','forecast') %in% names(predictions))){
      if(is.numeric(predictions$forecast)){
        print("Format OK")
        return(TRUE)
      } else {
        print("forecast information is not numeric")
        return(FALSE)                
      }
    } else {
      print("Wrong column names. Please provide 'product_content_id' and 'forecast' columns")
      return(FALSE)
    }
    
  } else {
    print("Wrong format. Please provide data.frame or data.table object")
    return(FALSE)
  }
}

# API Connection
# If data is updated in last 60 minutes, it uses offline data.
if (exists("data_last_updated_at")) {
  is_data_old = difftime(Sys.time(), data_last_updated_at, units = 'sec') > 60 * 60
} else {is_data_old = TRUE}
if (is_data_old) {
  token = get_token(username = username,password = password,url = subm_url)
  data_dt = get_data(token = token, url = subm_url)
}

# Functions
initialize_predictions = function (){
  if (!exists("predictions")) {
    predictions = unique(data_dt[, list(product_content_id)])
    predictions$product_content_id = as.integer(predictions$product_content_id)
    predictions[, forecast := 0]
    predictions = merge(predictions, product_names_dt[,c('product_content_id', 'bottom_hierarchy')], by='product_content_id', sort = F)
    print("Predictions are initialized")
    print(predictions)
  }
  return(predictions)
}

set_prediction = function(product_id, prediction){
  predictions = initialize_predictions()
  predictions[product_content_id == product_id]$forecast = prediction
  print(predictions)
  return(predictions)
}

set_default_prediction = function(product_id){
  if(exists('results')){
    results = results[order(results$ro_MAPE),]
    min_mape = results[1, 'ro_MAPE']
    if(min_mape < Inf){
      prediction = results[1, 'forecast']
    }
    else{
      results = results[order(results$ro_MAE),]
      prediction = results[1, 'forecast']
    }
    set_prediction(product_id, round(prediction))
  }
}

mape_quantile = function (actual, forecast) 
{
  if (length(actual) != length(forecast)) {
    message("The length of the provided data differs.")
    message(paste0("Length of actual: ", length(actual)))
    message(paste0("Length of forecast: ", length(forecast)))
    stop("Cannot proceed.", call. = FALSE)
  }
  else {
    p = quantile(abs((actual - forecast)/actual), na.rm = TRUE, probs = seq(0, 1, 0.25))
    return(c(p))
  }
}


# Product Analysis
product_names_dt = fread("API Products.csv")
# date_annotation_dt= data.table(sapply(read.csv('date_annotation.csv'), as.Date))
# data_dt[,"is_lock_down"] = as.numeric(data_dt$event_date %in% date_annotation_dt$lock_down_dates)

# Product Analysis
product_ids = unique(data_dt$product_content_id)
product_id = product_ids[forecasted_product_number]
product_data_dt = data_dt[product_content_id == product_id]
# Fix data for all products
product_data_dt[event_date == as.Date('2020-06-01'),] = product_data_dt[event_date == as.Date('2020-05-31'),]
product_data_dt[event_date == as.Date('2020-06-11'),] = product_data_dt[event_date == as.Date('2020-06-10'),]
if (product_id == 31515569){
  product_data_dt[event_date == as.Date('2020-05-17'),] = product_data_dt[event_date == as.Date('2020-05-16'),]
}
# Fix Islak Mendil data for unexpected sold count
if (product_id == 4066298){
  product_data_dt[event_date == as.Date('2020-05-21'),] = product_data_dt[event_date == as.Date('2020-05-22'),]
}

# Imporevement of data for regression
product_data_dt[,lag1_sold_count:=shift(sold_count,1)]
product_data_dt[,lag7_sold_count:=shift(sold_count,7)]
product_data_dt[,lag28_sold_count:=shift(sold_count,28)]
product_data_dt[,lag365_sold_count:=shift(sold_count,365)]

product_data_dt[, 'is_after_corona'] = c(0)
product_data_dt[event_date >= as.Date('2020-03-12')]$is_after_corona = c(1)

# Firstly fills NA prices to last observation, then first price is set for the prior NA prices.
product_data_dt[price == -1]$price = c(NA)
product_data_dt$price = na.locf(product_data_dt$price, na.rm = FALSE)
product_data_dt$price = na.locf(product_data_dt$price, na.rm = FALSE, fromLast = TRUE)

data_seq <- seq(from = first(product_data_dt$event_date),
                last(product_data_dt$event_date),
                by = "days")

data_seq1 <-
  seq(
    from = first(product_data_dt[product_data_dt$sold_count != 0, ]$event_date),
    last(product_data_dt$event_date),
    by = "days"
  )

x1 <- (length(data_seq) - length(data_seq1))
product_data_dt <- product_data_dt[(x1 + 1):length(data_seq),]
product_data_dt$event_date <- NULL
product_data_dt$product_content_id <- NULL
product_data_dt <- data.table(sapply(product_data_dt, as.numeric))
# product_data$is_after_corona = as.factor(product_data$is_after_corona)
# product_data$is_lock_down = as.factor(product_data$is_lock_down)
product_data_dt[is.na(product_data_dt)] <- 0
product_data_xts <- xts(product_data_dt, order.by = data_seq1)

product_data_xts=tail(product_data_xts,filter_last_n_days)
print(paste0("Last event date: ", tail(data_dt, 1)$event_date))
print(paste0("Product ID: ", product_id))
product_name = product_names_dt[product_content_id == product_id]$bottom_hierarchy
print(paste0("Product Name: ", product_name))
# browseURL(toString(product_names_dt[product_content_id == product_id]$url))

# product forecasts
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

# Rolling Origin
ro_MAPE <- rep(NA, length(method_names))
ro_MAE <- rep(NA, length(method_names))
ro_MAPE_25 <- rep(NA, length(method_names))
ro_MAPE_50 <- rep(NA, length(method_names))
ro_MAPE_75 <- rep(NA, length(method_names))

# Common parameters
ourValue = "mean"
ourTs = ts(product_data_xts$sold_count,frequency = 7)
ourOrigins = nrow(product_data_xts)*0.20

index=1
ourCall<-"naive(x=data, h=h)"
ro_obj <- ro(ourTs, h=2, origins=ourOrigins, call=ourCall, value=ourValue, ci=FALSE, co=TRUE, parallel=parallel_processing_on)
plot(ro_obj, main = paste0("Rolling Origin - ", method_names[index]))
ro_MAPE[index] <- MAPE(ro_obj$holdout[2,],ro_obj$mean[2,])
ro_MAE[index] <- MAE(ro_obj$holdout[2,],ro_obj$mean[2,])
ro_MAPE_25[index] <- mape_quantile(ro_obj$holdout[2,],ro_obj$mean[2,])[2]
ro_MAPE_50[index] <- mape_quantile(ro_obj$holdout[2,],ro_obj$mean[2,])[3]
ro_MAPE_75[index] <- mape_quantile(ro_obj$holdout[2,],ro_obj$mean[2,])[4]

index=2
ourCall<-"meanf(x=data, h=h)"
ro_obj <- ro(ourTs, h=2, origins=ourOrigins, call=ourCall, value=ourValue, ci=FALSE, co=TRUE, parallel=parallel_processing_on)
plot(ro_obj, main = paste0("Rolling Origin - ", method_names[index]))
ro_MAPE[index] <- MAPE(ro_obj$holdout[2,],ro_obj$mean[2,])
ro_MAE[index] <- MAE(ro_obj$holdout[2,],ro_obj$mean[2,])
ro_MAPE_25[index] <- mape_quantile(ro_obj$holdout[2,],ro_obj$mean[2,])[2]
ro_MAPE_50[index] <- mape_quantile(ro_obj$holdout[2,],ro_obj$mean[2,])[3]
ro_MAPE_75[index] <- mape_quantile(ro_obj$holdout[2,],ro_obj$mean[2,])[4]

index=3
ourCall<-"forecast(HoltWinters(x=data, seasonal = 'additive'), h=h)"
ro_obj <- ro(ourTs, h=2, origins=ourOrigins, call=ourCall, value=ourValue, ci=FALSE, co=TRUE, parallel=parallel_processing_on)
plot(ro_obj, main = paste0("Rolling Origin - ", method_names[index]))
ro_MAPE[index] <- MAPE(ro_obj$holdout[2,],ro_obj$mean[2,])
ro_MAE[index] <- MAE(ro_obj$holdout[2,],ro_obj$mean[2,])
ro_MAPE_25[index] <- mape_quantile(ro_obj$holdout[2,],ro_obj$mean[2,])[2]
ro_MAPE_50[index] <- mape_quantile(ro_obj$holdout[2,],ro_obj$mean[2,])[3]
ro_MAPE_75[index] <- mape_quantile(ro_obj$holdout[2,],ro_obj$mean[2,])[4]

index=4
if (sum(ts(product_data_xts$sold_count , frequency = 7)[, 1] == 0) == 0) {
  ourCall<-"forecast(HoltWinters(x=data, seasonal = 'multiplicative'), h=h)"
  ro_obj <- ro(ourTs, h=2, origins=ourOrigins, call=ourCall, value=ourValue, ci=FALSE, co=TRUE, parallel=parallel_processing_on)
  plot(ro_obj, main = paste0("Rolling Origin - ", method_names[index]))
  ro_MAPE[index] <- MAPE(ro_obj$holdout[2,],ro_obj$mean[2,])
  ro_MAE[index] <- MAE(ro_obj$holdout[2,],ro_obj$mean[2,])
  ro_MAPE_25[index] <- mape_quantile(ro_obj$holdout[2,],ro_obj$mean[2,])[2]
  ro_MAPE_50[index] <- mape_quantile(ro_obj$holdout[2,],ro_obj$mean[2,])[3]
  ro_MAPE_75[index] <- mape_quantile(ro_obj$holdout[2,],ro_obj$mean[2,])[4]
}else {
  ro_MAPE[index] <- NA
  ro_MAE[index] <- MAE(ro_obj$holdout[2,],ro_obj$mean[2,])
  ro_MAPE_25[index] <- mape_quantile(ro_obj$holdout[2,],ro_obj$mean[2,])[2]
  ro_MAPE_50[index] <- mape_quantile(ro_obj$holdout[2,],ro_obj$mean[2,])[3]
  ro_MAPE_75[index] <- mape_quantile(ro_obj$holdout[2,],ro_obj$mean[2,])[4]
}

index=5
ourCall<-"ses(x=data, h=h)"
ro_obj <- ro(ourTs, h=2, origins=ourOrigins, call=ourCall, value=ourValue, ci=FALSE, co=TRUE, parallel=parallel_processing_on)
plot(ro_obj, main = paste0("Rolling Origin - ", method_names[index]))
ro_MAPE[index] <- MAPE(ro_obj$holdout[2,],ro_obj$mean[2,])
ro_MAE[index] <- MAE(ro_obj$holdout[2,],ro_obj$mean[2,])
ro_MAPE_25[index] <- mape_quantile(ro_obj$holdout[2,],ro_obj$mean[2,])[2]
ro_MAPE_50[index] <- mape_quantile(ro_obj$holdout[2,],ro_obj$mean[2,])[3]
ro_MAPE_75[index] <- mape_quantile(ro_obj$holdout[2,],ro_obj$mean[2,])[4]

index=6
ourCall<-"forecast(auto.arima(x=data), h=h)"
ro_obj <- ro(ourTs, h=2, origins=ourOrigins, call=ourCall, value=ourValue, ci=FALSE, co=TRUE, parallel=parallel_processing_on)
plot(ro_obj, main = paste0("Rolling Origin - ", method_names[index]))
ro_MAPE[index] <- MAPE(ro_obj$holdout[2,],ro_obj$mean[2,])
ro_MAE[index] <- MAE(ro_obj$holdout[2,],ro_obj$mean[2,])
ro_MAPE_25[index] <- mape_quantile(ro_obj$holdout[2,],ro_obj$mean[2,])[2]
ro_MAPE_50[index] <- mape_quantile(ro_obj$holdout[2,],ro_obj$mean[2,])[3]
ro_MAPE_75[index] <- mape_quantile(ro_obj$holdout[2,],ro_obj$mean[2,])[4]

index=7
ourCall<-"forecast(tbats(y=data), h=h)"
ro_obj <- ro(ourTs, h=2, origins=ourOrigins, call=ourCall, value=ourValue, ci=FALSE, co=TRUE, parallel=parallel_processing_on)
plot(ro_obj, main = paste0("Rolling Origin - ", method_names[index]))
ro_MAPE[index] <- MAPE(ro_obj$holdout[2,],ro_obj$mean[2,])
ro_MAE[index] <- MAE(ro_obj$holdout[2,],ro_obj$mean[2,])
ro_MAPE_25[index] <- mape_quantile(ro_obj$holdout[2,],ro_obj$mean[2,])[2]
ro_MAPE_50[index] <- mape_quantile(ro_obj$holdout[2,],ro_obj$mean[2,])[3]
ro_MAPE_75[index] <- mape_quantile(ro_obj$holdout[2,],ro_obj$mean[2,])[4]

index=11
ourCall<-"forecast(auto.arima(x=data, lambda='auto'), h=h)"
ro_obj <- ro(ourTs, h=2, origins=ourOrigins, call=ourCall, value=ourValue, ci=FALSE, co=TRUE, parallel=parallel_processing_on)
plot(ro_obj, main = paste0("Rolling Origin - ", method_names[index]))
ro_MAPE[index] <- MAPE(ro_obj$holdout[2,],ro_obj$mean[2,])
ro_MAE[index] <- MAE(ro_obj$holdout[2,],ro_obj$mean[2,])
ro_MAPE_25[index] <- mape_quantile(ro_obj$holdout[2,],ro_obj$mean[2,])[2]
ro_MAPE_50[index] <- mape_quantile(ro_obj$holdout[2,],ro_obj$mean[2,])[3]
ro_MAPE_75[index] <- mape_quantile(ro_obj$holdout[2,],ro_obj$mean[2,])[4]

index=12
ourCall<-"forecast(nnetar(y=data, lambda='auto'), h=h)"
ro_obj <- ro(ourTs, h=2, origins=ourOrigins, call=ourCall, value=ourValue, ci=FALSE, co=TRUE, parallel=parallel_processing_on)
plot(ro_obj, main = paste0("Rolling Origin - ", method_names[index]))
ro_MAPE[index] <- MAPE(ro_obj$holdout[2,],ro_obj$mean[2,])
ro_MAE[index] <- MAE(ro_obj$holdout[2,],ro_obj$mean[2,])
ro_MAPE_25[index] <- mape_quantile(ro_obj$holdout[2,],ro_obj$mean[2,])[2]
ro_MAPE_50[index] <- mape_quantile(ro_obj$holdout[2,],ro_obj$mean[2,])[3]
ro_MAPE_75[index] <- mape_quantile(ro_obj$holdout[2,],ro_obj$mean[2,])[4]

index=13
ourCall<-"ses(x=data, h=h, lambda='auto')"
ro_obj <- ro(ourTs, h=2, origins=ourOrigins, call=ourCall, value=ourValue, ci=FALSE, co=TRUE, parallel=parallel_processing_on)
plot(ro_obj, main = paste0("Rolling Origin - ", method_names[index]))
ro_MAPE[index] <- MAPE(ro_obj$holdout[2,],ro_obj$mean[2,])
ro_MAE[index] <- MAE(ro_obj$holdout[2,],ro_obj$mean[2,])
ro_MAPE_25[index] <- mape_quantile(ro_obj$holdout[2,],ro_obj$mean[2,])[2]
ro_MAPE_50[index] <- mape_quantile(ro_obj$holdout[2,],ro_obj$mean[2,])[3]
ro_MAPE_75[index] <- mape_quantile(ro_obj$holdout[2,],ro_obj$mean[2,])[4]

results <- as.data.frame(cbind(results, ro_MAPE,ro_MAE, ro_MAPE_25, ro_MAPE_50, ro_MAPE_75))
print("Rolling Origin Done!")

results = results[order(results$ro_MAPE),]
predictions = set_default_prediction(product_id)

View(results)

# Set Product Prediction
predictions = set_prediction(product_id = product_id, prediction = 1)

# Send Submission
# send_submission(predictions, token, url=subm_url, submit_now=F)