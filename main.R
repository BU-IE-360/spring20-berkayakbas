source("api_connection.R")

# Requirements
require("forecast")
require("ggplot2")
require("xts")

make_xts_withsales <-function(product1) {
  data_seq <- seq(from = first(product1$event_date),
                  last(product1$event_date),
                  by = "days")
  
  data_seq1 <-
    seq(from = first(product1[product1$sold_count != 0,]$event_date),
        last(product1$event_date),
        by = "days")
  
  x1 <- (length(data_seq) - length(data_seq1))
  product1 <- product1[(x1 + 1):length(data_seq),]
  product1$event_date <- NULL
  product1$product_content_id <- NULL
  product1 <- sapply(product1, as.numeric)
  product1 <- xts(product1, order.by = data_seq1)
  return(product1)
}

# 1. For loop ile başlangıç, her bir product için ayrı datanın hazırlanması @BERKAY
# 2. make_xts_withsales ile bu dataların xtse dönüştürülmesi @BERKAY
# 3. xts datasının farklı methodlar ile incelenmesi @BERKAY
# Her bir method için;
    # input product datası, 
    # outputs 
    # 1. point forecast (h=1) + plot
    # 2. AIC, BIC accuracy, chechresiduals ()
    # 3. data.table ile AIC, BIC ve point forecast'in return edilmesi

#   3.1 Naive method @BERKAY
#   3.2 Linear Regression @BERKAY
#   3.3.1 Holt Winters Additive @BBG
#   3.3.2 Holt Winters Multiplicative @BBG
#   3.4 Exponential Smoothing @BERKAY
#   3.5 Arima @BBG
#   3.6 Mean @BBG


returnBBGforecasts<- function (product){
  BBGforecasts<-vector("numeric", 4)
  
  BBGforecasts[1]<-as.numeric(naive(product$sold_count, h=1)$mean)
  BBGforecasts[2]<-as.numeric(meanf(product$sold_count, h=1)$mean)
  BBGforecasts[3]<-as.numeric(forecast(HoltWinters(ts(product$sold_count ,frequency = 7)[,],seasonal = "additive" ),h=1)$mean)
  if(sum(ts(product3$sold_count ,frequency = 7)[,1]==0)==0){
    BBGforecasts[4]<-as.numeric(forecast(HoltWinters(ts(product$sold_count ,frequency = 7)[,],seasonal = "multiplicative" ),h=1)$mean)
  } else {
    BBGforecasts[4]<-NA
  }
  productfs<-as.data.frame(BBGforecasts)
  row.names(productfs)<-c("Naive", "Mean","HWAdditive","HWMultiplicative")
  return(productfs)
}