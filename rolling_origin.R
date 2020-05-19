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
accuracy_MAPE <- rep(NA, length(method_names))
accuracy_MAE <- rep(NA, length(method_names))

index=1
ourcall<-"naive(x=data, h=h)"
ourValue<-"mean"
ro_denemee <- ro(ts(product_data_xts$sold_count,frequency = 7), h=2, origins=(nrow(product_data_xts)*0.15), call=ourcall, value=ourValue, ci=FALSE, co=TRUE)
plot(ro_denemee)
accuracy_MAPE[index] <- mape(ro_denemee$holdout[2,],ro_denemee$mean[2,])
accuracy_MAE[index] <- MAE(ro_denemee$holdout[2,],ro_denemee$mean[2,])

index=2
ourcall<-"meanf(x=data, h=h)"
ourValue<-"mean"
ro_denemee <- ro(ts(product_data_xts$sold_count,frequency = 7), h=2, origins=(nrow(product_data_xts)*0.15), call=ourcall, value=ourValue, ci=FALSE, co=TRUE)
plot(ro_denemee)
accuracy_MAPE[index] <- mape(ro_denemee$holdout[2,],ro_denemee$mean[2,])
accuracy_MAE[index] <- MAE(ro_denemee$holdout[2,],ro_denemee$mean[2,])

index=3
ourcall<-"forecast(HoltWinters(x=data, seasonal = 'additive'), h=h)"
ourValue<-"mean"
ro_denemee <- ro(ts(product_data_xts$sold_count,frequency = 7), h=2, origins=(nrow(product_data_xts)*0.15), call=ourcall, value=ourValue, ci=FALSE, co=TRUE)
plot(ro_denemee)
accuracy_MAPE[index] <- mape(ro_denemee$holdout[2,],ro_denemee$mean[2,])
accuracy_MAE[index] <- MAE(ro_denemee$holdout[2,],ro_denemee$mean[2,])

index=4
if (sum(ts(product_data_xts$sold_count , frequency = 7)[, 1] == 0) == 0) {
  ourcall<-"forecast(HoltWinters(x=data, seasonal = 'multiplicative'), h=h)"
  ourValue<-"mean"
  ro_denemee <- ro(ts(product_data_xts$sold_count,frequency = 7), h=2, origins=(nrow(product_data_xts)*0.15), call=ourcall, value=ourValue, ci=FALSE, co=TRUE)
  plot(ro_denemee)
  accuracy_MAPE[index] <- mape(ro_denemee$holdout[2,],ro_denemee$mean[2,])
  accuracy_MAE[index] <- MAE(ro_denemee$holdout[2,],ro_denemee$mean[2,])
}else {
  accuracy_MAPE[index] <- NA
  accuracy_MAE[index] <- MAE(ro_denemee$holdout[2,],ro_denemee$mean[2,])
}

index=5
ourcall<-"ses(x=data, h=h)"
ourValue<-"mean"
ro_denemee <- ro(ts(product_data_xts$sold_count,frequency = 7), h=2, origins=(nrow(product_data_xts)*0.15), call=ourcall, value=ourValue, ci=FALSE, co=TRUE)
plot(ro_denemee)
accuracy_MAPE[index] <- mape(ro_denemee$holdout[2,],ro_denemee$mean[2,])
accuracy_MAE[index] <- MAE(ro_denemee$holdout[2,],ro_denemee$mean[2,])

index=6
ourcall<-"forecast(auto.arima(x=data), h=h)"
ourValue<-"mean"
ro_denemee <- ro(ts(product_data_xts$sold_count,frequency = 7), h=2, origins=(nrow(product_data_xts)*0.15), call=ourcall, value=ourValue, ci=FALSE, co=TRUE)
plot(ro_denemee)
accuracy_MAPE[index] <- mape(ro_denemee$holdout[2,],ro_denemee$mean[2,])
accuracy_MAE[index] <- MAE(ro_denemee$holdout[2,],ro_denemee$mean[2,])

index=7
ourcall<-"forecast(tbats(y=data), h=h)"
ourValue<-"mean"
ro_denemee <- ro(ts(product_data_xts$sold_count,frequency = 7), h=2, origins=(nrow(product_data_xts)*0.15), call=ourcall, value=ourValue, ci=FALSE, co=TRUE)
plot(ro_denemee)
accuracy_MAPE[index] <- mape(ro_denemee$holdout[2,],ro_denemee$mean[2,])
accuracy_MAE[index] <- MAE(ro_denemee$holdout[2,],ro_denemee$mean[2,])

results_rolling_origin <- as.data.frame(cbind(accuracy_MAPE,accuracy_MAE))
row.names(results_rolling_origin) <- method_names
print("Rolling Origin Done!")