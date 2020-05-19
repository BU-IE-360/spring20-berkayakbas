ro_MAPE <- rep(NA, length(method_names))
ro_MAE <- rep(NA, length(method_names))

# Common parameters
ourValue = "mean"
ourTs = ts(product_data_xts$sold_count,frequency = 7)
ourOrigins = nrow(product_data_xts)*0.15

index=1
ourCall<-"naive(x=data, h=h)"
ro_obj <- ro(ourTs, h=2, origins=ourOrigins, call=ourCall, value=ourValue, ci=FALSE, co=TRUE, parallel=TRUE)
plot(ro_obj, main = paste0("Rolling Origin - ", method_names[index]))
ro_MAPE[index] <- MAPE(ro_obj$holdout[2,],ro_obj$mean[2,])
ro_MAE[index] <- MAE(ro_obj$holdout[2,],ro_obj$mean[2,])

index=2
ourCall<-"meanf(x=data, h=h)"
ro_obj <- ro(ourTs, h=2, origins=ourOrigins, call=ourCall, value=ourValue, ci=FALSE, co=TRUE, parallel=TRUE)
plot(ro_obj, main = paste0("Rolling Origin - ", method_names[index]))
ro_MAPE[index] <- MAPE(ro_obj$holdout[2,],ro_obj$mean[2,])
ro_MAE[index] <- MAE(ro_obj$holdout[2,],ro_obj$mean[2,])

index=3
ourCall<-"forecast(HoltWinters(x=data, seasonal = 'additive'), h=h)"
ro_obj <- ro(ourTs, h=2, origins=ourOrigins, call=ourCall, value=ourValue, ci=FALSE, co=TRUE, parallel=TRUE)
plot(ro_obj, main = paste0("Rolling Origin - ", method_names[index]))
ro_MAPE[index] <- MAPE(ro_obj$holdout[2,],ro_obj$mean[2,])
ro_MAE[index] <- MAE(ro_obj$holdout[2,],ro_obj$mean[2,])

index=4
if (sum(ts(product_data_xts$sold_count , frequency = 7)[, 1] == 0) == 0) {
  ourCall<-"forecast(HoltWinters(x=data, seasonal = 'multiplicative'), h=h)"
  ro_obj <- ro(ourTs, h=2, origins=ourOrigins, call=ourCall, value=ourValue, ci=FALSE, co=TRUE, parallel=TRUE)
  plot(ro_obj, main = paste0("Rolling Origin - ", method_names[index]))
  ro_MAPE[index] <- MAPE(ro_obj$holdout[2,],ro_obj$mean[2,])
  ro_MAE[index] <- MAE(ro_obj$holdout[2,],ro_obj$mean[2,])
}else {
  ro_MAPE[index] <- NA
  ro_MAE[index] <- MAE(ro_obj$holdout[2,],ro_obj$mean[2,])
}

index=5
ourCall<-"ses(x=data, h=h)"
ro_obj <- ro(ourTs, h=2, origins=ourOrigins, call=ourCall, value=ourValue, ci=FALSE, co=TRUE, parallel=TRUE)
plot(ro_obj, main = paste0("Rolling Origin - ", method_names[index]))
ro_MAPE[index] <- MAPE(ro_obj$holdout[2,],ro_obj$mean[2,])
ro_MAE[index] <- MAE(ro_obj$holdout[2,],ro_obj$mean[2,])

index=6
ourCall<-"forecast(auto.arima(x=data), h=h)"
ro_obj <- ro(ourTs, h=2, origins=ourOrigins, call=ourCall, value=ourValue, ci=FALSE, co=TRUE, parallel=TRUE)
plot(ro_obj, main = paste0("Rolling Origin - ", method_names[index]))
ro_MAPE[index] <- MAPE(ro_obj$holdout[2,],ro_obj$mean[2,])
ro_MAE[index] <- MAE(ro_obj$holdout[2,],ro_obj$mean[2,])

index=7
ourCall<-"forecast(tbats(y=data), h=h)"
ro_obj <- ro(ourTs, h=2, origins=ourOrigins, call=ourCall, value=ourValue, ci=FALSE, co=TRUE, parallel=TRUE)
plot(ro_obj, main = paste0("Rolling Origin - ", method_names[index]))
ro_MAPE[index] <- MAPE(ro_obj$holdout[2,],ro_obj$mean[2,])
ro_MAE[index] <- MAE(ro_obj$holdout[2,],ro_obj$mean[2,])

results <- as.data.frame(cbind(results, ro_MAPE,ro_MAE))
print("Rolling Origin Done!")