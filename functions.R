mape <- function(actual,preds){
  mape <- mean(abs((actual - preds)/actual))*100
  return (mape)
}

set_accuracy_metrics <- function (model, index, results){
  accuracy = accuracy(model)
  results[index, 'accuracy_ME'] <- accuracy[, 'ME']
  results[index, 'accuracy_RMSE'] <- accuracy[, 'RMSE']
  results[index, 'accuracy_MAE'] <- accuracy[, 'MAE']
  results[index, 'accuracy_MAPE']<- accuracy[, 'MAPE']
  results[index, 'accuracy_MASE']<- accuracy[, 'MASE']
  results[index, 'accuracy_ACF1']<- accuracy[, 'ACF1']
  return(results)
}

set_test_accuracy_metrics <- function (model, index, results){
  accuracy = accuracy(model)
  results[index, 'accuracy_test_ME'] <- accuracy["Test Set", 'ME']
  results[index, 'accuracy_test_RMSE'] <- accuracy["Test Set", 'RMSE']
  results[index, 'accuracy_test_MAE'] <- accuracy["Test Set", 'MAE']
  results[index, 'accuracy_test_MAPE']<- accuracy["Test Set", 'MAPE']
  results[index, 'accuracy_test_MASE']<- accuracy["Test Set", 'MASE']
  results[index, 'accuracy_test_ACF1']<- accuracy["Test Set", 'ACF1']
  return(results)
}