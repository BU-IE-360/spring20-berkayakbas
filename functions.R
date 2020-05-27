# Functions will be added

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