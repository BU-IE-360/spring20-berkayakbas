# Functions will be added

set_prediction = function(product_id, prediction){
  predictions[product_content_id == product_id]$forecast = prediction
  print(predictions)
  return(predictions)
}

initialize_predictions = function (){
  if (!exists("predictions")) {
    predictions = unique(data_dt[, list(product_content_id)])
    predictions$product_content_id = as.integer(predictions$product_content_id)
    predictions[, forecast := 0]
    predictions = merge(predictions, product_names_dt[,c('product_content_id', 'bottom_hierarchy')], by='product_content_id', sort = F)
    print("Predictions are initialized")
  }
  else{
    print("Predictions are already exist")
  }
  print(predictions)
  return(predictions)
}