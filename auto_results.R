forecasted_product_number = 1
filter_last_n_days = 135
test_period = 1

# Comment source("inputs.R") in "main3.R" before run

while(forecasted_product_number <= 8){
  source("main3.R")
  forecasted_product_number = forecasted_product_number + 1
  assign(paste0('results_', tolower(gsub(' ', '_', product_name))), results)
}
