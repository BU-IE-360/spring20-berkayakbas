source("api_connection.R")
source("requirements.R")
source("functions.R")
source("inputs.R")

while(forecasted_product_number <= 8){
  source("product_analysis.R")
  forecasted_product_number = forecasted_product_number + 1
  product_results_table_name = paste0('results_', tolower(gsub(' ', '_', product_name)))
  assign(product_results_table_name, results)
}