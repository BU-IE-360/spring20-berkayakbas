source("api_connection.R")
source("requirements.R")
source("functions.R")
source("inputs.R")

product_names_dt = data.table(read.csv("API Products.csv", sep = ';'))
# date_annotation_dt= data.table(sapply(read.csv('date_annotation.csv'), as.Date))
# data_dt[,"is_lock_down"] = as.numeric(data_dt$event_date %in% date_annotation_dt$lock_down_dates)

# Product Analysis
product_ids = unique(data_dt$product_content_id)
product_id = product_ids[forecasted_product_number]
product_data_dt = data_dt[product_content_id == product_id]
source("make_xts.R")

product_data_xts=tail(product_data_xts,filter_last_n_days)
print(paste0("Last event date: ", tail(data_dt, 1)$event_date))
print(paste0("Product ID: ", product_id))
print(paste0("Product Name: ", product_names_dt[product_content_id == product_id]$bottom_hierarchy))
browseURL(toString(product_names_dt[product_content_id == product_id]$url))
source("product_forecasts.R")
source("rolling_origin.R")

View(results)

# Initialize Predictions
predictions=unique(data_dt[,list(product_content_id)])
predictions[,forecast:=0]
# Set Product Prediction
predictions[product_content_id == product_id]$forecast = 85
predictions

# Send Submission
# send_submission(predictions, token, url=subm_url, submit_now=F)
