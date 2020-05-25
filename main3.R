source("api_connection.R")
source("requirements.R")
source("functions.R")
source("inputs.R")

source("product_analysis.R")
View(results)

# Set Product Prediction
predictions = set_prediction(product_id = product_id, prediction = 1)

# Send Submission
# send_submission(predictions, token, url=subm_url, submit_now=F)