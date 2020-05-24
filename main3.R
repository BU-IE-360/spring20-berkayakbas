source("api_connection.R")
source("requirements.R")
source("functions.R")
source("inputs.R")

source("product_analysis.R")
View(results)

# Initialize Predictions
predictions = initialize_predictions()
# Set Product Prediction
predictions = set_prediction(product_id = product_id, prediction = 5)

# Send Submission
# send_submission(predictions, token, url=subm_url, submit_now=F)