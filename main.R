source("api_connection.R")

# Requirements
require("forecast")
require("ggplot2")

# Naive forecast pipeline
pipeline_naive = function(ts){
  ts %>% naive() %>% checkresiduals()
  readline(prompt = "Please press a key to continue")
  print("OK!")
}