# Imporevement of data for regression
product_data_dt[,lag1_sold_count:=shift(sold_count,1)]
product_data_dt[,lag7_sold_count:=shift(sold_count,7)]
product_data_dt[,lag28_sold_count:=shift(sold_count,28)]
product_data_dt[,lag365_sold_count:=shift(sold_count,365)]

product_data_dt[, 'is_after_corona'] = c(0)
product_data_dt[event_date >= as.Date('2020-03-12')]$is_after_corona = c(1)

# Firstly fills NA prices to last observation, then first price is set for the prior NA prices.
product_data_dt[price == -1]$price = c(NA)
product_data_dt$price = na.locf(product_data_dt$price, na.rm = FALSE)
product_data_dt$price = na.locf(product_data_dt$price, na.rm = FALSE, fromLast = TRUE)

data_seq <- seq(from = first(product_data_dt$event_date),
                last(product_data_dt$event_date),
                by = "days")

data_seq1 <-
  seq(
    from = first(product_data_dt[product_data_dt$sold_count != 0, ]$event_date),
    last(product_data_dt$event_date),
    by = "days"
  )

x1 <- (length(data_seq) - length(data_seq1))
product_data_dt <- product_data_dt[(x1 + 1):length(data_seq),]
product_data_dt$event_date <- NULL
product_data_dt$product_content_id <- NULL
product_data_dt <- data.table(sapply(product_data_dt, as.numeric))
# product_data$is_after_corona = as.factor(product_data$is_after_corona)
# product_data$is_lock_down = as.factor(product_data$is_lock_down)
product_data_dt[is.na(product_data_dt)] <- 0
product_data_xts <- xts(product_data_dt, order.by = data_seq1)