library(tidyverse)

dir.create("data", showWarnings = FALSE)

input_path <- file.path("data", "agent_reviews.csv")
output_path <- file.path("data", "reviews_clean.rds")

if (!file.exists(input_path)) {
  stop("Input file not found: ", input_path)
}

reviews_raw <- read_csv(input_path, show_col_types = FALSE)

# Your actual columns:
# "Agent Name", "Rating", "Review Text", "Delivery Time (min)",
# "Location", "Order Type", "Customer Feedback Type", "Price Range",
# "Discount Applied", "Product Availability", "Customer Service Rating", "Order Accuracy"

# We create fields expected by the rest of the project:
# - review_id              : row number
# - order_date             : synthetic date (align roughly with deliveries)
# - rating                 : "Rating"
# - order_accuracy         : "Order Accuracy"
# - product_availability   : "Product Availability"
# - discount_applied       : from "Discount Applied"
# - price_range            : "Price Range"
# - city                   : "Location"
# - agent_id               : synthetic id from "Agent Name"
# - agent_name             : "Agent Name"
# - review_text            : "Review Text"

n_rows <- nrow(reviews_raw)

if (n_rows == 0) {
  stop("agent_reviews.csv has no rows.")
}

start_date <- Sys.Date() - n_rows

reviews_clean <- reviews_raw %>%
  mutate(
    review_id = row_number(),
    order_date = Sys.Date() - sample(0:60, n(), replace = TRUE),
    rating = as.numeric(Rating),
    order_accuracy = as.numeric(`Order Accuracy`),
    product_availability = as.numeric(`Product Availability`),
    discount_applied = if_else(tolower(as.character(`Discount Applied`)) %in% c("yes", "1", "true"), 1L, 0L),
    price_range = factor(`Price Range`, ordered = TRUE),
    city = as.character(Location),
    agent_name = as.character(`Agent Name`),
    agent_id = as.character(factor(agent_name)),
    review_text = as.character(`Review Text`)
  ) %>%
  filter(
    !is.na(review_id),
    !is.na(order_date),
    !is.na(rating),
    !is.na(city)
  )

saveRDS(reviews_clean, output_path)

message("Agent reviews data cleaned and saved to: ", output_path)

