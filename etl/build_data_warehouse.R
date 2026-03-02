library(tidyverse)
library(lubridate)
library(DBI)
library(RSQLite)

dir.create("data", showWarnings = FALSE)

supply_path <- file.path("data", "supply_clean.rds")
reviews_path <- file.path("data", "reviews_clean.rds")

if (!file.exists(supply_path)) {
  stop("Missing cleaned supply data. Run etl_supply_chain.R first.")
}
if (!file.exists(reviews_path)) {
  stop("Missing cleaned reviews data. Run etl_agent_reviews.R first.")
}

supply_clean <- readRDS(supply_path)
reviews_clean <- readRDS(reviews_path)

dim_time <- supply_clean %>%
  select(order_date) %>%
  distinct() %>%
  arrange(order_date) %>%
  mutate(
    time_id = row_number(),
    year = year(order_date),
    month = month(order_date),
    day = day(order_date),
    weekday = wday(order_date, label = TRUE, abbr = TRUE)
  ) %>%
  relocate(time_id)

dim_city <- bind_rows(
  supply_clean %>% select(city),
  reviews_clean %>% select(city)
) %>%
  distinct() %>%
  arrange(city) %>%
  mutate(city_id = row_number()) %>%
  relocate(city_id)

dim_agent <- reviews_clean %>%
  select(agent_id, agent_name) %>%
  distinct() %>%
  arrange(agent_id) %>%
  mutate(agent_key = row_number()) %>%
  relocate(agent_key)

fact_delivery <- supply_clean %>%
  left_join(dim_time, by = "order_date") %>%
  left_join(dim_city, by = "city") %>%
  mutate(
    delayed_flag = if_else(delivery_time_min > sla_min, 1L, 0L)
  ) %>%
  transmute(
    delivery_id = delivery_id,
    time_id = time_id,
    city_id = city_id,
    distance_km = distance_km,
    delivery_time_min = delivery_time_min,
    sla_min = sla_min,
    delayed_flag = delayed_flag,
    traffic_level = as.character(traffic_level),
    weather = as.character(weather),
    vehicle_type = as.character(vehicle_type),
    is_festival = is_festival
  )

fact_review <- reviews_clean %>%
  left_join(dim_time, by = "order_date") %>%
  left_join(dim_city, by = "city") %>%
  left_join(dim_agent, by = c("agent_id", "agent_name")) %>%
  transmute(
    review_id = review_id,
    time_id = time_id,
    city_id = city_id,
    agent_key = agent_key,
    rating = rating,
    order_accuracy = order_accuracy,
    product_availability = product_availability,
    discount_applied = discount_applied,
    price_range = as.character(price_range),
    review_text = review_text
  )

db_path <- file.path("data", "hyperlocal_dw.sqlite")

con <- dbConnect(RSQLite::SQLite(), dbname = db_path)

dbWriteTable(con, "dim_time", dim_time, overwrite = TRUE)
dbWriteTable(con, "dim_city", dim_city, overwrite = TRUE)
dbWriteTable(con, "dim_agent", dim_agent, overwrite = TRUE)
dbWriteTable(con, "fact_delivery", fact_delivery, overwrite = TRUE)
dbWriteTable(con, "fact_review", fact_review, overwrite = TRUE)

dbDisconnect(con)

message("Data warehouse created at: ", db_path)

