library(DBI)
library(RSQLite)
library(tidyverse)

db_path <- file.path("data", "hyperlocal_dw.sqlite")
con <- dbConnect(RSQLite::SQLite(), dbname = db_path)

message("--- Database Summary ---")

# Check Tables
tables <- dbListTables(con)
message("Tables in database: ", paste(tables, collapse = ", "))

# Sample fact_delivery
delivery_summary <- dbGetQuery(con, "SELECT COUNT(*) as total_deliveries, AVG(delivery_time_min) as avg_time FROM fact_delivery")
print(delivery_summary)

# Sample fact_review
review_summary <- dbGetQuery(con, "SELECT COUNT(*) as total_reviews, AVG(rating) as avg_rating FROM fact_review")
print(review_summary)

# Anomaly check
anomalies <- read_csv("data/delivery_anomalies.csv", show_col_types = FALSE)
message("Number of anomalies detected: ", nrow(anomalies))

dbDisconnect(con)
message("--- Verification Complete ---")
