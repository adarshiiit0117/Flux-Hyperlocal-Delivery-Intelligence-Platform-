library(DBI)
library(RSQLite)
library(tidyverse)
library(arules)

db_path <- file.path("data", "hyperlocal_dw.sqlite")

if (!file.exists(db_path)) {
  stop("Data warehouse not found at ", db_path, ". Run build_data_warehouse.R first.")
}

con <- dbConnect(RSQLite::SQLite(), dbname = db_path)

rules_df <- dbGetQuery(con, "
  SELECT
    CASE WHEN order_accuracy >= 4 THEN 'HighAccuracy' ELSE 'LowAccuracy' END AS accuracy_cat,
    CASE WHEN product_availability >= 4 THEN 'GoodAvailability' ELSE 'LowAvailability' END AS availability_cat,
    CASE WHEN discount_applied = 1 THEN 'Discount' ELSE 'NoDiscount' END AS discount_cat,
    CASE WHEN rating >= 4 THEN 'HighRating' ELSE 'LowRating' END AS rating_cat
  FROM fact_review
")

dbDisconnect(con)

if (nrow(rules_df) == 0) {
  stop("No review data available for association rules.")
}

trans_list <- as(split(rules_df, seq_len(nrow(rules_df))), "transactions")

rules <- apriori(
  trans_list,
  parameter = list(supp = 0.05, conf = 0.6, minlen = 2)
)

rules_sorted <- sort(rules, by = "lift")

inspect(head(rules_sorted, 20))

saveRDS(rules_sorted, file = file.path("data", "association_rules.rds"))

message("Top association rules saved to data/association_rules.rds")

