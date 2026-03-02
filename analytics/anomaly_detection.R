library(DBI)
library(RSQLite)
library(tidyverse)

db_path <- file.path("data", "hyperlocal_dw.sqlite")

if (!file.exists(db_path)) {
  stop("Data warehouse not found at ", db_path, ". Run build_data_warehouse.R first.")
}

con <- dbConnect(RSQLite::SQLite(), dbname = db_path)

del <- dbGetQuery(con, "
  SELECT delivery_id,
         city_id,
         distance_km,
         delivery_time_min,
         sla_min
  FROM fact_delivery
")

dbDisconnect(con)

del <- del %>%
  mutate(
    delay_diff = delivery_time_min - sla_min
  ) %>%
  drop_na(delay_diff)

if (nrow(del) == 0) {
  stop("No delivery records available for anomaly detection.")
}

del <- del %>%
  mutate(
    z_delay = (delay_diff - mean(delay_diff)) / sd(delay_diff)
  )

anomalies <- del %>%
  filter(abs(z_delay) > 3)

output_path <- file.path("data", "delivery_anomalies.csv")
write_csv(anomalies, output_path)

message("Anomalous deliveries saved to ", output_path)

