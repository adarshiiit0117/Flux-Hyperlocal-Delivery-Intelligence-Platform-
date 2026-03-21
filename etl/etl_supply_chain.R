library(tidyverse)
library(lubridate)

dir.create("data", showWarnings = FALSE)

input_path <- file.path("data", "supply_chain.csv")
output_path <- file.path("data", "supply_clean.rds")

if (!file.exists(input_path)) {
  stop("Input file not found: ", input_path)
}

supply_raw <- read_csv(input_path, show_col_types = FALSE)

# Your actual columns:
# "Product type", "SKU", "Price", "Availability",
# "Number of products sold", "Revenue generated", "Customer demographics",
# "Stock levels", "Lead times", "Order quantities", "Shipping times",
# "Shipping carriers", "Shipping costs", "Supplier name", "Location",
# "Lead time", "Production volumes", "Manufacturing lead time",
# "Manufacturing costs", "Inspection results", "Defect rates",
# "Transportation modes", "Routes", "Costs"

# We will create the fields the rest of the project expects:
# - delivery_id       : row number
# - order_date        : synthetic date (sequence over last N days)
# - delivery_time_min : use "Shipping times"
# - sla_min           : global SLA = median delivery time (so some are early, some delayed)
# - distance_km       : derived from "Shipping costs" (scaled)
# - city              : use "Location"
# - traffic_level     : simple bucket from "Shipping times"
# - weather           : placeholder "Clear"
# - vehicle_type      : use "Transportation modes" if present, else "Unknown"
# - is_festival       : 0 (no festival info in this dataset)

n_rows <- nrow(supply_raw)

if (n_rows == 0) {
  stop("supply_chain.csv has no rows.")
}

start_date <- Sys.Date() - n_rows

# Compute a single SLA value from the overall delivery-time distribution
sla_global <- median(as.numeric(supply_raw$`Shipping times`), na.rm = TRUE)

supply_clean <- supply_raw %>%
  mutate(
    delivery_id = row_number(),
    order_date = Sys.Date() - sample(0:60, n(), replace = TRUE),
    delivery_time_min = as.numeric(`Shipping times`),
    sla_min = sla_global,
    distance_km = as.numeric(`Shipping costs`) / 10,
    city = as.character(Location),
    traffic_level = case_when(
      delivery_time_min <= quantile(delivery_time_min, 0.33, na.rm = TRUE) ~ "Low",
      delivery_time_min <= quantile(delivery_time_min, 0.66, na.rm = TRUE) ~ "Medium",
      TRUE ~ "High"
    ),
    weather = sample(c("Clear", "Rainy", "Stormy"), n(), replace = TRUE),
    vehicle_type = if ("Transportation modes" %in% names(supply_raw)) {
      as.character(`Transportation modes`)
    } else {
      "Unknown"
    },
    is_festival = 0L
  ) %>%
  filter(
    !is.na(delivery_id),
    !is.na(order_date),
    !is.na(delivery_time_min),
    !is.na(sla_min),
    !is.na(city)
  )

saveRDS(supply_clean, output_path)

message("Supply chain data cleaned and saved to: ", output_path)

