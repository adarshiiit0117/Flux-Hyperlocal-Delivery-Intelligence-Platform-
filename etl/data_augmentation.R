library(tidyverse)
library(DBI)
library(RSQLite)

db_path <- file.path("data", "hyperlocal_dw.sqlite")
con <- dbConnect(RSQLite::SQLite(), dbname = db_path)

# Load existing fact tables
fact_delivery <- dbReadTable(con, "fact_delivery")
fact_review <- dbReadTable(con, "fact_review")

set.seed(42)

# Augment fact_delivery with more features for Classification/Prediction
fact_delivery <- fact_delivery %>%
    mutate(
        # Simulate numeric features for clustering and discretization
        order_value = round(runif(n(), 5, 100), 2),
        agent_experience_years = round(runif(n(), 0.5, 5), 1),
        traffic_density_index = round(runif(n(), 0, 1), 2),
        actual_distance = distance_km * runif(n(), 0.9, 1.2),

        # Simulate "Weather Impact" category for Discretization
        weather_severity = case_when(
            weather == "Clear" ~ runif(n(), 0, 3),
            weather == "Rainy" ~ runif(n(), 4, 7),
            weather == "Stormy" ~ runif(n(), 8, 10),
            TRUE ~ runif(n(), 0, 5)
        ),

        # Target for prediction (simulated logical outcome)
        customer_complaint = if_else(delayed_flag == 1 & runif(n()) > 0.4, 1, 0)
    )

# Augment fact_review for Web/Text Mining simulation
fact_review <- fact_review %>%
    mutate(
        # Simulate "User Source" (Web vs Mobile)
        source = sample(c("Web", "Android", "iOS"), n(), replace = TRUE, prob = c(0.2, 0.4, 0.4)),
        # Simulate more text for mining
        review_text = paste(review_text, sample(c("The delivery was fast!", "Driver was polite", "Food was cold", "Great service", "Awful delay", "Highly recommend"), n(), replace = TRUE))
    )

# Write back to DB
dbWriteTable(con, "fact_delivery", fact_delivery, overwrite = TRUE)
dbWriteTable(con, "fact_review", fact_review, overwrite = TRUE)

dbDisconnect(con)

message("Data augmentation complete. Added new features to fact_delivery and fact_review.")
