library(tidyverse)
library(DBI)
library(RSQLite)
library(caret)

db_path <- file.path("data", "hyperlocal_dw.sqlite")
con <- dbConnect(RSQLite::SQLite(), dbname = db_path)

# Extract fact_delivery for processing
df_raw <- dbGetQuery(con, "SELECT * FROM fact_delivery")
dbDisconnect(con)

# 1. Data Cleaning: Handle missing values (if any) and out-of-range data
df_clean <- df_raw %>%
    mutate(
        order_value = if_else(is.na(order_value), median(order_value, na.rm = TRUE), order_value),
        delivery_time_min = if_else(delivery_time_min < 0, 0, delivery_time_min)
    )

# 2. Data Transformation: Scaling & Centering
# We'll scale numeric predictors for PCA
num_cols <- c("distance_km", "order_value", "agent_experience_years", "traffic_density_index")
df_numeric <- df_clean[, num_cols]
preproc_values <- preProcess(df_numeric, method = c("center", "scale"))
df_transformed <- predict(preproc_values, df_numeric)

# 3. Data Reduction: PCA (Principal Component Analysis)
pca_res <- prcomp(df_transformed, center = TRUE, scale. = TRUE)
# Keep first 2 PCs for visualization demonstration
df_pca <- as.data.frame(pca_res$x[, 1:2])
colnames(df_pca) <- c("PC1", "PC2")
df_clean <- bind_cols(df_clean, df_pca)

# 4. Data Discretization & Concept Hierarchy Generation
# Categorize 'order_value' into 'Low', 'Medium', 'High'
df_clean <- df_clean %>%
    mutate(
        order_segment = cut(order_value,
            breaks = c(-Inf, 25, 60, Inf),
            labels = c("Low Value", "Medium Value", "High Value")
        )
    )

# Save preprocessed data for the dashboard
# We will save it as a separate RDS for easier loading in Shiny
dir.create("data", showWarnings = FALSE)
saveRDS(df_clean, file.path("data", "preprocessed_data.rds"))

message("Data preprocessing complete. PCA and Discretization applied. Saved to data/preprocessed_data.rds")
