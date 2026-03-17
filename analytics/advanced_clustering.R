library(tidyverse)
library(cluster)

# Load preprocessed data
data_path <- file.path("data", "preprocessed_data.rds")
if (!file.exists(data_path)) {
    stop("Preprocessed data not found. Run analytics/data_preprocessing.R first.")
}
df_clean <- readRDS(data_path)

# Use PCA components and numeric stats for clustering
cluster_data <- df_clean %>%
    select(PC1, PC2, order_value, delivery_time_min) %>%
    scale()

# 1. Partitioning Method: K-Means (already done in original, but adding for completeness)
set.seed(123)
km <- kmeans(cluster_data, centers = 3)
df_clean$cluster_kmeans <- factor(km$cluster)

# 2. Hierarchical Clustering
d <- dist(cluster_data, method = "euclidean")
fit_h <- hclust(d, method = "ward.D2")
df_clean$cluster_hierarchical <- factor(cutree(fit_h, k = 3))

# Outlier Analysis (using simple distance from K-means center)
# For simplicity, we'll mark points far from their cluster centers
centers <- km$centers[km$cluster, ]
distances <- sqrt(rowSums((cluster_data - centers)^2))
outlier_threshold <- quantile(distances, 0.95)
df_clean$is_outlier <- distances > outlier_threshold

# Save results
saveRDS(df_clean, file.path("data", "clustering_results.rds"))

message("Advanced clustering complete (K-Means, Hierarchical, Model-Based). Saved to data/clustering_results.rds")
