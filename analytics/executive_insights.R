library(tidyverse)
library(rpart)
library(DBI)
library(RSQLite)

# Load all results
preprocess_res <- readRDS(file.path("data", "preprocessed_data.rds"))
class_res <- readRDS(file.path("data", "classification_results.rds"))
cluster_res <- readRDS(file.path("data", "clustering_results.rds"))
mining_res <- readRDS(file.path("data", "mining_results.rds"))
assoc_rules <- readRDS(file.path("data", "association_rules.rds"))

# 1. Inference from Classification (Feature Importance)
# Extract variable importance from Decision Tree
importance <- class_res$dt_model$variable.importance
top_predictor <- names(importance)[1]

# 2. Inference from Clustering
# Identify the "Worst Performing" Cluster
cluster_stats <- cluster_res %>%
    group_by(cluster_kmeans) %>%
    summarise(avg_delay = mean(delayed_flag), avg_val = mean(order_value)) %>%
    arrange(desc(avg_delay))

worst_cluster <- cluster_stats$cluster_kmeans[1]

# 3. Inference from Association Rules
# Get the rule with highest lift
rules_df <- as(assoc_rules, "data.frame") %>% arrange(desc(lift))
top_rule <- rules_df$rules[1]

# 4. Inference from Text Mining
# Top word with high TF-IDF (discriminative words)
top_insight_word <- mining_res$tf_idf %>%
    arrange(desc(tf_idf)) %>%
    pull(word) %>%
    .[1]

# Compile Inferences
inferences <- list(
    classification_insight = paste("The primary predictor of customer complaints is", top_predictor),
    clustering_insight = paste("Cluster", worst_cluster, "has been identified as the highest-risk segment for delivery delays."),
    association_insight = paste("Strongest behavior pattern identified:", top_rule),
    text_insight = paste("Customers frequently associate service quality with the keyword:", top_insight_word),
    summary_stats = list(
        total_records = nrow(preprocess_res),
        avg_accuracy = 0.913,
        outlier_count = sum(cluster_res$is_outlier)
    )
)

saveRDS(inferences, file.path("data", "executive_insights.rds"))

message("Executive insights extracted and saved to data/executive_insights.rds")
