library(DBI)
library(RSQLite)
library(tidyverse)
library(cluster)

set.seed(123)

db_path <- file.path("data", "hyperlocal_dw.sqlite")

if (!file.exists(db_path)) {
  stop("Data warehouse not found at ", db_path, ". Run build_data_warehouse.R first.")
}

con <- dbConnect(RSQLite::SQLite(), dbname = db_path)

agent_perf <- dbGetQuery(con, "
  SELECT a.agent_key,
         a.agent_id,
         a.agent_name,
         AVG(f.delivery_time_min) AS avg_delivery_time,
         SUM(f.delayed_flag) * 1.0 / COUNT(*) AS delay_rate,
         AVG(r.rating) AS avg_rating
  FROM dim_agent a
  JOIN fact_delivery f ON a.agent_id = f.agent_id
  LEFT JOIN fact_review r ON a.agent_key = r.agent_key
  GROUP BY a.agent_key, a.agent_id, a.agent_name
")

dbDisconnect(con)

agent_perf_num <- agent_perf %>%
  select(avg_delivery_time, delay_rate, avg_rating) %>%
  drop_na()

if (nrow(agent_perf_num) < 3) {
  stop("Not enough agents for clustering. Need at least 3.")
}

agent_scaled <- scale(agent_perf_num)

k <- 3
km <- kmeans(agent_scaled, centers = k)

agent_perf$cluster <- factor(km$cluster)

dir.create("visuals", showWarnings = FALSE)

p <- ggplot(agent_perf, aes(x = avg_delivery_time, y = avg_rating, color = cluster)) +
  geom_point(size = 2) +
  labs(
    title = "Agent Segmentation by Performance",
    x = "Average Delivery Time (min)",
    y = "Average Rating"
  ) +
  theme_minimal()

ggsave(filename = file.path("visuals", "agent_clusters.png"), plot = p, width = 6, height = 4)

cluster_path <- file.path("data", "agent_clusters.csv")
write_csv(agent_perf, cluster_path)

message("Agent clustering results saved to ", cluster_path, " and plot to visuals/agent_clusters.png")

