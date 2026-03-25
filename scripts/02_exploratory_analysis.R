library(DBI)
library(RSQLite)
library(tidyverse)

db_path <- file.path("data", "hyperlocal_dw.sqlite")

if (!file.exists(db_path)) {
  stop("Data warehouse not found at ", db_path, ". Run build_data_warehouse.R first.")
}

con <- dbConnect(RSQLite::SQLite(), dbname = db_path)

olap_city_traffic <- dbGetQuery(con, "
  SELECT c.city,
         f.traffic_level,
         AVG(f.delivery_time_min) AS avg_delivery_time,
         SUM(f.delayed_flag) * 1.0 / COUNT(*) AS delay_rate
  FROM fact_delivery f
  JOIN dim_city c ON f.city_id = c.city_id
  GROUP BY c.city, f.traffic_level
")

olap_city_month <- dbGetQuery(con, "
  SELECT c.city,
         t.year,
         t.month,
         AVG(f.delivery_time_min) AS avg_delivery_time,
         SUM(f.delayed_flag) * 1.0 / COUNT(*) AS delay_rate
  FROM fact_delivery f
  JOIN dim_city c ON f.city_id = c.city_id
  JOIN dim_time t ON f.time_id = t.time_id
  GROUP BY c.city, t.year, t.month
")

dbDisconnect(con)

dir.create("visuals", showWarnings = FALSE)

p1 <- ggplot(olap_city_traffic, aes(x = traffic_level, y = avg_delivery_time, fill = city)) +
  geom_col(position = "dodge") +
  labs(
    title = "Average Delivery Time by City and Traffic Level",
    x = "Traffic Level",
    y = "Average Delivery Time (min)"
  ) +
  theme_minimal()

ggsave(filename = file.path("visuals", "avg_delivery_by_city_traffic.png"), plot = p1, width = 8, height = 4)

p2 <- ggplot(olap_city_month, aes(x = month, y = delay_rate, color = city, group = city)) +
  geom_line() +
  facet_wrap(~ year, scales = "free_x") +
  labs(
    title = "Delay Rate by City and Month",
    x = "Month",
    y = "Delay Rate"
  ) +
  theme_minimal()

ggsave(filename = file.path("visuals", "delay_rate_by_city_month.png"), plot = p2, width = 8, height = 4)

message("OLAP plots saved in 'visuals' folder.")

