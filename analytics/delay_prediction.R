library(DBI)
library(RSQLite)
library(tidyverse)
library(caret)
library(e1071)

set.seed(123)

db_path <- file.path("data", "hyperlocal_dw.sqlite")

if (!file.exists(db_path)) {
  stop("Data warehouse not found at ", db_path, ". Run build_data_warehouse.R first.")
}

con <- dbConnect(RSQLite::SQLite(), dbname = db_path)

df <- dbGetQuery(con, "
  SELECT delivery_time_min,
         sla_min,
         delayed_flag,
         traffic_level,
         weather,
         distance_km,
         is_festival
  FROM fact_delivery
")

dbDisconnect(con)

df <- df %>%
  mutate(
    delayed_flag = factor(delayed_flag, levels = c(0, 1), labels = c("OnTime", "Delayed")),
    traffic_level = factor(traffic_level),
    weather = factor(weather),
    is_festival = factor(is_festival)
  ) %>%
  drop_na()

train_idx <- createDataPartition(df$delayed_flag, p = 0.7, list = FALSE)
train <- df[train_idx, ]
test <- df[-train_idx, ]

ctrl <- trainControl(method = "cv", number = 5)

model <- train(
  delayed_flag ~ distance_km + is_festival + traffic_level + weather,
  data = train,
  method = "naive_bayes",
  trControl = ctrl
)

pred <- predict(model, newdata = test)
conf_mtx <- confusionMatrix(pred, test$delayed_flag)

print(model)
print(conf_mtx)

saveRDS(model, file = file.path("data", "delay_model_naive_bayes.rds"))

message("Delay prediction model trained and saved to data/delay_model_naive_bayes.rds")
