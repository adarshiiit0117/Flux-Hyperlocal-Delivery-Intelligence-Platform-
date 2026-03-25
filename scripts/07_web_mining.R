library(tidyverse)
library(tidytext)
library(DBI)
library(RSQLite)

# Load fact_review from DB
db_path <- file.path("data", "hyperlocal_dw.sqlite")
con <- dbConnect(RSQLite::SQLite(), dbname = db_path)
fact_review <- dbReadTable(con, "fact_review")
dbDisconnect(con)

# 1. Text Mining: Term Frequency - Inverse Document Frequency (TF-IDF)
review_words <- fact_review %>%
    unnest_tokens(word, review_text) %>%
    anti_join(stop_words, by = "word") %>%
    count(review_id, word, sort = TRUE)

total_words <- review_words %>%
    group_by(review_id) %>%
    summarize(total = sum(n))

review_words <- left_join(review_words, total_words)

review_tf_idf <- review_words %>%
    bind_tf_idf(word, review_id, n)

# 2. Web Mining simulation
# Simulate "Search Trends" related to delivery in different cities
web_trends <- data.frame(
    city = rep(c("City_1", "City_2", "City_3", "City_4", "City_5"), each = 10),
    search_query = sample(c("fast delivery", "cheap food", "grocery delivery", "late night snack", "coupons"), 50, replace = TRUE),
    traffic_volume = round(runif(50, 100, 5000))
)

# 3. Correlation between Review Rating and Source
summary_mining <- fact_review %>%
    group_by(source) %>%
    summarise(avg_rating = mean(rating), count = n())

# Save for Shiny
mining_results <- list(
    tf_idf = review_tf_idf %>% head(100),
    web_trends = web_trends,
    source_summary = summary_mining
)

saveRDS(mining_results, file.path("data", "mining_results.rds"))

message("Web and Text Mining analysis complete. Saved to data/mining_results.rds")
