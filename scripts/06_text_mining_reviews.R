library(DBI)
library(RSQLite)
library(tidyverse)
library(tidytext)
library(tm)

db_path <- file.path("data", "hyperlocal_dw.sqlite")

if (!file.exists(db_path)) {
  stop("Data warehouse not found at ", db_path, ". Run build_data_warehouse.R first.")
}

con <- dbConnect(RSQLite::SQLite(), dbname = db_path)

reviews <- dbGetQuery(con, "
  SELECT review_id,
         review_text,
         rating
  FROM fact_review
")

dbDisconnect(con)

if (nrow(reviews) == 0) {
  stop("No reviews available for text mining.")
}

data("stop_words")

tokens <- reviews %>%
  filter(!is.na(review_text)) %>%
  unnest_tokens(word, review_text) %>%
  anti_join(stop_words, by = "word")

word_counts <- tokens %>%
  count(word, sort = TRUE)

dir.create("visuals", showWarnings = FALSE)

p_top <- ggplot(head(word_counts, 20), aes(x = reorder(word, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top 20 Words in Reviews",
    x = "Word",
    y = "Count"
  ) +
  theme_minimal()

ggsave(filename = file.path("visuals", "top_words_reviews.png"), plot = p_top, width = 6, height = 4)

# Calculate sentiment score per review
review_sentiments <- tokens %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  mutate(sentiment_value = if_else(sentiment == "positive", 1, -1)) %>%
  group_by(review_id) %>%
  summarise(sentiment_score = sum(sentiment_value))

# Join back to original reviews to see score alongside text
final_reviews_with_sentiment <- reviews %>%
  left_join(review_sentiments, by = "review_id") %>%
  mutate(sentiment_score = replace_na(sentiment_score, 0))

write_csv(final_reviews_with_sentiment, file.path("data", "reviews_with_sentiment_scores.csv"))

message("Enhanced sentiment analysis saved to data/reviews_with_sentiment_scores.csv")
