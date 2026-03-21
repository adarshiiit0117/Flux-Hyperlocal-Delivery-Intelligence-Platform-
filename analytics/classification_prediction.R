library(tidyverse)
library(caret)
library(rpart)
library(e1071)

# Load preprocessed data
data_path <- file.path("data", "preprocessed_data.rds")
if (!file.exists(data_path)) {
    stop("Preprocessed data not found. Run analytics/data_preprocessing.R first.")
}
df_clean <- readRDS(data_path)

# Prepare data for classification
# Target: customer_complaint
# Features: PC1, PC2, order_segment, traffic_level
df_ml <- df_clean %>%
    select(customer_complaint, PC1, PC2, order_segment, traffic_level) %>%
    mutate(customer_complaint = factor(customer_complaint, levels = c(0, 1), labels = c("No_Complaint", "Complaint")))

# Split data
set.seed(123)
trainIndex <- createDataPartition(df_ml$customer_complaint, p = .8, list = FALSE)
train_data <- df_ml[trainIndex, ]
test_data <- df_ml[-trainIndex, ]

# 1. Classification by Decision Tree
fit_dt <- rpart(customer_complaint ~ ., data = train_data, method = "class")

# 2. Bayesian Classification (Naive Bayes)
fit_nb <- naiveBayes(customer_complaint ~ ., data = train_data)

# 3. Support Vector Machines (SVM)
fit_svm <- svm(customer_complaint ~ ., data = train_data, probability = TRUE)

# Predictions and Accuracy
pred_dt <- predict(fit_dt, test_data, type = "class")
pred_nb <- predict(fit_nb, test_data)
pred_svm <- predict(fit_svm, test_data)

acc_dt <- confusionMatrix(pred_dt, test_data$customer_complaint)$overall["Accuracy"]
acc_nb <- confusionMatrix(pred_nb, test_data$customer_complaint)$overall["Accuracy"]
acc_svm <- confusionMatrix(pred_svm, test_data$customer_complaint)$overall["Accuracy"]

# Save models and results for Shiny
results <- list(
    dt_model = fit_dt,
    nb_model = fit_nb,
    svm_model = fit_svm,
    accuracies = data.frame(
        Model = c("Decision Tree", "Naive Bayes", "SVM"),
        Accuracy = c(acc_dt, acc_nb, acc_svm)
    )
)

saveRDS(results, file.path("data", "classification_results.rds"))

message("Classification models trained (Decision Tree, Naive Bayes, SVM). Results saved to data/classification_results.rds")
