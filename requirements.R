# R Package Requirements
# Run this file to install all necessary packages for the project

# Core framework and logic
install.packages("shiny")
install.packages("shinydashboard")
install.packages("tidyverse")
install.packages("lubridate")

# Data processing and Database
install.packages("DBI")
install.packages("RSQLite")

# Machine Learning & Data Mining
install.packages("caret")
install.packages("randomForest")
install.packages("e1071")       # SVM
install.packages("rpart")       # Decision Trees
install.packages("rpart.plot")
install.packages("cluster")
install.packages("factoextra")
install.packages("arules")      # Association Rules
install.packages("arulesViz")

# Text & Web Mining
install.packages("tm")
install.packages("wordcloud")
install.packages("rvest")

# Visualization
install.packages("ggplot2")
install.packages("plotly")
install.packages("DT")

cat("All essential packages installed successfully!\n")
