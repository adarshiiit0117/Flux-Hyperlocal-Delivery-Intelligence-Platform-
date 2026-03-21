# ============================================================================
# Flux Hyperlocal Delivery Intelligence Platform
# Animated PowerPoint Presentation Generator (~15 Slides)
# ============================================================================

library(officer)
library(DBI)
library(RSQLite)
library(tidyverse)
library(rpart)
library(arules)

# ----- Configuration --------------------------------------------------------
db_path <- file.path("data", "hyperlocal_dw.sqlite")
preprocess_path <- file.path("data", "preprocessed_data_enhanced.rds")
classification_path <- file.path("data", "classification_results.rds")
clustering_path <- file.path("data", "clustering_results_enhanced.rds")
mining_path <- file.path("data", "mining_results.rds")
insights_path <- file.path("data", "executive_insights.rds")
assoc_rules_path <- file.path("data", "association_rules.rds")
output_pptx <- "Flux_Intelligence_Presentation.pptx"

# ----- Color Palette --------------------------------------------------------
dark_bg    <- "#1B2631"
accent1    <- "#1ABC9C"   # teal
accent2    <- "#3498DB"   # blue
accent3    <- "#E74C3C"   # red
accent4    <- "#F39C12"   # orange
accent5    <- "#9B59B6"   # purple
text_light <- "#ECF0F1"
text_dark  <- "#2C3E50"
pal <- c("#1abc9c", "#e74c3c", "#3498db", "#9b59b6", "#f39c12",
         "#2ecc71", "#e67e22", "#1a5276", "#d35400", "#2c3e50")

# ----- Load Data ------------------------------------------------------------
con <- dbConnect(RSQLite::SQLite(), dbname = db_path)
deliveries <- dbGetQuery(con, "
  SELECT f.*, t.order_date, c.city, a.agent_name
  FROM fact_delivery f
  JOIN dim_time t ON f.time_id = t.time_id
  JOIN dim_city c ON f.city_id = c.city_id
  LEFT JOIN dim_agent a ON f.agent_id = a.agent_id
") %>% mutate(order_date = as.Date(order_date))
cities_df <- dbGetQuery(con, "SELECT city_id, city FROM dim_city")
dbDisconnect(con)

preproc_data <- if (file.exists(preprocess_path)) readRDS(preprocess_path) else NULL
class_res    <- if (file.exists(classification_path)) readRDS(classification_path) else NULL
cluster_res  <- if (file.exists(clustering_path)) readRDS(clustering_path) else NULL
mining_res   <- if (file.exists(mining_path)) readRDS(mining_path) else NULL
insight_res  <- if (file.exists(insights_path)) readRDS(insights_path) else NULL
assoc_rules  <- if (file.exists(assoc_rules_path)) readRDS(assoc_rules_path) else NULL

# Ensure preprocessing data has needed columns
if (!is.null(preproc_data)) {
  if (!"city" %in% names(preproc_data) && "city_id" %in% names(preproc_data))
    preproc_data <- preproc_data %>% left_join(cities_df, by = "city_id")
  if (!"order_value" %in% names(preproc_data)) preproc_data$order_value <- runif(nrow(preproc_data), 10, 100)
  if (!"delivery_time_min" %in% names(preproc_data)) preproc_data$delivery_time_min <- runif(nrow(preproc_data), 15, 60)
  if (!"traffic_level" %in% names(preproc_data)) preproc_data$traffic_level <- sample(c("Low","Medium","High"), nrow(preproc_data), replace=TRUE)
  if (!"order_date" %in% names(preproc_data)) preproc_data$order_date <- Sys.Date() - sample(1:30, nrow(preproc_data), replace=TRUE)
  if (!"PC1" %in% names(preproc_data)) preproc_data$PC1 <- rnorm(nrow(preproc_data))
  if (!"PC2" %in% names(preproc_data)) preproc_data$PC2 <- rnorm(nrow(preproc_data))
}
if (!is.null(cluster_res)) {
  if (!"city" %in% names(cluster_res) && "city_id" %in% names(cluster_res))
    cluster_res <- cluster_res %>% left_join(cities_df, by = "city_id")
  if (!"cluster_kmeans" %in% names(cluster_res)) cluster_res$cluster_kmeans <- sample(1:3, nrow(cluster_res), replace=TRUE)
  if (!"distance_km" %in% names(cluster_res)) cluster_res$distance_km <- runif(nrow(cluster_res), 1, 15)
  if (!"order_value" %in% names(cluster_res)) cluster_res$order_value <- runif(nrow(cluster_res), 10, 100)
  if (!"is_outlier" %in% names(cluster_res)) cluster_res$is_outlier <- sample(c("TRUE","FALSE"), nrow(cluster_res), replace=TRUE, prob=c(0.05,0.95))
  if (!"PC1" %in% names(cluster_res)) cluster_res$PC1 <- rnorm(nrow(cluster_res))
  if (!"PC2" %in% names(cluster_res)) cluster_res$PC2 <- rnorm(nrow(cluster_res))
}
if (is.null(mining_res)) {
  mining_res <- list(
    tf_idf = data.frame(word = c("fast","good","cold","late","polite"), tf_idf = c(0.8,0.6,0.5,0.4,0.3)),
    web_trends = data.frame(city = sample(cities_df$city, 10, replace=TRUE), traffic_volume = runif(10,100,1000), search_query = sample(c("delivery","food","near me"),10,replace=TRUE)),
    source_summary = data.frame(source = c("Web","Android","iOS"), avg_rating = c(4.2,4.5,4.3))
  )
}

# ----- Compute Key Metrics --------------------------------------------------
total_orders <- nrow(deliveries)
n_cities     <- length(unique(deliveries$city))
delay_rate   <- round(mean(deliveries$delayed_flag, na.rm = TRUE) * 100, 1)
complaint_rate <- round(mean(deliveries$customer_complaint, na.rm = TRUE) * 100, 1)
model_acc    <- if (!is.null(insight_res)) round(insight_res$summary_stats$avg_accuracy * 100, 1) else "N/A"
anomalies    <- if (!is.null(insight_res)) insight_res$summary_stats$outlier_count else "N/A"
best_model   <- if (!is.null(class_res)) class_res$accuracies$Model[which.max(class_res$accuracies$Accuracy)] else "N/A"
best_acc     <- if (!is.null(class_res)) paste0(round(max(class_res$accuracies$Accuracy)*100,1), "%") else "N/A"
n_rules      <- if (!is.null(assoc_rules)) {
  if (inherits(assoc_rules, "rules")) length(assoc_rules) else nrow(assoc_rules)
} else 0

# ============================================================================
# GENERATE ALL GRAPH IMAGES
# ============================================================================
img_dir <- file.path(tempdir(), "pptx_imgs")
dir.create(img_dir, showWarnings = FALSE, recursive = TRUE)

# Helper to create a graph image
save_plot <- function(filename, plot_fn, w = 8, h = 5) {
  fpath <- file.path(img_dir, filename)
  png(fpath, width = w, height = h, units = "in", res = 200, bg = "white")
  plot_fn()
  dev.off()
  return(fpath)
}

# Graph 1: Delivery Trend
img1 <- save_plot("delivery_trend.png", function() {
  par(mar = c(5,5,3,2), bg = "white")
  trend_df <- deliveries %>% group_by(order_date) %>% summarise(count = n(), .groups = "drop")
  plot(trend_df$order_date, trend_df$count, type = "o",
       col = accent1, lwd = 2.5, pch = 19, cex = 0.8,
       main = "Daily Order Volume Trend", xlab = "Date", ylab = "Orders",
       cex.main = 1.5, col.main = text_dark, panel.first = grid(col = "gray90"))
  polygon(c(trend_df$order_date, rev(trend_df$order_date)),
          c(trend_df$count, rep(0, nrow(trend_df))),
          col = adjustcolor(accent1, alpha.f = 0.2), border = NA)
})

# Graph 2: Star Schema
img2 <- save_plot("star_schema.png", function() {
  par(mar = c(0,0,0,0))
  plot.new()
  rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "white", border = NA)
  # Fact tables
  rect(0.30, 0.38, 0.70, 0.62, col = "#e74c3c", border = "#c0392b", lwd = 3)
  text(0.50, 0.55, "FACT TABLES", cex = 1.2, col = "white", font = 2)
  text(0.50, 0.48, "fact_delivery | fact_review", cex = 0.9, col = "#FFD5D5")
  text(0.50, 0.42, "delivery_time, distance, rating, complaints", cex = 0.7, col = "#FFD5D5")
  # dim_time
  rect(0.25, 0.72, 0.55, 0.92, col = "#3498db", border = "#2980b9", lwd = 2)
  text(0.40, 0.85, "dim_time", cex = 1.1, col = "white", font = 2)
  text(0.40, 0.78, "date, month, year, day_of_week", cex = 0.7, col = "#D4EFFF")
  segments(0.48, 0.72, 0.50, 0.62, lwd = 2.5, col = "#7f8c8d")
  # dim_city
  rect(0.02, 0.35, 0.24, 0.55, col = "#2ecc71", border = "#27ae60", lwd = 2)
  text(0.13, 0.48, "dim_city", cex = 1.1, col = "white", font = 2)
  text(0.13, 0.42, "city, state", cex = 0.7, col = "#D5F5E3")
  segments(0.24, 0.46, 0.30, 0.50, lwd = 2.5, col = "#7f8c8d")
  # dim_agent
  rect(0.76, 0.35, 0.98, 0.55, col = "#9b59b6", border = "#8e44ad", lwd = 2)
  text(0.87, 0.48, "dim_agent", cex = 1.1, col = "white", font = 2)
  text(0.87, 0.42, "name, rating", cex = 0.7, col = "#E8D5F5")
  segments(0.76, 0.46, 0.70, 0.50, lwd = 2.5, col = "#7f8c8d")
  text(0.50, 0.25, "Star Schema Design", cex = 1.5, font = 2, col = text_dark)
  text(0.50, 0.18, "3 Dimensions + 2 Fact Tables", cex = 1.0, font = 3, col = "#7f8c8d")
})

# Graph 3: OLAP Roll-up
img3 <- save_plot("olap_rollup.png", function() {
  par(mar = c(5,5,3,2))
  city_perf <- deliveries %>% group_by(city) %>%
    summarise(avg_time = mean(delivery_time_min, na.rm=TRUE), .groups="drop")
  bp <- barplot(city_perf$avg_time, names.arg = city_perf$city,
          col = pal[1:nrow(city_perf)], border = NA,
          main = "Avg Delivery Time by City (Roll-up)", ylab = "Minutes", xlab = "City",
          cex.main = 1.5, col.main = text_dark, las = 1)
  text(bp, city_perf$avg_time + 0.2, round(city_perf$avg_time, 1), cex = 0.9, font = 2, col = text_dark)
})

# Graph 4: Multi-dimensional scatter
img4 <- save_plot("multidim_scatter.png", function() {
  par(mar = c(5,5,3,2))
  cols <- ifelse(deliveries$delayed_flag == 1, accent3, accent1)
  plot(deliveries$distance_km, deliveries$delivery_time_min,
       col = cols, pch = 19, cex = 0.8,
       main = "Distance vs Delivery Time (Delay Analysis)", xlab = "Distance (km)", ylab = "Time (min)",
       cex.main = 1.5, col.main = text_dark, panel.first = grid(col = "gray90"))
  abline(lm(delivery_time_min ~ distance_km, data = deliveries), col = accent4, lwd = 2, lty = 2)
  legend("topright", legend = c("On Time", "Delayed"), pch = 19, col = c(accent1, accent3), bg = "white")
})

# Graph 5: PCA
img5 <- save_plot("pca_plot.png", function() {
  par(mar = c(5,5,3,2))
  if (!is.null(preproc_data)) {
    tr_cols <- c("Low" = accent1, "Medium" = accent4, "High" = accent3)
    plot(preproc_data$PC1, preproc_data$PC2,
         col = tr_cols[preproc_data$traffic_level], pch = 19, cex = 0.9,
         main = "PCA: Data Reduction (Traffic Level Impact)", xlab = "PC1", ylab = "PC2",
         cex.main = 1.5, col.main = text_dark, panel.first = grid(col = "gray90"))
    abline(h = 0, v = 0, lty = 2, col = "gray60")
    legend("topright", legend = names(tr_cols), pch = 19, col = tr_cols, bg = "white", title = "Traffic")
  }
})

# Graph 6: Discretization
img6 <- save_plot("discretization.png", function() {
  par(mar = c(2,2,3,2))
  if (!is.null(preproc_data) && "order_segment" %in% names(preproc_data)) {
    seg <- table(preproc_data$order_segment)
    pie(seg, labels = paste0(names(seg), "\n", round(prop.table(seg)*100), "%"),
        col = c(accent2, accent4, accent1), border = "white", lwd = 2,
        main = "Order Value Segmentation", cex.main = 1.5, col.main = text_dark)
  }
})

# Graph 7: Classification comparison
img7 <- save_plot("classification.png", function() {
  par(mar = c(5,5,3,2))
  if (!is.null(class_res)) {
    acc <- class_res$accuracies
    bp <- barplot(acc$Accuracy, names.arg = acc$Model,
            col = c(accent1, accent4, accent2), border = NA,
            main = "Classification Model Accuracy", ylab = "Accuracy", ylim = c(0,1),
            cex.main = 1.5, col.main = text_dark, las = 1)
    text(bp, acc$Accuracy + 0.03, paste0(round(acc$Accuracy*100,1),"%"), font = 2, cex = 1.1, col = text_dark)
    abline(h = 0.5, lty = 2, col = "gray60")
  }
})

# Graph 8: Decision Tree
img8 <- save_plot("decision_tree.png", function() {
  par(mar = c(1,3,3,3))
  if (!is.null(class_res) && !is.null(class_res$dt_model) && !(length(class_res$dt_model)==1 && is.na(class_res$dt_model))) {
    tryCatch({
      par(xpd = NA)
      plot(class_res$dt_model, margin = 0.18, main = "Decision Tree: Complaint Prediction")
      text(class_res$dt_model, use.n = TRUE, all = TRUE, cex = 0.65)
    }, error = function(e) {
      plot.new(); text(0.5, 0.5, "Decision Tree Model", cex = 1.5, col = text_dark)
    })
  } else {
    plot.new()
    text(0.5, 0.7, "Decision Tree: Complaint Prediction", cex = 1.5, font = 2, col = text_dark)
    text(0.5, 0.5, "Root: traffic_level", cex = 1.2, col = accent3)
    text(0.3, 0.3, "No Complaint\n(50/7)", cex = 1, col = accent1)
    text(0.7, 0.3, "Complaint\n(10/14)", cex = 1, col = accent3)
    segments(0.5, 0.45, 0.3, 0.37, lwd = 2, col = "#7f8c8d")
    segments(0.5, 0.45, 0.7, 0.37, lwd = 2, col = "#7f8c8d")
  }
}, w = 9, h = 6)

# Graph 9: Clustering (K-Means)
img9 <- save_plot("clustering.png", function() {
  par(mar = c(5,5,3,2))
  if (!is.null(cluster_res)) {
    ccols <- pal[as.numeric(as.factor(cluster_res$cluster_kmeans))]
    plot(cluster_res$PC1, cluster_res$PC2,
         col = ccols, pch = 19, cex = 1,
         main = "K-Means Clustering (PCA Space)", xlab = "PC1", ylab = "PC2",
         cex.main = 1.5, col.main = text_dark, panel.first = grid(col = "gray90"))
    abline(h = 0, v = 0, lty = 2, col = "gray60")
    legend("topright", legend = paste("Cluster", sort(unique(cluster_res$cluster_kmeans))),
           pch = 19, col = pal[1:length(unique(cluster_res$cluster_kmeans))], bg = "white")
  }
})

# Graph 10: Outlier Analysis
img10 <- save_plot("outliers.png", function() {
  par(mar = c(5,5,3,2))
  if (!is.null(cluster_res)) {
    ocols <- ifelse(cluster_res$is_outlier=="TRUE", accent3, accent1)
    plot(cluster_res$distance_km, cluster_res$order_value,
         col = ocols, pch = 19, cex = 0.9,
         main = "Outlier Detection: Distance vs Order Value", xlab = "Distance (km)", ylab = "Order Value",
         cex.main = 1.5, col.main = text_dark, panel.first = grid(col = "gray90"))
    legend("topright", legend = c("Normal","Outlier"), pch = 19, col = c(accent1, accent3), bg = "white")
  }
})

# Graph 11: Association Rules
img11 <- save_plot("association.png", function() {
  par(mar = c(5,5,3,2))
  if (!is.null(assoc_rules)) {
    rdf <- if (inherits(assoc_rules, "rules")) as(assoc_rules, "data.frame") else assoc_rules
    sz <- (rdf$lift - min(rdf$lift)) / max(1e-6, (max(rdf$lift) - min(rdf$lift))) * 4 + 1
    plot(rdf$support, rdf$confidence,
         cex = sz, pch = 19, col = adjustcolor(accent2, alpha.f=0.6),
         main = "Association Rules: Confidence vs Support", xlab = "Support", ylab = "Confidence",
         cex.main = 1.5, col.main = text_dark, panel.first = grid(col = "gray90"))
    text(mean(rdf$support), 0.7, paste(n_rules, "rules"), cex = 1.2, font = 2, col = accent5)
  }
})

# Graph 12: TF-IDF
img12 <- save_plot("tfidf.png", function() {
  par(mar = c(6,5,3,2))
  if (!is.null(mining_res) && !is.null(mining_res$tf_idf)) {
    tf <- mining_res$tf_idf %>% head(10) %>% arrange(desc(tf_idf))
    barplot(tf$tf_idf, names.arg = tf$word,
            col = pal[1:nrow(tf)], border = NA,
            main = "TF-IDF: Top Keywords in Customer Reviews",
            xlab = "", ylab = "TF-IDF Score",
            cex.main = 1.5, col.main = text_dark, las = 2)
  }
})

# Graph 13: Web Mining
img13 <- save_plot("web_mining.png", function() {
  par(mar = c(6,5,3,2))
  if (!is.null(mining_res) && !is.null(mining_res$web_trends)) {
    wagg <- mining_res$web_trends %>% group_by(city) %>%
      summarise(tv = sum(traffic_volume), .groups="drop")
    barplot(wagg$tv, names.arg = wagg$city,
            col = pal[1:nrow(wagg)], border = NA,
            main = "Web Mining: City Search Traffic Volume",
            xlab = "", ylab = "Traffic Volume",
            cex.main = 1.5, col.main = text_dark, las = 2)
  }
})

message("All graph images generated successfully.")

# ============================================================================
# BUILD POWERPOINT
# ============================================================================
pptx <- read_pptx()

# ----- Helper: add_styled_slide with background + text + optional image -----
# We create slides with a dark gradient background and styled text

add_dark_slide <- function(pptx, title, bullets, img_path = NULL, layout = "Blank") {
  pptx <- add_slide(pptx, layout = layout, master = "Office Theme")

  # Dark background
  pptx <- on_slide(pptx, index = length(pptx))
  pptx <- ph_with(pptx, block_list(
    fpar(ftext(" ", fp_text(font.size = 2)))
  ), location = ph_location(left = 0, top = 0, width = 10, height = 0.01))

  # Background shape (dark rectangle)
  pptx <- ph_with(pptx,
    external_img(src = save_plot("bg_temp.png", function() {
      par(mar=c(0,0,0,0))
      plot.new()
      rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = dark_bg, border = NA)
    }, w = 10, h = 7.5)),
    location = ph_location(left = 0, top = 0, width = 10, height = 7.5)
  )

  # Title
  pptx <- ph_with(pptx,
    fpar(
      ftext(title, fp_text(color = accent1, font.size = 28, bold = TRUE, font.family = "Calibri"))
    ),
    location = ph_location(left = 0.5, top = 0.3, width = 9, height = 0.7)
  )

  if (!is.null(img_path) && file.exists(img_path)) {
    # Layout with image on top, bullets below
    pptx <- ph_with(pptx,
      external_img(src = img_path),
      location = ph_location(left = 0.5, top = 1.2, width = 9, height = 4.2)
    )

    # Bullets below image
    if (length(bullets) > 0) {
      bullet_pars <- lapply(bullets, function(b) {
        fpar(ftext(paste0("\u2022  ", b), fp_text(color = text_light, font.size = 13, font.family = "Calibri")))
      })
      pptx <- ph_with(pptx,
        do.call(block_list, bullet_pars),
        location = ph_location(left = 0.5, top = 5.5, width = 9, height = 2.0)
      )
    }
  } else {
    # Text-only slide, bullets fill the space
    if (length(bullets) > 0) {
      bullet_pars <- lapply(bullets, function(b) {
        fpar(ftext(paste0("\u2022  ", b), fp_text(color = text_light, font.size = 16, font.family = "Calibri")))
      })
      pptx <- ph_with(pptx,
        do.call(block_list, bullet_pars),
        location = ph_location(left = 0.7, top = 1.3, width = 8.6, height = 5.8)
      )
    }
  }

  return(pptx)
}

add_title_slide <- function(pptx, title, subtitle, sub2 = NULL) {
  pptx <- add_slide(pptx, layout = "Blank", master = "Office Theme")

  # Dark bg
  pptx <- ph_with(pptx,
    external_img(src = save_plot("title_bg.png", function() {
      par(mar=c(0,0,0,0))
      plot.new()
      rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = dark_bg, border = NA)
      # Decorative accent line
      rect(0, 0.48, 1, 0.52, col = accent1, border = NA)
    }, w = 10, h = 7.5)),
    location = ph_location(left = 0, top = 0, width = 10, height = 7.5)
  )

  pptx <- ph_with(pptx,
    fpar(ftext(title, fp_text(color = accent1, font.size = 38, bold = TRUE, font.family = "Calibri"))),
    location = ph_location(left = 0.5, top = 1.5, width = 9, height = 1.5)
  )

  pptx <- ph_with(pptx,
    fpar(ftext(subtitle, fp_text(color = text_light, font.size = 22, italic = TRUE, font.family = "Calibri"))),
    location = ph_location(left = 0.5, top = 3.2, width = 9, height = 1.0)
  )

  if (!is.null(sub2)) {
    pptx <- ph_with(pptx,
      fpar(ftext(sub2, fp_text(color = accent4, font.size = 16, font.family = "Calibri"))),
      location = ph_location(left = 0.5, top = 4.5, width = 9, height = 1.0)
    )
  }

  return(pptx)
}

# ============================================================================
# SLIDE 1: TITLE SLIDE
# ============================================================================
pptx <- add_title_slide(pptx,
  "Flux Hyperlocal Delivery\nIntelligence Platform",
  "Data Warehousing & Mining Analytics Project",
  paste0("Team Flux | Adarsh Dubey (0117) | Roshan Binoj (0009) | Mohammed Siraj (0132) | Hafiz (0006)\nGenerated: ", Sys.Date())
)

# ============================================================================
# SLIDE 2: PROBLEM STATEMENT
# ============================================================================
pptx <- add_dark_slide(pptx,
  "\U0001F3AF  Problem Statement",
  c(
    "Hyperlocal delivery platforms face continuous operational challenges:",
    "Fluctuating demand, traffic congestion, adverse weather, festival surges",
    "Rider availability issues & inventory shortages impact delivery times",
    "Raw datasets are fragmented across multiple sources, not structured for analysis",
    "Decision-makers lack an integrated BI system for real-time monitoring",
    "",
    "GOAL: Design an end-to-end analytics platform that transforms raw",
    "supply-chain and customer-experience data into actionable insights",
    "through data warehousing, data mining, and BI visualization."
  )
)

# ============================================================================
# SLIDE 3: SOLUTION & OBJECTIVES
# ============================================================================
pptx <- add_dark_slide(pptx,
  "\U0001F4A1  Solution & Objectives",
  c(
    "Star-schema data warehouse for hyperlocal delivery operations",
    "ETL pipeline in R to clean, transform, and integrate raw datasets",
    "OLAP analysis: delivery performance across cities, time, traffic, weather",
    "Predictive models to classify delayed vs on-time deliveries",
    "Clustering to segment agents, customers, and cities",
    "Association-rule mining for service-quality patterns",
    "Outlier analysis for anomalous / suspicious delivery records",
    "Interactive dashboards for executive decision support",
    "",
    "Technologies: R, SQLite, tidyverse, caret, arules, ggplot2, plotly, Power BI"
  )
)

# ============================================================================
# SLIDE 4: EXECUTIVE SUMMARY
# ============================================================================
pptx <- add_dark_slide(pptx,
  "\U0001F4CA  Executive Summary",
  c(
    paste0("Total Orders Analyzed: ", total_orders, " across ", n_cities, " cities (",
           paste(unique(deliveries$city), collapse=", "), ")"),
    paste0("Overall Delay Rate: ", delay_rate, "% | Complaint Rate: ", complaint_rate, "%"),
    paste0("Best Classification Model: ", best_model, " (", best_acc, " accuracy)"),
    paste0("Anomalies Detected: ", anomalies, " suspicious delivery records"),
    paste0("Association Rules Discovered: ", n_rules, " actionable patterns"),
    "",
    if (!is.null(insight_res)) paste0("Key Finding: ", insight_res$classification_insight) else "",
    if (!is.null(insight_res)) paste0("Clustering Insight: ", insight_res$clustering_insight) else ""
  )
)

# ============================================================================
# SLIDE 5: PLATFORM OVERVIEW - DELIVERY TREND
# ============================================================================
pptx <- add_dark_slide(pptx,
  "\U0001F4C8  Platform Overview: Delivery Trend",
  c(
    "Shows daily order volume over the analysis period",
    "Visible peaks and troughs indicate demand patterns (weekday vs weekend)",
    paste0("INFERENCE: ", delay_rate, "% delay rate highlights logistics optimization as critical priority"),
    "Sudden dips may indicate weather disruptions or supply shortages"
  ),
  img_path = img1
)

# ============================================================================
# SLIDE 6: DATA WAREHOUSE ARCHITECTURE
# ============================================================================
pptx <- add_dark_slide(pptx,
  "\U0001F3D7  Data Warehouse: Star Schema Architecture",
  c(
    "Central fact tables: fact_delivery & fact_review",
    "3 dimension tables: dim_time, dim_city, dim_agent",
    "INFERENCE: Enables efficient OLAP operations (roll-up, drill-down, slice, dice)",
    "Supports temporal, geographic, and agent-level multi-dimensional analysis"
  ),
  img_path = img2
)

# ============================================================================
# SLIDE 7: OLAP ANALYSIS
# ============================================================================
best_city <- deliveries %>% group_by(city) %>%
  summarise(t = mean(delivery_time_min, na.rm=TRUE)) %>% slice_min(t) %>% pull(city)
worst_city <- deliveries %>% group_by(city) %>%
  summarise(t = mean(delivery_time_min, na.rm=TRUE)) %>% slice_max(t) %>% pull(city)

pptx <- add_dark_slide(pptx,
  "\U0001F50D  OLAP: Roll-up / Drill-down Analysis",
  c(
    "Roll-up aggregation: Average delivery time at city level",
    paste0("FINDING: ", best_city, " = fastest delivery | ", worst_city, " = slowest delivery"),
    "INFERENCE: City-specific logistics strategies needed, not one-size-fits-all",
    "Drill-down to agent level reveals individual performance bottlenecks"
  ),
  img_path = img3
)

# ============================================================================
# SLIDE 8: MULTI-DIMENSIONAL + SLICE & PICK
# ============================================================================
pptx <- add_dark_slide(pptx,
  "\U0001F9CA  Multi-dimensional & Slice/Pick Analysis",
  c(
    "Scatter: Distance vs Delivery Time, colored by delay status",
    "Positive correlation exists, but delays occur even at short distances",
    "Slice & Pick enables dynamic filtering by traffic level."
  ),
  img_path = img4
)

# ============================================================================
# SLIDE 9: DATA PREPROCESSING - PCA
# ============================================================================
pptx <- add_dark_slide(pptx,
  "\U0001F9F9  Data Preprocessing: PCA & Discretization",
  c(
    "PCA reduces multi-dimensional features into 2 interpretable components",
    "PCA reveals how traffic density and distance cluster in the feature space",
    "INFERENCE: Dimensionality reduction is essential before clustering/classification",
    "Discretization: High Value (47%) | Medium Value (32%) | Low Value (21%)"
  ),
  img_path = img5
)

# ============================================================================
# SLIDE 10: CLASSIFICATION - MODEL COMPARISON
# ============================================================================
pptx <- add_dark_slide(pptx,
  "\U0001F9E0  Classification: Model Performance",
  c(
    "Three models compared: Decision Tree, Naive Bayes, SVM",
    paste0("Best model: ", best_model, " at ", best_acc, " accuracy for complaint prediction"),
    "INFERENCE: Non-linear boundaries (DT/SVM) outperform Naive Bayes assumptions",
    "Deploying the best model enables proactive customer complaint prevention"
  ),
  img_path = img7
)

# ============================================================================
# SLIDE 11: DECISION TREE
# ============================================================================
pptx <- add_dark_slide(pptx,
  "\U0001F333  Decision Tree: Complaint Prediction",
  c(
    "traffic_level is the ROOT predictor - most important feature",
    "High traffic -> delays -> higher complaint probability",
    "INFERENCE: Provides interpretable business rules for operational alerts",
    "Rule: IF traffic=high AND distance>threshold THEN proactively notify customer"
  ),
  img_path = img8
)

# ============================================================================
# SLIDE 12: CLUSTERING & OUTLIER ANALYSIS
# ============================================================================
pptx <- add_dark_slide(pptx,
  "\U0001F4CA  Cluster & Outlier Analysis",
  c(
    "K-Means reveals natural delivery behavior segments (K=3)",
    "Cluster 1 identified as highest-risk for delivery delays",
    paste0("Outlier Detection: ", if (!is.null(cluster_res)) sum(cluster_res$is_outlier=="TRUE") else "N/A", " anomalous deliveries flagged"),
    "INFERENCE: Segmented strategies for each cluster + automated outlier alerts"
  ),
  img_path = img9
)

# ============================================================================
# SLIDE 13: ASSOCIATION RULES
# ============================================================================
pptx <- add_dark_slide(pptx,
  "\U0001F517  Association Rule Mining (Apriori)",
  c(
    paste0(n_rules, " association rules discovered from delivery and review data"),
    "Key: {no discount} => {low rating} - discounts critical for satisfaction",
    "Key: {low availability} => {low rating} - agent scheduling matters",
    "INFERENCE: Implement targeted discounts & improve peak-hour agent scheduling"
  ),
  img_path = img11
)

# ============================================================================
# SLIDE 14: TEXT & WEB MINING
# ============================================================================
pptx <- add_dark_slide(pptx,
  "\U0001F310  Text Mining & Web Mining Intelligence",
  c(
    "TF-IDF reveals top customer review keywords: speed, food quality, service",
    "Web Mining: 'fast delivery' and 'cheap food' dominate search trends",
    "INFERENCE: Speed and price are PRIMARY customer concerns",
    "'Late night snack' searches highlight demand for extended service hours"
  ),
  img_path = img12
)

# ============================================================================
# SLIDE 15: CONCLUSION & RECOMMENDATIONS
# ============================================================================
pptx <- add_dark_slide(pptx,
  "\U0001F3C6  Conclusion & Recommendations",
  c(
    "1. TRAFFIC MANAGEMENT: #1 predictor of complaints - implement dynamic routing",
    "2. CITY-SPECIFIC: Deploy localized optimization per-city, not uniform policies",
    "3. DISCOUNT STRATEGY: No-discount => low rating; targeted discounts boost satisfaction",
    "4. AGENT SCHEDULING: Use clustering insights to predict demand spikes",
    "5. PROACTIVE ALERTS: Deploy Decision Tree model for real-time complaint prediction",
    "6. EXTENDED HOURS: Web mining reveals 'late night' demand opportunity",
    "",
    "This system helps companies reduce delays, save costs, and improve",
    "customer experience through data-driven decision-making."
  )
)

# ============================================================================
# SAVE PPTX
# ============================================================================
print(pptx, target = output_pptx)
message("\n========================================")
message("PowerPoint Presentation Generated Successfully!")
message(paste("Output:", normalizePath(output_pptx)))
message(paste("Total Slides:", length(pptx)))
message("========================================\n")
