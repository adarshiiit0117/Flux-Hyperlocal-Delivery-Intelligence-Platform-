# Data Pipeline Flow: Flux Hyperlocal Delivery Intelligence

## 1. Data Collection (Raw Sources)
*   **Agent Reviews (`agent_reviews.csv`)**: Customer feedback, ratings, and agent performance metrics.
*   **Supply Chain (`supply_chain.csv`)**: Logistics data, shipping times, costs, and product availability.

## 2. ETL & Cleaning (The "Clean" Phase)
*   **`etl_agent_reviews.R`**: Standardizes ratings, handles missing text, and generates unique IDs.
*   **`etl_supply_chain.R`**: Calculates Service Level Agreements (SLA), categorizes traffic levels, and cleans location data.
*   **Result**: Cleaned `.rds` files ready for integration.

## 3. Data Warehouse Integration (Building the Brain)
*   **`build_data_warehouse.R`**: Implements a **Star Schema** in a SQLite database (`hyperlocal_dw.sqlite`).
    *   **Dimensions**: `dim_time`, `dim_city`, `dim_agent`.
    *   **Facts**: `fact_delivery`, `fact_review`.
*   **`data_augmentation.R`**: Enriches the warehouse with synthetic features like "Weather Severity" and "Traffic Density Index" to enable advanced mining.

## 4. Advanced Data Mining & Analytics
*   **Clustering (`clustering_analysis.R`)**: K-means segmentation of delivery agents into performance tiers (Top Performers vs. Low Rating).
*   **Association Rules (`association_rules.R`)**: Discovery of patterns using the Apriori algorithm (e.g., "High Accuracy" + "Discount" → "High Rating").
*   **Text & Web Mining (`text_mining_reviews.R`, `web_mining.R`)**: 
    *   Sentiment Analysis on customer feedback.
    *   Word Clouds for common service issues.
    *   TF-IDF analysis of review text.
*   **Predictive Modeling (`classification_prediction.R`)**: Decision trees to predict delivery delays and customer complaints.

## 5. OLAP & Insights
*   **OLAP (`olap_analysis.R`)**: Multi-dimensional analysis including slice-and-dice, roll-up (by city), and drill-down (by agent).
*   **Executive Insights**: High-level ROI and fleet efficiency metrics.

## 6. Visualization & Reporting
*   **Interactive Dashboard (`dashboard.R`)**: A real-time R Shiny web application for monitoring the entire ecosystem.
*   **Automated Reporting**: Generation of PDF (`generate_report.R`) and PowerPoint (`generate_pptx.R`) summaries for stakeholders.
