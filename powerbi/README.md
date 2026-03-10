## Power BI Dashboards for Hyperlocal Delivery Intelligence

This folder describes how to connect Power BI to the SQLite data warehouse and which dashboards to build to support executive decision-making.

---

### 1. Connect Power BI to the Data Warehouse

1. Ensure you have run the R ETL scripts:
   - `etl/etl_supply_chain.R`
   - `etl/etl_agent_reviews.R`
   - `etl/build_data_warehouse.R`
2. Confirm that `data/hyperlocal_dw.sqlite` has been created.
3. Open **Power BI Desktop**.
4. Click **Get data** → choose **SQLite database** (or the appropriate connector you have installed).
5. Browse to and select:
   - `data/hyperlocal_dw.sqlite`
6. In the Navigator window, select the following tables:
   - `dim_time`
   - `dim_city`
   - `dim_agent`
   - `fact_delivery`
   - `fact_review`
7. Load the tables into Power BI.

---

### 2. Define Relationships (Star Schema)

In **Model** view in Power BI, ensure these relationships are active:

- `fact_delivery[city_id]` → `dim_city[city_id]`
- `fact_review[city_id]` → `dim_city[city_id]`
- `fact_delivery[time_id]` → `dim_time[time_id]`
- `fact_review[time_id]` → `dim_time[time_id]`
- `fact_review[agent_key]` → `dim_agent[agent_key]`

Cardinality should generally be **Many-to-One** from fact tables to dimensions, with **single** cross-filter direction from dimension to fact.

---

### 3. Dashboard Page 1 – Executive Overview

**Purpose**: Quick view of overall performance.

Recommended visuals:

- **Cards**
  - Total Orders: `COUNT(fact_delivery[delivery_id])`
  - On-Time %: measure = `1 - AVERAGE(fact_delivery[delayed_flag])`
  - Average Delivery Time: `AVERAGE(fact_delivery[delivery_time_min])`
  - Average Rating: `AVERAGE(fact_review[rating])`
- **Clustered column chart**
  - Axis: `dim_city[city]`
  - Values: `Average delivery_time_min`
- **Line chart**
  - Axis: `dim_time[order_date]` (or `Year-Month`)
  - Values: `Average delivery_time_min`
- **Slicers**
  - `dim_city[city]`
  - `fact_delivery[traffic_level]`
  - `fact_delivery[weather]`
  - `fact_delivery[is_festival]`

---

### 4. Dashboard Page 2 – Operations & Delay Analysis

**Purpose**: Understand operational bottlenecks.

- **Matrix / heatmap**
  - Rows: `dim_city[city]`
  - Columns: `fact_delivery[traffic_level]`
  - Values: `Average delivery_time_min`
- **Stacked column chart**
  - Axis: `fact_delivery[vehicle_type]`
  - Values: `Average delivery_time_min`
- **Scatter plot**
  - X: `distance_km`
  - Y: `delivery_time_min`
  - Legend: delayed flag (On-Time vs Delayed)

You can add a calculated column or measure for delay rate by city for deeper analysis.

---

### 5. Dashboard Page 3 – Customer Experience

**Purpose**: View customer satisfaction and review patterns.

- **Bar chart**
  - Axis: `dim_city[city]`
  - Values: `Average rating`
- **Column chart**
  - Axis: rating bucket (e.g., 1–2, 3–4, 5) using a calculated column
  - Values: `Count of reviews`
- **Table**
  - Columns: city, average `order_accuracy`, average `product_availability`, average `rating`

Optionally, you can pre-aggregate text mining outputs in R (e.g., common negative words) and load them as a separate table for display.

---

### 6. Dashboard Page 4 – Agent Performance & Segmentation

After running `analytics/clustering_analysis.R`, a file `data/agent_clusters.csv` is created. You can import this into Power BI and link by `agent_key` or `agent_id`.

Suggested visuals:

- **Scatter plot**
  - X: `avg_delivery_time`
  - Y: `avg_rating`
  - Legend: `cluster`
  - Details: `agent_name`
- **Table**
  - Columns: `agent_name`, `avg_delivery_time`, `delay_rate`, `avg_rating`, `cluster`

This page helps identify:

- Top-performing agents (fast and high-rated)
- Under-performing agents (slow and low-rated)
- Agents who may need training or route optimization

---

### 7. Exporting for Your Report

For a 20-mark academic submission:

- Take **screenshots** of each dashboard page.
- Add them to your written report.
- Under each screenshot, briefly explain the **insights**:
  - How traffic and weather affect delays.
  - Which cities or agents are performing best/worst.
  - How customer ratings relate to operational metrics.

---

### 8. Recommended Kaggle Datasets for Enhancement

To further improve your project with real-world data, consider downloading and integrating these datasets:

1.  **India's Fast Delivery Agents (Blinkit, Zepto, etc.)**
    - [Link to Kaggle](https://www.kaggle.com/datasets/thedevastator/india-fast-delivery-agents-reviews-and-ratings)
    - *Use for*: Real customer sentiment and agent performance benchmarks.
2.  **Amazon Delivery Dataset**
    - [Link to Kaggle](https://www.kaggle.com/datasets/annavictoria/amazon-delivery-dataset)
    - *Use for*: Analyzing weather and traffic impacts on a larger scale.
3.  **Zomato Delivery Operations**
    - [Link to Kaggle](https://www.kaggle.com/datasets/annavictoria/zomato-delivery-operations-analytics-dataset)
    - *Use for*: Hyperlocal food delivery patterns.

### 9. Advanced "Intelligence" Visuals

To "WOW" your evaluators, add these advanced visuals in Power BI:

- **Decomposition Tree**: (Visuals pane -> Decomposition Tree)
  - Analyze "Delay" by breaking it down through City -> Weather -> Traffic Level.
- **Key Influencers**: (Visuals pane -> Key Influencers)
  - Find what "influences" a Rating to be High or a Delivery to be Delayed.
- **Smart Narrative**: (Visuals pane -> Smart Narrative)
  - Automatically generates a text summary of the trends on your page.

