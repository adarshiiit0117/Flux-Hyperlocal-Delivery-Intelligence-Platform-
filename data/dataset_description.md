# Dataset Description

Dataset Name: Flux Hyperlocal Delivery Data

Source:
Internal simulated delivery and review data prepared for academic data mining workflows.

Description:
This dataset contains delivery operations, agent performance, and customer review text. It supports classification, clustering, association-rule mining, and text mining use cases.

Number of instances:
Approximately 10,000 rows across prepared files.

Number of attributes:
Approximately 30 attributes across delivery, supply, and review features.

Important attributes:
- `delivery_time_min`
- `traffic_level`
- `distance_km`
- `order_value`
- `delayed_flag`
- `rating`
- `review_text`

How to download/use:
1. This repository already includes the working data files under `data/`.
2. If regeneration is required, run preprocessing scripts from `scripts/` starting with `01_data_preparation.R`.
3. For very large/public datasets in future versions, avoid uploading raw files and provide only source links plus download instructions.

Screenshot of dataset structure:
Add a screenshot (table preview or schema view) in this section before final submission.
