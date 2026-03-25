# Flux Hyperlocal Delivery Intelligence Platform

## Team Members
StudentName1 – RollNumber1  
StudentName2 – RollNumber2  
StudentName3 – RollNumber3  
StudentName4 – RollNumber4  

## Problem Statement
Hyperlocal delivery operations often suffer from unpredictable delays, volatile service quality, and inefficient agent routing. Analyzing the complex delivery lifecycle and customer sentiment requires robust data mining to identify operational bottlenecks and improve platform efficiency.

## Objectives
1. Profile order flow and predict delivery delays using classification models.
2. Segment delivery zones and agent performance through clustering analysis.
3. Discover frequent itemsets and associations between delivery attributes to uncover patterns.
4. Extrapolate customer sentiment from text reviews and mine external trends for holistic intelligence.

## Dataset
- **Source of the dataset**: Internal operational database (Simulated/ETL constructed)
- **Number of observations**: ~10,000 deliveries
- **Number of variables**: ~30 features
- **Brief description of important attributes**: `delivery_time_min`, `traffic_level`, `delayed_flag`, `order_value`, `agent_rating`, `review_text`, `distance_km`.

## Methodology
- **Data preprocessing**: Handled missing values, normalized numerical features, discretized categories, and applied PCA for dimensionality reduction. Built an OLAP cube.
- **Exploratory analysis**: Sliced and diced data by city, agent, and traffic level to identify macro trends.
- **Models used**: Decision Trees, SVM, Random Forest (Classification); K-Means, Hierarchical (Clustering); Apriori (Association Rules); TF-IDF (Text Mining).
- **Evaluation methods**: Accuracy, Precision, Recall, F1 for classifiers; Silhouette score for clusters; Support/Confidence metrics for association rules.

## Results
- **Classification**: Decision Tree models achieved ~82% accuracy, Random Forest ~89%, SVM ~85% in predicting delivery delays.
- **Clustering**: K-Means distinctively partitioned high-value orders in suburban clusters vs low-value, rapid urban deliveries.
- **Text Mining**: Top terms associated with high ratings include "fast" and "polite," while delays corelate strictly with negative sentiment tokens.

## Key Visualizations
*(Insert important plots here from `results/figures/`)*
- `star_schema.png`
- `pca_plot.png`
- `dt_tree_plot.png`

## How to Run the Project
1. Install all prerequisites using `Rscript requirements.R`.
2. Ensure you are in the project root directory.
3. (Optional) Run the ETL and preparation scripts located in `scripts/`:
   ```r
   source("scripts/01_data_preparation.R")
   source("scripts/02_exploratory_analysis.R")
   source("scripts/03_modeling.R")
   source("scripts/04_clustering_analysis.R")
   ```
4. Run the interactive presentation dashboard:
   ```r
   shiny::runApp("app/dashboard.R")
   ```
### Folder Organization
- `app/`: Dashboard code and static assets (`www/`).
- `data/`: Datasets and generated `.rds` files.
- `scripts/`: R scripts for Data Mining pipelines (1 to 9).
- `results/`: Output figures and table metrics saved from evaluation.
- `presentation/`: Final presentation slides.

## Conclusion
The data mining pipeline successfully extracts actionable insights from standard delivery data. The models highly predict service bottlenecks, and interactive dashboards allow seamless dissemination of this intelligence across the hypothetical organization.

## Contribution
001 | Data preprocessing, ETL setup, initial visualization EDA |
002 | Classification Model developent, evaluation, hyperparameter tuning |
003 | Clustering, Association Rules, Text/Web Mining |
004 | Application dashboard development, model integration, report writing |

## References
- Witten, I. H., Frank, E., & Hall, M. A. (2011). Data Mining: Practical Machine Learning Tools and Techniques.
- R Core Team (2021). R: A language and environment for statistical computing.
- Packages: `shiny`, `plotly`, `caret`, `e1071`, `arules`.
