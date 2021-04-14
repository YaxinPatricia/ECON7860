# ECON7860 Big Data Analytics for Business

Project files on the dataset "HR_Analytics"

Instructor: Dr. Wan Shui Ki

Group Members:
LU Qichang    20436343
MIAO Zhaokuan 20431406
OU Jiajie     20436335
PAN Yihan     20428715
YANG Yaxin    20442041


**Folders:**
  - **/R Codes/**: [contains all R code files.]
  - **/Resuls/**
    - **/Feature Importance Plots/**: [Feature importance ranking for all models.]
    - **/Model R Markdown Reports/**: [Compiled reports from "R Codes".]
    - **/R Workspace Files/**: [R workspace files with all results saved in .RData format + results objects.]
  - **/EDA Graphs/**
    - **/Cluster/**: [Item clustering analysis results with the _psych_ package.]
    - **/Correlation matrix/**: [Correlation plots.]
    - **/FA/**: [Factor analysis results and data visualization.]
    - **/PCA/**: [Principal component analysis results.]
    - **/Combined Correlation Matrices.pdf**: [Collection of results in /EDA Graphs/Correlation matrix/]
    - **/Rplot_boxplot.png**: [Boxplots of numeric features for outlier detection.]


**Feature Engineering Settings:**
  1. **Original**: [dataset with only standardization and dummification.]
  2. **Indicator**: [transform _"time_spend_company"_ to an indicator variable (> 5 vs. < 6) + standardization + dummification.]
  3. **Partition 1**: [cases which _"time_spend_company"_ > 5 + standardization + dummification.]
  4. **Partition 0**: [cases which _"time_spend_company"_ <= 5 + standardization + dummification.]
  5. **FA**: [feature extraction with FA. 1 factor replacing variables_ “average_montly_hours”_, _“number_project”_, and _“last_evaluation”_. 1 factor replacing _"satisfaction_level"_.]


**Suffix Annotations:**
  - **"*_org" (or"*_orig")**: [Original setting.]
  - **"*_ind"**: [Indicator setting.]
  - **"*_ind_1"**: [Partition 1 setting.]
  - **"*_ind_0"**: [Partition 0 setting.]
  - **"x"**: [Original dataset.]
  - **"x1"**: [Subset which "time_spend_company" > 5]
  - **"x2"**: [Subset which "time_spend_company" <= 5]


**Notes:**
 1. 3-D data visualization plots with 2 extracted factors are included in /EDA Graphs/FA/x, and /EDA Graphs/FA/x2
 2. 3-D data visualization plots with 2 principal components are included in /EDA Graphs/PCA/x, /EDA Graphs/PCA/x1, and /EDA Graphs/PCA/x2
 3. "sales"_ and _"salary"_ do not make real sense in all correlation plots. We are just too lazy to exclude them.
 4. The list object _results_ are saved in /Results/R Workspace Files/ manually, i.e., saving process not included in codes.
