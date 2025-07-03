# 🏠 Data Analysis: House Prices Regression Analysis (R)

An end-to-end exploratory data analysis and regression modeling project using the Ames housing dataset to understand the impact of **numerical** and **categorical** variables on house prices. Conducted as part of the *MH3511: Data Analysis with Computer* course at NTU.

---

## 🔍 Project Overview

This project involves:

1. **Data Cleaning & Transformation**
   - Removed irrelevant and non-numeric variables
   - Log-transformed skewed numeric variables (e.g., SalePrice, LotArea)
   - Removed outliers using IQR filtering

2. **Feature Engineering**
   - Created new features such as `totalSF` (total square footage)
   - Combined similar factor levels in categorical variables (e.g., brick types)

3. **Exploratory Data Analysis**
   - Correlation matrix and reordering of variables based on `SalePrice`
   - Histograms and boxplots for visual inspection of distributions
   - Spearman rank correlations for ordinal/time-based variables (e.g., `YearBuilt`)

4. **Statistical Tests**
   - Levene’s test for equal variances
   - Kruskal-Wallis test for non-parametric group comparison
   - T-tests for mean comparisons between binary categories

---

## 📊 Key Variables Analyzed

### 🔢 Numerical Variables (log-transformed)
- `logLotArea`  
- `logGarageArea`  
- `logTotalBsmtSF`  
- `logtotalSF`  
- `logSalePrice`  

### 📆 Temporal / Ordinal
- `YearBuilt`  
  - Spearman correlation and scatterplots show moderate relationship with `logSalePrice`

### 🔤 Categorical Variables
- `MSSubClass`  
  - Grouped into: *1story*, *2story*, *Split*
- `MasVnrType`  
  - Grouped into: *Brick*, *None*, *Stone*
- `LotShape`  
  - Grouped into: *IR* (irregular), *Reg*

---

## 📈 Insights

- **Log-transformations** improved normality and linear relationships in numerical predictors.
- **Outlier removal** significantly reduced variance in log-transformed variables.
- **MSSubClass** showed statistically significant differences in median sale price (Kruskal-Wallis p < 0.001).
- **MasVnrType** and **LotShape** also contributed meaningfully to price variance.
- **YearBuilt** has a positive Spearman correlation with price but is not linearly associated.

---

## 🛠 Tools & Packages

- **Language**: R  
- **Libraries**: `ggplot2`, `dplyr`, `corrplot`, `PerformanceAnalytics`, `car`, `ggpubr`

---

## 📁 Data Source

- Dataset: [`train.csv`](https://www.kaggle.com/competitions/house-prices-advanced-regression-techniques/data)
  - From the **Kaggle House Prices** competition
  - Cleaned to exclude rare house types and properties with pools (due to sparsity)

---

## 👨‍💻 Authors
Fartun Mohamed
Han Tonnu
Sophie Frijns
Chloe Ng
**Tathagato Mukherjee**  

---

