# Gender Pay Gap Analysis in Lithuania

This repository contains the code and data used in my Master’s thesis "The Analysis of dynamics of women’s and men’s wages", which analyses the salaries of men and women, used the publicly available database of the Lithuanian State Data Agency - Statistical survey of wage structure.

The study combines regression based methods and machine learning techniques to examine
the extent of gender wage differences and the factors explaining them.

---

## Data

The data used in this project are publicly available database of the Lithuanian State Data Agency - Statistical survey of wage structure
They are included in this repository for transparency and reproducibility.

Files:
- `DarboUzmokestis2014.csv`
- `DarboUzmokestis2018.csv`
- `DarboUzmokestis2022.csv`

An additional file is used for occupation grouping:
- `Summary-of-Groups.csv` 

---

## Code

The main analysis script is:

- `Final code.R` – data cleaning, descriptive analysis, regression based models,
  decomposition methods, and machine learning models.

The script covers:
- data preparation 
- descriptive statistics and visualisations
- Two-way ANOVA
- Multivariate Regression, Quantile Regression and Oaxaca–Blinder decomposition
- Machine learning models: Random Forest, XGBoost and Support Vector Regression (SVR)


---

## Software and Packages

- R version: 4.4.x
- Key packages include:
  - dplyr
  - data.table
  - ggplot2
  - car
  - quantreg
  - oaxaca
  - xgboost
  - randomForest

---

## Reproducibility

To reproduce the analysis:
1. Clone or download this repository.
2. Open `Master-thesis.Rproj` in RStudio.
3. Run `Final code.R` from top to bottom.

---

## Author

Amanda Vilkončiūtė  
Master’s thesis, Lithuania
