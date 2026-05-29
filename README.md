# Energy Efficiency Analysis of Buildings in Aragón

## Overview

This project performs an exploratory data analysis (EDA) and statistical modeling of energy efficiency data from buildings located in Aragón, Spain.

The analysis focuses on identifying patterns in:

- Energy consumption
- CO₂ emissions
- Building characteristics
- Energy efficiency classifications
- Construction periods

The project was developed in R using a modular and reproducible analytical workflow.

---

## Objectives

The main objectives of this project are:

- Explore the distribution of energy consumption and CO₂ emissions
- Detect outliers and extreme observations
- Analyze relationships between numerical variables
- Study categorical energy classifications
- Evaluate correlations between variables
- Apply logarithmic transformations to improve interpretability
- Build an exploratory linear regression model

---

## Dataset

Source:

https://www.kaggle.com/datasets/isabelocastillo/eficiencia-energetica-edificios-de-aragon/data

The dataset contains information related to:

- Energy consumption
- CO₂ emissions
- Building area
- Construction year
- Energy classifications
- Geographic distribution
- Building conditions

---

## Technologies Used

- R
- ggplot2
- dplyr
- corrplot
- Statistical analysis
- Exploratory Data Analysis (EDA)
- Linear regression

---

## Project Structure


proyecto-energia-aragon/

│
├── data/
│   └── Energia_Aragon.csv
│
├── graficos/
│
├── outputs/
│   ├── tabla_correlaciones.csv
│   ├── tabla_resumen_numerico.csv
│   ├── tabla_valores_faltantes.csv
│   ├── outliers_consumo.csv
│   ├── outliers_emisiones.csv
│   └── modelo_regresion.txt
│
├── scripts/
│   ├── funciones.R
│   └── analisis.R
│
├── informe/
│   └── informe.tex
│
├── README.md
│
├── .gitignore
│
└── proyecto.Rproj

## Main Analyses Performed

Data Cleaning

- Date conversion and formatting
- Removal of duplicate records
- Filtering invalid observations
- Handling missing values

Exploratory Data Analysis

- Histograms
- Boxplots
- Scatter plots
- Categorical distributions
- Correlation analysis
- Heatmaps

Statistical Analysis

- Spearman correlation
- Outlier detection
- Logarithmic transformations
- Linear regression modeling


## Reproducibility

The project follows a modular and reproducible workflow structure.

Main scripts:

- `scripts/funciones.R`
- `scripts/analisis.R`

To execute the full analysis:

`source("scripts/analisis.R")`

Generated outputs will be automatically stored in:

- `graficos/`
- `outputs/`

## Author

Juan Villalobos

Physics Engineering Student
Data Analysis & Statistical Modeling