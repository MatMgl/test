# Skoda Price Prediction

This repository contains the final project for the course **Statistical Learning in Practice**.

## Dataset

The project is based on the [100,000 UK Used Car Dataset](https://www.kaggle.com/datasets/adityadesai13/used-car-dataset-ford-and-mercedes), which includes listings of used vehicles in the UK from nine different manufacturers. For this analysis, we focused exclusively on Škoda vehicles.

The dataset contains key vehicle attributes such as:
- model  
- year of manufacture  
- price (in GBP)  
- mileage  
- transmission type  
- fuel type  
- annual tax  
- fuel efficiency (MPG)  
- engine size  

## Goal

The aim of the project was to develop accurate predictive models for estimating the price of a used Škoda car based on its features. This involved data cleaning, feature engineering, and a thorough comparison of multiple statistical and machine learning methods.

## Methods Used

The following methods were applied and compared:
- Linear regression (full model, stepwise selection, best subset)
- Regularized regression: Ridge and LASSO
- Principal Component Regression (PCR) and Partial Least Squares (PLS)
- Tree-based models: decision trees, bagging, random forest, boosting (GBM), XGBoost
- Bayesian Additive Regression Trees (BART)
- Cross-validation and bootstrap methods for model evaluation

## Project Files

- 📓 [Jupyter Notebook – Partial analysis & results (EN)](checkpoint_en.ipynb)
- 📄 [R Script – Full code of the project (PL)](fullcode_pl.r)
- 📘 [Final Report – PDF (EN)](Statistical_Learning_report.pdf)

## Authors

- Baltazar Augustynek
- Mateusz Mglej
