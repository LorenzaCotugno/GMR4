# GMR4


# Regularized Reduced Rank Regression for Mixed Predictor and Response Variables (GMR4)

This repository contains the R functions and scripts used in the manuscript **“Regularized Reduced Rank Regression for Mixed Predictor and Response Variables.”**  
It provides a complete implementation of the proposed GMR4 methodology, including estimation, regularization, model selection, prediction, cross-validation, and simulation procedures.

---

## Core R Functions

The following R scripts implement the core methods introduced in the paper:

- **auxiliary.R** – collection of helper functions  
- **gmr4.R** – main estimation routine for the GMR4 model  
- **gmr4.start.R** – initialization step for model estimation  
- **predict.R** – prediction methods for fitted GMR4 models  
- **xval.gmr4.R** – cross-validation for tuning and model selection  
- **xval.start.R** – initialization functions for cross-validation  

---

## Empirical Analysis (Health Dataset)

To reproduce the empirical study from the manuscript, download the dataset from the GESIS – Leibniz Institute for the Social Sciences:

Dataset link (ZA8794):  
https://search.gesis.org/research_data/ZA8794

Script for preparing and analyzing the data:

- **dataHealth.R**

---

## Simulation Study

The following scripts reproduce the simulation experiments presented in the manuscript:

- **NonInformativePredictors.R**  
- **rrr.sim3b.R**  
- **simulation_study.R**

---

## Plotting Utilities

Scripts for generating all figures used in the paper:

- **plot.quantifications.R**  
- **xvalplot.R**  
- **simplot.R**


