# 🚧 Highway to... Predicting Fatal Traffic Accidents in Brazil

This repository contains the code and instructions for reproducing the results presented in the paper:

> **Highway to... Determining Fatal Outcomes in Traffic Accidents Based on Police Reports**  
> *Submitted to BRACIS 2025*  
> *(Authors omitted for review)*

## Overview

This study investigates the use of machine learning to predict the severity of traffic accidents in southern Brazil using open data from the Brazilian Federal Highway Police (PRF). We compare the performance of **Random Forest (RF)**, **k-Nearest Neighbors (kNN)**, and **Multilayer Perceptron (MLP)** classifiers, applying preprocessing techniques such as categorical encoding, feature selection, and **SMOTE** for class balancing.

The best performing models are further analyzed using **SHAP (SHapley Additive exPlanations)** to provide insight into the most influential features contributing to accident fatality predictions.

## 📁 Repository Structure
```
.
├── data/
│ └── README.md # Instructions to download official PRF dataset
├── models/ # Directory for saving or loading pre-trained models
├── results/ # Directory for saving performance metrics and plots
├── train_models.R # Script to preprocess data and train all models
├── explain.R # Script to compute and visualize SHAP values
└── README.md # You're here :)
```

## Data Acquisition

Due to size and licensing constraints, we do not host the raw datasets here. To use the training script, **please follow the instructions in `data/README.md`** to download the required data directly from official [PRF](https://www.gov.br/prf/pt-br) sources.

### 👉 Pre-processed datasets are avaialable as well:
Trainning set without SMOTE: <https://drive.google.com/file/d/1kKGdiQz6M_kpNIpU4nlsRlFh-vjNolYh/view?usp=share_link>

Trainning set with SMOTE: <https://drive.google.com/file/d/16eS_T6T_fZ7H5n88wbayhhKMtXVS4_NS/view?usp=share_link>

Validation set: <https://drive.google.com/file/d/1AWJ7l5aIqSyBo-0EAom9f2xBJO506Dix/view?usp=share_link>

## Training the Models

To train all models from scratch:

```R
source("train_models.R")
```

This script performs:
- Data loading and preprocessing
- Class balancing using SMOTE
- Training of RF, kNN, and MLP models
- Evaluation through metrics: F1-score, specificity, and AUC-ROC
- Model persistence in models/ directory
  
### 👉 Or download pre-trained models:
Models without SMOTE: <https://drive.google.com/file/d/1L_QRj-K4PtuZvwV0vh49DwKcyd3SQBOB/view?usp=share_link>

Models with SMOTE: <https://drive.google.com/file/d/19QhEcVT6uvseQ9RcftyI0aTzELu5wLOR/view?usp=share_link>

## Model Explainability

To generate SHAP-based explanations for the RF model:

```R
source("explain.R")
```

The explainability script includes:
- SHAP value computation
- Global feature importance visualization
- Local explanation trough beeswarm plot
- Categorical features individual factors explanations

### 👉 Pre-computed SHAP values are available here:
Download SHAP Values: <https://drive.google.com/file/d/1WB6KWUGQDfdrP1vIX87BAwpOZji3fnz3/view?usp=share_link>

Encoding maps: <https://drive.google.com/file/d/1Y_vXTFcKNzhQJzk6gHo6-LvcdUEKd9uM/view?usp=share_link>

