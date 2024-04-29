R scripts for BIS687 capstone

Group 8

<br/>

<br/>

- **function definition**: `utils.R`  ==> @JingnanBai

<br/>

- **data preprocessing**: `preprocessing.R` ==> @JingnanBai
  - conduct data cleaning
  - WOE + IV for feature importance (preliminary)
  - NMS frame to deal with collinearity
  - propensity score matching (PSM)

<br/>

- **Aim 1**: explore features distinguishing people with/without PD
  - **Data Exploration**: `data_exploration.Rmd`
  - **Plan 1**: investigate key baseline characteristics prior to PD onset
 
  - **Plan 2**: analyze longitudinal trends following PD diagnosis `aim1_task2.Rmd` ==> @kexinwang3
    - propensity score matching
    - mixed-effects model & longitudinal trend

    ![workflow](https://github.com/JingnanBai/BIS687-Capstone-group-8/figure/workflow)

<br/>

- **Aim 2**: build predictive model `aim2_predictive_mod.R` ==> @JingnanBai
  - model training (random forest)
  - model evaluation
    - confusion matrix
    - ROC / AUC
    - K-fold
  - model comparison
    - compare models (baseline data / dataset w iv / dataset w iv + nms) based on recall
  - feature importance


<br/>


      
