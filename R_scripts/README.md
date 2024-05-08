R scripts for BIS687 capstone

Group 8

<br/>

<br/>

- **function definition**: `utils.R`  ==> @JingnanBai

<br/>

<br/>

- **Aim 1**: explore features distinguishing people with/without PD
  - **Data Exploration**: `data_exploration.Rmd`

  - **Task 1**: investigate key baseline characteristics prior to PD onset
    - propensity score matching
    - forward stepwise selection
    - logistic regression

<img src="https://github.com/JingnanBai/BIS687-Capstone-group-8/blob/main/figure/workflow aim1 task1.png" alt="workflow" width="75%;" height="75%;" margin="auto;"/>
 
  - **Task 2**: analyze longitudinal trends following PD diagnosis `aim1_task2.Rmd` ==> @kexinwang3
    - propensity score matching
    - mixed-effects model & longitudinal trend


    <img src="https://github.com/JingnanBai/BIS687-Capstone-group-8/blob/main/figure/workflow.jpg" alt="workflow" width="75%;" height="75%;" margin="auto;"/>

<br/>

<br/>

- **Aim 2**: build predictive model `aim2_predictive_mod.R` ==> @JingnanBai
  - **data preprocessing**: `aim2_preprocessing.R` ==> @JingnanBai
    - basic data cleaning
    - WOE + IV for feature importance (preliminary)
    - NMS algorithm to deal with collinearity
    - propensity score matching (PSM)
  - model training (random forest)
  - model evaluation
    - confusion matrix
    - ROC / AUC
    - K-fold
  - model comparison
    - compare models (baseline data / dataset w iv / dataset w iv + nms) based on recall
  - feature importance


<br/>


      
