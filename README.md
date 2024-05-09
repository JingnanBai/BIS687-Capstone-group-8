# BIS687 Capstone

Group 8

Kexin Wang, Yiming Miao, Jingnan Bai, Qiyu Huang

---

<br/>

<br/>

<br/>

## Introduction

> contributor: @QYHuang00

Parkinson's disease (PD) stands as the second-most prevalent neurodegenerative disorder in the United States, significantly deteriorating the quality of life for those affected. This disease's widespread effects not only make daily living difficult for people, but they also place a significant strain on healthcare systems.

Data: UKbiobank

Given the backgound above, our research team aims to enhance understanding and management of PD through several approaches: investigating key baseline characteristics before PD onset, analyzing longitudinal trends post-diagnosis, and developing practical, interpretable models to predict PD risk. 

<br/>

<br/>


### Aim1: Investigate Key Inferences for Pre-Diagnostic and Post-Diagnostic PD Patients

> contributor: @YimingMiao; @kexinwang3

#### Challenges

- Primary response variable is the date of parkinson disease diagnosis
- 4 instances of variable collection: 2006-2010, 2012, 2014, and 2019
- Temporal framework facilitates longitudinal analysis but complicates aligning PD report dates with the data collection phases.

Thus, we separated the PD patients into two cohorts based on their PD diagnosis status at baseline for separate analyses:

- Cohort 1: participants who were not diagnosed with PD at baseline but received a diagnosis in subsequent follow-ups
- Cohort 2: patients who have already been diagnosed with PD at baseline and four instances were documented after the PD report

<br/>

#### Task 1: Investigate key baseline characteristics prior to PD onset

> contributor: @YimingMiao

- Experimental Approach
    - Propensity score matching
    - MICE Imputation
    - Forward stepwise selection
    - Logistic regression
- Summary
    - Decreased physical activity, irregular or unhealthy dietary habits, sleep disorder, and mental health challenges could serve as potential PD risk factors or early indicators

<br/>

#### Task 2: Analyze longitudinal trends following PD diagnosis   

> contributor: @kexinwang3

- Experimental Approach
    - Propensity score matching
    - Mixed-effects model & longitudinal trend after PD diagnosis
- Summary
    - Baseline features distinguishing PD patients: computer usage, sleep duration, longest period of depression, average weekly red wine intake
    - Trend features distinguishing PD patients: computer usage, sitting height, average weekly fortified wine intake

<br/>

<br/>

<br/>

### Aim2: Build practical and interpretable models for predicting the risk of Parkinson’s disease

> contributor: @JingnanBai


#### Hypothesis
Parkinson's disease is widely believed to be associated with a combination of various risk factors, including basic demographic information, environmental risk factors, lifestyle, and family history of related syndromes.

#### Rationale
Contribute to early detection of the disease, while constructing a cost-efficient methodology for application.

<br/>

#### Experimental Approach

- Data preprocessing:
    1) WOE+IV as preliminary feature filtering;
    2) NMS algorithm to deal with colinearity;
    3) Propensity Score Matching for data imbalance;
- Modeling: Random Forest + 10-fold cross validation

#### Result

- Model performance:
  - recall: 0.562
  - AUC: 0.546
- Interpretation & summary:
  - Physical activity measurements are a crucial factor for the early detection of Parkinson's disease
  - As abnormalities of movement are widely recognized as a preliminary indicator of Parkinson's, related factors such as average acceleration may significantly contribute to classification.

<br/>

<br/>

<br/>

## Appendix: Project file structure

> please do NOT edit this part as it is ONLY a reference for file structure (not a place to show your contribution :)

```
├─figure                                // figure hosting for proposal & report file
|
├─final_report
│      BIS687-Group-8-final-report.pdf  // final report knit with rmd
|      report.RMD
|      report.tex
|      template.tex                     // template for rmd (same as proposal)
│      
├─proposal  
│      
└─R_scripts                             // project codes & R scripts
│        aim1_task1.R                   // Aim1_Task 1  @YimingMiao
│        aim1_task2.pdf                 // Aim1_Task 2  @kexinwang3
│        aim1_task2.Rmd                 
│        aim2_predictive_mod.R          // Aim2 @JingnanBai
│        aim2_preprocessing.R
│        data_exploration.pdf           // EDA @kexinwang3
│        data_exploration.Rmd
│        utils.R                        // funtion for aim2 @JingnanBai
└─       README.md

```
