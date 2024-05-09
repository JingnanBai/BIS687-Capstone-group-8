# BIS687 Capstone

Group 8

Kexin Wang, Yiming Miao, Jingnan Bai, Qiyu Huang

---

<br/>

<br/>



## introduction

data: UKbiobank

### Aim1: Explore features distinguishing people with and without PD

#### Task 1: Investigate key baseline characteristics prior to PD onset

contributor: @YimingMiao

#### Task 2: Analyze longitudinal trends following PD diagnosis   

contributor: @kexinwang3

<br/>

<br/>


### Aim2: Build practical and interpretable models for predicting the risk of Parkinson’s disease

> contributor: @JingnanBai


#### Hypothesis
Parkinson's disease is widely believed to be associated with a combination of various risk factors, including basic demographic information, environmental risk factors, lifestyle, and family history of related syndromes.

#### Rationale
Contribute to early detection of the disease, while constructing a cost-efficient methodology for application.

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

## contribution

Aim1:
- Task 1: @YimingMiao
- Task 2: @kexinwang3

Aim2: @Jingnan Bai

<br/>

<br/>

## Appendix: Project file structure

```
├─figure                                 // figure hosting for proposal & report file
|
├─Group 8 Final Report
│      BIS 687 Group 8 Report.pdf       // final report
│      
├─proposal  
│      
└─R_scripts                            // project codes & R scripts
        aim1_task1.R                   // Aim1_Task 1  @YimingMiao
        aim1_task2.pdf                 // Aim1_Task 2  @kexinwang3
        aim1_task2.Rmd                 
        aim2_predictive_mod.R          // Aim2 @JingnanBai
        aim2_preprocessing.R
        data_exploration.pdf           // EDA @kexinwang3
        data_exploration.Rmd
        utils.R                        // funtion for aim2 @JingnanBai
        README.md
```
