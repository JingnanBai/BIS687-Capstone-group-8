#######################
# BIS 687 capstone II
# Jingnan Bai
# 
# 24 Spring
# 
# R version 4.2.2
#######################

rm(list = ls())

library(dplyr)
library(randomForest)
library(pROC)


# set working space
setwd("C:/FOLD/yale_course/BIS687_capstoneii/ukbiobank")
source("utils.R")
# source("preprocessing.R")



### modeling (RF)
data.fin <- readRDS("data/aim2_ukbiobank.rds")
data.baseline <- readRDS("data/aim2_ukbiobank_baseline.rds")
data.iv <- readRDS("data/aim2_ukbiobank_iv.rds")
var.map <- readRDS("data/variable_field_map.rds")
dim(data.fin) # 211

# kfold
build_kfold <- function(k, n, seed = NA) {
  if (!is.na(seed)) {
    set.seed(seed)
  }
  kfold <- sample(1:n, floor(n / k) * k)
  kfold <- matrix(kfold, ncol = k)
  return(kfold)
}

# evaluation
eval_model <- function(pre, gt) {
  pre <- pre |> as.factor()
  gt <- gt |> as.factor()
  levels(pre) <- c(0, 1)
  levels(gt) <- c(0, 1)
  temp <- table(pre, gt)
  tp <- temp[2, 2]
  fp <- temp[2, 1]
  fn <- temp[1, 2]
  tn <- temp[1, 1]
  result <- data.frame(matrix(NA, 1, 0))
  result$acc <- (tp + tn) / (tp + fp + tn + fn)
  result$precision <- tp / (tp + fp)
  result$recall <- tp / (tp + fn)
  result$f1 <- 2 * result$pre * result$recall / (result$pre + result$recall)
  return(result)
}


# model
build_random_forest <- function(data, ycol,
                                is_kfold = FALSE, cv_num = 10, seed = NA,
                                ntree = 100, maxnodes = 50, mtry = NA, 
                                is_roc = TRUE) {
  if (!is.na(seed)) {
    set.seed(seed)
  }
  if (is_kfold) {
    epoch_num <- cv_num
  } else {
    epoch_num <- 1
  }
  form <- as.formula(paste(ycol, " ~ ."))
  auc <- 0
  pre <- 0
  acc <- 0
  recall <- 0
  fscore <- 0
  thre <- NA
  if (is_kfold) {
    kfold <- build_kfold(cv_num, dim(data)[1], seed = seed)
  }
  for (epoch in 1:epoch_num) {
    if (is_kfold) {
      train <- data[-kfold[, epoch], ]
      test <- data[kfold[, epoch], ]
    } else {
      train <- data
      test <- data
    }
    if ((is.na(mtry)) | (mtry == "auto")) {
      mtry <- log2(dim(train)[2]) |> floor()
    }
    mod <- randomForest(form, data = train,
                        ntree = ntree, maxnodes = maxnodes, mtry = mtry, 
                        na.action = na.roughfix)
    mod$importance <- randomForest::importance(mod)
    pred_prob <- predict(mod, newdata = test, "prob")[, 2]
    if (is_roc){
      roc0 <- roc(as.ordered(test[, ycol]), as.ordered(pred_prob),
                  quiet = TRUE)
      if (is.na(thre)) {
        max_temp <- abs(1 - roc0$specificities - roc0$sensitivities)
        idx <- which(max_temp == max(max_temp), arr.ind = TRUE)
        thre_temp <- roc0$thresholds[idx]
      } else {
        thre_temp <- thre
      }
    }
    else {thre_temp <- ifelse(is.na(thre), median(pred_prob), thre)}
    test$pred <- 0
    test$pred[pred_prob > thre_temp] <- 1
    test$pred <- test$pred |> as.factor()
    result <- eval_model(test$pred, test[, ycol])
    
    auc <- ifelse(is_roc, auc + roc0$auc, NA)
    acc <- acc + result$acc
    pre <- pre + result$pre
    recall <- recall + result$recall
    fscore <- result$f1 + fscore
  }
  if (is_kfold) {
    mod <- randomForest(form, data = data, 
                        na.action = na.roughfix)
    mod$importance <- randomForest::importance(mod)
    pred_prob <- predict(mod, newdata = data, "prob")[, 2]
    if (is_roc){
      roc0 <- roc(as.ordered(data[, ycol]), as.ordered(pred_prob),
                  quiet = TRUE)
      max_temp <- abs(1 - roc0$specificities - roc0$sensitivities)
      idx <- which(max_temp == max(max_temp), arr.ind = TRUE)
      thre <- roc0$thresholds[idx]
    }
    else {thre <- ifelse(is.na(thre), median(pred_prob), thre)}
  }
  eval_tab <- cbind(c("accuracy", "precision", "F1-score", "recall", "AUC"),
                    round(c(acc, pre, fscore, recall, auc) / epoch_num, 3)) |>
    data.frame()
  colnames(eval_tab) <- c("index", "value")
  result <- list(eval_tab = eval_tab, model = mod,
                 roc_res = ifelse(is_roc, roc0, NA), best_thre = thre)
  return(result)
}


# build model
data.fin$parkinsons <- as.factor(data.fin$parkinsons)
rf.res <- build_random_forest(data.fin, ycol = "parkinsons",
                              is_kfold = TRUE, cv_num = 10, seed = 103221,
                              ntree = 100, maxnodes = 50, mtry = "auto")
# result
rf.res$eval_tab
# feature importance
impt <- rf.res$model$importance
impt <- impt[order(impt[, "MeanDecreaseGini"], decreasing = TRUE), ] |>
  data.frame()
colnames(impt) <- c("MeanDecreaseGini")
impt$var <- rownames(impt)
impt <- merge(impt, var.map[, c("field", "variable")],
              by.x = "var", by.y = "variable", all.x = TRUE)
impt_sum <- impt |>
  group_by(field) |>
  summarise(importance_gini = sum(MeanDecreaseGini)) |>
  arrange(desc(importance_gini))
print(head(impt_sum, 50), n=50)


### assessment
# # baseline
# data.baseline$parkinsons <- as.factor(data.baseline$parkinsons)
# rf.res.baseline <- build_random_forest(data.baseline, ycol = "parkinsons",
#                                        is_kfold = TRUE, cv_num = 10, seed = 103221,
#                                        ntree = 100, maxnodes = 50, mtry = "auto", 
#                                        is_roc = FALSE)
# rf.res.baseline$eval_tab
# # + iv (w/o nms)
# data.iv$parkinsons <- as.factor(data.iv$parkinsons)
# rf.res.iv <- build_random_forest(data.iv, ycol = "parkinsons",
#                                  is_kfold = TRUE, cv_num = 10, seed = 103221,
#                                  ntree = 100, maxnodes = 50, mtry = "auto", 
#                                  is_roc = FALSE)
# rf.res.iv$eval_tab


