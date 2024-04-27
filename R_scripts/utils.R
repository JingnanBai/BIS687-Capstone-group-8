#######################
# BIS 687 capstone II
# Jingnan Bai
# 
# 24 Spring
# 
# R version 4.2.2
#######################


library(dplyr)
# library(mice)
library(scorecard)
library(MatchIt)
library(randomForest)
library(pROC)


### data merging
func_merge <- function(colname, type){
  # freqence for cat
  if (type == "Categorical single"){
    return(apply(data[, colname], 1, function(x){
      freq_tab <- table(x)
      return(names(which.max(freq_tab)))
    }))
  }
  # mean value for num
  else{
    return(mean(data[, colname], na.rm = TRUE))
  }
}



### WOE
build_woe <- function(data, ycol = "parkinsons", positive = 1,
                      is_dropsame = TRUE) {
  bins <- woebin(data, y = ycol, positive = positive, check_cate_num = FALSE)
  data_woe <- woebin_ply(data, bins = bins) |> data.frame()
  if (is_dropsame) {
    same_list <- c()
    for (idx in colnames(data_woe)) {
      if (idx == ycol) {
        next
      }
      if (max(data_woe[, idx]) == min(data_woe[, idx])) {
        same_list <- c(same_list, idx)
      }
    }
    if (!length(same_list)) {
      print("there is no constant columns found")
    } else {
      data_woe <- data_woe |> select(-all_of(same_list))
      print(paste("delete", length(same_list), "constant columns"))
    }
  }
  res <- list(woe_bins = bins, newdata = data_woe)
  return(res)
}


### IV filtering
iv_filter <- function(data_woe, iv_limit = 0.02, ycol, positive = 1) {
  iv_chart <- iv(data_woe, y = ycol, positive = positive)
  iv_chart <- iv_chart |>
    data.frame() |>
    dplyr::mutate(info_value = info_value |> round(4))
  data_iv <- var_filter(data_woe, y = ycol, iv_limit = iv_limit,
                        positive = positive)
  data_iv[, ycol] <- data_woe[[ycol]] |> as.factor()
  out <- list(data_iv = data_iv, iv_info = iv_chart)
  return(out)
}



