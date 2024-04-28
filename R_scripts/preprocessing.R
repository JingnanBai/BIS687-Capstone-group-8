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
# library(mice)
library(scorecard)
library(MatchIt)

# set working space
setwd("C:/FOLD/yale_course/BIS687_capstoneii/ukbiobank")
source("utils.R")


### read data
data <- readRDS("data/ukbiobank.rds")
dim(data) # 96512  1296

var_data <- read.csv("data/variable_table.csv")
var_data_type <- read.csv("data/Data_Dictionary_Showcase.csv") 
var_data <- merge(var_data, var_data_type[, c('FieldID', 'ValueType')], 
                  by.x = "fieldid", by.y = "FieldID", 
                  all.x = TRUE)
unique(var_data$ValueType)


### response
data$recruit_year <- (data$year_of_birth_f34_0_0 |> as.integer()) + 
  (data$age_at_recruitment_f21022_0_0 |> as.integer())
data$parkinsons <- data$date_of_parkinsons_disease_report_f42032_0_0 |> 
  substr(1, 4) |> as.integer()
data <- data[(is.na(data$parkinsons))|(data$recruit_year < data$parkinsons), ]
dim(data)
data$parkinsons <- ifelse(is.na(data$parkinsons), 0, 1) |>
  as.factor()
summary(data$parkinsons)
data.f <- data.frame("eid" = data$eid, "parkinsons" = data$parkinsons)
skipli <- c("eid", "parkinsons")


### data merge (instance)
for (field in unique(var_data$field)){
  if (field %in% skipli){next}
  
  # drop ValueType = Text / ID type / NA
  tempd <- var_data[var_data$field == field, ]
  temptype <- tempd$ValueType[1]
  if (!(temptype %in% c("Integer", "Continuous", "Categorical single"))){next}
  
  # merging index based on type and create new col as field_instance
  if (nrow(tempd) == 1){
    data.f[field] = data[tempd$variable[1]]
    next
  }
  for (inst in 0:max(tempd$instance)){
    colname <- tempd[tempd$instance == inst, "variable"]
    if (length(colname) == 1){
      data.f[paste(field, "_", inst, sep = "")] <- data[colname]
    }
    else{
      data.f[paste(field, "_", inst, sep = "")] <- func_merge(colname, temptype)
      # cal diff
      if ((inst > 0)&(temptype != "Categorical single")){
        data.f[paste(field, "_", inst, sep = "")] <- 
          data.f[paste(field, "_", inst, sep = "")] - 
          data.f[paste(field, "_", inst-1, sep = "")]
      }
      else if (temptype == "Categorical single"){
        data.f[paste(field, "_", inst, sep = "")] <- 
          data.f[paste(field, "_", inst, sep = "")] |> as.factor()
      }
    }
  }
}
data <- data.f
rm(data.f)
dim(data) # 96512   965



### drop missing
missing_thr <- 0.8
is_col_keep <- apply(data, 2, function(x){
  return(sum(is.na(x))/dim(data)[1] < missing_thr)
}) 
data <- data[, is_col_keep]
dim(data) # 458

### constant col
is_col_keep <- apply(data, 2, function(x){
  freq_tab <- table(x)
  return ((max(freq_tab) / dim(data)[1]) < 0.999)
})
data <- data[, is_col_keep]
dim(data) # 451



### WOE + iv for feature importance
res_woe <- build_woe(data[, -which(names(data) == "eid")], 
                     ycol = "parkinsons", 
                     positive = 1, is_dropsame = FALSE)
data_woe <- res_woe$newdata
dim(data_woe) # 450 (exclude eid)
# iv (collinearity free)
iv_info <- bind_rows(res_woe$woe_bins)
iv_info <- iv_info |>
  group_by(variable) |>
  summarise(total_iv = max(total_iv)) |>
  as.data.frame()
iv_info['field'] = sapply(iv_info$variable, function(x){
  if (substr(x, nchar(x)-1, nchar(x)-1) == "_"){x <- substr(x, 1, nchar(x) - 2)}
  return(x)
})
iv_info <- merge(iv_info, var_data[, c("field", "ValueType")] |> unique(), 
                 by.x = "field", by.y = "field", 
                 all.x = TRUE)
var_data <- iv_info[, c("field", "variable", "ValueType")]
head(iv_info)



##### collinearity
linearity_thre <- 0.8
cat_var <- iv_info[iv_info$ValueType == "Categorical single", 
                   c("variable", "field", "total_iv")]
num_var <- iv_info[iv_info$ValueType != "Categorical single",
                   c("variable", "field", "total_iv")] # 283
### NMS filtering (numerical)
num_corr <- cor(data[, colnames(data) %in% num_var$variable]) 
num_corr <- num_corr |> as.data.frame()
num_iv <- num_var |> group_by(field) |> 
  summarize(total_iv = sum(total_iv)) |>
  arrange(desc(total_iv))
num_drop <- c("eid", "parkinsons")
for (field in num_iv$field){
  if (field %in% num_drop){next}
  tempcor <- num_corr[names(num_corr) %in% num_var[num_var$field == field, "variable"], ]
  tempcor <- apply(tempcor, 2, max) |> na.omit() |> abs()
  collist <- num_var[num_var$variable %in% names(tempcor[tempcor > linearity_thre]), "field"]
  collist <- ifelse(field %in% collist, collist[collist != field], collist)
  num_drop <- c(num_drop, collist) |> unique()
}
num_var_keep <- num_var[!(num_var$field %in% num_drop), "variable"]
length(num_var_keep) # 255
var_keep <- c(num_var_keep, cat_var$variable, c("parkinsons")) |> unique()
length(var_keep)  # 422


### iv filtering (keep WOE data)
res_iv <- iv_filter(data_woe,
                    iv_limit = 0.1, ycol = "parkinsons", positive = 1)
data_iv <- res_iv$data_iv |> as.data.frame()  # 225
# collinearity filtering
tempcol <- sapply(colnames(data_iv), function(x){
  if (substr(x, nchar(x)-3, nchar(x)) == "_woe"){
    x <- substr(x, 1, nchar(x)-4)
  }
  return (x)
})
colnames(data_iv) <- tempcol
data_iv_wcol <- data_iv[, colnames(data_iv) %in% var_keep]
dim(data_iv_wcol)  # 211
# keep original features <--- need previous imputation here
# iv_info <- bind_rows(res_woe$woe_bins)
# iv_info <- iv_info |>
#   group_by(variable) |>
#   summarise(total_iv = max(total_iv))
# iv_info <- iv_info[iv_info$total_iv > 0.1, ]
# data_iv <- data[, iv_info$variable]
# data_iv["parkinsons"] <- data$parkinsons
# dim(data_iv)



### PSM (matching)
psm_mod <- matchit(parkinsons ~.,
                   method = "nearest", distance = "glm", 
                   data = data_iv_wcol)
# summary(psm_mod) # quite slow here ...
# assess the quality of matches
plot(psm_mod, 
     type = "density", 
     interactive = FALSE,
     which.xs = ~ sex + year_of_birth) 
# apply on dataset
data.psm <- match.data(psm_mod) |> data.frame()
data.psm <- data.psm[, -which(colnames(data.psm) %in% 
                                c("eid", "distance", "weights", "subclass"))]
dim(data.psm)




### save ###
# original data
data.baseline <- match.data(psm_mod, data = data) |> data.frame()
data.baseline <- data.baseline[, -which(colnames(data.baseline) %in% 
                                          c("eid", "distance", "weights", "subclass"))]
# data.baseline <- mice(data.baseline, m=1, maxit=50, meth='cart') # slow ...
dim(data.baseline)
# (w/o nms)
data.iv <- match.data(psm_mod, data = data_iv) |> data.frame()
data.iv <- data.iv[, -which(colnames(data.iv) %in% 
                              c("eid", "distance", "weights", "subclass"))]
dim(data.iv)

# variable directory
saveRDS(var_data, file = "data/variable_field_map.rds")
# baseline
saveRDS(data.baseline, file = "data/aim2_ukbiobank_baseline.rds")
# + iv/woe
saveRDS(data.iv, file = "data/aim2_ukbiobank_iv.rds")
# + iv/woe + nms
saveRDS(data.psm, file = "data/aim2_ukbiobank.rds")



