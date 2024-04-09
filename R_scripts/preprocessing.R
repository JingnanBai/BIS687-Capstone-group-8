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
setwd("C:/FOLD/workspace/yale_course/BIS687_capstoneii/ukbiobank")
source("utils.R")


### read data
data = readRDS("data/ukbiobank.rds")
dim(data) # 96512  1296

var_data <- read.csv("data/variable_table.csv")
var_data_type <- read.csv("data/Data_Dictionary_Showcase.csv") 
var_data <- merge(var_data, var_data_type[, c('FieldID', 'ValueType')], 
                  by.x = "fieldid", by.y = "FieldID", 
                  all.x = TRUE)
unique(var_data$ValueType)




##### preprocessing #####

# response
data$parkinsons <- ifelse(is.na(data$date_of_parkinsons_disease_report_f42032_0_0), 0, 1) |>
  as.factor()
summary(data$parkinsons)

data.f <- data.frame("eid" = data$eid, "parkinsons" = data$parkinsons)
skipli <- c("eid", "parkinsons")


# data merge
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
      else{
        data.f[paste(field, "_", inst, sep = "")] <- 
          data.f[paste(field, "_", inst, sep = "")] |> as.factor()
      }
    }
  }
}
data <- data.f
rm(data.f)
dim(data) # 96512   964


# drop missing
missing_thr <- 0.8
is_col_keep <- apply(data, 2, function(x){
  return(sum(is.na(x))/dim(data)[1] < missing_thr)
}) 
data <- data[, is_col_keep]
dim(data) # keep 458/964
# constant col
is_col_keep <- apply(data, 2, function(x){
  freq_tab <- table(x)
  return ((max(freq_tab) / dim(data)[1]) < 0.999)
})
data <- data[, is_col_keep]
dim(data) 

# imputation <------------------------------------------------------ ! confirm sequence here !
# data_imp <- mice(data, m=1, maxit=50, meth='pmm')




### feature filtering

# build WOE
res_woe <- build_woe(data[, -which(names(data) == "eid")], 
                     ycol = "parkinsons", 
                     positive = 1, is_dropsame = TRUE)
data_woe <- res_woe$newdata
dim(data_woe)

# keep original features < -------------------------------- ! need previous imputation here !
# iv_info <- bind_rows(res_woe$woe_bins)
# iv_info <- iv_info |>
#   group_by(variable) |>
#   summarise(total_iv = max(total_iv))
# iv_info <- iv_info[iv_info$total_iv > 0.1, ]
# data_iv <- data[, iv_info$variable]
# data_iv["parkinsons"] <- data$parkinsons
# dim(data_iv)

# keep WOE data
res_iv <- iv_filter(data_woe,
                    iv_limit = 0.1, ycol = "parkinsons", positive = 1)
data_iv <- res_iv$data_iv
dim(data_iv)




### PSM (matching)

psm_mod <- matchit(parkinsons ~.,
                   method = "nearest", distance = "glm", 
                   data = data_iv)
# summary(psm_mod) # quite slow here ...
# assess the quality of matches
plot(psm_mod, 
     type = "density", 
     interactive = FALSE,
     which.xs = ~ sex_woe + year_of_birth_woe) 
# apply on dataset
data_psm <- match.data(psm_mod) |> data.frame()
data_psm <- data_psm[, -which(colnames(data_psm) %in% c("distance", "weights", "subclass"))]
dim(data_psm)


### save
saveRDS(data_psm, file = "data/mod_ukbiobank.rds")




