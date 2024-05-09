# load libraries
library(MatchIt)
library(tidyverse)
library(car)
library(mice)
library(caret)
library(glmnet)
library(MASS)
library(forestploter)
library(forestplot)


# load data
data <- readRDS("/Users/yimingmiao/Desktop/graduate/spring\ 2024/ds\ capstone/ukbiobank/ukbiobank.rds")
dim(data) # 96512  1296

# Select PD patients with diagnosis after baseline
report_year <- as.numeric(format(as.Date(data$date_of_parkinsons_disease_report_f42032_0_0), "%Y"))
demo_feature <- c("eid", "sex_f31_0_0", "townsend_deprivation_index_at_recruitment_f189_0_0", 
                  "ethnic_background_f21000_0_0", "age_at_recruitment_f21022_0_0")
no_data <- data[is.na(report_year), demo_feature]
no_data <- no_data[complete.cases(no_data),]
no_data$parkinsons <- 0
yes_data <- data[(report_year > 2010) & (!is.na(report_year)), demo_feature]
yes_data <- yes_data[complete.cases(yes_data),]
yes_data$parkinsons <- 1
data1 <- rbind(yes_data, no_data)


# propensity score matching
m1 <- glm(parkinsons ~ sex_f31_0_0+townsend_deprivation_index_at_recruitment_f189_0_0+ethnic_background_f21000_0_0+
            age_at_recruitment_f21022_0_0, family = binomial, data = data1)
data1$propensity_score <- predict(m1, type = "response")
m2 <- matchit(parkinsons ~ propensity_score, data = data1, method = "nearest")
matched_data <- match.data(m2)
data2 <- data[data$eid %in% matched_data$eid,]
dim(data2) # 720 1296


# select baseline measures
data2$parkinsons <- ifelse(is.na(data2$date_of_parkinsons_disease_report_f42032_0_0), 0, 1)
data2.1 <- data2[grep("_0_0$", colnames(data2))]
data2.1$parkinsons <- data2$parkinsons
dim(data2.1) # 720 469


# filter out variables with missing rate > 20%
missing_thr <- 0.2
is_col_keep <- apply(data2.1, 2, function(x){
  return(sum(is.na(x))/dim(data2.1)[1] < missing_thr)
}) 
data2.1 <- data2.1[, is_col_keep]
dim(data2.1)
data2.2 <- data2.1[, 8:dim(data2.1)[2]] # demographics and eid are not used as variables of interest
dim(data2.2) # 720 323




# Some variables have only 1 level that makes regression unable to fit
# Examine number of levels for each variable
# Reference: 
# Li Z. How to debug "contrasts can be applied only to factors with 2 or more levels" error? August 21, 2018. Accessed April 27, 2024.
debug_contr_error <- function (dat, subset_vec = NULL) {
  if (!is.null(subset_vec)) {
    ## step 0
    if (mode(subset_vec) == "logical") {
      if (length(subset_vec) != nrow(dat)) {
        stop("'logical' `subset_vec` provided but length does not match `nrow(dat)`")
      }
      subset_log_vec <- subset_vec
    } else if (mode(subset_vec) == "numeric") {
      ## check range
      ran <- range(subset_vec)
      if (ran[1] < 1 || ran[2] > nrow(dat)) {
        stop("'numeric' `subset_vec` provided but values are out of bound")
      } else {
        subset_log_vec <- logical(nrow(dat))
        subset_log_vec[as.integer(subset_vec)] <- TRUE
      } 
    } else {
      stop("`subset_vec` must be either 'logical' or 'numeric'")
    }
    dat <- base::subset(dat, subset = subset_log_vec)
  } else {
    ## step 1
    dat <- stats::na.omit(dat)
  }
  if (nrow(dat) == 0L) warning("no complete cases")
  ## step 2
  var_mode <- sapply(dat, mode)
  if (any(var_mode %in% c("complex", "raw"))) stop("complex or raw not allowed!")
  var_class <- sapply(dat, class)
  if (any(var_mode[var_class == "AsIs"] %in% c("logical", "character"))) {
    stop("matrix variables with 'AsIs' class must be 'numeric'")
  }
  ind1 <- which(var_mode %in% c("logical", "character"))
  dat[ind1] <- lapply(dat[ind1], as.factor)
  ## step 3
  fctr <- which(sapply(dat, is.factor))
  if (length(fctr) == 0L) warning("no factor variables to summary")
  ind2 <- if (length(ind1) > 0L) fctr[-ind1] else fctr
  dat[ind2] <- lapply(dat[ind2], base::droplevels.factor)
  ## step 4
  lev <- lapply(dat[fctr], base::levels.default)
  nl <- lengths(lev)
  ## return
  list(nlevels = nl, levels = lev)
}

debug_contr_error(data2.2)


# examine collinearity
cor_matrix <- cor(select_if(data2.2, is.numeric))
find_high_cor <- function(cor_matrix, threshold = 0.8) {
  diag(cor_matrix) <- 0
  cor_matrix[lower.tri(cor_matrix)] <- 0  # Consider only upper triangle
  high_cor_pairs <- which(abs(cor_matrix) > threshold, arr.ind = TRUE)
  
  # Extracting variable names
  pairs <- data.frame(var1 = rownames(cor_matrix)[high_cor_pairs[, 1]],
                      var2 = colnames(cor_matrix)[high_cor_pairs[, 2]],
                      cor_value = cor_matrix[high_cor_pairs])
  return(pairs)
}
high_cor_pairs <- find_high_cor(cor_matrix, 0.7)



# For some categorical variables, one level constitutes a extremely high percentage that results in failure in further imputation process
# deal with Date type variables
data2.2 %>% 
  select_if(~ inherits(., "Date"))
data2.2$invitation_to_physical_activity_study_date_sent_f110006_0_0 <- as.numeric(data2.2$invitation_to_physical_activity_study_date_sent_f110006_0_0)

data2.2$parkinsons <- as.factor(data2.2$parkinsons)
data2.2_categorical <- select_if(data2.2, negate(is.numeric)) 
# Use near zero variance to identify those categorical variables
nzv <- nearZeroVar(data2.2_categorical, saveMetrics = TRUE)
rownames(nzv[nzv$nzv=='TRUE',])


# filter out variables with 
# 1. high missing rate
# 2. high correlation with other variables
# 3. categorical variables with new zero variance
data2.3 <- data2.2[, -which(colnames(data2.2) %in% c(
  
  # variables with only one level
  "acceleration_data_cwa_format_f90001_0_0",
  "acceleration_intensity_timeseries_f90004_0_0",
  "data_quality_good_wear_time_f90015_0_0",
  "data_quality_good_calibration_f90016_0_0",
  "data_quality_calibrated_on_own_data_f90017_0_0",
  "invitation_to_physical_activity_study_acceptance_f110005_0_0",
  "start_of_requested_wear_period_f90003_0_0",
  "start_time_of_wear_f90010_0_0",
  "end_time_of_wear_f90011_0_0",
  "alcohol_drinker_status_f20117_0_0",
  
  # collinearity
  colnames(data2.2)[grep("fraction_acceleration", colnames(data2.2))],
  "total_data_readings_f90187_0_0",
  colnames(data2.2)[grep("wear_duration_during", colnames(data2.2))],
  "non_wear_duration_overall_f90052_0_0",
  "readings_exceeding_8_gravities_after_calibration_f90185_0_0",
  "maximum_readings_exceeding_8_gravities_after_calibration_in_a_5_second_epoch_f90186_0_0",
  "sample_rate_maximum_f90191_0_0",
  "data_recording_errors_f90182_0_0",
  "calibration_coefficients_z_offset_f90163_0_0",
  "overall_acceleration_average_f90012_0_0",
  "nowear_time_bias_adjusted_average_acceleration_f90087_0_0",
  "standard_deviation_of_acceleration_f90013_0_0",
  
  rownames(nzv[nzv$nzv=='TRUE',])
  
))]

dim(data2.3) # 720 202





# separate variables into numeric and categorical
data2.3$parkinsons <- as.factor(data2.3$parkinsons)
data2.3_categorical <- select_if(data2.3, negate(is.numeric)) 
data2.3_numeric <- select_if(data2.3, is.numeric) 



# impute NA using MICE algorithm
imputed_data <- mice(data2.3_numeric, m = 1, method = 'pmm', seed = 123)  # pmm: predictive mean matching
data2.4_numeric_complete <- complete(imputed_data)

imputed_data <- mice(data2.3_categorical, m = 1, method = 'pmm', seed = 123)  # pmm: predictive mean matching
data2.4_categorical_complete <- complete(imputed_data)
# 
# imputed_data <- mice(data2.3, m = 1, method = 'pmm', seed = 123)  # pmm: predictive mean matching
# data2.4 <- complete(imputed_data)


# combine the imputed numeric and categorical variables
data2.4 <- cbind(data2.4_numeric_complete, data2.4_categorical_complete)

# 6 numeric variables still have several NAs after MICE
names(which(colSums(is.na(data2.4)) > 0))
# impute them with mean
data2.4$weight_f23098_0_0[is.na(data2.4$weight_f23098_0_0)] <- mean(data2.4$weight_f23098_0_0, na.rm=TRUE)
data2.4$whole_body_water_mass_f23102_0_0[is.na(data2.4$whole_body_water_mass_f23102_0_0)] <- mean(data2.4$whole_body_water_mass_f23102_0_0, na.rm=TRUE)
data2.4$body_mass_index_bmi_f23104_0_0[is.na(data2.4$body_mass_index_bmi_f23104_0_0)] <- mean(data2.4$body_mass_index_bmi_f23104_0_0, na.rm=TRUE)
data2.4$leg_predicted_mass_right_f23114_0_0[is.na(data2.4$leg_predicted_mass_right_f23114_0_0)] <- mean(data2.4$leg_predicted_mass_right_f23114_0_0, na.rm=TRUE)
data2.4$trunk_predicted_mass_f23130_0_0[is.na(data2.4$trunk_predicted_mass_f23130_0_0)] <- mean(data2.4$trunk_predicted_mass_f23130_0_0, na.rm=TRUE)
data2.4$leg_predicted_mass_left_f23118_0_0[is.na(data2.4$leg_predicted_mass_left_f23118_0_0)] <- mean(data2.4$leg_predicted_mass_left_f23118_0_0, na.rm=TRUE)




# most of categorical variables are ordinal factor in original data
# transform to regular factor for easier interpretation
data2.5 <- data2.4 %>% 
  mutate(across(.cols = where(is.ordered), .fns = ~ factor(.,ordered = FALSE)))


# build Forward Stepwise Selection
initial_model <- glm(parkinsons ~ 1, family = binomial, data = data2.5)
full_model <- glm(parkinsons ~ ., family = binomial, data = data2.5)

forward_selected_model <- stepAIC(initial_model, 
                                  scope = list(lower = initial_model, upper = full_model), 
                                  direction = "forward")
summary(forward_selected_model)


# Visualize model results

dt <- data.frame(Variable = rownames(summary(forward_selected_model)$coefficients),
                 est = exp(summary(forward_selected_model)$coefficients[, 'Estimate']),
                 se = summary(forward_selected_model)$coefficients[, 'Std. Error'],
                 low = exp(summary(forward_selected_model)$coefficients[, 'Estimate']-summary(forward_selected_model)$coefficients[, 'Std. Error']),
                 high = exp(summary(forward_selected_model)$coefficients[, 'Estimate']+summary(forward_selected_model)$coefficients[, 'Std. Error']),
                 pval = summary(forward_selected_model)$coefficients[, 'Pr(>|z|)'])
dt <- dt[dt$pval<0.05, ]
    
dt$` ` <- paste(rep(" ", 8), collapse = " ")
dt$`  ` <- paste(rep(" ", 30), collapse = " ")
dt$`   ` <- paste(rep(" ", 8), collapse = " ")
dt$`Odds Ratio (95% CI)` <- ifelse(is.na(dt$est), "",
                                   sprintf("%.2f (%.2f, %.2f)",
                                           dt$est, dt$low, dt$high))
dt <- dt[c(-1,-6),]
rownames(dt) <- NULL

dt[8,1] <- "usual_walking_pace_f924_0_0Steady average pace vs. Slow pace"
dt[9,1] <- "usual_walking_pace_f924_0_0Brisk pace vs. Slow pace"
dt[10,1] <- "sleeplessness_insomnia_f1200_0_0Sometimes vs. Never/rarely"
dt[11,1] <- "sleeplessness_insomnia_f1200_0_0Usually vs. Never/rarely"
dt[12,1] <- "sensitivity_hurt_feelings_f1950_0_0Yes vs. Do not know"






tm <- forestploter::forest_theme(core = list(bg_params=list(fill = c("white"))),
                                 ci_Theight = 0.2# Set a T end at the end of CI 
)

p <- forestploter::forest(dt[, c(1,7:10)],
                          est = dt$est,
                          lower = dt$low, 
                          upper = dt$high,
                          sizes = dt$se,
                          ci_column = 3,
                          ref_line = 1,
                          xlim = c(0, 3),
                          #ticks_at = c(0.2, 0.5, 1,1.5, 2, 2.5),
                          theme = tm) %>% 
    add_border(
      row = c(0),
      col = NULL,
      part = c("body", "header"),
      gp = gpar(lwd = 2))

# Print plot
plot(p)


