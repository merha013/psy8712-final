# Script Settings and Resources
library(tidyverse)
library(haven)
library(caret)
library(tibble)
library(parallel) # added to enable parrallelization
library(doParallel) # added to enable parrallelization 
library(tictoc) # added to track times
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Data Import and Cleaning

data <- read_csv("../data/data.sav")  # Retrieve data file for use in analysis
## using read_csv because I used write_csv (even though its a .sav file)

## Analysis

## Calculate how the other variables (year, age, gender, education, 
## work status, and income plus the level of confidence in each of the other 
## institutions) can predict confidence in the military with machine learning.
holdout_indices <- createDataPartition(
  data$Military,
  p = .25,
  list = T)$Resample1
test_tbl <- data[holdout_indices,]
train_tbl <- data[-holdout_indices,]
train_folds <- createFolds(train_tbl$Military)

## ORIGIONAL (non-parallelized version of code)
tic()
set.seed(8712)
model1 <- train( 
  Military ~ ., 
  train_tbl,
  method="lm",
  na.action = na.pass,
  preProcess = c("center","scale","zv","nzv","medianImpute"),
  trControl = trainControl(method="cv", 
                           number=10, 
                           verboseIter=T, 
                           indexOut = train_folds)
)
model1_time <-  toc() # 8.08 sec elapsed

tic()
set.seed(8712)
model2 <- train(
  Military ~ .,
  train_tbl,
  method="glmnet",
  na.action = na.pass,
  preProcess = c("center","scale","zv","nzv","medianImpute"),
  trControl = trainControl(method="cv", 
                           number=10, 
                           verboseIter=T, 
                           indexOut = train_folds)
)
model2_time <-  toc() # 16.97 sec elapsed

tic()
set.seed(8712)
model3 <- train(
  Military ~ .,
  train_tbl,
  method="ranger",
  na.action = na.pass,
  preProcess = c("center","scale","zv","nzv","medianImpute"),
  trControl = trainControl(method="cv", 
                           number=10, 
                           verboseIter=T, 
                           indexOut = train_folds)
)
model3_time <-  toc() # 324.61 sec elapsed

tic()
set.seed(8712)
model4 <- train(
  Military ~ .,
  train_tbl,
  method="xgbLinear",
  na.action = na.pass,
  preProcess = c("center","scale","zv","nzv","medianImpute"),
  trControl = trainControl(method="cv", 
                           number=10, 
                           verboseIter=T, 
                           indexOut = train_folds)
)
model4_time <-  toc() # 453.53 sec elapsed

## PARALLELIZED
local_cluster <- makeCluster(detectCores() - 1) 
## to customize the number of cores used
registerDoParallel(local_cluster)
## signals to R that anytime something can be parrallelized, to do so.

tic()
model1.par <- train( 
  Military ~ ., 
  train_tbl,
  method="lm",
  na.action = na.pass,
  preProcess = c("center","scale","zv","nzv","medianImpute"),
  trControl = trainControl(method="cv", 
                           number=10, 
                           verboseIter=T, 
                           indexOut = train_folds)
)
model1.par_time <-  toc() # 7.17 sec elapsed

tic()
model2.par <- train(
  Military ~ .,
  train_tbl,
  method="glmnet",
  na.action = na.pass,
  preProcess = c("center","scale","zv","nzv","medianImpute"),
  trControl = trainControl(method="cv", 
                           number=10, 
                           verboseIter=T, 
                           indexOut = train_folds)
)
model2.par_time <-  toc() # 5.48 sec elapsed

tic()
set.seed(8712)
model3.par <- train(
  Military ~ .,
  train_tbl,
  method="ranger",
  na.action = na.pass,
  preProcess = c("center","scale","zv","nzv","medianImpute"),
  trControl = trainControl(method="cv", 
                           number=10, 
                           verboseIter=T, 
                           indexOut = train_folds)
)
model3.par_time <-  toc() # 385.53 sec elapsed

tic()
set.seed(8712)
model4.par <- train(
  Military ~ .,
  train_tbl,
  method="xgbLinear",
  na.action = na.pass,
  preProcess = c("center","scale","zv","nzv","medianImpute"),
  trControl = trainControl(method="cv", 
                           number=10, 
                           verboseIter=T, 
                           indexOut = train_folds)
)
model4.par_time <-  toc() # 224.64 sec elapsed

stopCluster(local_cluster)
registerDoSEQ()


# Publication

## Show details from each of the models
model1
cv_model1 <- model1$results$Rsquared
holdout_model1 <- cor(predict(model1, test_tbl, na.action = na.pass),
                      test_tbl$Military)^2

model2
cv_model2 <- max(model2$results$Rsquared)
holdout_model2 <- cor(predict(model2, test_tbl, na.action = na.pass),
                      test_tbl$Military)^2

model3
cv_model3 <- max(model3$results$Rsquared)
holdout_model3 <- cor(predict(model3, test_tbl, na.action = na.pass),
                      test_tbl$Military)^2

model4
cv_model4 <- max(model4$results$Rsquared)
holdout_model4 <- cor(predict(model4, test_tbl, na.action = na.pass),
                      test_tbl$Military)^2

summary(resamples(list(model1, model2, model3, model4)), metric="Rsquared")
dotplot(resamples(list(model1, model2, model3, model4)), metric="Rsquared")

Table_1 <- tibble(
  algo = c("OLS Regression", 
           "Elastic Net", 
           "Random Forest", 
           "eXtreme Gradient Boosting"),
  cv_rsq = c(
    str_remove(formatC(cv_model1, format = 'f', digits = 2), "^0"),
    str_remove(formatC(cv_model2, format = 'f', digits = 2), "^0"),
    str_remove(formatC(cv_model3, format = 'f', digits = 2), "^0"),
    str_remove(formatC(cv_model4, format = 'f', digits = 2), "^0")),
  ho_rsq = c( # round all values to 2 decimal places & with no leading zero
    str_remove(formatC(holdout_model1, format = 'f', digits = 2), "^0"),
    str_remove(formatC(holdout_model2, format = 'f', digits = 2), "^0"),
    str_remove(formatC(holdout_model3, format = 'f', digits = 2), "^0"),
    str_remove(formatC(holdout_model4, format = 'f', digits = 2), "^0"))
)

Table_2 <- tibble(
  algo = c("OLS Regression", 
           "Elastic Net", 
           "Random Forest", 
           "eXtreme Gradient Boosting"),
  original = (c(as.numeric(abs(model1_time$tic-model1_time$toc)), 
                as.numeric(abs(model2_time$tic-model2_time$toc)), 
                as.numeric(abs(model3_time$tic-model3_time$toc)), 
                as.numeric(abs(model4_time$tic-model4_time$toc)))),
  parallelized = c(as.numeric(abs(model1.par_time$tic-model1.par_time$toc)), 
                   as.numeric(abs(model2.par_time$tic-model2.par_time$toc)), 
                   as.numeric(abs(model3.par_time$tic-model3.par_time$toc)), 
                   as.numeric(abs(model4.par_time$tic-model4.par_time$toc)))
)

## The results show that using Random Forest without parallelizing is the most
## accurate and timely method to see how the other variables predict confidence
## in the military, but it is still not a good predictor overall. It did well
## on the training data but not so much on the holdout. The combination of
## variables utilized do not appear to help predict trust in the military.

# Data Export
write_csv(Table_1, "../out/table1.csv")  # Save Files
write_csv(Table_2, "../out/table2.csv")  # Save Files
