# Script Settings and Resources
library(tidyverse)
library(haven)
library(caret)
library(parallel) 
library(doParallel)
library(tictoc)

# Data Import and Cleaning
data <- read_csv("../data/data.sav")  # to pull data from the file

# Analysis
holdout_indices <- createDataPartition(
  data$Military,
  p = .25,
  list = T)$Resample1
test_tbl <- data[holdout_indices,]
train_tbl <- data[-holdout_indices,]
train_folds <- createFolds(train_tbl$Military)

## ORIGIONAL (non-parallelized version of code)
tic()
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
model1_time <-  toc()

tic()
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
model2_time <-  toc()

tic()
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
model3_time <-  toc()

tic()
model4 <- train(
  Military ~ .,
  train_tbl,
  method="xgbLinear",
  na.action = na.pass,
  tuneLength = 100, 
  preProcess = c("center","scale","zv","nzv","medianImpute"),
  trControl = trainControl(method="cv", 
                           number=10, 
                           verboseIter=T, 
                           indexOut = train_folds)
)
model4_time <-  toc()

## PARALLELIZED
local_cluster <- makeCluster(75) 
  ## amdsmall has 1 node with 128 cores per node
  ## but 127 (128-1) seems like  a lot of cores to be using.
  ## I also got a failed slurm job using 127 nodes. So, I'm trying 75.
registerDoParallel(local_cluster)

tic()
model1.par <- train( 
  Military ~ .,
  train_tbl,
  method="lm",
  na.action = na.pass,
  tuneLength = 100, 
  preProcess = c("center","scale","zv","nzv","medianImpute"),
  trControl = trainControl(method="cv", 
                           number=10, 
                           verboseIter=T, 
                           indexOut = train_folds)
)
model1.par_time <-  toc()

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
model2.par_time <-  toc()

tic()
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
model3.par_time <-  toc()

tic()
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
model4.par_time <-  toc()

stopCluster(local_cluster)
registerDoSEQ()


# Publication
cv_model1 <- model1$results$Rsquared
holdout_model1 <- cor(predict(model1, test_tbl, na.action = na.pass),
                      test_tbl$Military)^2

cv_model2 <- max(model2$results$Rsquared)
holdout_model2 <- cor(predict(model2, test_tbl, na.action = na.pass),
                      test_tbl$Military)^2

cv_model3 <- max(model3$results$Rsquared)
holdout_model3 <- cor(predict(model3, test_tbl, na.action = na.pass),
                      test_tbl$Military)^2

cv_model4 <- max(model4$results$Rsquared)
holdout_model4 <- cor(predict(model4, test_tbl, na.action = na.pass),
                      test_tbl$Military)^2

Table_3 <- tibble(
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

Table_4 <- tibble(
  algo = c("OLS Regression", 
           "Elastic Net", 
           "Random Forest", 
           "eXtreme Gradient Boosting"),
  supercomputer = (c(as.numeric(abs(model1_time$tic-model1_time$toc)), 
                as.numeric(abs(model2_time$tic-model2_time$toc)), 
                as.numeric(abs(model3_time$tic-model3_time$toc)), 
                as.numeric(abs(model4_time$tic-model4_time$toc)))),
  supercomputer_75 = c(as.numeric(abs(model1.par_time$tic-model1.par_time$toc)), 
                   as.numeric(abs(model2.par_time$tic-model2.par_time$toc)), 
                   as.numeric(abs(model3.par_time$tic-model3.par_time$toc)), 
                   as.numeric(abs(model4.par_time$tic-model4.par_time$toc)))
)

# Save Files
write_csv(Table_3, "../out/table3.csv")
write_csv(Table_4, "../out/table4.csv")