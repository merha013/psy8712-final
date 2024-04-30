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
## The document I downloaded from https://gss.norc.org/get-the-data/spss 
## was the cumulative data set from 1972-2018. It was too large to store on 
## github. So, I'm commenting this section (in which I imported and edited it) 
## out and only leaving the final edited data file (that I used for 
## visualization and anlysis) in the data folder.
# gssALL_tbl <- read_sav("../data/GSS7218_R3.sav")
    ## read_sav is a specific version of read_spss for .sav files

## Define the columns I want to keep
### I want to look at data reflecting confidence in different industries,
### with specific interest in the military. I also don't want to use every
### demographic question as I think most won't/don't actually affect an
### individuals level of confidence in the military. So, I only picked a few
### to focus on: Confidence in other industries along with the demographics
### of age, education, gender, work status, and income.
# columns_to_keep <- c("YEAR", "AGE", "EDUC", "SEX", "WRKSTAT", "INCOME",
                     # "CONFINAN", "CONBUS", "CONCLERG", "CONEDUC", 
                     # "CONFED", "CONLABOR", "CONPRESS", "CONMEDIC", 
                     # "CONJUDGE", "CONSCI", "CONLEGIS", "CONARMY")
# data_ALL <- gssALL_tbl[,columns_to_keep]

## cleaning data
# data <- data_ALL %>%
  # filter(!is.na(CONARMY)) %>% # removes 22,018 rows with N/A for CONARMY
  # mutate_all(as.numeric) %>% # so the data is usable
  # mutate_at(vars(-YEAR, -EDUC, -SEX, -WRKSTAT, -INCOME), 
            # ~ ifelse(. ==1, 3, ifelse(. ==3,1,.))) %>%
    ## this makes 3 = "a great deal", and 1 = "hardly any"
    ## which makes it easier to interpret higher numbers as better
  # rename("Banks" = CONFINAN, "Major Companies" = CONBUS, 
         # "Organized Religion" = CONCLERG, "Education" = CONEDUC,
         # "Federal Government" = CONFED, "Organized Labor" = CONLABOR,
         # "Press" = CONPRESS, "Medicine" = CONMEDIC, 
         # "Supreme Court" = CONJUDGE, "Scientific Community" = CONSCI, 
         # "Congress" = CONLEGIS, "Military" = CONARMY)

## Save data file for easier access later
# write_csv(data, file = "../data/data.sav")
  ## write_csv is a reader function used to write .csv or .sav files

## Retrieve data file for use in analysis
data <- read_csv("../data/data.sav")  
  ## using read_csv because I used write_csv (even though its a .sav file)

## Calculate average confidence level in each institution by year
average_confidence_yr <- data %>%  # select data and...
  group_by(YEAR) %>%  # keep the years column
  summarise(across(c("Banks", "Major Companies", "Organized Religion",
                     "Education", "Federal Government", "Organized Labor",
                     "Press", "Medicine", "Supreme Court", 
                     "Scientific Community", "Congress", "Military"), 
                   mean, na.rm = TRUE)) %>%
      ## calculate the mean for each of these columns and put it in the table
  pivot_longer(cols = c("Banks", "Major Companies", "Organized Religion",
                        "Education", "Federal Government", "Organized Labor",
                        "Press", "Medicine", "Supreme Court", 
                        "Scientific Community", "Congress", "Military"),
               names_to = "Category",
               values_to = "Average_Confidence_Level")
      ## Pivot the data longer to help make a ggplot out of it

## Calculate average confidence level in each institution by gender
average_confidence_sex <- data %>%
  group_by(SEX) %>%
  summarise(across(c("Banks", "Major Companies", "Organized Religion",
                     "Education", "Federal Government", "Organized Labor",
                     "Press", "Medicine", "Supreme Court", 
                     "Scientific Community", "Congress", "Military"), 
                   mean, na.rm = TRUE)) %>%
  pivot_longer(cols = c("Banks", "Major Companies", "Organized Religion", 
                        "Education", "Federal Government", "Organized Labor",
                        "Press", "Medicine", "Supreme Court", 
                        "Scientific Community", "Congress", "Military"),
               names_to = "Category",
               values_to = "Average_Confidence_Level")
      ## Pivot the data longer to help make a ggplot out of it

## Calculate average confidence level in each institution by age
average_confidence_age <- data %>%
  group_by(AGE) %>%
  filter(!is.na(AGE)) %>% # removes rows with NA in the age
  summarise(across(c("Banks", "Major Companies", "Organized Religion",
                     "Education", "Federal Government", "Organized Labor",
                     "Press", "Medicine", "Supreme Court", 
                     "Scientific Community", "Congress", "Military"), 
                   mean, na.rm = TRUE)) %>%
  pivot_longer(cols = c("Banks", "Major Companies", "Organized Religion", 
                        "Education", "Federal Government", "Organized Labor",
                        "Press", "Medicine", "Supreme Court", 
                        "Scientific Community", "Congress", "Military"),
               names_to = "Category",
               values_to = "Average_Confidence_Level")
      ## Pivot the data longer to help make a ggplot out of it

## Calculate average confidence level in each institution by education 
average_confidence_ed <- data %>%
  group_by(EDUC) %>%
  filter(!is.na(EDUC)) %>%
  summarise(across(c("Banks", "Major Companies", "Organized Religion",
                     "Education", "Federal Government", "Organized Labor",
                     "Press", "Medicine", "Supreme Court", 
                     "Scientific Community", "Congress", "Military"), 
                   mean, na.rm = TRUE)) %>%
  pivot_longer(cols = c("Banks", "Major Companies", "Organized Religion", 
                        "Education", "Federal Government", "Organized Labor",
                        "Press", "Medicine", "Supreme Court", 
                        "Scientific Community", "Congress", "Military"),
               names_to = "Category",
               values_to = "Average_Confidence_Level")
      ## Pivot the data longer to help make a ggplot out of it


# Visualization
## Provide an overview of which institutions individuals have had the most
## confidence in over time using the three years of 1978, 1998, and 2018.
## The hypothesis is that there will be some sort of trend in which
## institutions are ranked similarly as the military. 
years_to_display <- c(1978, 1998, 2018) # IDing years for first plot.
average_confidence_yr %>%
  filter(YEAR %in% years_to_display) %>% # keep only the three years ID'd
  ggplot(aes(x=factor(YEAR), 
             y=Average_Confidence_Level,
             fill = Category)) +
  geom_col(position = "dodge") +
  labs(x = "Year",
       y = "Average Confidence Level",
       fill = "Category") +
  ggtitle("Average Confidence Levels Across Organizations by Year") +
  scale_fill_brewer("Category", palette = "Paired") +  # selecting fill color
  coord_flip() + # flip the axes and make it more readable
  scale_x_discrete(labels = function(x) as.character(x)) +
    ## to make the Years axis not continuous
  scale_y_continuous(limits = c(NA, 3),
                     breaks = c(1, 2, 3),
                     labels = c("1" = "Hardly Any",
                                "2" = "Only Some",
                                "3" = "A Great Deal")) +
  theme_bw()
### This shows that the Military along with Medicine and the Scientific
### Community tend to be ranked higher. Perhaps there is some correlation that
### those who trust Medicine or the Scientific Community also tend to trust
### the Military. Let's look at correlations to learn more.

## Calculate correlations between Military other institutions
cleaned_data <- na.omit(data)
cor(cleaned_data)
### All correlations with Military are low (less than 0.26) but statistically 
### significant due to the extremely large sample size.

## Provide an overview of any difference in confidence in the military by year
average_confidence_yr %>%
  filter(Category == "Military") %>%
  ggplot(aes(x=YEAR, y = Average_Confidence_Level)) +
  geom_step() + # displays a trend line clearly depicting the value for each yr
  labs(x = "Year",
       y = "Average Confidence Level") +
  ggtitle("Average Confidence Levels in the Military by Year") +
  scale_y_continuous(limits = c(1, 3),
                     breaks = c(1, 2, 3), 
                     labels = c("1" = "Hardly Any", 
                                "2" = "Only Some",  
                                "3" = "A Great Deal")) +
  theme_bw()
### This shows that confidence in the military has slowly increased over time.

## Provide an overview of any difference in confidence in the military by age
average_confidence_age %>%
  filter(Category == "Military") %>%
  ggplot(aes(x=AGE, y = Average_Confidence_Level)) +
  geom_step() + # display a trend line clearly depicting the value for each age
  labs(x = "Age",
       y = "Average Confidence Level") +
  ggtitle("Average Confidence Levels in the Military by Age") +
  scale_y_continuous(limits = c(1, 3),
                     breaks = c(1, 2, 3), 
                     labels = c("1" = "Hardly Any", 
                                "2" = "Only Some",  
                                "3" = "A Great Deal")) +
  theme_bw()
### This shows that confidence in the military does not change much with age.

## Overview of any difference in confidence in the military by education level
average_confidence_ed %>%
  filter(Category == "Military") %>%
  ggplot(aes(x=EDUC, y = Average_Confidence_Level)) +
  geom_step() + # display a trend line clearly depicting the value for each age
  labs(x = "Year of Education",
       y = "Average Confidence Level") +
  ggtitle("Average Confidence Levels in the Military by Education") +
  scale_y_continuous(limits = c(1, 3),
                     breaks = c(1, 2, 3), 
                     labels = c("1" = "Hardly Any", 
                                "2" = "Only Some",  
                                "3" = "A Great Deal")) +
  theme_bw()
### This shows that confidence in the military has a slight decrease with more 
### years of education.


# Analysis
## Calculate how the other variables (year, age, gender, education, workstatus,
## and income plus the level of confidence in each of the other institutions) 
## can predict confidence in the military.
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
model1_time <-  toc() # 5.58 sec elapsed

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
model2_time <-  toc() # 16.24 sec elapsed

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
model3_time <-  toc() # 359.32 sec elapsed

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
model4_time <-  toc() # 520.62 sec elapsed

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
model1.par_time <-  toc() # 11.68 sec elapsed

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
model2.par_time <-  toc() # 5.8 sec elapsed

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
model3.par_time <-  toc() # 422.03 sec elapsed

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
model4.par_time <-  toc() # 299.3 sec elapsed

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

# Save Files
write_csv(Table_1, "../out/table1.csv")
write_csv(Table_2, "../out/table2.csv")

## The results show that using Random Forest without parallelizing is the most
## accurate and timely method to see how the other variables predict confidence
## in the military, but it is still not a good predictor overall. It did well
## on the training data but not so much on the holdout. The combination of
## variables utilized do not appear to help predict trust in the military.
