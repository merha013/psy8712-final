# Script Settings and Resources
library(tidyverse)
library(haven)
library(caret)
library(tibble)
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

## Create data to calculate confidence level in each institution by year
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

## Create data to calculate average confidence level in each institution by age
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

## Create data to calculate average confidence level in each institution by ed
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

## Create data to calculate average confidence level in the military by gender
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
### the Military. We'll look at correlations and machine learning in the 
### anlysis section to learn more.

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
### We'll look at linear regression in the analysis section to learn more.

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
### It appears confidence in the military increases very slightly with age.
### We'll look at linear regression in the analysis section to learn more.

## Overview of any difference in confidence in the military by education level
average_confidence_ed %>%
  filter(Category == "Military") %>%
  ggplot(aes(x=EDUC, y = Average_Confidence_Level)) +
  geom_step() + # display a trend line clearly depicting the value for each age
  labs(x = "Years of Education",
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
### We'll look at linear regression in the analysis section to learn more.

average_confidence_sex %>%
  filter(Category=="Military") %>%
  mutate(SEX=factor(SEX, labels = c("Male", "Female"))) %>% 
    ## change 1 to Male and 2 to Female
  ggplot(aes(x=SEX, y=Average_Confidence_Level, fill=SEX)) +
  geom_col(position = "dodge") +
  labs(x = "Gender",
       y = "Average Confidence Level",
       fill = "Category") +
  ggtitle("Average Military Confidence by Gender") +
  scale_fill_brewer("Category", palette = "Paired") +  # selecting fill color
  scale_x_discrete() +
    ## to make the Years axis not continuous
  scale_y_continuous(limits = c(NA, 3),
                     breaks = c(1, 2, 3),
                     labels = c("1" = "Hardly Any",
                                "2" = "Only Some",
                                "3" = "A Great Deal")) +
  theme_bw()
### This shows that males tend to have a slightly higher confidence in the 
### military than females.
### We'll look at a t-test in the analysis section to learn more.


# Analysis

## Calculate correlations between Military and demographics to see relationship
cleaned_data <- data %>%
  na.omit() %>%
  select(YEAR, AGE, EDUC, SEX, WRKSTAT, INCOME, Military)
correlation <- cor(cleaned_data)
correlation_df <- as.data.frame(correlation)
View(correlation_df)
### All correlations with Military are low (less than 0.15) but statistically 
### significant due to the extremely large sample size. We'll look at this 
### more with machine learning in another R file (machinelearning.R).

## Compare confidence in the military by year
data_df <- as.data.frame(data) # make it a data frame so I can run lm() on it
yr_model <- lm(Military ~ YEAR, data = data_df) # run a linear regression
summary(yr_model) # show results
### This supports rejecting the null hypothesis and concluding that there is a
### statistically significant very small positive change in the level of 
### confidence in the military over the years. However, this only explains 
### approximately 1.974% of the variation in military confidence.

## Compare confidence in the military by age
age_model <- lm(Military ~ AGE, data = data_df) # run a linear regression
summary(age_model) # show results
### This supports rejecting the null hypothesis and concluding that there is a
### statistically significant small positive change in an individual's level of ### confidence in the military with age. However, this only explains 
### approximately 0.59% of the variation in military confidence.

## Compare confidence in the military by years of education
ed_model <- lm(Military ~ EDUC, data = data_df) # run a linear regression
summary(ed_model) # show results
### This supports the null hypothesis and concluding that there is a
### statistically significant negative (rather than positive) change in an 
### individual's level of confidence in the military with more years of 
### education. However, this only explains approximately 0.5893% of the 
### variation in military confidence.

## Compare confidence in the military by gender with a t-test
male_data <- data[data$SEX=="1", "Military"]
female_data <- data[data$SEX=="2", "Military"]
(t.test(male_data, female_data))  # use t-test to compare
### This supports rejecting the null hypothesis and concluding that there is a
### statistically significant difference in the mean level of confidence 
### between males and females. Males appear to have a higher level of 
### confidence in the military than females.


# Data Export

average_confidence_yr %>%  # save this tbl to be used in a shiny app
  saveRDS(average_confidence_yr, "../shiny/final/skinny.rds")
