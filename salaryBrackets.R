library(tidyverse)
source('conn.R')

fullTime <- tbl(conStudent, 'fulltime')
currentPosition <- tbl(conStudent, 'currentPosition')

xx <- left_join(fullTime, currentPosition, by = 'idCurrentPosition') %>%
  select(graduationId, idCurrentPosition, salary)
yy <- collect(xx)

avgSalary <- mean(yy$salary, na.rm = T)
boxStats <- boxplot.stats(yy$salary)
quantiles <- boxStats$stats
names(quantiles) <- c("low", "Q1", "Q2", "Q3", "high")

yy <- filter(yy, !is.na(salary))
yy %>% 
  mutate(salaryBracket = case_when(
    salary > quantiles['Q3'] ~ "high",
    salary < quantiles['Q1'] ~ "low",
    TRUE ~ "medium"
  )) -> yy

kaggle <- read.csv("degrees-that-pay-back.csv")
