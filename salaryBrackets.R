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

kaggle <- read.csv("degrees-that-pay-back.csv") %>%
  select(Undergraduate.Major, Starting.Median.Salary) %>% 
  mutate('Undergraduate.Major' = tolower(Undergraduate.Major))

title <- tbl(conStudent, 'title')
graduationTitle <- tbl(conStudent, 'graduationTitle')
majors <- left_join(graduationTitle, title, by = "idTitle") %>% 
  mutate("majorName" = tolower(majorName)) %>% 
  select(graduationId, majorName) %>% collect()
majorSalary <- left_join(yy, majors, by = "graduationId") %>%
  filter(majorName %in% kaggle$Undergraduate.Major)

notIn <- kaggle %>% filter(!(Undergraduate.Major %in% majors$majorName))
majorList <- majors %>% count(majorName) %>% arrange(majorName)

majors <- majors %>% mutate('majorName' = case_when(
  majorName == "biological sciences" ~ "biology",
  majorName %in% grep("agricult", majorList$majorName, value = T) ~ "agriculture",
  majorName == "management" ~"business management",
  majorName == "theatre" ~ "drama",
  majorName == "fine arts - art history" ~ "art history",
  majorName %in% grep("mass communication", majorList$majorName, value = T) ~ "communications",
  majorName == "construction management" ~ "construction",
  majorName == "forestry - forest management" ~ "forestry",
  majorName == "information systems and decision sciences" ~ "management information systems (mis)",
  majorName %in% grep("mathematic", majorList$majorName, value = T ) ~"math",
  majorName == "nutrition and food sciences" ~ "nutrition",
  majorName == "kinesiology" ~ "physician assistant",
  majorName == "international studies" ~ "international relations",
  TRUE ~ majorName
))


