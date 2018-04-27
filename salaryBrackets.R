library(tidyverse)
library(stringr)
library(tools)
source('conn.R')

mn <- 15000

suppressWarnings(fullTime <- tbl(conStudent, 'fulltime'))
currentPosition <- tbl(conStudent, 'currentPosition')
graduation <- tbl(conStudent, 'graduation') %>% select(graduationId, degreeLevel)

xx <- left_join(fullTime, currentPosition, by = 'idCurrentPosition') %>%
  select(graduationId, idCurrentPosition, salary) %>% filter(salary > mn)
xx <- left_join(xx, graduation, by = 'graduationId') %>% filter(degreeLevel == 'Bachelor')
suppressWarnings(yy <- collect(xx))

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
yy$salaryBracket <- factor(yy$salaryBracket, levels = c("low", "medium", "high"))

kaggle <- read.csv("degrees-that-pay-back.csv") %>%
  select(Undergraduate.Major, Starting.Median.Salary) %>% 
  mutate('Undergraduate.Major' = tolower(Undergraduate.Major)) %>%
  mutate('Starting.Median.Salary' = str_remove_all(Starting.Median.Salary, "[$,]")) %>%
  mutate('Starting.Median.Salary' = as.double(Starting.Median.Salary))

title <- tbl(conStudent, 'title')
graduationTitle <- tbl(conStudent, 'graduationTitle')
majors <- left_join(graduationTitle, title, by = "idTitle") %>% 
  mutate("majorName" = tolower(majorName)) %>% 
  select(graduationId, majorName) %>% collect()

majorList <- majors %>% count(majorName) %>% arrange(majorName)

majors <- majors %>% mutate('majorName' = case_when(
  majorName == "biological sciences" ~ "biology",
  majorName %in% grep("edu", majorList$majorName, value = T) ~ "education",
  majorName %in% c("agricultural business", "agricultural economics") ~ "agriculture",
  majorName == "management" ~"business management",
  majorName == "theatre" ~ "drama",
  majorName == "fine arts - art history" ~ "art history",
  majorName %in% grep("mass communication", majorList$majorName, value = T) ~ "communications",
  majorName == "construction management" ~ "construction",
  majorName == "forestry - forest management" ~ "forestry",
  majorName == "information systems and decision sciences" ~ "management information systems (mis)",
  majorName == "mathematics" ~ "math",
  majorName == "nutrition and food sciences" ~ "nutrition",
  majorName == "international studies" ~ "international relations",
  TRUE ~ majorName
))

notIn <- kaggle %>% filter(!(Undergraduate.Major %in% majors$majorName))
majorSalary <- left_join(yy, majors, by = "graduationId") %>%
  filter(majorName %in% kaggle$Undergraduate.Major)
majorSalary <- majorSalary %>% group_by(majorName) %>% summarise(LSU = median(salary))
allA <- left_join(majorSalary, kaggle, by = c("majorName" = "Undergraduate.Major"))
all <- gather(allA, key = "Source", value = "salary", -majorName) %>%
  mutate('majorName' = toTitleCase(majorName), 'Source' = case_when(
    Source == 'Starting.Median.Salary' ~ 'USA',
    TRUE ~ Source
  ))
salaryPlot <- ggplot(all) + 
  geom_col(aes(x = majorName, y = salary, fill = Source), position = "dodge") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(name = "Major") + 
  scale_y_continuous(name = "Median Salary", labels = scales::dollar) +
  ggtitle(label = 'Median Starting Salaries by Major') +
  labs(caption = "Source: WSJ")

allA %>% filter(LSU >= Starting.Median.Salary) %>%
  gather(., key = "Source", value = "salary", -majorName) %>%
  mutate('majorName' = toTitleCase(majorName), 'Source' = case_when(
    Source == 'Starting.Median.Salary' ~ 'USA',
    TRUE ~ Source
  ))-> higher
allA %>% filter(LSU < Starting.Median.Salary) %>%
  gather(., key = "Source", value = "salary", -majorName) %>%
  mutate('majorName' = toTitleCase(majorName), 'Source' = case_when(
    Source == 'Starting.Median.Salary' ~ 'USA',
    TRUE ~ Source
  ))-> lower

higherPlot <- ggplot(higher) + 
  geom_col(aes(x = majorName, y = salary, fill = Source), position = "dodge") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(name = "Major") + 
  scale_y_continuous(name = "Median Salary", labels = scales::dollar) +
  ggtitle(label = 'Median Starting Salaries by Major', subtitle = "Where the LSU Median Salary is Higher") +
  labs(caption = "Source: WSJ")

lowerPlot <- ggplot(lower) + 
  geom_col(aes(x = majorName, y = salary, fill = Source), position = "dodge") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(name = "Major") + 
  scale_y_continuous(name = "Median Salary", labels = scales::dollar) +
  ggtitle(label = 'Median Starting Salaries by Major', subtitle = "Where the LSU Median Salary is Lower") +
  labs(caption = "Source: WSJ")
