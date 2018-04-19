library(tidyverse)
source("conn.R")

internship <- tbl(conStudent, 'internship')
graduation <- tbl(conStudent, 'graduation')

right_join(internship, graduation, by = c('graduationId', 'graduationId')) %>%
  select(graduationId, internshipId) %>% mutate(interns = !is.na(internshipId)) %>%
  count(graduationId, interns) %>% mutate(interns = interns*n) %>% select(graduationId, interns) %>% collect() -> xxx

right_join(internship, graduation, by = c('graduationId', 'graduationId')) %>%
  select(graduationId, internshipId) %>% mutate(interns = !is.na(internshipId)) %>% count(interns) %>% collect() -> yyy

right_join(internship, graduation, by = c('graduationId', 'graduationId')) %>%
  select(graduationId, internshipId) %>% mutate(interns = !is.na(internshipId)) %>% select(graduationId, interns) %>%
  count(interns)
  
yyy %>% 
  ggplot() +
  geom_col(aes(x = interns, y = n)) -> yGraph

xxx %>% 
  ggplot() +
  geom_bar(aes(interns)) -> xGraph

source("salaryBrackets.R")
fullTime

xxx %>% collect() %>%  inner_join(yy, by = "graduationId")

