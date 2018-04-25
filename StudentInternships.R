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
  select(graduationId, internshipId) %>% mutate(interns = !is.na(internshipId)) %>%
  select(graduationId, interns) %>%
  count(interns)

yyy %>% 
  ggplot() +
  geom_col(aes(x = interns, y = n)) -> yGraph

xxx %>% 
  ggplot() +
  geom_bar(aes(interns)) +
  facet_wrap(~ interns) -> xGraph

source("salaryBrackets.R")

xxx %>% filter(interns == 0) %>% nrow() #There are 15174 students who have not done internship.
xxx %>% filter(interns == 1) %>% nrow() #There are 2529 studnets who have done an internship.
xxx %>% filter(interns == 2) %>% nrow() #There are 1165 students who have done internship 2 times.
xxx %>% filter(interns == 3) %>% nrow() #There are 1254 students who have done internship 3 times.
xxx %>% filter(interns == 4) %>% nrow() #There are 33 students who have done internship 4 times.


xxx %>% collect() %>%  inner_join(yy, by = "graduationId") %>% 
  ggplot() +
  geom_col(aes(x = interns, y = salaryBracket))

xxx %>% collect() %>%  inner_join(yy, by = "graduationId") -> xxxx

xxx %>% collect() %>%  inner_join(yy, by = "graduationId") %>% count(interns, salaryBracket)

xxxx %>% inner_join(majors, by = "graduationId")

