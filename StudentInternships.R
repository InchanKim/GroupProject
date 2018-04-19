library(tidyverse)
source("conn.R")

internship <- tbl(conStudent, 'internship')
graduation <- tbl(conStudent, 'graduation')

right_join(internship, graduation, by = c('graduationId', 'graduationId')) %>%
  select(graduationId, internshipId) %>% mutate(interns = !is.na(internshipId)) %>%
  count(graduationId, interns) %>% mutate(interns = interns*n) %>% select(graduationId, interns) -> x

right_join(internship, graduation, by = c('graduationId', 'graduationId')) %>%
  select(graduationId, internshipId) %>% mutate(interns = !is.na(internshipId)) %>% count(interns) -> y

