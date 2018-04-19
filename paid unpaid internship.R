
library(tidyverse)
source('conn.R')


#paid unpaid intership


paid<- tbl(conStudent,'internship') %>% inner_join(tbl(conStudent,'graduation')) %>% select(graduationId,jobTitle,internshipPaid) %>% 
  filter(internshipPaid == 'Yes')




unpaid <- tbl(conStudent,'internship') %>% inner_join(tbl(conStudent,'graduation')) %>% select(graduationId,jobTitle,internshipPaid) %>% 
  filter(internshipPaid == 'No')

total<- tbl(conStudent,'internship') %>% inner_join(tbl(conStudent,'graduation')) %>% select(graduationId,jobTitle,internshipPaid)%>% count(internshipPaid) %>% 
  filter(!is.na(internshipPaid))

