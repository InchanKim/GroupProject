source("salaryBrackets.R")

college <- tbl(conStudent, "college")
graduate <- tbl(conStudent, 'graduation')
big <- left_join(graduationTitle, title, by = "idTitle") %>% 
  right_join(graduate, by = "graduationId") %>%
  left_join(college, by = "idCollege") %>%
  select(graduationId, majorName, collegeName) %>%
  collect() %>%
  right_join(yy, by = 'graduationId') %>%
  filter(salary > 15000, salary < 150000)

sub <- split(big, big$collegeName)
subPlots <- sub %>% map(~ ggplot(.) + geom_boxplot(aes(x = majorName, y = salary)) +
                          ggtitle(label = "Salaries by Major") +
                          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
                          scale_x_discrete(name = "Major") +
                          scale_y_continuous(name = "Salary"))
subFiles <- paste0("plots/box", names(sub) %>% str_remove_all("[ &]"), ".png")

map2(subFiles, subPlots, ggsave)

