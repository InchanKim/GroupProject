---
  title: "Team 02 Project"
author: "Isabella Hundley, Admir Isnaeni, Inchan Kim, Peyton Thompson"
date: "April 20, 2018"
output: pdf_document
editor_options: 
  chunk_output_type: console
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, message = F)
library(tinytex)
library(tidyverse)
library(knitr) 
library(DBI)
library(scales)

source("conn.R")
source("salaryBrackets.R")
source("StudentInternships.R")

#ISABELLA - briefly explain the salary system in introduction

```

#Introduction to Analysis
Our team conducted an anaylysis to determine how recent LSU Bachelor's degree graduate's past internships affect their starting salary after graduation, if at all. 

[Salaries were broken into 3 quantiles: low, medium, and high....]

Our original hypothesis inferred that students who had participated in an internship prior to graduation would be amongst those who have a higher starting salary.

###Steps for Process

We first compared the starting salaries for those accepted full time against 3 different indicators:
  - if the graduate had a previous internship
- the compensation (paid or unpaid) of the internship they had
- if the graduate stayed with that company

Then, we broke down the salary performance statistics we found by LSU Majors against that of the national average from a national study conducted by the Wall Street Journal, "Majors That Pay Back".

#Results
###Number of Internships
```{r}
graduation <- tbl(conStudent, 'graduation') %>% filter(degreeLevel == 'Bachelor') %>% collect()
internship <- tbl(conStudent, 'internship') %>% collect()
organization <- tbl(conStudent, 'organization') %>% collect()
currPosition <- tbl(conStudent, 'currentPosition') %>% collect()
fulltime <- tbl(conStudent, 'fulltime') %>% collect() 
employment <- tbl(conStudent, 'employment') %>% collect()
graduationTitle <- tbl(conStudent, 'graduationTitle') %>% collect()
title <- tbl(conStudent, 'title') %>% collect()
commencement <- tbl(conStudent, 'commencement') %>% collect()

internGrad <- right_join(internship, graduation, by = c('graduationId', 'graduationId')) %>% select(graduationId, internshipId, idCollege) %>% mutate(interns = !is.na(internshipId)) %>% count(graduationId, interns, idCollege) %>% mutate(interns = interns*n) %>% mutate(interns = as.numeric(interns)) %>% select(graduationId, interns, idCollege) %>% filter(interns != 0, graduationId)

comName <- right_join(commencement, graduation, by = "commencementId") %>% distinct(commencementName)

```

Of the recent LSU undergrad graduates from `r comName[1,1]` to `r comName[8,1]`, there was a total of `r sum(internGrad$interns)` completed internships, with some students completing as many as `r internGrad %>% distinct(max(interns))`.

The following graph displays the count of the total number of internships individual graduates completed per college.  

```{r}
internGrad %>% group_by(idCollege, interns) %>% summarise(nc = n()) %>% 
  ggplot() +
  geom_col(aes(x = idCollege, y = nc, group = interns, fill = interns), position = 'dodge') +
  ggtitle("Count of Graduate Internships by College", "Internships Completed Per Graduate") + scale_fill_gradient(low = 'purple', high = 'yellow') +
  ylab("Count of Internships per Graduate") +
  xlab("College ID") +
  guides(fill=guide_legend(title="Internship Number"))

```

The College of Engineering has the most internships with a total of `r internGrad %>% group_by(idCollege) %>% summarise(nc = n()) %>% distinct(max(nc))` internships completed, and the College of Coast and Environment has the least internships completed at `r internGrad %>% group_by(idCollege) %>% summarise(nc = n()) %>% distinct(min(nc))` total.

Engineering's internships make up `r percent(internGrad %>% group_by(idCollege) %>% summarise(nc = n()) %>% distinct(max(nc)) / internGrad %>% group_by(idCollege) %>% summarise(nc = n()) %>% distinct(sum(nc)))` of the total internships completed.

###Highest Number of Internships
Internship Number | College | Total Internships
----------------- | ----- | ------
1 | Business | `r internGrad %>% group_by(idCollege, interns) %>% summarise(nc = n()) %>% ungroup(idCollege) %>%  filter(interns == '1') %>% distinct(max(nc)))`
2 | Engineering | `r internGrad %>% group_by(idCollege, interns) %>% summarise(nc = n()) %>% ungroup(idCollege) %>%  filter(interns == '2') %>% distinct(max(nc))`
3 | Engineering | `r internGrad %>% group_by(idCollege, interns) %>% summarise(nc = n()) %>% ungroup(idCollege) %>%  filter(interns == '3') %>% distinct(max(nc))`
4 | Engineering | `r internGrad %>% group_by(idCollege, interns) %>% summarise(nc = n()) %>% ungroup(idCollege) %>%  filter(interns == '4') %>% distinct(max(nc))`

The total number of students who completed internships were `r percent(length(internGrad$interns)/length(graduation$graduationId))` of the graduating class.

There were `r  ` students who did not have internships.

```{r}
#INCHAN ^^ Put students who didn't have internships

right_join(internship, graduation, by = 'graduationId') %>%
  select(graduationId, internshipId) %>% mutate(interns = !is.na(internshipId)) %>%
  count(graduationId, interns) %>% mutate(interns = interns*n) %>% 
  select(graduationId, interns) %>% collect() -> xxx

right_join(internship, graduation, by = 'graduationId') %>%
  select(graduationId, internshipId) %>% mutate(interns = !is.na(internshipId)) %>% 
  count(interns) %>% collect() -> yyy
yyy$interns <- factor(yyy$interns, levels = c("0","1"), labels = c("No", "Yes"))
yyy

right_join(internship, graduation, by = 'graduationId') %>%
  select(graduationId, internshipId) %>% mutate(interns = !is.na(internshipId)) %>%
  select(graduationId, interns) %>% filter(interns != T) -> noInterns

yyy %>% 
  ggplot() +
  geom_col(aes(x = interns, y = n)) -> yGraph
yGraph

#INCHAN ^^ double check that this is working - it didn't work for me - peyton

xxx %>% 
  ggplot() +
  geom_bar(aes(interns)) -> xGraph
xGraph

xxx %>% collect() %>%  right_join(yy, by = "graduationId") %>% 
  ggplot() +
  geom_bar(aes(x = interns, fill = salaryBracket), position = "dodge") +
  ggtitle(label = "Count of Internships Compared to Salaries") +
  ylab("Count") +
  xlab("Internship Number") +
  guides(fill = guide_legend(title = "Salary Bracket"))

xy <- xxx %>% collect() %>%  inner_join(yy, by = "graduationId") %>% inner_join(majors, by = "graduationId")

#INCHAN narrative below
```

[Inchan Narrative]

###Paid or Unpaid Internships
```{r}

graduat <- tbl(conStudent,'graduation')
graduat1 <- as_data_frame(graduat)
graduat <- tbl(conStudent,'graduation')
graduat1 <- as_data_frame(graduat)

intern <- tbl(conStudent,'internship')
intern1 <- as_data_frame(intern)

oo <- intern1 %>% inner_join(graduat1) %>% count(internshipPaid) %>% 
  filter(!is.na(internshipPaid))

paid<- tbl(conStudent,'internship') %>% inner_join(tbl(conStudent,'graduation')) %>% select(graduationId,internshipPaid)

total<- tbl(conStudent,'internship') %>% inner_join(tbl(conStudent,'graduation')) %>% select(graduationId,jobTitle,internshipPaid)%>% count(internshipPaid) %>% 
  filter(!is.na(internshipPaid))

internshipP <- paid %>% collect() 

tn <- oo %>% filter(internshipPaid== "no")
ty <- oo %>% filter(internshipPaid== "yes")
```
As noted, `r length(unique(intern1$graduationId))` graduates had internships while completing their degree.  

From this number there is a total of `r oo[1,2]` unpaid internships, and `r oo[2,2]` paid internships taken by students that graduated in 2017.  

In this analysis we will find out about the correlation between paid and unpaid internship on the starting salaries for graduated students in 2017.  

```{r}
paidcol <- tbl(conStudent,'internship') %>% inner_join(tbl(conStudent,'graduation')) %>% select(graduationId,jobTitle,internshipPaid)%>% count(internshipPaid) %>% 
  filter(!is.na(internshipPaid)) %>% collect() %>% 
  ggplot()+
  geom_col(aes(internshipPaid, n), fill = 'purple')
paidcol

#ADMIR NEED TITLE and AXIS TITLES

```

```{r}
kk <- paid %>% collect() %>% right_join(yy, by = "graduationId") %>%  filter(!is.na(internshipPaid)) %>% select(salaryBracket, internshipPaid) %>% count(salaryBracket, internshipPaid)%>% mutate(percentage = percent(round(n/2099, digits = 3)))

```

From the dataset we found out that `r sum(kk$n)` get a job right after graduation. The distribution based on the salary bracket is :
  
  * `r kk[1,3]` graduates with unpaid internships, and `r kk[2,3]` graduates with paid internships got high starting salaries.
* `r kk[5,3]` graduates with unpaid internship, and `r kk[6,3]` graduates with paid internship got medium starting salaries.
* `r kk[3,3]` graduates with unpaid internship, and `r kk[4,3]` graduates with paid internship got low starting salaries.

```{r}
paidbar <- paid %>% collect() %>% right_join(yy, by = "graduationId") %>%  filter(!is.na(internshipPaid)) %>% 
  ggplot() + geom_bar(aes(x = internshipPaid, fill = salaryBracket), position = "dodge") + scale_fill_discrete(guide_legend(title = "Salary Bracket")) 
paidbar
```

Based on data and graph above we can conclude that paid internship have highly positive correlation on starting salary with the highest percentage of `r kk[6,4]` for high starting salary, and second highest of `r kk[4,4]` for medium starting salary, together making up more than half of those having an internship. 

###Internship Turnovers

```{r}

major <- left_join(graduationTitle, title, by = "idTitle") %>% select(graduationId, majorName, titleCode)

internCurrent <- inner_join(internship, currPosition, by = c('graduationId')) %>% mutate(organizationId = as.numeric(organizationId.y)) %>% mutate(intOrganizationId = as.numeric(organizationId.x)) %>% select(internshipId, academicCredit, graduationId, intOrganizationId, organizationId, idCurrentPosition, positionType)

byMajor <- left_join(internCurrent, major, by = 'graduationId')%>% distinct(graduationId, .keep_all = TRUE) %>% mutate(nOrs = if_else(condition = intOrganizationId == organizationId, true = "Same Company", false = "New Company")) %>% select(graduationId, internshipId, intOrganizationId, organizationId, nOrs, positionType, majorName, titleCode, idCurrentPosition, academicCredit) %>% filter(!is.na(nOrs))

co <- ggplot(byMajor) +
  geom_bar(aes(x = nOrs), fill = 'purple') +
  ggtitle("New vs Same Company", "Comparing Interns Who Left or Stayed Upon Graduation") +
  xlab("Employment Company") +
  ylab("Count")
co  

```

As seen in the graph above, `r byMajor %>% group_by(nOrs) %>% summarise(nnn = n()) %>% ungroup(nOrs) %>% distinct(max(nnn))` decided to venture to a position in a new company while `r byMajor %>% group_by(nOrs) %>% summarise(nnn = n()) %>% ungroup(nOrs) %>% distinct(min(nnn))` took on a fulltime position with the same company.

```{r}
stayed <- byMajor %>% filter(d == 'Same Company') 
newco <- byMajor %>% filter(d == 'New Company')
ftstay <- left_join(stayed, fulltime, by = "idCurrentPosition") %>% select(salary) %>% filter(salary > 1)
ftnew <- left_join(newco, fulltime, by = "idCurrentPosition") %>% select(salary) %>% filter(salary > 6)
ftTotSalary <- fulltime %>% select(salary) %>% filter(salary > 6) 

salCo <- left_join(byMajor, yy, by = 'graduationId') %>%
  filter(!is.na(salaryBracket)) %>% 
  select(salaryBracket, nOrs) %>% count(salaryBracket, nOrs) %>% mutate(perc = percent(n/sum(n)))

salCo %>% ggplot() +
  geom_col(aes(x = nOrs, y = n, fill = salaryBracket), position = 'dodge') +
  ggtitle("Salary for Full Time Employees", "New Company vs Same Company") +
  xlab("Employment Company") +
  ylab("Count") + scale_fill_discrete(guide_legend(title = "Salary Bracket")) 
#+ scale_color_continuous(low = "purple", high = "yellow")

salCo
```

In breaking down the Employment Company by Salary Brackets, we found that overall there were slightly less people in each salary bracket who stayed with the same company, ultimately with new companies offering more high income salaries with `r salCo[5,4]` offering high income salaries and `r salCo[3,4]` serving medium salaries.  

In addition to the graph above and with the removal of outliers - of the `r length(ftTotSalary$salary)` students who submitted their salary for full-time positions, we found the following...

Employment Company | Total | Percent of Full Time | Salary Total | Salary Average
-------------------| ------ |--------------- | --------------- | ---------------
  Same Company | `r length(stayed$organizationId)` | `r  percent(length(stayed$organizationId)/length(fulltime$idCurrentPosition))` | `r dollar(sum(ftstay))` | `r dollar(sum(ftstay))` | `r dollar(sum(ftstay)/length(ftstay$salary))`
New Company | `r length(newco$organizationId)` |  `r  percent(length(newco$organizationId)/length(fulltime$idCurrentPosition))` | `r length(ftnew$salary)` | `r dollar(sum(ftnew)/length(ftnew$salary))`


###How LSU Graduates Stack Up
To put everything in perspective, we wanted to compare the starting salaries of LSU graduates to that of the average college graduate in the US. To do this, we used a dataset from *The Wall Street Journal* (WSJ) that included the median starting salaries of college graduates by major: http://online.wsj.com/public/resources/documents/info-Degrees_that_Pay_you_Back-sort.html.  
Of the `r nrow(kaggle)` majors listed in the WSJ dataset, only `r nrow(kaggle) - nrow(notIn)` matched up with the majors of the LSU graduates who went into the workforce after graduation. Some of the major names had to be adjusted to match up properly to the WSJ dataset.  

The changes that were made:
  
  LSU | WSJ
---------------------------------------- | --------------------------------------
  biological sciences | biology
management | business management
theatre | drama
fine arts - art history | art history
construction management | construction
forestry - forest management | forestry
information systems and decision sciences | management information systems (mis)
mathematics | math
nutrition and food sciences | nutrition
international studies | international relations

Also :
  
  * `r grep("edu", majorList$majorName, value = T)` were changed to **education**
  * agricultural business, agricultural economics were changed to **agriculture**
  * `r grep("mass communication", majorList$majorName, value = T)` were changed to **mass communication**
  
  ```{r}
salaryPlot
```

There are `r length(unique(all$majorName))` majors listed in this graph, so it might be hard to tell which is which. We've broken it down into two graphs instead.

```{r}
higherPlot
lowerPlot
```

As you can see, there are `r length(unique(higher$majorName))` LSU majors that pay equal to or higher than the national median and `r length(unique(lower$majorName))` LSU majors that pay less than the national median.

Some majors have a wider range of salaries than others, so we have included box-and-whisker plots at the end of this report for each major.

#Conclusion


#Salary Box-and-Whisker Plots

```{r}
#source("boxPlotMaker.R")
```


![College of Agriculture](plots/boxAgriculture.png)

![College of Art & Design](plots/boxArtDesign.png)

![College of Business](plots/boxBusiness.png)

![College of Engineering](plots/boxEngineering.png)

![College of Humanities & Social Sciences](plots/boxHumanitiesSocialSciences.png)

![College of Human Sciences & Education](plots/boxHumanSciencesandEducation.png)

![College of Mass Communication](plots/boxMassCommunication.png)

![College of Music & Dramatic Arts](plots/boxMusicDramaticArts.png)

![College of Science](plots/boxScience.png)