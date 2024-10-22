#load relevant libraries
library(dplyr) #### cave if also loading plyr!!!!!

#Set working directory
setwd("/Users/Walle/Documents/R_Scripts/ARSEC/Study_1")

####Behavioral data####
#loading csv-files
demographics<- read.csv("/Users/Walle/Documents/R_Scripts/ARSEC/Study_1/DEMOGRAPHICS_included.csv", sep=",")

#exclude rows without a participant code
demographics <- subset(demographics, demographics$code != "")

#exclude one participant, who did not participate in the questionnaire
demographics <- subset(demographics, demographics$excluded.from.questionnaire != "x")

#select only the relevant columns
demographics_2 <-select(demographics,c("code","X.12","X.13"))

#name the columns
colnames(demographics_2)<-c("code", "Age","Gender")

#count the different genders
demographics_2 %>% count(demographics_2$Gender, wt = NULL, sort = TRUE, name = NULL)

#count the different age classes
demographics_2 %>% count(demographics_2$Age, wt = NULL, sort = TRUE, name = NULL)

#calculate averaged age
mean(as.numeric(demographics_2$Age))

#Gender: 31 male, 6 female
#Age range: 18-72
#average age: 30.81

####demographics interview

#select only the participants with interview
demographics_interview <-subset(demographics,demographics$interview=="x")

#select only the relevant columns
demographics_interview2 <-select(demographics_interview,c("code","X.12","X.13"))

#name the columns
colnames(demographics_interview2)<-c("code", "Age","Gender")

#count the different genders
demographics_interview2 %>% count(demographics_interview2$Gender, wt = NULL, sort = TRUE, name = NULL)

#count the different age classes
demographics_interview2 %>% count(demographics_interview2$Age, wt = NULL, sort = TRUE, name = NULL)

#calculate averaged age
mean(as.numeric(demographics_interview2$Age))

#gender 6 male, 4 female
#age range: 22 - 42
#age mean: 32.2


####demographics only participants, who completed the task and whose behavioral data was analyzed
#select only the participants who completed the task and whose behavioral data was analyzed
demographics_task<-subset(demographics,demographics$excluded.from.task.analysis != "x")

#select only the relevant columns
demographics_task2 <-select(demographics_task,c("code","X.12","X.13"))

#name the columns
colnames(demographics_task2)<-c("code", "Age","Gender")

#count the different genders
demographics_task2 %>% count(demographics_task2$Gender, wt = NULL, sort = TRUE, name = NULL)

#count the different age classes
demographics_task2 %>% count(demographics_task2$Age, wt = NULL, sort = TRUE, name = NULL)

#calculate averaged age
mean(as.numeric(demographics_task2$Age))

'demographics task only'
#Gender 24 male, 6 female
#age range: 18 - 72
#averaged age: 31.37
#Gender