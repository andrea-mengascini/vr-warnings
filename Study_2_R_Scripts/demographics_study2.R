#load relevant libraries
library(dplyr) #### cave if also loading plyr!!!!!

#Set working directory
setwd("/Users/Walle/Documents/R_Scripts/ARSEC/Study_2")

####Behavioral data####
#loading csv-files
demographics<- read.csv("/Users/Walle/Documents/R_Scripts/ARSEC/Study_2/Study_2_Demographics.csv", sep=",")

#count how many participants are female, male, or diverse
demographics %>% count(demographics$Sex, wt = NULL, sort = TRUE, name = NULL)
#count how often which age is present in the data
demographics %>% count(demographics$Age, wt = NULL, sort = TRUE, name = NULL)

#calculate mean for age
mean(as.numeric(demographics$Age),na.rm=TRUE)

#Female = 7, Male = 11; Age range:19-36,Age_mean = 25.33