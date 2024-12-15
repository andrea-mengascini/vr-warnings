#load relevant libraries
library (data.table) 
library(ggplot2)
library(dplyr) #### cave if also loading plyr!!!!!

#Set working directory
setwd("/Users/Account/Documents/R_Scripts/ARSEC/Study_1")

####Behavioral data####
#loading csv-files
Behavioral_data<- read.csv("/Users/Account/Documents/R_Scripts/ARSEC/Study_1/Behavioral_data_2023_05_25.csv", sep=";")
#clean up empty rows in data table
Behavioral_data <-subset(Behavioral_data, Behavioral_data$code != "")

#####Preparation of Data#####
#use uniform labels for the same condition
Behavioral_data$warning<-ifelse(Behavioral_data$warning == "scaledown", "scaleDown",Behavioral_data$warning)

#clean up data set, only columns that we need!
Behavioral_data_tidy<-select(Behavioral_data,c("code","object","action","start_time","end_time","duration","object_group","warning")) #drop unnecessary columns

setDT(Behavioral_data_tidy) #convert data into a data table

Behavioral_data_tidy


test <-Behavioral_data_tidy[,
                            list(
                              min_start=min(as.numeric(gsub(",",".",start_time))),#from inside to outside: changes comma to point, converts start time into a number, and looks for the smallest number in start_time
                              max_end=max(as.numeric(gsub(",",".",end_time))), #from inside to outside: changes comma to point, converts end time into a number, and looks for the largest number in end_time
                              warning=setdiff(unique(warning),"no_warning")    #if there is a different value for warning than "no warning" set the variable value to that different value
                            ),
                            keyby="code"]     # do everything above for each individual participant separately.

#calculate difference between endtime (maximum) and starttime (minimum)
test$diff <- test$max_end - test$min_start


'factorize warning'
test$warning <-as.factor(test$warning)
#ANOVA
res.aov <- aov(diff ~ warning, data = test) 
summary(res.aov)

#calculate means. For SDs change "mean" to "sd"
agg_mean<-aggregate(diff ~ warning, data=test, mean)


#Boxplot for warning
plot(diff~warning,
     xlab = "Warning",
     ylab = "Difference [sec]",
     col = c(),
     names = c("Blur","Popup", "Red", "Scaledown"),
     data=test)
