#load relevant libraries
library (data.table) 
library(ggplot2)
library(dplyr) #### cave if also loading plyr!!!!!

####Behavioral data####
#loading csv-files
Behavioral_data<- read.csv("data/user_study_1_interaction_times.csv", sep=";")

#clean up empty rows in data table
Behavioral_data <-subset(Behavioral_data, Behavioral_data$code != "")

#####Preparation of Data#####
#use uniform labels for the same condition
Behavioral_data$warning<-ifelse(Behavioral_data$warning == "scaledown", "scaleDown",Behavioral_data$warning)

#clean up data set, only columns that we need!
Behavioral_data_tidy<-select(Behavioral_data,c("code","object","action","start_time","end_time","duration","object_group","warning")) #drop unnecessary columns

setDT(Behavioral_data_tidy) #convert data into a data table

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

#Boxplot for warning
pdf(file = "Rplots3.pdf",
    width=6.5, height=4)

plot(diff~warning,
     xlab = "Type of Warning",
     ylab = "Interaction Time [sec]",
     col = alpha(c("slategray","yellow","firebrick3","orange"),0.65),
     names = c("Blur","Pop-Up", "Red Glow", "Scale Down"),
     par(mar = c(5.1, 4.1, 1.1, 2.1)), #bottom, left, top, right
     data=test)

dev.off()

