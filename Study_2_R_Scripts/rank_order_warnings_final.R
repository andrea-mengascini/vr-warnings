#load relevant libraries
library(dplyr) #### cave if also loading plyr!!!!!
library(reshape2)

#Set working directory
setwd("/Users/Account/Documents/R_Scripts/ARSEC/Study_2")

####Behavioral data####
#loading csv-files
data<- read.csv("/Users/Account/Documents/R_Scripts/ARSEC/Study_2/Study_2_Demographics.csv", sep=",")

#keep only relevant columns
warning<-select(data, c("Participant.number","warning.1","warning.2","warning.3","warning.4"))

#bring data in longformat
warning_long <- melt(warning,id.vars="Participant.number",
                         measure.vars=c("warning.1", "warning.2","warning.3","warning.4"),
                         variable.name="order",
                         value.name="warning_type")

#ensure, that every warning condition has only one name
warning_long$warning_type<-ifelse(warning_long$warning_type=="Red","red",
                              ifelse(warning_long$warning_type=="Pop-up","pop up",
                                  ifelse(warning_long$warning_type=="Shrink","scale down",
                                      ifelse(warning_long$warning_type=="Blurry","blur",
                                        ifelse(warning_long$warning_type=="pop-up","pop up",
                                           ifelse(warning_long$warning_type=="shrink","scale down",
                                              ifelse(warning_long$warning_type=="scale-down","scale down",warning_long$warning_type)))))))

#give each order condition a rank
warning_long$order <- ifelse(warning_long$order=="warning.1",1,
                        ifelse(warning_long$order=="warning.2",2,
                          ifelse(warning_long$order=="warning.3",3,4)))

#convert warning-type to a factor and order it in a meaningful sense for the figure
warning_long$warning_type<-factor(warning_long$warning_type, levels = c("red", "blur","scale down","pop up"))
warning_long$Participant.number <- as.factor(warning_long$Participant.number)
warning_long$order<-factor(warning_long$order, levels = c("1", "2","3","4"))

#load library for the model
library (ordinal)

#ordinal cumulative link model
model1 <- clm(order~ warning_type,data=warning_long)
summary(model1)

library(car)
#test 1 scale down blur
nominal_test(model1)

#prepare pairwise comparisons
#Test 1 Scale down versus blur
hypothesis_matrix <-
  matrix(0, ncol = length(coef(model1)),nrow=1)

colnames(hypothesis_matrix) <-
  names(coef(model1))

hypothesis_matrix[1,c("warning_typescale down", "warning_typeblur")] <-
  c(1,-1)

linearHypothesis(
  model1,
  (hypothesis_matrix)
)

#test 2 scale down pop up
hypothesis_matrix <-
  matrix(0, ncol = length(coef(model1)),nrow=1)

colnames(hypothesis_matrix) <-
  names(coef(model1))

hypothesis_matrix[1,c("warning_typescale down", "warning_typepop up")] <-
  c(1,-1)

linearHypothesis(
  model1,
  (hypothesis_matrix)
)

#test 3 blur pop up
hypothesis_matrix <-
  matrix(0, ncol = length(coef(model1)),nrow=1)

colnames(hypothesis_matrix) <-
  names(coef(model1))

hypothesis_matrix[1,c("warning_typeblur", "warning_typepop up")] <-
  c(1,-1)

linearHypothesis(
  model1,
  (hypothesis_matrix)
)

#test 4 red - blur
hypothesis_matrix <-
  matrix(0, ncol = length(coef(model1)),nrow=1)

colnames(hypothesis_matrix) <-
  names(coef(model1))

hypothesis_matrix[1,c("warning_typeblur")] <-
  c(1)

linearHypothesis(
  model1,
  (hypothesis_matrix)
)

#test 5 red - pop up
hypothesis_matrix <-
  matrix(0, ncol = length(coef(model1)),nrow=1)

colnames(hypothesis_matrix) <-
  names(coef(model1))

hypothesis_matrix[1,c("warning_typepop up")] <-
  c(1)

linearHypothesis(
  model1,
  (hypothesis_matrix)
)

#test 6 red - scale down
hypothesis_matrix <-
  matrix(0, ncol = length(coef(model1)),nrow=1)

colnames(hypothesis_matrix) <-
  names(coef(model1))

hypothesis_matrix[1,c("warning_typescale down")] <-
  c(1)

linearHypothesis(
  model1,
  (hypothesis_matrix)
)

#we make a plot
#aggregate percentages per group

#set a variable to count the data
warning_long$counter <-1

#aggregate the count data
agg_count<-aggregate(counter ~ order + warning_type, data=warning_long, length)

#add an additional row for blur to complete the aggregated data as there was not any participant who has ranked blur with 1
  #1.) make a new small dataframe with the new row  
  addrow<-data.frame("1","blur","0")
  colnames(addrow)<-c("order","warning_type","counter")
  
  #2.)add new row to old dataframe
  aggregated_data<-rbind(agg_count,addrow)

  #3.)factorize factor order and convert counter into a number
  aggregated_data$order<-as.factor(aggregated_data$order)
  aggregated_data$counter<-as.numeric(aggregated_data$counter)
  #aggregated_data$warning_type<-factor(aggregated_data$warning_type, levels = c("red","blur","scale down","pop up"))
  aggregated_data$axislabel <-ifelse(aggregated_data$order==1, "1st choice",
                                     ifelse(aggregated_data$order==2, "2nd choice",
                                            ifelse(aggregated_data$order==3, "3rd choice", "Last choice")))
  aggregated_data$axislabel<-factor(aggregated_data$axislabel, levels = c("Last choice","3rd choice","2nd choice","1st choice"))
  
  #calculate percentage of participants who gave a specific warning to each rank
  aggregated_data$percent <-(aggregated_data$counter/18)*100
  
  #aggregate dataset that shows for each warning how many participants have given which rank
  aggregated_data_short<- dcast(aggregated_data,warning_type ~ order,value.var="counter")
  
#load library for plot
  library("ggplot2")
  
  pdf(file = "Rank_data.pdf",
      width=7.5, height=4)
  ggplot(aggregated_data, aes(fill=factor(warning_type), y=axislabel, x=percent)) + #defines what should be plotted
    geom_bar(position="stack", stat="identity", width=0.5, color="black")+  #defines stacked bar plot, as well as the width of the bars and the border
    scale_fill_manual(values=alpha(c( "firebrick3","slategray", "orange",'yellow'), 0.65), labels = c("   Red Glow",   "   Blur","   Scale Down","   Pop-Up" ))+
    xlab("Distribution of Choices in %") + #Axis title of y axis
    ylab("Assigned Rank") + #Axis title of y axis
    labs(fill = "Type of Warning")+          #Title of Legend
    theme_bw()  +  #deletes background color
    theme(panel.grid.major = element_blank(), #removes lines within
          panel.grid.minor = element_blank(), #removes lines within
          axis.line = element_line(color = "black", linewidth = 0.125), #adds axis lines in black with a specific size
          axis.title=element_text(size=12), #specifics of axis labels
          axis.title.y=element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)), #increase distance from y axis label to the text on the y axis
          axis.title.x=element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)), #increase distance from x axis label to the text on the y axis
          axis.text=element_text(color = "black",size=12),#specifics of axis text
          legend.background = element_rect(color = "black", linetype = "solid",linewidth = 0.25),# border around legend
          legend.text=element_text(size=12), # font size of legend text
          legend.title = element_text(size=12), #font size of legend title
          axis.ticks.length=unit(.25, "cm"),#adjust the length of the axis
          axis.ticks = element_line(color = "black",linewidth = 0.25)) #specifics of the ticks
  
  dev.off()   
  
  
  
  

  