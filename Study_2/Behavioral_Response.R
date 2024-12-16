#load libraries
library(dplyr)#### cave if also loading plyr!!!!!
library(readxl) 

###Load behavioral_data####
Behavioral_data<- read.csv("data/user_study_2_behavioral_response.csv", sep=";")

#Exclude participant 16 and 22
Behavioral_data_without<-subset(Behavioral_data, Behavioral_data$participant!= 22 & Behavioral_data$participant!= 16)

#Factorize the factor variables and bring them in the correct order, if necessary
Behavioral_data_without$object<-as.factor(Behavioral_data_without$object)
Behavioral_data_without$warning<-factor(Behavioral_data_without$warning, levels = c("no_warning", "Blur", "red","scaleDown","popup"))
Behavioral_data_without$scene<-as.factor(Behavioral_data_without$scene)

#Calculate the binary Go/noGo-Variable
Behavioral_data_without$Gonogo<-ifelse(Behavioral_data_without$interacted==TRUE,1,0)

#####Models
#load the necessary libraries for the models
library(lme4)
library(emmeans)

#Build a null model with the random intercept participant and display it
model_null = glmer(Gonogo ~ 1 + (1 | participant), family=binomial,data = Behavioral_data_without)
summary(model_null)

#Build a model with the random intercept participant and the fixed effect "object" and display it
model_1 = glmer(Gonogo ~ object  + (1 | participant), family=binomial,data = Behavioral_data_without)
summary(model_1)
#test model with fixed effect "object" against null model
anova (model_null, model_1)

# we tested whether the model with a random slope for object and a random intercept for participant converge. We also checked, whether this model would fit with optimizer ='bobyqa' and optCtrl=list(maxfun = 10.000). -> it did not, therefore we dropped the model
      #model_1 = glmer(Gonogo ~ object  + (1 | participant) + (object | participant), family=binomial,data = Behavioral_data_without,control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=10000)))
      #summary(model_1)
      #anova (model_null, model_1)

#Build a model with the random intercept participant and the fixed effect "warning" and display it
model_2 = glmer(Gonogo ~ warning + (1 | participant), family=binomial,data = Behavioral_data_without)
summary(model_2)
#test model with fixed effect "warning" against null model
anova (model_null, model_2)

#Build a model with the random intercept participant and the fixed effects for "object" and "warning" and display it
model_3 = glmer(Gonogo ~ object + warning + (1 | participant), family=binomial,data = Behavioral_data_without)
summary(model_3)

#Build a model with the random intercept participant and the fixed effects for "object" and "warning" as well as their interaction and display it
'As the interaction did not converged we increased the maximum number of evaluations in the model for the optimizer bobyqa. see ?convergence for more information'
model_4 = glmer(Gonogo ~ object * warning + (1 | participant), family=binomial,data = Behavioral_data_without, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=10000)))
summary(model_4)
#Test the model with both fixed effects against the model with both fixed effects and their interaction
anova (model_3, model_4)

#Simple comparisons comparing the different warnings on each level of the factor object
model_simple_comp <- emmeans(model_4, ~ object * warning)
pairs(model_simple_comp, simple = "warning")
summary(model_simple_comp)

#Simple comparisons comparing the different objects on each level of the factor warning
model_simple_comp2 <- emmeans(model_4, ~ object * warning)
pairs(model_simple_comp2, simple = "object")
summary(model_simple_comp2)
#show a model plot for the comparisons
emmip(model_4, object ~ warning)

#Simple comparisons comparing each factor level of the factor warning with the other levels of warning
model_simple_comp3 <- emmeans(model_4, ~ warning)
pairs(model_simple_comp3, simple = "warning")
summary(model_simple_comp3)

#Calculating means

#Aggregate mean of Go/nogo for each warning-object combination 
aggr_without<-aggregate(Gonogo ~ warning+object, data=Behavioral_data_without, mean)

#add appropriate column names
colnames(aggr_without) <- c("warning","object","mean_go")

#factorize the factors warning and objects with the factor levels being in the correct order
aggr_without$warning<-factor(aggr_without$warning, levels = c("no_warning", "Blur", "red","scaleDown","popup")) #should be done for correct labels
aggr_without$object<-as.factor(aggr_without$object)

#####Figure
#load necessary libraries for the figure
library(ggplot2)

#####bar plot

#Boxplot for warning
pdf(file = "R_Interaction_object_Study2.pdf",
    width=7.5, height=4)
  ggplot(data= aggr_without,aes(x=warning, y=mean_go,fill=object)) + #specify which data to plot and what should be plotted on the x and y axis
    geom_rect(data=NULL,aes(xmin=0.25,xmax=1.5,ymin=-Inf,ymax=Inf),
              fill="whitesmoke",alpha = 0.03)+
    geom_rect(data=NULL,aes(xmin=1.5,xmax=2.5,ymin=-Inf,ymax=Inf),
              fill="slategray",alpha = 0.03)+
    geom_rect(data=NULL,aes(xmin=2.5,xmax=3.5,ymin=-Inf,ymax=Inf),
              fill="firebrick",alpha = 0.03)+
    geom_rect(data=NULL,aes(xmin=3.5,xmax=4.5,ymin=-Inf,ymax=Inf),
              fill="orange",alpha = 0.03)+
    geom_rect(data=NULL,aes(xmin=4.5,xmax=5.75,ymin=-Inf,ymax=Inf),
              fill="yellow",alpha = 0.02)+
    geom_bar(stat="identity", position=position_dodge(width=0.75),  width=0.6, color="black")+ #stat="identity skips the aggregation of geom_bar, this is necessary since we provide the aggregated values, postion = position.dodge specifies the width between the bars for each warning, width specifies the width of the bars, color = black specifies the border of the bar
    #scale_fill_manual(values=c( "firebrick3","slategray", "slategray2",'whitesmoke'),labels = c("Frame", "Pen", "Switch","Plant")) +
    scale_fill_brewer(palette="Greys",labels = c("Frame", "Pen", "Switch","Plant"))+ #adds colors and introduces the labels for the legend
    theme_bw()  +  #deletes background color
    ylab("Interactions with Object (%) \n") +    #Axis title of y axis
    xlab("\n Type of Warning")+                  #Axis title of x axis
    labs(fill = "Type of Object")+          #Title of Legend
    scale_x_discrete(labels = c("No Warning", "Blur","Red Glow","Scale Down","Pop-Up"))+ #change text of x axis labels, introduce spaces before the first and behind the last bar
    scale_y_continuous(limits=c(0,1),breaks=seq(0,1,0.1), labels=c("0"," ","20"," ","40", " ","60"," ", "80"," ","100"))+  #location of tick marks on the y axis
    #geom_errorbar(aes(ymin= lowlimit, ymax=highlimit), width=0.3, colour="black", position=position_dodge(.75))+ #specifics of the error bar: smallest value, highest value, how big the error bars are, color of the error bars and position of error bars on bar
    theme(panel.grid.major = element_blank(), #removes border lines
          panel.grid.minor = element_blank(), #removes border lines
          #panel.border = element_line(color = "black"),     #makes border black
          axis.line = element_line(color = "black", size = 0.125), #adds axis lines in black with a specific size
          axis.title=element_text(size=12), #specifics of axis labels
          axis.text=element_text(color = "black", size=12),#specifics of axis text
          legend.background = element_rect(color = "black", linetype = "solid",linewidth = 0.25), # border around legend
          axis.ticks.length=unit(.25, "cm"),#adjust the length of the axis
          axis.ticks = element_line(color = "black",size = 0.25)) #specifics of the ticks
dev.off()           
