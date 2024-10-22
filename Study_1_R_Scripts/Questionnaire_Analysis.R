#Set working directory
  setwd("/Users/Walle/Documents/R_Scripts/ARSEC/Study_1")

####Questionnaire Data####
#load libraries
  library (dplyr) #### cave if also loading plyr!!!!!
  library(psych) #cave: ggplot 2 also has an alpha-Function!
  library(reshape2)
  library(splithalfr)

#loading questionnaire data
  Question_data<- read.csv("/Users/Walle/Documents/R_Scripts/ARSEC/Study_1/Questionnaire_data_2023_05_25.csv", sep=";")
  
#clean data set -> only columns, we need!
  #drop unnecessary columns
    Question_data_tidy<-select(Question_data,c("code","number","object","warning","question","answer")) #drop unnecessary columns
    
  #clean empty rows in data table
    Question_data_tidy<-subset(Question_data_tidy,Question_data_tidy$code!="")
    
  #use uniform labels for the same condition
    Question_data_tidy$warning <-ifelse(Question_data_tidy$warning=="Blur","blur",
                                      ifelse(Question_data_tidy$warning=="scaleDown","scaledown",
                                             ifelse(Question_data_tidy$warning== "","no warning",Question_data_tidy$warning)))

#create variables
  #variable indicating where the question comes from
    Question_data_tidy$scale <- ifelse(Question_data_tidy$question< 6, "SUS",
                                       ifelse(Question_data_tidy$question>9, "Own", "NASA"))
      
  #create variable where all question were coded in the same direction (i.e., with recoded reverse questions!)
    Question_data_tidy$answer_r <-ifelse((Question_data_tidy$question==1| 
                                          Question_data_tidy$question==4| 
                                          Question_data_tidy$question==6| 
                                          Question_data_tidy$question==8| 
                                          Question_data_tidy$question==9),6-Question_data_tidy$answer,Question_data_tidy$answer)
    


#separate dataframes for SUS questionnaire, NASA questionnaire, as well as want question and safe question
      Question_t_SUS <-subset(Question_data_tidy,Question_data_tidy$scale == "SUS")
      Question_t_NASA <-subset(Question_data_tidy,Question_data_tidy$scale == "NASA")
      Question_t_own_want <- subset(Question_data_tidy,Question_data_tidy$question == 10)
      Question_t_own_safe <- subset(Question_data_tidy,Question_data_tidy$question == 11)

      
#Calculate Cronbachs Alpha
      #select only necessary columns
      Cron_SUS_analysis_data <- select(Question_t_SUS,c("code","number","question","answer_r")) #select only relevant variables
      
      #select only necessary columns
      Cron_NASA_analysis_data <- select(Question_t_NASA,c("code","number","question","answer_r")) #select only relevant variables
      
      
      #Step 2: Convert data to short format (SUS)  
      Cron_SUS_analysis_short <- dcast(Cron_SUS_analysis_data,code~question+number,value.var="answer_r")
      
      #Step 3: Eliminate ID (SUS)
      Cron_SUS_analysis_short_noID<-select(Cron_SUS_analysis_short,c("1_1","1_2","1_3","1_4","2_1","2_2","2_3","2_4","3_1","3_2","3_3","3_4","4_1","4_2","4_3","4_4","5_1","5_2","5_3","5_4"))
      
      #Step 4: Calculate Cronbachs Alpha (Cronbachs Alpha: more than 3 items; Spearman-Brown-Coefficient: 2 Items) (SUS)
      alpha(Cron_SUS_analysis_short_noID)
      
      
      #Step 2: Convert data to short format  
      Cron_NASA_analysis_short <- dcast(Cron_NASA_analysis_data,code~question+number,value.var="answer_r")
      
      #Step 3: Eliminate ID
      Cron_NASA_analysis_short_noID<-select(Cron_NASA_analysis_short,c("6_1","6_2","6_3","6_4","7_1","7_2","7_3","7_4","8_1","8_2","8_3","8_4","9_1","9_2","9_3","9_4"))
      
      #Step 4: Calculate Cronbachs Alpha (Cronbachs Alpha: more than 3 items; Spearman-Brown-Coefficient: 2 Items) 
      alpha(Cron_NASA_analysis_short_noID)
      
        
#####Data Analysis#####

  #individual models for the items in the different subscales
    #SUS
      #load library for multilevel models
      library(lme4)
        
      #factorize specific variables:
        Question_t_SUS$code <- as.factor(Question_t_SUS$code)
        Question_t_SUS$object <- as.factor(Question_t_SUS$object)
        Question_t_SUS$warning<-factor(Question_t_SUS$warning, levels = c("no warning", "blur", "red","scaledown","popup"))
        Question_t_SUS$question <- as.factor(Question_t_SUS$question)
      
      #Build null model with intercepts
        Model_SUS_1.0<-lmer(answer_r~1+(1|code)+(1|question)+(1|object), data= Question_t_SUS)
        summary(Model_SUS_1.0) 
        
        #with random slopes # we do not use random slopes since a) there is no convergence n(random slope for code) or b) the fitted model is close to singularity with the corresponding slope see help('isSingular') for references
        Model_SUS_1.5<-lmer(answer_r~1+(warning|code)+(warning|question)+(warning|object), data= Question_t_SUS)
        isSingular(Model_SUS_1.5, tol = 1e-4)
        summary(Model_SUS_1.5)
        
          
      #Build model with fixed effect for warning  
        Model_SUS_2.0<-lmer(answer_r~warning+(1|code)+(1|question)+(1|object), data= Question_t_SUS)
        summary(Model_SUS_2.0)
        
      #Compare null model and model with warning
        anova(Model_SUS_1.0,Model_SUS_2.0) 
       
      #for p-values in the model, you can use the lmerTest package
      # cave: lmerTest has its own lmer function with p-values (i.e, if you have calculated the lmer with lme4, you have to do a new analysis (run the code again when lmerTest is loaded))
        library(lmerTest) 
        summary(Model_SUS_2.0)      
        anova(Model_SUS_2.0)
        
        #calculate values for confidence intervals
        Model_SUS_2.0_summary <-summary(Model_SUS_2.0)
        Model_SUS_2.0_summary$coefficient
        CI_SUS <- Model_SUS_2.0_summary$coefficient[,"Std. Error"]*1.96
        print(CI_SUS)
        
        
    #NASA
      #load library for multilevel models
        library(lme4)
        
      #factorize specific variables:
        Question_t_NASA$code <- as.factor(Question_t_NASA$code)
        Question_t_NASA$object <- as.factor(Question_t_NASA$object)
        Question_t_NASA$warning<-factor(Question_t_NASA$warning, levels = c("no warning", "blur", "red","scaledown","popup"))
        Question_t_NASA$question <- as.factor(Question_t_NASA$question)
      
      #Build null model     
        Model_NASA_1.0<-lmer(answer_r~1+(1|code)+(1|question)+(1|object), data= Question_t_NASA)
        summary(Model_NASA_1.0)
      
      #with random slopes # we do not use random slopes since a) there is no convergence n(random slope for code) or b) the fitted model is close to singularity with the corresponding slopp see help('isSingular') for references
        Model_NASA_1.5<-lmer(answer_r~1+(warning|code)+(warning|question)+(warning|object), data= Question_t_NASA)
        isSingular(Model_NASA_1.5, tol = 1e-4)
        summary(Model_NASA_1.5)
        
      #Build model with fixed effect for warning 
        Model_NASA_2.0<-lmer(answer_r~warning+(1|code)+(1|question)+(1|object), data= Question_t_NASA)
        summary(Model_NASA_2.0)
      
      #Compare null model and model with warning
        anova(Model_NASA_1.0,Model_NASA_2.0) 
       
      #for p-values in the model, you can use the lmerTest package
      # cave: lmerTest has its own lmer function with p-values (i.e, if you have calculated the lmer with lme4, you have to do a new analysis (run the code again when lmerTest is loaded))
        library(lmerTest)   
        summary(Model_NASA_2.0)      
        anova(Model_NASA_2.0)
      
        #calculate values for confidence intervals
        Model_NASA_2.0_summary <-summary(Model_NASA_2.0)
        Model_NASA_2.0_summary$coefficient
        CI_NASA <- Model_NASA_2.0_summary$coefficient[,"Std. Error"]*1.96
        print(CI_NASA)
     
      #Own questions:
      # Question 10 ("Want") and Question 11 ("Safe")
        #load library for multilevel models
          library(lme4)
        
        #factorize specific variables:
          Question_t_own_want$code <- as.factor(Question_t_own_want$code)
          Question_t_own_want$object <- as.factor(Question_t_own_want$object)
          Question_t_own_want$warning<-factor(Question_t_own_want$warning, levels = c("no warning", "blur", "red","scaledown","popup"))
          Question_t_own_want$question <- as.factor(Question_t_own_want$question)
          
          Question_t_own_safe$code <- as.factor(Question_t_own_safe$code)
          Question_t_own_safe$object <- as.factor(Question_t_own_safe$object)
          Question_t_own_safe$warning<-factor(Question_t_own_safe$warning, levels = c("no warning", "blur", "red","scaledown","popup"))
          Question_t_own_safe$question <- as.factor(Question_t_own_safe$question)
      
        #Models "want"
          #Build null model  
            Model_want_1.0<-lmer(answer_r~1+(1|code)+(1|object), data= Question_t_own_want)
            summary(Model_want_1.0)
      
          #with random slopes # we do not use random slopes since a) there is no convergence n(random slope for code) or b) the fitted model is close to singularity with the corresponding slopp see help('isSingular') for references or c) number of observations is lower than number of random effects
            Model_want_1.5<-lmer(answer_r~1+(warning|code)+(warning|object), data= Question_t_own_want)
            isSingular(Model_want_1.5, tol = 1e-4)
            summary(Model_want_1.5)
            
          #Build model with fixed effect for warning 
            Model_want_2.0<-lmer(answer_r~warning+(1|code)+(1|object), data= Question_t_own_want)
            summary(Model_want_2.0)  
      
          #Compare null model and model with warning  
            anova(Model_want_1.0,Model_want_2.0) 
      
            #calculate values for confidence intervals
            Model_want_2.0_summary <-summary(Model_want_2.0)
            Model_want_2.0_summary$coefficient
            CI_want <- Model_want_2.0_summary$coefficient[,"Std. Error"]*1.96
            print(CI_want)
          #for p-values in the model, you can use the lmerTest package
          #cave: lmerTest has its own lmer function with p-values (i.e, if you have calculated the lmer with lme4, you have to do a new analysis (run the code again when lmerTest is loaded))
            library(lmerTest)
            summary(Model_want_2.0)      
            anova(Model_want_2.0)
      
      
        #Models "safe"
          #Build null model 
            Model_safe_1.0<-lmer(answer_r~1+(1|code)+(1|object), data= Question_t_own_safe)
            summary(Model_safe_1.0)
      
            
          #with random slopes # we do not use random slopes since a) there is no convergence n(random slope for code) or b) the fitted model is close to singularity with the corresponding slopp see help('isSingular') for references or c) number of observations is lower than number of random effects
            Model_safe_1.5<-lmer(answer_r~1+(warning|code)+(warning|object), data= Question_t_own_safe)
            isSingular(Model_safe_1.5, tol = 1e-4)
            summary(Model_safe_1.5)
            
          #Build model with warning 
            Model_safe_2.0<-lmer(answer_r~warning+(1|code)+(1|object), data= Question_t_own_safe)
            summary(Model_safe_2.0) 
            
            #calculate values for confidence intervals
            Model_safe_2.0_summary <-summary(Model_safe_2.0)
            Model_safe_2.0_summary$coefficient
            CI <- Model_safe_2.0_summary$coefficient[,"Std. Error"]*1.96
           print(CI)
          #Compare null model and model with warning 
            anova(Model_safe_1.0,Model_safe_2.0) 
            
          #for p-values in the model, you can use the lmerTest package
          #cave: lmerTest has its own lmer function with p-values (i.e, if you have calculated the lmer with lme4, you have to do a new analysis (run the code again when lmerTest is loaded))
            library(lmerTest)
            summary(Model_safe_2.0)      
            anova(Model_safe_2.0)
      
        
            #aggregate means and SDs for the individual questions, SUS and NASA-parts
            agg_questions_want_mean<-aggregate(answer_r ~ warning+question, data=Question_t_own_want, mean)
            agg_questions_want_sd<-aggregate(answer_r ~ warning+question, data=Question_t_own_want, sd)
            
            agg_questions_safe_mean<-aggregate(answer_r ~ warning+question, data=Question_t_own_safe, mean)
            agg_questions_safe_sd<-aggregate(answer_r ~ warning+question, data=Question_t_own_safe, sd)

            agg_questions_SUS_mean<-aggregate(answer_r ~ warning, data=Question_t_SUS, mean)
            agg_questions_SUS_sd<-aggregate(answer_r ~ warning, data=Question_t_SUS, sd)
            agg_questions_NASA_mean<-aggregate(answer_r ~ warning, data=Question_t_NASA, mean)
            agg_questions_NASA_sd<-aggregate(answer_r ~ warning, data=Question_t_NASA, sd)
          
