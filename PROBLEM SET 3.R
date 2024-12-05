library(tidyverse)
library(survival)
library(ggsurvfit)
library(broom)
library(finalfit)

binData <- read_csv("binlymph.csv")
binData

# q1b ---------------------------------------------------------------------


binData |> 
  ggplot(aes(x=mid_days,y=Survival,color=factor(stage)))+
  geom_line(linewidth = 1)+
  geom_point(size = 3)+
  theme_bw()+
  labs(color="Cancer Stage")+
  scale_color_discrete(labels=c("Stage3","stage4"))+
  scale_colour_manual(values = c("red",'blue'))
  



# Q1c ---------------------------------------------------------------------

binData <- binData |> 
  mutate(t=mid_days,
         N=P_Days,
         t_60=t-60,
         t60sp=if_else(t>60,t-60,0))


# Q1d ---------------------------------------------------------------------

modelA <- binData |> 
  glm(D ~ stage,offset = log(N),family=poisson(link="log"),data=_)
summary(modelA)
tidy(modelA)
modelA$coefficients
exp(modelA$coefficients)
exp(confint.default(modelA))


modelB <- binData |> 
  glm(D ~ stage + t_60, offset=log(N),family=poisson(link="log"),data=_)
summary(modelB)
tidy(modelB)
modelB$coefficients
exp(modelB$coefficients)
exp(confint.default(modelB))


modelC <- binData |> 
  glm(D ~ stage + t_60 + t60sp,offset=log(N),family = poisson(link = "log"),data=_)
summary(modelC)
tidy(modelC)
modelC$coefficients
exp(modelC$coefficients)
exp(confint.default(modelC))


modelD <-binData |> 
  glm(D ~ stage*t_60+ stage*t60sp,offset=log(N),family=poisson(link = "log"),data=_)
summary(modelD) 
tidy(modelD)
modelD$coefficients  
exp(modelD$coefficients)  
exp(confint.default(modelD))

# Q1h ---------------------------------------------------------------------


AIC(modelA, modelB, modelC, modelD)
  


# Q1I ---------------------------------------------------------------------

lymphdata <- read_csv("lymphoma.csv")

## Kaplan_Meier Plots

SurvObj <- lymphdata %$% Surv(days,died==1)

lymphdata$SurvObj <- lymphdata %$% Surv(days,died==1)

km.stage <- survfit(SurvObj ~ stage,data = lymphdata,type = "kaplan-meier",conf.type = "log-log" )
summary(km.stage)
tidy(km.stage)

km.stage |> 
  ggsurvfit(linewidth = 0.8)+
  labs(title = "Kaplan-Meier Survival Estimates by Cancer Stage",
       x="Time",y = "S(t)",color = "Cancer Stage")+
  scale_color_discrete(labels=c("Stage3","stage4"))+
  scale_colour_manual(values = c("red",'blue'))
?ggsurvfit



# Q1k ---------------------------------------------------------------------

## log-rank test to compare two kmplots

survdiff(SurvObj ~ stage, data = lymphdata)


# Q1i ---------------------------------------------------------------------

# Cox proportional hazards model

model1 <- coxph(SurvObj ~ stage, data = lymphdata, ties = "breslow")
summary(model1) 
tidy(model1)
  
  














  
  
  






