library(tidyverse)
library(survival)
library(ggsurvfit)
library(finalfit)
library(broom)
library(gtsummary)
library(ggeasy)


heart <- stanford2 

mismatchlevel<-cut(stanford2$t5, breaks=3, labels = c(0, 1, 2))
over40<-cut(stanford2$age, breaks=c(-Inf,40,Inf), labels=c(0,1))
stan<-cbind(stanford2, mismatchlevel, over40)

# remove any rows that have missing values (this part important for anova)
numbermissing<-apply(stan, 1, function(x) sum(is.na(x)))
nonemissing<-numbermissing==0
newstan<-stan[nonemissing,]

## fit the cox_PH model,and use over 40
## and MMLevel

coxmodel <- coxph(Surv(time,status)~over40+ mismatchlevel,data=newstan)
summary(coxmodel)
coxmodel

## no coefficient for intercept because in cox models,baseline 
## interpretation for the exp(coef) of 40 is that ät a given instant in time,someone who
## is over 40 is 1.72  times as likely to die as someone who is under 40
## adjusting for mismatch level or you can say
#3 Ä person who is ver 40 is 72% more likely to die than someone who is under 40
##adjustng for mismatch level 

# the hazard ratio from the output is the exp(-coef) = 0.5797 meaning someone who is 
## under 40 is 0.59 times as likely to die as somoene who is over 40 adjusting for 
# mismatch level

##concordance is a goodness fitness of fit test for survival
##fraction of pairs of observations which are concordant i.e model prediction is 
## concordant with observed data


## comparing nested models using the Likelihood ratio test(can we drop the MMlevel?)
coxmodel2 <- coxph(Surv(time,status)~over40,data=newstan)
anova(coxmodel2,coxmodel,test="LRT")

## p_value of 0.1998 which implies that the we fail to reject the hull hypothesis
## that the two models are the same,meaning MMlevel is not important for survival


##we could include numeric covariates 
## that we could direcltly compare to the KM_model
coxmodel3 <- coxph(Surv(time,status)~age + t5,data=newstan)
summary(coxmodel3)

##exp(coef) = 1.03 which is interpreted as ät any given point in time,the 
## probability of dying for somoen who is 1 year older is 3% higher than someone 
## who  is younger adjusting for t5 score


# CHECKING COX PH MODEL ASSUMPTIONS ---------------------------------------
# how to check linearity...that the relationship between any of the
## numeric covariates and the log_hazard is linear


 























