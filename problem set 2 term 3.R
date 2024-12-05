library(tidyverse)
library(survival)
library(ggsurvfit)
library(finalfit)
library(broom)
library(gtsummary)
library(ggeasy)
library(survminer)


# question a --------------------------------------------------------------

pbcData <- read_csv("pbctrial.csv")
ff_glimpse(pbcData)
summary(pbcData)
str(pbcData)

## counts
 pbcData |> 
  select(sex,drug,death,histo,agecat) |> 
  map(~ table(.x)) |> 
  map(print)

  
## proportions
pbcData |> 
  select(sex,drug,death,histo,agecat) |> 
  map(~prop.table(table(.x))) |> 
  map(print)

## continuos variables
pbcData |> 
  select(bil,survyr,ageyr) |> 
  summary()

## check for missing values in each variable
missing_na <- pbcData |> 
  summarise(across(everything(),~ sum(is.na(.x)))) |> 
  pivot_longer(everything(),names_to = "'Variable",values_to = " Missing")
print(missing_na)

##


library(tableone)

## specify variables
variables <- c("sex","bil","survyr","death","ageyr","histo","agecat")

## create the table
table_1 <- CreateTableOne(vars=variables,strata="drug",data = pbcData,factorVars = 
                            c("sex","histo","agecat","death"))


## print variables
print(table_1,nonnormal = c("bil","survyr","ageyr"))


## survival analysis

## pbcData$SurvObj <- with(pbcData, Surv(survyr, death == 1))

  pbcData <-   pbcData |> mutate(survobject=Surv(survyr,death))   
    
    km_model <- pbcData |> 
      survfit(survobject~ 1,data=_,conf.type="log-log")
    
    summary(km_model)
    
    ## plot km curve
    km_model |> 
      ggsurvfit(color="steelblue",linewidth=1)+
      add_risktable()+
      add_confidence_interval(fill="steelblue")


# estimate survival curves for entire sample
km.overall <- survfit(survobject ~ 1, data = pbcData,
                     type="kaplan-meier", conf.type="log-log")
km.overall
summary(km.overall)

# estimate survival curves for drug group
km.drug <- survfit(survobject ~ drug, data = pbcData,
                  type="kaplan-meier", conf.type="log-log")
km.drug
summary(km.drug)

km.drug |> 
  ggsurvfit(color="steelblue",linewidth=1)+
  add_risktable()+
  add_confidence_interval(fill="steelblue")

# plot km curves
plot(km.overall)
plot(km.drug)
# log rank test for equality of survivor functions
survdiff(survobject ~ drug, data=pbcData)
# complimentary log-log plot
plot(km.drug, fun="cloglog", ylab="log(-log(Survival Probability)",
     xlab="Analysis time (shown on log scale)")


model1 = coxph(survobject~ drug, data = pbcData)
summary(model1)
model2 = coxph(survobject ~ sex + bil + as.factor(histo), data = pbcData)
summary(model2)




