library(tidyverse)

library(survival)
library(finalfit)
leaukaemia <- read_csv("trt.csv")


surv_leukaemia <- leaukaemia %$%
  Surv(weeks,failure)


surv_leukaemia <- leaukaemia %$% Surv(weeks, failure)


survfit_leukaemia <-survfit(surv_leukaemia~1,data=leaukaemia)

summary(survfit_leukaemia)

summary_leukaemia <- summary(survfit_leukaemia)

leaukaemia |> 
  surv_plot(surv_leukaemia,trt,pval=T)

explanatory <- c("trt")
dependent <- "surv_leukaemia"

leaukaemia |> 
  surv_plot(dependent,explanatory,pval=TRUE)

library(survminer)
library(ggsurvfit)

leukaemia <- read_csv("trt.csv")


survfit2(Surv(weeks,failure)~trt,data=leaukaemia) |> 
  ggsurvfit()+
  add_risktable()+
  labs(y="Relapse")+
  add_confidence_interval()
 
df_colon 

leukaemia
 survfit2(Surv(weeks,failure)~trt,data=leaukaemia) |> 
  ggsurvfit()+
  add_risktable()+
  labs(y="Relapse")





