library(tidyverse)
library(survival)
library(ggsurvfit)
library(finalfit)
library(broom)
library(gtsummary)
library(ggeasy)

nepal_class <- read_csv("nepal_class7.csv")

coxmodel <- coxph(Surv(time,status)~over40+ mismatchlevel,data=newstan)
summary(coxmodel)

ff_glimpse(nepal_class)

survfit2(Surv(time,status) ~ surg,data = colon_ca) |> 
  ggsurvfit(linewidth=1)+
  add_risktable()

survfit2(Surv(stime,cens))