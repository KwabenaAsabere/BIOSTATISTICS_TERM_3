library(tidyverse)
library(survival)
library(ggsurvfit)
library(broom)
library(finalfit)
binData <- read_csv("binlymph.csv")
lymphdata <- read_csv("lymphoma.csv")

Lymph_surv <- survfit2(Surv(days,died==1) ~ stage,data = lymphdata ) |> 
  tidy_survfit()
Lymph_surv

Lymph_surv |> 
  ggplot(aes(x=time,y= estimate,min=conf.low,
             ymax = conf.low,color = strata,fill = strata))+
  geom_step()+
  theme_bw()+
  scale_colour_manual(values=c("red","blue"))


survfit2(Surv(days,died==1) ~ stage,data = lymphdata ) |> 
  tidy_survfit() |> 
  ggplot(aes(x=time,y= estimate,min=conf.low,
             ymax = conf.low,color = strata,fill = strata))+
  geom_step()+
  theme_bw()+
  scale_colour_manual(values=c("red","blue"))+
  scale_color_discrete(labels=c("Stage 3","Stage 4"))
