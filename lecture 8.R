library(tidyverse)
library(survival)
library(ggsurvfit)
library(finalfit)
library(broom)
library(gtsummary)
library(ggeasy)

trt <- read_csv("trt.csv")
trt <- trt |> 
  mutate(weeks_bins=cut(weeks,c(0,2.5,5,7.5,10,12.5,15,17.5,20,Inf)),
         person_time=if_else())
