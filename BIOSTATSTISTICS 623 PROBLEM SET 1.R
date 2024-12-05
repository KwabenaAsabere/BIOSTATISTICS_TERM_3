library(tidyverse)
library(finalfit)
library(broom)
nepal621 <- read_csv("nepal621.csv")


power.prop.test(n=NULL, p1=0.0294, p2=0.0245, sig.level=0.05, power=0.8,
                alternative="two.sided")

str(nepal621)
nepal <- nepal621 |> 
  mutate(age=as.factor(age),
         sex=as.factor(sex),
         trt=as.factor(trt),
         status=as.factor(status))
str(nepal)
nepal <- nepal |> 
  filter(age!="3-4")
  
nepal |> 
  summary_factorlist(dependent="status",
                     explanatory="trt")


table <- nepal %$% 
  table(trt,status) 
addmargins(table)  

nepal |> 
  prop.table(trt,status)

power.prop.test(n=NULL, p1=0.0294, p2=0.0245, sig.level=0.05, power=0.8,
                alternative="two.sided")


power.prop.test(n=NULL, p1=0.0244, p2=0.0245, sig.level=0.05, power=0.8,
                alternative="two.sided")























  
