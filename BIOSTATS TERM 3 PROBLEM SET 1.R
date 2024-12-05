library(tidyverse)
library(finalfit)
library(broom)
nepal621 <- read_csv("nepal621.csv")

nepal <- nepal621 |> 
  mutate(age=as.factor(age),
         sex=as.factor(sex),
         trt=as.factor(trt),
         status=as.factor(status))

nepal <- nepal |> 
  filter(age!="3-4")

addmargins(xtabs(~trt+status,data=nepal))

table <- nepal %$% 
  table(trt,status) 
addmargins(table) 


## For RR =1.2
power.prop.test(n=NULL, p1=0.0244, p2=0.0203, power=0.8)
power.prop.test(n=NULL, p1=0.0294, p2=0.0245, power=0.8)
power.prop.test(n=NULL, p1=0.0344, p2=0.0287, power=0.8)

## for RR=1.5
power.prop.test(n=NULL, p1=0.0244, p2=0.0163, power=0.8)
power.prop.test(n=NULL, p1=0.0294, p2=0.0196, power=0.8)
power.prop.test(n=NULL, p1=0.0344, p2=0.0229, power=0.8)


## FOR RR= 1.75
power.prop.test(n=NULL, p1=0.0244, p2=0.0139, power=0.8)
power.prop.test(n=NULL, p1=0.0294, p2=0.0168, power=0.8)
power.prop.test(n=NULL, p1=0.0344, p2=0.0197, power=0.8)

##For RR =1.2:
  power.prop.test(n=NULL, p1=0.0244, p2=0.0203, power=0.9)
power.prop.test(n=NULL, p1=0.0294, p2=0.0245, power=0.9)
power.prop.test(n=NULL, p1=0.0344, p2=0.0287, power=0.9)


##For RR =1.5:
  power.prop.test(n=NULL, p1=0.0244, p2=0.0163, power=0.9)
power.prop.test(n=NULL, p1=0.0294, p2=0.0196, power=0.9)
power.prop.test(n=NULL, p1=0.0344, p2=0.0229, power=0.9)


##RR =1.75:
  power.prop.test(n=NULL, p1=0.0244, p2=0.0139, power=0.9)
power.prop.test(n=NULL, p1=0.0294, p2=0.0168, power=0.9)
power.prop.test(n=NULL, p1=0.0344, p2=0.0197, power=0.9)








