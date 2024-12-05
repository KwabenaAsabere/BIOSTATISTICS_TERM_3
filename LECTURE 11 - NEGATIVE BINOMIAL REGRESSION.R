library(tidyverse)
library(broom)
class <- read_csv("class11concussions.csv")
class <- class |> 
  mutate(rate=conc/gameexp)
names(class)

model1 <- class |> 
  glm(conc~female+sport,family=poisson(link=log),offset=log(gameexp),data=_)
summary(model1)
exp(model1$coefficients)
exp(confint.default(model1))

class |> 
  group_by(female) |> 
  summarise(mean=mean(conc,na.rm=T),
            SD=sd(conc,na.rm=T),
            VAR=var(conc,na.rm=T))

model2 <- class |> 
  glm(conc~female,family=poisson(link=log),offset=log(gameexp),data=_)
summary(model2)

str(class)

class <- class |> 
  mutate(sport=as.factor(sport)) |> 
  mutate(sport=fct_relevel(sport,"Soccer"))

levels(class$sport)

# NEGATIVE BINOMIAL REGRESSION --------------------------------------------



library(MASS)  ## needed for glm.nb model
model3 <- class |> 
  glm.nb(conc ~  female + sport + offset(log(gameexp)),data=_)
summary(model3)



exp(model3$coefficients)
exp(confint.default(model3))

LRTstat= 2*(logLik(model3)-logLik(model1))
df=1
pval <- 1-pchisq(LRTstat,df)
pval
