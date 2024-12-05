library(tidyverse)
classData <- read_csv("class12Data.csv")
CT = table(classData$HealthGen, classData$Smoke100)
chisq.test(CT)

classData = classData %>% 
  mutate(cpoor = ifelse(HealthGen <=1, 1, 0)) %>%
  mutate(cfair = ifelse(HealthGen <=2, 1, 0)) %>% 
  mutate(cgood = ifelse(HealthGen <=3, 1, 0)) %>%
  mutate(cvgood = ifelse(HealthGen <=4, 1, 0)) %>% 
  mutate(cexc = ifelse(HealthGen <=5, 1, 0))

classData %>% 
  select(HealthGen, cpoor, cfair, cgood, cvgood, cexc) %>%
  head(20)

model1 = glm(cpoor ~ Smoke100, data=classData, family=binomial(link="logit"))
summary(model1)

model2 = glm(cfair ~ Smoke100, data=classData, family=binomial(link="logit"))
summary(model2)

model3 = glm(cgood ~ Smoke100, data=classData, family=binomial(link="logit"))
summary(model3)


model4 = glm(cvgood ~ Smoke100, data=classData, family=binomial(link="logit")) 
summary(model4)


model5 = glm(cexc ~ Smoke100, data=classData, family=binomial(link="logit"))

library(MASS) # for polr function select <- dplyr::select # define the select function to be from tidyverse 
model6 = polr(as.factor(HealthGen) ~ Smoke100, data = classData, Hess=TRUE, method="logistic") 
summary(model6)

exp(model6$coefficients)
exp(confint.default(model6))

## to check the proportional odds model
library(brant)

brant_result <- brant(model6)
summary(brant_result)



# MULTIVARIABLE POM -------------------------------------------------------

model7 = polr(as.factor(HealthGen) ~ Age + Gender + Smoke100 + Diabetes + PhysActive, data = classData, Hess=TRUE, method="logistic")
summary(model7)
exp(model7$coefficients)
exp(confint.default(model7))

# MULTINOMIAL REGRESSION --------------------------------------------------
library(nnet)
model8 = multinom(as.factor(HealthGen) ~ Smoke100, data = classData)
summary(model8)
confint(model8)
exp(coefficients(model8))

classData = classData %>% 
  mutate(HealthGen = relevel(as.factor(HealthGen), ref="3")) 
model9 <- multinom(HealthGen ~ Smoke100, data = classData)
summary(model9)
confint(model9)
exp(coefficients(model9))





