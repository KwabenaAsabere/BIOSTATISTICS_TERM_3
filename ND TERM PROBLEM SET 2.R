library(tidyverse)
library(broom)
library(finalfit)
library(GGally)
library(survey)
nepalA <- read_csv("nepal_anthro.csv")

nepalData <- nepalA |> 
  filter(age<=12) |> 
  na.omit() 

nepalData <- nepalA |> 
  filter(age<=12) |> 
  filter(!is.na(height),!is.na(weight),!is.na(armcirc))

nepalData <- nepalData |> 
  mutate(sex=as.factor(sex)) |> 
  mutate(gender=fct_recode(sex,
                          "Male"="1",
                          "Female"="2"))


nepalData |> 
  ggplot(aes(x=age,y=weight,color=gender)) +
  geom_jitter()+
  theme_bw()+
  labs(x="Age In Months",y="Weight In Kg")

## ANCOVA Model
model1 <- nepalData |> 
  lm(weight~age+gender,data=_)
summary(model1)
confint(model1)
model1_aug <- augment(model1)

model2 <- nepalData |> 
  lm(weight~age*gender,data=_)
summary(model2)
confint(model2)
model2_aug <- augment(model2)


model2_aug |> 
  ggplot(aes(x=age,y=weight))+
  geom_jitter(aes(color=gender,shape=gender))+
  geom_line(aes(x=age,y=.fitted,color=gender))+
  theme_bw()

## a linear spline rerm with break at 4 month and it's interaction with gender
nepalData <- nepalData |> 
  mutate(agesp=if_else(age>4,age-4,0))

model3 <- nepalData |> 
  lm(weight ~ age*gender + agesp*gender,data=_)
summary(model3)
confint(model3)
model3_aug <- augment(model3)

model3_aug |> 
  ggplot(aes(x=age,y=weight))+
  geom_jitter(aes(color=gender,shape=gender))+
  geom_line(aes(x=age,y=.fitted,color=gender))+
  theme_bw()+
  labs(x="Age In Months",y="Weight In Kg")

##anova tests whether the coefficients are different between the two models
anova(model2,model3)

## alternative way of testing whether the additional terms(agespn and it's interaction term)
## are needed in model 3 using regTermTest from the survey package

model3 |> 
  regTermTest(~ agesp + agesp:gender)

#the F-test of the null hypothesis that both interaction terms are
# zero is the following

model3 |> 
  regTermTest(~ age:gender + agesp:gender)





# # USING  surveycontrast() to combine coefficients -----------------------

















