library(tidyverse)
library(broom)
library(finalfit)
library(GGally)


ce621 <-  read_csv("ce621.csv")
ce621 <-  ce621 %>% # start with the original data, the create subgroups
  mutate(agegen=case_when(sex=="Male" & age<=60 ~ "m <=60",
                          sex=="Female" & age<=60 ~ "f <=60",
                          sex=="Male" & age > 60 ~ "m >60",
                          sex=="Female" & age > 60 ~ "f >60"))

## Inspect the data using side-by-side boxplots
ce621 |> 
  ggplot(aes(x=agegen,y=totchg,fill=agegen))+
  geom_boxplot()+
  theme_bw()+
  labs(y="Total Charges In Dollars",x="Agege & Gender Group")

## SUMMARY STATS OF THE AGEGEN GROUPS
ce621 |> 
  group_by(agegen) |> 
  summarize(
    obs=n(),
    mean=mean(totchg),
    median=median(totchg),
    sd=sd(totchg),
    min= min(totchg),
    max=max(totchg)
  )

model1 <- ce621 |> 
  lm(totchg ~ as.factor(agegen),data=_)
anova(model1)
summary(model1)
tidy(model1)
model1_aug <- augment(model1)
model1_aug

##boxplot of residuals
model1_aug |> 
  ggplot(aes(x=`as.factor(agegen)`,y=.resid,fill=`as.factor(agegen)`))+
  geom_boxplot()+
  theme_bw()+
  labs(x="Age Gender Groups",y="Residuals")

## plot of fitted vs resiaul values
model1_aug |> 
  ggplot(aes(x=.fitted,y=.resid))+
  geom_point()+
  theme_bw()+
  labs(x="Predicted Values",y="Residuals")

## jitterplot of predicted vs residuals
model1_aug |> 
  ggplot(aes(x=.fitted,y=.resid))+
  geom_jitter(color="navyblue")+
  theme_bw()+
  labs(x="Predicted Values",y="Residuals")


## log transformation
ce621 <- ce621 |> 
  mutate(logtotchg=log10(totchg))

ce621 |> 
  ggplot(aes(x=agegen,y=logtotchg,fill=agegen))+
  geom_boxplot()+
  theme_bw()+
  labs(y="Total Charges In Dollars",x="Agege & Gender Group")



























