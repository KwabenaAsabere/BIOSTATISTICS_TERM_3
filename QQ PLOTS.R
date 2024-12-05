library(tidyverse)
norms <- rnorm(200)
qplot(sample=norms)

df <- data.frame(norms)
df |> 
  ggplot(aes(sample=norms))+
  geom_qq()+
  geom_qq_line()


## to quickly see a histogram of the data
qplot(x=norms)

df$group <-factor(sample(1:3,200,replace=TRUE))

df |> 
  ggplot(aes(sample=norms,color=group))+
  geom_qq()+
  geom_qq_line()+
  facet_wrap(~group)+
  theme_minimal()
