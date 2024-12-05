library(tidyverse)
library(broom)
iris %>% 
  mutate(map_sqrt = map_dbl(Sepal.Length,sqrt)) %>% 
  unnest()



iris %>% 
  nest(-species) %>% 
  mutate(model = map(data,.f=function(x){
    
  }))




iris %>% 
  nest(-Species) %>% 
  mutate(model = map(data,~lm(Sepal.Length~Sepal.Width+Petal.Length+Petal.Width,data=.x)),
         tidy_model = map(model,tidy)) %>% 
  select(Species,tidy_model) %>% 
  unnest(cols = c(tidy_model)) %>% 
  ggplot(aes(x=term,y=estimate,color=Species))+
  geom_point(size=3)+
  theme_bw()


my_fn <- function(x,y){
  
}







  
