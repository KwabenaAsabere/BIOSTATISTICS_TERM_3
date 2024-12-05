library(tidyverse)
library(gcookbook)
cabbage_exp
cabbage_exp |> 
  ggplot(aes(x=interaction(Date,Cultivar),y=Weight))+
  geom_col()+
  geom_text(aes(label=Weight),vjust=1.5,color="white")


## text above top
cabbage_exp |> 
  ggplot(aes(x=interaction(Date,Cultivar),y=Weight))+
  geom_col()+
  geom_text(aes(label=Weight),vjust=-0.2)



mtcars |> 
  ggplot(aes(x=factor(cyl)))+
  geom_bar()+
  geom_text(aes(label = ..count..),stat = "count",vjust=1.5,colour = "white")
