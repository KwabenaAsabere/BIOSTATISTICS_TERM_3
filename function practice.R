library(tidyverse)


df <- tibble(
  a = rnorm(5),
  b =rnorm(5),
  c = rnorm(5),
  d = rnorm(5)
)


z_score <- function(x){
  (x-mean(x,na.rm = TRUE))/sd(x,na.rm = TRUE)
}



df %>% 
  mutate(across(a:d, z_score))

df %>% summarise(across(a:d,list(mean = mean,sd = sd),na.rm = TRUE))

 nepal <- read_csv('nepal_anthro.csv')

 group_mean <- function(df,group_var,mean_var){
   df %>% 
     group_by({{group_var}}) %>% 
    summarise(mean({{mean_var}},na.rm=TRUE))
 }

 
 nepal %>% group_mean(sex,armcirc)

 plot_nepal <- function(df,x,y){
   df %>% 
     ggplot(aes(x={{x}}, y = {{y}}))+
     geom_point(color = 'steelblue',alpha = 0.8)+
     geom_smooth(method = "lm",color = "red",se = FALSE)+
     theme_bw()
 }
nepal %>% plot_nepal(weight,armcirc)









