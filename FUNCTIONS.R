library(tidyverse)
library(nycflights13)

# VECTOR FUNCTIONS --------------------------------------------------------


df <- tibble(
  a = rnorm(5),
  b =rnorm(5),
  c = rnorm(5),
  d = rnorm(5)
)

rescale <- function(x){
  (x-min(x,na.rm=TRUE))/(max(x,na.rm=TRUE)-min(x,na.rm=TRUE))
}

rescale(c(-10,0,10))
rescale(c(1,2,3,NA,5))

df %>% 
  mutate(a=rescale(a),
         b = rescale(b),
         c = rescale(c),
         d = rescale(d)
         )

df %>% 
  mutate(across(a:d,rescale))

rescale <- function(x){
  rng <- range(x,na.rm = TRUE)
  (x-rng[1] / (rng[2] -rng[1]))
}



# MUTATE FUNCTIONS --------------------------------------------------------

z_score <- function(x){
  (x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)
}

clamp <- function(x,min,max){

case_when(
  x<min ~ min,
  x> max ~ max,
  .default = x
)
}

clamp(1:10,min=3,max=7)


# DATAFRAME FUNCTIONS -----------------------------------------------------

grouped_mean <- function(df,group_var,mean_var){
  df %>% 
    group_by({{group_var}}) %>% 
    summarise(mean({{mean_var}}))
}
diamonds %>% grouped_mean(cut,carat)

## wrap name of the argument in double braces inside the dplyr verb to tell the verb to use 
## the value inside the data frame and not to treat the argument as a variable name

summary_fxn <- function(data,var){
  data %>% 
    summarise(
      min = min({{var}},na.rm=TRUE),
      mean =mean({{var}},na.rm=TRUE),
      median = median({{var}},na.rm=TRUE),
      max = max({{var}},na.rm=TRUE),
      n = n(),
      n_miss = sum(is.na({{var}})),
      .groups = "drop"
    )
}

diamonds %>% summary_fxn(carat)
diamonds %>% 
  group_by(cut) %>% 
  summary_fxn(carat)

diamonds %>% 
  group_by(cut) %>% 
  summary_fxn(log10(carat))

count_prop <- function(df,var,sort=FALSE){
  df %>% 
    count({{var}},sort=sort) %>% 
    mutate(prop = n/sum(n))
}

diamonds %>% count_prop(clarity)

count_wide <- function(data,rows,cols){
  data %>% 
    count(pick(c({{rows}}, {{cols}}))) %>% 
    pivot_wider(
      names_from = {{cols}},
      values_from = n,
      names_sort = TRUE,
      values_fill = 0
    )
}
diamonds %>% count_wide(c(clarity,color),cut)

diamonds %>% 
  ggplot(aes(x=carat))+
  geom_histogram(binwidth=0.1)



diamonds %>% 
  ggplot(aes(x=carat))+
  geom_histogram(binwidth=0.05)

histogram <- function(df,var,binwidth=NULL){
  df %>% 
    ggplot(aes(x = {{var}}))+
    geom_histogram(binwidth = binwidth,fill = "steelblue")
}
diamonds %>% histogram(carat,0.1)


diamonds %>% histogram(carat,0.1)+
  theme_bw()+
  labs(x = "Size (in carats)", y= "Number of diamonds")


linearity_check <- function(df,x,y){
  df %>% 
    ggplot(aes(x = {{x}},y = {{y}}))+
    geom_point()+
    geom_smooth(method = 'loess',formula = y~x,color='red',se = FALSE)+
    geom_smooth(method = "lm",formula = y~ x,color = 'blue',se = FALSE)
}

starwars %>% 
  filter(mass < 1000) %>% 
  linearity_check(mass,height)

hex_plot <- function(df,x,y,z,bins = 20,fun = "mean"){
  df %>% 
    ggplot(aes(x = {{x}},y={{y}},z = {{z}}))+
             stat_summary_hex(
               aes(color= after_scale(fill)), #make border same as color fill
               bins = bins,
               fun = fun,
             )+
    theme_bw()
}

diamonds %>% hex_plot(carat,price,depth)

sorted_bars <- function(df,var){
  df %>% 
    mutate({{var}}:=fct_rev(fct_infreq({{var}}))) %>% 
    ggplot(aes(y={{var}}))+
    geom_bar(fill ="steelblue")+
    theme_bw()
}
diamonds %>% sorted_bars(clarity)


conditional_bars <- function(df,condition,var){
  df %>% 
    filter({{condition}}) %>% 
    ggplot(aes(x={{var}}))+
    geom_bar(fill = "steelblue")+
    theme_bw()
}
diamonds %>% conditional_bars(cut == "Good",clarity)






































































