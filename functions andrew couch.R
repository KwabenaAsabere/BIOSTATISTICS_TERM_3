library(tidyverse)
library(tidytuesdayR)
tuesdata <- tidytuesdayR::tt_load('2020-07-21')

animal_outcomes <- tuesdata$animal_outcomes

animal_complaints <- tuesdata$animal_complaints

brisbane_complaints <- tuesdata$brisbane_complaints

animal_complaints
my_first_function <- function(){
  print("Hello World")
}
my_first_function()

## clean data using a helper/lambda function

animal_complaints <-  animal_complaints |> 
  rename_all(.funs = function(.x){
  .x |> tolower() |> str_replace(
    pattern = " ",replacement = "_"
  )   
  })
animal_complaints

convert_to_decimal <- function(value,total){
  return(value/total)
}
#3 Apply function across columns using Across
animal_outcomes <- animal_outcomes |> 
  mutate(across(ACT:WA, ~convert_to_decimal(
    value=.x ,total=Total
  )))
animal_outcomes

## create a function that is pipeable that does the same thing
convert_area_to_decimal <- 
function(df){
  df |> mutate(across(ACT:WA,~convert_to_decimal(value=.x,total=Total)))
}

## looks cleaner and does the same thing
animal_outcomes |> convert_area_to_decimal()

## another to create the same function

tidy_area_percent <- . %>%  mutate(across(ACT:WA,~convert_to_decimal(value=.x,total=Total)))
animal_outcomes |> tidy_area_percent()


animal_outcomes %>% 
  select(outcome) %>% 
  count(outcome) %>% 
  mutate(outcome=fct_reorder(outcome,n)) %>% 
  ggplot(aes(x=outcome,y=n))+
  geom_col()+
  coord_flip()
 
## Tidy Eval
create_bar_chart <- function(column_var,df){
  column_var <- enquo(column_var)
  df  %>%
    select(!!column_var) %>% #must add bang-bang
  count(!!column_var) %>% 
    mutate(outcome := fct_reorder(!!column_var,n)) %>% ##must use != instaed of = when assigning 
    ggplot(aes(x=!!column_var,y=n,fill = !!column_var))+
    geom_col()+
    coord_flip()+
    theme_bw()+
    theme(legend.position = "none")
}

create_bar_chart(outcome,animal_outcomes)

create_bar_chart(animal_type,animal_outcomes)



animal_outcomes %>% 
  count(outcome) %>% 
  mutate(outcome=fct_reorder(outcome,n)) %>% 
  ggplot(aes(x=outcome,y=n))+
  geom_col()+
  coord_flip()

conflicts()

brisbane_complaints %>% 
  filter(suburb=="SUNNYBANK",
         animal_type == "Attack") %>% 
  count(category) %>%
  drop_na() %>% 
  mutate(category=fct_reorder(category,n)) %>% 
  ggplot(aes(x=category,y=n,fill = category))+
  geom_col()+
  coord_flip()+
  theme(legend.position = "none")

##make a function that creates a chart for each suburb and saves it as a pdf

save_charts <- function(df,filename){
  temp_chart <- df %>% 
  mutate(category=fct_reorder(category,n)) %>% 
    ggplot(aes(x=category,y=n,fill = category))+
    geom_col()+
    coord_flip()+
    theme(legend.position = "none")+
    ggtitle(paste0(filename,"Attacks"))
  ggsave(filename=paste0(filename,".pdf"),plot=temp_chart,width= 11,height=8.5,units ="in"  )
}

library(magrittr)
brisbane_complaints %>% 
  filter(animal_type == "Attack") %>% 
  count(suburb,category) %>% 
  drop_na() %>% 
  nest(-suburb) %>% 
  mutate(suburb=str_replace(suburb," ","_")) %$% 
  walk2(data,suburb,save_charts)





