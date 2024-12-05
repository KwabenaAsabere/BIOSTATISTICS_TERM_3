library(tidyverse)
starwars <- starwars
starwars
glimpse(starwars)

starwars %>% 
  summarise(height_mean= mean(height,na.rm=TRUE),
            mass_mean = mean(mass,na.rm = TRUE))



# USING across() ----------------------------------------------------------


starwars %>% 
  summarise(across(height:mass,mean,na.rm=TRUE))

starwars %>% 
  summarise(across(.cols=height:mass,.fns=mean,na.rm=TRUE))


## using where() with across()

starwars %>% 
  summarise(across(where(is.numeric),mean,na.rm=TRUE))

## more than one function

starwars %>% 
  summarise(across(where(is.numeric),list(min = min,max = max, mean = mean),
                   na.rm=TRUE))

## find number of unique values in character variables

skimr::skim(starwars)



starwars %>% 
  summarise(unique(hair_color))

starwars %>% 
  reframe(unique(hair_color))


## function syntax
starwars %>% 
  summarise(across(where(is.character), ~length(unique(.x))))

starwars %>% 
  summarise(across(where(is.numeric),~min(.x,na.rm=TRUE)))

starwars %>% 
  reframe(across(where(is.numeric),~min(.x,na.rm=TRUE)))



starwars %>% 
  group_by(species) %>% 
  filter(n()>1) %>% 
  summarise(across(c(sex,gender,homeworld),
            ~length(unique(.x))),
            n=n())



 starwars %>% 
    
  filter(n()>1) %>% 
  reframe(across(c(sex,gender,homeworld),
                   ~length(unique(.x))),
            n=n(),
          .by = species)
          






## controlling names
starwars %>% 
  summarise(across( where(is.numeric),
                   list(min = min,max = max, mean = mean,sd = sd),
                   na.rm=TRUE,
                   .names = "{.fn}_{col}"))

starwars %>% 
  summarise(across( where(is.numeric),
                    list(min,max, mean),
                    na.rm=TRUE,
                    .names = "{.fn}_{col}"))


df <- data.frame(x = c(1,2,3), y= c(1,4,9))
df %>% 
  summarise( across(where(is.numeric),sd),
             n=n())

## show me the rows with no missing values

starwars %>% 
  filter(across(everything(),~!is.na(.x)))


starwars %>% 
  distinct(across(contains("color"))) %>% 
  arrange(hair_color,skin_color)


starwars %>% 
  count(across(contains('color')))

## calculate the median for each numeric variable for each species and gender

starwars %>% 
  group_by(species,gender) %>% 
  summarise(across(where(is.numeric),~median(.x,na.rm=TRUE)))




# CASE WHEN ---------------------------------------------------------------

x <- 1:16
case_when(
  x< 5 ~ "less than 5",
  x<10 ~ "less than 10",
  TRUE ~ as.character(x)  ## otherwise the rest of the numbers would be returned as NAs 
                          ## since characters and numbers can't be put together
)


## modular arithmetic
## fizz =3, buzz = 5,fuzzbuzz = 3 & 5

case_when(
  x %% 15 == 0 ~ "fizzbuzz",
  x %% 5 == 0 ~ "buzz",
  x %% 3 ==0 ~ "fizz",
  TRUE ~ as.character(x)
)


starwars %>% 
  select(species,gender,height) %>% 
  mutate(height_cat = case_when (height > quantile(height,3/4,na.rm = TRUE) ~ "Tall",
                                 height < quantile(height,1/4,na.rm = TRUE) ~ " Short",
                                 TRUE ~ "Average"))



starwars %>% 
  arrange(-height) %>% 
  select(name:mass,gender,species) %>% 
  mutate(type = case_when(
    height > 200 | mass > 100 ~ 'Large',
    species == "droid" ~ "Robot" ,
    TRUE ~ "Other"
  ))



# rownames_to_columns -----------------------------------------------------

state <- state.x77
state %>% 
  as.data.frame() %>% 
  rownames_to_column("state") %>% 
  as_tibble()

# distinct() --------------------------------------------------------------


starwars %>% 
  distinct(homeworld,species)

starwars %>% 
  count(homeworld,species)

library(gapminder)
data("gapminder")

gapminder %>% 
  distinct(country,continent) %>% 
  count(continent)





# rowwise() ---------------------------------------------------------------

fruits <- tribble(
  ~"fruit",~"height_1",~'height_2',~"height_3",~"width",~"weight",
  "Banana",4,4.2,3.5,1,0.5,
  "Strawberry",1,0.9,1.2,1,.25,
  "Pineaple",18,17.7,19.2,6,3
)
fruits

fruits %>% 
    mutate(height_mean = mean(c(height_1,height_2,height_3)))


fruits %>% 
  rowwise(fruit) %>% 
  mutate(height_mean = mean(height_1:height_3))


### pick() has been introduced as a substitute to across()
##
























