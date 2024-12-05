library(tidyverse)
set.seed(123)

df_xyz <- tibble(
  x = rnorm(10),
  y = rnorm(10),
  z = rnorm(10)
) %>% 
  print()

df_xyz %>% 
  summarise(
    across(
      .cols = everything(),
      .fns = mean,
      .names = "{col}_mean"
    )
  )


xyz_means <- map_dbl(
  .x = df_xyz,
  .f = mean
)

xyz_means

library(readxl)
getwd()

path <- "C:/Users/KAsab/Desktop/BIOSTATS TERM 3/city_ses.xlsx"

## using walk to read excel sheets
##map returns only one output whereas walk can return multiple outputs
walk(
  .x = excel_sheets(path),
  .f = function(x){
    new_nm <- tolower(x)
    assign(new_nm,read_excel(path,sheet = x),envir = .GlobalEnv)
  }
)

## a more concise form using purr_style lambda syntax
walk(
  .x =excel_sheets(path),
  .f = ~assign(tolower(.),read_excel(path,sheet = .),envir = .GlobalEnv)
)

atlanta
houston
charlotte




excel_sheets(path)
## using  map_dfr to read excel sheets
cities <- map_dfr(
  .x = excel_sheets(path),
  .f = ~read_excel(path,sheet = .x)
)
cities


# USING purr FOR DATA MANAGEMENT ------------------------------------------

## adding NAs to our dataframe
df_xyz$x[2] <- NA_real_
df_xyz$y[4] <- NA_real_
df_xyz$z[6] <- NA_real_

df_xyz

## using a function to add NAs
add_na_at <- function(vect,pos){
 vect[[pos]] <- NA
 vect
}

add_na_at(df_xyz$x,2) 

##data frame columns are vectors, so we can use our new function inside of mutate 
##to add NA values to each column in our data frame at a position of our choosing

df_xyz %>% 
  mutate( 
    x = add_na_at(x,2),
    y = add_na_at(y,4),
    z = add_na_at(z,6)
    
  )

## using purr 

map(
  .x = df_xyz,
  .f = add_na_at,2
)


##alternatively, we can use the purrr-style lambda to pass our function to .f

map(
  .x = df_xyz,
  .f = ~add_na_at(.x,2)
)
# output of map() is always a list
## we can convert to dataframe  by passing it to the as.data.frame() function:

map(
  .x = df_xyz,
  .f = ~add_na_at(.x,2)
) %>% 
  as.data.frame()

##Alternatively, we could just use map_dfc as a shortcut instead

map_dfc(
  .x = df_xyz,
  .f = ~add_na_at(.x,2)
  
)

## Why map_dfc instead of map_dfr? Because we want to combine x, y, and z together
## as columns, not as rows

## map2() variants of the map() function allows us to iterate over two objects 
## instead of just one as allowed by map()

map2_dfc(
  .x = df_xyz,
  .y = c(2,4,6),
  .f = ~add_na_at(.x,.y)
)

set.seed(8142020)

map2_dfc(
  .x = df_xyz,
  .y = sample(1:10,3,TRUE), ##generates 3 random numbers between 1-10 with replacement
  .f = ~add_na_at(.x,.y)
)         


## alternatively

map2_dfc(
  .x = df_xyz,
  .y = sample(1:10,3,TRUE),
  .f = function(vect,pos){
    vect[[pos]] <- NA
    vect
  }
)

### alternatively using purr lambda function style

map2_dfc(
  .x = df_xyz,
  .y = sample(1:10,3,TRUE),
  .f = ~ {
      .x[[.y]] <- NA
      .x
  }
)



# USING purr FOR DATA ANALYSIS --------------------------------------------

study <- tibble(
  age       = c(32, 30, 32, 29, 24, 38, 25, 24, 48, 29, 22, 29, 24, 28, 24, 25, 
                25, 22, 25, 24, 25, 24, 23, 24, 31, 24, 29, 24, 22, 23, 26, 23, 
                24, 25, 24, 33, 27, 25, 26, 26, 26, 26, 26, 27, 24, 43, 25, 24, 
                27, 28, 29, 24, 26, 28, 25, 24, 26, 24, 26, 31, 24, 26, 31, 34, 
                26, 25, 27, NA),
  age_group = c(2, 2, 2, 1, 1, 2, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 
                1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 2, 
                2, 1, 1, 1, NA),
  gender    = c(2, 1, 1, 2, 1, 1, 1, 2, 2, 2, 1, 1, 2, 1, 1, 1, 1, 2, 2, 1, 1, 
                1, 1, 2, 1, 1, 2, 1, 1, 1, 2, 1, 1, 2, 2, 1, 2, 2, 1, 2, 2, 1, 
                1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 2, 2, 1, 1, 2, 1, 2, 1, 
                1, 1, 2, 1, NA),
  ht_in     = c(70, 63, 62, 67, 67, 58, 64, 69, 65, 68, 63, 68, 69, 66, 67, 65, 
                64, 75, 67, 63, 60, 67, 64, 73, 62, 69, 67, 62, 68, 66, 66, 62, 
                64, 68, NA, 68, 70, 68, 68, 66, 71, 61, 62, 64, 64, 63, 67, 66, 
                69, 76, NA, 63, 64, 65, 65, 71, 66, 65, 65, 71, 64, 71, 60, 62, 
                61, 69, 66, NA),
  wt_lbs    = c(216, 106, 145, 195, 143, 125, 138, 140, 158, 167, 145, 297, 146, 
                125, 111, 125, 130, 182, 170, 121, 98, 150, 132, 250, 137, 124, 
                186, 148, 134, 155, 122, 142, 110, 132, 188, 176, 188, 166, 136, 
                147, 178, 125, 102, 140, 139, 60, 147, 147, 141, 232, 186, 212, 
                110, 110, 115, 154, 140, 150, 130, NA, 171, 156, 92, 122, 102, 
                163, 141, NA),
  bmi       = c(30.99, 18.78, 26.52, 30.54, 22.39, 26.12, 23.69, 20.67, 26.29, 
                25.39, 25.68, 45.15, 21.56, 20.17, 17.38, 20.8, 22.31, 22.75, 
                26.62, 21.43, 19.14, 23.49, 22.66, 32.98, 25.05, 18.31, 29.13, 
                27.07, 20.37, 25.01, 19.69, 25.97, 18.88, 20.07, NA, 26.76, 
                26.97, 25.24, 20.68, 23.72, 24.82, 23.62, 18.65, 24.03, 23.86, 
                10.63, 23.02, 23.72, 20.82, 28.24, NA, 37.55, 18.88, 18.3, 
                19.13, 21.48, 22.59, 24.96, 21.63, NA, 29.35, 21.76, 17.97, 
                22.31, 19.27, 24.07, 22.76, NA),
  bmi_3cat  = c(3, 1, 2, 3, 1, 2, 1, 1, 2, 2, 2, 3, 1, 1, 1, 1, 1, 1, 2, 1, 1, 
                1, 1, 3, 2, 1, 2, 2, 1, 2, 1, 2, 1, 1, NA, 2, 2, 2, 1, 1, 1, 1, 
                1, 1, 1, 1, 1, 1, 1, 2, NA, 3, 1, 1, 1, 1, 1, 1, 1, NA, 2, 1, 
                1, 1, 1, 1, 1, NA)
) %>% 
  mutate(
    age_group = factor(age_group, labels = c("Younger than 30", "30 and Older")),
    gender    = factor(gender, labels = c("Female", "Male")),
    bmi_3cat  = factor(bmi_3cat, labels = c("Normal", "Overweight", "Obese"))
  ) %>% 
  print()



# Continuous statistics ---------------------------------------------------


continuous_statisitcs <- function(var){
  study %>% 
    summarise(
      varaible = quo_name(var),## add variable name to the output
      n_miss = sum(is.na({{var}})),
      mean = mean({{var}},na.rm = TRUE),
      median = median({{var}},na.rm = TRUE),
      min = min({{var}},na.rm = TRUE),
      max = max({{var}},na.rm = TRUE),
      sd = sd({{var}},na.rm = TRUE)
      
    )
}
 
  continuous_statisitcs(age)
  
  
  map_dfr(
    .x = quos(age,ht_in,wt_lbs,bmi),##If we didn’t want to type our column names in quotes
    .f = continuous_statisitcs
  )

  
  
  
  ## to generalize the function
  
  
  continuous_stats <- function(df,var){
    df %>% 
      summarise(
        variable = quo_name(var),## add variable name to the output
        n_miss = sum(is.na({{var}})),
        mean = mean({{var}},na.rm = TRUE),
        median = median({{var}},na.rm = TRUE),
        min = min({{var}},na.rm = TRUE),
        max = max({{var}},na.rm = TRUE),
        sd = sd({{var}},na.rm = TRUE)
        
      )
  }
  
 
   map_dfr(
    .x = quos(age,ht_in,wt_lbs,bmi),
    .f = continuous_stats, df = study
  )

  
   map_dfr(
     .x = quos(x,y,z),
     .f = continuous_stats, df = df_xyz
   )

 
   summary_stats <- study %>% 
     summarise(
       across(
         .cols  = c(age, ht_in, wt_lbs, bmi),
         .fns   = list(
           n_miss = ~ sum(is.na(.x)),
           mean   = ~ mean(.x, na.rm = TRUE),
           median = ~ median(.x, na.rm = TRUE),
           min    = ~ min(.x, na.rm = TRUE),
           max    = ~ max(.x, na.rm = TRUE)
         ),
         .names = "{col}-{fn}" 
       ) 
     ) %>% 
     print() 
   
   summary_stats %>% 
     tidyr::pivot_longer(
       cols      = everything(),
       names_to  = c("characteristic", ".value"),
       names_sep = "-"
     ) 
 
   
   
# CATEGORICAL STATISTICS --------------------------------------------------

   
     cat_stats <- function(df,var) {
     df %>% 
       count({{ var }}) %>% 
       mutate(variable = names(.)[1]) %>% ## . is the placeholder for the name of the df
       rename(category = 1) %>% 
       select(variable, category, n)
   }
   
   map_dfr(
     .x = quos(age_group, gender, bmi_3cat),
     .f =cat_stats,df = study
   ) 

 
   
   

# across() with filter() --------------------------------------------------
   
   ## across is no longer to be used with filter
   ## rather we should use if_any() and if_all()
   
 ##if_any() will keep the rows where any value of x, y, or z are TRUE
  ## On the other hand, if_all() will the keep the rows where
   ##all value of x, y, and z are TRUE
   
   df_xyz %>% 
     filter(
       if_all(
         .cols = x:z,
         .fns = ~!is.na(.x)
       )
     )

   
   
   
   
      
   
   
   
   
   
   
      




















