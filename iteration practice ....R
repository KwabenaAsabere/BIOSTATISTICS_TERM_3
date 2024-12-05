library(tidyverse)

map(1:10,log)

map_dbl(1:10,exp) 

map_chr(mtcars,typeof)

map_lgl(mtcars,is.double)

n_unique <- function(x){
  length(unique(x))
}

map_int(mtcars,n_unique)

map_dbl(mtcars,mean)
map_dbl(mtcars,function(x) length(unique(x)))

map_dbl(mtcars,~length(unique(.x)))

x <- map(1:10,~runif(5))
x
str(x)


x <- list(
  list(-1, x = 1, y = c(2), z = "a"),
  list(-2, x = 4, y = c(5, 6), z = "b"),
  list(-3, x = 8, y = c(9, 10, 11))
)

map_dbl(x,"x")

map_dbl(x,1)



x <- list(1:5,c(1:10,NA))


map_dbl(x,~mean(.x,na.rm=TRUE))


by_cyl <- split(mtcars,mtcars$cyl)

by_cyl

by_cyl %>% 
  map(~lm(mpg ~ wt, data = .x)) %>% 
  map(coef)  %>% 
  map_dbl(2)


df <- data.frame(
  x=1:3,
  y=6:4
)
df
map(df,~.x*2)

modify(df, ~.x*2)


x <- 8

if (x >=10){
  print(" x is greater than or equal to 10")
  
}else if(x>5){
  print("x is greater than five but less than 10")
}

else{
  print(" x is less than 10")
}


x <- 4==3

map(df,~.x*2)


df_xy <- data.frame(x= sample(1:20,10,TRUE),
                    y = rnorm(10))
df_xy


map(df_xy,~log(.x))

map(df_xy,~exp(.x))

map_dbl(df_xy, exp)





































































