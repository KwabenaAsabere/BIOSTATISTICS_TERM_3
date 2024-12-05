library(tidyverse)
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

heart <- read_csv("heart.csv")
heart

heart %>% summary_fxn(stop)
nepal <- read_csv("nepal_anthro.csv")

nepal %>% group_by(sex) %>% 
  summary_fxn(height)
                      