library(tidyverse)
library(survival)
library(ggsurvfit)
library(broom)
library(finalfit)

library(gt)

library(palmerpenguins)
penguins <- penguins

penguins |> 
  gt() |> 
  tab_header(
    title= "The Penguins Dataset"
  )

## if you want to format a text in Markdown text u wrap it in md()
#3 and put tht word in ` ` or * *
penguins |> 
  gt() |> 
  tab_header(
    title= md("The `penguins` Dataset")
  )


penguins |> 
  gt() |> 
  tab_header(
    title= md("The Penguins Dataset"),
    subtitle=md("**Three** *years of data on Island*")
  ) |> 
  opt_align_table_header(align="left")

## summaries


penguins |>
  group_by(species) |> 
  summarise_at(
    .vars=c("bill_length_mm",
            "bill_depth_mm",
            "flipper_length_mm",
            "body_mass_g"),
    .funs = ~ mean(. ,na.rm = TRUE)
  )
  
  
  
  
penguins |>
  group_by(species) |> 
  summarise_at(
    .vars=c("bill_length_mm",
            "bill_depth_mm",
            "flipper_length_mm",
            "body_mass_g"),
    .funs = ~ mean(. ,na.rm = TRUE)
  ) |> 
  gt(rowname_col = "species") |> 
  tab_header(
    title= md(" Summary of The Penguins Dataset"),
    subtitle=md("**Three** *years of data on Island*")
  ) |> 
  cols_label(
    bill_length_mm=md("Bill Length,<br>mm"),
    bill_depth_mm=md("Bill Depth,<br>mm"),
    flipper_length_mm=md("Flipper Length,<br>mm"),
    body_mass_g=md("Body Mass,<br>kg")
  ) |> 
  opt_align_table_header(align="left") |> 
  fmt_number(columns = everything()) |>  ##reduces decimals to 2 places
fmt_number(columns = body_mass_g,scale_by = 1/1000) |> 
  cols_width(
    bill_length_mm~px(120),
    bill_depth_mm~px(120),
    flipper_length_mm~px(120),
    body_mass_g~px(120),
    everything()~px(100)
  ) |> 
  tab_source_note(source_note =md("Data is from the `palmerpenguins` **R** package")) |> 
  tab_footnote(
    footnote = "Gentoo is the largest of the three penguins studied.",
    locations = cells_stub(rows= "Gentoo")
  )
























