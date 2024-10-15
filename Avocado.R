#Rachael Lee
rm(list = ls())
library(readr)
library(tidyverse)
library(tibble)
library(gt)

avocado <- read_csv("C:/Users/relee/OneDrive/Desktop/MSBAData/avocado2020.csv")

#Q2.1
glimpse(avocado)

#Q2.2
regional_avocado <- filter(avocado, geography == "West" | geography == "Northeast" | geography == "Great Lakes")
unique(regional_avocado$geography)
count(regional_avocado)

#Q2.3
regional_avocado %>% 
  group_by(geography, type) %>% 
  summarize(mean(average_price))

#Q2.4
regional_avocado %>%
  group_by(year, type, geography) %>%
  filter(type == "organic") %>%
  mutate(value = average_price * total_volume) %>%
  summarize(n(), mean(value), sd(value))

#Q3
z <- tibble(
  x = c(1, 2, 3, 4, 5), 
  y = c(4.2342, 10.3444, 3.0093, 6.367, 20.659)
)

z %>% gt %>%
  tab_header(title = md("**The z Variable**"),
             subtitle = "A mystery...") %>%
  fmt_number(y, 2) %>% 
  tab_footnote(md("_source: Unknown_"),
               cells_title(groups = "title")) %>%
  tab_style(cell_fill(color = "#B2D3C2"), cells_body(rows = y == min(y)))
  
