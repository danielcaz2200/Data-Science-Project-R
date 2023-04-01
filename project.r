library(tidyverse)
library(ggplot2)
library(modelr)

covid_data <- read_csv("owid-covid-data.csv")
demographics <- read_csv("demographics.csv")

demographics <- demographics %>% filter(nchar(`Country Code`) == 3)

pop_filter <- demographics %>% 
  filter(`Series Code` == "SP.POP.TOTL" & YR2015 >= 1000000) %>% 
  select(`Country Name`)

demographics <- demographics %>% 
  filter(`Country Name` %in% pop_filter$`Country Name`)

demographics <- demographics %>% select(`Country Name`, `Country Code`, `Series Code`, YR2015)

covid_data <- covid_data %>%
  select(iso_code, location, date, new_cases, new_cases_smoothed, new_deaths_smoothed, total_vaccinations)

covid_data <- covid_data %>% 
  mutate(new_deaths_smoothed_2wk = lead(new_deaths_smoothed, n=14))

demographics <- demographics %>% pivot_wider(names_from = `Series Code`, values_from = YR2015) 

covid_data %>% inner_join(demographics, by=c(iso_code = "Country Code", location = "Country Name"))
