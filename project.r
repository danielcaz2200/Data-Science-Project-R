library(tidyverse)
library(ggplot2)
library(modelr)

# Read in both datasets
covid_data <- read_csv("owid-covid-data.csv")
demographics <- read_csv("demographics.csv")

# Keep only the rows where country_code is 3 letters
demographics <- demographics %>% filter(nchar(`Country Code`) == 3)

# Remove the countries whose total population is less than 1 million
pop_filter <- demographics %>% 
  filter(`Series Code` == "SP.POP.TOTL" & YR2015 >= 1000000) %>% 
  select(`Country Name`)

demographics <- demographics %>% 
  filter(`Country Name` %in% pop_filter$`Country Name`)

# Select all columns BUT Series Name
demographics <- demographics %>% select(-`Series Name`)

# Remove columns that should not be used for linear modeling // death and excess mortality cols
covid_data <- covid_data %>% select(-c(total_deaths, new_deaths, total_deaths_per_million, new_deaths_per_million, new_deaths_smoothed_per_million))
covid_data <- covid_data %>% select(-c(excess_mortality_cumulative_absolute, excess_mortality_cumulative, excess_mortality, excess_mortality_cumulative_per_million))

covid_data %>% View()
# Add new column new_deaths_smoothed_2wk that has the same values as 
# new_deaths_smoothed but two weeks ahead
covid_data <- covid_data %>%
  mutate(new_deaths_smoothed_2wk = lead(new_deaths_smoothed, n=14))

# Tidy demographic table as needed
demographics <- demographics %>% pivot_wider(names_from = `Series Code`, values_from = YR2015)

covid_data %>% inner_join(demographics, by=c(iso_code = "Country Code", location = "Country Name"))
