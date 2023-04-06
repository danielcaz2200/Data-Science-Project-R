library(tidyverse)
library(ggplot2)
library(modelr)

# ------------------------------
# PROJECT PART 1: DATA WRAGLING
# ------------------------------

# Read in both datasets
covid_data <- read_csv("owid-covid-data.csv")
demographics <- read_csv("demographics.csv")

# Keep only the rows where country_code is 3 letters
# demographics <- demographics %>% filter(nchar(`Country Code`) == 3)
covid_data <- covid_data %>%
  filter(nchar(iso_code) == 3, population >= 1000000)

# Select all columns BUT Series Name, we don't need this column
demographics <- demographics %>% select(-`Series Name`)

# Remove columns that should not be used for linear modeling
# In our case, cols dealting with death and excess mortality
covid_data <- covid_data %>%
  select(-c(total_deaths, new_deaths, total_deaths_per_million, new_deaths_per_million, new_deaths_smoothed_per_million))
covid_data <- covid_data %>% 
  select(-c(excess_mortality_cumulative_absolute, excess_mortality_cumulative, excess_mortality, excess_mortality_cumulative_per_million))

# Add new column new_deaths_smoothed_2wk that has the same values as 
# new_deaths_smoothed but two weeks ahead ... 

# FIRST copy the data into a new dataframe
covid_data_copy <- covid_data

# Subtract 14 from date field, and rename the new_deaths_smoothed 
# field in the copy
covid_data_copy <- covid_data_copy %>% mutate(date=as.Date(date)-14)
covid_data_copy <- covid_data_copy %>%
  rename(new_deaths_smoothed_2wk = new_deaths_smoothed)

# Join original covid data with copy
covid_data <- covid_data %>% inner_join(covid_data_copy)

# Tidy demographic table as needed
demographics <- demographics %>% 
  pivot_wider(names_from = `Series Code`, values_from = YR2015)

# Join the two tables together to get ready for linear regression
# demographics %>% inner_join(covid_data, by=c("Country Code"="iso_code"))
covid_data <- covid_data %>% 
  inner_join(demographics, by=c("iso_code" = "Country Code"))
