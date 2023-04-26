library(tidyverse)
library(ggplot2)
library(modelr)
library(lubridate)
library(dplyr)

# ------------------------------
# PROJECT PART 1: DATA WRANGLING
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
  select(-c(total_deaths, new_deaths, total_deaths_per_million, 
            new_deaths_per_million, new_deaths_smoothed_per_million))
covid_data <- covid_data %>% 
  select(-c(excess_mortality_cumulative_absolute, excess_mortality_cumulative, 
            excess_mortality, excess_mortality_cumulative_per_million))

# Add new column new_deaths_smoothed_2wk that has the same values as 
# new_deaths_smoothed but two weeks ahead ... 

# FIRST copy the data into a new dataframe
covid_data_copy <- covid_data

# Subtract 14 from date field, and rename the new_deaths_smoothed 
# field in the copy
covid_data_copy <- covid_data_copy %>% mutate(date=as.Date(date)-14)
covid_data_copy <- covid_data_copy %>%
  rename(new_deaths_smoothed_2wk = new_deaths_smoothed)

# Select specific columns to use on the join call
covid_data_copy <- covid_data_copy %>%
  select(iso_code, date, new_deaths_smoothed_2wk)

# Join original covid data with copy
covid_data <- covid_data %>% inner_join(covid_data_copy)

# Tidy demographic table as needed
demographics <- demographics %>% 
  pivot_wider(names_from = `Series Code`, values_from = YR2015)

# Join the two tables together to get ready for linear regression
# demographics %>% inner_join(covid_data, by=c("Country Code"="iso_code"))
covid_data <- covid_data %>% 
  inner_join(demographics, by=c("iso_code" = "Country Code"))

covid_data

# -------------------------------
# PROJECT PART 2: LINEAR MODELING
# -------------------------------

# most recently available new deaths per day two weeks ahead
# 2023-03-15 is the last date before we get NAs
recent_new_d_smoothed_2wk <- covid_data %>% 
  filter(!is.na(new_deaths_smoothed_2wk)) %>% group_by(iso_code) %>% top_n(1, date)

ggplot(data=recent_new_d_smoothed_2wk) + 
  geom_point(mapping=aes(x = new_cases_smoothed, y = new_deaths_smoothed_2wk))

# most recently available new deaths per day
recent_new_d_smoothed <- covid_data %>% 
  filter(!is.na(new_deaths_smoothed))%>% group_by(iso_code) %>% top_n(1, date)

ggplot(data=recent_new_d_smoothed) + 
  geom_point(mapping=aes(x = SP.URB.TOTL, y = new_deaths_smoothed))

# 2b. Generate at least 3 transformed vars

# Description of variable and the R code transformations:

# Cardiovascular deaths is the cardiovascular disease death rate times total population
covid_data <- covid_data %>% mutate(cardiovasc_deaths = cardiovasc_death_rate * population)

# This is the population density per square mile/km, this helps if there is a non-linear
# relationship between population density and covid deaths
covid_data <- covid_data %>% mutate(population_density_squared = population_density^2)

# This is the rate of people living in urban areas, which is the urban pop total / pop total
covid_data <- covid_data %>% mutate(urban_pop_rate = (SP.URB.TOTL/SP.POP.TOTL)*100)

# 2c. Split your data set into train and test subsets

# Split data into train and test sets based on date
train_data <- covid_data %>% filter(year(date) == 2022)
test_data <- covid_data %>% filter(year(date) == 2023)

# filter out rows where new_deaths_smoothed_2wk is NA
test_data <- test_data %>% filter(!is.na(new_deaths_smoothed_2wk))

# Check the number of rows in each subset
nrow(train_data)
nrow(test_data)

# Run linear regression with at least 5 different combinations of predictor variables
model1 <- lm(data = train_data, formula = new_deaths_smoothed_2wk ~ new_cases_smoothed + gdp_per_capita +
               diabetes_prevalence + icu_patients)

model2 <- lm(data = train_data, formula = new_deaths_smoothed_2wk ~ new_cases_smoothed + cardiovasc_deaths +
               population_density_squared + hospital_beds_per_thousand)

model3 <- lm(data = train_data, formula = new_deaths_smoothed_2wk ~ new_cases_smoothed + urban_pop_rate +
               human_development_index + hospital_beds_per_thousand)

model4 <- lm(data = train_data, formula = new_deaths_smoothed_2wk ~ new_cases_smoothed + life_expectancy +
               gdp_per_capita + hospital_beds_per_thousand + human_development_index)

model5 <- lm(data = train_data, formula = new_deaths_smoothed_2wk ~ new_cases_smoothed + total_vaccinations_per_hundred + 
               diabetes_prevalence + cardiovasc_deaths + population_density_squared + urban_pop_rate)

# Print summary of each model to view coefficients and model statistics
summary(model1)
summary(model2)
summary(model3)
summary(model4)
summary(model5)

# ---------------------------------
# PROJECT PART 3: EVALUATING MODELS
# ---------------------------------

# Calculating the Root Mean Squared Error (RMSE) over all days in 2023 and all countries
rmse_1 <- modelr::rmse(model=model1, data=test_data)
rmse_2 <- modelr::rmse(model=model2, data=test_data)
rmse_3 <- modelr::rmse(model=model3, data=test_data)
rmse_4 <- modelr::rmse(model=model4, data=test_data)
rmse_5 <- modelr::rmse(model=model5, data=test_data)

# Create RMSE Table

# Note: NaN may happen bc certain parameters not feasible in certain countries
rsme_by_country <- test_data %>% 
  group_by(iso_code) %>% 
  summarise(rmse_country = modelr::rmse(data=cur_data(), model=model1)) %>% top_n(20)

# View Best Model RMSE
rsme_by_country_top_20

# Create Best Model RMSE Table

