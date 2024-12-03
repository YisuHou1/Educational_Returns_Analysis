#### Preamble ####
# Purpose: Simulates a dataset of Chinese individual survey data
# Author: Yisu Hou
# Date: 28 November 2024
# Contact: yisu.hou@mail.utoronto.ca
# License: None
# Pre-requisites: R Package `tidyverse` must be installed


#### Workspace setup ####
library(tidyverse)

#### Simulate data ####
set.seed(114514)


#### Generate data with all variables of interest ####
# Define number of rows
n <- 500

# 1. yearly_income
# Generate from a normal distribution with mean=50000 and sd=20000
# Then round to nearest hundred and constrain between 0 and 1,000,000
# This is to mimic the actual dataset, where values are measured in hundreds
yearly_income <- rnorm(n, mean = 50000, sd = 20000)
# Replace negative incomes with 0
yearly_income[yearly_income < 0] <- 0
# Replace incomes above 1,000,000 with 1,000,000
yearly_income[yearly_income > 1000000] <- 1000000
# Round to nearest hundred
yearly_income <- round(yearly_income / 100) * 100

# 2. years_of_education
# Education is recorded as levels (primary, secondary, etc) in the actual
# dataset, which correspons to the numbers below
education_levels <- c(0, 6, 9, 12, 15, 16, 19, 22)
years_of_education <- sample(education_levels, n, replace = TRUE)

# 3. years_of_work
years_of_work <- sample(0:40, n, replace = TRUE)

# Dummy Variables
# 4. gender (0 or 1)
# 1 for male, 0 for female
gender <- sample(0:1, n, replace = TRUE)

# 5. city_hukou (0 or 1)
city_hukou <- sample(0:1, n, replace = TRUE)

# 6. is_party (0 or 1)
is_party <- sample(0:1, n, replace = TRUE)

# 7. is_married (0 or 1)
is_married <- sample(0:1, n, replace = TRUE)

# 8. is_east (0 or 1)
is_east <- sample(0:1, n, replace = TRUE)

# Combine all variables into a data frame
simulated_data <- data.frame(
  yearly_income = yearly_income,
  years_of_education = years_of_education,
  years_of_work = years_of_work,
  gender = gender,
  city_hukou = city_hukou,
  is_party = is_party,
  is_married = is_married,
  is_east = is_east
)

#### Write_csv ####
write_csv(simulated_data, file = "data/00-simulated_data/simulated_data.csv")

