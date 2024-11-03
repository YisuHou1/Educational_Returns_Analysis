#### Preamble ####
# Purpose: Simulates a dataset of Australian electoral divisions, including the 
  #state and party that won each division.
# Author: Rohan Alexander
# Date: 26 September 2024
# Contact: rohan.alexander@utoronto.ca
# License: MIT
# Pre-requisites: The `tidyverse` package must be installed
# Any other information needed? Make sure you are in the `starter_folder` rproj


#### Workspace setup ####
library(tidyverse)
library(lubridate)

#### Simulate data ####
set.seed(114514)

# define the range of pollsters
pollster_options <- c(
  "Siena/NYT", 
  "YouGov", 
  "CES / YouGov", 
  "Marquette Law School", 
  "The Washington Post", 
  "McCourtney Institute/YouGov"
)

# Define US states
us_states <- state.name

# Define number of rows
n <- 300

# For the state variable, have half of the values "National"
n_national <- n / 2  # 50
n_states <- n - n_national  # 50
# Create a vector with 50 "National" and 50 random state names
state_vector <- c(
  rep("National", n_national),
  sample(us_states, n_states, replace = TRUE)
)
# Shuffle the state_vector to randomize the order
state_vector <- sample(state_vector, n, replace = FALSE)

# Generate national simulated dataset
simulated_data <- tibble(
  # pollster randomly assigned from the pollsters with 3.0 numeric grade
  pollster = sample(pollster_options, n, replace = TRUE),
  # has_sponsor is either 0 or 1
  has_sponsor = sample(0:1, n, replace = TRUE),
  # pollscore between -1.1 and -1.5 to mimic the actual dataset
  pollscore = round(runif(n, min = -1.5, max = -1.1), 1),
  # transparency_score between 6.5 and 10.0 with 0.5 increments
  # to mimic the actual dataset
  transparency_score = sample(seq(6.5, 10.0, by = 0.5), n, replace = TRUE),
  sample_size = sample(500:5000, n, replace = TRUE),
  # end date between the day Harris announced and the last day of polls
  # recorded in the polls dataset
  end_date = sample(
    seq.Date(from = as.Date("2024-07-21"), 
    to = as.Date("2024-10-26"), by = "day"), n, replace = TRUE
  ),
  # percent support assigned with a normal distribution
  pct_support = round(rnorm(n, mean = 50, sd = 2))
)

# state is 50% National and 50% randomly selected from US states
simulated_data <- simulated_data %>% mutate(
  state = state_vector
)

#### Write_csv
write_csv(simulated_data, file = "data/00-simulated_data/simulated_polls.csv")

