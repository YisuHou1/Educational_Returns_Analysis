#### Preamble ####
# Purpose: Cleans the raw plane data recorded by two observers..... [...UPDATE THIS...]
# Author: Rohan Alexander [...UPDATE THIS...]
# Date: 6 April 2023 [...UPDATE THIS...]
# Contact: rohan.alexander@utoronto.ca [...UPDATE THIS...]
# License: MIT
# Pre-requisites: [...UPDATE THIS...]
# Any other information needed? [...UPDATE THIS...]

#### Workspace setup ####

# Load libraries
library(tidyverse)
library(janitor)
library(lubridate)
library(broom)
library(modelsummary)
library(rstanarm)
library(splines)


#### Read data ####
current_polls_data <- read_csv("data/01-raw_data/president_polls.csv") |>
  clean_names()
past_polls_data <- 
  read_csv("data/01-raw_data/president_polls_historical.csv") |>
  clean_names()

# Filter data to Harris estimates based on high-quality polls after she declared
just_harris_high_quality <- current_polls_data |>
  filter(
    candidate_name == "Kamala Harris",
    numeric_grade >= 3.0,
    population == 'lv' # keep likely voters only to be more predictive
  ) |>
  mutate(
    state = if_else(is.na(state), "National", state),
    end_date = mdy(end_date)
  ) |>
  filter(end_date >= as.Date("2024-07-21"))

# Filter data to Trump estimates based on high-quality polls
just_trump_high_quality <- current_polls_data |>
  filter(
    candidate_name == "Donald Trump",
    numeric_grade >= 3.0,
    population == 'lv' # keep likely voters only to be more predictive
  ) |>
  mutate(
    state = if_else(is.na(state), "National", state),
    end_date = mdy(end_date)
  )

# repeat for historical data
biden_past_high_quality <- past_polls_data |>
  filter(
    candidate_name == "Joe Biden",
    numeric_grade >= 3.0,
    population == 'lv' # keep likely voters only to be more predictive
  ) |>
  mutate(
    state = if_else(is.na(state), "National", state),
    end_date = mdy(end_date)
  )

trump_past_high_quality <- past_polls_data |>
  filter(
    candidate_name == "Donald Trump",
    numeric_grade >= 3.0,
    population == 'lv' # keep likely voters only to be more predictive
  ) |>
  mutate(
    state = if_else(is.na(state), "National", state),
    end_date = mdy(end_date)
  )


# remove duplicates and select relevant variables
just_harris_high_quality <- just_harris_high_quality %>%
  distinct(poll_id, pollster_id, state, end_date, sample_size, race_id,
  .keep_all = TRUE) %>% select(pollster, sponsors, pollscore, 
  transparency_score, sample_size, end_date, state, candidate_name, pct,
  cycle)

just_trump_high_quality <- just_trump_high_quality %>%
  distinct(poll_id, pollster_id, state, end_date, sample_size, race_id,
  .keep_all = TRUE) %>% select(pollster, sponsors, pollscore, 
  transparency_score, sample_size, end_date, state, candidate_name, pct,
  cycle)

biden_past_high_quality <- biden_past_high_quality %>%
  distinct(poll_id, pollster_id, state, end_date, sample_size, race_id,
           .keep_all = TRUE) %>% select(pollster, sponsors, pollscore, 
  transparency_score, sample_size, end_date, state, candidate_name, pct,
  cycle)

trump_past_high_quality <- trump_past_high_quality %>%
  distinct(poll_id, pollster_id, state, end_date, sample_size, race_id,
           .keep_all = TRUE) %>% select(pollster, sponsors, pollscore, 
  transparency_score, sample_size, end_date, state, candidate_name, pct,
  cycle)


#### Lower quality data for regional analysis ####
just_harris_lower_quality <- current_polls_data |>
  filter(
    candidate_name == "Kamala Harris",
    numeric_grade >= 2.5,
    population == 'lv' # keep likely voters only to be more predictive
  ) |>
  mutate(
    state = if_else(is.na(state), "National", state),
    end_date = mdy(end_date)
  ) |>
  filter(end_date >= as.Date("2024-07-21"))

just_trump_lower_quality <- current_polls_data |>
  filter(
    candidate_name == "Donald Trump",
    numeric_grade >= 2.5,
    population == 'lv' # keep likely voters only to be more predictive
  ) |>
  mutate(
    state = if_else(is.na(state), "National", state),
    end_date = mdy(end_date)
  )

just_harris_lower_quality <- just_harris_lower_quality %>%
  distinct(poll_id, pollster_id, state, end_date, sample_size, race_id,
  .keep_all = TRUE) %>% select(pollster, sponsors, pollscore, 
  transparency_score, sample_size, end_date, state, candidate_name, pct,
  cycle)

just_trump_lower_quality <- just_trump_lower_quality %>%
  distinct(poll_id, pollster_id, state, end_date, sample_size, race_id,
  .keep_all = TRUE) %>% select(pollster, sponsors, pollscore, 
  transparency_score, sample_size, end_date, state, candidate_name, pct,
  cycle)


#### Save data ####
write_csv(just_harris_high_quality,
          "data/02-analysis_data/harris_2024_analysis_data.csv")

write_csv(just_trump_high_quality,
          "data/02-analysis_data/trump_2024_analysis_data.csv")

write_csv(biden_past_high_quality,
          "data/02-analysis_data/biden_2020_analysis_data.csv")

write_csv(trump_past_high_quality,
          "data/02-analysis_data/trump_2020_analysis_data.csv")

write_csv(just_harris_lower_quality,
          "data/02-analysis_data/harris_2024_analysis_data_lower.csv")

write_csv(just_trump_lower_quality,
          "data/02-analysis_data/trump_2024_analysis_data_lower.csv")



