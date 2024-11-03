#### Preamble ####
# Purpose: Cleans the raw US presidential election polls data from FiveThirtyEight
# Author: Andrew Goh, Yisu Hou
# Date: 3 November 2024
# Contact: rohan.alexander@utoronto.ca
# License: MIT
# Pre-requisites:

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
    end_date = mdy(end_date),
    has_sponsor = if_else(is.na(sponsors), 0, 1)
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
    end_date = mdy(end_date),
    has_sponsor = if_else(is.na(sponsors), 0, 1)
  ) |>
  filter(end_date >= as.Date("2024-07-21"))

# repeat for historical data
biden_past_high_quality <- past_polls_data |>
  filter(
    candidate_name == "Joe Biden",
    numeric_grade >= 3.0,
    population == 'lv' # keep likely voters only to be more predictive
  ) |>
  mutate(
    state = if_else(is.na(state), "National", state),
    end_date = mdy(end_date),
    has_sponsor = if_else(is.na(sponsors), 0, 1)
  )

trump_past_high_quality <- past_polls_data |>
  filter(
    candidate_name == "Donald Trump",
    numeric_grade >= 3.0,
    population == 'lv' # keep likely voters only to be more predictive
  ) |>
  mutate(
    state = if_else(is.na(state), "National", state),
    end_date = mdy(end_date),
    has_sponsor = if_else(is.na(sponsors), 0, 1)
  )


# remove duplicates and select relevant variables
just_harris_high_quality <- just_harris_high_quality %>%
  distinct(poll_id, pollster_id, state, end_date, sample_size, race_id,
  .keep_all = TRUE) %>% select(pollster, has_sponsor, numeric_grade, pollscore, 
  transparency_score, sample_size, end_date, state, candidate_name, pct,
  cycle) %>% 
  drop_na(end_date, has_sponsor, pollscore, transparency_score, sample_size)

just_trump_high_quality <- just_trump_high_quality %>%
  distinct(poll_id, pollster_id, state, end_date, sample_size, race_id,
  .keep_all = TRUE) %>% select(pollster, has_sponsor, numeric_grade, pollscore, 
  transparency_score, sample_size, end_date, state, candidate_name, pct,
  cycle) %>%
  drop_na(end_date, has_sponsor, pollscore, transparency_score, sample_size)

biden_past_high_quality <- biden_past_high_quality %>%
  distinct(poll_id, pollster_id, state, end_date, sample_size, race_id,
  .keep_all = TRUE) %>% select(pollster, has_sponsor, numeric_grade, pollscore, 
  transparency_score, sample_size, end_date, state, candidate_name, pct,
  cycle) %>%
  drop_na(end_date, has_sponsor, pollscore, transparency_score, sample_size)

trump_past_high_quality <- trump_past_high_quality %>%
  distinct(poll_id, pollster_id, state, end_date, sample_size, race_id,
  .keep_all = TRUE) %>% select(pollster, has_sponsor, numeric_grade, pollscore, 
  transparency_score, sample_size, end_date, state, candidate_name, pct,
  cycle) %>%
  drop_na(end_date, has_sponsor, pollscore, transparency_score, sample_size)


#### Lower quality data for regional analysis ####
just_harris_lower_quality <- current_polls_data |>
  filter(
    candidate_name == "Kamala Harris",
    numeric_grade >= 2.5,
    population == 'lv' # keep likely voters only to be more predictive
  ) |>
  mutate(
    state = if_else(is.na(state), "National", state),
    end_date = mdy(end_date),
    has_sponsor = if_else(is.na(sponsors), 0, 1)
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
    end_date = mdy(end_date),
    has_sponsor = if_else(is.na(sponsors), 0, 1)
  ) |>
  filter(end_date >= as.Date("2024-07-21"))

just_harris_lower_quality <- just_harris_lower_quality %>%
  distinct(poll_id, pollster_id, state, end_date, sample_size, race_id,
  .keep_all = TRUE) %>% select(pollster, has_sponsor, numeric_grade, pollscore, 
  transparency_score, sample_size, end_date, state, candidate_name, pct,
  cycle) %>% 
  drop_na(end_date, has_sponsor, pollscore, transparency_score, sample_size)

just_trump_lower_quality <- just_trump_lower_quality %>%
  distinct(poll_id, pollster_id, state, end_date, sample_size, race_id,
  .keep_all = TRUE) %>% select(pollster, has_sponsor, numeric_grade, pollscore, 
  transparency_score, sample_size, end_date, state, candidate_name, pct,
  cycle) %>% 
  drop_na(end_date, has_sponsor, pollscore, transparency_score, sample_size)


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



