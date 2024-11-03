#### Preamble ####
# Purpose: Models... [...UPDATE THIS...]
# Author: Andrew Goh, Yisu Hou
# Date: 3 November 2024
# Contact: rohan.alexander@utoronto.ca [...UPDATE THIS...]
# License: MIT
# Pre-requisites: [...UPDATE THIS...]
# Any other information needed? [...UPDATE THIS...]


#### Workspace setup ####

# Load libraries
library(tidyverse)
library(caret)
library(glmnet)
library(nnet)
library(lubridate)
library(pROC)
library(stringr)


#### Read data ####
trump_2024 <- read_csv("data/02-analysis_data/trump_2024_analysis_data.csv")
harris_2024 <- read_csv("data/02-analysis_data/harris_2024_analysis_data.csv")
trump_2024_lower <- 
  read_csv("data/02-analysis_data/trump_2024_analysis_data_lower.csv")
harris_2024_lower <- 
  read_csv("data/02-analysis_data/harris_2024_analysis_data_lower.csv")
trump_2020 <- read_csv("data/02-analysis_data/trump_2020_analysis_data.csv")
biden_2020 <- read_csv("data/02-analysis_data/biden_2020_analysis_data.csv")

# Summary statistics of datasets [to be updated]

# Regional polls analysis
trump_2024_regional <- trump_2024_lower %>% filter(state != "National")
harris_2024_regional <- harris_2024_lower %>% filter(state != "National")

trump_by_state <- trump_2024_regional %>%
  group_by(state) %>%
  summarize(
    Trump_State_Support = mean(pct, na.rm = TRUE),
    Trump_SD_support = sd(pct, na.rm = TRUE),
    Trump_n = n()
  ) %>%
  mutate(
    Trump_SE = Trump_SD_support / sqrt(Trump_n),                
    Trump_t = qt(0.975, df = Trump_n - 1),                       
    Trump_CI_lower = Trump_State_Support - Trump_t * Trump_SE,   
    Trump_CI_upper = Trump_State_Support + Trump_t * Trump_SE      
  )
harris_by_state <- harris_2024_regional %>%
  group_by(state) %>%
  summarize(
    Harris_State_Support = mean(pct, na.rm = TRUE),
    Harris_SD_support = sd(pct, na.rm = TRUE),
    Harris_n = n()
  ) %>%
  mutate(
    Harris_SE = Harris_SD_support / sqrt(Harris_n),                
    Harris_t = qt(0.975, df = Harris_n - 1),                       
    Harris_CI_lower = Harris_State_Support - Harris_t * Harris_SE,   
    Harris_CI_upper = Harris_State_Support + Harris_t * Harris_SE      
  )


# combine the two data frames for comparison
combined_regional <- left_join(
  trump_by_state,
  harris_by_state,
  by = "state"
)

# change all NaN values to NA
combined_regional <- combined_regional %>%
  mutate(across(everything(), ~ replace(.x, is.nan(.x), NA)))

# Mutate expected winner column, determined by the confidence intervals
# If confidence intervals are missing, compare average support levels instead
combined_regional <- combined_regional %>% mutate(
  exp_winner = case_when(
    # 1. Both Candidates Have CIs
    !is.na(Trump_CI_lower) & !is.na(Trump_CI_upper) &
      !is.na(Harris_CI_lower) & !is.na(Harris_CI_upper) &
      Trump_CI_lower > Harris_CI_upper ~ "Trump",
    
    !is.na(Trump_CI_lower) & !is.na(Trump_CI_upper) &
      !is.na(Harris_CI_lower) & !is.na(Harris_CI_upper) &
      Harris_CI_lower > Trump_CI_upper ~ "Harris",
    
    # If Both Have CIs but They Overlap
    !is.na(Trump_CI_lower) & !is.na(Trump_CI_upper) &
      !is.na(Harris_CI_lower) & !is.na(Harris_CI_upper) ~ "Unknown",
    
    # 2a. Only Trump Has CI
    !is.na(Trump_CI_lower) & !is.na(Trump_CI_upper) &
      is.na(Harris_CI_lower) & is.na(Harris_CI_upper) &
      Harris_State_Support < Trump_CI_lower ~ "Trump",
    
    !is.na(Trump_CI_lower) & !is.na(Trump_CI_upper) &
      is.na(Harris_CI_lower) & is.na(Harris_CI_upper) &
      Harris_State_Support > Trump_CI_upper ~ "Harris",
    
    # If Only Trump Has CI and Harris's Support is Within Trump's CI
    !is.na(Trump_CI_lower) & !is.na(Trump_CI_upper) &
      is.na(Harris_CI_lower) & is.na(Harris_CI_upper) ~ "Unknown",
    
    # 2b. Only Harris Has CI
    !is.na(Harris_CI_lower) & !is.na(Harris_CI_upper) &
      is.na(Trump_CI_lower) & is.na(Trump_CI_upper) &
      Trump_State_Support < Harris_CI_lower ~ "Harris",
    
    !is.na(Harris_CI_lower) & !is.na(Harris_CI_upper) &
      is.na(Trump_CI_lower) & is.na(Trump_CI_upper) &
      Trump_State_Support > Harris_CI_upper ~ "Trump",
    
    # If Only Harris Has CI and Trump's Support is Within Harris's CI
    !is.na(Harris_CI_lower) & !is.na(Harris_CI_upper) &
      is.na(Trump_CI_lower) & is.na(Trump_CI_upper) ~ "Unknown",
    
    # 4. Special Scenario: Harris_State_Support is NA
    is.na(Harris_State_Support) ~ case_when(
      Trump_State_Support > 50 ~ "Trump",
      Trump_State_Support < 49 ~ "Harris",
      Trump_State_Support >= 49 & Trump_State_Support <= 50 ~ "Unknown",
      TRUE ~ "Unknown"
    ),
    
    # 3. Neither Candidate Has CI
    is.na(Trump_CI_lower) & is.na(Trump_CI_upper) &
      is.na(Harris_CI_lower) & is.na(Harris_CI_upper) &
      Trump_State_Support > Harris_State_Support ~ "Trump",
    
    is.na(Trump_CI_lower) & is.na(Trump_CI_upper) &
      is.na(Harris_CI_lower) & is.na(Harris_CI_upper) &
      Harris_State_Support > Trump_State_Support ~ "Harris",
    
    # If Both Support Equal
    is.na(Trump_CI_lower) & is.na(Trump_CI_upper) &
      is.na(Harris_CI_lower) & is.na(Harris_CI_upper) &
      Trump_State_Support == Harris_State_Support ~ "Unknown",
    
    # Catch-All for Any Other Cases
    TRUE ~ "Unknown"
  )
)

# include electoral votes to determine who is in the lead
state_evs <- tibble::tribble(
  ~state, ~electoral_votes,
  "Alaska", 3,
  "Arizona", 11,
  "Arkansas", 6,
  "California", 55,
  "Colorado", 9,
  "Connecticut", 7,
  "Florida", 29,
  "Georgia", 16,
  "Illinois", 20,
  "Indiana", 11,
  "Iowa", 6,
  "Kansas", 6,
  "Louisiana", 8,
  "Maine", 2,
  "Maine CD-1", 1,
  "Maine CD-2", 1,
  "Maryland", 10,
  "Massachusetts", 11,
  "Michigan", 16,
  "Minnesota", 10,
  "Mississippi", 6,
  "Missouri", 10,
  "Montana", 3,
  "Nebraska", 2,
  "Nebraska CD-2", 1,
  "Nevada", 6,
  "New Hampshire", 4,
  "New Mexico", 5,
  "New York", 29,
  "North Carolina", 15,
  "Ohio", 18,
  "Oklahoma", 7,
  "Oregon", 7,
  "Pennsylvania", 20,
  "Rhode Island", 4,
  "South Carolina", 9,
  "South Dakota", 3,
  "Texas", 38,
  "Utah", 6,
  "Vermont", 3,
  "Virginia", 13,
  "Washington", 12,
  "Wisconsin", 10
)

combined_regional <- combined_regional %>%
  left_join(state_evs, by = "state")

