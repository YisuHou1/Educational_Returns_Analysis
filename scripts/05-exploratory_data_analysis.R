#### Preamble ####
# Purpose: Models... [...UPDATE THIS...]
# Author: Rohan Alexander [...UPDATE THIS...]
# Date: 11 February 2023 [...UPDATE THIS...]
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


#### Read data ####
trump_2024 <- read_csv("data/02-analysis_data/trump_2024_analysis_data.csv")
harris_2024 <- read_csv("data/02-analysis_data/harris_2024_analysis_data.csv")
trump_2024_lower <- 
  read_csv("data/02-analysis_data/trump_2024_analysis_data_lower.csv")
harris_2024_lower <- 
  read_csv("data/02-analysis_data/harris_2024_analysis_data_lower.csv")
trump_2020 <- read_csv("data/02-analysis_data/trump_2020_analysis_data.csv")
biden_2020 <- read_csv("data/02-analysis_data/biden_2020_analysis_data.csv")

# find biases of pollsters from 2020 national polls data
# filter out national polls from the datasets
trump_2024_national <- trump_2024 %>% filter(state == "National")
harris_2024_national <- harris_2024 %>% filter(state == "National")
trump_2020_national <- trump_2020 %>% filter(state == "National")
biden_2020_national <- biden_2020 %>% filter(state == "National")

# Define actual results
actual_results_2020 <- list(Trump_pct = 47, Biden_pct = 51)

# Calculate bias per pollster
rep_pollster_bias <- trump_2020_national %>%
  group_by(pollster) %>%
  summarize(
    Trump_bias = mean(pct - actual_results_2020$Trump_pct, na.rm = TRUE)
  )

dem_pollster_bias <- biden_2020_national %>%
  group_by(pollster) %>%
    summarize(
      Biden_bias = mean(pct - actual_results_2020$Biden_pct, na.rm = TRUE)
  )

# adjust 2024 national polls percentages by the average pollster biases
# also mutate has_sponsor variable into dummy variable and remove NA's
trump_2024_national_adj <- trump_2024_national %>%
  left_join(rep_pollster_bias, by = "pollster") %>%
  mutate(
    pct_adj = pct - ifelse(!is.na(Trump_bias), Trump_bias, 0),
    has_sponsor = if_else(is.na(sponsors), 0, 1)
  ) %>%
  select(-Trump_bias) %>%
  drop_na(end_date, has_sponsor, pollscore, transparency_score, sample_size)

# Adjust Harris 2024 National Polls
harris_2024_national_adj <- harris_2024_national %>%
  left_join(dem_pollster_bias, by = "pollster") %>%
  mutate(
    pct_adj = pct - ifelse(!is.na(Biden_bias), Biden_bias, 0),
    has_sponsor = if_else(is.na(sponsors), 0, 1)
  ) %>%
  select(-Biden_bias) %>%
  drop_na(end_date, has_sponsor, pollscore, transparency_score, sample_size)


# model adjusted support percentage by time and control variables
# first split training and testing dataset using an 80/20 split
set.seed(11451)
trainIndex_trump <- createDataPartition(trump_2024_national_adj$pct_adj, 
      p = 0.8, list = FALSE)
data_train_trump <- trump_2024_national_adj[trainIndex_trump, ]
data_test_trump  <- trump_2024_national_adj[-trainIndex_trump, ]

trainIndex_harris <- createDataPartition(harris_2024_national_adj$pct_adj, 
      p = 0.8, list = FALSE)
data_train_harris <- harris_2024_national_adj[trainIndex_harris, ]
data_test_harris  <- harris_2024_national_adj[-trainIndex_harris, ]

model_date_harris <- lm(pct_adj ~ end_date + has_sponsor +
  transparency_score + sample_size, data = data_train_harris)
model_date_trump <- lm(pct_adj ~ end_date + has_sponsor +
  transparency_score + sample_size, data = data_train_trump)


# Function to evaluate model using testing dataset
evaluate_model <- function(model, test_data) {
  predictions <- predict(model, newdata = test_data)
  actuals <- test_data$pct_adj
  rmse <- sqrt(mean((predictions - actuals)^2, na.rm = TRUE))
  r_squared <- summary(model)$r.squared
  return(list(RMSE = rmse, R_squared = r_squared))
}

# Evaluate Harris model
harris_evaluation <- evaluate_model(model_date_harris, data_test_harris)
print("Harris Model Evaluation:")
print(harris_evaluation)

# Evaluate Trump model
trump_evaluation <- evaluate_model(model_date_trump, data_test_trump)
print("Trump Model Evaluation:")
print(trump_evaluation)

# Augment data with model predictions
model_harris <- lm(pct_adj ~ end_date + has_sponsor +
  transparency_score + sample_size, data = harris_2024_national_adj)
model_trump <- lm(pct_adj ~ end_date + has_sponsor +
  transparency_score + sample_size, data = trump_2024_national_adj)

harris_2024_national_adj <- harris_2024_national_adj %>% mutate(
  fitted_date = predict(model_harris)
)

trump_2024_national_adj <- trump_2024_national_adj %>% mutate(
  fitted_date = predict(model_trump)
)

# plot model predictions
# combine the datasets to create a single visual
combined_data <- bind_rows(harris_2024_national_adj, trump_2024_national_adj)

# create the plot that contains scattered points of adjusted percentages
# and lines for fitted values
ggplot(combined_data, aes(x = end_date, y = pct_adj, color = candidate_name)) +
  # Scatter points for actual adjusted percentages
  geom_point(alpha = 0.6) +
  # Regression lines
  geom_line(aes(y = fitted_date), size = 0.5) +
  xlim(as.Date("2024-07-01"), as.Date("2024-10-25")) +
  # Assign specific colors
  scale_color_manual(values = 
      c("Kamala Harris" = "blue", "Donald Trump" = "red")) +
  # Customize the plot appearance
  theme_classic() +
  labs(
    y = "Adjusted Percent Support",
    x = "End Date",
    title = "Multiple Linear Models for Harris and Trump (2024)",
    color = "Candidate"
  ) +
  theme(
    text = element_text(size = 12),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )


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

# Trump has data in 43 states, while Harris only has data in 33 states
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

#################################################################
# potential changes area
prediction_harris <- data.frame(end_date = end_date_seq) %>%
  mutate(
    has_sponsor = 0,  # Adjust as needed
    pollscore = mean(harris_2024_national_adj$pollscore, na.rm = TRUE),
    transparency_score = mean(harris_2024_national_adj$transparency_score, na.rm = TRUE),
    sample_size = mean(harris_2024_national_adj$sample_size, na.rm = TRUE),
    candidate = "Harris"
  ) %>%
  mutate(pct_adj = predict(model_harris_full, newdata = .))

# Combine prediction data
combined_prediction <- bind_rows(prediction_trump, prediction_harris)

# CI of actual outcome, using the date of the election and average CVs

