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
    pct_adj = pct - ifelse(!is.na(Trump_bias), Trump_bias, 0)
  ) %>%
  select(-Trump_bias)

# Adjust Harris 2024 National Polls
harris_2024_national_adj <- harris_2024_national %>%
  left_join(dem_pollster_bias, by = "pollster") %>%
  mutate(
    pct_adj = pct - ifelse(!is.na(Biden_bias), Biden_bias, 0)
  ) %>%
  select(-Biden_bias)


# model adjusted support percentage by time and control variables
# first split training and testing dataset using an 80/20 split
set.seed(11451)
trainIndex_trump_adj <- createDataPartition(trump_2024_national_adj$pct_adj, 
                                        p = 0.8, list = FALSE)
data_train_trump_adj <- trump_2024_national_adj[trainIndex_trump_adj, ]
data_test_trump_adj  <- trump_2024_national_adj[-trainIndex_trump_adj, ]

trainIndex_harris_adj <- createDataPartition(harris_2024_national_adj$pct_adj, 
                                         p = 0.8, list = FALSE)
data_train_harris_adj <- harris_2024_national_adj[trainIndex_harris_adj, ]
data_test_harris_adj  <- harris_2024_national_adj[-trainIndex_harris_adj, ]

model_date_harris_adj <- lm(pct_adj ~ end_date + has_sponsor +
                          transparency_score + sample_size, data = data_train_harris_adj)
model_date_trump_adj <- lm(pct_adj ~ end_date + has_sponsor +
                         transparency_score + sample_size, data = data_train_trump_adj)

# repeat for unadjusted support levels
trainIndex_trump <- createDataPartition(trump_2024_national$pct, 
                                        p = 0.8, list = FALSE)
data_train_trump <- trump_2024_national[trainIndex_trump, ]
data_test_trump  <- trump_2024_national[-trainIndex_trump, ]

trainIndex_harris <- createDataPartition(harris_2024_national$pct, 
                                         p = 0.8, list = FALSE)
data_train_harris <- harris_2024_national[trainIndex_harris, ]
data_test_harris  <- harris_2024_national[-trainIndex_harris, ]

model_date_harris <- lm(pct ~ end_date + has_sponsor +
                          transparency_score + sample_size, data = data_train_harris)
model_date_trump <- lm(pct ~ end_date + has_sponsor +
                         transparency_score + sample_size, data = data_train_trump)


# Functions to evaluate model using testing dataset
evaluate_model_adj <- function(model, test_data) {
  predictions <- predict(model, newdata = test_data)
  actuals <- test_data$pct_adj
  rmse <- sqrt(mean((predictions - actuals)^2, na.rm = TRUE))
  r_squared <- summary(model)$r.squared
  return(list(RMSE = rmse, R_squared = r_squared))
}

evaluate_model <- function(model, test_data) {
  predictions <- predict(model, newdata = test_data)
  actuals <- test_data$pct
  rmse <- sqrt(mean((predictions - actuals)^2, na.rm = TRUE))
  r_squared <- summary(model)$r.squared
  return(list(RMSE = rmse, R_squared = r_squared))
}

# Evaluate Harris model with adjusted support percentages
harris_evaluation_adj <- evaluate_model_adj(model_date_harris_adj, data_test_harris_adj)
print("Harris Adjusted Model Evaluation:")
print(harris_evaluation_adj)

# Evaluate Trump model with adjusted support percentages
trump_evaluation_adj <- evaluate_model_adj(model_date_trump_adj, data_test_trump_adj)
print("Trump Adjusted Model Evaluation:")
print(trump_evaluation_adj)

# Evaluate Harris model with unadjusted support percentages
harris_evaluation <- evaluate_model(model_date_harris, data_test_harris)
print("Harris Unadjusted Model Evaluation:")
print(harris_evaluation)

# Evaluate Trump model with unadjusted support percentages
trump_evaluation <- evaluate_model(model_date_trump, data_test_trump)
print("Trump Unadjusted Model Evaluation:")
print(trump_evaluation)

# Augment data with model predictions
model_harris_adj <- lm(pct_adj ~ end_date + has_sponsor +
                     transparency_score + sample_size, data = harris_2024_national_adj)
model_trump_adj <- lm(pct_adj ~ end_date + has_sponsor +
                    transparency_score + sample_size, data = trump_2024_national_adj)

harris_2024_national_adj <- harris_2024_national_adj %>% mutate(
  fitted_date = predict(model_harris_adj)
)

trump_2024_national_adj <- trump_2024_national_adj %>% mutate(
  fitted_date = predict(model_trump_adj)
)

# Unadjusted datasets
model_harris <- lm(pct ~ end_date + has_sponsor +
                         transparency_score + sample_size, data = harris_2024_national)
model_trump <- lm(pct ~ end_date + has_sponsor +
                        transparency_score + sample_size, data = trump_2024_national)

harris_2024_national <- harris_2024_national %>% mutate(
  fitted_date = predict(model_harris)
)

trump_2024_national <- trump_2024_national %>% mutate(
  fitted_date = predict(model_trump)
)

# plot model predictions
# combine the datasets to create a single visual
combined_data_adj <- bind_rows(harris_2024_national_adj, trump_2024_national_adj)
combined_data <- bind_rows(harris_2024_national, trump_2024_national)

# create the plot that contains scattered points of adjusted percentages
# and lines for fitted values
ggplot(combined_data_adj, aes(x = end_date, y = pct_adj, color = candidate_name)) +
  # Scatter points for actual adjusted percentages
  geom_point(alpha = 0.6) +
  # Regression lines
  geom_line(aes(y = fitted_date), size = 0.5) +
  xlim(as.Date("2024-07-19"), as.Date("2024-10-25")) +
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

# Unadjusted support percentages
ggplot(combined_data, aes(x = end_date, y = pct, color = candidate_name)) +
  # Scatter points for actual adjusted percentages
  geom_point(alpha = 0.6) +
  # Regression lines
  geom_line(aes(y = fitted_date), size = 0.5) +
  xlim(as.Date("2024-07-19"), as.Date("2024-10-25")) +
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

#### Logistic regression for national support ####

combined_national <- full_join(
  trump_2024_national,
  harris_2024_national,
  by = c("pollster", "has_sponsor", "pollscore", 
         "transparency_score", "sample_size", "end_date", "state", "cycle")
)

combined_national <- combined_national %>% mutate(
  harris_win = case_when(
    pct.y >= pct.x ~ 1,
    pct.y < pct.x ~ 0
  )
)

# repeat for adjusted support levels
combined_national_adj <- full_join(
  trump_2024_national_adj,
  harris_2024_national_adj,
  by = c("pollster", "has_sponsor", "pollscore",
         "transparency_score", "sample_size", "end_date", "state", "cycle")
)

combined_national_adj <- combined_national_adj %>% mutate(
  harris_win = case_when(
    pct_adj.y >= pct_adj.x ~ 1,
    pct_adj.y < pct_adj.x ~ 0
  )
)


# logistic model to predict the winner
model_logistic <- glm(
  harris_win ~ end_date + has_sponsor + transparency_score + sample_size,
  data = combined_national,
  family = binomial
)

summary(model_logistic)

model_logistic_adj <- glm(
  harris_win ~ end_date + has_sponsor + transparency_score + sample_size,
  data = combined_national_adj,
  family = binomial
)

summary(model_logistic_adj)

# include predicted outcomes to the dataframes
combined_national <- combined_national %>%
  mutate(
    harris_win_prob = round(100*predict(model_logistic, type = "response"), 2)
  )

combined_national_adj <- combined_national_adj %>%
  mutate(
    harris_win_prob = round(100*predict(model_logistic_adj, 
                                        type = "response"), 2)
  )

#### Evaluate the Model ####

# ROC Curve and AUC
roc_obj <- roc(combined_national$harris_win,
               combined_national$harris_win_prob)
plot(roc_obj, main = "ROC Curve for Harris Win Prediction")
auc_value <- auc(roc_obj)
print(paste("AUC:", auc_value))

# note: roc curve above diagonal line is good
# auc value closer to 1 is good, closer to 0 is bad

# Check correlation matrix
cor_matrix <- combined_national %>%
  select(has_sponsor, 
         transparency_score, sample_size) %>%
  cor()

print(cor_matrix)




#### Save model ####
saveRDS(
  first_model,
  file = "models/first_model.rds"
)


