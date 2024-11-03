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
  model_harris_adj,
  file = "models/MLR_adjusted_harris.rds"
)

saveRDS(
  model_trump_adj,
  file = "models/MLR_adjusted_trump.rds"
)

saveRDS(
  model_harris,
  file = "models/MLR_harris.rds"
)

saveRDS(
  model_trump,
  file = "models/MLR_trump.rds"
)

saveRDS(
  model_logistic_adj,
  file = "models/Logistic_model_adjusted.rds"
)

saveRDS(
  model_logistic,
  file = "models/Logistic_model.rds"
)
