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
library(gt)


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

# note: roc curve above diagonal line is good
# auc value closer to 1 is good, closer to 0 is bad

# Check correlation matrix
cor_matrix <- combined_national %>%
  select(has_sponsor, 
         transparency_score, sample_size) %>%
  cor()

print(cor_matrix)

# Regional polls analysis
trump_2024_regional <- trump_2024_lower %>% filter(state != "National")
harris_2024_regional <- harris_2024_lower %>% filter(state != "National")


trump_Pennsylvania <- trump_2024_regional %>% filter(state == "Pennsylvania")
trump_Georgia <- trump_2024_regional %>% filter(state == "Georgia")
trump_North_Carolina <- trump_2024_regional %>% filter(state == "North Carolina")
trump_Michigan <- trump_2024_regional %>% filter(state == "Michigan")
trump_Arizona <- trump_2024_regional %>% filter(state == "Arizona")
trump_Wisconsin <- trump_2024_regional %>% filter(state == "Wisconsin")
trump_Nevada <- trump_2024_regional %>% filter(state == "Nevada")

harris_Pennsylvania <- harris_2024_regional %>% filter(state == "Pennsylvania")
harris_Georgia <- harris_2024_regional %>% filter(state == "Georgia")
harris_North_Carolina <- harris_2024_regional %>% filter(state == "North Carolina")
harris_Michigan <- harris_2024_regional %>% filter(state == "Michigan")
harris_Arizona <- harris_2024_regional %>% filter(state == "Arizona")
harris_Wisconsin <- harris_2024_regional %>% filter(state == "Wisconsin")
harris_Nevada <- harris_2024_regional %>% filter(state == "Nevada")

combined_Pennsylvania <- full_join(
  trump_Pennsylvania,
  harris_Pennsylvania,
  by = c("pollster", "has_sponsor", "pollscore", 
         "transparency_score", "sample_size", "end_date", "state", "cycle")
)

combined_Georgia <- full_join(
  trump_Georgia,
  harris_Georgia,
  by = c("pollster", "has_sponsor", "pollscore", 
         "transparency_score", "sample_size", "end_date", "state", "cycle")
)

combined_North_Carolina <- full_join(
  trump_North_Carolina,
  harris_North_Carolina,
  by = c("pollster", "has_sponsor", "pollscore", 
         "transparency_score", "sample_size", "end_date", "state", "cycle")
)

combined_Michigan <- full_join(
  trump_Michigan,
  harris_Michigan,
  by = c("pollster", "has_sponsor", "pollscore", 
         "transparency_score", "sample_size", "end_date", "state", "cycle")
)

combined_Arizona <- full_join(
  trump_Arizona,
  harris_Arizona,
  by = c("pollster", "has_sponsor", "pollscore", 
         "transparency_score", "sample_size", "end_date", "state", "cycle")
)

combined_Wisconsin <- full_join(
  trump_Wisconsin,
  harris_Wisconsin,
  by = c("pollster", "has_sponsor", "pollscore", 
         "transparency_score", "sample_size", "end_date", "state", "cycle")
)

combined_Nevada <- full_join(
  trump_Nevada,
  harris_Nevada,
  by = c("pollster", "has_sponsor", "pollscore", 
         "transparency_score", "sample_size", "end_date", "state", "cycle")
)

# mutate harris_win variable by comparing polled support rates
# preparing data for logistic regression
combined_Pennsylvania <- combined_Pennsylvania %>% mutate(
  harris_win = case_when(
    pct.y >= pct.x ~ 1,
    pct.y < pct.x ~ 0
  )
)

combined_Georgia <- combined_Georgia %>% mutate(
  harris_win = case_when(
    pct.y >= pct.x ~ 1,
    pct.y < pct.x ~ 0
  )
)

combined_North_Carolina <- combined_North_Carolina %>% mutate(
  harris_win = case_when(
    pct.y >= pct.x ~ 1,
    pct.y < pct.x ~ 0
  )
)

combined_Michigan <- combined_Michigan %>% mutate(
  harris_win = case_when(
    pct.y >= pct.x ~ 1,
    pct.y < pct.x ~ 0
  )
)

combined_Arizona <- combined_Arizona %>% mutate(
  harris_win = case_when(
    pct.y >= pct.x ~ 1,
    pct.y < pct.x ~ 0
  )
)

combined_Wisconsin <- combined_Wisconsin %>% mutate(
  harris_win = case_when(
    pct.y >= pct.x ~ 1,
    pct.y < pct.x ~ 0
  )
)

combined_Nevada <- combined_Nevada %>% mutate(
  harris_win = case_when(
    pct.y >= pct.x ~ 1,
    pct.y < pct.x ~ 0
  )
)

# Create 7 different logistic regressions to predict Harris's win rate
# in each state
logistic_Pennsylvania <- glm(
  harris_win ~ end_date + has_sponsor + transparency_score + sample_size,
  data = combined_Pennsylvania,
  family = binomial
)

logistic_Georgia <- glm(
  harris_win ~ end_date + has_sponsor + transparency_score + sample_size,
  data = combined_Georgia,
  family = binomial
)

logistic_North_Carolina <- glm(
  harris_win ~ end_date + has_sponsor + transparency_score + sample_size,
  data = combined_North_Carolina,
  family = binomial
)

logistic_Michigan <- glm(
  harris_win ~ end_date + has_sponsor + transparency_score + sample_size,
  data = combined_Michigan,
  family = binomial
)

logistic_Arizona <- glm(
  harris_win ~ end_date + has_sponsor + transparency_score + sample_size,
  data = combined_Arizona,
  family = binomial
)

logistic_Wisconsin <- glm(
  harris_win ~ end_date + has_sponsor + transparency_score + sample_size,
  data = combined_Wisconsin,
  family = binomial
)

logistic_Nevada <- glm(
  harris_win ~ end_date + has_sponsor + transparency_score + sample_size,
  data = combined_Nevada,
  family = binomial
)

# test logistic models for state specific analysis
# mutate fitted values

combined_Pennsylvania <- combined_Pennsylvania %>%
  mutate(
    harris_win_prob = round(100*predict(logistic_Pennsylvania, type = "response"), 2)
  )

roc_obj_Pennsylvania <- roc(combined_Pennsylvania$harris_win,
                  combined_Pennsylvania$harris_win_prob)
plot(roc_obj_Pennsylvania, main = "ROC Curve for Harris Win Prediction, Pennsylvania")
auc_value_Pennsylvania <- auc(roc_obj_Pennsylvania)


combined_Georgia <- combined_Georgia %>%
  mutate(
    harris_win_prob = round(100*predict(logistic_Georgia, type = "response"), 2)
  )

roc_obj_Georgia <- roc(combined_Georgia$harris_win,
                            combined_Georgia$harris_win_prob)
plot(roc_obj_Georgia, main = "ROC Curve for Harris Win Prediction, Georgia")
auc_value_Georgia <- auc(roc_obj_Georgia)


combined_North_Carolina <- combined_North_Carolina %>%
  mutate(
    harris_win_prob = round(100*predict(logistic_North_Carolina, type = "response"), 2)
  )

roc_obj_North_Carolina <- roc(combined_North_Carolina$harris_win,
                            combined_North_Carolina$harris_win_prob)
plot(roc_obj_North_Carolina, main = "ROC Curve for Harris Win Prediction, North_Carolina")
auc_value_North_Carolina <- auc(roc_obj_North_Carolina)


combined_Michigan <- combined_Michigan %>%
  mutate(
    harris_win_prob = round(100*predict(logistic_Michigan, type = "response"), 2)
  )

roc_obj_Michigan <- roc(combined_Michigan$harris_win,
                            combined_Michigan$harris_win_prob)
plot(roc_obj_Michigan, main = "ROC Curve for Harris Win Prediction, Michigan")
auc_value_Michigan <- auc(roc_obj_Michigan)

combined_Arizona <- combined_Arizona %>%
  mutate(
    harris_win_prob = round(100*predict(logistic_Arizona, type = "response"), 2)
  )

roc_obj_Arizona <- roc(combined_Arizona$harris_win,
                            combined_Arizona$harris_win_prob)
plot(roc_obj_Arizona, main = "ROC Curve for Harris Win Prediction, Arizona")
auc_value_Arizona <- auc(roc_obj_Arizona)


combined_Wisconsin <- combined_Wisconsin %>%
  mutate(
    harris_win_prob = round(100*predict(logistic_Wisconsin, type = "response"), 2)
  )

roc_obj_Wisconsin <- roc(combined_Wisconsin$harris_win,
                            combined_Wisconsin$harris_win_prob)
plot(roc_obj_Wisconsin, main = "ROC Curve for Harris Win Prediction, Wisconsin")
auc_value_Wisconsin <- auc(roc_obj_Wisconsin)


combined_Nevada <- combined_Nevada %>%
  mutate(
    harris_win_prob = round(100*predict(logistic_Nevada, type = "response"), 2)
  )

roc_obj_Nevada <- roc(combined_Nevada$harris_win,
                            combined_Nevada$harris_win_prob)
plot(roc_obj_Nevada, main = "ROC Curve for Harris Win Prediction, Nevada")
auc_value_Nevada <- auc(roc_obj_Nevada)


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

saveRDS(
  logistic_Pennsylvania,
  file = "models/Logistic_model_Pennsylvania.rds"
)

saveRDS(
  logistic_Georgia,
  file = "models/Logistic_model_Georgia.rds"
)

saveRDS(
  logistic_North_Carolina,
  file = "models/Logistic_model_North_Carolina.rds"
)

saveRDS(
  logistic_Michigan,
  file = "models/Logistic_model_Michigan.rds"
)

saveRDS(
  logistic_Arizona,
  file = "models/Logistic_model_Arizona.rds"
)

saveRDS(
  logistic_Wisconsin,
  file = "models/Logistic_model_Wisconsin.rds"
)

saveRDS(
  logistic_Nevada,
  file = "models/Logistic_model_Nevada.rds"
)


#### Results ####
# To predict the outcome on election day, generate 1000 hypothetical data points
# using the election day as end_date and the normal curve of other input 
# variables, mimicing all the situations that may happen on election day
hypothetical_data_Pennsylvania <- tibble(
  end_date = as.Date("2024-11-05"),
  has_sponsor = rbinom(1000, size = 1, prob = mean(combined_Pennsylvania$has_sponsor)),
  transparency_score = rnorm(1000, mean = mean(combined_Pennsylvania$transparency_score), 
                             sd = sd(combined_Pennsylvania$transparency_score)),
  sample_size = rnorm(1000, mean = mean(combined_Pennsylvania$sample_size),
                      sd = sd(combined_Pennsylvania$sample_size))
)

hypothetical_data_Georgia <- tibble(
  end_date = as.Date("2024-11-05"),
  has_sponsor = rbinom(1000, size = 1, prob = mean(combined_Georgia$has_sponsor)),
  transparency_score = rnorm(1000, mean = mean(combined_Georgia$transparency_score), 
                             sd = sd(combined_Georgia$transparency_score)),
  sample_size = rnorm(1000, mean = mean(combined_Georgia$sample_size),
                      sd = sd(combined_Georgia$sample_size))
)

hypothetical_data_North_Carolina <- tibble(
  end_date = as.Date("2024-11-05"),
  has_sponsor = rbinom(1000, size = 1, prob = mean(combined_North_Carolina$has_sponsor)),
  transparency_score = rnorm(1000, mean = mean(combined_North_Carolina$transparency_score), 
                             sd = sd(combined_North_Carolina$transparency_score)),
  sample_size = rnorm(1000, mean = mean(combined_North_Carolina$sample_size),
                      sd = sd(combined_North_Carolina$sample_size))
)

hypothetical_data_Michigan <- tibble(
  end_date = as.Date("2024-11-05"),
  has_sponsor = rbinom(1000, size = 1, prob = mean(combined_Michigan$has_sponsor)),
  transparency_score = rnorm(1000, mean = mean(combined_Michigan$transparency_score), 
                             sd = sd(combined_Michigan$transparency_score)),
  sample_size = rnorm(1000, mean = mean(combined_Michigan$sample_size),
                      sd = sd(combined_Michigan$sample_size))
)

hypothetical_data_Arizona <- tibble(
  end_date = as.Date("2024-11-05"),
  has_sponsor = rbinom(1000, size = 1, prob = mean(combined_Arizona$has_sponsor)),
  transparency_score = rnorm(1000, mean = mean(combined_Arizona$transparency_score), 
                             sd = sd(combined_Arizona$transparency_score)),
  sample_size = rnorm(1000, mean = mean(combined_Arizona$sample_size),
                      sd = sd(combined_Arizona$sample_size))
)

hypothetical_data_Wisconsin <- tibble(
  end_date = as.Date("2024-11-05"),
  has_sponsor = rbinom(1000, size = 1, prob = mean(combined_Wisconsin$has_sponsor)),
  transparency_score = rnorm(1000, mean = mean(combined_Wisconsin$transparency_score), 
                             sd = sd(combined_Wisconsin$transparency_score)),
  sample_size = rnorm(1000, mean = mean(combined_Wisconsin$sample_size),
                      sd = sd(combined_Wisconsin$sample_size))
)

hypothetical_data_Nevada <- tibble(
  end_date = as.Date("2024-11-05"),
  has_sponsor = rbinom(1000, size = 1, prob = mean(combined_Nevada$has_sponsor)),
  transparency_score = rnorm(1000, mean = mean(combined_Nevada$transparency_score), 
                             sd = sd(combined_Nevada$transparency_score)),
  sample_size = rnorm(1000, mean = mean(combined_Nevada$sample_size),
                      sd = sd(combined_Nevada$sample_size))
)

# run the corresponding logistic regression on the hypothetical data of each
# state and average the outcomes to find the expected win rate of Harris
# on election day
hypothetical_data_Pennsylvania <- hypothetical_data_Pennsylvania %>%
  mutate(
    harris_win_prob = round(100*predict(logistic_Pennsylvania, newdata = ., type = "response"), 2)
  )


hypothetical_data_Georgia <- hypothetical_data_Georgia %>%
  mutate(
    harris_win_prob = round(100*predict(logistic_Georgia, newdata = ., type = "response"), 2)
  )


hypothetical_data_North_Carolina <- hypothetical_data_North_Carolina %>%
  mutate(
    harris_win_prob = round(100*predict(logistic_North_Carolina, newdata = ., type = "response"), 2)
  )


hypothetical_data_Michigan <- hypothetical_data_Michigan %>%
  mutate(
    harris_win_prob = round(100*predict(logistic_Michigan, newdata = ., type = "response"), 2)
  )


hypothetical_data_Arizona <- hypothetical_data_Arizona %>%
  mutate(
    harris_win_prob = round(100*predict(logistic_Arizona, newdata = ., type = "response"), 2)
  )


hypothetical_data_Wisconsin <- hypothetical_data_Wisconsin %>%
  mutate(
    harris_win_prob = round(100*predict(logistic_Wisconsin, newdata = ., type = "response"), 2)
  )


hypothetical_data_Nevada <- hypothetical_data_Nevada %>%
  mutate(
    harris_win_prob = round(100*predict(logistic_Nevada, newdata = ., type = "response"), 2)
  )


# produce a table of mean winning probabilities
mean_probabilities <- tibble(
  State = c("Pennsylvania", "Georgia", "North Carolina", "Michigan", "Arizona", "Wisconsin", "Nevada"),
  Harris_Win_Probability = c(
    round(mean(hypothetical_data_Pennsylvania$harris_win_prob, na.rm = TRUE), 2),
    round(mean(hypothetical_data_Georgia$harris_win_prob, na.rm = TRUE), 2),
    round(mean(hypothetical_data_North_Carolina$harris_win_prob, na.rm = TRUE), 2),
    round(mean(hypothetical_data_Michigan$harris_win_prob, na.rm = TRUE), 2),
    round(mean(hypothetical_data_Arizona$harris_win_prob, na.rm = TRUE), 2),
    round(mean(hypothetical_data_Wisconsin$harris_win_prob, na.rm = TRUE), 2),
    round(mean(hypothetical_data_Nevada$harris_win_prob, na.rm = TRUE), 2)
  )
)


# include electoral votes to determine who is in the lead
state_evs <- tibble::tribble(
  ~State, ~electoral_votes,
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

mean_probabilities <- mean_probabilities %>%
  left_join(state_evs, by = "State")

mean_probabilities %>%
  gt() %>%
  tab_header(
    title = "Average Probability of Harris Winning by State",
    subtitle = "Based on Logistic Model Predictions of 1,000 Hypothetical Polls per State"
  ) %>%
  fmt_number(
    columns = vars(Harris_Win_Probability),
    decimals = 2
  ) %>%
  cols_label(
    State = "State",
    Harris_Win_Probability = "Harris Win Probability (%)",
    electoral_votes = "Electoral Votes"
  ) %>%
  tab_options(
    table.font.size = 12,
    heading.title.font.size = 16,
    heading.subtitle.font.size = 12,
    table.border.top.width = px(2),
    table.border.bottom.width = px(2)
  ) 

#### Overall Probability of Winning ####
# Harris
# Create data frame of states
states <- data.frame(
  state = c("PA", "GA", "NC", "MI", "WI", "NV", "AZ"),
  prob = c(0.5551, 0.6801, 0.1033, 0.5151, 0.3792, 0.7679, 0),
  votes = c(20, 16, 15, 16, 10, 6, 11)
)

# Function to calculate probability of a specific combination
calc_combo_prob <- function(combo, states) {
  prob <- 1
  for(i in 1:nrow(states)) {
    if(i %in% combo) {
      prob <- prob * states$prob[i]
    } else {
      prob <- prob * (1 - states$prob[i])
    }
  }
  return(prob)
}

# Generate all possible combinations
n_states <- nrow(states)
total_prob <- 0

# Check each possible combination
for(i in 1:(2^n_states - 1)) {
  # Convert number to binary to get combination
  combo <- which(intToBits(i)[1:n_states] == 1)
  
  # Calculate total electoral votes for this combination
  votes <- sum(states$votes[combo])
  
  # If this combination has enough votes, add its probability
  if(votes >= 44) {
    prob <- calc_combo_prob(combo, states)
    total_prob <- total_prob + prob
  }
}

print(paste("Probability of Harris winning at least 44 electoral votes:", round(total_prob * 100, 2), "%"))

# Trump
# Create data frame of states
states <- data.frame(
  state = c("PA", "GA", "NC", "MI", "WI", "NV","AZ"),
  prob = c(1-0.5551, 1-0.6801, 1-0.1033, 1-0.5151, 1-0.3792, 1-0.7679, 1-0), # Using 1-p for Trump's probabilities
  votes = c(20, 16, 15, 16, 10, 6, 11)
)

# Function to calculate probability of a specific combination
calc_combo_prob <- function(combo, states) {
  prob <- 1
  for(i in 1:nrow(states)) {
    if(i %in% combo) {
      prob <- prob * states$prob[i]
    } else {
      prob <- prob * (1 - states$prob[i])
    }
  }
  return(prob)
}

# Generate all possible combinations
n_states <- nrow(states)
total_prob <- 0

# Check each possible combination
for(i in 1:(2^n_states - 1)) {
  # Convert number to binary to get combination
  combo <- which(intToBits(i)[1:n_states] == 1)
  
  # Calculate total electoral votes for this combination
  votes <- sum(states$votes[combo])
  
  # If this combination has enough votes, add its probability
  if(votes >= 51) {
    prob <- calc_combo_prob(combo, states)
    total_prob <- total_prob + prob
  }
}

print(paste("Probability of Trump winning at least 51 electoral votes:", round(total_prob * 100, 2), "%"))

