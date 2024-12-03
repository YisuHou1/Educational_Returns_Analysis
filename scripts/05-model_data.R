#### Preamble ####
# Purpose: Models the educational returns of working individuals in China
# Author: Yisu Hou
# Date: 28 November 2024
# Contact: yisu.hou@mail.utoronto.ca
# License: None
# Pre-requisites:
# - Packages `tidyverse`, `car`, `glmnet`, and `MatchIt` must be installed
# - 02-clean_data.R, 03-test_analysis_data.R,
#   and 04-exploratory_data_analysis.R must have been run


#### Workspace setup ####

library(car)
library(tidyverse)
library(MatchIt)

# import datasets
data_2020 <- read_parquet("data/02-analysis_data/2020_analysis_data.parquet")
data_2018 <- read_parquet("data/02-analysis_data/2018_analysis_data.parquet")
data_2016 <- read_parquet("data/02-analysis_data/2016_analysis_data.parquet")
data_2014 <- read_parquet("data/02-analysis_data/2014_analysis_data.parquet")
data_2012 <- read_parquet("data/02-analysis_data/2012_analysis_data.parquet")
data_2010 <- read_parquet("data/02-analysis_data/2010_analysis_data.parquet")


# split everything into quartiles
quartile_1_2020 <- subset(data_2020, income_quartile == 1)
quartile_2_2020 <- subset(data_2020, income_quartile == 2)
quartile_3_2020 <- subset(data_2020, income_quartile == 3)
quartile_4_2020 <- subset(data_2020, income_quartile == 4)

quartile_1_2018 <- subset(data_2018, income_quartile == 1)
quartile_2_2018 <- subset(data_2018, income_quartile == 2)
quartile_3_2018 <- subset(data_2018, income_quartile == 3)
quartile_4_2018 <- subset(data_2018, income_quartile == 4)

quartile_1_2016 <- subset(data_2016, income_quartile == 1)
quartile_2_2016 <- subset(data_2016, income_quartile == 2)
quartile_3_2016 <- subset(data_2016, income_quartile == 3)
quartile_4_2016 <- subset(data_2016, income_quartile == 4)

quartile_1_2014 <- subset(data_2014, income_quartile == 1)
quartile_2_2014 <- subset(data_2014, income_quartile == 2)
quartile_3_2014 <- subset(data_2014, income_quartile == 3)
quartile_4_2014 <- subset(data_2014, income_quartile == 4)

quartile_1_2012 <- subset(data_2012, income_quartile == 1)
quartile_2_2012 <- subset(data_2012, income_quartile == 2)
quartile_3_2012 <- subset(data_2012, income_quartile == 3)
quartile_4_2012 <- subset(data_2012, income_quartile == 4)

quartile_1_2010 <- subset(data_2010, income_quartile == 1)
quartile_2_2010 <- subset(data_2010, income_quartile == 2)
quartile_3_2010 <- subset(data_2010, income_quartile == 3)
quartile_4_2010 <- subset(data_2010, income_quartile == 4)

#### 2020 Analysis ####
# compare quartile 1 and 2
quartile_1_2020$treatment = 1
quartile_2_2020$treatment = 0

quartile_1_2_compare_2020 <- rbind(quartile_1_2020, quartile_2_2020)

# Perform propensity score matching
m.out <- matchit(treatment ~ gender + pot_work_years 
                 + city_hukou + is_married + is_party + is_east, 
                 data = quartile_1_2_compare_2020, method = "nearest",
                 distance = "logit", caliper = 0.1, ratio = 2)

# Check the matching result
summary(m.out)

# arriving at the matched sample
matched_data <- match.data(m.out)

# using the Mincer Equation to fit both groups in the matched sample
lm_fit_q1 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years + exp2
                + city_hukou + is_married + gender + is_east
                + is_party, data = matched_data, subset = income_quartile == 1)
lm_fit_q2 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years + exp2
                + city_hukou + is_married + gender + is_east
                + is_party, data = matched_data, subset = income_quartile == 2)

# Display the result
summary(lm_fit_q1)
summary(lm_fit_q2)

# Save Models
saveRDS(
  lm_fit_q1,
  file = "models/edu_returns_2020_q1_q1vsq2.rds"
)

saveRDS(
  lm_fit_q2,
  file = "models/edu_returns_2020_q2_q1vsq2.rds"
)


# quartile 1 vs quartile 3
quartile_1_2020$treatment = 1
quartile_3_2020$treatment = 0

quartile_1_3_compare_2020 <- rbind(quartile_1_2020, quartile_3_2020)

# Perform propensity score matching
m.out2 <- matchit(treatment ~ gender + pot_work_years 
                  + city_hukou + is_married + is_party + is_east, 
                  data = quartile_1_3_compare_2020, method = "nearest",
                  distance = "logit", caliper = 0.1, ratio = 3)

# Check the matching result
summary(m.out2)

# arriving at the matched sample
matched_data2 <- match.data(m.out2)

# using the Mincer Equation to fit both groups in the matched sample
lm_fit_q13 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years + exp2
                 + city_hukou + is_married + gender + is_east
                 + is_party, data = matched_data2, subset = income_quartile == 1)
lm_fit_q31 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years + exp2
                 + city_hukou + is_married + gender + is_east
                 + is_party, data = matched_data2, subset = income_quartile == 3)

# Display the result
summary(lm_fit_q13)
summary(lm_fit_q31)

# Save Models
saveRDS(
  lm_fit_q13,
  file = "models/edu_returns_2020_q1_q1vsq3.rds"
)

saveRDS(
  lm_fit_q31,
  file = "models/edu_returns_2020_q3_q1vsq3.rds"
)


# quartile 1 vs quartile 4
quartile_1_2020$treatment = 1
quartile_4_2020$treatment = 0

quartile_1_4_compare_2020 <- rbind(quartile_1_2020, quartile_4_2020)

# Perform propensity score matching
m.out3 <- matchit(treatment ~ gender + pot_work_years 
                  + city_hukou + is_married + is_party + is_east, 
                  data = quartile_1_4_compare_2020, method = "nearest",
                  distance = "logit", caliper = 0.1, ratio = 3)

# Check the matching result
summary(m.out3)

# arriving at the matched sample
matched_data3 <- match.data(m.out3)

# using the Mincer Equation to fit both groups in the matched sample
lm_fit_q14 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years + exp2
                 + city_hukou + is_married + gender + is_east
                 + is_party, data = matched_data3, subset = income_quartile == 1)
lm_fit_q41 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years + exp2
                 + city_hukou + is_married + gender + is_east
                 + is_party, data = matched_data3, subset = income_quartile == 4)

# Display the result
summary(lm_fit_q14)
summary(lm_fit_q41)

# Save Models
saveRDS(
  lm_fit_q14,
  file = "models/edu_returns_2020_q1_q1vsq4.rds"
)

saveRDS(
  lm_fit_q41,
  file = "models/edu_returns_2020_q4_q1vsq4.rds"
)



# quartile 2 vs quartile 3
quartile_2_2020$treatment = 1
quartile_3_2020$treatment = 0

quartile_2_3_compare_2020 <- rbind(quartile_2_2020, quartile_3_2020)

# Perform propensity score matching
m.out4 <- matchit(treatment ~ gender + pot_work_years 
                  + city_hukou + is_married + is_party + is_east, 
                  data = quartile_2_3_compare_2020, method = "nearest",
                  distance = "logit", caliper = 0.1, ratio = 3)

# Check the matching result
summary(m.out4)

# arriving at the matched sample
matched_data4 <- match.data(m.out4)

# using the Mincer Equation to fit both groups in the matched sample
lm_fit_q23 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years + exp2
                 + city_hukou + is_married + gender + is_east
                 + is_party, data = matched_data4, subset = income_quartile == 2)
lm_fit_q32 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years + exp2
                 + city_hukou + is_married + gender + is_east
                 + is_party, data = matched_data4, subset = income_quartile == 3)

# Display the result
summary(lm_fit_q23)
summary(lm_fit_q32)

# Save Models
saveRDS(
  lm_fit_q23,
  file = "models/edu_returns_2020_q2_q2vsq3.rds"
)

saveRDS(
  lm_fit_q32,
  file = "models/edu_returns_2020_q3_q2vsq3.rds"
)



# quartile 2 vs quartile 4
quartile_2_2020$treatment = 1
quartile_4_2020$treatment = 0

quartile_2_4_compare_2020 <- rbind(quartile_2_2020, quartile_4_2020)

# Perform propensity score matching
m.out5 <- matchit(treatment ~ gender + pot_work_years 
                  + city_hukou + is_married + is_party + is_east, 
                  data = quartile_2_4_compare_2020, method = "nearest",
                  distance = "logit", caliper = 0.1, ratio = 3)

# Check the matching result
summary(m.out5)

# arriving at the matched sample
matched_data5 <- match.data(m.out5)

# using the Mincer Equation to fit both groups in the matched sample
lm_fit_q24 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years + exp2
                 + city_hukou + is_married + gender + is_east
                 + is_party, data = matched_data5, subset = income_quartile == 2)
lm_fit_q42 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years + exp2
                 + city_hukou + is_married + gender + is_east
                 + is_party, data = matched_data5, subset = income_quartile == 4)

# Display the result
summary(lm_fit_q24)
summary(lm_fit_q42)

# Save Models
saveRDS(
  lm_fit_q24,
  file = "models/edu_returns_2020_q2_q2vsq4.rds"
)

saveRDS(
  lm_fit_q42,
  file = "models/edu_returns_2020_q4_q2vsq4.rds"
)


# quartile 3 vs 4
quartile_3_2020$treatment = 1
quartile_4_2020$treatment = 0

quartile_3_4_compare_2020 <- rbind(quartile_3_2020, quartile_4_2020)

# Perform propensity score matching
m.out6 <- matchit(treatment ~ gender + pot_work_years 
                  + city_hukou + is_married + is_party + is_east, 
                  data = quartile_3_4_compare_2020, method = "nearest",
                  distance = "logit", caliper = 0.1, ratio = 3)

# Check the matching result
summary(m.out6)

# arriving at the matched sample
matched_data6 <- match.data(m.out6)

# using the Mincer Equation to fit both groups in the matched sample
lm_fit_q34 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years + exp2
                 + city_hukou + is_married + gender + is_east
                 + is_party, data = matched_data6, subset = income_quartile == 3)
lm_fit_q43 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years + exp2
                 + city_hukou + is_married + gender + is_east
                 + is_party, data = matched_data6, subset = income_quartile == 4)

# Display the result
summary(lm_fit_q34)
summary(lm_fit_q43)

# Save Models
saveRDS(
  lm_fit_q34,
  file = "models/edu_returns_2020_q3_q3vsq4.rds"
)

saveRDS(
  lm_fit_q43,
  file = "models/edu_returns_2020_q4_q3vsq4.rds"
)


#### 2018 Analysis ####
# compare quartile 1 and 2
quartile_1_2018$treatment = 1
quartile_2_2018$treatment = 0

quartile_1_2_compare_2018 <- rbind(quartile_1_2018, quartile_2_2018)

# Perform propensity score matching
m.out <- matchit(treatment ~ gender + pot_work_years 
                 + city_hukou + is_married + is_party + is_east, 
                 data = quartile_1_2_compare_2018, method = "nearest",
                 distance = "logit", caliper = 0.1, ratio = 2)

# Check the matching result
summary(m.out)

# arriving at the matched sample
matched_data <- match.data(m.out)

# using the Mincer Equation to fit both groups in the matched sample
lm_fit_q1 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years + exp2
                + city_hukou + is_married + gender + is_east
                + is_party, data = matched_data, subset = income_quartile == 1)
lm_fit_q2 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years + exp2
                + city_hukou + is_married + gender + is_east
                + is_party, data = matched_data, subset = income_quartile == 2)

# Display the result
summary(lm_fit_q1)
summary(lm_fit_q2)

# Save Models
saveRDS(
  lm_fit_q1,
  file = "models/edu_returns_2018_q1_q1vsq2.rds"
)

saveRDS(
  lm_fit_q2,
  file = "models/edu_returns_2018_q2_q1vsq2.rds"
)


# quartile 1 vs quartile 3
quartile_1_2018$treatment = 1
quartile_3_2018$treatment = 0

quartile_1_3_compare_2018 <- rbind(quartile_1_2018, quartile_3_2018)

# Perform propensity score matching
m.out2 <- matchit(treatment ~ gender + pot_work_years 
                  + city_hukou + is_married + is_party + is_east, 
                  data = quartile_1_3_compare_2018, method = "nearest",
                  distance = "logit", caliper = 0.1, ratio = 3)

# Check the matching result
summary(m.out2)

# arriving at the matched sample
matched_data2 <- match.data(m.out2)

# using the Mincer Equation to fit both groups in the matched sample
lm_fit_q13 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years + exp2
                 + city_hukou + is_married + gender + is_east
                 + is_party, data = matched_data2, subset = income_quartile == 1)
lm_fit_q31 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years + exp2
                 + city_hukou + is_married + gender + is_east
                 + is_party, data = matched_data2, subset = income_quartile == 3)

# Display the result
summary(lm_fit_q13)
summary(lm_fit_q31)

# Save Models
saveRDS(
  lm_fit_q13,
  file = "models/edu_returns_2018_q1_q1vsq3.rds"
)

saveRDS(
  lm_fit_q31,
  file = "models/edu_returns_2018_q3_q1vsq3.rds"
)


# quartile 1 vs quartile 4
quartile_1_2018$treatment = 1
quartile_4_2018$treatment = 0

quartile_1_4_compare_2018 <- rbind(quartile_1_2018, quartile_4_2018)

# Perform propensity score matching
m.out3 <- matchit(treatment ~ gender + pot_work_years 
                  + city_hukou + is_married + is_party + is_east, 
                  data = quartile_1_4_compare_2018, method = "nearest",
                  distance = "logit", caliper = 0.1, ratio = 3)

# Check the matching result
summary(m.out3)

# arriving at the matched sample
matched_data3 <- match.data(m.out3)

# using the Mincer Equation to fit both groups in the matched sample
lm_fit_q14 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years + exp2
                 + city_hukou + is_married + gender + is_east
                 + is_party, data = matched_data3, subset = income_quartile == 1)
lm_fit_q41 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years + exp2
                 + city_hukou + is_married + gender + is_east
                 + is_party, data = matched_data3, subset = income_quartile == 4)

# Display the result
summary(lm_fit_q14)
summary(lm_fit_q41)

# Save Models
saveRDS(
  lm_fit_q14,
  file = "models/edu_returns_2018_q1_q1vsq4.rds"
)

saveRDS(
  lm_fit_q41,
  file = "models/edu_returns_2018_q4_q1vsq4.rds"
)



# quartile 2 vs quartile 3
quartile_2_2018$treatment = 1
quartile_3_2018$treatment = 0

quartile_2_3_compare_2018 <- rbind(quartile_2_2018, quartile_3_2018)

# Perform propensity score matching
m.out4 <- matchit(treatment ~ gender + pot_work_years 
                  + city_hukou + is_married + is_party + is_east, 
                  data = quartile_2_3_compare_2018, method = "nearest",
                  distance = "logit", caliper = 0.1, ratio = 3)

# Check the matching result
summary(m.out4)

# arriving at the matched sample
matched_data4 <- match.data(m.out4)

# using the Mincer Equation to fit both groups in the matched sample
lm_fit_q23 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years + exp2
                 + city_hukou + is_married + gender + is_east
                 + is_party, data = matched_data4, subset = income_quartile == 2)
lm_fit_q32 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years + exp2
                 + city_hukou + is_married + gender + is_east
                 + is_party, data = matched_data4, subset = income_quartile == 3)

# Display the result
summary(lm_fit_q23)
summary(lm_fit_q32)

# Save Models
saveRDS(
  lm_fit_q23,
  file = "models/edu_returns_2018_q2_q2vsq3.rds"
)

saveRDS(
  lm_fit_q32,
  file = "models/edu_returns_2018_q3_q2vsq3.rds"
)



# quartile 2 vs quartile 4
quartile_2_2018$treatment = 1
quartile_4_2018$treatment = 0

quartile_2_4_compare_2018 <- rbind(quartile_2_2018, quartile_4_2018)

# Perform propensity score matching
m.out5 <- matchit(treatment ~ gender + pot_work_years 
                  + city_hukou + is_married + is_party + is_east, 
                  data = quartile_2_4_compare_2018, method = "nearest",
                  distance = "logit", caliper = 0.1, ratio = 3)

# Check the matching result
summary(m.out5)

# arriving at the matched sample
matched_data5 <- match.data(m.out5)

# using the Mincer Equation to fit both groups in the matched sample
lm_fit_q24 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years + exp2
                 + city_hukou + is_married + gender + is_east
                 + is_party, data = matched_data5, subset = income_quartile == 2)
lm_fit_q42 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years + exp2
                 + city_hukou + is_married + gender + is_east
                 + is_party, data = matched_data5, subset = income_quartile == 4)

# Display the result
summary(lm_fit_q24)
summary(lm_fit_q42)

# Save Models
saveRDS(
  lm_fit_q24,
  file = "models/edu_returns_2018_q2_q2vsq4.rds"
)

saveRDS(
  lm_fit_q42,
  file = "models/edu_returns_2018_q4_q2vsq4.rds"
)


# quartile 3 vs 4
quartile_3_2018$treatment = 1
quartile_4_2018$treatment = 0

quartile_3_4_compare_2018 <- rbind(quartile_3_2018, quartile_4_2018)

# Perform propensity score matching
m.out6 <- matchit(treatment ~ gender + pot_work_years 
                  + city_hukou + is_married + is_party + is_east, 
                  data = quartile_3_4_compare_2018, method = "nearest",
                  distance = "logit", caliper = 0.1, ratio = 3)

# Check the matching result
summary(m.out6)

# arriving at the matched sample
matched_data6 <- match.data(m.out6)

# using the Mincer Equation to fit both groups in the matched sample
lm_fit_q34 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years + exp2
                 + city_hukou + is_married + gender + is_east
                 + is_party, data = matched_data6, subset = income_quartile == 3)
lm_fit_q43 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years + exp2
                 + city_hukou + is_married + gender + is_east
                 + is_party, data = matched_data6, subset = income_quartile == 4)

# Display the result
summary(lm_fit_q34)
summary(lm_fit_q43)

# Save Models
saveRDS(
  lm_fit_q34,
  file = "models/edu_returns_2018_q3_q3vsq4.rds"
)

saveRDS(
  lm_fit_q43,
  file = "models/edu_returns_2018_q4_q3vsq4.rds"
)

#### 2016 Analysis ####
# compare quartile 1 and 2
quartile_1_2016$treatment = 1
quartile_2_2016$treatment = 0

quartile_1_2_compare_2016 <- rbind(quartile_1_2016, quartile_2_2016)

# Perform propensity score matching
m.out <- matchit(treatment ~ gender + pot_work_years 
                 + city_hukou + is_married + is_party + is_east, 
                 data = quartile_1_2_compare_2016, method = "nearest",
                 distance = "logit", caliper = 0.1, ratio = 2)

# Check the matching result
summary(m.out)

# arriving at the matched sample
matched_data <- match.data(m.out)

# using the Mincer Equation to fit both groups in the matched sample
lm_fit_q1 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years + exp2
                + city_hukou + is_married + gender + is_east
                + is_party, data = matched_data, subset = income_quartile == 1)
lm_fit_q2 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years + exp2
                + city_hukou + is_married + gender + is_east
                + is_party, data = matched_data, subset = income_quartile == 2)

# Display the result
summary(lm_fit_q1)
summary(lm_fit_q2)

# Save Models
saveRDS(
  lm_fit_q1,
  file = "models/edu_returns_2016_q1_q1vsq2.rds"
)

saveRDS(
  lm_fit_q2,
  file = "models/edu_returns_2016_q2_q1vsq2.rds"
)


# quartile 1 vs quartile 3
quartile_1_2016$treatment = 1
quartile_3_2016$treatment = 0

quartile_1_3_compare_2016 <- rbind(quartile_1_2016, quartile_3_2016)

# Perform propensity score matching
m.out2 <- matchit(treatment ~ gender + pot_work_years 
                  + city_hukou + is_married + is_party + is_east, 
                  data = quartile_1_3_compare_2016, method = "nearest",
                  distance = "logit", caliper = 0.1, ratio = 3)

# Check the matching result
summary(m.out2)

# arriving at the matched sample
matched_data2 <- match.data(m.out2)

# using the Mincer Equation to fit both groups in the matched sample
lm_fit_q13 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years + exp2
                 + city_hukou + is_married + gender + is_east
                 + is_party, data = matched_data2, subset = income_quartile == 1)
lm_fit_q31 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years + exp2
                 + city_hukou + is_married + gender + is_east
                 + is_party, data = matched_data2, subset = income_quartile == 3)

# Display the result
summary(lm_fit_q13)
summary(lm_fit_q31)

# Save Models
saveRDS(
  lm_fit_q13,
  file = "models/edu_returns_2016_q1_q1vsq3.rds"
)

saveRDS(
  lm_fit_q31,
  file = "models/edu_returns_2016_q3_q1vsq3.rds"
)


# quartile 1 vs quartile 4
quartile_1_2016$treatment = 1
quartile_4_2016$treatment = 0

quartile_1_4_compare_2016 <- rbind(quartile_1_2016, quartile_4_2016)

# Perform propensity score matching
m.out3 <- matchit(treatment ~ gender + pot_work_years 
                  + city_hukou + is_married + is_party + is_east, 
                  data = quartile_1_4_compare_2016, method = "nearest",
                  distance = "logit", caliper = 0.1, ratio = 3)

# Check the matching result
summary(m.out3)

# arriving at the matched sample
matched_data3 <- match.data(m.out3)

# using the Mincer Equation to fit both groups in the matched sample
lm_fit_q14 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years + exp2
                 + city_hukou + is_married + gender + is_east
                 + is_party, data = matched_data3, subset = income_quartile == 1)
lm_fit_q41 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years + exp2
                 + city_hukou + is_married + gender + is_east
                 + is_party, data = matched_data3, subset = income_quartile == 4)

# Display the result
summary(lm_fit_q14)
summary(lm_fit_q41)

# Save Models
saveRDS(
  lm_fit_q14,
  file = "models/edu_returns_2016_q1_q1vsq4.rds"
)

saveRDS(
  lm_fit_q41,
  file = "models/edu_returns_2016_q4_q1vsq4.rds"
)



# quartile 2 vs quartile 3
quartile_2_2016$treatment = 1
quartile_3_2016$treatment = 0

quartile_2_3_compare_2016 <- rbind(quartile_2_2016, quartile_3_2016)

# Perform propensity score matching
m.out4 <- matchit(treatment ~ gender + pot_work_years 
                  + city_hukou + is_married + is_party + is_east, 
                  data = quartile_2_3_compare_2016, method = "nearest",
                  distance = "logit", caliper = 0.1, ratio = 3)

# Check the matching result
summary(m.out4)

# arriving at the matched sample
matched_data4 <- match.data(m.out4)

# using the Mincer Equation to fit both groups in the matched sample
lm_fit_q23 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years + exp2
                 + city_hukou + is_married + gender + is_east
                 + is_party, data = matched_data4, subset = income_quartile == 2)
lm_fit_q32 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years + exp2
                 + city_hukou + is_married + gender + is_east
                 + is_party, data = matched_data4, subset = income_quartile == 3)

# Display the result
summary(lm_fit_q23)
summary(lm_fit_q32)

# Save Models
saveRDS(
  lm_fit_q23,
  file = "models/edu_returns_2016_q2_q2vsq3.rds"
)

saveRDS(
  lm_fit_q32,
  file = "models/edu_returns_2016_q3_q2vsq3.rds"
)



# quartile 2 vs quartile 4
quartile_2_2016$treatment = 1
quartile_4_2016$treatment = 0

quartile_2_4_compare_2016 <- rbind(quartile_2_2016, quartile_4_2016)

# Perform propensity score matching
m.out5 <- matchit(treatment ~ gender + pot_work_years 
                  + city_hukou + is_married + is_party + is_east, 
                  data = quartile_2_4_compare_2016, method = "nearest",
                  distance = "logit", caliper = 0.1, ratio = 3)

# Check the matching result
summary(m.out5)

# arriving at the matched sample
matched_data5 <- match.data(m.out5)

# using the Mincer Equation to fit both groups in the matched sample
lm_fit_q24 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years + exp2
                 + city_hukou + is_married + gender + is_east
                 + is_party, data = matched_data5, subset = income_quartile == 2)
lm_fit_q42 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years + exp2
                 + city_hukou + is_married + gender + is_east
                 + is_party, data = matched_data5, subset = income_quartile == 4)

# Display the result
summary(lm_fit_q24)
summary(lm_fit_q42)

# Save Models
saveRDS(
  lm_fit_q24,
  file = "models/edu_returns_2016_q2_q2vsq4.rds"
)

saveRDS(
  lm_fit_q42,
  file = "models/edu_returns_2016_q4_q2vsq4.rds"
)


# quartile 3 vs 4
quartile_3_2016$treatment = 1
quartile_4_2016$treatment = 0

quartile_3_4_compare_2016 <- rbind(quartile_3_2016, quartile_4_2016)

# Perform propensity score matching
m.out6 <- matchit(treatment ~ gender + pot_work_years 
                  + city_hukou + is_married + is_party + is_east, 
                  data = quartile_3_4_compare_2016, method = "nearest",
                  distance = "logit", caliper = 0.1, ratio = 3)

# Check the matching result
summary(m.out6)

# arriving at the matched sample
matched_data6 <- match.data(m.out6)

# using the Mincer Equation to fit both groups in the matched sample
lm_fit_q34 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years + exp2
                 + city_hukou + is_married + gender + is_east
                 + is_party, data = matched_data6, subset = income_quartile == 3)
lm_fit_q43 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years + exp2
                 + city_hukou + is_married + gender + is_east
                 + is_party, data = matched_data6, subset = income_quartile == 4)

# Display the result
summary(lm_fit_q34)
summary(lm_fit_q43)

# Save Models
saveRDS(
  lm_fit_q34,
  file = "models/edu_returns_2016_q3_q3vsq4.rds"
)

saveRDS(
  lm_fit_q43,
  file = "models/edu_returns_2016_q4_q3vsq4.rds"
)


#### 2014 Analysis ####
# compare quartile 1 and 2
quartile_1_2014$treatment = 1
quartile_2_2014$treatment = 0

quartile_1_2_compare_2014 <- rbind(quartile_1_2014, quartile_2_2014)

# Perform propensity score matching
m.out <- matchit(treatment ~ gender + pot_work_years 
                 + city_hukou + is_married + is_party + is_east, 
                 data = quartile_1_2_compare_2014, method = "nearest",
                 distance = "logit", caliper = 0.1, ratio = 2)

# Check the matching result
summary(m.out)

# arriving at the matched sample
matched_data <- match.data(m.out)

# using the Mincer Equation to fit both groups in the matched sample
lm_fit_q1 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years + exp2
                + city_hukou + is_married + gender + is_east
                + is_party, data = matched_data, subset = income_quartile == 1)
lm_fit_q2 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years + exp2
                + city_hukou + is_married + gender + is_east
                + is_party, data = matched_data, subset = income_quartile == 2)

# Display the result
summary(lm_fit_q1)
summary(lm_fit_q2)

# Save Models
saveRDS(
  lm_fit_q1,
  file = "models/edu_returns_2014_q1_q1vsq2.rds"
)

saveRDS(
  lm_fit_q2,
  file = "models/edu_returns_2014_q2_q1vsq2.rds"
)


# quartile 1 vs quartile 3
quartile_1_2014$treatment = 1
quartile_3_2014$treatment = 0

quartile_1_3_compare_2014 <- rbind(quartile_1_2014, quartile_3_2014)

# Perform propensity score matching
m.out2 <- matchit(treatment ~ gender + pot_work_years 
                  + city_hukou + is_married + is_party + is_east, 
                  data = quartile_1_3_compare_2014, method = "nearest",
                  distance = "logit", caliper = 0.1, ratio = 3)

# Check the matching result
summary(m.out2)

# arriving at the matched sample
matched_data2 <- match.data(m.out2)

# using the Mincer Equation to fit both groups in the matched sample
lm_fit_q13 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years + exp2
                 + city_hukou + is_married + gender + is_east
                 + is_party, data = matched_data2, subset = income_quartile == 1)
lm_fit_q31 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years + exp2
                 + city_hukou + is_married + gender + is_east
                 + is_party, data = matched_data2, subset = income_quartile == 3)

# Display the result
summary(lm_fit_q13)
summary(lm_fit_q31)

# Save Models
saveRDS(
  lm_fit_q13,
  file = "models/edu_returns_2014_q1_q1vsq3.rds"
)

saveRDS(
  lm_fit_q31,
  file = "models/edu_returns_2014_q3_q1vsq3.rds"
)


# quartile 1 vs quartile 4
quartile_1_2014$treatment = 1
quartile_4_2014$treatment = 0

quartile_1_4_compare_2014 <- rbind(quartile_1_2014, quartile_4_2014)

# Perform propensity score matching
m.out3 <- matchit(treatment ~ gender + pot_work_years 
                  + city_hukou + is_married + is_party + is_east, 
                  data = quartile_1_4_compare_2014, method = "nearest",
                  distance = "logit", caliper = 0.1, ratio = 3)

# Check the matching result
summary(m.out3)

# arriving at the matched sample
matched_data3 <- match.data(m.out3)

# using the Mincer Equation to fit both groups in the matched sample
lm_fit_q14 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years + exp2
                 + city_hukou + is_married + gender + is_east
                 + is_party, data = matched_data3, subset = income_quartile == 1)
lm_fit_q41 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years + exp2
                 + city_hukou + is_married + gender + is_east
                 + is_party, data = matched_data3, subset = income_quartile == 4)

# Display the result
summary(lm_fit_q14)
summary(lm_fit_q41)

# Save Models
saveRDS(
  lm_fit_q14,
  file = "models/edu_returns_2014_q1_q1vsq4.rds"
)

saveRDS(
  lm_fit_q41,
  file = "models/edu_returns_2014_q4_q1vsq4.rds"
)



# quartile 2 vs quartile 3
quartile_2_2014$treatment = 1
quartile_3_2014$treatment = 0

quartile_2_3_compare_2014 <- rbind(quartile_2_2014, quartile_3_2014)

# Perform propensity score matching
m.out4 <- matchit(treatment ~ gender + pot_work_years 
                  + city_hukou + is_married + is_party + is_east, 
                  data = quartile_2_3_compare_2014, method = "nearest",
                  distance = "logit", caliper = 0.1, ratio = 3)

# Check the matching result
summary(m.out4)

# arriving at the matched sample
matched_data4 <- match.data(m.out4)

# using the Mincer Equation to fit both groups in the matched sample
lm_fit_q23 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years + exp2
                 + city_hukou + is_married + gender + is_east
                 + is_party, data = matched_data4, subset = income_quartile == 2)
lm_fit_q32 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years + exp2
                 + city_hukou + is_married + gender + is_east
                 + is_party, data = matched_data4, subset = income_quartile == 3)

# Display the result
summary(lm_fit_q23)
summary(lm_fit_q32)

# Save Models
saveRDS(
  lm_fit_q23,
  file = "models/edu_returns_2014_q2_q2vsq3.rds"
)

saveRDS(
  lm_fit_q32,
  file = "models/edu_returns_2014_q3_q2vsq3.rds"
)



# quartile 2 vs quartile 4
quartile_2_2014$treatment = 1
quartile_4_2014$treatment = 0

quartile_2_4_compare_2014 <- rbind(quartile_2_2014, quartile_4_2014)

# Perform propensity score matching
m.out5 <- matchit(treatment ~ gender + pot_work_years 
                  + city_hukou + is_married + is_party + is_east, 
                  data = quartile_2_4_compare_2014, method = "nearest",
                  distance = "logit", caliper = 0.1, ratio = 3)

# Check the matching result
summary(m.out5)

# arriving at the matched sample
matched_data5 <- match.data(m.out5)

# using the Mincer Equation to fit both groups in the matched sample
lm_fit_q24 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years + exp2
                 + city_hukou + is_married + gender + is_east
                 + is_party, data = matched_data5, subset = income_quartile == 2)
lm_fit_q42 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years + exp2
                 + city_hukou + is_married + gender + is_east
                 + is_party, data = matched_data5, subset = income_quartile == 4)

# Display the result
summary(lm_fit_q24)
summary(lm_fit_q42)

# Save Models
saveRDS(
  lm_fit_q24,
  file = "models/edu_returns_2014_q2_q2vsq4.rds"
)

saveRDS(
  lm_fit_q42,
  file = "models/edu_returns_2014_q4_q2vsq4.rds"
)


# quartile 3 vs 4
quartile_3_2014$treatment = 1
quartile_4_2014$treatment = 0

quartile_3_4_compare_2014 <- rbind(quartile_3_2014, quartile_4_2014)

# Perform propensity score matching
m.out6 <- matchit(treatment ~ gender + pot_work_years 
                  + city_hukou + is_married + is_party + is_east, 
                  data = quartile_3_4_compare_2014, method = "nearest",
                  distance = "logit", caliper = 0.1, ratio = 3)

# Check the matching result
summary(m.out6)

# arriving at the matched sample
matched_data6 <- match.data(m.out6)

# using the Mincer Equation to fit both groups in the matched sample
lm_fit_q34 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years + exp2
                 + city_hukou + is_married + gender + is_east
                 + is_party, data = matched_data6, subset = income_quartile == 3)
lm_fit_q43 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years + exp2
                 + city_hukou + is_married + gender + is_east
                 + is_party, data = matched_data6, subset = income_quartile == 4)

# Display the result
summary(lm_fit_q34)
summary(lm_fit_q43)

# Save Models
saveRDS(
  lm_fit_q34,
  file = "models/edu_returns_2014_q3_q3vsq4.rds"
)

saveRDS(
  lm_fit_q43,
  file = "models/edu_returns_2014_q4_q3vsq4.rds"
)

#### 2012 Analysis ####
# compare quartile 1 and 2
quartile_1_2012$treatment = 1
quartile_2_2012$treatment = 0

quartile_1_2_compare_2012 <- rbind(quartile_1_2012, quartile_2_2012)

# Perform propensity score matching
m.out <- matchit(treatment ~ gender + pot_work_years 
                 + city_hukou + is_married + is_party + is_east, 
                 data = quartile_1_2_compare_2012, method = "nearest",
                 distance = "logit", caliper = 0.1, ratio = 2)

# Check the matching result
summary(m.out)

# arriving at the matched sample
matched_data <- match.data(m.out)

# using the Mincer Equation to fit both groups in the matched sample
lm_fit_q1 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years + exp2
                + city_hukou + is_married + gender + is_east
                + is_party, data = matched_data, subset = income_quartile == 1)
lm_fit_q2 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years + exp2
                + city_hukou + is_married + gender + is_east
                + is_party, data = matched_data, subset = income_quartile == 2)

# Display the result
summary(lm_fit_q1)
summary(lm_fit_q2)

# Save Models
saveRDS(
  lm_fit_q1,
  file = "models/edu_returns_2012_q1_q1vsq2.rds"
)

saveRDS(
  lm_fit_q2,
  file = "models/edu_returns_2012_q2_q1vsq2.rds"
)


# quartile 1 vs quartile 3
quartile_1_2012$treatment = 1
quartile_3_2012$treatment = 0

quartile_1_3_compare_2012 <- rbind(quartile_1_2012, quartile_3_2012)

# Perform propensity score matching
m.out2 <- matchit(treatment ~ gender + pot_work_years 
                  + city_hukou + is_married + is_party + is_east, 
                  data = quartile_1_3_compare_2012, method = "nearest",
                  distance = "logit", caliper = 0.1, ratio = 3)

# Check the matching result
summary(m.out2)

# arriving at the matched sample
matched_data2 <- match.data(m.out2)

# using the Mincer Equation to fit both groups in the matched sample
lm_fit_q13 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years + exp2
                 + city_hukou + is_married + gender + is_east
                 + is_party, data = matched_data2, subset = income_quartile == 1)
lm_fit_q31 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years + exp2
                 + city_hukou + is_married + gender + is_east
                 + is_party, data = matched_data2, subset = income_quartile == 3)

# Display the result
summary(lm_fit_q13)
summary(lm_fit_q31)

# Save Models
saveRDS(
  lm_fit_q13,
  file = "models/edu_returns_2012_q1_q1vsq3.rds"
)

saveRDS(
  lm_fit_q31,
  file = "models/edu_returns_2012_q3_q1vsq3.rds"
)


# quartile 1 vs quartile 4
quartile_1_2012$treatment = 1
quartile_4_2012$treatment = 0

quartile_1_4_compare_2012 <- rbind(quartile_1_2012, quartile_4_2012)

# Perform propensity score matching
m.out3 <- matchit(treatment ~ gender + pot_work_years 
                  + city_hukou + is_married + is_party + is_east, 
                  data = quartile_1_4_compare_2012, method = "nearest",
                  distance = "logit", caliper = 0.1, ratio = 3)

# Check the matching result
summary(m.out3)

# arriving at the matched sample
matched_data3 <- match.data(m.out3)

# using the Mincer Equation to fit both groups in the matched sample
lm_fit_q14 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years + exp2
                 + city_hukou + is_married + gender + is_east
                 + is_party, data = matched_data3, subset = income_quartile == 1)
lm_fit_q41 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years + exp2
                 + city_hukou + is_married + gender + is_east
                 + is_party, data = matched_data3, subset = income_quartile == 4)

# Display the result
summary(lm_fit_q14)
summary(lm_fit_q41)

# Save Models
saveRDS(
  lm_fit_q14,
  file = "models/edu_returns_2012_q1_q1vsq4.rds"
)

saveRDS(
  lm_fit_q41,
  file = "models/edu_returns_2012_q4_q1vsq4.rds"
)



# quartile 2 vs quartile 3
quartile_2_2012$treatment = 1
quartile_3_2012$treatment = 0

quartile_2_3_compare_2012 <- rbind(quartile_2_2012, quartile_3_2012)

# Perform propensity score matching
m.out4 <- matchit(treatment ~ gender + pot_work_years 
                  + city_hukou + is_married + is_party + is_east, 
                  data = quartile_2_3_compare_2012, method = "nearest",
                  distance = "logit", caliper = 0.1, ratio = 3)

# Check the matching result
summary(m.out4)

# arriving at the matched sample
matched_data4 <- match.data(m.out4)

# using the Mincer Equation to fit both groups in the matched sample
lm_fit_q23 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years + exp2
                 + city_hukou + is_married + gender + is_east
                 + is_party, data = matched_data4, subset = income_quartile == 2)
lm_fit_q32 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years + exp2
                 + city_hukou + is_married + gender + is_east
                 + is_party, data = matched_data4, subset = income_quartile == 3)

# Display the result
summary(lm_fit_q23)
summary(lm_fit_q32)

# Save Models
saveRDS(
  lm_fit_q23,
  file = "models/edu_returns_2012_q2_q2vsq3.rds"
)

saveRDS(
  lm_fit_q32,
  file = "models/edu_returns_2012_q3_q2vsq3.rds"
)



# quartile 2 vs quartile 4
quartile_2_2012$treatment = 1
quartile_4_2012$treatment = 0

quartile_2_4_compare_2012 <- rbind(quartile_2_2012, quartile_4_2012)

# Perform propensity score matching
m.out5 <- matchit(treatment ~ gender + pot_work_years 
                  + city_hukou + is_married + is_party + is_east, 
                  data = quartile_2_4_compare_2012, method = "nearest",
                  distance = "logit", caliper = 0.1, ratio = 3)

# Check the matching result
summary(m.out5)

# arriving at the matched sample
matched_data5 <- match.data(m.out5)

# using the Mincer Equation to fit both groups in the matched sample
lm_fit_q24 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years + exp2
                 + city_hukou + is_married + gender + is_east
                 + is_party, data = matched_data5, subset = income_quartile == 2)
lm_fit_q42 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years + exp2
                 + city_hukou + is_married + gender + is_east
                 + is_party, data = matched_data5, subset = income_quartile == 4)

# Display the result
summary(lm_fit_q24)
summary(lm_fit_q42)

# Save Models
saveRDS(
  lm_fit_q24,
  file = "models/edu_returns_2012_q2_q2vsq4.rds"
)

saveRDS(
  lm_fit_q42,
  file = "models/edu_returns_2012_q4_q2vsq4.rds"
)


# quartile 3 vs 4
quartile_3_2012$treatment = 1
quartile_4_2012$treatment = 0

quartile_3_4_compare_2012 <- rbind(quartile_3_2012, quartile_4_2012)

# Perform propensity score matching
m.out6 <- matchit(treatment ~ gender + pot_work_years 
                  + city_hukou + is_married + is_party + is_east, 
                  data = quartile_3_4_compare_2012, method = "nearest",
                  distance = "logit", caliper = 0.1, ratio = 3)

# Check the matching result
summary(m.out6)

# arriving at the matched sample
matched_data6 <- match.data(m.out6)

# using the Mincer Equation to fit both groups in the matched sample
lm_fit_q34 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years + exp2
                 + city_hukou + is_married + gender + is_east
                 + is_party, data = matched_data6, subset = income_quartile == 3)
lm_fit_q43 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years + exp2
                 + city_hukou + is_married + gender + is_east
                 + is_party, data = matched_data6, subset = income_quartile == 4)

# Display the result
summary(lm_fit_q34)
summary(lm_fit_q43)

# Save Models
saveRDS(
  lm_fit_q34,
  file = "models/edu_returns_2012_q3_q3vsq4.rds"
)

saveRDS(
  lm_fit_q43,
  file = "models/edu_returns_2012_q4_q3vsq4.rds"
)

#### 2010 Analysis ####
# compare quartile 1 and 2
quartile_1_2010$treatment = 1
quartile_2_2010$treatment = 0

quartile_1_2_compare_2010 <- rbind(quartile_1_2010, quartile_2_2010)

# Perform propensity score matching
m.out <- matchit(treatment ~ gender + pot_work_years 
                 + city_hukou + is_married + is_party + is_east, 
                 data = quartile_1_2_compare_2010, method = "nearest",
                 distance = "logit", caliper = 0.1, ratio = 2)

# Check the matching result
summary(m.out)

# arriving at the matched sample
matched_data <- match.data(m.out)

# using the Mincer Equation to fit both groups in the matched sample
lm_fit_q1 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years + exp2
                + city_hukou + is_married + gender + is_east
                + is_party, data = matched_data, subset = income_quartile == 1)
lm_fit_q2 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years + exp2
                + city_hukou + is_married + gender + is_east
                + is_party, data = matched_data, subset = income_quartile == 2)

# Display the result
summary(lm_fit_q1)
summary(lm_fit_q2)

# Save Models
saveRDS(
  lm_fit_q1,
  file = "models/edu_returns_2010_q1_q1vsq2.rds"
)

saveRDS(
  lm_fit_q2,
  file = "models/edu_returns_2010_q2_q1vsq2.rds"
)


# quartile 1 vs quartile 3
quartile_1_2010$treatment = 1
quartile_3_2010$treatment = 0

quartile_1_3_compare_2010 <- rbind(quartile_1_2010, quartile_3_2010)

# Perform propensity score matching
m.out2 <- matchit(treatment ~ gender + pot_work_years 
                  + city_hukou + is_married + is_party + is_east, 
                  data = quartile_1_3_compare_2010, method = "nearest",
                  distance = "logit", caliper = 0.1, ratio = 3)

# Check the matching result
summary(m.out2)

# arriving at the matched sample
matched_data2 <- match.data(m.out2)

# using the Mincer Equation to fit both groups in the matched sample
lm_fit_q13 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years + exp2
                 + city_hukou + is_married + gender + is_east
                 + is_party, data = matched_data2, subset = income_quartile == 1)
lm_fit_q31 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years + exp2
                 + city_hukou + is_married + gender + is_east
                 + is_party, data = matched_data2, subset = income_quartile == 3)

# Display the result
summary(lm_fit_q13)
summary(lm_fit_q31)

# Save Models
saveRDS(
  lm_fit_q13,
  file = "models/edu_returns_2010_q1_q1vsq3.rds"
)

saveRDS(
  lm_fit_q31,
  file = "models/edu_returns_2010_q3_q1vsq3.rds"
)


# quartile 1 vs quartile 4
quartile_1_2010$treatment = 1
quartile_4_2010$treatment = 0

quartile_1_4_compare_2010 <- rbind(quartile_1_2010, quartile_4_2010)

# Perform propensity score matching
m.out3 <- matchit(treatment ~ gender + pot_work_years 
                  + city_hukou + is_married + is_party + is_east, 
                  data = quartile_1_4_compare_2010, method = "nearest",
                  distance = "logit", caliper = 0.1, ratio = 3)

# Check the matching result
summary(m.out3)

# arriving at the matched sample
matched_data3 <- match.data(m.out3)

# using the Mincer Equation to fit both groups in the matched sample
lm_fit_q14 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years + exp2
                 + city_hukou + is_married + gender + is_east
                 + is_party, data = matched_data3, subset = income_quartile == 1)
lm_fit_q41 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years + exp2
                 + city_hukou + is_married + gender + is_east
                 + is_party, data = matched_data3, subset = income_quartile == 4)

# Display the result
summary(lm_fit_q14)
summary(lm_fit_q41)

# Save Models
saveRDS(
  lm_fit_q14,
  file = "models/edu_returns_2010_q1_q1vsq4.rds"
)

saveRDS(
  lm_fit_q41,
  file = "models/edu_returns_2010_q4_q1vsq4.rds"
)



# quartile 2 vs quartile 3
quartile_2_2010$treatment = 1
quartile_3_2010$treatment = 0

quartile_2_3_compare_2010 <- rbind(quartile_2_2010, quartile_3_2010)

# Perform propensity score matching
m.out4 <- matchit(treatment ~ gender + pot_work_years 
                  + city_hukou + is_married + is_party + is_east, 
                  data = quartile_2_3_compare_2010, method = "nearest",
                  distance = "logit", caliper = 0.1, ratio = 3)

# Check the matching result
summary(m.out4)

# arriving at the matched sample
matched_data4 <- match.data(m.out4)

# using the Mincer Equation to fit both groups in the matched sample
lm_fit_q23 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years + exp2
                 + city_hukou + is_married + gender + is_east
                 + is_party, data = matched_data4, subset = income_quartile == 2)
lm_fit_q32 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years + exp2
                 + city_hukou + is_married + gender + is_east
                 + is_party, data = matched_data4, subset = income_quartile == 3)

# Display the result
summary(lm_fit_q23)
summary(lm_fit_q32)

# Save Models
saveRDS(
  lm_fit_q23,
  file = "models/edu_returns_2010_q2_q2vsq3.rds"
)

saveRDS(
  lm_fit_q32,
  file = "models/edu_returns_2010_q3_q2vsq3.rds"
)



# quartile 2 vs quartile 4
quartile_2_2010$treatment = 1
quartile_4_2010$treatment = 0

quartile_2_4_compare_2010 <- rbind(quartile_2_2010, quartile_4_2010)

# Perform propensity score matching
m.out5 <- matchit(treatment ~ gender + pot_work_years 
                  + city_hukou + is_married + is_party + is_east, 
                  data = quartile_2_4_compare_2010, method = "nearest",
                  distance = "logit", caliper = 0.1, ratio = 3)

# Check the matching result
summary(m.out5)

# arriving at the matched sample
matched_data5 <- match.data(m.out5)

# using the Mincer Equation to fit both groups in the matched sample
lm_fit_q24 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years + exp2
                 + city_hukou + is_married + gender + is_east
                 + is_party, data = matched_data5, subset = income_quartile == 2)
lm_fit_q42 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years + exp2
                 + city_hukou + is_married + gender + is_east
                 + is_party, data = matched_data5, subset = income_quartile == 4)

# Display the result
summary(lm_fit_q24)
summary(lm_fit_q42)

# Save Models
saveRDS(
  lm_fit_q24,
  file = "models/edu_returns_2010_q2_q2vsq4.rds"
)

saveRDS(
  lm_fit_q42,
  file = "models/edu_returns_2010_q4_q2vsq4.rds"
)


# quartile 3 vs 4
quartile_3_2010$treatment = 1
quartile_4_2010$treatment = 0

quartile_3_4_compare_2010 <- rbind(quartile_3_2010, quartile_4_2010)

# Perform propensity score matching
m.out6 <- matchit(treatment ~ gender + pot_work_years 
                  + city_hukou + is_married + is_party + is_east, 
                  data = quartile_3_4_compare_2010, method = "nearest",
                  distance = "logit", caliper = 0.1, ratio = 3)

# Check the matching result
summary(m.out6)

# arriving at the matched sample
matched_data6 <- match.data(m.out6)

# using the Mincer Equation to fit both groups in the matched sample
lm_fit_q34 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years + exp2
                 + city_hukou + is_married + gender + is_east
                 + is_party, data = matched_data6, subset = income_quartile == 3)
lm_fit_q43 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years + exp2
                 + city_hukou + is_married + gender + is_east
                 + is_party, data = matched_data6, subset = income_quartile == 4)

# Display the result
summary(lm_fit_q34)
summary(lm_fit_q43)

# Save Models
saveRDS(
  lm_fit_q34,
  file = "models/edu_returns_2010_q3_q3vsq4.rds"
)

saveRDS(
  lm_fit_q43,
  file = "models/edu_returns_2010_q4_q3vsq4.rds"
)


#### Robustness Testing: Regress without PSM ####

lm_10_1 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years + exp2
              + city_hukou + is_married + gender + is_east
              + is_party, data = quartile_1_2010)
lm_10_2 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years + exp2
              + city_hukou + is_married + gender + is_east
              + is_party, data = quartile_2_2010)
lm_10_3 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years + exp2
              + city_hukou + is_married + gender + is_east
              + is_party, data = quartile_3_2010)
lm_10_4 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years + exp2
              + city_hukou + is_married + gender + is_east
              + is_party, data = quartile_4_2010)

summary(lm_10_1)
summary(lm_10_2)
summary(lm_10_3)
summary(lm_10_4)

saveRDS(
  lm_10_1,
  file = "models/edu_returns_2010_q1.rds"
)
saveRDS(
  lm_10_2,
  file = "models/edu_returns_2010_q2.rds"
)
saveRDS(
  lm_10_3,
  file = "models/edu_returns_2010_q3.rds"
)
saveRDS(
  lm_10_4,
  file = "models/edu_returns_2010_q4.rds"
)

lm_12_1 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years + exp2
              + city_hukou + is_married + gender + is_east
              + is_party, data = quartile_1_2012)
lm_12_2 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years + exp2
              + city_hukou + is_married + gender + is_east
              + is_party, data = quartile_2_2012)
lm_12_3 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years + exp2
              + city_hukou + is_married + gender + is_east
              + is_party, data = quartile_3_2012)
lm_12_4 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years + exp2
              + city_hukou + is_married + gender + is_east
              + is_party, data = quartile_4_2012)

summary(lm_12_1)
summary(lm_12_2)
summary(lm_12_3)
summary(lm_12_4)

saveRDS(
  lm_12_1,
  file = "models/edu_returns_2012_q1.rds"
)
saveRDS(
  lm_12_2,
  file = "models/edu_returns_2012_q2.rds"
)
saveRDS(
  lm_12_3,
  file = "models/edu_returns_2012_q3.rds"
)
saveRDS(
  lm_12_4,
  file = "models/edu_returns_2012_q4.rds"
)

lm_14_1 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years
              + city_hukou + is_married + gender + is_east + exp2
              + is_party, data = quartile_1_2014)
lm_14_2 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years
              + city_hukou + is_married + gender + is_east + exp2
              + is_party, data = quartile_2_2014)
lm_14_3 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years
              + city_hukou + is_married + gender + is_east + exp2
              + is_party, data = quartile_3_2014)
lm_14_4 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years
              + city_hukou + is_married + gender + is_east + exp2
              + is_party, data = quartile_4_2014)

summary(lm_14_1)
summary(lm_14_2)
summary(lm_14_3)
summary(lm_14_4)

saveRDS(
  lm_14_1,
  file = "models/edu_returns_2014_q1.rds"
)
saveRDS(
  lm_14_2,
  file = "models/edu_returns_2014_q2.rds"
)
saveRDS(
  lm_14_3,
  file = "models/edu_returns_2014_q3.rds"
)
saveRDS(
  lm_14_4,
  file = "models/edu_returns_2014_q4.rds"
)


lm_16_1 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years
              + city_hukou + is_married + gender + is_east + exp2
              + is_party, data = quartile_1_2016)
lm_16_2 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years
              + city_hukou + is_married + gender + is_east + exp2
              + is_party, data = quartile_2_2016)
lm_16_3 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years
              + city_hukou + is_married + gender + is_east + exp2
              + is_party, data = quartile_3_2016)
lm_16_4 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years
              + city_hukou + is_married + gender + is_east + exp2
              + is_party, data = quartile_4_2016)

summary(lm_16_1)
summary(lm_16_2)
summary(lm_16_3)
summary(lm_16_4)

saveRDS(
  lm_16_1,
  file = "models/edu_returns_2016_q1.rds"
)
saveRDS(
  lm_16_2,
  file = "models/edu_returns_2016_q2.rds"
)
saveRDS(
  lm_16_3,
  file = "models/edu_returns_2016_q3.rds"
)
saveRDS(
  lm_16_4,
  file = "models/edu_returns_2016_q4.rds"
)


lm_18_1 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years
              + city_hukou + is_married + gender + is_east + exp2
              + is_party, data = quartile_1_2018)
lm_18_2 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years
              + city_hukou + is_married + gender + is_east + exp2
              + is_party, data = quartile_2_2018)
lm_18_3 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years
              + city_hukou + is_married + gender + is_east + exp2
              + is_party, data = quartile_3_2018)
lm_18_4 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years
              + city_hukou + is_married + gender + is_east + exp2
              + is_party, data = quartile_4_2018)

summary(lm_18_1)
summary(lm_18_2)
summary(lm_18_3)
summary(lm_18_4)

saveRDS(
  lm_18_1,
  file = "models/edu_returns_2018_q1.rds"
)
saveRDS(
  lm_18_2,
  file = "models/edu_returns_2018_q2.rds"
)
saveRDS(
  lm_18_3,
  file = "models/edu_returns_2018_q3.rds"
)
saveRDS(
  lm_18_4,
  file = "models/edu_returns_2018_q4.rds"
)

lm_20_1 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years + exp2
              + city_hukou + is_married + gender + is_east
              + is_party, data = quartile_1_2020)
lm_20_2 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years + exp2
              + city_hukou + is_married + gender + is_east
              + is_party, data = quartile_2_2020)
lm_20_3 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years + exp2
              + city_hukou + is_married + gender + is_east
              + is_party, data = quartile_3_2020)
lm_20_4 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years + exp2
              + city_hukou + is_married + gender + is_east
              + is_party, data = quartile_4_2020)

summary(lm_20_1)
summary(lm_20_2)
summary(lm_20_3)
summary(lm_20_4)

saveRDS(
  lm_20_1,
  file = "models/edu_returns_2020_q1.rds"
)
saveRDS(
  lm_20_2,
  file = "models/edu_returns_2020_q2.rds"
)
saveRDS(
  lm_20_3,
  file = "models/edu_returns_2020_q3.rds"
)
saveRDS(
  lm_20_4,
  file = "models/edu_returns_2020_q4.rds"
)
