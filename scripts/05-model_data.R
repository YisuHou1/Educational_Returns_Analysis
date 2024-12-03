#### Preamble ####
# Purpose: Models the educational returns of working individuals in China
# Author: Yisu Hou
# Date: 28 November 2024
# Contact: yisu.hou@mail.utoronto.ca
# License: None
# Pre-requisites:
# - Packages `tidyverse`, `caret`, `glmnet`, `nnet`, `lubridate`, `pROC`,
#   `stringr`, and `gt` must be installed
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


#### Robustness Testing: Regress without PSM ####

lm_10_1 <- lm(ln_hourly_wage ~ cfps2010eduy_best + pot_work_years + exp2
              + city_hukou + is_married + gender + is_east
              + is_party, data = quartile_1_2010)
lm_10_2 <- lm(ln_hourly_wage ~ cfps2010eduy_best + pot_work_years + exp2
              + city_hukou + is_married + gender + is_east
              + is_party, data = quartile_2_2010)
lm_10_3 <- lm(ln_hourly_wage ~ cfps2010eduy_best + pot_work_years + exp2
              + city_hukou + is_married + gender + is_east
              + is_party, data = quartile_3_2010)
lm_10_4 <- lm(ln_hourly_wage ~ cfps2010eduy_best + pot_work_years + exp2
              + city_hukou + is_married + gender + is_east
              + is_party, data = quartile_4_2010)

summary(lm_10_1)
summary(lm_10_2)
summary(lm_10_3)
summary(lm_10_4)

lm_12_1 <- lm(ln_hourly_wage ~ eduy2012 + pot_work_years + exp2
              + city_hukou + is_married + gender + is_east
              + is_party, data = quartile_1_2012)
lm_12_2 <- lm(ln_hourly_wage ~ eduy2012 + pot_work_years + exp2
              + city_hukou + is_married + gender + is_east
              + is_party, data = quartile_2_2012)
lm_12_3 <- lm(ln_hourly_wage ~ eduy2012 + pot_work_years + exp2
              + city_hukou + is_married + gender + is_east
              + is_party, data = quartile_3_2012)
lm_12_4 <- lm(ln_hourly_wage ~ eduy2012 + pot_work_years + exp2
              + city_hukou + is_married + gender + is_east
              + is_party, data = quartile_4_2012)

summary(lm_12_1)
summary(lm_12_2)
summary(lm_12_3)
summary(lm_12_4)


lm_14_1 <- lm(ln_hourly_wage ~ cfps2014eduy + pot_work_years
              + city_hukou + is_married + gender + is_east + exp2
              + is_party, data = quartile_1_2014)
lm_14_2 <- lm(ln_hourly_wage ~ cfps2014eduy + pot_work_years
              + city_hukou + is_married + gender + is_east + exp2
              + is_party, data = quartile_2_2014)
lm_14_3 <- lm(ln_hourly_wage ~ cfps2014eduy + pot_work_years
              + city_hukou + is_married + gender + is_east + exp2
              + is_party, data = quartile_3_2014)
lm_14_4 <- lm(ln_hourly_wage ~ cfps2014eduy + pot_work_years
              + city_hukou + is_married + gender + is_east + exp2
              + is_party, data = quartile_4_2014)

summary(lm_14_1)
summary(lm_14_2)
summary(lm_14_3)
summary(lm_14_4)


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


lm_18_1 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years
              + city_hukou + is_married + GENDER + is_east + exp2
              + is_party, data = quartile_1_2018)
lm_18_2 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years
              + city_hukou + is_married + GENDER + is_east + exp2
              + is_party, data = quartile_2_2018)
lm_18_3 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years
              + city_hukou + is_married + GENDER + is_east + exp2
              + is_party, data = quartile_3_2018)
lm_18_4 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years
              + city_hukou + is_married + GENDER + is_east + exp2
              + is_party, data = quartile_4_2018)

summary(lm_18_1)
summary(lm_18_2)
summary(lm_18_3)
summary(lm_18_4)


lm_20_1 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years + exp2
              + city_hukou + is_married + QA002 + is_east
              + is_party, data = quartile_1_2020)
lm_20_2 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years + exp2
              + city_hukou + is_married + QA002 + is_east
              + is_party, data = quartile_2_2020)
lm_20_3 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years + exp2
              + city_hukou + is_married + QA002 + is_east
              + is_party, data = quartile_3_2020)
lm_20_4 <- lm(ln_hourly_wage ~ years_of_education + pot_work_years + exp2
              + city_hukou + is_married + QA002 + is_east
              + is_party, data = quartile_4_2020)

summary(lm_20_1)
summary(lm_20_2)
summary(lm_20_3)
summary(lm_20_4)


