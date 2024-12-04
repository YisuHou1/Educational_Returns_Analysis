#### Preamble ####
# Purpose: Cleans the raw survey data from CFPS
# Author: Yisu Hou
# Date: 28 November 2024
# Contact: yisu.hou@mail.utoronto.ca
# License: None
# Pre-requisites:
# - Packages `tidyverse`, `janitor`, `arrow`, and `haven must be installed`
# - Raw data downloaded from CFPS and located in data/01-raw_data
# - 00-simulate_data.R and 01-test_simulate_data.R must have been run

#### Workspace setup ####

# Load libraries
library(tidyverse)
library(janitor)
library(haven)
library(arrow)


#### Read data ####
# import raw datasets for all CFPS iterations
# 2020 data
cfps2020person_raw <- read_sas(
  "data/01-raw_data/cfps2020person_202306.sas7bdat",
  NULL
)
cfps2020famecon <- read_sas(
  "data/01-raw_data/cfps2020famecon_202306.sas7bdat",
  NULL
)


# 2018 data
cfps2018person_raw <- read_sas(
  "data/01-raw_data/cfps2018person_202012.sas7bdat",
  NULL
)
cfps2018famecon <- read_sas(
  "data/01-raw_data/cfps2018famecon_202101.sas7bdat",
  NULL
)

# 2016 data
cfps2016person_raw <- read_sas(
  "data/01-raw_data/cfps2016adult_201906.sas7bdat",
  NULL
)
cfps2016famecon <- read_sas(
  "data/01-raw_data/cfps2016famecon_201807.sas7bdat",
  NULL
)

# 2014 data
cfps2014person_raw <- read_sas(
  "data/01-raw_data/cfps2014adult_201906.sas7bdat",
  NULL
)
cfps2014famecon <- read_sas(
  "data/01-raw_data/cfps2014famecon_201906.sas7bdat",
  NULL
)

# 2012 data
cfps2012person_raw <- read_sas(
  "data/01-raw_data/cfps2012adult_201906.sas7bdat",
  NULL
)
cfps2012famecon <- read_sas(
  "data/01-raw_data/cfps2012famecon_201906.sas7bdat",
  NULL
)

# 2010 data
cfps2010person_raw <- read_sas(
  "data/01-raw_data/cfps2010adult_202008.sas7bdat",
  NULL
)
cfps2010famecon <- read_sas(
  "data/01-raw_data/cfps2010famecon_202008.sas7bdat",
  NULL
)

# clean 2020 data
# Join family dataset with individual dataset and select relevant variables
cfps2020person <- cfps2020person_raw |>
  left_join(cfps2020famecon, by = "fid20") |>
  select(W01, age, QG12, QG6, QA002, QA301, QEA0, QN4001, provcd20.x, fincome1_per_p)

# remove full datasets to conserve memory
cfps2020famecon <- NULL
cfps2020person_raw <- NULL


# new varialbe: years of education
cfps2020person <- mutate(cfps2020person, years_of_education = case_when(
  W01 == 0 ~ 0,
  W01 == 10 ~ 0,
  W01 == 3 ~ 6,
  W01 == 4 ~ 9,
  W01 == 5 ~ 12,
  W01 == 6 ~ 15,
  W01 == 7 ~ 16,
  W01 == 8 ~ 19,
  W01 == 9 ~ 22,
))

# new variable: potential work years (years out of school)
# 2020 - year of leaving school
cfps2020person <- mutate(cfps2020person,
  pot_work_years = case_when(
    is.na(age) | age < 0 | years_of_education < 0 ~ NA,
    years_of_education == 0 ~ age - 16,
    years_of_education == 6 ~ age - 16,
    years_of_education == 9 ~ age - 17,
    years_of_education == 12 ~ age - 19,
    years_of_education == 15 ~ age - 22,
    years_of_education == 16 ~ age - 23,
    years_of_education == 19 ~ age - 26,
    years_of_education == 22 ~ age - 29
  )
)

cfps2020person <- mutate(cfps2020person, exp2 = pot_work_years^2)

# ln hourly income
# yearly income/52/weekly work hours
cfps2020person <- mutate(cfps2020person,
  ln_hourly_wage =
    if_else(QG12 <= 0 | QG6 <= 0, NA, log((QG12 / 52.1428571429) / QG6))
)
# city hukou
cfps2020person <- mutate(cfps2020person, city_hukou = case_when(
  QA301 == 1 ~ 0,
  QA301 == 3 ~ 1,
  QA301 == 7 ~ 1,
))

# marrital status
cfps2020person <- mutate(cfps2020person, is_married = case_when(
  QEA0 == 1 ~ 0,
  QEA0 == 2 ~ 1,
  QEA0 == 3 ~ 1,
  QEA0 == 4 ~ 0,
  QEA0 == 5 ~ 0,
))

# party member status
cfps2020person <- mutate(cfps2020person, is_party = case_when(
  QN4001 == 0 ~ 0,
  QN4001 == 1 ~ 1,
))

# east
cfps2020person <- mutate(cfps2020person, is_east = if_else(
  provcd20.x == 11 | provcd20.x == 12 | provcd20.x == 13 | provcd20.x == 31 |
    provcd20.x == 32 | provcd20.x == 33 | provcd20.x == 35 | provcd20.x == 37 |
    provcd20.x == 44 | provcd20.x == 46, 1, 0
))


# Have a subset with no missing values
# Also rename variables for consistency and keep only variables used for
# analysis
cfps2020person_noNA <- cfps2020person |>
  rename(
    gender = QA002,
    income_quartile = fincome1_per_p
  ) |>
  select(
    ln_hourly_wage, years_of_education, gender, pot_work_years, exp2, city_hukou,
    is_married, is_party, is_east, income_quartile
  ) |>
  drop_na()

# Save the dataset
write_parquet(
  cfps2020person_noNA,
  "data/02-analysis_data/2020_analysis_data.parquet"
)


################################################################################
# clean 2018 data
# Reshape family_data
# note that this is only necessary for the 2018 dataset because it is
# structured a bit differently
reshaped_family_data <- cfps2018famecon |>
  gather(key = "member", value = "PID", pid_a_1:pid_a_15) |>
  filter(!is.na(PID)) |>
  select(-member) |>
  group_by(PID) |>
  filter(row_number() == 1)

# Join reshaped family dataset with individual dataset
# and select relevant variables
cfps2018person <- cfps2018person_raw |>
  left_join(reshaped_family_data, by = "PID") |>
  select(W01, AGE, QG12, QG6, GENDER, QA301, QEA0, QN4001, PROVCD18, fincome1_per_p)

# remove full datasets to conserve memory
cfps2018famecon <- NULL
cfps2018person_raw <- NULL
reshaped_family_data <- NULL

# new varialbe: years of education
cfps2018person <- mutate(cfps2018person, years_of_education = case_when(
  W01 == 0 ~ 0,
  W01 == 10 ~ 0,
  W01 == 3 ~ 6,
  W01 == 4 ~ 9,
  W01 == 5 ~ 12,
  W01 == 6 ~ 15,
  W01 == 7 ~ 16,
  W01 == 8 ~ 19,
  W01 == 9 ~ 22,
))

# new variable: years out of school
# 2020 - year of leaving school
cfps2018person <- mutate(cfps2018person, pot_work_years = case_when(
  is.na(AGE) | AGE < 0 | years_of_education < 0 ~ NA,
  years_of_education == 0 ~ AGE - 16,
  years_of_education == 6 ~ AGE - 16,
  years_of_education == 9 ~ AGE - 17,
  years_of_education == 12 ~ AGE - 19,
  years_of_education == 15 ~ AGE - 22,
  years_of_education == 16 ~ AGE - 23,
  years_of_education == 19 ~ AGE - 26,
  years_of_education == 22 ~ AGE - 29
))

cfps2018person <- mutate(cfps2018person, exp2 = pot_work_years^2)


# ln hourly income
# yearly income/52/weekly work hours
cfps2018person <- mutate(cfps2018person,
  ln_hourly_wage =
    if_else(QG12 <= 0 | QG6 <= 0, NA, log((QG12 / 52.1428571429) / QG6))
)
# city hukou
cfps2018person <- mutate(cfps2018person, city_hukou = case_when(
  QA301 == 1 ~ 0,
  QA301 == 3 ~ 1,
  QA301 == 7 ~ 1,
))

# marrital status
cfps2018person <- mutate(cfps2018person, is_married = case_when(
  QEA0 == 1 ~ 0,
  QEA0 == 2 ~ 1,
  QEA0 == 3 ~ 1,
  QEA0 == 4 ~ 0,
  QEA0 == 5 ~ 0,
))

# party member status
cfps2018person <- mutate(cfps2018person, is_party = case_when(
  QN4001 == 0 ~ 0,
  QN4001 == 1 ~ 1,
))

# east
cfps2018person <- mutate(cfps2018person, is_east = if_else(
  PROVCD18 == 11 | PROVCD18 == 12 | PROVCD18 == 13 | PROVCD18 == 31 |
    PROVCD18 == 32 | PROVCD18 == 33 | PROVCD18 == 35 | PROVCD18 == 37 |
    PROVCD18 == 44 | PROVCD18 == 46, 1, 0
))


# Have a subset with no missing values
cfps2018person_noNA <- cfps2018person |>
  rename(
    gender = GENDER,
    income_quartile = fincome1_per_p
  ) |>
  select(
    ln_hourly_wage, years_of_education, gender, pot_work_years, exp2, city_hukou,
    is_married, is_party, is_east, income_quartile
  ) |>
  drop_na()

# Save the dataset
write_parquet(
  cfps2018person_noNA,
  "data/02-analysis_data/2018_analysis_data.parquet"
)


################################################################################
# clean 2016 data
# Join family dataset with individual dataset and select relevant variables
cfps2016person <- cfps2016person_raw |>
  left_join(cfps2016famecon, by = "fid16") |>
  select(cfps2016edu, CFPS_AGE, CFPS_GENDER, QG12, QG6, PA301, QEA0, QN4001, provcd16.x, fincome1_per_p)

# remove full datasets to conserve memory
cfps2016famecon <- NULL
cfps2016person_raw <- NULL


# new varialbe: years of education
cfps2016person <- mutate(cfps2016person, years_of_education = case_when(
  cfps2016edu == 1 ~ 0,
  cfps2016edu == 2 ~ 6,
  cfps2016edu == 3 ~ 9,
  cfps2016edu == 4 ~ 12,
  cfps2016edu == 5 ~ 15,
  cfps2016edu == 6 ~ 16,
  cfps2016edu == 7 ~ 19,
  cfps2016edu == 8 ~ 22,
))

# new variable: years out of school
# 2020 - year of leaving school
cfps2016person <- mutate(cfps2016person, pot_work_years = case_when(
  is.na(CFPS_AGE) | CFPS_AGE < 0 | years_of_education < 0 ~ NA,
  years_of_education == 0 ~ CFPS_AGE - 16,
  years_of_education == 6 ~ CFPS_AGE - 16,
  years_of_education == 9 ~ CFPS_AGE - 17,
  years_of_education == 12 ~ CFPS_AGE - 19,
  years_of_education == 15 ~ CFPS_AGE - 22,
  years_of_education == 16 ~ CFPS_AGE - 23,
  years_of_education == 19 ~ CFPS_AGE - 26,
  years_of_education == 22 ~ CFPS_AGE - 29
))

cfps2016person <- mutate(cfps2016person, exp2 = pot_work_years^2)

# ln hourly income
# yearly income/52/weekly work hours
cfps2016person <- mutate(cfps2016person,
  ln_hourly_wage =
    if_else(QG12 <= 0 | QG6 <= 0, NA, log((QG12 / 52.1428571429) / QG6))
)
# city hukou
cfps2016person <- mutate(cfps2016person, city_hukou = case_when(
  PA301 == 1 ~ 0,
  PA301 == 3 ~ 1
))

# marrital status
cfps2016person <- mutate(cfps2016person, is_married = case_when(
  QEA0 == 1 ~ 0,
  QEA0 == 2 ~ 1,
  QEA0 == 3 ~ 1,
  QEA0 == 4 ~ 0,
  QEA0 == 5 ~ 0
))

# party member status
cfps2016person <- mutate(cfps2016person, is_party = case_when(
  QN4001 == 0 ~ 0,
  QN4001 == 1 ~ 1
))

cfps2016person <- mutate(cfps2016person, gender = case_when(
  CFPS_GENDER == 0 ~ 0,
  CFPS_GENDER == 1 ~ 1
))

# east
cfps2016person <- mutate(cfps2016person, is_east = if_else(
  provcd16.x == 11 | provcd16.x == 12 | provcd16.x == 13 | provcd16.x == 31 |
    provcd16.x == 32 | provcd16.x == 33 | provcd16.x == 35 | provcd16.x == 37 |
    provcd16.x == 44 | provcd16.x == 46, 1, 0
))


# Have a subset with no missing values
cfps2016person_noNA <- cfps2016person |>
  rename(
    income_quartile = fincome1_per_p
  ) |>
  select(
    ln_hourly_wage, years_of_education, gender, pot_work_years, exp2, city_hukou,
    is_married, is_party, is_east, income_quartile
  ) |>
  drop_na()

# Save the dataset
write_parquet(
  cfps2016person_noNA,
  "data/02-analysis_data/2016_analysis_data.parquet"
)


################################################################################
# clean 2014 data
cfps2014person <- cfps2014person_raw |>
  left_join(cfps2014famecon, by = "fid14") |>
  select(
    cfps2014eduy, CFPS2014_AGE, CFPS_GENDER, QG12, QG6, QA301,
    QEA0, QN401_S_1, provcd14.x, fincome1_per_p
  )

# remove full datasets to conserve memory
cfps2014famecon <- NULL
cfps2014person_raw <- NULL

# new variable: years out of school
# 2020 - year of leaving school
cfps2014person <- mutate(cfps2014person, pot_work_years = case_when(
  is.na(CFPS2014_AGE) | CFPS2014_AGE < 0 | cfps2014eduy < 0 ~ NA,
  cfps2014eduy < 9 ~ CFPS2014_AGE - 16,
  cfps2014eduy >= 9 & cfps2014eduy < 12 ~ CFPS2014_AGE - 17,
  cfps2014eduy >= 12 ~ CFPS2014_AGE - 7 - cfps2014eduy
))

cfps2014person <- mutate(cfps2014person, exp2 = pot_work_years^2)

# ln hourly income
# yearly income/52/weekly work hours
cfps2014person <- mutate(cfps2014person,
  ln_hourly_wage =
    if_else(QG12 <= 0 | QG6 <= 0, NA, log((QG12 / 52.1428571429) / QG6))
)

# city hukou
cfps2014person <- mutate(cfps2014person, city_hukou = case_when(
  QA301 == 1 ~ 0,
  QA301 == 3 ~ 1,
))

# marrital status
cfps2014person <- mutate(cfps2014person, is_married = case_when(
  QEA0 == 1 ~ 0,
  QEA0 == 2 ~ 1,
  QEA0 == 3 ~ 1,
  QEA0 == 4 ~ 0,
  QEA0 == 5 ~ 0,
))

# party member status
cfps2014person <- mutate(cfps2014person, is_party = case_when(
  QN401_S_1 == 1 ~ 1,
  QN401_S_1 == 2 ~ 0,
  QN401_S_1 == 3 ~ 0,
  QN401_S_1 == 4 ~ 0,
  QN401_S_1 == 5 ~ 0,
  QN401_S_1 == 6 ~ 0,
  QN401_S_1 == 7 ~ 0,
  QN401_S_1 == 8 ~ 0,
  QN401_S_1 == 9 ~ 0,
  QN401_S_1 == 10 ~ 0,
  QN401_S_1 == 11 ~ 0,
  QN401_S_1 == 12 ~ 0,
  QN401_S_1 == 77 ~ 0,
  QN401_S_1 == 78 ~ 0
))

cfps2014person <- mutate(cfps2014person, gender = case_when(
  CFPS_GENDER == 0 ~ 0,
  CFPS_GENDER == 1 ~ 1
))

# east
cfps2014person <- mutate(cfps2014person, is_east = if_else(
  provcd14.x == 11 | provcd14.x == 12 | provcd14.x == 13 | provcd14.x == 31 |
    provcd14.x == 32 | provcd14.x == 33 | provcd14.x == 35 | provcd14.x == 37 |
    provcd14.x == 44 | provcd14.x == 46, 1, 0
))


# Have a subset with no missing values
cfps2014person_noNA <- cfps2014person |>
  rename(
    income_quartile = fincome1_per_p,
    years_of_education = cfps2014eduy
  ) |>
  select(
    ln_hourly_wage, years_of_education, gender, pot_work_years, exp2, city_hukou,
    is_married, is_party, is_east, income_quartile
  ) |>
  drop_na()

# Save the dataset
write_parquet(
  cfps2014person_noNA,
  "data/02-analysis_data/2014_analysis_data.parquet"
)


################################################################################
# clean 2012 data
cfps2012person <- cfps2012person_raw |>
  left_join(cfps2012famecon, by = "fid12")

# new variable: years out of school
# 2020 - year of leaving school
cfps2012person <- mutate(cfps2012person, pot_work_years = case_when(
  is.na(CFPS2012_AGE) | CFPS2012_AGE < 0 | eduy2012 < 0 ~ NA,
  eduy2012 < 9 ~ CFPS2012_AGE - 16,
  eduy2012 >= 9 & eduy2012 < 12 ~ CFPS2012_AGE - 17,
  eduy2012 >= 12 ~ CFPS2012_AGE - 7 - eduy2012
))

cfps2012person <- mutate(cfps2012person, exp2 = pot_work_years^2)

# identify last year's total work hours
# make start and end of 'last year'
cfps2012person <- mutate(cfps2012person,
  end_last_year = make_date(CYEAR, CMONTH.x)
)
cfps2012person <- mutate(cfps2012person,
  start_last_year = end_last_year - years(1)
)

# now get start and end times of jobs 1-5
cfps2012person <- mutate(cfps2012person,
  start_date_job1 =
    if_else(QG4121Y_A_1 <= 0 | QG4121M_A_1 <= 0, NA, make_date(
      QG4121Y_A_1,
      QG4121M_A_1
    )),
  end_date_job1 = if_else(QG4122Y_A_1 <= 0 | QG4122M_A_1 <= 0, NA,
    make_date(QG4122Y_A_1, QG4122M_A_1)
  ),
  start_date_job2 = if_else(QG4121Y_A_2 <= 0 | QG4121M_A_2 <= 0, NA,
    make_date(QG4121Y_A_2, QG4121M_A_2)
  ),
  end_date_job2 = if_else(QG4122Y_A_2 <= 0 | QG4122M_A_2 <= 0, NA,
    make_date(QG4122Y_A_2, QG4122M_A_2)
  ),
  start_date_job3 = if_else(QG4121Y_A_3 <= 0 | QG4121M_A_3 <= 0, NA,
    make_date(QG4121Y_A_3, QG4121M_A_3)
  ),
  end_date_job3 = if_else(QG4122Y_A_3 <= 0 | QG4122M_A_3 <= 0, NA,
    make_date(QG4122Y_A_3, QG4122M_A_3)
  )
)

# how long have they been doing each job in the past year
cfps2012person <- mutate(cfps2012person,
  start_date_job1_year = pmax(start_date_job1, start_last_year),
  end_date_job1_year = pmin(end_date_job1, end_last_year),
  months_job1_year = if_else(start_date_job1_year > end_date_job1_year, NA,
    interval(start_date_job1_year, end_date_job1_year) / months(1)
  ),
  start_date_job2_year = pmax(start_date_job2, start_last_year),
  end_date_job2_year = pmin(end_date_job2, end_last_year),
  months_job2_year = if_else(start_date_job2_year > end_date_job2_year, NA,
    interval(start_date_job2_year, end_date_job2_year) / months(1)
  ),
  start_date_job3_year = pmax(start_date_job3, start_last_year),
  end_date_job3_year = pmin(end_date_job3, end_last_year),
  months_job3_year = if_else(start_date_job3_year > end_date_job3_year, NA,
    interval(start_date_job3_year, end_date_job3_year) / months(1)
  )
)


# get total work time in the past year
cfps2012person <- mutate(cfps2012person,
  work_time_job1_year = if_else(QG413_A_1 < 0 | QG414_A_1 < 0, NA,
    months_job1_year * QG413_A_1 * QG414_A_1
  ),
  work_time_job2_year = if_else(QG413_A_2 < 0 | QG414_A_2 < 0, NA,
    months_job2_year * QG413_A_2 * QG414_A_2
  ),
  work_time_job3_year = if_else(QG413_A_3 < 0 | QG414_A_3 < 0, NA,
    months_job3_year * QG413_A_3 * QG414_A_3
  )
)

cfps2012person <- mutate(cfps2012person,
  total_work_hours =
    rowSums(cfps2012person[, c(
      "work_time_job1_year",
      "work_time_job2_year", "work_time_job3_year"
    )], na.rm = TRUE)
)


# ln hourly income
# yearly income/52/weekly work hours
cfps2012person <- mutate(cfps2012person,
  ln_hourly_wage =
    if_else(INCOME <= 0 | total_work_hours <= 0, NA,
      log(INCOME / total_work_hours)
    )
)



# city hukou
cfps2012person <- mutate(cfps2012person, city_hukou = case_when(
  QA301 == 1 ~ 0,
  QA301 == 3 ~ 1,
))

# marrital status
cfps2012person <- mutate(cfps2012person, is_married = case_when(
  QE104 == 1 ~ 0,
  QE104 == 2 ~ 1,
  QE104 == 3 ~ 1,
  QE104 == 4 ~ 0,
  QE104 == 5 ~ 0,
))

# party member status
cfps2012person <- mutate(cfps2012person, is_party = case_when(
  QN401_S_1 == 1 ~ 1,
  QN401_S_1 == 2 ~ 0,
  QN401_S_1 == 3 ~ 0,
  QN401_S_1 == 4 ~ 0,
  QN401_S_1 == 5 ~ 0,
  QN401_S_1 == 6 ~ 0,
  QN401_S_1 == 7 ~ 0,
  QN401_S_1 == 8 ~ 0,
  QN401_S_1 == 9 ~ 0,
  QN401_S_1 == 10 ~ 0,
  QN401_S_1 == 11 ~ 0,
  QN401_S_1 == 12 ~ 0,
  QN401_S_1 == 77 ~ 0,
  QN401_S_1 == 78 ~ 0
))

cfps2012person <- mutate(cfps2012person, gender = case_when(
  CFPS2012_GENDER == 0 ~ 0,
  CFPS2012_GENDER == 1 ~ 1
))

# east
cfps2012person <- mutate(cfps2012person, is_east = if_else(
  provcd.x == 11 | provcd.x == 12 | provcd.x == 13 | provcd.x == 31 |
    provcd.x == 32 | provcd.x == 33 | provcd.x == 35 | provcd.x == 37 |
    provcd.x == 44 | provcd.x == 46, 1, 0
))


# Have a subset with no missing values
cfps2012person_noNA <- subset(
  cfps2012person,
  !(is.na(gender)) & !(is.na(pot_work_years)) &
    !(is.na(city_hukou)) & !(is.na(fincper_p)) &
    !(is.na(is_married)) & !(is.na(is_party)) &
    !(is.na(is_east)) & !(is.na(ln_hourly_wage)) &
    !(is.na(eduy2012))
)

cfps2012person_noNA <- cfps2012person_noNA |>
  rename(
    income_quartile = fincper_p,
    years_of_education = eduy2012
  ) |>
  select(
    ln_hourly_wage, years_of_education, gender, pot_work_years, exp2, city_hukou,
    is_married, is_party, is_east, income_quartile
  ) |>
  drop_na()

# Save the dataset
write_parquet(
  cfps2012person_noNA,
  "data/02-analysis_data/2012_analysis_data.parquet"
)


################################################################################
# clean 2010 data
# Join family dataset with individual dataset
cfps2010person <- cfps2010person_raw |>
  left_join(cfps2010famecon, by = "fid") |>
  select(
    cfps2010eduy_best, QA1AGE, INCOME, QG401, QG402, QG403, QA2, QE2, QA7_S_1,
    GENDER, provcd.x, INDINC
  )

cfps2010person_raw <- NULL
cfps2010famecon <- NULL


# new variable: years out of school
cfps2010person <- mutate(cfps2010person, pot_work_years = case_when(
  is.na(QA1AGE) | QA1AGE < 0 | cfps2010eduy_best < 0 ~ NA,
  cfps2010eduy_best < 9 ~ QA1AGE - 16,
  cfps2010eduy_best >= 9 & cfps2010eduy_best < 12 ~ QA1AGE - 17,
  cfps2010eduy_best >= 12 ~ QA1AGE - 7 - cfps2010eduy_best
))

cfps2010person <- mutate(cfps2010person, exp2 = pot_work_years^2)

# ln hourly income
# yearly income/52/weekly work hours
cfps2010person <- mutate(cfps2010person,
  ln_hourly_wage =
    if_else(INCOME <= 0 | QG401 <= 0 |
      QG402 <= 0 | QG403 <= 0, NA,
    log(INCOME / QG401 / QG402 / QG403)
    )
)

# city hukou
cfps2010person <- mutate(cfps2010person, city_hukou = case_when(
  QA2 == 1 ~ 0,
  QA2 == 3 ~ 1,
))

# marrital status
cfps2010person <- mutate(cfps2010person, is_married = case_when(
  QE2 == -8 ~ 0,
  QE2 == 0 ~ 1,
  QE2 == 1 ~ 1,
))

# party member status
cfps2010person <- mutate(cfps2010person, is_party = case_when(
  QA7_S_1 == 1 ~ 1,
  QA7_S_1 == 2 ~ 0,
  QA7_S_1 == 3 ~ 0,
  QA7_S_1 == 4 ~ 0,
  QA7_S_1 == 5 ~ 0,
  QA7_S_1 == 6 ~ 0,
  QA7_S_1 == 7 ~ 0,
  QA7_S_1 == 8 ~ 0,
  QA7_S_1 == 9 ~ 0,
  QA7_S_1 == 10 ~ 0,
  QA7_S_1 == 11 ~ 0,
  QA7_S_1 == 12 ~ 0,
  QA7_S_1 == 77 ~ 0,
  QA7_S_1 == 78 ~ 0
))

cfps2010person <- mutate(cfps2010person, gender = case_when(
  GENDER == 0 ~ 0,
  GENDER == 1 ~ 1
))

# east
cfps2010person <- mutate(cfps2010person, is_east = if_else(
  provcd.x == 11 | provcd.x == 12 | provcd.x == 13 | provcd.x == 31 |
    provcd.x == 32 | provcd.x == 33 | provcd.x == 35 | provcd.x == 37 |
    provcd.x == 44 | provcd.x == 46, 1, 0
))

# make quartiles
cfps2010person <- mutate(cfps2010person,
  fincper_p =
    ntile(INDINC, 4)
)

# Have a subset with no missing values
cfps2010person_noNA <- cfps2010person |>
  rename(
    income_quartile = fincper_p,
    years_of_education = cfps2010eduy_best
  ) |>
  select(
    ln_hourly_wage, years_of_education, gender, pot_work_years, exp2, city_hukou,
    is_married, is_party, is_east, income_quartile
  ) |>
  drop_na()

# Save the dataset
write_parquet(
  cfps2010person_noNA,
  "data/02-analysis_data/2010_analysis_data.parquet"
)


#### Placeholder datasets for the repo (remove when replicating) ####
# Create empty dataset
placeholder_df <- tibble(
  yearly_income = numeric(),
  years_of_education = numeric(),
  years_of_work = numeric(),
  gender = numeric(),
  city_hukou = numeric(),
  is_party = numeric(),
  is_married = numeric(),
  is_east = numeric(),
  income_quartile = numeric()
)

# Save into repo
write_csv(
  placeholder_df,
  "data/01-raw_data/raw_data_placeholder.csv"
)

# This is in csv to prevent being ignored
# Real analysis datasets are stored as parquet files
write_csv(
  placeholder_df,
  "data/02-analysis_data/analysis_data_placeholder.csv"
)
