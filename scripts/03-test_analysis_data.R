#### Preamble ####
# Purpose: Tests data used for analysis
# Author: Yisu Hou
# Date: 28 November 2024
# Contact: yisu.hou@mail.utoronto.ca
# License: None
# Pre-requisites:
# - Packages `tidyverse`, `arrow` and `testthat` must be installed
# - 02-clean_data.R must have been run


#### Workspace setup ####
library(tidyverse)
library(testthat)
library(arrow)

data_2020 <- read_parquet("data/02-analysis_data/2020_analysis_data.parquet")
data_2018 <- read_parquet("data/02-analysis_data/2018_analysis_data.parquet")
data_2016 <- read_parquet("data/02-analysis_data/2016_analysis_data.parquet")
data_2014 <- read_parquet("data/02-analysis_data/2014_analysis_data.parquet")
data_2012 <- read_parquet("data/02-analysis_data/2012_analysis_data.parquet")
data_2010 <- read_parquet("data/02-analysis_data/2010_analysis_data.parquet")


# Create a named list of datasets
datasets_list <- list(
  data_2020 = data_2020,
  data_2018 = data_2018,
  data_2016 = data_2016,
  data_2014 = data_2014,
  data_2012 = data_2012,
  data_2010 = data_2010
)


# Define the helper function to run tests
run_sanity_checks <- function(data, dataset_name) {
  test_that(paste("Sanity Checks for", dataset_name), {
    #### Data Structure Tests ####

    # Check if the dataset is a dataframe
    expect_true(is.data.frame(data), info = paste(dataset_name, "is not a dataframe"))

    # Check number of rows
    expect_true(nrow(data) >= 1000 && nrow(data) <= 10000,
      info = paste(dataset_name, "does not have between 1000 and 10000 rows")
    )

    # Check number of columns (10)
    # There are 10 analysis variables
    expect_equal(ncol(data), 10,
      info = paste(dataset_name, "does not have exactly 10 columns")
    )

    # Check for required columns
    required_cols <- c(
      "ln_hourly_wage", "years_of_education", "gender", "pot_work_years",
      "exp2", "city_hukou", "is_married",
      "is_party", "is_east", "income_quartile"
    )
    expect_true(all(required_cols %in% colnames(data)),
      info = paste(dataset_name, "is missing required columns")
    )

    # All variables should be numeric
    expect_type(data$ln_hourly_wage, "double")
    expect_type(data$years_of_education, "double")
    expect_type(data$gender, "double")
    expect_type(data$pot_work_years, "double")
    expect_type(data$exp2, "double")
    expect_type(data$city_hukou, "double")
    expect_type(data$is_married, "double")
    expect_type(data$is_party, "double")
    expect_type(data$is_east, "double")
    expect_true(
      typeof(data$city_hukou) %in% c("double", "integer")
    )

    #### Value Ranges and Constraints ####
    # income variable for analysis is measured in natural log, so
    # it can take negative values
    expect_true(all(data$ln_hourly_wage >= -999 & data$ln_hourly_wage <= 100))

    # For the analysis datasets, there are no specific values to look for
    # because the data in some iterations of the survey just asks how many
    # years of education someone has received, while other iterations ask
    # about education level
    # However, values should still be between 0 (no school) and 22 (doctoral)
    # and they should be whole numbers
    expect_true(all(data$years_of_education >= 0 & data$years_of_education <= 22))
    expect_true(all(data$years_of_education == as.integer(data$years_of_education)))

    # Work years are whole numbers and not too large
    expect_true(all(data$pot_work_years <= 80))
    expect_true(all(data$pot_work_years == as.integer(data$pot_work_years)))

    # Dummy variables should all be 0 or 1
    binary_vars <- c("gender", "city_hukou", "is_party", "is_married", "is_east")
    for (var in binary_vars) {
      expect_true(all(data[[var]] %in% c(0, 1)), info = paste("Checking values of", var))
    }


    # Family income quartile variable should be whole numbers between 1 and 4
    expect_true(all(data$income_quartile >= 1 & data$income_quartile <= 4))
    expect_true(all(data$income_quartile == as.integer(data$income_quartile)))

    #### Missing Values Tests ####
    # No missing values in the dataset
    expect_false(any(is.na(data)),
      info = paste(dataset_name, "contains missing values")
    )
  })
}


# Iterate over each dataset and run sanity checks
for (dataset_name in names(datasets_list)) {
  dataset <- datasets_list[[dataset_name]]
  run_sanity_checks(dataset, dataset_name)
}
