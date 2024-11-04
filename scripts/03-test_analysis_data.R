#### Preamble ####
# Purpose: Tests data used for analysis
# Author: Andrew Goh, Yisu Hou
# Date: 3 November 2024
# Contact: yisu.hou@mail.utoronto.ca
# License: None
# Pre-requisites: [...UPDATE THIS...]
  # - Packages `tidyverse` and `testthat` must be installed
  # - 02-clean_data.R must have been run


#### Workspace setup ####
library(tidyverse)
library(testthat)

biden_2020 <- read_csv("data/02-analysis_data/biden_2020_analysis_data.csv")
trump_2020 <- read_csv("data/02-analysis_data/trump_2020_analysis_data.csv")
harris_2024 <- read_csv("data/02-analysis_data/harris_2024_analysis_data.csv")
trump_2024 <- read_csv("data/02-analysis_data/trump_2024_analysis_data.csv")
harris_2024_low <- read_csv("data/02-analysis_data/harris_2024_analysis_data_lower.csv")
trump_2024_low <- read_csv("data/02-analysis_data/trump_2024_analysis_data_lower.csv")


# Create a named list of datasets
datasets_list <- list(
  biden_2020 = biden_2020,
  trump_2020 = trump_2020,
  harris_2024 = harris_2024,
  trump_2024 = trump_2024
)

datasets_list_low <- list(
  harris_2024_low = harris_2024_low,
  trump_2024_low = trump_2024_low
)


# Define the helper function to run tests
run_sanity_checks <- function(data, dataset_name) {
  
  test_that(paste("Sanity Checks for", dataset_name), {
    
    #### Data Structure Tests ####
    
    # Check if the dataset is a dataframe
    expect_true(is.data.frame(data), info = paste(dataset_name, "is not a dataframe"))
    
    # Check number of rows
    expect_true(nrow(data) >= 50 && nrow(data) <= 1000, 
                info = paste(dataset_name, "does not have between 50 and 1000 rows"))
    
    # Check number of columns (11)
    expect_equal(ncol(data), 11, 
                 info = paste(dataset_name, "does not have exactly 11 columns"))
    
    # Check for required columns
    required_cols <- c("pollster", "has_sponsor", "numeric_grade", "pollscore", 
                       "transparency_score", "sample_size", "end_date",
                       "state", "candidate_name", "pct", "cycle")
    expect_true(all(required_cols %in% colnames(data)), 
                info = paste(dataset_name, "is missing required columns"))
    
    # Check data types of important variables
    expect_type(data$pollster, "character")
    expect_type(data$has_sponsor, "double")
    expect_type(data$pollscore, "double")
    expect_type(data$transparency_score, "double")
    expect_type(data$sample_size, "double")
    expect_s3_class(data$end_date, "Date")
    expect_type(data$pct, "double")
    expect_type(data$state, "character")
    
    #### Value Ranges and Constraints ####
    
    # Valid pollster names from FiveThirthEight's list of pollsters with 3.0
    # rating
    valid_pollsters <- c(
      "Siena/NYT", 
      "YouGov", 
      "CES / YouGov", 
      "Marquette Law School", 
      "The Washington Post", 
      "McCourtney Institute/YouGov",
      "ABC/Washington Post"
    )
    expect_true(all(data$pollster %in% valid_pollsters), 
                info = paste(dataset_name, "has invalid pollster names"))
    
    # has_sponsor only 0 or 1
    expect_true(all(data$has_sponsor %in% c(0, 1)), 
                info = paste(dataset_name, "has values in has_sponsor other than 0 or 1"))
    
    # pollscore between -1.5 and -0.1
    expect_true(all(data$pollscore >= -1.5 & data$pollscore <= -0.1), 
                info = paste(dataset_name, "pollscore values are out of range"))
    
    # numeric grade = 3.0
    expect_true(all(data$numeric_grade == 3.0),
                info = paste(dataset_name, "numeric grade values invalid"))
    
    # pollscore has one decimal place
    decimals_pollscore <- (data$pollscore * 10) %% 1
    expect_true(all(decimals_pollscore == 0), 
                info = paste(dataset_name, "pollscore does not have exactly one decimal place"))
    
    # transparency_score between 0.0 and 10.0 in 0.5 increments
    valid_transparency_scores <- seq(0.0, 10.0, by = 0.5)
    expect_true(all(data$transparency_score %in% valid_transparency_scores), 
                info = paste(dataset_name, "transparency_score values are invalid"))
    
    # sample_size between 1 and 1,000,000 and integers
    expect_true(all(data$sample_size >= 1 & data$sample_size <= 1000000), 
                info = paste(dataset_name, "sample_size values are out of range"))
    expect_true(all(data$sample_size == as.integer(data$sample_size)), 
                info = paste(dataset_name, "sample_size contains non-integer values"))
    
    # end_date between 2020-01-01 and 2024-11-03
    # since polls from the 2020 cycle are also in the analysis data
    start_date <- as.Date("2020-01-01")
    end_date_limit <- as.Date("2024-11-03")
    expect_true(all(data$end_date >= start_date & data$end_date <= end_date_limit), 
                info = paste(dataset_name, "end_date values are out of range"))
    
    # pct_support between 0 and 100
    expect_true(all(data$pct >= 0 & data$pct <= 100), 
                info = paste(dataset_name, "pct_support values are out of range"))
    
    # cycle is either 2020 or 2024
    expect_true(all(data$cycle == 2020 | data$cycle == 2024), 
                info = paste(dataset_name, "cycle values are out of range"))
    
    # state column contains 'National' between 1 and 500 times
    national_count <- sum(data$state == "National")
    expect_true(national_count >= 1 && national_count <= 500, 
                info = paste(dataset_name, "does not have between 1 and 500 'National' entries in state"))
    
    # state column contains valid US states or 'National'
    non_national_states <- data$state[data$state != "National"]
    # Create a regex pattern that matches any US state name at the beginning of the string
    pattern <- paste0("^(", paste(state.name, collapse = "|"), ")")
    
    # Use grepl to check if each non-National state entry starts with a valid state name
    matches <- grepl(pattern, non_national_states)
    
    # Assert that all non-National state entries match the pattern
    expect_true(all(matches), 
                info = paste(dataset_name, "state column contains invalid state names"))
    
    # No duplicate rows
    expect_false(any(duplicated(data)), 
                 info = paste(dataset_name, "contains duplicate rows"))
    
    #### Missing Values Tests ####
    
    # No missing values in the dataset
    expect_false(any(is.na(data)), 
                 info = paste(dataset_name, "contains missing values"))
    
  })
  
}


# Define the helper function to run tests on lower quality datasets
run_sanity_checks_low <- function(data, dataset_name) {
  
  test_that(paste("Sanity Checks for", dataset_name), {
    
    #### Data Structure Tests ####
    
    # Check if the dataset is a dataframe
    expect_true(is.data.frame(data), info = paste(dataset_name, "is not a dataframe"))
    
    # Check number of rows
    expect_true(nrow(data) >= 50 && nrow(data) <= 1000, 
                info = paste(dataset_name, "does not have between 50 and 1000 rows"))
    
    # Check number of columns (11)
    expect_equal(ncol(data), 11, 
                 info = paste(dataset_name, "does not have exactly 11 columns"))
    
    # Check for required columns
    required_cols <- c("pollster", "has_sponsor", "numeric_grade", "pollscore", 
                       "transparency_score", "sample_size", "end_date",
                       "state", "candidate_name", "pct", "cycle")
    expect_true(all(required_cols %in% colnames(data)), 
                info = paste(dataset_name, "is missing required columns"))
    
    # Check data types of important variables
    expect_type(data$pollster, "character")
    expect_type(data$has_sponsor, "double")
    expect_type(data$pollscore, "double")
    expect_type(data$transparency_score, "double")
    expect_type(data$sample_size, "double")
    expect_s3_class(data$end_date, "Date")
    expect_type(data$pct, "double")
    expect_type(data$state, "character")
    
    #### Value Ranges and Constraints ####
    
    # Pollster name is not tested in this case, as there are too many pollsters
    # with more than 2.5 numeric score, and pollster names are irrelevant to
    # the analysis
    
    # has_sponsor only 0 or 1
    expect_true(all(data$has_sponsor %in% c(0, 1)), 
                info = paste(dataset_name, "has values in has_sponsor other than 0 or 1"))
    
    # pollscore between -1.5 and -0.1
    expect_true(all(data$pollscore >= -1.5 & data$pollscore <= -0.1), 
                info = paste(dataset_name, "pollscore values are out of range"))
    
    # numeric grade greater than or equal to 2.5, less than or equal to 3.0
    expect_true(all(data$numeric_grade <= 3.0 & data$numeric_grade >= 2.5),
                info = paste(dataset_name, "numeric grade values invalid"))
    
    # pollscore has one decimal place
    decimals_pollscore <- (data$pollscore * 10) %% 1
    expect_true(all(decimals_pollscore == 0), 
                info = paste(dataset_name, "pollscore does not have exactly one decimal place"))
    
    # transparency_score between 0.0 and 10.0 in 0.5 increments
    valid_transparency_scores <- seq(0.0, 10.0, by = 0.5)
    expect_true(all(data$transparency_score %in% valid_transparency_scores), 
                info = paste(dataset_name, "transparency_score values are invalid"))
    
    # sample_size between 1 and 1,000,000 and integers
    expect_true(all(data$sample_size >= 1 & data$sample_size <= 1000000), 
                info = paste(dataset_name, "sample_size values are out of range"))
    expect_true(all(data$sample_size == as.integer(data$sample_size)), 
                info = paste(dataset_name, "sample_size contains non-integer values"))
    
    # end_date between 2024-07-21 and 2024-11-03
    # for the lower quality data, only the 2024 cycle is included
    start_date <- as.Date("2024-07-21")
    end_date_limit <- as.Date("2024-11-03")
    expect_true(all(data$end_date >= start_date & data$end_date <= end_date_limit), 
                info = paste(dataset_name, "end_date values are out of range"))
    
    # pct_support between 0 and 100
    expect_true(all(data$pct >= 0 & data$pct <= 100), 
                info = paste(dataset_name, "pct_support values are out of range"))
    
    # cycle is 2024
    expect_true(all(data$cycle == 2024), 
                info = paste(dataset_name, "cycle values are out of range"))
    
    # state column contains 'National' between 1 and 500 times
    national_count <- sum(data$state == "National")
    expect_true(national_count >= 1 && national_count <= 500, 
                info = paste(dataset_name, "does not have between 1 and 500 'National' entries in state"))
    
    # state column contains valid US states or 'National'
    non_national_states <- data$state[data$state != "National"]
    # Create a regex pattern that matches any US state name at the beginning of the string
    pattern <- paste0("^(", paste(state.name, collapse = "|"), ")")
    
    # Use grepl to check if each non-National state entry starts with a valid state name
    matches <- grepl(pattern, non_national_states)
    
    # Assert that all non-National state entries match the pattern
    expect_true(all(matches), 
                info = paste(dataset_name, "state column contains invalid state names"))
    
    # No duplicate rows
    expect_false(any(duplicated(data)), 
                 info = paste(dataset_name, "contains duplicate rows"))
    
    #### Missing Values Tests ####
    
    # No missing values in the dataset
    expect_false(any(is.na(data)), 
                 info = paste(dataset_name, "contains missing values"))
    
  })
  
}


# Iterate over each dataset and run sanity checks
for (dataset_name in names(datasets_list)) {
  dataset <- datasets_list[[dataset_name]]
  run_sanity_checks(dataset, dataset_name)
}

for (dataset_name in names(datasets_list_low)) {
  dataset <- datasets_list_low[[dataset_name]]
  run_sanity_checks_low(dataset, dataset_name)
}

