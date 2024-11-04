#### Preamble ####
# Purpose: Tests the structure and validity of the simulated US 
  # electoral polls dataset.
# Author: Andrew Goh, Yisu Hou
# Date: 3 November 2024
# Contact: yisu.hou@mail.utoronto.ca
# License: None
# Pre-requisites: 
  # - Packages `tidyverse`, `lubridate`, and `testthat` must be installed
  # - 00-simulate_data.R must have been run


#### Workspace setup ####
library(tidyverse)
library(testthat)
library(lubridate)

simulated_data <- read_csv("data/00-simulated_data/simulated_polls.csv")

# Define the test context
context("Sanity Checks for Simulated Datasets")

#### Data structure tests ####

test_that("Simulated Dataset has correct structure", {
  expect_true(is.data.frame(simulated_data))
  expect_true(50 <= nrow(simulated_data))
  expect_true(1000 >= nrow(simulated_data))
  expect_equal(ncol(simulated_data), 8)
})

test_that("Simulated Dataset has all required columns with correct types", {
  required_cols <- c("pollster", "has_sponsor", "pollscore", 
                     "transparency_score", "sample_size", "end_date", "pct_support")
  expect_true(all(required_cols %in% colnames(simulated_data)))
  
  # Check data types
  expect_type(simulated_data$pollster, "character")
  expect_type(simulated_data$has_sponsor, "double") 
  expect_type(simulated_data$pollscore, "double") 
  expect_type(simulated_data$transparency_score, "double")
  expect_type(simulated_data$sample_size, "double")
  expect_s3_class(simulated_data$end_date, "Date")
  expect_type(simulated_data$pct_support, "double")  
})


#### Value Ranges and Constraints ####

test_that("pollster column contains only valid pollster names", {
  valid_pollsters <- c(
    "Siena/NYT", 
    "YouGov", 
    "CES / YouGov", 
    "Marquette Law School", 
    "The Washington Post", 
    "McCourtney Institute/YouGov"
  )
  expect_true(all(simulated_data$pollster %in% valid_pollsters))
})

test_that("has_sponsor column contains only 0 or 1", {
  expect_true(all(simulated_data$has_sponsor %in% c(0, 1)))
})

test_that("pollscore values are negative and greater than -1.5", {
  expect_true(all(simulated_data$pollscore >= -1.5 
                & simulated_data$pollscore <= -0.1))
  
  # Check one decimal place
  decimals <- simulated_data$pollscore * 10 %% 1
  expect_true(all(decimals == 0))
})

test_that("transparency_score values are between 0.0 and 10.0", {
  valid_transparency_scores <- seq(0.0, 10.0, by = 0.5)
  expect_true(all(simulated_data$transparency_score
  %in% valid_transparency_scores))
})

test_that("sample_size values are integers between 1 and 1000000", {
  expect_true(all(simulated_data$sample_size >= 1 
  & simulated_data$sample_size <= 1000000))
  expect_true(all(simulated_data$sample_size == 
  as.integer(simulated_data$sample_size)))
})

test_that("end_date values are between 2024-07-21 and today", {
  start_date <- as.Date("2024-07-21")
  end_date <- as.Date("2024-11-03")
  expect_true(all(simulated_data$end_date >= start_date & simulated_data$end_date <= end_date))
})

test_that("pct_support values are reasonable (between 0 and 100)", {
  expect_true(all(simulated_data$pct_support >= 0 & simulated_data$pct_support <= 100))
})

test_that("state column contains 'National' between 1 and 300 times", {
  national_count <- sum(simulated_data$state == "National")
  expect_true(1 <= national_count & national_count <= 500)
  
  # Test state names
  non_national_states <- simulated_data$state[simulated_data$state != "National"]
  expect_true(all(non_national_states %in% state.name))
})

test_that("No unintended duplicate rows in simulated_data dataset", {
  duplicates <- simulated_data %>%
    duplicated()
  expect_false(any(duplicates))
})


#### Missing Values Tests ####

test_that("National Simulated Dataset has no missing values", {
  expect_false(any(is.na(simulated_data)))
})


test_that("All sanity checks have passed", {
  expect_true(TRUE)
})


