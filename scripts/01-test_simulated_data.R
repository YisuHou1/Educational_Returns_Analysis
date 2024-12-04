#### Preamble ####
# Purpose: Tests the structure and validity of the simulated Chinese
# individual survey dataset.
# Author: Yisu Hou
# Date: 28 November 2024
# Contact: yisu.hou@mail.utoronto.ca
# License: None
# Pre-requisites:
# - Packages `tidyverse` and `testthat` must be installed
# - 00-simulate_data.R must have been run


#### Workspace setup ####
library(tidyverse)
library(testthat)

dataset <- read_csv("data/00-simulated_data/simulated_data.csv")

# Define the test context
context("Sanity Checks for Simulated Datasets")

#### Data structure tests ####

test_that("Simulated Dataset has correct structure", {
  expect_true(is.data.frame(dataset))
  expect_equal(nrow(dataset), 500)
  expect_equal(ncol(dataset), 8)
})

test_that("Simulated Dataset has all required columns with correct types", {
  required_cols <- c(
    "yearly_income",
    "years_of_education",
    "years_of_work",
    "gender",
    "city_hukou",
    "is_party",
    "is_married",
    "is_east"
  )
  expect_true(all(required_cols %in% colnames(dataset)))

  # Check data types
  expect_type(dataset$yearly_income, "double") # Numeric
  expect_type(dataset$years_of_education, "double")
  expect_type(dataset$years_of_work, "double")
  expect_type(dataset$gender, "double")
  expect_type(dataset$city_hukou, "double")
  expect_type(dataset$is_party, "double")
  expect_type(dataset$is_married, "double")
  expect_type(dataset$is_east, "double")
})


#### Value Ranges and Constraints ####

test_that("yearly_income values are between 0 and 1,000,000 and multiples of 100", {
  expect_true(all(dataset$yearly_income >= 0 & dataset$yearly_income <= 1000000))

  # Check if all values are multiples of 100
  expect_true(all(dataset$yearly_income %% 100 == 0))
})

test_that("years_of_education contains only valid education levels", {
  valid_education_levels <- c(0, 6, 9, 12, 15, 16, 19, 22)
  expect_true(all(dataset$years_of_education %in% valid_education_levels))
})

test_that("years_of_work contains integers between 0 and 80", {
  expect_true(all(dataset$years_of_work >= 0 & dataset$years_of_work <= 80))
  expect_true(all(dataset$years_of_work == as.integer(dataset$years_of_work)))
})

test_that("Binary variables contain only 0 or 1", {
  binary_vars <- c("gender", "city_hukou", "is_party", "is_married", "is_east")
  for (var in binary_vars) {
    expect_true(all(dataset[[var]] %in% c(0, 1)), info = paste("Checking", var))
  }
})

test_that("yearly_income has an average close to 50,000", {
  avg_income <- mean(dataset$yearly_income)
  expect_true(avg_income >= 49000 & avg_income <= 51000,
    info = paste("Average income is", avg_income)
  )
})

test_that("years_of_education has a reasonable distribution", {
  # Check that all specified education levels are present
  valid_education_levels <- c(0, 6, 9, 12, 15, 16, 19, 22)
  expect_true(all(valid_education_levels %in% unique(dataset$years_of_education)))
})

#### Data Integrity Tests ####

test_that("No unintended duplicate rows in dataset", {
  duplicates <- dataset |> duplicated()
  expect_false(any(duplicates), info = "There are duplicate rows in the dataset")
})

#### Missing Values Tests ####

test_that("Simulated Dataset has no missing values", {
  expect_false(any(is.na(dataset)), info = "There are missing values in the dataset")
})


test_that("All sanity checks have passed", {
  expect_true(TRUE)
})
