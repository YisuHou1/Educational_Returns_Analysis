#### Preamble ####
# Purpose: Exploratory data analysis using analysis data
# Author: Andrew Goh, Yisu Hou
# Date: 3 November 2024
# Contact: yisu.hou@mail.utoronto.ca
# License: None
# Pre-requisites:
  # - Packages `tidyverse`, `caret`, `glmnet`, `nnet`, `lubridate`, `pROC`, 
  # and `stringr`must be installed
  # - 02-clean_data.R and 03-test_analysis_data.R must have been run


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

# Distribution of variables in the dataset
all_combined <- bind_rows(harris_2024_lower, trump_2024_lower,
                          biden_2020, trump_2020)

# Generate plots to display the distributions
ggplot(all_combined, aes(x = pct)) +
  geom_histogram(
    binwidth = 1,                # Adjust bin width for granularity
    fill = "#2c7bb6",            # Aesthetic fill color (steel blue)
    color = "white",             # Border color for bins
    alpha = 0.8                   # Transparency of the fill
  ) +
  labs(
    x = "Percentage Support",
    y = "Number of Polls"
  ) +
  theme_minimal(base_size = 14) +   # Minimal theme with increased base font size
  theme(
    plot.title = element_text(
      size = 16, 
      face = "bold", 
      hjust = 0.5,                # Center the title
      margin = margin(b = 10)     # Add space below the title
    ),
    plot.subtitle = element_text(
      size = 12, 
      hjust = 0.5, 
      margin = margin(b = 20)     # Add space below the subtitle
    ),
    axis.title = element_text(face = "bold"), # Bold axis titles
    axis.text = element_text(color = "gray30"), # Dark gray axis text
    panel.grid.major = element_line(color = "gray80"), # Light gray major grid lines
    panel.grid.minor = element_blank()                 # Remove minor grid lines
  ) +
  # Add a vertical line for the mean percentage support
  geom_vline(
    aes(xintercept = mean(pct, na.rm = TRUE)),
    color = "#d73027",           # Aesthetic color (red)
    linetype = "dashed", 
    size = 1
  ) +
  # Annotate the mean value on the plot
  annotate(
    "text",
    x = mean(all_combined$pct, na.rm = TRUE) - 7, # Position text slightly to the right of the mean line
    y = Inf,                                     # Position text at the top of the plot
    label = paste("Mean:", round(mean(all_combined$pct, na.rm = TRUE), 1), "%"),
    vjust = 2,                                   # Vertical adjustment
    color = "#d73027",
    size = 5,
    fontface = "bold"
  )

ggplot(all_combined %>% filter(cycle == 2020), aes(x = end_date)) +
  geom_histogram(binwidth = 7, fill = "#1f78b4", color = "white", alpha = 0.7) +
  labs(
    x = "End Date",
    y = "Number of Polls"
  ) +
  scale_x_date(date_labels = "%b %d, %Y", date_breaks = "1 month") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggplot(all_combined %>% filter(cycle == 2024), aes(x = end_date)) +
  geom_histogram(binwidth = 7, fill = "#1f78b4", color = "white", alpha = 0.7) +
  labs(
    x = "End Date",
    y = "Number of Polls"
  ) +
  scale_x_date(date_labels = "%b %d, %Y", date_breaks = "1 month") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggplot(all_combined, aes(x = factor(has_sponsor, levels = c(0,1), labels = c("No Sponsor", "Has Sponsor")))) +
  geom_bar(fill = "#1f78b4", color = "white", alpha = 0.7) +
  labs(
    x = "Has Sponsor",
    y = "Number of Polls"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold")
  )

ggplot(all_combined, aes(x = factor(transparency_score))) +
  geom_bar(fill = "#1f78b4", color = "white", alpha = 0.7) +
  labs(
    x = "Transparency Score",
    y = "Number of Polls"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold")
  )

ggplot(all_combined, aes(x = sample_size)) +
  geom_histogram(binwidth = 500, fill = "#1f78b4", color = "white", alpha = 0.7) +
  labs(
    x = "Sample Size",
    y = "Number of Polls"
  ) +
  scale_x_continuous(labels = scales::comma) +
  coord_cartesian(xlim = c(0, 6000)) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold")
  )

