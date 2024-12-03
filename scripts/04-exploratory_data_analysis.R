#### Preamble ####
# Purpose: Exploratory data analysis using analysis data
# Author: Yisu Hou
# Date: 28 November 2024
# Contact: yisu.hou@mail.utoronto.ca
# License: None
# Pre-requisites:
# - Packages `tidyverse`, `arrow` are installed
# - 02-clean_data.R and 03-test_analysis_data.R must have been run


#### Workspace setup ####

# Load libraries
library(tidyverse)
library(arrow)

#### Read data ####
data_2020 <- read_parquet("data/02-analysis_data/2020_analysis_data.parquet")
data_2018 <- read_parquet("data/02-analysis_data/2018_analysis_data.parquet")
data_2016 <- read_parquet("data/02-analysis_data/2016_analysis_data.parquet")
data_2014 <- read_parquet("data/02-analysis_data/2014_analysis_data.parquet")
data_2012 <- read_parquet("data/02-analysis_data/2012_analysis_data.parquet")
data_2010 <- read_parquet("data/02-analysis_data/2010_analysis_data.parquet")

# Distribution of variables in the dataset
# Make a combined dataset that includes all survey iterations
all_combined <- bind_rows(
  data_2020, data_2018, data_2016, data_2014, data_2012, data_2010
)

# Generate plots to display the distributions
# Outcome variable: ln hourly wage
ggplot(all_combined, aes(x = ln_hourly_wage)) +
  geom_histogram(
    fill = "#2c7bb6", # Aesthetic fill color (steel blue)
    color = "white", # Border color for bins
    alpha = 1 # Transparency of the fill
  ) +
  labs(
    x = "ln Hourly Wage",
    y = "Frequency"
  ) +
  theme_minimal(base_size = 14) + # Minimal theme with increased base font size
  theme(
    plot.title = element_text(
      size = 16,
      face = "bold",
      hjust = 0.5, # Center the title
      margin = margin(b = 10) # Add space below the title
    ),
    plot.subtitle = element_text(
      size = 12,
      hjust = 0.5,
      margin = margin(b = 20) # Add space below the subtitle
    ),
    axis.title = element_text(face = "bold"), # Bold axis titles
    axis.text = element_text(color = "gray30"), # Dark gray axis text
    panel.grid.major = element_line(color = "gray80"), # Light gray major grid lines
    panel.grid.minor = element_blank() # Remove minor grid lines
  ) +
  # Add a vertical line for the mean percentage support
  geom_vline(
    aes(xintercept = mean(ln_hourly_wage, na.rm = TRUE)),
    color = "#d73027", # Aesthetic color (red)
    linetype = "dashed",
    size = 1
  ) +
  # Annotate the mean value on the plot
  annotate(
    "text",
    x = mean(all_combined$ln_hourly_wage, na.rm = TRUE) - 7, # Position text slightly to the right of the mean line
    y = Inf, # Position text at the top of the plot
    label = paste("Mean:", round(mean(all_combined$ln_hourly_wage, na.rm = TRUE), 1)),
    vjust = 2, # Vertical adjustment
    color = "#d73027",
    size = 5,
    fontface = "bold"
  )

# Predictors
# Years of education
ggplot(all_combined, aes(x = years_of_education)) +
  geom_histogram(
    binwidth = 1,
    fill = "#2c7bb6", # Aesthetic fill color (steel blue)
    color = "white", # Border color for bins
    alpha = 1 # Transparency of the fill
  ) +
  labs(
    x = "Years of Education",
    y = "Frequency"
  ) +
  theme_minimal(base_size = 14) + # Minimal theme with increased base font size
  theme(
    plot.title = element_text(
      size = 16,
      face = "bold",
      hjust = 0.5, # Center the title
      margin = margin(b = 10) # Add space below the title
    ),
    plot.subtitle = element_text(
      size = 12,
      hjust = 0.5,
      margin = margin(b = 20) # Add space below the subtitle
    ),
    axis.title = element_text(face = "bold"), # Bold axis titles
    axis.text = element_text(color = "gray30"), # Dark gray axis text
    panel.grid.major = element_line(color = "gray80"), # Light gray major grid lines
    panel.grid.minor = element_blank() # Remove minor grid lines
  ) +
  # Add a vertical line for the mean percentage support
  geom_vline(
    aes(xintercept = mean(years_of_education, na.rm = TRUE)),
    color = "#d73027", # Aesthetic color (red)
    linetype = "dashed",
    size = 1
  ) +
  # Annotate the mean value on the plot
  annotate(
    "text",
    x = mean(all_combined$years_of_education, na.rm = TRUE) + 2.5, # Position text slightly to the right of the mean line
    y = Inf, # Position text at the top of the plot
    label = paste("Mean:", round(mean(all_combined$years_of_education, na.rm = TRUE), 1)),
    vjust = 2, # Vertical adjustment
    color = "#d73027",
    size = 5,
    fontface = "bold"
  )

# Gender
ggplot(all_combined, aes(x = factor(gender, levels = c(0, 1), labels = c("Female", "Male")))) +
  geom_bar(fill = "#1f78b4", color = "white", alpha = 1) +
  labs(
    x = "Gender",
    y = "Frequency"
  ) +
  theme_minimal(base_size = 14) + # Minimal theme with increased base font size
  theme(
    plot.title = element_text(
      size = 16,
      face = "bold",
      hjust = 0.5, # Center the title
      margin = margin(b = 10) # Add space below the title
    ),
    plot.subtitle = element_text(
      size = 12,
      hjust = 0.5,
      margin = margin(b = 20) # Add space below the subtitle
    ),
    axis.title = element_text(face = "bold"), # Bold axis titles
    axis.text = element_text(color = "gray30"), # Dark gray axis text
    panel.grid.major = element_line(color = "gray80"), # Light gray major grid lines
    panel.grid.minor = element_blank() # Remove minor grid lines
  )

# Potential work experience
ggplot(all_combined, aes(x = pot_work_years)) +
  geom_histogram(
    fill = "#2c7bb6", # Aesthetic fill color (steel blue)
    color = "white", # Border color for bins
    alpha = 1 # Transparency of the fill
  ) +
  labs(
    x = "Potential Years of Work Experience",
    y = "Frequency"
  ) +
  coord_cartesian(xlim = c(0, 60)) +
  theme_minimal(base_size = 14) + # Minimal theme with increased base font size
  theme(
    plot.title = element_text(
      size = 16,
      face = "bold",
      hjust = 0.5, # Center the title
      margin = margin(b = 10) # Add space below the title
    ),
    plot.subtitle = element_text(
      size = 12,
      hjust = 0.5,
      margin = margin(b = 20) # Add space below the subtitle
    ),
    axis.title = element_text(face = "bold"), # Bold axis titles
    axis.text = element_text(color = "gray30"), # Dark gray axis text
    panel.grid.major = element_line(color = "gray80"), # Light gray major grid lines
    panel.grid.minor = element_blank() # Remove minor grid lines
  ) +
  # Add a vertical line for the mean percentage support
  geom_vline(
    aes(xintercept = mean(pot_work_years, na.rm = TRUE)),
    color = "#d73027", # Aesthetic color (red)
    linetype = "dashed",
    size = 1
  ) +
  # Annotate the mean value on the plot
  annotate(
    "text",
    x = mean(all_combined$pot_work_years, na.rm = TRUE) + 6, # Position text slightly to the right of the mean line
    y = Inf, # Position text at the top of the plot
    label = paste("Mean:", round(mean(all_combined$pot_work_years, na.rm = TRUE), 1)),
    vjust = 2, # Vertical adjustment
    color = "#d73027",
    size = 5,
    fontface = "bold"
  )


# Squared work experience
ggplot(all_combined, aes(x = exp2)) +
  geom_histogram(
    fill = "#2c7bb6", # Aesthetic fill color (steel blue)
    color = "white", # Border color for bins
    alpha = 1 # Transparency of the fill
  ) +
  labs(
    x = "Years of Work Experience Squared",
    y = "Frequency"
  ) +
  coord_cartesian(xlim = c(0, 3200)) +
  theme_minimal(base_size = 14) + # Minimal theme with increased base font size
  theme(
    plot.title = element_text(
      size = 16,
      face = "bold",
      hjust = 0.5, # Center the title
      margin = margin(b = 10) # Add space below the title
    ),
    plot.subtitle = element_text(
      size = 12,
      hjust = 0.5,
      margin = margin(b = 20) # Add space below the subtitle
    ),
    axis.title = element_text(face = "bold"), # Bold axis titles
    axis.text = element_text(color = "gray30"), # Dark gray axis text
    panel.grid.major = element_line(color = "gray80"), # Light gray major grid lines
    panel.grid.minor = element_blank() # Remove minor grid lines
  ) +
  # Add a vertical line for the mean percentage support
  geom_vline(
    aes(xintercept = mean(exp2, na.rm = TRUE)),
    color = "#d73027", # Aesthetic color (red)
    linetype = "dashed",
    size = 1
  ) +
  # Annotate the mean value on the plot
  annotate(
    "text",
    x = mean(all_combined$exp2, na.rm = TRUE) + 350, # Position text slightly to the right of the mean line
    y = Inf, # Position text at the top of the plot
    label = paste("Mean:", round(mean(all_combined$exp2, na.rm = TRUE), 1)),
    vjust = 2, # Vertical adjustment
    color = "#d73027",
    size = 5,
    fontface = "bold"
  )


# City ID
ggplot(all_combined, aes(x = factor(city_hukou, levels = c(0, 1), labels = c("Rural", "Urban")))) +
  geom_bar(fill = "#1f78b4", color = "white", alpha = 1) +
  labs(
    x = "Hukou Categorization",
    y = "Frequency"
  ) +
  theme_minimal(base_size = 14) + # Minimal theme with increased base font size
  theme(
    plot.title = element_text(
      size = 16,
      face = "bold",
      hjust = 0.5, # Center the title
      margin = margin(b = 10) # Add space below the title
    ),
    plot.subtitle = element_text(
      size = 12,
      hjust = 0.5,
      margin = margin(b = 20) # Add space below the subtitle
    ),
    axis.title = element_text(face = "bold"), # Bold axis titles
    axis.text = element_text(color = "gray30"), # Dark gray axis text
    panel.grid.major = element_line(color = "gray80"), # Light gray major grid lines
    panel.grid.minor = element_blank() # Remove minor grid lines
  )

# Married
ggplot(all_combined, aes(x = factor(is_married, levels = c(0, 1), labels = c("Unmarried", "Married")))) +
  geom_bar(fill = "#1f78b4", color = "white", alpha = 1) +
  labs(
    x = "Marital Status",
    y = "Frequency"
  ) +
  theme_minimal(base_size = 14) + # Minimal theme with increased base font size
  theme(
    plot.title = element_text(
      size = 16,
      face = "bold",
      hjust = 0.5, # Center the title
      margin = margin(b = 10) # Add space below the title
    ),
    plot.subtitle = element_text(
      size = 12,
      hjust = 0.5,
      margin = margin(b = 20) # Add space below the subtitle
    ),
    axis.title = element_text(face = "bold"), # Bold axis titles
    axis.text = element_text(color = "gray30"), # Dark gray axis text
    panel.grid.major = element_line(color = "gray80"), # Light gray major grid lines
    panel.grid.minor = element_blank() # Remove minor grid lines
  )

# CCP Membership
ggplot(all_combined, aes(x = factor(is_party, levels = c(0, 1), labels = c("Not CCP Member", "CCP Member")))) +
  geom_bar(fill = "#1f78b4", color = "white", alpha = 1) +
  labs(
    x = "Party Membership",
    y = "Frequency"
  ) +
  theme_minimal(base_size = 14) + # Minimal theme with increased base font size
  theme(
    plot.title = element_text(
      size = 16,
      face = "bold",
      hjust = 0.5, # Center the title
      margin = margin(b = 10) # Add space below the title
    ),
    plot.subtitle = element_text(
      size = 12,
      hjust = 0.5,
      margin = margin(b = 20) # Add space below the subtitle
    ),
    axis.title = element_text(face = "bold"), # Bold axis titles
    axis.text = element_text(color = "gray30"), # Dark gray axis text
    panel.grid.major = element_line(color = "gray80"), # Light gray major grid lines
    panel.grid.minor = element_blank() # Remove minor grid lines
  )

# Eastern Province
ggplot(all_combined, aes(x = factor(is_east, levels = c(0, 1), labels = c("Other Provinces", "Eastern Provinces")))) +
  geom_bar(fill = "#1f78b4", color = "white", alpha = 1) +
  labs(
    x = "Area of Residency",
    y = "Frequency"
  ) +
  theme_minimal(base_size = 14) + # Minimal theme with increased base font size
  theme(
    plot.title = element_text(
      size = 16,
      face = "bold",
      hjust = 0.5, # Center the title
      margin = margin(b = 10) # Add space below the title
    ),
    plot.subtitle = element_text(
      size = 12,
      hjust = 0.5,
      margin = margin(b = 20) # Add space below the subtitle
    ),
    axis.title = element_text(face = "bold"), # Bold axis titles
    axis.text = element_text(color = "gray30"), # Dark gray axis text
    panel.grid.major = element_line(color = "gray80"), # Light gray major grid lines
    panel.grid.minor = element_blank() # Remove minor grid lines
  )

ggplot(all_combined, aes(x = income_quartile)) +
  geom_bar(fill = "#1f78b4", color = "white", alpha = 1) +
  labs(
    x = "Family Income Quartile",
    y = "Frequency"
  ) +
  theme_minimal(base_size = 14) + # Minimal theme with increased base font size
  theme(
    plot.title = element_text(
      size = 16,
      face = "bold",
      hjust = 0.5, # Center the title
      margin = margin(b = 10) # Add space below the title
    ),
    plot.subtitle = element_text(
      size = 12,
      hjust = 0.5,
      margin = margin(b = 20) # Add space below the subtitle
    ),
    axis.title = element_text(face = "bold"), # Bold axis titles
    axis.text = element_text(color = "gray30"), # Dark gray axis text
    panel.grid.major = element_line(color = "gray80"), # Light gray major grid lines
    panel.grid.minor = element_blank() # Remove minor grid lines
  )


