require(ggplot2)
require(dplyr)
source("CustomTheme_Light.R")

# Read Data Source From CSV
data_source <- read.csv("2016-FCC-New-Coders-Survey-Data.csv", stringsAsFactors = FALSE)

# Select the Columns of MonthsProgramming and JobRoleInterest
age_and_interest_data_source <- data_source %>% select(Age, JobRoleInterest)

# Omit NA Values
age_and_interest_data_source <- age_and_interest_data_source[complete.cases(age_and_interest_data_source),]

# Predefined Color Palette
preded_color_palette <- c("#E94C36", "#E87F04", "#F2C500", "#00BD9C", "#F8A31B", 
              "#9B56B7", "#2E97DE", "#1DCE6C", "#FD326D")

# Third Plot Part A: Those Most Popular Job Roles among Coders based on Age (Survey 2016)
age_and_interest_plot <- ggplot(age_and_interest_data_source, aes(x = reorder(JobRoleInterest, Age, FUN=median),
                                                                    y=Age,
                                                                    color = JobRoleInterest)) +
  geom_point(alpha = 0.5, position = "jitter") +
  scale_colour_manual(values = preded_color_palette) +
  scale_y_continuous(breaks = seq(10,70,10)) +
  scale_size_area() +
  ggtitle("Different Job Role Interests based on Age") +
  coord_flip() +
  CustomTheme_Light

# Print the Plot
print(age_and_interest_plot)

# Third Plot Part A: Those Most Popular Job Roles among Coders based on Age - Box Plot (Survey 2016)
age_and_interest_boxplot <- ggplot(age_and_interest_data_source, aes(x = reorder(JobRoleInterest, Age, FUN=median),
                                                                     y = Age,
                                                                     fill = JobRoleInterest)) +
  geom_boxplot(outlier.colour = "black", colour = "black") +
  scale_fill_manual(values = preded_color_palette) +
  scale_y_continuous(breaks = seq(10,70,10)) +
  ggtitle("Different Job Role Interests based on Age - Box Plot") +
  coord_flip() +
  CustomTheme_Light

# Print the Plot
print(age_and_interest_boxplot)

# Select the Columns of MonthsProgramming and JobRoleInterest
time_and_interest_data_source <- data_source %>% select(MonthsProgramming, JobRoleInterest)

time_and_interest_data_source <- time_and_interest_data_source[time_and_interest_data_source$MonthsProgramming<=24,]

# Omit NA Values
time_and_interest_data_source <- time_and_interest_data_source[complete.cases(time_and_interest_data_source),]

# Third Plot Part B: Those Most Popular Job Roles among Coders based on How Long They've been Programming (Survey 2016)
time_and_interest_plot <- ggplot(time_and_interest_data_source, aes(x=JobRoleInterest,
                                                                    y=MonthsProgramming,
                                                                    color = JobRoleInterest)) +
  geom_point(alpha = 0.5, position = "jitter") +
  scale_colour_manual(values = preded_color_palette) +
  scale_y_continuous(breaks = seq(0,24,2)) +
  scale_size_area() +
  ggtitle("What are those Job Roles that People who are New to Programming Love") +
  coord_flip() +
  CustomTheme_Light

# Print the Plot
print(time_and_interest_plot)

