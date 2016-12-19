require(ggplot2)
require(dplyr)
source("CustomTheme_Light.R")

# Read Data Source From CSV
data_source <- read.csv("2016-FCC-New-Coders-Survey-Data.csv", stringsAsFactors = FALSE)

second_color_palette <- c("#FD326D", "#2E97DE")

# Select the Columns of Age and Gender, merge small gender groups into one category
age_and_income_data_source <- data_source %>% select(Age, Income, Gender)

age_and_income_data_source$Gender[age_and_income_data_source$Gender == 'agender'] <- 'other gender groups'
age_and_income_data_source$Gender[age_and_income_data_source$Gender == 'trans'] <- 'other gender groups'
age_and_income_data_source$Gender[age_and_income_data_source$Gender == 'genderqueer'] <- 'other gender groups'

age_and_income_data_source <- age_and_income_data_source %>% group_by(Age, Income)

# Omit NA Values among Income
age_and_income_data_source <- age_and_income_data_source[complete.cases(age_and_income_data_source),]

# Second Plot Part A: The Age and Income Distribution (Gender) of Coders (Survey 2016)
age_income_plot <- ggplot(age_and_income_data_source, aes(Age, fill = factor(Gender, levels=c("other gender groups","female", "male")), 
                                                          color = factor(Gender, levels=c("other gender groups","female", "male")))) +
  scale_fill_manual(values = preded_color_palette) +
  scale_color_manual(values = preded_color_palette) +
  geom_density(alpha = 0.35) +
  # facet_wrap(~Gender) +
  ggtitle("Income Analysis through Age based on Diffent Gender Groups") +
  CustomTheme_Light +
  guides(fill=FALSE, color=FALSE)

# Print the Plot
print(age_income_plot)

# Select the Columns of Age, Income and JobRoleInterest
jobrole_and_income_data_source <- data_source %>% select(Age, Income, JobRoleInterest) %>%
  group_by(Age, Income)

# Omit NA Values among Income
jobrole_and_income_data_source <- jobrole_and_income_data_source[complete.cases(jobrole_and_income_data_source),]

# Second Plot Part B: The Age and Income Distribution (Job Role) of Coders (Survey 2016)
jobrole_income_plot <- ggplot(jobrole_and_income_data_source, aes(Age, fill=JobRoleInterest, color=JobRoleInterest)) +
  scale_fill_manual(values = preded_color_palette) +
  scale_color_manual(values = preded_color_palette) +
  geom_density(alpha = 0.35) +
  # facet_wrap(~JobRoleInterest) +
  ggtitle("Income Analysis through Age based on Diffent Job Roles") +
  CustomTheme_Light

# Print the Plot
print(jobrole_income_plot)

# Select the Columns of MoneyForLearning and Income
budget_and_income_data_source <- data_source %>% select(MoneyForLearning, Income, JobRoleInterest)

# Remove Exceeding Data from Data Source
budget_and_income_data_source <- budget_and_income_data_source[budget_and_income_data_source$MoneyForLearning<=25000,]

# Omit NA Values
budget_and_income_data_source <- budget_and_income_data_source[complete.cases(budget_and_income_data_source),]

# Categorized into groups based on level of income
budget_and_income_data_source$level_of_income <- cut(budget_and_income_data_source$Income,
                       breaks = c(-Inf, 25000, 50000, 75000, 100000, 125000, 150000, 175000, Inf),
                       labels = c("0-25k", "25k-50k", "50k-75k", "75k-100k", "100k-125k", "125k-150k", "150k-175k", "175k-200k"),
                       right = FALSE)

# Second Plot Part C: How Do Learning Budget and Income Balanced among Coders (Survey 2016)
budget_and_income_plot <-  ggplot(budget_and_income_data_source, aes(x=MoneyForLearning,
                                                                     y=Income)) +
  geom_bin2d(binwidth = c(500, 1000)) +
  scale_y_continuous(breaks = seq(0,200000,10000)) +
  scale_fill_gradient2(low = "#2C3E51",
                       mid = "#2E97DE",
                       high = "#00FF72",
                       midpoint = 20,
                       guide = "colourbar") +
  ggtitle("How Do Learning Budget and Income Balance on Different Job Roles") +
  # facet_wrap(~JobRoleInterest) +
  CustomTheme_Light

# Print the Plot
print(budget_and_income_plot)

budget_and_income_boxplot <- ggplot(budget_and_income_data_source, aes(x = level_of_income,
                                                                     y = MoneyForLearning,
                                                                     fill = JobRoleInterest)) +
  geom_boxplot(outlier.colour = "black", colour = "black") +
  scale_fill_manual(values = preded_color_palette) +
  facet_wrap(~JobRoleInterest) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ggtitle("Different Job Role Interests based on Age - Box Plot") +
  CustomTheme_Light

# Print the Plot
print(budget_and_income_boxplot)

# Select the Columns of MoneyForLearning and Income
expect_and_income_data_source <- data_source %>% select(Age, Income, ExpectedEarning)

# Omit NA Values among Income
expect_and_income_data_source <- expect_and_income_data_source[complete.cases(expect_and_income_data_source),]

# Make a New Column of Difference between Income and Expected Earning
expect_and_income_data_source$ExpectedGap <- (expect_and_income_data_source$ExpectedEarning - expect_and_income_data_source$Income)

# Second Plot Part D: The Difference between Income and Expected Earning Among Coders
expect_and_income_plot <- ggplot(expect_and_income_data_source, aes(x=Age,
                                                                    y=ExpectedGap)) +
  geom_point(color = "#FD326D", alpha = 0.5) +
  geom_smooth(color = "#00FF72") +
  scale_size_area() +
  ggtitle("The Gap between Income and Expected Earning among Coders") +
  CustomTheme_Light

# Print the Plot
print(expect_and_income_plot)

expect_and_income_data_source$gap_category <- cut(expect_and_income_data_source$ExpectedGap,
                                                     breaks = c(-Inf, 0, Inf),
                                                     labels = c("less", "more"),
                                                     right = FALSE)

expect_and_income_segment_plot <- ggplot(expect_and_income_data_source, aes(y = Age)) +
                                  geom_point(aes(x = Age, y = Income), color = "#FD326D", alpha = 0.5) +
                                  geom_point(aes(x = Age, y = ExpectedEarning),  color = "#2E97DE", alpha = 0.5) +
                                  geom_segment(aes(x = Age,
                                                   y = Income,
                                                   xend = Age,
                                                   yend = ExpectedEarning,
                                                   color = gap_category,
                                                   alpha = 0.5)) +
                                  scale_color_manual(values = second_color_palette) +
                                  CustomTheme_Light

print(expect_and_income_segment_plot)
