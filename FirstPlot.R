require(ggplot2)
require(dplyr)
source("CustomTheme_Light.R")

# Read Data Source From CSV
data_source <- read.csv("2016-FCC-New-Coders-Survey-Data.csv", stringsAsFactors = FALSE)

# Select the Columns of Age and Gender, merge small gender groups into one category
age_and_gender_data_source <- data_source %>% select(Age, Gender)

age_and_gender_data_source$Gender[age_and_gender_data_source$Gender == 'agender'] <- 'other gender groups'
age_and_gender_data_source$Gender[age_and_gender_data_source$Gender == 'trans'] <- 'other gender groups'
age_and_gender_data_source$Gender[age_and_gender_data_source$Gender == 'genderqueer'] <- 'other gender groups'

age_and_gender_data_source <- age_and_gender_data_source %>% group_by(Age, Gender) %>%
                              summarize(Amount_Of_People = n())

# Omit NA Values
age_and_gender_data_source <- age_and_gender_data_source[complete.cases(age_and_gender_data_source),]

# First Plot Part A: The Age and Gender Distribution of Coders (Survey 2016)
age_gender_plot <- ggplot(age_and_gender_data_source,
                          aes(Age, Amount_Of_People, fill = factor(Gender, levels=c("other gender groups","female", "male")))) +
  scale_fill_manual(values = preded_color_palette) +
  geom_bar(stat = "identity", color = "white", position = "stack") +
  scale_y_continuous(breaks=pretty(age_and_gender_data_source$Amount_Of_People, n=10)) +
  scale_x_continuous(breaks = seq(10,70,10)) +
  scale_size_area() +
  ggtitle("Age and Gender Distribution of Code Survey 2016") +
  CustomTheme_Light +
  guides(fill=FALSE)

# Print the Plot
print(age_gender_plot)

# Select the Columns of City Population and Gender, merge small gender groups into one category
population_and_gender_data_source <- data_source %>% select(CityPopulation, Gender)

population_and_gender_data_source$Gender[population_and_gender_data_source$Gender == 'agender'] <- 'other gender groups'
population_and_gender_data_source$Gender[population_and_gender_data_source$Gender == 'trans'] <- 'other gender groups'
population_and_gender_data_source$Gender[population_and_gender_data_source$Gender == 'genderqueer'] <- 'other gender groups'

population_and_gender_data_source <- population_and_gender_data_source %>% group_by(CityPopulation, Gender) %>%
  summarize(Amount_Of_People = n())

# Omit NA Values among Income
population_and_gender_data_source <- population_and_gender_data_source[complete.cases(population_and_gender_data_source),]

# First Plot Part B: The Population and Gender Distribution of Coders (Survey 2016)
population_gender_plot <- ggplot(population_and_gender_data_source,
                          aes(CityPopulation, Amount_Of_People, fill =factor(Gender, levels=c("other gender groups","female", "male")))) +
  scale_fill_manual(values = preded_color_palette) +
  geom_bar(stat = "identity", color = "white", position = "stack") +
  scale_y_continuous(breaks=pretty(population_and_gender_data_source$Amount_Of_People, n=10)) +
  scale_size_area() +
  ggtitle("City Population and Gender Distribution") +
  CustomTheme_Light +
  guides(fill=FALSE)

# Print the Plot
print(population_gender_plot)
