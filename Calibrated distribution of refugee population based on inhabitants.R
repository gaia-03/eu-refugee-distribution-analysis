# Load the necessary libraries
library(tidyverse)
library(ggplot2)
library(igraph)
library(refugees)
library(WDI)
# Define the list of EU countries
eu_countries <- c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czechia", "Denmark", "Estonia",
                  "Finland", "France", "Germany", "Greece", "Hungary", "Ireland", "Italy", "Latvia", "Lithuania",
                  "Luxembourg", "Malta", "Netherlands", "Poland", "Portugal", "Romania", "Slovak Republic", "Slovenia",
                  "Spain", "Sweden")

# Fetch population data for all countries
# Indicator "SP.POP.TOTL" corresponds to total population
population_data <- WDI(indicator = "SP.POP.TOTL", start = 2023, end = 2023) %>%
  filter(country %in% eu_countries) %>%
  select(c(country, SP.POP.TOTL)) %>%
  rename("population_size" = "SP.POP.TOTL") %>%
  mutate(country = ifelse(country == "Slovak Republic", "Slovakia", country))

# Display the first few rows of the population data
head(population_data, n=50)

# Filter the dataset for European Union countries and the latest year
eu_countries <- c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czechia", "Denmark", "Estonia",
                  "Finland", "France", "Germany", "Greece", "Hungary", "Ireland", "Italy", "Latvia", "Lithuania",
                  "Luxembourg", "Malta", "Netherlands (Kingdom of the)", "Poland", "Portugal", "Romania", "Slovakia", "Slovenia",
                  "Spain", "Sweden")

latest_year <- max(population$year, na.rm = TRUE)

eu_refugee_data <- population %>%
  filter(coa_name %in% eu_countries, year == latest_year) %>%
  mutate(coa_name = ifelse(coa_name == "Netherlands (Kingdom of the)", "Netherlands", coa_name)) %>%
  group_by(coa_name) %>%
  summarise(refugee_population = sum(refugees, na.rm = TRUE)) %>%
  ungroup()

print(eu_refugee_data, n=50)

# Merge with country sizes data
eu_refugee_data <- eu_refugee_data %>%
  left_join(population_data, by = c("coa_name" = "country"))

# Calculate the calibrated refugee population (e.g., refugees per million inhabitants)
eu_refugee_data <- eu_refugee_data %>%
  mutate(calibrated_population = (refugee_population / population_size) * 1000000)

# Summary statistics
summary(eu_refugee_data)

# Visualization
# Bar plot of calibrated refugee populations by country
ggplot(eu_refugee_data, aes(x = reorder(coa_name, -calibrated_population), y = calibrated_population)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme_minimal() +
  labs(title = "Calibrated Refugee Populations in EU Countries (per million inhabitants)",
       x = "Country",
       y = "Calibrated Refugee Population (per million inhabitants)") +
  scale_y_continuous(labels = scales::comma_format(), breaks = seq(0, max(eu_refugee_data$calibrated_population), by = 10000)) +
  coord_flip()
