# Load the libraries
library(tidyverse)
library(ggplot2)
library(igraph)
library(refugees)

# Load the population dataset from the refugees package
data("population")

# Filter the dataset for European Union countries and the latest year
eu_countries <- c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czechia", "Denmark", "Estonia",
                  "Finland", "France", "Germany", "Greece", "Hungary", "Ireland", "Italy", "Latvia", "Lithuania",
                  "Luxembourg", "Malta", "Netherlands (Kingdom of the)", "Poland", "Portugal", "Romania", "Slovakia", "Slovenia",
                  "Spain", "Sweden")

latest_year <- max(population$year, na.rm = TRUE)

eu_refugee_data <- population %>%
  filter(coa_name %in% eu_countries, year == latest_year) %>%
  mutate(coa_name = ifelse(coa_name == "Netherlands (Kingdom of the)", "Netherlands", coa_name)) %>%
  mutate(coo_name = ifelse(coo_name == "Netherlands (Kingdom of the)", "Netherlands", coo_name)) %>%
  group_by(coa_name) %>%
  summarise(refugee_population = sum(refugees, na.rm = TRUE)) %>%
  ungroup()

# Exploratory Data Analysis
# Summary statistics
summary(eu_refugee_data)

# Heat map of refugees distribution
ggplot(eu_refugee_data, aes(x = coa_name, y = "", fill = refugee_population)) +
  geom_tile(color = "white") +
  theme_minimal() +
  labs(title = "Heat Map of Refugee Distribution in EU", x = "Country", y = "", fill = "Population") +
  scale_fill_gradient(low = "yellow", high = "red")+
  coord_flip()
