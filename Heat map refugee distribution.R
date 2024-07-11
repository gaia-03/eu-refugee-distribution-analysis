# Load the libraries
library(tidyverse)
library(ggplot2)
library(igraph)
library(refugees)

# Load the population dataset from the refugees package
data("population")

# Define European Union countries
eu_countries <- c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czechia", "Denmark", "Estonia",
                  "Finland", "France", "Germany", "Greece", "Hungary", "Ireland", "Italy", "Latvia", "Lithuania",
                  "Luxembourg", "Malta", "Netherlands (Kingdom of the)", "Poland", "Portugal", "Romania", "Slovakia", "Slovenia",
                  "Spain", "Sweden")

# Filter for the latest year
latest_year <- max(population$year, na.rm = TRUE)

eu_refugee_data <- population %>%
  filter(coa_name %in% eu_countries, year == 2023) %>%
  mutate(coa_name = ifelse(coa_name == "Netherlands (Kingdom of the)", "Netherlands", coa_name)) %>%
  mutate(coo_name = ifelse(coo_name == "Netherlands (Kingdom of the)", "Netherlands", coo_name)) %>%
  group_by(coa_name, coo_name) %>%
  summarise(refugee_population = sum(refugees, na.rm = TRUE)) %>%
  ungroup()

# Filter for top 30 countries of origin with the highest refugee populations
top_30_coo <- eu_refugee_data %>%
  filter(!coo_name=="Unknown") %>%
  # mutate(coo_name = ifelse(coo_name == "Iran (Islamic Rep. of)", "Iran", coo_name)) %>%
  group_by(coo_name) %>%
  summarise(total_refugee_population = sum(refugee_population)) %>%
  top_n(20, wt = total_refugee_population) %>%
  pull(coo_name)

eu_refugee_data_top_30 <- eu_refugee_data %>%
  filter(coo_name %in% top_30_coo)
# Sort the rows in descending order by total refugee population
eu_refugee_data_top_30 <- eu_refugee_data_top_30 %>%
  arrange(desc(refugee_population))
eu_refugee_data_top_30 <- eu_refugee_data_top_30 %>%
  mutate(coo_name = substr(coo_name, 1, 7))


# Exploratory Data Analysis
# Summary statistics
summary(eu_refugee_data_top_30)

# Heat map of refugees distribution
ggplot(eu_refugee_data_top_30, aes(x = coa_name, y = coo_name, fill = refugee_population)) +
  geom_tile(color = "white") +
  theme_minimal() +
  labs(title = "Heat Map of Refugee Distribution in EU (Top 30 Countries of Origin)", x = "Country", y = "", fill = "Population") +
  scale_fill_gradient(low = "lightyellow", high = "red") +
  coord_flip()