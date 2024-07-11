# Load required packages
library(refugees)
library(tidyverse)
library(ggplot2)

# Filter the dataset for European Union countries and the latest year
eu_countries <- c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czechia", "Denmark", "Estonia",
                  "Finland", "France", "Germany", "Greece", "Hungary", "Ireland", "Italy", "Latvia", "Lithuania",
                  "Luxembourg", "Malta", "Netherlands (Kingdom of the)", "Poland", "Portugal", "Romania", "Slovakia", "Slovenia",
                  "Spain", "Sweden")

# Import data on population
population_data <- data.frame(refugees::population) %>%
  filter(coa_name %in% eu_countries) %>%
  filter(year == 2023) %>%
  select(c(year, coo_name, coa_name, refugees, returned_refugees))

head(population_data)

# Primary movement data
pop_data_pri_mov <- population_data %>%
  filter(!coo_name %in% eu_countries) %>%
  filter(!coo_name=="Unknown") %>%
  group_by(coo_name) %>%
  summarise(refugee_population = sum(refugees, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(coo_name = ifelse(coo_name == "Venezuela (Bolivarian Republic of)", "Venezuela", coo_name)) %>%
  mutate(coo_name = ifelse(coo_name == "Iran (Islamic Rep. of)", "Iran", coo_name)) %>%
  filter(refugee_population >= 50000)

head(pop_data_pri_mov)

# Plot graph
ggplot(pop_data_pri_mov, aes(x = reorder(coo_name, -refugee_population), y = refugee_population)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme_minimal() +
  labs(title = "Top 10 Refugee Populations in EU Countries", x = "Country", y = "Refugee Population") +
  scale_y_continuous(labels = scales::comma_format(), breaks = seq(0, max(pop_data_pri_mov$refugee_population), by = 500000)) +
  coord_flip()
