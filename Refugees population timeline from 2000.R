# Load the libraries
library(tidyverse)
library(ggplot2)
library(refugees)
library(RColorBrewer)

# Load the population dataset from the refugees package
data("population")

# Define European Union countries
eu_countries <- c(
  "Austria",
  "Belgium",
  "Bulgaria",
  "Croatia",
  "Cyprus",
  "Czechia",
  "Denmark",
  "Estonia",
  "Finland",
  "France",
  "Germany",
  "Greece",
  "Hungary",
  "Ireland",
  "Italy",
  "Latvia",
  "Lithuania",
  "Luxembourg",
  "Malta",
  "Netherlands (Kingdom of the)",
  "Poland",
  "Portugal",
  "Romania",
  "Slovakia",
  "Slovenia",
  "Spain",
  "Sweden"
)

# Filter the dataset for EU countries and years after 1970
eu_refugee_data <- population %>%
  filter(coa_name %in% eu_countries, year >= 2000) %>%
  mutate(coa_name = ifelse(coa_name == "Netherlands (Kingdom of the)", "Netherlands", coa_name)) %>%
  mutate(coo_name = ifelse(coo_name == "Netherlands (Kingdom of the)", "Netherlands", coo_name)) %>%
  group_by(year, coa_name) %>%
  summarise(refugee_population = sum(refugees, na.rm = TRUE)) %>%
  ungroup()

# Define a color palette
num_countries <- length(unique(eu_refugee_data$coa_name))
extended_colors <- my_colors <- colorRampPalette(brewer.pal(12, "Paired"))(num_countries)

# Plot graph
ggplot(eu_refugee_data,
       aes(x = year, y = refugee_population, color = coa_name)) +
  geom_line(size=1.1) +
  scale_color_manual(values = extended_colors) +
  scale_x_continuous(breaks = seq(min(eu_refugee_data$year), max(eu_refugee_data$year), by = 1)) +
  scale_y_continuous(labels = scales::comma_format(), breaks = seq(0, max(eu_refugee_data$refugee_population, na.rm = TRUE), by = 500000))+
  theme_minimal() +
  labs(
    title = "Refugee Population by Year in EU Countries",
    x = "Year",
    y = "Refugee Population",
    color = "Country"
  )