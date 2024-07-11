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
  filter(coa_name %in% eu_countries, year == 2023) %>%
  mutate(coa_name = ifelse(coa_name == "Netherlands (Kingdom of the)", "Netherlands", coa_name)) %>%
  mutate(coo_name = ifelse(coo_name == "Netherlands (Kingdom of the)", "Netherlands", coo_name)) %>%
  group_by(coa_name) %>%
  summarise(refugee_population = sum(refugees, na.rm = TRUE)) %>%
  ungroup()
num_countries <- length(unique(eu_refugee_data$coa_name))

my_colors <- colorRampPalette(brewer.pal(12, "Paired"))(num_countries)

# Create the pie chart
ggplot(eu_refugee_data, aes(x = "", y = refugee_population, fill = coa_name)) +
  geom_bar(width = 1, stat = "identity") +
  
  coord_polar(theta = "y") +
  scale_fill_manual(values = my_colors) +
  labs(title = "Refugee Population Distribution in EU Countries (2023)",
       fill = "Country") +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5))