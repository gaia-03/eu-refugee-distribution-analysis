# Load necessary libraries
library(tidyverse)
library(geosphere)
library(refugees)
library(ggplot2)
library(countrycode)
library(rnaturalearth)
library(sf)

# Define the year for analysis
year_of_interest <- 2023

# Load the population dataset from the refugees package
data("population")

# Define European Union countries
eu_countries <- c(
  "Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czechia", "Denmark", 
  "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Ireland", 
  "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands", 
  "Poland", "Portugal", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden"
)

# Filter the dataset for EU countries and the specified year
eu_refugee_data <- population %>%
  filter(coa_name %in% eu_countries, !coo_name %in% eu_countries, year == year_of_interest) %>%
  group_by(coo_name, coa_name) %>%
  summarise(refugee_population = sum(refugees, na.rm = TRUE)) %>%
  ungroup()

# Get country coordinates from rnaturalearth
world <- ne_countries(scale = "medium", returnclass = "sf")
world_coords <- st_centroid(world) %>%
  st_coordinates() %>%
  as.data.frame() %>%
  mutate(country = world$name_long)

# Merge coordinates with EU and non-EU countries
non_eu_coords <- world_coords %>%
  filter(country %in% unique(eu_refugee_data$coo_name)) %>%
  rename(lat_coo = Y, lon_coo = X, coo_name = country)

eu_coords <- world_coords %>%
  filter(country %in% eu_countries) %>%
  rename(lat_coa = Y, lon_coa = X, coa_name = country)

# Calculate distances between non-EU origins and EU destinations
distance_data <- expand.grid(coo_name = unique(eu_refugee_data$coo_name), coa_name = eu_countries) %>%
  left_join(non_eu_coords, by = "coo_name") %>%
  left_join(eu_coords, by = "coa_name") %>%
  rowwise() %>%
  mutate(distance = distHaversine(c(lon_coo, lat_coo), c(lon_coa, lat_coa))) %>%
  ungroup() %>%
  select(coo_name, coa_name, distance)

# Check for NAs in distance data
sum(is.na(distance_data$distance))

# Merge data
gravity_data <- eu_refugee_data %>%
  left_join(distance_data, by = c("coo_name", "coa_name"))

# Check for NAs in merged data
sum(is.na(gravity_data$distance))

# Filter out rows with non-positive refugee_population and check for NAs
gravity_data <- gravity_data %>%
  filter(!is.na(refugee_population) & refugee_population > 0 & !is.na(distance))

# Log transform refugee_population
gravity_data <- gravity_data %>%
  mutate(log_refugee_population = log(refugee_population))

# Check for NAs in log_refugee_population
sum(is.na(gravity_data$log_refugee_population))

# Create linear model
lm_model <- lm(log_refugee_population ~ distance, data = gravity_data)

# Calculate the correlation coefficient
cor_coeff <- cor(gravity_data$log_refugee_population, gravity_data$distance, use = "complete.obs")

# Calculate y-axis limits
y_min <- min(gravity_data$refugee_population)
y_max <- max(gravity_data$refugee_population)
y_limit <- y_max * 1.25

# Visualization: Distance vs Refugee Population
ggplot(gravity_data, aes(x = distance, y = refugee_population)) +
  geom_point(color = "red", alpha = 0.3) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  scale_x_continuous(labels = scales::comma_format()) +
  scale_y_log10(labels = scales::comma_format(), limits = c(y_min, y_limit)) +
  theme_minimal() +
  labs(
    title = paste("Distance vs Refugee Population (Initial Entry) in", year_of_interest, "- logaritmic scale"),
    x = "Distance (meters)",
    y = "Refugee Population"
  ) +
  theme(legend.position = "bottom") +
  annotate("text", x = Inf, y = Inf, label = paste("R = ", round(cor_coeff, 3)), 
           hjust = 1.1, vjust = 2, size = 5)