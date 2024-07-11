# Install required packages
if (!require("refugees")) install.packages("refugees")
if (!require("igraph")) install.packages("igraph")

# Load libraries
library(refugees)
library(igraph)
library(dplyr)

# Get the dataset
data("population")
head(population)

# Select 20 countries of origin (coo) in descending order of refugees
coo_countries <- population %>%
  filter(!is.na(refugees), year == 2023) %>%
  group_by(coo_name) %>%
  summarize(total_refugees = sum(refugees, na.rm = TRUE)) %>%
  arrange(desc(total_refugees)) %>%
  slice_head(n = 20) %>%
  pull(coo_name)

# Select 5 unique EU countries as arrival countries (coa)
eu_countries <- c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czechia", "Denmark", "Estonia",
                  "Finland", "France", "Germany", "Greece", "Hungary", "Ireland", "Italy", "Latvia", "Lithuania",
                  "Luxembourg", "Malta", "Netherlands (Kingdom of the)", "Poland", "Portugal", "Romania", "Slovakia", "Slovenia",
                  "Spain", "Sweden")

coa_countries <- population %>%
  filter(coa_name %in% eu_countries, year == 2023) %>%
  group_by(coa_name) %>%
  summarize(total_refugees = sum(refugees, na.rm = TRUE)) %>%
  arrange(desc(total_refugees)) %>%
  slice_max(total_refugees, n = 5) %>%
  pull(coa_name)

# Create edges ensuring no duplicate coo and coa combination
edges <- population %>%
  # mutate(coo_name = ifelse(coo_name == "Venezuela (Bolivarian Republic of)", "Venezuela", coo_name)) %>%
  filter(!coo_name=="Unknown") %>%
  filter(coo_name %in% coo_countries, coa_name %in% coa_countries, year == 2023) %>%
  select(from = coo_name, to = coa_name) %>%
  distinct(from, to)

# Create the graph
g <- graph_from_data_frame(edges, directed = TRUE)

# Get the vertices
vertices <- V(g)$name

# Ensure all coa_countries and coo_countries are in the vertices
valid_coa_countries <- coa_countries[coa_countries %in% vertices]
valid_coo_countries <- coo_countries[coo_countries %in% vertices]

# Create a custom layout
layout <- matrix(0, nrow = length(vertices), ncol = 2)
num_coa <- length(valid_coa_countries)
num_coo <- length(valid_coo_countries)

# Place coa countries in a small circle in the middle
inner_radius <- 4 # Radius for the small circle
inner_angle <- seq(0, 2 * pi, length.out = num_coa + 1)[-1]
layout[match(valid_coa_countries, vertices), ] <- cbind(inner_radius * cos(inner_angle), inner_radius * sin(inner_angle))

# Place coo countries in a larger circle around the coa countries
outer_radius <- 10 # Radius for the larger circle
outer_angle <- seq(0, 2 * pi, length.out = num_coo + 1)[-1]
layout[match(valid_coo_countries, vertices), ] <- cbind(outer_radius * cos(outer_angle), outer_radius * sin(outer_angle))

# Plot the graph with more spread out nodes
plot(g, layout = layout, vertex.label.cex = 0.7, edge.arrow.size = 0.3, vertex.size = 30,
     vertex.color = ifelse(V(g)$name %in% valid_coo_countries, "lightblue", "salmon"),
     main = "Refugee Flow from Origin to Arrival Countries")