# Load the libraries
library(tidyverse)
library(igraph)
library(ggplot2)
library(refugees)

# Load the population dataset from the refugees package
data("population")

# Define European Union countries
eu_countries <- c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czech Republic", "Denmark", "Estonia",
                  "Finland", "France", "Germany", "Greece", "Hungary", "Ireland", "Italy", "Latvia", "Lithuania",
                  "Luxembourg", "Malta", "Netherlands", "Poland", "Portugal", "Romania", "Slovakia", "Slovenia",
                  "Spain", "Sweden")

# Simulated data for secondary movements (replace with actual data if available)
set.seed(123)

eu_refugee_movements <- population %>%
  filter(coa_name %in% eu_countries, coo_name %in% eu_countries, coo_name != coa_name, year==2023) %>%
  group_by(coo_name, coa_name) %>%
  summarise(refugee_population = sum(refugees, na.rm = TRUE)) %>%
  ungroup()

# Find the top 5 countries with the highest intake
top_countries <- eu_refugee_movements %>%
  group_by(coa_name) %>%
  summarise(total_refugees = sum(refugee_population)) %>%
  arrange(desc(total_refugees)) %>%
  top_n(5, wt = total_refugees) %>%
  pull(coa_name)

# Create a directed graph from the data
refugee_graph <- graph_from_data_frame(d = eu_refugee_movements, directed = TRUE)

# Calculate the total intake for each country
total_intake <- degree(refugee_graph, mode = "in")
V(refugee_graph)$intake <- total_intake

# Create the layout
layout <- layout_in_circle(refugee_graph)
# Find the indices of the top countries
center_indices <- match(top_countries, V(refugee_graph)$name)

# Place the top countries near the center but not overlapping
center_positions <- cbind(c(0, 0.3, -0.3, 0.3, -0.3), c(0, 0.3, 0.3, -0.3, -0.3))
layout[center_indices, ] <- center_positions

# Spread out other countries in a circle around the center
non_center_indices <- setdiff(seq_len(vcount(refugee_graph)), center_indices)
angle <- seq(0, 2 * pi, length.out = length(non_center_indices) + 1)[-1]
circle_layout <- matrix(c(cos(angle), sin(angle)), ncol = 2)
layout[non_center_indices, ] <- circle_layout

# Define color scale based on the number of intakes
intake_range <- range(V(refugee_graph)$intake)
colors <- colorRampPalette(c("blue", "red"))(diff(intake_range) + 1)
V(refugee_graph)$color <- colors[V(refugee_graph)$intake - intake_range[1] + 1]

# Plot the network graph
plot(refugee_graph,
     layout = layout,
     vertex.size = 15, # Increase the size of the nodes to fit the names
     vertex.label.cex = 1, # Adjust the label size for better readability
     vertex.label.dist = 2.5, # Adjust the distance of the labels from the nodes
     vertex.color = V(refugee_graph)$color,
     edge.arrow.size = 0.5,
     main = "Network Graph of Refugee Movements within the European Union")