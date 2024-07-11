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

# Data cleaning
# - get data for our objective (only eu countries)
# - get data from this century (from 2000 onward)
# - transform NA entries to 0
# - rename lengthy values
# - remove unnecessary columns
population_data <- data.frame(refugees::population) %>%
  filter(coa_name %in% eu_countries) %>%
  filter(year >= 2000) %>%
  mutate(across(where(anyNA), ~ replace_na(., 0))) %>%
  mutate(coa_name = ifelse(coa_name == "Netherlands (Kingdom of the)", "Netherlands", coa_name)) %>%
  mutate(coo_name = ifelse(coo_name == "Netherlands (Kingdom of the)", "Netherlands", coo_name)) %>%
  select(c(year, coo_name, coa_name, refugees))

head(population_data)

# Primary movement data
pop_data_pri_mov <- population_data %>%
  filter(!coo %in% eu_countries_tag)

head(pop_data_pri_mov)

# Secondary movement data
pop_data_sec_mov <- population_data %>%
  filter(coo %in% eu_countries_tag)
  
head(pop_data_sec_mov)

# Sec mov data only from last year available
latest_year <- max(population$year, na.rm = TRUE)

pop_data_sec_mov_latest <- pop_data_sec_mov %>%
    filter(year == latest_year) %>%
    select(-year)

head(pop_data_sec_mov_latest)
