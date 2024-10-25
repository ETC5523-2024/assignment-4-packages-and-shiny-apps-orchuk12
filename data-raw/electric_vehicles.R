## code to prepare `electric_vehicles` dataset goes here

library(tidyverse)
library(janitor)

# Loading raw data
electric_vehicle_raw <- read_csv("data-raw/Electric_Vehicle_Population_Data.csv")

# Removing all missing observations and cleaning variable names
clean_vehicle_raw <- electric_vehicle_raw %>%
  drop_na() %>%
  clean_names()

# Filtering the necessary data
clean_vehicle <- clean_vehicle_raw %>%
  filter(model_year >= 2010) %>%
  filter(state == "WA")

# Deselecting vin-number
clean_vehicle <- clean_vehicle %>%
  select(county, city, state, postal_code, model_year, make, model, electric_vehicle_type,
         clean_alternative_fuel_vehicle_cafv_eligibility, electric_range, vehicle_location)

# Cleaning location variables
clean_vehicle <- clean_vehicle %>%
  mutate(vehicle_location = str_remove_all(vehicle_location, "POINT \\(|\\)"))

clean_vehicle <- clean_vehicle %>%
  mutate(lon = as.numeric(str_extract(vehicle_location, "^[-\\d\\.]+")),
         lat = as.numeric(str_extract(vehicle_location, "[-\\d\\.]+$")))

# Converting model_year and electric_range into an integer
clean_vehicle <- clean_vehicle %>%
  mutate(model_year = as.integer(model_year),
         electric_range = as.integer(electric_range))

usethis::use_data(clean_vehicle, overwrite = TRUE)
