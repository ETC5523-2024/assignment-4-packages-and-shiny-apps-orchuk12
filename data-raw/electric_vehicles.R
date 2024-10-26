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

## Creating grid_counts for leaflet map generation in shiny app

# Create the spatial data frame from clean_vehicle data
clean_vehicle_sf <- st_as_sf(clean_vehicle, coords = c("lon", "lat"), crs = 4326)

# Create a grid over the area with a specific cell size (e.g., 0.05 degrees)
grid <- st_make_grid(clean_vehicle_sf, cellsize = 0.05, square = TRUE)

# Convert the grid to an sf object
grid_sf <- st_as_sf(grid)

# Perform a spatial join to count the number of vehicles in each grid cell
grid_counts <- st_join(clean_vehicle_sf, grid_sf, join = st_intersects) %>%
  group_by(geometry) %>%
  summarize(count = n()) %>%
  st_as_sf()

# Simplifying the data for faster map generation
grid_counts_simplified <- st_simplify(grid_counts, dTolerance = 0.01)

# Locations of most populous cities to overlay on the map
cities <- data.frame(
  city = c("Seattle", "Olympia", "Tacoma", "Spokane"),
  lat = c(47.6062, 47.0379, 47.2529, 47.4588),
  lon = c(-122.3321, -122.9007, -122.4443, -117.4360)
)

usethis::use_data(clean_vehicle, overwrite = TRUE)
usethis::use_data(grid_counts_simplified, overwrite = TRUE)
usethis::use_data(cities, overwrite = TRUE)

