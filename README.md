
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ElectricVehiclesR

<!-- badges: start -->
<!-- badges: end -->

The goal of ElectricVehiclesR is to study the statistics behind electric
vehicles in the State of Washington in the US. An RShiny app provides
interactivity for the user to test combinations of variables and come
upon their own conclusions regarding electric vehicles.

## Installation

You can install the development version of ElectricVehiclesR from
[GitHub](https://github.com/ETC5523-2024/assignment-4-packages-and-shiny-apps-orchuk12)
with:

``` r
# install.packages("devtools")
devtools::install_github("ETC5523-2024/assignment-4-packages-and-shiny-apps-orchuk12")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r

library(ElectricVehiclesR)
library(tidyverse)

clean_vehicle |>
  group_by(model_year) |>
  summarize(average_range = mean(electric_range, na.rm = TRUE)) |>
  ggplot(aes(x = model_year, 
             y = average_range)) +
  geom_line() +
  geom_point() +
  labs(title = "Average Electric Range by Model Year",
       x = "Model Year",
       y = "Average Electric Range (miles)") +
  theme_bw()
```

<img src="man/figures/README-example-1.png" width="100%" />
