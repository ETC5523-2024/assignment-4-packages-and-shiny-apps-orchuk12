
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ElectricVehiclesR

<!-- badges: start -->
<!-- badges: end -->

The goal of the ElectricVehiclesR package is to study the statistics
behind electric vehicles in the State of Washington in the United States
of America. An RShiny app is included within this package and provides
interactivity for the user to view various plots and filter them
according to their will and test combinations of criteria and come upon
their own personal electric vehicles options.

You can view the in-depth guide to this package on its
[website](https://etc5523-2024.github.io/assignment-4-packages-and-shiny-apps-orchuk12/).

NOTE: Upon launching the RShiny app, it may appear to be laggy, slow, or
the plots may not be appearing. Please allow the app approximately 15-30
seconds to boot up correctly. This is caused by the various large
datasets that this app depends upon.

## Installation

You can install the development version of ElectricVehiclesR from
[GitHub](https://github.com/ETC5523-2024/assignment-4-packages-and-shiny-apps-orchuk12)
with:

``` r
# install.packages("devtools")
devtools::install_github("ETC5523-2024/assignment-4-packages-and-shiny-apps-orchuk12")
```

## Example

This is a basic example which shows you how to produce some plots with
the main dataset `clean_vehicle`:

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
