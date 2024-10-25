#' @title ElectricVehiclesR: Examining Electric Vehicle Data in Washington State, USA.
#'
#' @description This package includes an RShiny app that analyzes electric vehicles in
#' the State of Washington, USA. The app allows users to test combinations of variables
#' and reach their own conclusions regarding electric vehicles.
#'
#' @details The package contains the following functions:
#' \itemize{
#'   \item \code{\link{filter_vehicles}}: Filters the dataset based on electric range, make, and vehicle type.
#'   \item \code{\link{summary_filtered_data}}: Generates summary statistics for the filtered data.
#'   \item \code{\link{load_app}}: Launches the Shiny app to interactively explore electric vehicle options.
#' }
#'
#' @section Data Source:
#' The data was extracted from Kaggle and is titled "Electric Vehicle Population".
#' You can access it [here](https://www.kaggle.com/datasets/jainaru/electric-vehicle-population/data).
#'
#' @keywords package
"_PACKAGE"
