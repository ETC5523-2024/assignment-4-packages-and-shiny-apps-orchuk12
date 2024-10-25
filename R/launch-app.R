#' Loading the Rshiny app
#'
#' @description
#' This function launches the RShiny app which allows the user to explore the Electric Vehicle
#' data for themselves and come upon their own conclusions.
#'
#' @export
load_app <- function() {
  app_dir <- system.file("ev-app", package = "ElectricVehiclesR")
  shiny::runApp(app_dir, display.mode = "normal")
}
