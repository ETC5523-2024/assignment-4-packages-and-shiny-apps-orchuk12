#' @export
load_app <- function() {
  app_dir <- system.file("ev-app", package = "ElectricVehiclesR")
  shiny::runApp(app_dir, display.mode = "normal")
}
