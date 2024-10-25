#' Filter Vehicles Based on User Preferences
#'
#' This function filters the dataset based on user-selected electric range,
#' car make, type of vehicle, and model year.
#'
#' @param data The dataset to filter (dataframe).
#' @param mileage_range A numeric vector of length 2 indicating the range of desired electric mileage.
#' @param car_make A character vector of car makes to include.
#' @param vehicle_type A character vector of vehicle types to include.
#' @param year_range A numeric vector of length 2 indicating the range of desired model years.
#'
#' @return A filtered dataframe based on the user's provided criteria.
#' @export
filter_vehicles <- function(data, mileage_range = c(0, 500), car_make = NULL, vehicle_type = NULL, year_range = NULL) {
  data_filtered <- data |>
    dplyr::filter(
      electric_range >= mileage_range[1],
      electric_range <= mileage_range[2],
      (is.null(car_make) | toupper(make) %in% toupper(car_make)),
      (is.null(vehicle_type) | toupper(electric_vehicle_type) %in% toupper(vehicle_type)),
      (is.null(year_range) | model_year %in% year_range)
    )

  return(data_filtered)
}
