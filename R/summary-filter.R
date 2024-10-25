#' Summarize Filtered Vehicle Data
#'
#' This function provides summary statistics for the filtered dataset.
#'
#' @param data The filtered dataset (dataframe).
#'
#' @return A list with summary statistics like average electric range, common makes, and count of vehicle types.
#' @export
summary_filtered_data <- function(data) {
  avg_range <- mean(data$electric_range, na.rm = TRUE)
  common_make <- names(sort(table(data$make), decreasing = TRUE))[1]
  type_count <- table(data$electric_vehicle_type)

  list(
    avg_electric_range = avg_range,
    most_common_make = common_make,
    vehicle_type_count = type_count
  )
}
