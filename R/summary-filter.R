#' Summarize Filtered Vehicle Data
#'
#' This function provides a comprehensive summary of the filtered dataset,
#' including average, median, min, max electric range, most common makes,
#' and vehicle type distribution as percentages.
#'
#' @param data A dataframe containing the filtered vehicle data.
#'   It should include columns for electric range, make, and electric vehicle type.
#'
#' @return A list with the following elements:
#'   \item{avg_electric_range}{Average electric range across the filtered dataset.}
#'   \item{median_electric_range}{Median electric range across the filtered dataset.}
#'   \item{min_electric_range}{Minimum electric range in the filtered dataset.}
#'   \item{max_electric_range}{Maximum electric range in the filtered dataset.}
#'   \item{top_common_makes}{Vector of the top one to three most common vehicle makes in the filtered dataset.}
#'   \item{vehicle_type_percentage}{Named vector showing the percentage distribution of vehicle types.}
#'
#' @export
summary_filtered_data <- function(data) {
  # Calculate basic statistics for electric range
  avg_range <- round(mean(data$electric_range, na.rm = TRUE), 2)
  median_range <- round(median(data$electric_range, na.rm = TRUE), 2)
  min_range <- round(min(data$electric_range, na.rm = TRUE), 2)
  max_range <- round(max(data$electric_range, na.rm = TRUE), 2)

  # Identify the top three most common makes
  make_freq <- sort(table(data$make), decreasing = TRUE)
  top_makes <- names(make_freq)[1:min(3, length(make_freq))]

  # Calculate vehicle type distribution as a percentage
  type_count <- table(data$electric_vehicle_type)
  type_percentage <- round(prop.table(type_count) * 100, 2)

  list(
    avg_electric_range = avg_range,
    median_electric_range = median_range,
    min_electric_range = min_range,
    max_electric_range = max_range,
    top_common_makes = top_makes,
    vehicle_type_percentage = type_percentage
  )
}
