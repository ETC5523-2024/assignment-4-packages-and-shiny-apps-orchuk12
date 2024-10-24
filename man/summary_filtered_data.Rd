% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summary-filter.R
\name{summary_filtered_data}
\alias{summary_filtered_data}
\title{Summarize Filtered Vehicle Data}
\usage{
summary_filtered_data(data)
}
\arguments{
\item{data}{A dataframe containing the filtered vehicle data.
It should include columns for electric range, make, and electric vehicle type.}
}
\value{
A list with the following elements:
\item{avg_electric_range}{Average electric range across the filtered dataset.}
\item{median_electric_range}{Median electric range across the filtered dataset.}
\item{min_electric_range}{Minimum electric range in the filtered dataset.}
\item{max_electric_range}{Maximum electric range in the filtered dataset.}
\item{top_common_makes}{Vector of the top one to three most common vehicle makes in the filtered dataset.}
\item{vehicle_type_percentage}{Named vector showing the percentage distribution of vehicle types.}
}
\description{
This function provides a comprehensive summary of the filtered dataset,
including average, median, min, max electric range, most common makes,
and vehicle type distribution as percentages.
}
