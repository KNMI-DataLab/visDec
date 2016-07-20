#' Properties of camera
#'
#' @description Obtained by read.csv("inst/extdata/properties.csv", stringsAsFactors = FALSE)
#' @format A data frame with 53940 rows and 10 variables:
#' \describe{
#'   \item{locationID}{indentifier of camera, integer}
#'   \item{stationID}{identifier of camera station KNMI id, string}
#'   \item{locationName}{Name of the location}
#'   \item{lon}{Longitude}
#'   \item{lat}{Latitude}
#'   \item{filePattern}{Pattern of the location name and date time the file name}
#'   \item{imagePrefix}{Name of sensor}
#' }
#' @note This should be improved
"properties"
