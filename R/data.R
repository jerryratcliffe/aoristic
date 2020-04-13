#' Burglaries, Washington DC, first six months of 2016
#'
#' A dataset containing the X and Y coordinates of burglaries, with
#' the start and end date and times of the crimes. Sourced from Open Data Washington DC
#' with start date in the first six months 2016. X, Y geometry for Washington DC is from
#' FIPS 1900 (crs 7058).
#'
#' @format A data frame with 1025 rows and 4 variables:
#' \describe{
#'   \item{X}{X coordinate of burglary location}
#'   \item{Y}{Y coordinate of burglary location}
#'   \item{StartDateTime}{Start date and time of the burglary in POSIXct format}
#'   \item{EndDateTime}{End date and time of the burglary in POSIXct format}
#' }
#' @source \url{https://opendata.dc.gov/}
"dcburglaries"
