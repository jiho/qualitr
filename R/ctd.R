#' CTD profiles
#'
#' A dataset containing 82 CTD profiles between 0 and 80 m depth.
#'
#' @format A data frame with 6272 rows and 7 variables:
#' \describe{
#'   \item{id}{unique profile identifier (string)}
#'   \item{date}{date of the profile (Date)}
#'   \item{month}{month portion of the date, abbreviated (factor)}
#'   \item{depth}{depth of measurement, in m (string)}
#'   \item{temp, sal, fluo}{variables measured along the profiles: temperature in ºC, salinity in PSU, and fluorescence of Chl a in mg/m3 (numeric)}
#' }
#'
#' @source \url{http://www.obs-vlfr.fr/data/view/radehydro/ctd/}
"ctd"
