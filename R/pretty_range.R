#' Compute a nice looking range for a slider
#'
#' @param x an object coercible to numeric by as.numeric.
#'
#' @return A vector with two elements: the nice looking range
#'
#' @examples
#' range(ctd$temp)
#' pretty_range(ctd$temp)
#' range(ctd$date)
#' pretty_range(ctd$date)
pretty_range <- function(x) {
  r <- range(pretty(x, n=11))

  # old implementation
  # r <- range(x, na.rm=TRUE)
  # span <- diff(r)
  # # guess a good rounding interval based in the span of the data, e.g.
  # # data spanning 1 -> 10^6 can probably be rounded at 100
  # # data spanning 0 -> 0.0234 needs to be rounded at 0.01
  # if (span < 1) {
  #   n <- -nchar(prettyNum(round(1/span)))
  # } else {
  #   n <- max(0, nchar(prettyNum(round(span))) - 2)
  # }
  # accuracy <- 10^n
  # # TODO: deal with POSIXct separately and try to be clever about fate formatting based on the range; check what is in scales for this purpose
  # # use it to round numbers of POSIXct ranges
  # r <- c(plyr::round_any(r[1], accuracy, floor), plyr::round_any(r[2], accuracy, ceiling))

  return(r)
}