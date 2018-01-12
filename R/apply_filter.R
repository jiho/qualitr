#' Apply a filtering criterion
#'
#' @param d input data frame.
#' @param var variable name (as a quoted character string).
#' @param values vector of values used as filtering criterion: for qualitative variables, a vector of valid values; for quantitative variables, the valid range.
#'
#' @return A data frame, comprising only the subset of rows of the input data frame that match the filter.
#'
#' @examples
#' summary(ctd)
#' summary(apply_filter(ctd, "month", c("Mar", "Apr", "May")))
#' hist(ctd$sal, breaks=100)
#' ctd_clean <- apply_filter(ctd, "sal", c(37.7, 38.4))
#' hist(ctd_clean$sal, breaks=100)
#' @export
apply_filter <- function(d, var, values) {
  x <- d[[var]]
  if (is.character(x) | is.factor(x)) {
    # o <- dplyr::filter(d, rlang::UQ(rlang::sym(var)) %in% values)
    o <- d[which(d[[var]] %in% values),]
  } else {
    # o <- dplyr::filter(d, dplyr::between(rlang::UQ(rlang::sym(var)), values[1], values[2]))
    o <- d[which(d[[var]] >= values[1] & d[[var]] <= values[2]),]
  }
  return(o)
}
