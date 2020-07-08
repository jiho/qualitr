
files <- list.files(".", pattern="RData")

a <- function(n, N) {
  1 - 0.8 * min(n/N, 1)
}

format_range <- function(x) {
  # compute range
  r <- range(x, na.rm=T)
  # compute span of range and shift it by 2 orders of magnitude
  d <- diff(r) * 100
  n <- 2
  # continue shifting it until the span is large enough
  while(d < 1 & n < 10) {
    message(d)
    d <- d * 10
    n <- n + 1
  }
  # return the output
  paste0("[", round(r[1], n), ",", round(r[2], n), "]")
}
