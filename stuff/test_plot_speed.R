library("microbenchmark")
library("hexbin")
library("ggplot2")

# Generate a largish data set
n <- 10^5+1
d <- data.frame(i=1:n, x=rnorm(n), y=rnorm(n))
d1 <- d
d2 <- d[sample.int(n, size=n/5),]


plot.base <- function() {
  png()
  plot(d$x, d$y, col="grey")
  points(d2$x, d2$y, col="black")
  dev.off()
}

plot.dot <- function() {
  png()
  plot(d$x, d$y, col="grey", pch=".")
  points(d2$x, d2$y, col="black", pch=".")
  dev.off()
}

plot.ggplot <- function() {
  png()
  print(ggplot(mapping=aes(x, y)) +
    geom_point(data=d1, col="grey") +
    geom_point(data=d2, col="black"))
  dev.off()
}

# plot.hexbin <- function() {
#   png()
#   plot(hexbin(d1$x, d1$y))
#   points(d2$x, d2$y, col="black")
#   dev.off()
# }

plot.smooth <- function() {
  png()
  smoothScatter(d1$x, d1$y, nrpoints=0, colramp=colorRampPalette(c("white", "grey20")))
  points(d2$x, d2$y, col="black", pch=".", cex=3)
  dev.off()
}


microbenchmark(
  plot.base(),
  plot.dot(),
  # plot.ggplot(),
  plot.smooth(),
  times=10
)
# -> dot is best for 10^4
#    smooth is better for more points



