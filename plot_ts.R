plot_ts <- function(x, y, annoy_brian = FALSE, ...) {
  library(ggplot2)
  if (annoy_brian) {
    ggplot(aes(x, y), data = data.frame(x, y)) + 
      geom_line() + 
      geom_smooth() +
      theme_minimal()
  } else {
    fit <- mgcv::gam(y ~ s(x))
    plot(x, y, type = 'l', ...)
    lines(x, fitted(fit), col = 2, lwd = 2)
  }
}
