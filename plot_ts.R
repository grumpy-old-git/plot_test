plot_ts <- function(x, y, ...) {
  fit <- mgcv::gam(y ~ s(x))
  plot(x, y, type = 'l', ...)
  lines(x, fitted(fit), col = 2)
}