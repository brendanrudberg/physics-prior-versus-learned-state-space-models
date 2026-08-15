plot_ACF <- function(rw_i, rw_o, name = NULL) {
  if (!is.null(name)) png(sprintf("figures/acf_%s.png", name), 1000, 800)
  old <- par(mfrow = c(2, 2)); on.exit(par(old))
  acf(rw_i, lag.max = 200, na.action = na.pass, main = paste("ACF inner", name))
  acf(rw_o, lag.max = 200, na.action = na.pass, main = paste("ACF outer", name))
  ri <- rw_i[!is.na(rw_i)]; ro <- rw_o[!is.na(rw_o)]
  cpgram(ri); cpgram(ro)
  if (!is.null(name)) dev.off()
}