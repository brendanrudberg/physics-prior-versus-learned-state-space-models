library(ctsmTMB)

fit_ctsm <- function(model, training) {
  fit <- model$estimate(training, ode.timestep = 1/60)
  fit
}

predict_ctsm <- function(model, test_inputs) {
  d <- test_inputs                 # has t, Tai, Tao, G
  d$yTwi <- NA_real_               # add blanked observation columns
  d$yTwo <- NA_real_
  pr <- fit$predict(data = d, k.ahead = nrow(d) - 1, method = "ekf")
  list(yTwi = ..., yTwo = ...)     # extract forward-predicted trajectory
}

extract_residuals <- function(fit) {
  res <- residuals(fit)          # CTSM's generic; must not be shadowed
  list(
    yTwi = res$normalized[, "yTwi"],
    yTwo = res$normalized[, "yTwo"]
  )
}