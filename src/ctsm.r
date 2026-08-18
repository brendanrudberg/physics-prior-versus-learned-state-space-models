library(ctsmTMB)

fit_ctsm <- function(model, training) {
  fit <- model$estimate(training, ode.timestep = 1/60)
  fit
}

predict_ctsm <- function(model, test_input) {
  d <- test_input                    # copy the input frame
  d$yTwi <- NA_real_                  # add blanked observation columns to d
  d$yTwo <- NA_real_
  pr <- model$predict(data = d, k.ahead = nrow(d) - 1, method = "ekf",
                      ode.timestep = 1/60, use.hessian = FALSE)
  list(
    t    = pr$observations[, "t.j"],
    yTwi = pr$observations[, "yTwi"],
    yTwo = pr$observations[, "yTwo"]
  )
}

extract_residuals <- function(fit) {
  res <- residuals(fit)          # CTSM's generic; must not be shadowed
  list(
    yTwi = res$normalized[, "yTwi"],
    yTwo = res$normalized[, "yTwo"]
  )
}