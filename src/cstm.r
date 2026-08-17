library(ctsmTMB)

fit_ctsm <- function(model, data, fit_path) {
  if (file.exists(fit_path)) {
    fit <- readRDS(fit_path)
  } else {
    fit <- model$estimate(data, ode.timestep = 1/60)
    saveRDS(fit, fit_path)
  }
  fit
}

extract_residuals <- function(fit) {
  res <- residuals(fit)          # CTSM's generic; must not be shadowed
  list(
    yTwi = res$normalized[, "yTwi"],
    yTwo = res$normalized[, "yTwo"]
  )
}