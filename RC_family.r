fit_model <- function(model, data, fit_path) {
  if (file.exists(fit_path)) {
    fit <- readRDS(fit_path)
  } else {
    fit <- model$estimate(data, ode.timestep = 1/60)
    saveRDS(fit, fit_path)
  }
  fit
}