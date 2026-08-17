plot_ACF <- function(rw_i, rw_o, name = NULL) {
  if (!is.null(name)) png(sprintf("figures/acf_%s.png", name), 1000, 800)
  old <- par(mfrow = c(2, 2)); on.exit(par(old))
  acf(rw_i, lag.max = 200, na.action = na.pass, main = paste("ACF inner", name))
  acf(rw_o, lag.max = 200, na.action = na.pass, main = paste("ACF outer", name))
  ri <- rw_i[!is.na(rw_i)]; ro <- rw_o[!is.na(rw_o)]
  cpgram(ri); cpgram(ro)
  if (!is.null(name)) dev.off()
}

plot_prediction <- function(prediction, truth = NULL, inputs = NULL,
                            name = NULL, display_RMSE = FALSE) {
  # prediction: list with t, yTwi, yTwo
  # truth:      optional data.frame with t, yTwi, yTwo (overlay + RMSE)
  # inputs:     optional data.frame with t, Tai, Tao (air temps, same axis as walls)
  # name:       optional title label
  # display_RMSE: if TRUE and truth given, prints per-wall RMSE top-left

  old <- par(mfrow = c(2, 1), mar = c(4, 4, 2, 1))
  on.exit(par(old))

  rmse_txt <- function(pred, obs) {
    sprintf("RMSE = %.3f", sqrt(mean((pred - obs)^2, na.rm = TRUE)))
  }

  add_air <- function() {
    if (is.null(inputs)) return(invisible())
    lines(inputs$t, inputs$Tai, col = adjustcolor("grey60", 0.5))
    lines(inputs$t, inputs$Tao, col = adjustcolor("grey30", 0.5))
  }

  # --- inner wall (y-range spans prediction, truth, AND air temps) ---
  yr_i <- range(c(prediction$yTwi, truth$yTwi, inputs$Tai, inputs$Tao), na.rm = TRUE)
  plot(prediction$t, prediction$yTwi, type = "l", col = "blue", ylim = yr_i,
       xlab = "t (hours)", ylab = "temperature (deg C)",
       main = paste("Inner wall: prediction vs truth", name))
  add_air()
  if (!is.null(truth)) lines(truth$t, truth$yTwi, col = "black")
  if (display_RMSE && !is.null(truth))
    legend("topleft", legend = rmse_txt(prediction$yTwi, truth$yTwi), bty = "n")
  legend("topright",
         legend = c("prediction", "truth", "Tai", "Tao"),
         col = c("blue", "black", adjustcolor("grey60",0.7), adjustcolor("grey30",0.7)),
         lty = 1, bty = "n")

  # --- outer wall ---
  yr_o <- range(c(prediction$yTwo, truth$yTwo, inputs$Tai, inputs$Tao), na.rm = TRUE)
  plot(prediction$t, prediction$yTwo, type = "l", col = "red", ylim = yr_o,
       xlab = "t (hours)", ylab = "temperature (deg C)",
       main = paste("Outer wall: prediction vs truth", name))
  add_air()
  if (!is.null(truth)) lines(truth$t, truth$yTwo, col = "black")
  if (display_RMSE && !is.null(truth))
    legend("topleft", legend = rmse_txt(prediction$yTwo, truth$yTwo), bty = "n")
  legend("topright",
         legend = c("prediction", "truth", "Tai", "Tao"),
         col = c("red", "black", adjustcolor("grey60",0.7), adjustcolor("grey30",0.7)),
         lty = 1, bty = "n")
}

# Residuals: prediction - truth, per wall. Aligns on t if both carry it.
compute_residuals <- function(prediction, test_truth) {
  # length guard: predictions and truth must line up 1:1
  stopifnot(length(prediction$yTwi) == nrow(test_truth),
            length(prediction$yTwo) == nrow(test_truth))
  # optional alignment check if prediction carries timestamps
  if (!is.null(prediction$t)) {
    stopifnot(all(abs(prediction$t - test_truth$t) < 1e-6))
  }
  list(
    yTwi = prediction$yTwi - test_truth$yTwi,
    yTwo = prediction$yTwo - test_truth$yTwo
  )
}

# Single accuracy metric: RMSE per wall (root mean squared error).
# Squaring prevents +/- cancellation, so this measures accuracy, not bias.
compute_rmse <- function(residuals) {
  list(
    yTwi = sqrt(mean(residuals$yTwi^2, na.rm = TRUE)),
    yTwo = sqrt(mean(residuals$yTwo^2, na.rm = TRUE))
  )
}