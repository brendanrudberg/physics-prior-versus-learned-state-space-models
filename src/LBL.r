fit_linear_baseline <- function(train_data) {
  fit_i <- lm(yTwi ~ Tai + Tao + G, data = train_data)
  fit_o <- lm(yTwo ~ Tai + Tao + G, data = train_data)
  list(fit_i = fit_i, fit_o = fit_o)
}

predict_linear_baseline <- function(fit, test_inputs) {
  pred_i <- predict(fit$fit_i, newdata = test_inputs)
  pred_o <- predict(fit$fit_o, newdata = test_inputs)
  list(yTwi = pred_i, yTwo = pred_o)
}