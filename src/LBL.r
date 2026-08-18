build_LBL <- function(training) {
  fit_i <- lm(yTwi ~ Tai + Tao + G, data = training)
  fit_o <- lm(yTwo ~ Tai + Tao + G, data = training)
  list(fit_i = fit_i, fit_o = fit_o)
}

predict_LBL <- function(fit, test_input) {
  pred_i <- predict(fit$fit_i, newdata = test_input)
  pred_o <- predict(fit$fit_o, newdata = test_input)
  list(t = test_input$t,
   yTwi = pred_i,
   yTwo = pred_o)
}