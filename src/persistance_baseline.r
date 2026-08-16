fit_persistence_baseline <- function(train_data) {

  last_i <- tail(train_data$yTwi, 1)
  last_o <- tail(train_data$yTwo, 1)
  list(
    yTwi = last_i,
    yTwo = last_o
  )
}

predict_persistence_baseline <- function(fit, test_inputs) {
  n <- nrow(test_inputs)
  list(
    yTwi = rep(fit$yTwi, n),
    yTwo = rep(fit$yTwo, n)
  )
}