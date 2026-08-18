build_PBL <- function(training) {
  last_i <- tail(training$yTwi, 1)
  last_o <- tail(training$yTwo, 1)
  list(
    yTwi = last_i,
    yTwo = last_o
  )
}

predict_PBL <- function(model, test_input) {
  n <- nrow(test_input)
  list(
    t    = test_input$t,
    yTwi = rep(model$yTwi, n),
    yTwo = rep(model$yTwo, n)
  )
}