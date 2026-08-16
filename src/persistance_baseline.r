predict_persistence_baseline <- function(train_path, start, end, dt = 0.5) {
  train <- readRDS(train_path)

  # persistence anchor: last observed wall temps at end of training
  last_i <- tail(train$yTwi, 1)
  last_o <- tail(train$yTwo, 1)

  # number of test timesteps from start to end (exclusive of end, half-open)
  n <- round((end - start) / dt)

  list(
    yTwi = rep(last_i, n),
    yTwo = rep(last_o, n)
  )