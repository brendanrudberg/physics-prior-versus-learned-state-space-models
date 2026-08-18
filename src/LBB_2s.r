library(ctsmTMB)

build_LBB_2s <- function(data) {

  model <- ctsmTMB$new()
  #x* = Ax + Bu + w
  model$addSystem(dTwi ~ (A11*Twi + A12*Two + B11*Tai + B12*Tao + B13*G)*dt + exp(p1)*dw1) 
  model$addSystem(dTwo ~ (A21*Twi + A22*Two + B21*Tai + B22*Tao + B23*G)*dt + exp(p2)*dw2)

  # Adding observations based on the measured wall temperatures
  model$addObs(yTwi ~ Twi)
  model$addObs(yTwo ~ Two)

  # Setting the variance based on estimation error
  model$setVariance(yTwi ~ exp(e1))
  model$setVariance(yTwo ~ exp(e2))

  # Adding the inputs to the model based on the measured air temperatures
  model$addInput(Tai, Tao, G)

  model$setParameter(
    A11 = c(initial = 0, lower = -10, upper = 10),
    A12 = c(initial = 0, lower = -10, upper = 10),
    A21 = c(initial = 0, lower = -10, upper = 10),
    A22 = c(initial = 0, lower = -10, upper = 10),
    B11 = c(initial = 0, lower = -10, upper = 10),
    B12 = c(initial = 0, lower = -10, upper = 10),
    B21 = c(initial = 0, lower = -10, upper = 10),
    B22 = c(initial = 0, lower = -10, upper = 10),
    B13 = c(initial = 0, lower = -10, upper = 10),
    B23 = c(initial = 0, lower = -10, upper = 10),
    p1 = c(initial = -1, lower = -30, upper = 10),
    p2 = c(initial = -1, lower = -30, upper = 10),
    e1 = c(initial = -8),
    e2 = c(initial = -8)
  )

  model$setInitialState(list(c(data$yTwi[1], data$yTwo[1]), diag(2))) # Setting the initial state
  # of the system based on the first measured values of Twi and Two, with a diagonal covariance matrix.

  model
}
