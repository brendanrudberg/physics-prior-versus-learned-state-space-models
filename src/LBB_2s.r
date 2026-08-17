library(ctsmTMB)

build_lbb_2s <- function(data) {

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
    A11 = c(initial = 0, lower = -10, upper = 10), # Inner wall temperature coefficient
    A12 = c(initial = 0, lower = -10, upper = 10), # Outer wall temperature coefficient
    A21 = c(initial = 0, lower = -10, upper = 10), # Inner wall temperature coefficient
    A22 = c(initial = 0, lower = -10, upper = 10), # Outer wall temperature coefficient
    B11 = c(initial = 0, lower = -10, upper =  10), # Inner wall air temperature coefficient
    B12 = c(initial = 0, lower = -10, upper =  10), # Outer wall air temperature coefficient
    B21 = c(initial = 0, lower = -10, upper =  10), # Inner wall air temperature coefficient
    B22 = c(initial = 0, lower = -10, upper =   10), # Outer wall air temperature coefficient
    B13 = c(initial = 0, lower = -10, upper =   10), # Inner wall solar coefficient
    B23 = c(initial = 0, lower = -10, upper =   10), # Outer wall solar coefficient
    p1 = c(initial = -1, lower = -30, upper = 10), # process noise for inner wall
    p2 = c(initial = -1, lower = -30, upper = 10), # process noise for outer wall
    e1 = c(initial = -8),
    e2 = c(initial = -8)
)

  model$setInitialState(list(c(data$yTwi[1], data$yTwo[1]), diag(2))) # Setting the initial state 
  # of the system based on the first measured values of Twi and Two, with a diagonal covariance matrix.

  model
}

predict_lbb <- function(model, data) {
  model$predict(data)
}
