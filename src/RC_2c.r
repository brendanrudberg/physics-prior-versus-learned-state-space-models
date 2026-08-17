library(ctsmTMB)

build_2c <- function(data) {

  model <- ctsmTMB$new()

  #Inner wall temperature ODE
  model$addSystem(dTwi ~ ((Tai - Twi)/(R1*(f*Ctot)) + (Two - Twi)/(R2*(f*Ctot))) * dt + exp(p1)*dw1)

  #Outer wall temperature ODE
  model$addSystem(dTwo ~ ((Twi - Two)/(R2*(Ctot-f*Ctot)) + (Tao - Two)/(R3*(Ctot-f*Ctot))) * dt + exp(p2)*dw2)

  # Adding observations based on the measured wall temperatures
  model$addObs(yTwi ~ Twi)
  model$addObs(yTwo ~ Two)

  # Setting the variance based on estimation error
  model$setVariance(yTwi ~ exp(e1))
  model$setVariance(yTwo ~ exp(e2))

  # Adding the inputs to the model based on the measured air temperatures
  model$addInput(Tai, Tao)

  model$setParameter(
    Ctot = c(initial = 0.30 * 2400 * 880 / 3.6e6), #total wall heat capacity (estimated constant)
    f   = c(initial = 0.5,  lower = 0, upper = 1), #ratio between different wall section heat capacities
    R1  = c(initial = 1,  lower = 1e-3, upper = 1e3), # thermal resistance of inner wall
    R2  = c(initial = 1,  lower = 1e-3, upper = 1e3), # thermal resistance of middle layer
    R3  = c(initial = 1,  lower = 1e-3, upper = 1e3), # thermal resistance of outer wall
    p1  = c(initial = -1, lower = -30, upper = 10), # process noise for inner wall
    p2  = c(initial = -1, lower = -30, upper = 10), # process noise for outer wall
    e1 = c(initial = -8),
    e2 = c(initial = -8)
  )

  model$setInitialState(list(c(data$yTwi[1], data$yTwo[1]), diag(2))) # Setting the initial state 
  # of the system based on the first measured values of Twi and Two, with a diagonal covariance matrix.

  model
}
