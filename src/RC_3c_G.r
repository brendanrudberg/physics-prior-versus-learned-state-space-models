library(ctsmTMB)

build_3c_G <- function(data) {

  model <- ctsmTMB$new()

  #Inner wall temperature ODE
  model$addSystem(dTwi ~ ((Tai - Twi)/(R1*(f1*Ctot)) + (Twm - Twi)/(R2*(f1*Ctot))) * dt + exp(p1)*dw1)

  #Middle wall temperature ODE
  model$addSystem(dTwm ~ ((Twi - Twm)/(R2*(f2*(1-f1)*Ctot)) + (Two - Twm)/(R3*(f2*(1-f1)*Ctot))) * dt + exp(p3)*dw3)

  #Outer wall temperature ODE
  model$addSystem(dTwo ~ ((Twm - Two)/(R3*((1-f2)*(1-f1)*Ctot)) + (Tao - Two)/(R4*((1-f2)*(1-f1)*Ctot)) + (Aw*G)/((1-f2)*(1-f1)*Ctot)) * dt + exp(p2)*dw2)

  # Adding observations based on the measured wall temperatures
  model$addObs(yTwi ~ Twi)
  model$addObs(yTwo ~ Two)

  # Setting the variance based on estimation error
  model$setVariance(yTwi ~ exp(e1))
  model$setVariance(yTwo ~ exp(e2))

  # Adding the inputs to the model based on the measured air temperatures
  model$addInput(Tai, Tao, G)

  model$setParameter(
    Ctot = c(initial = 0.30 * 2400 * 880 / 3.6e6), #total wall heat capacity (estimated constant)
    f1 = c(initial = 0.33, lower = 1e-3, upper = 0.999), #ratio between different wall section heat capacities
    f2 = c(initial = 0.5,  lower = 1e-3, upper = 0.999), #ratio between different wall section heat capacities
    R1  = c(initial = 1,  lower = 1e-3, upper = 1e3), # thermal resistance of inner wall
    R2  = c(initial = 1,  lower = 1e-3, upper = 1e3), # thermal resistance of middle layer
    R3  = c(initial = 1,  lower = 1e-3, upper = 1e3), # thermal resistance of outer wall
    R4  = c(initial = 1,  lower = 1e-3, upper = 1e3), # thermal resistance of outer wall
    p1  = c(initial = -1, lower = -30, upper = 10), # process noise for inner wall
    p2  = c(initial = -1, lower = -30, upper = 10), # process noise for outer wall
    p3  = c(initial = -1, lower = -30, upper = 10), # process noise for middle wall
    e1 = c(initial = -8), # measurement noise for inner wall (estimated constant)
    e2 = c(initial = -8), # measurement noise for outer wall (estimated constant)
    Aw = c(initial = 0.5, lower = 0, upper = 10) # Solar aperature constant
  )

  mid0 <- mean(c(data$yTwi[1], data$yTwo[1]))
  model$setInitialState(list(c(data$yTwi[1], mid0, data$yTwo[1]), diag(3)))

  model
}
