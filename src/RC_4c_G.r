library(ctsmTMB)

build_RC_4c_G <- function(training) {

  model <- ctsmTMB$new()
  #T1: Inside air temperature
  #T2: Inner wall temperature
  #T3: Inner-middle wall temperature
  #T4: Middle-outer wall temperature
  #T5: Outer wall temperature
  #T6: Outside air temperature

  #Inner wall temperature ODE
  model$addSystem(dT2 ~ ((Tai - T2)/(R1*(f1*Ctot)) + (T3 - T2)/(R2*(f1*Ctot))) * dt + exp(p1)*dw1)

  #Middle-inner wall temperature ODE
  model$addSystem(dT3 ~ ((T2 - T3)/(R2*(f2*(1-f1)*Ctot)) + (T4 - T3)/(R3*(f2*(1-f1)*Ctot))) * dt + exp(p2)*dw2)

  #Middle-outer wall temperature ODE
  model$addSystem(dT4 ~ ((T3 - T4)/(R3*(f2*(1-f1)*Ctot)) + (T5 - T4)/(R4*(f2*(1-f1)*Ctot))) * dt + exp(p3)*dw2)

  #Outer wall temperature ODE
  model$addSystem(dT5 ~ ((T4 - T5)/(R4*((1-f2)*(1-f1)*Ctot)) + (Tao - T5)/(R5*((1-f2)*(1-f1)*Ctot)) + (Aw*G)/((1-f2)*(1-f1)*Ctot)) * dt + exp(p4)*dw3)

  # Adding observations based on the measured wall temperatures
  model$addObs(yTwi ~ T2)
  model$addObs(yTwo ~ T5)

  # Setting the variance based on estimation error
  model$setVariance(yTwi ~ exp(e1))
  model$setVariance(yTwo ~ exp(e2))

  # Adding the inputs to the model based on the measured air temperatures
  model$addInput(Tai, Tao, G)   # not addInput(T1, T6, G)

  model$setParameter(
    Ctot = c(initial = 0.30 * 2400 * 880 / 3.6e6), #total wall heat capacity (estimated constant)
    f1 = c(initial = 0.33, lower = 1e-3, upper = 0.999), #ratio between different wall section heat capacities
    f2 = c(initial = 0.5,  lower = 1e-3, upper = 0.999), #ratio between different wall section heat capacities
    R1  = c(initial = 1,  lower = 1e-3, upper = 1e3), # thermal resistance of inside air-inner wall
    R2  = c(initial = 1,  lower = 1e-3, upper = 1e3), # thermal resistance of inner wall-middle inner wall
    R3  = c(initial = 1,  lower = 1e-3, upper = 1e3), # thermal resistance of middle inner wall-middle outer wall
    R4  = c(initial = 1,  lower = 1e-3, upper = 1e3), # thermal resistance of middle outer wall-outer wall
    R5  = c(initial = 1,  lower = 1e-3, upper = 1e3), # thermal resistance of outer wall-outside air
    p1  = c(initial = -1, lower = -30, upper = 10), # process noise for inner wall
    p2  = c(initial = -1, lower = -30, upper = 10), # process noise for inner-middle wall
    p3  = c(initial = -1, lower = -30, upper = 10), # process noise for middle-outer wall
    p4  = c(initial = -1, lower = -30, upper = 10), # process noise for outer wall
    e1 = c(initial = -8), # measurement noise for inner wall (estimated constant)
    e2 = c(initial = -8), # measurement noise for outer wall (estimated constant)
    Aw = c(initial = 0.5, lower = 0, upper = 10) # Solar aperature constant
  )

  # observed surface temps at t=0
  Ti <- training$yTwi[1]     # inner wall (T2), observed
  To <- training$yTwo[1]     # outer wall (T5), observed

  # latent middle nodes: linear interpolation between inner and outer
  # T2 -- T3 -- T4 -- T5 evenly spaced, so T3 at 1/3, T4 at 2/3
  T3_0 <- Ti + (To - Ti) * (1/3)
  T4_0 <- Ti + (To - Ti) * (2/3)

  x0 <- c(Ti, T3_0, T4_0, To)          # four states: T2, T3, T4, T5
  model$setInitialState(list(x0, diag(4)))

  model
}
