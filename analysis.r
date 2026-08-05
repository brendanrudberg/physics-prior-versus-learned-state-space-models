# fit_wall_2c.R  --  2-state wall: Twi (inner), Two (outer)
library(ctsmTMB)

# columns: t, Tai, Tao (air inputs), yTwi, yTwo (measured wall temps)
dat <- readRDS("processed_data/temp_log_8_5_trimmed_2.rds")

model <- ctsmTMB$new()

model$addSystem(dTwi ~ ((Tai - Twi)/(R1*Cwi) + (Two - Twi)/(R2*Cwi)) * dt + exp(p1)*dw1)
model$addSystem(dTwo ~ ((Twi - Two)/(R2*Cwo) + (Tao - Two)/(R3*Cwo)) * dt + exp(p2)*dw2)

model$addObs(yTwi ~ Twi)
model$addObs(yTwo ~ Two)
model$setVariance(yTwi ~ exp(e1))
model$setVariance(yTwo ~ exp(e2))

model$addInput(Tai, Tao)

model$setParameter(
  Cwi = c(initial = 5,  lower = 1e-2, upper = 200),
  Cwo = c(initial = 5,  lower = 1e-2, upper = 200),
  R1  = c(initial = 1,  lower = 1e-3, upper = 50),
  R2  = c(initial = 1,  lower = 1e-3, upper = 50),
  R3  = c(initial = 1,  lower = 1e-3, upper = 50),
  p1  = c(initial = -1, lower = -30, upper = 10),
  p2  = c(initial = -1, lower = -30, upper = 10),
  e1  = c(initial = -3.5),
  e2  = c(initial = -3.5)
  #e1  = c(initial = -1, lower = -30, upper = 10),
  #e2  = c(initial = -1, lower = -30, upper = 10)
)

model$setInitialState(list(c(dat$yTwi[1], dat$yTwo[1]), diag(2)))

fit <- model$estimate(dat, ode.timestep = 1/60)
summary(fit)
cov2cor(fit$cov.fixed)