# fit_wall_2c.R  --  2-state wall: Twi (inner), Two (outer)
library(ctsmTMB)

# columns: t, Tai, Tao (air inputs), yTwi, yTwo (measured wall temps)
dat <- readRDS("processed_data/temp_log_8_5_trimmed_2.rds")

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
  Ctot = c(initial = 10), #total wall heat capacity (estimated constant)
  f = c(initial = 0.5,  lower = 0, upper = 1), #ratio between different wall section heat capacities
  R1  = c(initial = 1,  lower = 1e-3, upper = 50), # thermal resistance of inner wall
  R2  = c(initial = 1,  lower = 1e-3, upper = 50), # thermal resistance of middle layer
  R3  = c(initial = 1,  lower = 1e-3, upper = 50), # thermal resistance of outer wall
  p1  = c(initial = -1, lower = -30, upper = 10), # process noise for inner wall
  p2  = c(initial = -1, lower = -30, upper = 10), # process noise for outer wall
  e1  = c(initial = -5), # measurement noise for inner wall (estimated constant)
  e2  = c(initial = -5) # measurement noise for outer wall (estimated constant)
)

model$setInitialState(list(c(dat$yTwi[1], dat$yTwo[1]), diag(2))) # Setting the initial state 
# of the system based on the first measured values of Twi and Two, with a diagonal covariance matrix.

# Fitting the model (new file if it doesn't exist, otherwise read from file)
fit_path <- "processed_data/fit_wall_2c_Ctot5_e-5.rds"

if (file.exists(fit_path)) {
  fit <- readRDS(fit_path)
} else {
  fit <- model$estimate(dat, ode.timestep = 1/60)
  saveRDS(fit, fit_path)
}

# Summarizing the fit and extracting residuals
fit_summary <- summary(fit)
fit_cor <- cov2cor(fit$cov.fixed)
res <- residuals(fit)
str(res)
class(res)

# normalized residuals: standardized one-step innovations
rw_i <- res$normalized[, "yTwi"]   # standardized one-step innovations, inner wall
rw_o <- res$normalized[, "yTwo"]   # outer walls

# Plotting the autocorrelation function (ACF) and cumulative periodogram for the residuals
par(mfrow = c(2, 2))        # 2x2 grid, four panels
acf(rw_i, lag.max = 200, na.action = na.pass, main = "ACF inner")
acf(rw_o, lag.max = 200, na.action = na.pass, main = "ACF outer")
cpgram(rw_i[!is.na(rw_i)])
cpgram(rw_o[!is.na(rw_o)])
par(mfrow = c(1, 1))        # reset when done

# noise calibration: mean ~0, variance ~1 if the fixed measurement noise was honest
sapply(list(yTwi = rw_i, yTwo = rw_o),
       function(r) c(mean = mean(r, na.rm = TRUE), var = var(r, na.rm = TRUE)))