source("src/diag.r") 
source("src/RC_2c.r")
source("src/RC_family.r")
source("src/RC_2c_G.r")


#data <- readRDS("processed_data/temp_log_8_5_trimmed_2.rds")
#model <- build_2c(data)
#fit <- fit_model(model, data, "processed_data/fit_wall_2c_Ctot_e-8_solar.rds")


data <- readRDS("processed_data/temp_log_8_5_solar.rds")
model <- build_2c_G(data)
fit <- fit_model(model, data, "processed_data/fit_wall_2c_Ctot_e-8_solar.rds")


fit_summary <- summary(fit)
fit_cor <- cov2cor(fit$cov.fixed)
res <- residuals(fit)
str(res)
class(res)

rr <- extract_residuals(fit)
plot_ACF(rr$yTwi, rr$yTwo)