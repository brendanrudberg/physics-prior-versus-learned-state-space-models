source("src/diag.r") 
source("src/RC_2c.r")
source("src/RC_family.r")
source("src/RC_2c_G.r")
source("src/RC_3c_G.r")

data <- readRDS("processed_data/splits/train.rds")
model <- build_2c_G(data)
fit <- fit_RC(model, data, "processed_data/splits/fit_train.rds")


fit_summary <- summary(fit)
fit_cor <- cov2cor(fit$cov.fixed)
res <- residuals(fit)
str(res)
class(res)

rr <- extract_residuals(fit)
plot_ACF(rr$yTwi, rr$yTwo)