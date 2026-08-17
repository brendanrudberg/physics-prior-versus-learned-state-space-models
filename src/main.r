library(ctsmTMB)
source("src/ctsm.r")
source("src/diag.r")
source("src/RC_2c.r")
source("src/RC_2c_G.r")
source("src/RC_3c_G.r")

data <- readRDS("processed_data/splits/train.rds")
model <- build_2c(data)
fit <- fit_ctsm(model, data)
saveRDS(fit, "processed_data/fits/fit_2c.rds")
saveRDS(model, "processed_data/models/model_2c.rds")

### Diagnostics
fit_summary <- summary(fit)
fit_cor <- cov2cor(fit$cov.fixed)
res <- residuals(fit)
str(res)
class(res)
rr <- extract_residuals(fit)
plot_ACF(rr$yTwi, rr$yTwo)


### Prediction
d <- readRDS("processed_data/splits/test_inputs.rds")
d$yTwi <- NA_real_               # add blanked observation columns
d$yTwo <- NA_real_
pr <- model$predict(data = d, k.ahead = nrow(d) - 1, method = "ekf")
str(pr)
head(pr)