library(ctsmTMB)
source("src/ctsm.r")
source("src/diag.r")
source("src/RC_2c.r")
source("src/RC_2c_G.r")
source("src/RC_3c_G.r")
source("src/LBB_2s.r")

if  (TRUE) {
training_data <- readRDS("processed_data/splits/train.rds")
model <- build_2c_G(data)
fit <- fit_ctsm(model, training_data)
saveRDS(fit, "processed_data/fits/fit_2c_G.rds")
saveRDS(model, "processed_data/models/model_2c_G.rds")
}

if (FALSE) {
### Fit diagnostics
fit_summary <- summary(fit)
fit_cor <- cov2cor(fit$cov.fixed)
res <- residuals(fit)
rr <- extract_residuals(fit)
plot_ACF(rr$yTwi, rr$yTwo)
}

if (TRUE) {
### Prediction using model
truth = readRDS("processed_data/splits/test_truth.rds")
input = readRDS("processed_data/splits/test_inputs.rds")
prediction = predict_ctsm(model, inputs)
plot_prediction(prediction, truth = truth, display_RMSE = TRUE, name = "2C_G")
}