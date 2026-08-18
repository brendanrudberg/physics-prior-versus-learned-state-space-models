library(ctsmTMB)
source("src/ctsm.r")
source("src/diag.r")
source("src/RC_2c.r")
source("src/RC_2c_G.r")
source("src/RC_3c_G.r")
source("src/LBL.r")
source("src/LBB_2s.r")
source("src/PBL.r")

if (FALSE) {
training <- readRDS("processed_data/splits/train.rds")
model <- build_LBB_2s(training)
fit <- fit_ctsm(model, training)
saveRDS(fit, "processed_data/fits/fit_LBB_2s")
saveRDS(model, "processed_data/models/fit_LBB_2s")
}

if (FALSE) {
training <- readRDS("processed_data/splits/train.rds")
model <- build_2c_G(training)
fit <- fit_ctsm(model, training)
saveRDS(fit, "processed_data/fits/fit_2c_G.rds")
saveRDS(model, "processed_data/models/model_2c_G.rds")
}

if (FALSE) {
training <- readRDS("processed_data/splits/train.rds")
model <- build_PBL(training)
saveRDS(model, "processed_data/models/PBL.rds")
}

if (FALSE) {
training <- readRDS("processed_data/splits/train.rds")
model <- build_LBL(training)
saveRDS(model, "processed_data/models/LBL.rds")
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
input = readRDS("processed_data/splits/test_input.rds")
prediction = predict_ctsm(model, input)
plot_prediction(prediction, truth = truth, display_RMSE = TRUE, name = "LBB_2s")
}