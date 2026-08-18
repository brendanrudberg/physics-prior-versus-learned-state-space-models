library(ctsmTMB)
source("src/ctsm.r")
source("src/diag.r")
source("src/LBB_2s.r")
source("src/LBL.r")
source("src/PBL.r")
source("src/RC_2c_G.r")
source("src/RC_2c.r")
source("src/RC_3c_G.r")


training <- readRDS("processed_data/geometric_splits/geometric_train.rds") # Read training data set
#========================================
# White Box Models
#========================================

#========== Persistence baseline ==========
if (FALSE) {
model <- build_PBL(training) # 
saveRDS(model, "processed_data/models/PBL.rds")
}

#========== Linear baseline ==========
if (FALSE) {
model <- build_LBL(training) # Build linear baseline model
saveRDS(model, "processed_data/models/LBL.rds")
}

#========================================
# RC Gray Box Models
#========================================

#========== 2 capacitor model ==========
if (FALSE) {
model <- build_RC_2c(training) # Build 2 capacitor model
fit <- fit_ctsm(model, training)
saveRDS(model, "processed_data/models/fit_RC_2c")
saveRDS(fit, "processed_data/fits/fit_RC_2c")
}

#========== 2 capacitor model with solar input ==========
if (FALSE) {
model <- build_RC_2c_G(training) # Build 2 capacitor model with solar input
fit <- fit_ctsm(model, training)
saveRDS(model, "processed_data/models/model_RC_2c_G.rds")
saveRDS(fit, "processed_data/fits/fit_RC_2c_G.rds")
}

#========== 3 capacitor model with solar input ==========
if (FALSE) {
model <- build_RC_3c_G(training) # Build 3 capacitor model with solar input
fit <- fit_ctsm(model, training)
saveRDS(model, "processed_data/models/model_RC_3c_G.rds")
saveRDS(fit, "processed_data/fits/fit_RC_3c_G.rds")
}

#========================================
# Black Box Models
#========================================

#========== 3 capacitor model with solar input ==========
if (FALSE) {
model <- build_LBB(training) # Build linear black box model
fit <- fit_ctsm(model, training)
saveRDS(model, "processed_data/models/model_LBB.rds")
saveRDS(fit, "processed_data/fits/fit_LBB.rds")
}

#========================================
# Diagnostics
#========================================
if (TRUE) {
### Fit diagnostics
fit_summary <- summary(fit)
fit_cor <- cov2cor(fit$cov.fixed)
res <- residuals(fit)
rr <- extract_residuals(fit)
plot_ACF(rr$yTwi, rr$yTwo)
}

#========================================
# Prediction
#========================================
if (FALSE) {
### Prediction using model
truth = readRDS("processed_data/splits/test_truth.rds")
input = readRDS("processed_data/splits/test_input.rds")
prediction = predict_ctsm(model, input)
plot_prediction(prediction, truth = truth, display_RMSE = TRUE, name = "LBB_2s")
}