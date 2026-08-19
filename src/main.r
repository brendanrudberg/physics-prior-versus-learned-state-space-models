library(ctsmTMB)
source("src/ctsm.r")
source("src/diag.r")
source("src/LBB_2s.r")
source("src/LBL.r")
source("src/PBL.r")
source("src/RC_2c_G.r")
source("src/RC_2c.r")
source("src/RC_3c_G.r")


geo_training <- readRDS("processed_data/geometric_splits/geometric_train.rds") # Read training data set
raw_training <- readRDS("processed_data/splits/train.rds") # Read training data set
#========================================
# White Box Models
#========================================

#========== Persistence baseline ==========
if (FALSE) {
model <- build_PBL(geo_training) # 
saveRDS(model, "processed_data/models/PBL.rds")
}

#========== Linear baseline ==========
if (FALSE) {
model <- build_LBL(geo_training) # Build linear baseline model
saveRDS(model, "processed_data/models/LBL.rds")
}

#========================================
# RC Gray Box Models
#========================================

#========== 2 capacitor model ==========
if (FALSE) {
model <- build_RC_2c(geo_training) # Build 2 capacitor model
fit <- fit_ctsm(model, geo_training)
saveRDS(model, "processed_data/models/model_RC_2c.rds")
saveRDS(fit, "processed_data/fits/fit_RC_2c.rds")
}

#========== 2 capacitor model with solar input ==========
if (FALSE) {
model <- build_RC_2c_G(geo_training) # Build 2 capacitor model with solar input
fit <- fit_ctsm(model, geo_training)
saveRDS(model, "processed_data/models/model_geometric_RC_2c_G.rds")
saveRDS(fit, "processed_data/fits/fit_geometric_RC_2c_G.rds")
}

#========== 3 capacitor model with solar input ==========
if (FALSE) {
model <- build_RC_3c_G(geo_training) # Build 3 capacitor model with solar input
fit <- fit_ctsm(model, geo_training)
saveRDS(model, "processed_data/models/model_RC_3c_G.rds")
saveRDS(fit, "processed_data/fits/fit_RC_3c_G.rds")
}

#========== 4 capacitor model with solar input ==========
if (FALSE) {
model <- build_RC_4c_G(geo_training) # Build 4 capacitor model with solar input
fit <- fit_ctsm(model, geo_training)
saveRDS(model, "processed_data/models/model_RC_4c_G.rds")
saveRDS(fit, "processed_data/fits/fit_RC_4c_G.rds")
}

#========================================
# Black Box Models
#========================================

#========== 2 capacitor model with solar input ==========
if (FALSE) {
model <- build_LBB_2s(geo_training) # Build linear black box model
fit <- fit_ctsm(model, geo_training)
saveRDS(model, "processed_data/models/model_LBB_2s2.rds")
saveRDS(fit, "processed_data/fits/fit_LBB_2s2.rds")
}

#========================================
# Model Selection
#========================================
if (TRUE) {
model <- readRDS("processed_data/models/model_LBB.rds")
fit <- readRDS("processed_data/fits/fit_LBB.rds")
}

#========================================
# Diagnostics
#========================================
if (FALSE) {
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
if (TRUE) {
  input <- readRDS("processed_data/splits/train.rds")
  truth <- input[, c("t", "yTwi", "yTwo")]         # the true wall temps
  in_only <- input[, c("t", "Tai", "Tao", "G")]     # inputs for prediction & overlay
  prediction <- predict_ctsm(model, in_only)
  plot_prediction(prediction, truth = truth, inputs = in_only,
                  display_RMSE = TRUE, name = "LBB")
}