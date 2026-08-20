library(ctsmTMB)
source("src/ctsm.r")
source("src/diag.r")
source("src/LBB_2s.r")
source("src/LBL.r")
source("src/PBL.r")
source("src/RC_2c_G.r")
source("src/RC_2c.r")
source("src/RC_3c_G.r")
source("src/RC_4c_G.r")

# ============================================================
# CONFIG
# ============================================================
VARIANT     <- "geo"        # "geo" or "raw"  -> solar treatment + data source
MODEL       <- "RC_3c_G"     # "PBL","LBL","RC_2c","RC_2c_G","RC_3c_G","RC_4c_G","LBB_2s"
BUILD       <- TRUE         # build + fit the model, save model/fit .rds
PLOT_ACF    <- TRUE         # write in-sample residual ACF figure (ctsm models only)
PLOT_PRED   <- TRUE         # write prediction-vs-truth figure
OVERWRITE   <- TRUE        # if FALSE, skip writing a figure that already exists
# ============================================================

# ---- data + suffix from VARIANT ----
if (VARIANT == "geo") {
  training <- readRDS("processed_data/geometric_splits/geometric_train.rds")
  test     <- readRDS("processed_data/geometric_splits/geometric_test.rds")
  sfx      <- "_geo"
} else {
  training <- readRDS("processed_data/splits/train.rds")
  test     <- readRDS("processed_data/splits/test.rds")
  sfx      <- "_raw"
}

# ---- builders keyed by MODEL ----
BUILDERS <- list(
  PBL     = build_PBL,
  LBL     = build_LBL,
  RC_2c   = build_RC_2c,
  RC_2c_G = build_RC_2c_G,
  RC_3c_G = build_RC_3c_G,
  RC_4c_G = build_RC_4c_G,
  LBB_2s  = build_LBB_2s
)
# which models are ctsm (need fitting + support ACF/predict) vs plain baselines
IS_CTSM <- !(MODEL %in% c("PBL", "LBL"))

# ---- path helpers, all tagged <MODEL><sfx> ----
mp        <- function() paste0("processed_data/models/model_", MODEL, sfx, ".rds")
fp        <- function() paste0("processed_data/fits/fit_",     MODEL, sfx, ".rds")
acf_path  <- function() paste0("figures/ACF/ACF_",             MODEL, sfx, ".png")
pred_path <- function() paste0("figures/predictions/prediction_", MODEL, sfx, ".png")

for (d in c("processed_data/models", "processed_data/fits",
            "figures/ACF", "figures/predictions"))
  dir.create(d, recursive = TRUE, showWarnings = FALSE)

# write a figure via `plot_fn`, honouring OVERWRITE
save_figure <- function(path, plot_fn) {
  if (!OVERWRITE && file.exists(path)) {
    cat("skip (exists):", path, "\n"); return(invisible())
  }
  png(path, width = 900, height = 700)
  plot_fn()
  dev.off()
  cat("wrote:", path, "\n")
}

# ============================================================
# BUILD + FIT
# ============================================================
if (BUILD) {
  build_fn <- BUILDERS[[MODEL]]
  if (is.null(build_fn)) stop("unknown MODEL: ", MODEL)
  model <- build_fn(training)
  if (IS_CTSM) {
    fit <- fit_ctsm(model, training)
    saveRDS(fit, fp())
  }
  saveRDS(model, mp())
  cat("built:", MODEL, sfx, "\n")
} else {
  model <- readRDS(mp())
  if (IS_CTSM) fit <- readRDS(fp())
}

# ============================================================
# DIAGNOSTICS: in-sample residual ACF  (ctsm models only)
# ============================================================
if (PLOT_ACF && IS_CTSM) {
  rr <- extract_residuals(fit)
  save_figure(acf_path(), function() plot_ACF(rr$yTwi, rr$yTwo))
}

# ============================================================
# PREDICTION: prediction-vs-truth figure
# ============================================================
if (PLOT_PRED) {
  input   <- training                                   # or `test`
  truth   <- input[, c("t", "yTwi", "yTwo")]
  in_only <- input[, c("t", "Tai", "Tao", "G")]

  prediction <- if (IS_CTSM) {
    predict_ctsm(model, in_only)
  } else if (MODEL == "PBL") {
    predict_PBL(model, in_only)
  } else if (MODEL == "LBL") {
    predict_LBL(model, in_only)
  } else {
    stop("no predictor for MODEL: ", MODEL)
  }

  save_figure(pred_path(), function()
    plot_prediction(prediction, truth = truth, inputs = in_only,
                    display_RMSE = TRUE, name = paste(MODEL, VARIANT)))
}