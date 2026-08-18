# ============================================================
# SPLIT: 20d train | 3d embargo | 10d test (on the t axis)
# Embargo [480,552) h spans the 24-27 Jul solar gap.
# ============================================================
dat <- readRDS("processed_data/preprocessed_raw/temp_log_33d_solar_geometric.rds")
train_end  <- 20 * 24                    # 480
test_start <- 23 * 24                     # 552  (20 + 3 embargo)
test_end   <- 33 * 24                     # 792

train   <- dat[dat$t >= 0          & dat$t < train_end,  ]
embargo <- dat[dat$t >= train_end  & dat$t < test_start, ]
test    <- dat[dat$t >= test_start & dat$t < test_end,   ]

test_inputs <- test[, c("t", "Tai", "Tao", "G")]     # model-visible
test_truth  <- test[, c("t", "yTwi", "yTwo")]         # scoring only

#dir.create("processed_data/splits", recursive = TRUE, showWarnings = FALSE)
saveRDS(train,       "processed_data/geometric_splits/geometric_train.rds")
saveRDS(test,        "processed_data/geometric_splits/geometric_test.rds")
saveRDS(embargo,     "processed_data/geometric_splits/geometric_embargo.rds")
saveRDS(test_inputs, "processed_data/geometric_splits/geometric_test_inputs.rds")
saveRDS(test_truth,  "processed_data/geometric_splits/geometric_test_truth.rds")

cat(sprintf("train %d | embargo %d | test %d rows\n",
            nrow(train), nrow(embargo), nrow(test)))