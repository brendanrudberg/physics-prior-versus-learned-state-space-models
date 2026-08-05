# prep_data.R -- align logger CSV into a CTSM-ready frame
# read.csv turns "outside air" into "outside.air"; match your real header.

raw <- read.csv("raw_data/temp_log_8_5_edited.csv")
raw <- read.csv("raw_data/temp_log_8_5_edited.csv")

names(raw) <- c("Tao", "Tai", "yTwi", "yTwo", "epoch")

d <- data.frame(
  epoch = raw$epoch,
  Tao   = raw$Tao,
  Tai   = raw$Tai,
  yTwi  = raw$yTwi,
  yTwo  = raw$yTwo
)

# time.time() = Unix epoch seconds -> hours, zero-based
d$t <- (d$epoch - d$epoch[1]) / 3600
d$epoch <- NULL

# monotonic time, no duplicate timestamps
d <- d[order(d$t), ]
d <- d[!duplicated(d$t), ]

# sanity check your actual cadence and coverage before trusting anything
cat("median interval (min):", round(median(diff(d$t)) * 60, 2), "\n")
cat("record length (h):    ", round(max(d$t), 1), "\n")
cat("NA counts:\n"); print(colSums(is.na(d)))

# --- downsample 1-min data onto a regular grid ---
# 10-min grid is ample for a ~40 h wall; averaging also denoises. Drop to
# 5/60 if you suspect identifiable dynamics faster than ~1 h.
dt_h <- 10 / 60
d$bin <- round(d$t / dt_h) * dt_h
agg <- aggregate(cbind(Tai, Tao, yTwi, yTwo) ~ bin, data = d,
                 FUN = function(x) mean(x, na.rm = TRUE))

grid <- data.frame(t = seq(0, max(d$bin), by = dt_h))
m <- merge(grid, agg, by.x = "t", by.y = "bin", all.x = TRUE)

# inputs must be gap-free -> interpolate. outputs keep NA (CTSM propagates).
fill <- function(y) approx(m$t[!is.na(y)], y[!is.na(y)], xout = m$t, rule = 2)$y
m$Tai <- fill(m$Tai)
m$Tao <- fill(m$Tao)

dat <- m[, c("t", "Tai", "Tao", "yTwi", "yTwo")]
saveRDS(dat, "processed_data/temp_log_8_5_processed.rds")
cat("\nwrote", nrow(dat), "rows to processed_data/temp_log_8_5_processed.rds\n")