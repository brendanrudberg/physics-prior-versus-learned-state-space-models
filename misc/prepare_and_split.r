# prepare_and_split.R
# Trim + bin temperature and measured solar independently onto a shared 30-min
# grid, merge, then split into 21d train / 1d embargo / 10d test.
# Run from project root.

library(lubridate)

WINDOW_DAYS <- 32
DT_H        <- 0.5                       # 30-min grid
TZ          <- "Atlantic/Reykjavik"      # UTC+0, no DST

# Window start: first midnight after logging began, plus one full day.
# Drops the opening partial evening + 1 day; end is start + WINDOW_DAYS.
window_start <- function(dt) ceiling_date(min(dt), "day") + days(1)

# Assign each time (hours from start) to a 30-min bin centre.
bin_index <- function(t) round(t / DT_H) * DT_H

# Average a long frame's numeric columns into 30-min bins keyed by 't'.
avg_bin <- function(df, value_cols) {
  df$bin <- bin_index(df$t)
  f <- as.formula(paste0("cbind(", paste(value_cols, collapse = ", "), ") ~ bin"))
  agg <- aggregate(f, data = df, FUN = function(x) mean(x, na.rm = TRUE))
  names(agg)[names(agg) == "bin"] <- "t"
  agg
}

# ---- temperature ----
load_temp <- function(path, start) {
  raw <- read.csv(path)
  names(raw) <- c("Tao", "Tai", "yTwi", "yTwo", "epoch")
  d <- raw[, c("epoch", "Tao", "Tai", "yTwi", "yTwo")]
  d$dt <- as.POSIXct(d$epoch, origin = "1970-01-01", tz = TZ)
  d <- d[order(d$dt), ]
  d <- d[!duplicated(d$dt), ]
  d$t <- as.numeric(difftime(d$dt, start, units = "hours"))
  d[d$t >= 0 & d$t < WINDOW_DAYS * 24, ]
}

# ---- solar (5-min measured GHI) ----
load_solar <- function(path, start) {
  raw <- read.csv(path)
  d <- data.frame(
    dt = as.POSIXct(raw$DateTime, tz = TZ, format = "%Y-%m-%d %H:%M:%S"),
    G  = as.numeric(raw$Solar_W_m2)
  )
  d <- d[order(d$dt), ]
  d$t <- as.numeric(difftime(d$dt, start, units = "hours"))
  d[d$t >= 0 & d$t < WINDOW_DAYS * 24, ]
}

# ============================================================
# BUILD DATASET
# ============================================================
temp_raw <- read.csv("raw_data/temp_log_8_6.csv")
names(temp_raw) <- c("Tao", "Tai", "yTwi", "yTwo", "epoch")
start <- window_start(as.POSIXct(temp_raw$epoch, origin = "1970-01-01", tz = TZ))

temp  <- load_temp("raw_data/temp_log_8_6.csv", start)
solar <- load_solar("raw_data/ISELTJ5_solar_2026-07-01_to_2026-08-07.csv", start)

if (max(temp$t) < WINDOW_DAYS * 24 - 1e-6)
  stop(sprintf("temperature covers only %.2f days.", max(temp$t) / 24))
if (max(solar$t) < WINDOW_DAYS * 24 - 1e-6)
  stop(sprintf("solar covers only %.2f days.", max(solar$t) / 24))

agg_temp  <- avg_bin(temp,  c("Tai", "Tao", "yTwi", "yTwo"))
agg_solar <- avg_bin(solar, "G")

# Regular grid; merge both binned sets onto it.
grid <- data.frame(t = seq(0, WINDOW_DAYS * 24 - DT_H, by = DT_H))
m <- merge(grid, agg_temp,  by = "t", all.x = TRUE)
m <- merge(m,    agg_solar, by = "t", all.x = TRUE)

# Interpolate any input gaps (endpoints held).
fill <- function(y) approx(m$t[!is.na(y)], y[!is.na(y)], xout = m$t, rule = 2)$y
m$Tai <- fill(m$Tai); m$Tao <- fill(m$Tao); m$G <- fill(m$G)

dat <- m[, c("t", "Tai", "Tao", "yTwi", "yTwo", "G")]

cat("window start:", format(start), "\n")
cat("rows:", nrow(dat), "| NA counts:\n"); print(colSums(is.na(dat)))

dir.create("processed_data", recursive = TRUE, showWarnings = FALSE)
saveRDS(dat, "processed_data/preprocessed_raw/temp_log_32d_solar_readings.rds")

# ============================================================
# SPLIT: 21d train | 1d embargo | 10d test (on the t axis)
# ============================================================
train_end  <- 21 * 24
test_start <- 22 * 24
test_end   <- 32 * 24

train <- dat[dat$t >= 0          & dat$t < train_end,  ]
test  <- dat[dat$t >= test_start & dat$t < test_end,   ]
embargo <- dat[dat$t >= train_end & dat$t < test_start, ]

test_inputs <- test[, c("t", "Tai", "Tao", "G")]     # model-visible
test_truth  <- test[, c("t", "yTwi", "yTwo")]         # scoring only

dir.create("processed_data/splits", recursive = TRUE, showWarnings = FALSE)
saveRDS(train,       "processed_data/splits/train.rds")
saveRDS(test,        "processed_data/splits/test.rds")
saveRDS(embargo,     "processed_data/splits/embargo.rds")
saveRDS(test_inputs, "processed_data/splits/test_inputs.rds")
saveRDS(test_truth,  "processed_data/splits/test_truth.rds")

cat(sprintf("train %d | embargo %d | test %d rows\n",
            nrow(train), nrow(embargo), nrow(test)))