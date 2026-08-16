# prepare_and_split.R
# raw (~34 days) -> trim to 32 midnight-aligned days -> add solar G ->
# split into 21d train / 1d embargo / 10d test.
# Run from project root.

library(lubridate)
source("misc/add_solar.r")   # provides add_solar() + sensor_irradiance()

# ============================================================
# 1. LOAD + TRIM RAW  ->  32 days on midnight boundaries
#    (cuts to midnights; hours removed fall out of that)
# ============================================================
raw <- read.csv("raw_data/temp_log_8_6_edited.csv")
names(raw) <- c("Tao", "Tai", "yTwi", "yTwo", "epoch")

d <- data.frame(
  epoch = raw$epoch,
  Tao   = raw$Tao,
  Tai   = raw$Tai,
  yTwi  = raw$yTwi,
  yTwo  = raw$yTwo
)

# UTC epoch seconds; Reykjavik is UTC+0, no DST -> local midnight = UTC midnight
d$dt <- as.POSIXct(d$epoch, origin = "1970-01-01", tz = "Atlantic/Reykjavik")
d <- d[order(d$dt), ]
d <- d[!duplicated(d$dt), ]

WINDOW_DAYS <- 32

# start = next midnight after logging began, + 1 full day (drops the opening partial
# evening + 1 day). end = start + 32 days. Both land exactly on midnights.
start    <- ceiling_date(d$dt[1], "day") + days(1)
end_excl <- start + days(WINDOW_DAYS)

if (start < d$dt[1] || end_excl > max(d$dt) + minutes(1))
  stop(sprintf("not enough data for a %d-day midnight window: have %.2f days.",
               WINDOW_DAYS, as.numeric(difftime(max(d$dt), d$dt[1], units = "days"))))

d <- d[d$dt >= start & d$dt < end_excl, ]

# re-zero time: t = 0 at day-1 00:00, in hours
d$t <- as.numeric(difftime(d$dt, start, units = "hours"))

cat("window start:", format(start), "\n")
cat("last sample: ", format(max(d$dt)), "\n")
cat("span (days): ", round(as.numeric(difftime(max(d$dt), start, units = "days")), 4), "\n")
cat("median interval (min):", round(median(diff(d$t)) * 60, 2), "\n")
cat("NA counts:\n"); print(colSums(is.na(d[c("Tai", "Tao", "yTwi", "yTwo")])))

# ============================================================
# 2. DOWNSAMPLE onto a regular 30-min grid
# ============================================================
dt_h <- 30 / 60
d$bin <- round(d$t / dt_h) * dt_h
agg <- aggregate(cbind(Tai, Tao, yTwi, yTwo) ~ bin, data = d,
                 FUN = function(x) mean(x, na.rm = TRUE))

grid <- data.frame(t = seq(0, max(d$bin), by = dt_h))
m <- merge(grid, agg, by.x = "t", by.y = "bin", all.x = TRUE)

fill <- function(y) approx(m$t[!is.na(y)], y[!is.na(y)], xout = m$t, rule = 2)$y
m$Tai <- fill(m$Tai)
m$Tao <- fill(m$Tao)

dat <- m[, c("t", "Tai", "Tao", "yTwi", "yTwo")]

# ============================================================
# 3. ADD SOLAR column G  (clear-sky wall irradiance)
# ============================================================
doy_start <- as.numeric(format(start, "%j"))   # day-of-year of t=0, from the trim
dat <- add_solar(dat, doy_start = doy_start, timezone = 0,
                 lat = 64.1355, lon = -21.8954, wall_az = 5.17)

dir.create("processed_data", recursive = TRUE, showWarnings = FALSE)
saveRDS(dat, "processed_data/temp_log_32d_solar.rds")
cat("\nwrote", nrow(dat), "rows (with G) to processed_data/temp_log_32d_solar.rds\n")

# ============================================================
# 4. SPLIT: 21d train | 1d embargo | 10d test  (on the t axis)
# ============================================================
train_end  <- 21 * 24                     # 504 h
test_start <- (21 + 1) * 24               # 528 h
test_end   <- (21 + 1 + 10) * 24          # 768 h

if (max(dat$t) < test_end - 1e-6)
  stop(sprintf("processed data ends at t=%.2f h but split needs %.0f h.", max(dat$t), test_end))

train   <- dat[dat$t >= 0          & dat$t < train_end,  ]
embargo <- dat[dat$t >= train_end  & dat$t < test_start, ]
test    <- dat[dat$t >= test_start & dat$t < test_end,   ]

cat("\ntrain:  ", nrow(train),  "rows | [",   0,        ",", train_end,  ") h\n")
cat("embargo:", nrow(embargo), "rows | [", train_end,  ",", test_start, ") h\n")
cat("test:   ", nrow(test),    "rows | [", test_start, ",", test_end,   ") h\n")

dir.create("processed_data/splits", recursive = TRUE, showWarnings = FALSE)
saveRDS(train,   "processed_data/splits/train.rds")
saveRDS(test,    "processed_data/splits/test.rds")
saveRDS(embargo, "processed_data/splits/embargo.rds")
cat("\nwrote train.rds / test.rds / embargo.rds to processed_data/splits/\n")