# prep_data.R -- align logger CSV into a CTSM-ready frame, trimmed to 30 days
library(lubridate)

raw <- read.csv("raw_data/temp_log_8_5_edited.csv")

names(raw) <- c("Tao", "Tai", "yTwi", "yTwo", "epoch")

d <- data.frame(
  epoch = raw$epoch,
  Tao   = raw$Tao,
  Tai   = raw$Tai,
  yTwi  = raw$yTwi,
  yTwo  = raw$yTwo
)

# time.time() is UTC epoch seconds; Reykjavik is UTC+0 with no DST, so
# interpreting in Atlantic/Reykjavik makes "midnight" local midnight.
d$dt <- as.POSIXct(d$epoch, origin = "1970-01-01", tz = "Atlantic/Reykjavik")
d <- d[order(d$dt), ]
d <- d[!duplicated(d$dt), ]

# --- trim to exactly 30 days on midnight boundaries ---
# start = first midnight after the opening partial day, plus 2 full days.
# The partial-day fraction + 2 days is the "2 and a fraction" off the front;
# whatever remains to reach 30 days is the "1 minus a fraction" off the back.
start    <- ceiling_date(d$dt[1], "day") + days(2)
end_excl <- start + days(30)               # exclusive upper edge = day 31, 00:00
if (start < d$dt[1] || end_excl > max(d$dt) + minutes(1))
  stop("not enough data for a 30-day window under this rule")

d <- d[d$dt >= start & d$dt < end_excl, ]   # last kept minute is 23:59 of day 30

# re-zero time to the start midnight: t = 0 at day 1, 00:00, in hours
d$t <- as.numeric(difftime(d$dt, start, units = "hours"))

cat("window start:", format(start), "\n")
cat("last sample: ", format(max(d$dt)), "\n")
cat("span (days): ", round(as.numeric(difftime(max(d$dt), start, units = "days")), 4), "\n")
cat("median interval (min):", round(median(diff(d$t)) * 60, 2), "\n")
cat("NA counts:\n"); print(colSums(is.na(d[c("Tai", "Tao", "yTwi", "yTwo")])))

# --- downsample onto a regular 10-min grid ---
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
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
saveRDS(dat, "processed_data/temp_log_8_5_trimmed_2.rds")
cat("\nwrote", nrow(dat), "rows to processed_data/temp_log_8_5_trimmed_2.rds\n")