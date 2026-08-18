dat <- readRDS("processed_data/temp_log_8_5_trimmed_2.rds")
print(colSums(is.na(dat)))
print(sapply(dat, function(x) any(is.infinite(x))))
print(range(diff(dat$t)))          # any zero or negative gaps?
print(summary(dat))                 # any wild values, sensor error codes like -127?