train <- readRDS("processed_data/geometric_splits/geometric_train.rds")

old <- par(mfrow = c(3, 1), mar = c(4, 4, 2, 1))

# air temperature inputs
plot(train$t, train$Tao, type = "l", col = "firebrick",
     ylim = range(c(train$Tai, train$Tao)),
     xlab = "t (h)", ylab = "deg C", main = "Air temperature inputs")
lines(train$t, train$Tai, col = "steelblue")
legend("topright", c("Tao (outside)", "Tai (inside)"),
       col = c("firebrick", "steelblue"), lty = 1, bty = "n")

# solar input
plot(train$t, train$G, type = "l", col = "darkorange",
     xlab = "t (h)", ylab = "W/m^2", main = "Solar irradiance input (G)")

# wall temperature outputs
plot(train$t, train$yTwo, type = "l", col = "red",
     ylim = range(c(train$yTwi, train$yTwo)),
     xlab = "t (h)", ylab = "deg C", main = "Wall temperature outputs")
lines(train$t, train$yTwi, col = "blue")
legend("topright", c("yTwo (outer)", "yTwi (inner)"),
       col = c("red", "blue"), lty = 1, bty = "n")

par(old)