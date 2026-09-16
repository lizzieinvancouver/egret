
# Parameters
alpha_logc <- log(0.15)  
beta_logc  <- 0.10
# logb <- 2  
pv <- 0.9


temp0 <- 17.5
germ_temps <- c(10.5, 15, 20, 25)
days <- seq(0, 60, by = 0.5)


cols <- c("#ED968CFF", "#AB3329FF", "#E78429FF", "#F9D14AFF")


par(mfrow = c(1, 3), mar = c(4,4,1,1))
temps <- seq(5, 30, by = 0.5)
logcs <- alpha_logc + beta_logc * (temps - temp0)
plot(temps, logcs, type = "l", xlab = "Temperature (degC)", ylab = "log(c)", 
     ylim = c(-3, 0),
     lty = 2, bty = 'n', col = 'grey80')
for(i in 1:length(germ_temps)){
  logci <- alpha_logc + beta_logc * (germ_temps[i] - temp0)
  points(germ_temps[i], logci, col = cols[i], pch = 20, cex = 2)
}


plot(NULL, xlim = range(days), ylim = c(0, 0.1),
     xlab = "Day", ylab = "Daily germination",
     bty = 'n')
for (i in 1:length(germ_temps)) {
  ci <- exp(alpha_logc + beta_logc * (germ_temps[i] - temp0))
  # Fi <- pv * exp(-exp(log_b - ci * t))
  Fi <- exp(-exp(log_b - ci * t))
  lines(t[-1], diff(Fi), col = cols[i], lwd = 1.5)
}


plot(NULL, xlim = range(days), ylim = c(0, 1),
     xlab = "Day", ylab = "Cumulative germination",
     bty = 'n')
for (i in 1:length(germ_temps)) {
  ci <- exp(alpha_logc + beta_logc * (germ_temps[i] - temp0))
  # Fi <- pv * exp(-exp(log_b - ci * t))
  Fi <- exp(-exp(log_b - ci * t))
  lines(t, Fi, col = cols[i], lwd = 1.5)
}
