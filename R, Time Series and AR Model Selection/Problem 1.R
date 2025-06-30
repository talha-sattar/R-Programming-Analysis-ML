###############################################################################
#                                                                             #
#            W2453 - Applied Time Series Analysis Project with R              #
#                                                                             #
#      Problem 1: Time Series Simulation and AR Model Selection via AIC       #
#                                                                             #
###############################################################################

# Insert your student number below
MatrNr <- 7291472

# Set the random seed using your student number for reproducibility
set.seed(MatrNr)

# Number of observations
n <- 400

# Simulate AR(1) process: Y_t = 0.8 * Y_{t-1} + eps_t, eps_t ~ N(0,1)
yt <- arima.sim(model = list(ar = 0.8), n = n)

# Add constant to get X_t
xt <- yt + 4.5

# ---- (c) Plotting the simulated time series ----
png("Problem1_timeseries.png", width=800, height=400)
plot.ts(xt, main = "Simulated Time Series X_t", xlab = "Time", ylab = "X_t")
dev.off()

# ---- (d) Plotting and exporting the ACF ----
png("Problem1_acf.png", width=800, height=400)
acf(xt, main = "ACF of Simulated Time Series X_t")
dev.off()

# ---- (e) Model selection by AIC: arima() and ar.yw() ----
# arima() method
aic_arima <- sapply(0:3, function(p) AIC(arima(xt, order = c(p,0,0))))
best_p_arima <- which.min(aic_arima) - 1
best_model_arima <- arima(xt, order = c(best_p_arima,0,0))

# ar.yw() method
library(stats)
aic_yw <- rep(NA, 4) # To keep length same as arima() AIC vector

# fit Model AR(1), AR(2), AR(3)
for (p in 1:3) {
  fit <- ar.yw(xt, order.max = p, aic = FALSE)
  aic_yw[p + 1] <- fit$aic[1]  # Take the first AIC value only
}

# For reporting: only consider p=1,2,3
best_p_yw <- which.min(aic_yw[2:4]) # will give 1 for AR(1), 2 for AR(2), etc.
best_model_yw <- ar.yw(xt, order.max = best_p_yw, aic = FALSE)

# Print results for your reference 
cat("AIC values for arima():", aic_arima, "\n")
cat("Best ARIMA(p) order:", best_p_arima, "\n")
cat("AIC values for ar.yw():", aic_yw, "\n")
cat("Best ar.yw(p) order:", best_p_yw, "\n")

# Show the model summaries
summary(best_model_arima)
summary(best_model_yw)