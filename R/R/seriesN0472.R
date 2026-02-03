# Load package and data
library(Mcomp)
data(M3)

# Extract the specific series
series_0472 <- M3[["N0472"]]



# Plot the complete time series
plot(series_0472,
     main = "M3 Series N0472 - Full Time Series (1959–1994)",
     xlab = "Year",
     ylab = "Value",
     col = "blue",
     type = "l",    # line plot
     lwd = 2)

start(series_0472$x)
end(series_0472$xx)
frequency(series_0472)
length(series_0472$xx)

series_0472$x   # the historical (training) time series

x <- series_0472$x    # historical / main data
xx <- series_0472$xx  # future / test data

start(xx)
end(xx)
frequency(xx)

train <- window(x, start = c(1959, 1), end = c(1989, 12))

start(train)
end(train)
length(train)

x <- series_0472$x   # full historical series
train <- window(x, start = c(1959, 1), end = c(1989, 12))

test <- window(xx, start = c(1989, 1))

start(test)
end(test)
length(test)

cat("TRAIN:\n"); print(start(train)); print(end(train)); print(frequency(train)); cat("length =", length(train), "\n\n")
cat("TEST:\n");  print(start(test));  print(end(test));  print(frequency(test));  cat("length =", length(test), "\n\n")

library(forecast)

# 1️⃣ Define train and test data
train <- series_0472$x
test  <- series_0472$xx

# 2️⃣ Forecast horizon
h <- length(test)

# 3️⃣ Apply forecasting methods
# Historical Mean Forecast
mean_f <- meanf(train, h = h)

# Naïve Forecast
naive_f <- naive(train, h = h)

# Holt’s Linear Trend
holt_f <- holt(train, h = h)

# ETS (Exponential Smoothing)
ets_f <- ets(train)
ets_fcast <- forecast(ets_f, h = h)



# 5️⃣ Calculate accuracy measures
acc_mean  <- accuracy(mean_f, test)
acc_naive <- accuracy(naive_f, test)
acc_holt  <- accuracy(holt_f, test)
acc_ets   <- accuracy(ets_fcast, test)

# 6️⃣ Combine results
accuracy_table <- rbind(
  "Historical Mean" = acc_mean[2, ],
  "Naive"           = acc_naive[2, ],
  "Holt's Linear"   = acc_holt[2, ],
  "ETS"             = acc_ets[2, ]
)

# 7️⃣ View accuracy table
round(accuracy_table, 3)


library(forecast)

# Train and test data
train <- series_0472$x
test  <- series_0472$xx

# Forecast horizon
h <- length(test)

# Apply Simple Exponential Smoothing
ses_model <- ses(train, h = h)

# Accuracy metrics comparing forecast vs actual test data
ses_accuracy <- accuracy(ses_model, test)

# Display the accuracy table
round(ses_accuracy, 3)


rmse <- function(actual, forecast) {
  sqrt(mean((actual - forecast)^2))
}

# Holt forecast values (numeric vector)
holt_pred <- as.numeric(holt_f$mean)

holt_rmse_manual <- rmse(test, holt_pred)

holt_rmse_manual

mase <- function(train, test, forecast) {
  
  # Numerator: MAE of forecast errors
  mae_forecast <- mean(abs(test - forecast))
  
  # Denominator: MAE of naive one-step forecasts on training data
  naive_errors <- abs(diff(train))
  mae_naive <- mean(naive_errors)
  
  # MASE
  mase_value <- mae_forecast / mae_naive
  
  return(mase_value)
}

holt_pred <- as.numeric(holt_f$mean)

holt_mase_manual <- mase(
  train = train,
  test  = test,
  forecast = holt_pred
)

holt_mase_manual

rmse <- function(actual, forecast) {
  sqrt(mean((actual - forecast)^2))
}

mean_pred  <- as.numeric(mean_f$mean)
naive_pred <- as.numeric(naive_f$mean)
holt_pred  <- as.numeric(holt_f$mean)
ses_pred   <- as.numeric(ses_model$mean)

rmse_mean  <- rmse(test, mean_pred)
rmse_naive <- rmse(test, naive_pred)
rmse_holt  <- rmse(test, holt_pred)
rmse_ses   <- rmse(test, ses_pred)

rmse_table <- data.frame(
  Method = c("Historical Mean", "Naive", "Holt Linear", "SES"),
  RMSE   = c(rmse_mean, rmse_naive, rmse_holt, rmse_ses)
)

round(rmse_table, 3)
