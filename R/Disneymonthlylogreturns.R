install.packages("quantmod")
library(quantmod)

getSymbols("DIS", src = "yahoo", from = "1990-01-01", to = "1999-12-31")

disney_monthly <- to.monthly(DIS, indexAt = "lastof", OHLC = FALSE)
prices <- Cl(disney_monthly)

log_returns <- diff(log(prices))[-1]
head(log_returns)
summary(log_returns)
plot(log_returns, main = "Monthly Log Returns of Disney (1990–1999)")

length(prices)

start(prices)
end(prices)
frequency(prices)

prices <- Cl(disney_monthly)
disney_ts <- ts(as.numeric(prices),
                start = c(1990, 1),
                frequency = 12)

disney_ts
plot(disney_logret_ts, main = "Disney Monthly Closing Prices (1990–1999)", ylab = "Price")

disney_logret_ts <- ts(as.numeric(log_returns),
                       start = c(1990, 2),  # starts from Feb 1990 (since first return is missing)
                       frequency = 12)

start(disney_logret_ts)
end(disney_logret_ts)
length(disney_logret_ts)
summary(disney_logret_ts)

plot(disney_logret_ts,
     main = "Monthly Log Returns of Disney (1990–1999)",
     ylab = "Log Returns",
     xlab = "Year")
train <- window(disney_logret_ts, end = c(1997, 12))
test  <- window(disney_logret_ts, start = c(1998, 1))
length(train)  # should be 96
length(test)   # should be 12
plot(train, main = "Disney Monthly Log Returns: Train vs Test", ylab = "Log Returns")
lines(test, col = "red")
legend("topright", legend = c("Train", "Test"), col = c("black", "red"), lty = 1)

# Load required package
library(forecast)

# 1. Historical Mean Forecast
mean_forecast <- meanf(train, h = length(test))

# 2. Naive Model
naive_forecast <- naive(train, h = length(test))

# 3. Simple Exponential Smoothing (SES)
ses_model <- ses(train, h = length(test))

# 4. Holt's Linear Trend Method
holt_model <- holt(train, h = length(test))

acc_mean  <- accuracy(mean_forecast, test)
acc_naive <- accuracy(naive_forecast, test)
acc_ses   <- accuracy(ses_model, test)
acc_holt  <- accuracy(holt_model, test)

# Combine all results
accuracy_results <- rbind(
  Mean  = acc_mean[2, c("RMSE", "MAE", "MASE")],
  Naive = acc_naive[2, c("RMSE", "MAE", "MASE")],
  SES   = acc_ses[2, c("RMSE", "MAE", "MASE")],
  Holt  = acc_holt[2, c("RMSE", "MAE", "MASE")]
)

round(accuracy_results, 4)

# Load forecasting library
library(forecast)

# Apply Historical Mean method on train data
mean_model <- meanf(train, h = length(test))

# View forecast summary
summary(mean_model)

mean_model <- meanf(train, h = length(test))
accuracy_mean <- accuracy(mean_model, test)
accuracy_mean


train  # training data (ts)
test   # test data (ts)
mean_model  # model from meanf()


# Forecasted values from historical mean
forecast_values <- mean_model$mean

# 1️⃣ Numerator: Mean Absolute Error of forecast
mae_forecast <- mean(abs(test - forecast_values))

# 2️⃣ Denominator: In-sample mean absolute difference (naive scaling)
mae_naive_in_sample <- mean(abs(diff(train)))

# 3️⃣ MASE Calculation
mase_manual <- mae_forecast / mae_naive_in_sample

mase_manual

library(forecast)

# Fit Naive Model
naive_model <- naive(train, h = length(test))

accuracy_naive <- accuracy(naive_model, test)
accuracy_naive

# Forecasted values
forecast_values <- naive_model$mean

# Numerator: Mean Absolute Error of forecast
mae_forecast <- mean(abs(test - forecast_values))

# Denominator: Mean Absolute Error of in-sample naive (one-step lag differences)
mae_naive_in_sample <- mean(abs(diff(train)))

# Manual MASE
mase_manual <- mae_forecast / mae_naive_in_sample

mase_manual

library(forecast)

# Fit Simple Exponential Smoothing model
ses_model <- ses(train, h = length(test))

accuracy_ses <- accuracy(ses_model, test)
accuracy_ses

# Forecasted values from SES model
forecast_values <- ses_model$mean
str(ses_model)

# 1️⃣ Numerator: Mean Absolute Forecast Error
mae_forecast <- mean(abs(test - forecast_values))

# 2️⃣ Denominator: In-sample mean absolute one-step difference
mae_naive_in_sample <- mean(abs(diff(train)))

# 3️⃣ Manual MASE
mase_manual_ses <- mae_forecast / mae_naive_in_sample

mase_manual_ses

accuracy_holt <- accuracy(holt_model, test)
accuracy_holt

# Forecasted values
forecast_values <- holt_model$mean

# Numerator: Mean Absolute Forecast Error
mae_forecast <- mean(abs(test - forecast_values))

# Denominator: In-sample Mean Absolute One-step Difference
mae_naive_in_sample <- mean(abs(diff(train)))

# Manual MASE Calculation
mase_manual_holt <- mae_forecast / mae_naive_in_sample

mase_manual_holt

rmse <- function(actual, forecast) {
  sqrt(mean((actual - forecast)^2))
}

mean_pred  <- as.numeric(mean_forecast$mean)
naive_pred <- as.numeric(naive_forecast$mean)
ses_pred   <- as.numeric(ses_model$mean)
holt_pred  <- as.numeric(holt_model$mean)


rmse_mean  <- rmse(test, mean_pred)
rmse_naive <- rmse(test, naive_pred)
rmse_ses   <- rmse(test, ses_pred)
rmse_holt  <- rmse(test, holt_pred)


rmse_table <- data.frame(
  Method = c("Mean", "Naive", "SES", "Holt"),
  RMSE   = c(rmse_mean, rmse_naive, rmse_ses, rmse_holt)
)

round(rmse_table, 4)
