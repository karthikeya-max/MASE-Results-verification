library(fpp2)

data("productC")

# Convert to ts object
y <- ts(as.numeric(productC), start = c(1986, 1), frequency = 12)

# Split: first 24 months = train, rest = test
train <- window(y, end = c(1987, 12))
test  <- window(y, start = c(1988, 1))
length(test)
length(train)



# ==============================
# 📈 1. Historical Mean Model
# ==============================
mean_model <- meanf(train, h = length(test))
acc_mean <- accuracy(mean_model, test)

# ==============================
# 📈 2. Naive Model
# ==============================
naive_model <- naive(train, h = length(test))
acc_naive <- accuracy(naive_model, test)

# ==============================
# 📈 3. Simple Exponential Smoothing (SES)
# ==============================
ses_model <- ses(train, h = length(test))
acc_ses <- accuracy(ses_model, test)

# ==============================
# 📈 4. Holt’s Linear Trend Model
# ==============================
holt_model <- holt(train, h = length(test))
acc_holt <- accuracy(holt_model, test)

# ==============================
# 📊 Combine Accuracy Results
# ==============================
accuracy_results <- rbind(
  Mean  = acc_mean[2, c("RMSE", "MAE", "MAPE", "MASE")],
  Naive = acc_naive[2, c("RMSE", "MAE", "MAPE", "MASE")],
  SES   = acc_ses[2, c("RMSE", "MAE", "MAPE", "MASE")],
  Holt  = acc_holt[2, c("RMSE", "MAE", "MAPE", "MASE")]
)

print(round(accuracy_results, 4))

# ==============================
# 🧮 Manual MASE Calculation
# ==============================
mae_naive_in_sample <- mean(abs(diff(train)))

mase_manual <- data.frame(
  Model = c("Mean", "Naive", "SES", "Holt"),
  Manual_MASE = c(
    mean(abs(test - mean_model$mean)) / mae_naive_in_sample,
    mean(abs(test - naive_model$mean)) / mae_naive_in_sample,
    mean(abs(test - ses_model$mean)) / mae_naive_in_sample,
    mean(abs(test - holt_model$mean)) / mae_naive_in_sample
  )
)

mase_manual$Manual_MASE <- round(mase_manual$Manual_MASE, 4)
print(mase_manual)


rmse <- function(actual, forecast) {
  sqrt(mean((actual - forecast)^2))
}

mean_pred  <- as.numeric(mean_model$mean)
naive_pred <- as.numeric(naive_model$mean)
ses_pred   <- as.numeric(ses_model$mean)
holt_pred  <- as.numeric(holt_model$mean)

rmse_mean  <- rmse(test, mean_pred)
rmse_naive <- rmse(test, naive_pred)
rmse_ses   <- rmse(test, ses_pred)
rmse_holt  <- rmse(test, holt_pred)
