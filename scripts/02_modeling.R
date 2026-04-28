#ACF / PACF 분석
#p, q 결정
#모델 선택 (ARIMA, ARMAX 등)
#모델 fitting


#source("scripts/setup.R")
#load("processed_data.RData")
load("data_preprocessed/processed_data.RData")

# 00_Load the Data -----------------------------------------------------------

#rawData <- read.csv("data_raw/Filtered_data.csv", header = TRUE)
rawData <- read.csv("scripts/Filtered_data.csv", header = TRUE)

#ncol(rawData)
#nrow(rawData)
#colnames(rawData)
data <- ts(rawData$OBS_VALUE,start = c(2005,1), frequency = 12)



# 01_Baseline Model -------------------------------------------------------

# Split the data into training and testing sets
train <- window(data, end=c(2022,12))
test  <- window(data, start=c(2023,1))

# Naive Model
naive_model <- naive(train, h=length(test))

# Seasonal Naive Model
snaive_model <- snaive(train, h=length(test))

# Evaluate the models
acc_naive <- accuracy(naive_model, test)
acc_snaive <- accuracy(snaive_model, test)

acc_naive
acc_snaive

#write.csv(acc_naive, "results/acc_naive.csv")
#write.csv(acc_snaive, "results/acc_snaive.csv")

# 02_ARIMA Model ----------------------------------------------------------

# Apply log transformation to stabilize the variance
log_train <- log(train)
log_test  <- log(test)

# Apply differencing to remove the trend
diff_log_train <- diff(log_train)
diff_seasonal_train <- diff(diff_log_train, lag = 12)

# ACF and PACF plots to determine p and q
acf(diff_seasonal_train)
pacf(diff_seasonal_train)

# Fit the ARIMA model
arima_011 <- Arima(log_train, order = c(0,1,1))
arima_111 <- Arima(log_train, order = c(1,1,1))
arima_110 <- Arima(log_train, order = c(1,1,0))

# Evaluate the models
fc_011 <- forecast(arima_011, h = length(test))
fc_111 <- forecast(arima_111, h = length(test))
fc_110 <- forecast(arima_110, h = length(test))

# Convert the forecasts back to the original scale
fc_011_raw <- exp(fc_011$mean)
fc_111_raw <- exp(fc_111$mean)
fc_110_raw <- exp(fc_110$mean)

# Evaluate the forecasts
acc_arima_011 <- accuracy(fc_011_raw, test)
acc_arima_111 <- accuracy(fc_111_raw, test)
acc_arima_110 <- accuracy(fc_110_raw, test)

acc_arima_011
acc_arima_111
acc_arima_110


# Saving the results ------------------------------------------------------

results <- data.frame(
  Model = c("Naive", "Seasonal Naive", "ARIMA(0,1,1)", "ARIMA(1,1,1)", "ARIMA(1,1,0)"),
  RMSE = c(acc_naive["Test set", "RMSE"], acc_snaive["Test set", "RMSE"], acc_arima_011["Test set", "RMSE"], acc_arima_111["Test set", "RMSE"], acc_arima_110["Test set", "RMSE"]),
  MAE = c(acc_naive["Test set", "MAE"], acc_snaive["Test set", "MAE"], acc_arima_011["Test set", "MAE"], acc_arima_111["Test set", "MAE"], acc_arima_110["Test set", "MAE"])
)

write.csv(results, "results/model_comparison.csv", row.names = FALSE)
