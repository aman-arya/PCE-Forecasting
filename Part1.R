# Install necessary packages
install.packages("forecast")
install.packages("gridExtra")
install.packages("VIM")
install.packages("tseries")
install.packages("Metrics")
install.packages("zoo")
install.packages("fpp")
install.packages("imputeTS")
install.packages("ggplot2")


setwd("/Users/amanarya/Documents/LUBS/BADS/Sem 2/Forecasting and Advanced Business Analytics/Assignment/Assignment Forecast")

# Load necessary libraries----
library(forecast)
library(imputeTS)
library(ggplot2)
library(gridExtra) 
library(imputeTS)
library(VIM)
library(stats)
library(tseries)
library(Metrics)
library(zoo)
library(fpp)
library(imputeTS)
library(ggplot2)


# Load the data----
pce_data <- read.csv("PCE.csv", stringsAsFactors = FALSE)

# set seed as last 3 digit of roll number for reproducibility
set.seed(345)

head(pce_data)
tail(pce_data)
summary(pce_data)

#Check structure of data
str(pce_data)

# Convert the DATE column to Date type
pce_data$DATE <- as.Date(pce_data$DATE, format="%d/%m/%Y")


# Created time series object----
# Since the data starts from January 1959 and is monthly, we set start=c(1959, 1)
# The frequency for monthly data is 12
pce_ts <- ts(pce_data$PCE, start=c(1959, 1), end =c(2023,11), frequency=12)
pce_ts

head(pce_ts)

plot(pce_ts, xlab="Time", ylab="PCE", main="Time Series of PCE")

# Checking for missing data----
sum(is.na(pce_ts))

# Impute missing values using the na.kalman function
pce_ts_imputed <- na_kalman(pce_ts)
pce_ts_imputed

# Checking for missing data
sum(is.na(pce_ts_imputed))

# Plot original time series
plot(pce_ts, type = "l", col = "red", ylim = range(pce_ts, pce_ts_imputed, na.rm = TRUE), ylab = "PCE", xlab = "Time", main = "Original vs. Imputed PCE Time Series")
# Add imputed time series to the plot
lines(pce_ts_imputed, type = "l", col = "blue")
legend("topright", legend = c("Original", "Imputed"), col = c("red", "blue"), lty = 1)

summary(pce_ts)
summary(pce_ts_imputed)

# Assuming the data is normally distributed, adjust the tests if not appropriate for your data
# Test for a difference in means
t_test_result <- t.test(pce_ts, pce_ts_imputed, na.action=na.exclude)

# Test for a difference in variance
var_test_result <- var.test(pce_ts, pce_ts_imputed, na.action=na.exclude)

print(t_test_result)
print(var_test_result)


hist(pce_ts, breaks=50, col=rgb(1,0,0,0.5), xlim=range(pce_ts, pce_ts_imputed, na.rm=TRUE), xlab="PCE", main="Histogram of Original vs. Imputed")
hist(pce_ts_imputed, breaks=50, col=rgb(0,0,1,0.5), add=TRUE)
legend("topright", legend=c("Original", "Imputed"), col=c("red", "blue"), fill=c(rgb(1,0,0,0.5), rgb(0,0,1,0.5)))

# Additive Decompose the time series to observe the trend, seasonal, and irregular components----
pce_additive <- decompose(pce_ts_imputed, type = "additive")
plot(pce_additive)




### Trend----
plot(pce_ts_imputed)
ggplot(data = as.data.frame(pce_ts_imputed), aes(x = index(pce_ts_imputed), y = coredata(pce_ts_imputed))) +
  geom_line() +
  labs(title = "PCE Time Series", x = "Time", y = "PCE") +
  theme_minimal()

# Create a time index
time_index <- seq_along(pce_ts_imputed)

# Fit a linear model
model <- lm(pce_ts_imputed ~ time_index)

# Summary of the model to check for trend significance
summary(model)


### Seasonality----

ggseasonplot(pce_ts_imputed, main="Seasonal Plot of PCE", xlab="Month", ylab="PCE", year.labels=TRUE, year.labels.left=TRUE)

boxplot(split(pce_ts_imputed, cycle(pce_ts_imputed)), 
        names = month.abb, col = "lightgreen",
        main = "Seasonal Distribution of the Time Series")



### volatility----

# Analyzing volatility using standard deviation over a rolling window
pce_volatility <- rollapply(pce_ts_imputed, width=12, FUN=sd, by.column = TRUE, align='center', fill=NA)
# Plot volatility
plot(pce_volatility, type = "l", col = "blue", xlab = "Time", ylab = "Volatility", main = "Rolling Standard Deviation of PCE")

# BOX COX----
# First, find an optimal lambda for the Box-Cox Transformation
lambda <- BoxCox.lambda(pce_ts_imputed)
# Print the optimal lambda value
print(paste("Optimal lambda:", lambda))

# Apply the Box-Cox Transformation
pce_ts_transformed <- BoxCox(pce_ts_imputed, lambda)

# Plot the original and transformed data to compare
par(mfrow=c(2, 1))
plot(pce_ts_imputed, main="Original PCE Time Series")
plot(pce_ts_transformed, main=paste("Box-Cox Transformed PCE Time Series, Lambda =", round(lambda, 5)))

summary(pce_ts_imputed)
summary(pce_ts_transformed)

# Autocorrelation Function (ACF) and Partial Autocorrelation Function (PACF)----

tsdisplay(pce_ts_transformed)
plot(pce_ts_transformed)

pce_ts_diff_2 <- diff(pce_ts_transformed, differences = 2)
kpss_test_result <- kpss.test(pce_ts_diff_2)
kpss_test_result
tsdisplay(pce_ts_diff_2)

# Analyzing volatility using standard deviation over a rolling window
pce_volatility <- rollapply(pce_ts_diff_2, width=12, FUN=sd, by.column = TRUE, align='center', fill=NA)
# Plot volatility
plot(pce_volatility, type = "l", col = "blue", xlab = "Time", ylab = "Volatility", main = "Rolling Standard Deviation of PCE")

# Stationarity checks using ADF and KPSS tests

# adf
adf_test_result <- adf.test(pce_ts_imputed)
adf_test_result
# kpss
kpss_test_result <- kpss.test(pce_ts_imputed)
kpss_test_result

# 1st difference
pce_ts_diff_1 <- diff(pce_ts_imputed, differences = 1)

# adf
adf_test_result <- adf.test(pce_ts_diff_1)
adf_test_result
# kpss
kpss_test_result <- kpss.test(pce_ts_diff_1)
kpss_test_result

# 2nd difference
pce_ts_diff_2 <- diff(pce_ts_imputed, differences = 2)

# adf
adf_test_result <- adf.test(pce_ts_diff_2)
adf_test_result
# kpss
kpss_test_result <- kpss.test(pce_ts_diff_2)
kpss_test_result

# Plot the differences data
plot(pce_ts_diff_1, main="1st Differenced PCE Time Series", xlab="Time", ylab="Differenced PCE")

tsdisplay(pce_ts_diff_1)

# Plot the differences data
plot(pce_ts_diff_2, main="2nd Differenced PCE Time Series", xlab="Time", ylab="Differenced PCE")

tsdisplay(pce_ts_diff_2)

### Moving average with window size 2----
ma_pce <- ma(pce_ts_diff_2, order=2)

# Plotting the original differenced series with the moving average overlay
plot(pce_ts_diff_2, main="Original Differenced Series vs. Moving Average", xlab="Time", ylab="Differenced PCE", type="l")
lines(ma_pce, col="blue", lwd=2)  # thicker line for better visibility
legend("topleft", legend=c("Differenced Series", "Moving Average"), col=c("black", "blue"), lty=1, lwd=2)

# Display the time series properties using tsdisplay
# For the original differenced series
tsdisplay(pce_ts_diff_2, main="Time Series Display of 2nd Differenced PCE")

# For the moving average smoothed series
tsdisplay(ma_pce, main="Time Series Display of Moving Average Smoothed Series")

# Comparing the effect visually and statistically
print("Summary of Original Differenced Series:")
print(summary(pce_ts_diff_2))

print("Summary of Moving Average Applied Series:")
print(summary(ma_pce))



# Modelling----


# Split the data into training and test sets
train_data <- window(pce_ts_imputed, end = c(2010, 12))
test_data <- window(pce_ts_imputed, start = c(2011, 1))
percentage_test = length(test_data)/779*100
percentage_test

#Drift----
# Fit a drift model to the training data
drift_model <- rwf(train_data, drift=TRUE, lambda = 0.0277327677958033, h=length(test_data))
# Summary of the model
summary(drift_model)


# Plot the fitted model along with the actual data
plot(drift_model,ylim=c(0, 19000), main="Drift Model Fitting on PCE Data")
lines(test_data, col='red')
legend("topleft", legend=c("DRIFT", "Actual"), col=c("blue", "red"), lty=1)



# Check residuals
checkresiduals(drift_model)

# Calculate and print model accuracy
accuracy_metrics <- forecast::accuracy(drift_model, test_data)
print(accuracy_metrics)


#Holt----
# Applying Holt's linear method to the train data
holt_model <- holt(train_data, lambda = 0.0277327677958033, h=length(test_data))

# Check the model summary
summary(holt_model)

# Forecasting using the Holt model
holt_forecast <- forecast(holt_model, h=length(test_data))

# Plot the training data, forecasts, and the actual test data
plot(holt_forecast, main="Holt's Linear Method Forecast vs Actual")
lines(test_data, col = "red", lwd = 2)
legend("topleft", legend=c("Holt Linear (A,N)", "Actual"), col=c("blue", "red"), lty=1, lwd=2)

# Check residuals
checkresiduals(holt_model)

# Calculate and print model accuracy
accuracy_metrics <- forecast::accuracy(holt_forecast, test_data)
print(accuracy_metrics)


#Arima----

# Fit a arima model to the training data

# 2nd difference on train data
pce_ts_diff_2 <- diff(train_data, differences = 2)


tsdisplay(pce_ts_diff_2)


arima_model <- Arima(train_data, lambda = 0.0277327677958033,order = c(3,2,2))
arima_fc <- forecast(arima_model, h=length(test_data))

# Summary of the model
summary(arima_model)
plot(arima_fc)

# Plot the fitted model along with the actual data
plot(arima_fc, ylim =c(0,19000), main="Arima Model Fitting on PCE Data")
lines(test_data, col='red')
legend("topleft", legend=c("ARIMA", "Actual"), col=c("blue", "red"), lty=1)

# Check residuals
checkresiduals(arima_fc)

# Calculate and print model accuracy
accuracy_metrics <- forecast::accuracy(arima_fc, test_data)
print(accuracy_metrics)

# Auto-Arima----

# Fit a arima model to the training data
auto_arima_model <- auto.arima(train_data , lambda = 0.0277327677958033)
auto_arima_fc <- forecast(auto_arima_model, h=length(test_data))

# Summary of the model
summary(auto_arima_model)

plot(auto_arima_fc)
# Plot the fitted model along with the actual data
plot(auto_arima_fc, main="Auto-Arima Model Fitting on PCE Data")
lines(test_data, col='red')
legend("topleft", legend=c("Fitted", "Actual"), col=c("blue", "red"), lty=1)

# Check residuals
checkresiduals(auto_arima_fc)

# Calculate and print model accuracy
accuracy_metrics <- forecast::accuracy(auto_arima_fc, test_data)
print(accuracy_metrics)


# OCT2024 USING ARIMA
best_arima_model <- Arima(train_data, lambda = 0.0277327677958033,order = c(3,2,2))
arima_fc <- forecast(arima_model, h=11)
summary(best_arima_model)
summary(arima_fc)
# Check the model summary

#rwf# Display the forecast for October 2024 specifically
oct_2024_forecast <- arima_fc$mean[11]  # Assuming the forecast starts from January 2024
print(paste("Forecasted PCE for October 2024: ", oct_2024_forecast))

# Final fit model using Holt to predict oct 2024 ----

best_holt_model <- holt(pce_ts_imputed, h=11, lambda = 0.0277327677958033)
summary(best_holt_model)
# Check the model summary

#rwf# Display the forecast for October 2024 specifically
oct_2024_forecast <- best_holt_model$mean[11]  # Assuming the forecast starts from January 2024
print(paste("Forecasted PCE for October 2024: ", oct_2024_forecast))


# Using auto.arima to find the best fit-------
arima_model <- auto.arima(pce_ts_imputed)

# Summary of the ARIMA model
summary(arima_model)

# Plot the fitted model along with the actual data
plot(forecast(arima_model, h=length(test_data)), main="ARIMA Model Forecast vs Actual Data")
lines(test_data, col='red')
legend("topleft", legend=c("Fitted", "Actual"), col=c("blue", "red"), lty=1)

# Check residuals
checkresiduals(arima_model)

# Predicting October 2024 Using Auto Arima-----

# Fit the ARIMA model to the entire dataset (pce_ts_imputed)
best_arima_model <- auto.arima(pce_ts_imputed)

# Summary of the best ARIMA model
summary(best_arima_model)

# Forecast future values
future_forecast <- forecast(best_arima_model, h=12)  # Forecasting for 12 months ahead
future_forecast



# Display the forecast for October 2024 specifically
oct_2024_forecast <- future_forecast$mean[11]  # Assuming the forecast starts from January 2024
print(paste("Forecasted PCE for October 2024: ", oct_2024_forecast))


# Plot compare models-------

autoplot(pce_ts_imputed) +
  autolayer(drift_model$mean) +
  autolayer(holt_model$mean) +
  autolayer(arima_fc$mean) +
  labs(title="Forecast Comparison of all models", x="Time", y="PCE") 


# One-step ahead rolling forecasting without re-estimation-----

# Drift Model - Rolling Forecast
fit_drift <- rwf(train_data, drift=TRUE, lambda = 0.0277327677958033)
refit_drift <- rwf(pce_ts_imputed, model=fit_drift$model)
fc_drift <- window(fitted(refit_drift), start=c(2011,1))

# Calculate and print model accuracy
accuracy_metrics <- forecast::accuracy(fc_drift, test_data)
print(accuracy_metrics)

# Holt linear - Rolling Forecast
fit_holt <- holt(pce_ts_imputed, lambda = 0.0277327677958033)
refit_holt <- holt(pce_ts_imputed, model=fit_holt)
fc_holt <- window(fitted(refit_holt), start=c(2011,1))

# Calculate and print model accuracy
accuracy_metrics <- forecast::accuracy(fc_holt, test_data)
print(accuracy_metrics)

# ARIMA - Rolling Forecast
fit_arima <- auto.arima(train_data, lambda = 0.0277327677958033)
refit_arima <- Arima(pce_ts_imputed, model=fit_arima)
fc_arima <- window(fitted(refit_arima), start=c(2011,1))

# Calculate and print model accuracy
accuracy_metrics <- forecast::accuracy(fc_arima, test_data)
print(accuracy_metrics)

# Plotting the forecasts against the actual data
plot(pce_ts_imputed, main="One-step Rolling Forecasts vs Actual Data", col="grey", lwd=1, ylim=c(min(pce_ts_imputed, fc_drift, fc_holt, fc_arima), max(pce_ts_imputed, fc_drift, fc_holt, fc_arima)))
lines(fc_drift, col="red", lwd=2)
lines(fc_holt, col="blue", lwd=2)
lines(fc_arima, col="green", lwd=2)
legend("topleft", legend=c("Actual", "Drift", "Holt Linear", "ARIMA"), col=c("grey", "red", "blue", "green"), lty=1, lwd=2)

# Plotting only the test period for clarity
plot(test_data, main="Test Period: One-step Rolling Forecasts vs Actual Data", col="grey", lwd=1)
lines(fc_drift, col="red", lwd=2)
lines(fc_holt, col="blue", lwd=2)
lines(fc_arima, col="green", lwd=2)
legend("topleft", legend=c("Actual", "Drift", "Holt Linear", "ARIMA"), col=c("grey", "red", "blue", "green"), lty=1, lwd=2)

