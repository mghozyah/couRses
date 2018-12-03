library(tidyverse)
library(stringr)
library(purrr)
library(lubridate)
library(readr)
library(forecast)

setwd("C:/Users/Ghozy Haqqoni/Documents/!Kuliah/Semester 5 - DS/Time Series/datas/")

#########
#CHAPTER 4
#########

####Simulate the autoregressive model####
# Simulate an AR model with 0.5 slope
x <- arima.sim(model = list(ar = 0.5), n = 100)
# Simulate an AR model with 0.9 slope
y <- arima.sim(model = list(ar = 0.9), 100)
# Simulate an AR model with -0.75 slope
z <- arima.sim(model = list(ar = -0.75), 100)
# Plot your simulated data
plot.ts(cbind(x, y, z))
####

####Estimate the autocorrelation function (ACF) for an autoregression####
# Calculate the ACF for x
acf(x)
# Calculate the ACF for y
acf(y)
# Calculate the ACF for z
acf(z)
####

####Compare the random walk (RW) and autoregressive (AR) models####
# Simulate and plot AR model with slope 0.9 
x <- arima.sim(model = list(ar =.9), n = 200)
ts.plot(x)
acf(x)
# Simulate and plot AR model with slope 0.98
y <- arima.sim(model = list(ar =.98), n = 200)
ts.plot(y)
acf(y)
# Simulate and plot RW model (diff = 1)
z <- arima.sim(model = list(order = c(0,1,0)), n = 200)
ts.plot(z)
acf(z)
####

####################################################
####AR Model Estimation and Forecasting#### <- video
#load data
data(Mishkin, package = "Ecdat")
class(Mishkin)
View(Mishkin)
#make inflation -> ts
inflation <- as.ts(Mishkin[,1])
class(inflation)
#AR Processes: Inflation Rate
ts.plot(inflation); acf(inflation)
#AR Model: Inflation Rate
library(forecast)
AR_inflation2 <- arima(inflation, order = c(1,0,0))
AR_inflation2
#AR Processes: Fitted Values 
ts.plot(inflation)
AR_inflation_fitted2 <- inflation - residuals(AR_inflation2)
points(AR_inflation_fitted2, type = "l", col = "red",
       lty = 2)
#forecasting
predict(AR_inflation2) #1-step ahead forecasts
predict(AR_inflation2, n.ahead = 5) #n-step ahead forecasts
####
####################################################

####Estimate the autoregressive (AR) model####
# Fit the AR model to x
arima(x, order = c(1,0,0))
# Copy and paste the slope (ar1) estimate
0.8575
# Copy and paste the slope mean (intercept) estimate
-0.0948
# Copy and paste the innovation variance (sigma^2) estimate
1.022
# Fit the AR model to AirPassengers
AR <- arima(AirPassengers, order = c(1,0,0))
print(AR)
# Run the following commands to plot the series and fitted values
ts.plot(AirPassengers)
AR_fitted <- AirPassengers - residuals(AR)
print(AR_fitted)
points(AR_fitted, type = "l", col = 2, lty = 2)
####

####Simple forecasts from an estimated AR model####
data("Nile")
# Fit an AR model to Nile
AR_fit <-arima(Nile, order  = c(1,0,0))
print(AR_fit)
# Use predict() to make a 1-step forecast
predict_AR <- predict(AR_fit)
# Obtain the 1-step forecast using $pred[1]
predict_AR$pred[1]
# Use predict to make 1-step through 10-step forecasts
predict(AR_fit, n.ahead = 10)
# Run to plot the Nile series plus the forecast and 95% prediction intervals
ts.plot(Nile, xlim = c(1871, 1980))
AR_forecast <- predict(AR_fit, n.ahead = 10)$pred
AR_forecast_se <- predict(AR_fit, n.ahead = 10)$se
points(AR_forecast, type = "l", col = 2)
points(AR_forecast - 2*AR_forecast_se, type = "l", col = 2, lty = 2)
points(AR_forecast + 2*AR_forecast_se, type = "l", col = 2, lty = 2)
####

#########
#CHAPTER 5
#########

#The Simple Moving Average Model

####Simulate the simple moving average model####
# Generate MA model with slope 0.5
x <- arima.sim(model = list(ma = 0.5), n = 100)
# Generate MA model with slope 0.9
y <- arima.sim(model = list(ma = 0.9), n = 100)
# Generate MA model with slope -0.5
z <- arima.sim(model = list(ma = -0.5), n = 100) 
# Plot all three models together
plot.ts(cbind(x, y, z))
####

####Estimate the autocorrelation function (ACF) for a moving average####
# Calculate ACF for x
acf(x)
# Calculate ACF for y
acf(y)
# Calculate ACF for z
acf(z)
####

####################################################
####MA Model Estimation and Forecasting
#load data
data(Mishkin, package = "Ecdat")
class(Mishkin)
View(Mishkin)
#make inflation -> ts
inflation <- as.ts(Mishkin[,1])
class(inflation)
#MA Processes: Changes (diff) in Inflation Rate
inflation_diff <- diff(inflation)
ts.plot(inflation); ts.plot(inflation_diff)
acf(inflation_diff, lag.max=24)
#MA Model
MA_inflation_diff <- arima(inflation_diff, order=c(0,0,1))
MA_inflation_diff
#MA Processes: Fitted Value
ts.plot(inflation_diff)
MA_inflation_diff_fitted <- inflation_diff - residuals(MA_inflation_diff)
points(MA_inflation_diff_fitted, type="l",
       col = "red", lty = 2)
#forecast
predict(MA_inflation_diff, n.ahead = 5)
####################################################

####Estimate the simple moving average model####
# Fit the MA model to x
arima(x, order = c(0,0,1))
# Paste the slope (ma1) estimate below
.7928
# Paste the slope mean (intercept) estimate below
.1589
# Paste the innovation variance (sigma^2) estimate below
.9576
# Fit the MA model to Nile
data("Nile")
MA <- arima(Nile, order = c(0,0,1))
print(MA)
AIC(MA); BIC(MA)
# Plot Nile and MA_fit 
ts.plot(Nile)
MA_fit <- Nile - resid(MA)
points(MA_fit, type = "l", col = 2, lty = 2)
####

####Simple forecasts from an estimated MA model####
# Make a 1-step forecast based on MA
predict_MA <- predict(MA)
predict_MA
# Obtain the 1-step forecast using $pred[1]
predict_MA$pred[1]
# Make a 1-step through 10-step forecast based on MA
predict(MA, 10)
# Plot the Nile series plus the forecast and 95% prediction intervals
ts.plot(Nile, xlim = c(1871, 1980))
MA_forecasts <- predict(MA, n.ahead = 10)$pred
MA_forecast_se <- predict(MA, n.ahead = 10)$se
points(MA_forecasts, type = "l", col = 2)
points(MA_forecasts - 2*MA_forecast_se, type = "l", col = 2, lty = 2)
points(MA_forecasts + 2*MA_forecast_se, type = "l", col = 2, lty = 2)
####

#Compare the AR and MA Models

####AR vs MA models####
# Find correlation between AR_fit and MA_fit
cor(AR_fit, MA_fit)
# Find AIC of AR
AIC(AR)
# Find AIC of MA
AIC(MA)
# Find BIC of AR
BIC(AR)
# Find BIC of MA
BIC(MA)
####
