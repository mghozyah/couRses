library(tidyverse)
library(astsa)
library(xts)
library(tseries)

####CHAPTER 1

####Data Play####
data("AirPassengers")
# Plot AirPassengers
plot(AirPassengers)
# Plot the DJIA daily closings
plot(djia$Close)
# Plot the Southern Oscillation Index
plot(soi)
####

####Differencing####
# Plot detrended y (trend stationary)
plot(diff(y))
# Plot detrended x (random walk)
plot(diff(x))
####

####Detrending Data####
# Plot globtemp and detrended globtemp
par(mfrow = c(2,1))
plot(globtemp)
plot(diff(globtemp))
# Plot cmort and detrended cmort
par(mfrow = c(2,1))
plot(cmort)
plot(diff(cmort))
####

####Dealing with Trend and Heteroscedasticity####
# astsa and xts are preloaded 
# Plot GNP series (gnp) and its growth rate
par(mfrow = c(3,1))
plot(gnp)
plot(diff(gnp))
plot(diff(log(gnp)))
# Plot DJIA closings (djia$Close) and its returns
par(mfrow = c(3,1))
plot(djia$Close)
plot(diff(djia$Close))
plot(diff(log(djia$Close)))
####

####Simulating ARMA Models####
#arima.sim(n = 63, list(ar = c(0.8897, -0.4858), ma = c(-0.2279, 0.2488)), rand.gen = function(n, ...) sqrt(0.1796) * rt(n, df = 5))
# Generate and plot white noise
WN <- arima.sim(model = list(order = c(0,0,0)), n=200)
plot(WN)
# Generate and plot an MA(1) with parameter .9 
MA <- arima.sim(model = list(order = c(0,0,1), ma = .9) ,n=200)
plot(MA)
# Generate and plot an AR(2) with parameters 1.5 and -.75
AR <- arima.sim(model = list(order = c(2,0,0), ar = c(1.5,-.75)), n=200)
plot(AR)
####


####CHAPTER 2

####Fitting an AR(1) Model####
# Generate 100 observations from the AR(1) model
x <- arima.sim(model = list(order = c(1, 0, 0), ar = .9), n = 100) 
# Plot the generated data 
plot(x)
# Plot the sample P/ACF pair
plot(acf2(x))
# Fit an AR(1) to the data and examine the t-table
sarima(x, 1, 0, 0)
####

####Fitting an AR(2) Model####
# astsa is preloaded
# Plot x
plot(x)
# Plot the sample P/ACF of x
plot(acf2(x))
# Fit an AR(2) to the data and examine the t-table
sarima(x,2,0,0)
####

####Fitting an MA(1) Model####
# astsa is preloaded
# Plot x
plot(x)
# Plot the sample P/ACF of x
plot(acf2(x))
# Fit an MA(1) to the data and examine the t-table
sarima(x,0,0,1)
####

####Fitting an ARMA model####
# astsa is preloaded
# Plot x
plot(x)
# Plot the sample P/ACF of x
plot(acf2(x))
# Fit an ARMA(2,1) to the data and examine the t-table
sarima(x,2,0,1)
####

####Model Choice - I####
# Fit an MA(1) to dl_varve.   
dl_varve <- diff(log(varve))
sarima(dl_varve, 0,0,1)
# Fit an MA(2) to dl_varve. Improvement?
sarima(dl_varve, 0,0,2)
# Fit an ARMA(1,1) to dl_varve. Improvement?
sarima(dl_varve, 1,0,1)
####

####Residual Analysis - I####
# Fit an MA(1) to dl_varve. Examine the residuals  
sarima(dl_varve,0,0,1)
# Fit an ARMA(1,1) to dl_varve. Examine the residuals
sarima(dl_varve,1,0,1)
####

####ARMA get in####
# Calculate approximate oil returns
oil_returns <- diff(log(oil))
# Plot oil_returns. Notice the outliers
plot(oil_returns)
# Plot the P/ACF pair for oil_returns
plot(acf2(oil_returns))
# Assuming both P/ACF are tailing, fit a model to oil_returns
#sarima(oil_returns,_,_,_)
sarima(oil_returns,1,0,1)
####
