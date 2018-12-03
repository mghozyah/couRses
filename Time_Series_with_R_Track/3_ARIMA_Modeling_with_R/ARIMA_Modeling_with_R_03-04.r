library(tidyverse)
library(readr)
library(xts)
library(tseries)
library(astsa)

####CHAPTER 3

####ARIMA - Plug and Play####
x <- arima.sim(model = list(order = c(1, 1, 0), 
                            ar = .9), n = 200)
# Plot x
plot(x)
# Plot the P/ACF pair of x
plot(acf2(x))
# Plot the differenced data
plot(diff(x))
# Plot the P/ACF pair of the differenced data
plot(acf2(diff(x)))
####

####Simulated ARIMA####
# Plot sample P/ACF of differenced data and determine model
y <- diff(x)
plot(acf2(y))
# Estimate parameters and examine output
sarima(x,2,1,0)
####

####Global Warming####
# Plot the sample P/ACF pair of the differenced data 
plot(acf2(diff(globtemp)))
# Fit an ARIMA(1,1,1) model to globtemp
sarima(globtemp, 1,1,1)
# Fit an ARIMA(0,1,2) model to globtemp. Which model is better?
sarima(globtemp, 0,1,2)
#0,1,2
####

####Diagnostics - Simulated Overfitting####
# Plot sample P/ACF pair of the differenced data
plot(acf2(diff(x)))
# Fit the first model, compare parameters, check diagnostics
sarima(x, 0,1,1)
# Fit the second model and compare fit
sarima(x, 0,1,2)
# As you can see from the t-table, the second MA parameter is not significantly 
# different from zero and the first MA parameter is approximately the same 
# in each run. Also, the AIC and BIC both increase when the parameter is added. 
# In addition, the residual analysis of your ARIMA(0,1,1) model is fine. 
# All of these facts together indicate that you have a successful model fit.
####

####Diagnostics - Global Temperatures####
# Fit ARIMA(0,1,2) to globtemp and check diagnostics  
sarima(globtemp, 0,1,2) #-3.58 , -4.51
# Fit ARIMA(1,1,1) to globtemp and check diagnostics
sarima(globtemp, 1,1,1) #-3.57 , -4.51
# Which is the better model?
# "ARIMA(___,___,___)"
"ARIMA(0,1,2)"
####

####Forecasting Simulated ARIMA####
# Plot P/ACF pair of differenced data 
plot(acf2(diff(x)))
# Fit model - check t-table and diagnostics
sarima(x, 1,1,0)
# Forecast the data 20 time periods ahead
# sarima.for(x, n.ahead = ___, p = ___, d = ___, q = ___) 
sarima.for(x, n.ahead =20, p = 1, d = 1, q = 0) 
lines(y)
####

####Forecasting Global Temperatures####
# Fit an ARIMA(0,1,2) to globtemp and check the fit
sarima(globtemp, 0,1,2)
# Forecast data 35 years into the future
sarima.for(globtemp, 35, 0,1,2)
####

####CHAPTER 4

####Fit a Pure Seasonal Model####
# Plot sample P/ACF to lag 60 and compare to the true values
acf2(x, max.lag = 60)
# Fit the seasonal model to x
#sarima(x, p = 0, d = 0, q = 0, 
#       P = ___, D = 0, Q = ___, S = ___)
sarima(x, p = 0, d = 0, q = 0, 
       P = 1, D = 0, Q = 1, 
       S = 12)
####

####Fit a Mixed Seasonal Model####
# Plot sample P/ACF pair to lag 60 and compare to actual
plot(acf2(x, max.lag=60))
# Fit the seasonal model to x
sarima(x, 0,0,1, 0,0,1, 12)
####

####Data Analysis - Unemployment I####
par(mfrow = c(3,1))
# Plot unemp 
plot(unemp)
# Difference your data and plot it
d_unemp <-diff(unemp)
plot(d_unemp)
# Seasonally difference d_unemp and plot it
#dd_unemp <- diff(___, lag = 12)  
dd_unemp <- diff(d_unemp, lag = 12) 
plot(dd_unemp)
####

####Data Analysis - Unemployment II####
# Plot P/ACF pair of fully differenced data to lag 60
dd_unemp <- diff(diff(unemp), lag = 12)
plot(acf2(dd_unemp, max.lag=60))
# Fit an appropriate model
sarima(unemp, 2,1,0 , 0,1,1, 12)
####

####Data Analysis - Commodity Prices####
# Plot differenced chicken
chicken
plot(diff(chicken))
# Plot P/ACF pair of differenced data to lag 60
plot(acf2(diff(chicken), max.lag=60))
# Fit ARIMA(2,1,0) to chicken - not so good
sarima(chicken, 2,1,0)
# Fit SARIMA(2,1,0,1,0,0,12) to chicken - that works
sarima(chicken, 2,1,0, 1,0,0, 12)
####

####Data Analysis - Birth Rate####
# Plot P/ACF to lag 60 of differenced data
d_birth <- diff(birth)
plot(acf2(d_birth, max.lag=60))
# Plot P/ACF to lag 60 of seasonal differenced data
dd_birth <- diff(d_birth, lag = 12)
plot(acf2(dd_birth, max.lag=60))
# Fit SARIMA(0,1,1)x(0,1,1)_12. What happens?
sarima(birth, 0,1,1, 0,1,1, 12)
# Add AR term and conclude
sarima(birth, 1,1,1, 0,1,1, 12)
####

####Forecasting Monthly Unemployment####
# Fit your previous model to unemp and check the diagnostics
sarima(unemp, 2,1,0, 0,1,1, 12)
# Forecast the data 3 years into the future
sarima.for(unemp,n.ahead = 3*12, 2,1,0, 
           P=0,D=1,Q=1, 
           S=12)
####

####How Hard is it to Forecast Commodity Prices?####
# Fit the chicken model again and check diagnostics
sarima(chicken, 2,1,0, 1,0,0, 12)
# Forecast the chicken data 5 years into the future
sarima.for(chicken, n.ahead=5*12, 2,1,0, 1,0,0, 12)
####