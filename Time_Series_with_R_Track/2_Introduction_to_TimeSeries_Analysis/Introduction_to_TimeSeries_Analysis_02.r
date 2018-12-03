library(tidyverse)
library(stringr)
library(purrr)
library(lubridate)
library(readr)

setwd("C:/Users/Ghozy Haqqoni/Documents/!Kuliah/Semester 5 - DS/Time Series/datas/")


####Removing trends in variability via the logarithmic transformation####
# Log rapid_growth
ts.plot(rapid_growth)
linear_growth <- log(rapid_growth)
# Plot linear_growth using ts.plot()
ts.plot(linear_growth)
####

####Removing trends in level by differencing####
# Generate the first difference of z
dz <- diff(z) 
# Plot dz
ts.plot(dz)
# View the length of z and dz, respectively
length(z)
length(dz)
####

####Removing seasonal trends with seasonal differencing####
# Generate a diff of x with lag = 4. Save this to dx
dx <- diff(x, lag = 4)
# Plot dx
ts.plot(dx)
# View the length of x and dx, respectively 
length(x)
length(dx)
####

####Simulate the white noise model####
# Simulate a WN model with list(order = c(0, 0, 0))
#ARIMA(0,0,0)
white_noise <- arima.sim(model = list(order = c(0, 0, 0)), n = 100) #produce 100obs
# Plot your white_noise data
ts.plot(white_noise)
# Simulate from the WN model with: mean = 100, sd = 10
white_noise_2 <- arima.sim(model = list(order = c(0, 0, 0)), n = 100, mean = 100, sd = 10)
# Plot your white_noise_2 data
ts.plot(white_noise_2)
####

####Estimate the white noise model####
# Fit the WN model to y using the arima command
arima(y, order = c(0,0,0))
# Calculate the sample mean and sample variance of y
mean(y)
var(y)
####

####Simulate the random walk model####
# Generate a RW model using arima.sim
random_walk <- arima.sim(model = list(order = c(0, 1, 0)), n = 100)
# Plot random_walk
ts.plot(random_walk)
# Calculate the first difference series
random_walk_diff <- diff(random_walk) 
# Plot random_walk_diff
ts.plot(random_walk_diff)
####

####Simulate the random walk model with a drift####
# Generate a RW model with a drift uing arima.sim
#mean=1 to produce a drift
rw_drift <- arima.sim(model = list(order = c(0,1,0)), n =100, mean = 1)
# Plot rw_drift
ts.plot(rw_drift)
# Calculate the first difference series
rw_drift_diff <- diff(rw_drift) 
# Plot rw_drift_diff
ts.plot(rw_drift)
####

####Estimate the random walk model####
# Difference your random_walk data
rw_diff <- diff(random_walk)
# Plot rw_diff
ts.plot(rw_diff)
# Now fit the WN model to the differenced data
model_wn <- arima(rw_diff, order = c(0,0,0))
# Store the value of the estimated time trend (intercept)
int_wn <- model_wn$coef
# Plot the original random_walk data
ts.plot(random_walk)
# Use abline(0, ...) to add time trend to the figure
abline(0, int_wn)
####

####Are the white noise model or the random walk model stationary?####
# Use arima.sim() to generate WN data
white_noise <- arima.sim(model = list(order = c(0,0,0)), n=100)
# Use cumsum() to convert your WN data to RW
random_walk <- cumsum(white_noise)
# Use arima.sim() to generate WN drift data
wn_drift <- arima.sim(model = list(order = c(0,0,0)), n=100, mean=0.4)
# Use cumsum() to convert your WN drift data to RW
rw_drift <-  cumsum(wn_drift)
# Plot all four data objects
plot.ts(cbind(white_noise, random_walk, wn_drift, rw_drift))
####

