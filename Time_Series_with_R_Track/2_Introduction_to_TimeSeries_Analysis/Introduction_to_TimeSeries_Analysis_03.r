library(tidyverse)
library(stringr)
library(purrr)
library(lubridate)
library(readr)

setwd("C:/Users/Ghozy Haqqoni/Documents/!Kuliah/Semester 5 - DS/Time Series/datas/")
data("EuStockMarkets")
eu_stocks <- EuStockMarkets

####SCATTER PLOT####

####Asset prices vs. asset returns####
# Plot eu_stocks
plot(eu_stocks)
# Use this code to convert prices to returns
returns <- eu_stocks[-1,] / eu_stocks[-1860,] - 1
# Convert returns to ts
returns <- ts(returns, start = c(1991, 130), frequency = 260)
# Plot returns
plot(returns)
# Use this code to convert prices to log returns
logreturns <- diff(log(eu_stocks))
# Plot logreturns
plot(logreturns)
####

####Characteristics of financial time series####
library(data.table)

# Generate means from eu_percentreturns
colMeans(eu_percentreturns)
# Use apply to calculate sample variance from eu_percentreturns
apply(eu_percentreturns, MARGIN = 2, FUN = var)
# Use apply to calculate standard deviation from eu_percentreturns
apply(eu_percentreturns, MARGIN = 2, FUN = sd)
# Display a histogram of percent returns for each index
par(mfrow = c(2,2))
apply(eu_percentreturns, MARGIN = 2, FUN = hist, main = "", xlab = "Percentage Return")
# Display normal quantile plots of percent returns for each index
par(mfrow = c(2,2))
apply(eu_percentreturns, MARGIN = 2, FUN = qqnorm, main = "")
qqline(eu_percentreturns)
####

####Plotting pairs of data####
# Make a scatterplot of DAX and FTSE
plot(DAX, FTSE)
# Make a scatterplot matrix of eu_stocks
pairs(eu_stocks)
# Convert eu_stocks to log returns
logreturns <- diff(log(eu_stocks))
# Plot logreturns
plot(logreturns)
# Make a scatterplot matrix of logreturns
pairs(logreturns)
####

####COVARIANCE AND CORRELATION####

####Calculating sample covariances and correlations####
# Use cov() with DAX_logreturns and FTSE_logreturns
cov(DAX_logreturns, FTSE_logreturns)
# Use cov() with logreturns
cov(logreturns)
# Use cor() with DAX_logreturns and FTSE_logreturns
cor(DAX_logreturns, FTSE_logreturns)
# Use cor() with logreturns
cor(logreturns)
####

####AUTOCORRELATION####

####Calculating autocorrelations####
# Define x_t0 as x[-1]
x_t0 <- x[-1]
# Define x_t1 as x[-n] #n=150
x_t1 <- x[-n]
# Confirm that x_t0 and x_t1 are (x[t], x[t-1]) pairs  
head(cbind(x_t0, x_t1))
# Plot x_t0 and x_t1
plot(x_t0, x_t1)
# View the correlation between x_t0 and x_t1
cor(x_t0, x_t1)
# Use acf with x
acf(x, lag.max = 1, plot = FALSE)
# Confirm that difference factor is (n-1)/n
cor(x_t1, x_t0) * (n-1)/n
####

####The autocorrelation function####
# Generate ACF estimates for x up to lag-10
acf(x, lag.max = 10, plot = FALSE)
# Type the ACF estimate at lag-10 
#0.100
# Type the ACF estimate at lag-5
#0.198
####

####Visualizing the autocorrelation function####
# View the ACF of x
acf(x)
# View the ACF of y
acf(y)
# View the ACF of z
acf(z)
####

