####CHAPTER 1
library(tidyverse)
library(xts)
library(tseries)
library(zoo)
library(astsa)
library(rugarch) #garch

setwd("C:/Users/Ghozy Haqqoni/Documents/!Bahan Data Science Ghozy/DS Courses/DATACAMP/Time_Series_with_R_Track/7_GARCH_Models_in_R/")

####ANALYZING VOLATILITY#### <-video
library(PerformanceAnalytics)
###Calculating Returns
# Example in R for daily S&P 500 prices (xts object)
SP500returns <- CalculateReturns(SP500prices)
###How to estimate return volatility
#Compute daily standard deviation
sd(sp500ret) #0.01099357
###Annualized volatility
# Compute annualized standard deviation
sqrt(252)*sd(sp500ret) #0.1745175
###Rolling volatility estimation
#Function chart.RollingPerformance()
chart.RollingPerformance(R = sp500ret ,
                         width = 22,
                         FUN = "sd.annualized",
                         scale = 252, 
                         main = "Rolling 1 month volatility")
############################

####Computing returns####
library(xts)
library(PerformanceAnalytics)
# Plot daily S&P 500 prices
plot(sp500prices)
# Compute daily returns
sp500ret <- CalculateReturns(sp500prices)
# Check the class of sp500ret
class(sp500ret)
# Plot daily returns
plot(sp500ret)
####

####Standard deviation on subsamples####
#number of periods in a year (daily scale = 252, 
#monthly scale = 12, quarterly scale = 4)
# Compute the daily standard deviation for the complete sample   
sd(sp500ret)
# Compute the annualized volatility for the complete sample
sqrt(252)*sd(sp500ret)
# Compute the annualized standard deviation for the year 2009 
sqrt(252)* sd(sp500ret["2009"])
# Compute the annualized standard deviation for the year 2017 
sqrt(252)* sd(sp500ret["2017"])
####

####Roll, roll, roll####
# Load the library PerformanceAnalytics
library(PerformanceAnalytics)

# Showing two plots on the same figure
par(mfrow=c(2,1)) 
#1 month estimate, width = 22
# Compute the rolling 1 month estimate of annualized volatility
chart.RollingPerformance(R = sp500ret["2000::2017"], 
                         width = 22,
                         FUN = "sd.annualized", 
                         scale = 252, 
                         main = "One month rolling volatility")
# Compute the rolling 3 months estimate of annualized volatility
chart.RollingPerformance(R = sp500ret["2000::2017"], width = 22*3,
                         FUN ="sd.annualized", scale = 252, 
                         main = "Three months rolling volatility")
####


####GARCH EQUATION FOR VOLATILITY PREDICTION#### <-video
###Specify Inputs
# Set parameter values
alpha <- 0.1
beta <- 0.8
omega <- var(sp500ret)*(1-alpha-beta)
# Then: var(sp500ret) = omega/(1-alpha-beta)
# Set series of prediction error
e <- sp500ret - mean(sp500ret) # Constant mean
e2 <- e^2
# Volatility is sqrt of predicted variance
predvol <- sqrt(predvar)
predvol <- xts(predvol, order.by = time(sp500ret))
# We compare with the unconditional volatility
uncvol <- sqrt(omega / (1 - alpha-beta))
uncvol <- xts(rep(uncvol, nobs), order.by = time(sp500ret))
# Plot
plot(predvol)
lines(uncvol, col = "red", lwd = 2)
#####################################

####Prediction errors####
# Compute the mean daily return
m <- mean(sp500ret)
# Define the series of prediction errors
e <- sp500ret - m
# Plot the absolute value of the prediction errors
par(mfrow = c(2,1),mar = c(3, 2, 2, 2))
plot(abs(e))
# Plot the acf of the absolute prediction errors
plot(acf(abs(e)))
####

####The recursive nature of the GARCH variance####
# Compute the predicted variances
predvar[1] <- var(sp500ret) 
for(t in 2:nobs){
  predvar[t] <- omega + alpha* e2[t-1] + beta * predvar[t-1]
}
# Create annualized predicted volatility
ann_predvol <- xts(sqrt(predvar) * sqrt(252), order.by = time(sp500ret))
# Plot the annual predicted volatility in 2008 and 2009
plot(ann_predvol["2008::2009"], main = "Ann. S&P 500 vol in 2008-2009")
####

####THE RUGARCH PACKAGE#### <-video
###Workflow in R
#ugarchspec(): Specify which GARCH model you want to use.
# Constant mean, standard garch(1,1) model
garchspec <- ugarchspec( 
  mean.model = list(armaOrder = c(0,0)),
  variance.model = list(model = "sGARCH"),
  distribution.model = "norm")
#ugarchfit(): Estimate the GARCH model
garchfit <- ugarchfit(data = sp500ret , 
                      spec = garchspec)
#ugarchforecast(): Forecast the volatility of the future returns
garchforecast <- ugarchforecast(fitORspec = garchfit, 
                                n.ahead = 5)
###Ugarchfit Object
# Coefficients
garchcoef <- coef(garchfit)
# Unconditional variance
garchuncvar <- uncvariance(garchfit)
# Predicted mean
garchmean <- fitted(garchfit) 
# Predicted volatilities
garchvol <- sigma(garchfit)
###Estimated GARCH coeff returns
print(garchcoef)
#           mu        omega       alpha1        beta1 
# 5.728020e-04 1.220515e-06 7.792031e-02 9.111455e-01
###Estimated volatilities 
garchvol <- sigma(garchfit)
plot(garchvol)
###What about future volatility?
tail(garchvol, 1)
# 2017-12-29 0.004862908
###Forecasting h-day ahead volatilities
#Applying the sigma() method to the ugarchforecast object gives the volatility forecasts:
sigma(garchforecast)
#Applying the fitted() method to the ugarchforecast object gives the mean forecasts:
fitted(garchforecast)
#####################################

####Specify and taste the GARCH model flavors####
library(rugarch)
citation("rugarch")
# Specify a standard GARCH model with constant mean
garchspec <- ugarchspec( 
  mean.model = list(armaOrder = c(0,0)),
  variance.model = list(model = "sGARCH"),
  distribution.model = "norm")
# Estimate the model
garchfit <- ugarchfit(data = sp500ret, spec = garchspec)
# Use the method sigma to retrieve the estimated volatilities 
garchvol <-  sigma(garchfit)
# Plot the volatility for 2017
plot(garchvol["2017"])
####

####Out-of-sample forecasting####
# Compute unconditional volatility
#The volatility is the square root of the variance.
sqrt(uncvariance(garchfit))
# Print last 10 ones in garchvol
tail(garchvol, 10)
# Forecast volatility 5 days ahead and add 
garchforecast <- ugarchforecast(fitORspec = garchfit, 
                                n.ahead = 5)
# Extract the predicted volatilities and print them
print(sigma(garchforecast))
####

####Volatility targeting in tactical asset allocation####
# Compute the annualized volatility
annualvol <- sqrt(252) * sqrt(sp500ret)
# Compute the 5% vol target weights  
vt_weights <- 0.05 / annualvol
vt_weights
# Compare the annualized volatility to the portfolio weights in a plot
plot(merge(annualvol, vt_weights), multi.panel = TRUE)

####