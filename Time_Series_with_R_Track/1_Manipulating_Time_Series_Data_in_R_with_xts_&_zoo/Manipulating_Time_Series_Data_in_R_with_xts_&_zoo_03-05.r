library(tidyverse)
library(xts)
library(zoo)
library(readr)

setwd("C:/Users/Ghozy Haqqoni/Documents/!Bahan Data Science Ghozy/DS Courses/DATACAMP/Time_Series_with_R_Track/1_Manipulating_Time_Series_Data_in_R_with_xts_&_zoo/")
temps <- read_csv(Temps.csv)
usd_eur <- read_csv(USDEUR.csv)

####CHAPTER_3

####Combining xts by column with merge####
# Perform an inner join of a and b
merge(a, b, join = "inner")
# Perform a left-join of a and b, fill missing values with 0
merge(a, b, join = "left", fill = 0)
####

####Combining xts by row with rbind####
# Row bind temps_june30 to temps, assign this to temps2
temps2 <- rbind(temps_june30, temps)
# Row bind temps_july17 and temps_july18 to temps2, call this temps3
temps3 <- rbind(temps_july17, temps_july18, temps2)
####

####Fill missing values using last or previous observation####
# Fill missing values in temps using the last observation
temps_last <- na.locf(temps)
# Fill missing values in temps using the next observation
temps_next <- na.locf(temps, fromLast = TRUE)
####

####NA interpolation using na.approx()####
# Interpolate NAs using linear approximation
na.approx(AirPass)
####

####Combine a leading and lagging time series####
# Create a leading object called lead_x
lead_x <- lag(x, k = -1)
# Create a lagging object called lag_x
lag_x <- lag(x, k = 1)
# Merge your three series together and assign to z
z <- cbind(lead_x, x ,lag_x)
####

####Calculate a difference of a series using diff()####
# Calculate the first difference of AirPass using lag and subtraction
diff_by_hand <- AirPass - lag(AirPass)
# Use merge to compare the first parts of diff_by_hand and diff(AirPass)
merge(head(diff_by_hand), head(diff(AirPass)))
# Calculate the first order 12 month difference of AirPass
diff(AirPass, lag = 12, differences = 1)
####

####CHAPTER_4
####Find intervals by time in xts####
#Use endpoints() to locate the end of week in your temps data.
# Use endpoints() to locate the end of every second week in your temps data. Remember to use the k argument.
# Locate the weeks
endpoints(temps, on = "weeks")
# Locate every two weeks
endpoints(temps, on = "weeks", k = 2)
####

####Apply a function by time period(s)####
# Calculate the weekly endpoints
ep <- endpoints(temps, on = "weeks")
# Now calculate the weekly mean and display the results
period.apply(temps[, "Temp.Mean"], INDEX = ep, FUN = mean)
####

####Using lapply() and split() to apply functions on intervals####
# Split temps by week
temps_weekly <- split(temps, f = "weeks")
temps_weekly
# Create a list of weekly means, temps_avg, and print this list
temps_avg <- lapply(X = temps_weekly, FUN = mean)
temps_avg
####

####Selection by endpoints vs. split-lapply-rbind####
# Use the proper combination of split, lapply and rbind
temps_1 <- do.call(rbind, lapply(split(temps, "weeks"), function(w) last(w, n = "1 day")))
# Create last_day_of_weeks using endpoints()
last_day_of_weeks <- endpoints(temps, "weeks")
# Subset temps using last_day_of_weeks 
temps_2 <- temps[last_day_of_weeks]
####

####Convert univariate series to OHLC data####
# Convert usd_eur to weekly and assign to usd_eur_weekly
usd_eur_weekly <- to.period(usd_eur, period = "weeks")
# Convert usd_eur to monthly and assign to usd_eur_monthly
usd_eur_monthly <- to.period(usd_eur, period = "months")
# Convert usd_eur to yearly univariate and assign to usd_eur_yearly
usd_eur_yearly <- to.period(usd_eur, period = "years", OHLC = FALSE)
####

####Convert a series to a lower frequency####
data(edhec, package = "PerformanceAnalysis")
# Convert eq_mkt to quarterly OHLC
mkt_quarterly <- to.period(eq_mkt, period = "quarters", OHLC = TRUE)
# Convert eq_mkt to quarterly using shortcut function
mkt_quarterly2 <- to.quarterly(eq_mkt, name = NULL, indexAt = "firstof")
####

####Calculate basic rolling value of series by month####
# Split edhec into years
edhec_years <- split(edhec , f = "years")
# Use lapply to calculate the cumsum for each year in edhec_years
?lapply
edhec_ytd <- lapply(edhec_years, FUN = cumsum)
# Use do.call to rbind the results
edhec_xts <- do.call(rbind, edhec_ytd)
####

####Calculate the rolling standard deviation of a time series####
# Use rollapply to calculate the rolling 3 period sd of eq_mkt
#rollapply(x, width = 10, FUN = max, na.rm = TRUE)
eq_sd <- rollapply(eq_mkt, 3, FUN = sd)
####

####CHAPTER_5

####Class attributes - tclass, tzone, and tformat####
# View the first three indexes of temps
index(temps)[1:3]
# Get the index class of temps
indexClass(temps)
# Get the timezone of temps
indexTZ(temps)
# Change the format of the time display
indexFormat(temps) <- "%b-%d-%Y"
# View the new format
head(temps)
####

####Time Zones (and why you should care!)####
# Construct times_xts with tzone set to America/Chicago
#times_xts <- xts(___:___, order.by = ___, tzone = ___)
times_xts <- xts(1:10, order.by = times, tzone = "America/Chicago")
# Change the time zone of times_xts to Asia/Hong_Kong
#tzone(___) <- 
tzone(times_xts) <- "Asia/Hong_Kong"
# Extract the current time zone of times_xts
indexTZ(times_xts)
####

####Determining periodicity####
# Calculate the periodicity of temps
periodicity(temps)
# Calculate the periodicity of edhec
periodicity(edhec)
# Convert edhec to yearly
edhec_yearly <- to.yearly(edhec)
# Calculate the periodicity of edhec_yearly
periodicity(edhec_yearly)
####

####Find the number of periods in your data####
# Count the months
nmonths(edhec)
# Count the quarters
nquarters(edhec)
# Count the years
nyears(edhec)
####

####Secret index tools####
# Explore underlying units of temps in two commands: .index() and .indexwday()
.index(temps)
.indexwday(temps)
# Create an index of weekend days using which()
index <- which(.indexwday(temps) == 0 | .indexwday(temps) == 6)
# Select the index
temps[index]
####

####Modifying timestamps####
# Make z have unique timestamps
z_unique <- make.index.unique(z, eps = 1e-4)
# Remove duplicate times in z
z_dup <- make.index.unique(z, drop = TRUE)
# Round observations in z to the next hour
z_round <- align.time(z, n = 3600)
####
