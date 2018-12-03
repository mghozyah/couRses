library(tidyverse)
library(xts)
library(zoo)

####CHAPTER_1

####Prolog
#the xts constructor
#xts(x = NULL, order.by = index(x),frequency = NULL,unique = NULL,tzone = Sys.getenv("TZ"))
#xts = matrix + index
x <- matrix(1:4, ncol = 2, nrow = 2)
idx <- as.Date(c("2018-11-01", "2018-12-01"))
#an xts example
X <- xts(x, order.by = idx)
X
#Deconstructing xts
coredata(X, fmt = FALSE)
index(X)
#####

####More than a matrix####
# View the structure of ex_matrix
str(ex_matrix)
# Extract the 3rd observation of the 2nd column of ex_matrix
ex_matrix[3, 2]
# Extract the 3rd observation of the 2nd column of core 
core[3, 2]
####

####Your first xts object####
?rnorm
# Create the object data using 5 random numbers
data <- rnorm(5, mean = 0, sd = 1)
# Create dates as a Date class object starting from 2016-01-01
dates <- seq(as.Date("2016-01-01"), length = 5, by = "days")
# Use xts() to create smith
smith <- xts(x = data, order.by = dates)
# Create bday (1899-05-08) using a POSIXct date class object
bday <- as.POSIXct("1899-05-08")
# Create hayek and add a new attribute called born
hayek <- xts(x = data, order.by = dates, born = bday)
####

####Deconstructing xts####
# Extract the core data of hayek
hayek_core <- coredata(hayek)
# View the class of hayek_core
class(hayek_core)
# Extract the index of hayek
hayek_index <- index(hayek)
# View the class of hayek_index
class(hayek_index)
####

####Time based indices####
# Create an object of 5 dates called dates starting at "2016-01-01".
dates <- as.Date("2016-01-01") + 0:4
# Create ts_a  using the numbers 1 through 5 as your data, and dates as your order.by index
ts_a <- xts(x = 1:5, order.by = dates)
# Create ts_b  using the numbers 1 through 5 as your data, and the same dates, but as POSIXct objects.
ts_b <- xts(x = 1:5, order.by = as.POSIXct(dates))
# Extract the rows of ts_a using the index of ts_b
ts_a[index(ts_b)]
# Extract the rows of ts_b using the index of ts_a
ts_b[index(ts_a)]
####

####################################################
####Importing, exporting and converting time series#### <- video
#load data from R datasets
data("sunspots")
class(sunspots)
sunspots_xts <- as.xts(sunspots)
class(sunspots_xts)
head(sunspots); head(sunspots_xts)
#export xts from R
setwd("C:/Users/Ghozy Haqqoni/Documents/!Bahan Data Science Ghozy/DS Courses/DATACAMP/Time_Series_with_R_Track/1_Manipulating_Time_Series_Data_in_R_with_xts_&_zoo")
getwd()
write.zoo(sunspots_xts, "belajar_XTS")
saveRDS(sunspots_xts, "belajar_XTS")
####################################################

####Converting xts objects####
#as.xts()
data("austres")
# Convert austres to an xts object called au
au <- as.xts(austres)
# Then convert your xts object (au) into a matrix am
am <- as.matrix(au)
# Inspect the head of am
head(am)
# Convert the original austres into a matrix am2
am2 <- as.matrix(austres)
# Inspect the head of am2
head(am2)
####

####Importing data####
# Create dat by reading tmp_file using read.csv()
dat <- read.csv(tmp_file) 
# Convert dat into xts constructor
xts(dat, order.by = as.Date(rownames(dat), "%m/%d/%Y"))
# Read tmp_file using read.zoo
dat_zoo <- read.zoo(tmp_file, index.column = 0, sep = ",", format = "%m/%d/%Y")
# Convert dat_zoo to xts
dat_xts <- as.xts(dat_zoo)
####

####Exporting xts objects####
# Convert sunspots to xts using as.xts().
sunspots_xts <- as.xts(sunspots)
# Get the temporary file name
tmp <- tempfile()
# Write the xts object using zoo to tmp 
write.zoo(sunspots_xts, sep = ",", file = tmp)
# Read the tmp file. FUN = as.yearmon converts strings such as Jan 1749 into a proper time class
sun <- read.zoo(tmp, sep = ",", FUN = as.yearmon)
# Convert sun into xts. Save this as sun_xts
sun_xts <- as.xts(sun)
####

####CHAPTER_2

####################################################
####Introducing time based queries#### <-video
#### <- video
#load data
data(edhec, package = "PerformanceAnalytics")
head(edhec)
head(edhec["2007-01", 1])
# January 2007 to March 2007
#this format
head(edhec["2007-01/2007-03",1])
#or this
head(edhec["200701/03", 1])
#Time Support
# YYYYMMDDTHHMM format
iday["20160808T2213"] #T, use to seperate the date and time
#repeating intraday interval
iday["T05:30/T06:30"]
####################################################

####Querying for dates####
# Select all of 2016 from x
x_2016 <- x["2016"]
# Select January 1, 2016 to March 22, 2016
jan_march <- x["2016-01-01/2016-03-22"]
# Verify that jan_march contains 82 rows
82 == length(jan_march)
####

####Extracting recurring intraday intervals####
# Extract all data from irreg between 8AM and 10AM
morn_2010 <- irreg["T08:00/T10:00"]
# Extract the observations in morn_2010 for January 13th, 2010
morn_2010["2010-01-13"]
####

####################################################
####Alternative extraction techniques#### <-video
#Row selection w time
  #integer indexing
  x[c(1,2,3),]
  #logical vectors
  x[index(x)] > "2016-08-20"
  #date objects (Date, POSIXct, etc.)
  dates <- as.POSIXct(c("2016-06-25", "2016-06-27"))
  x[dates]
  x["2007-06-26/2007-06-28"]
#Modifying time series
index <- x["2007-06-26/2007-06-28", which.i = TRUE]
index
####################################################

####Row selection with time objects####
# Subset x using the vector dates
x[dates]
# Subset x using dates as POSIXct
dates <- as.POSIXct(dates)
x[dates]
####

####Update and replace elements####
# Replace the values in x contained in the dates vector with NA
x[dates] <- NA
#Replace all values in x for dates (not referring to the vector) from June 9, 2016 onward with 0. Use ISO-8601 style replacement.
x["2016-06-09/2016-06-30"] <- 0
# Verify that the value in x for June 11, 2016 is now indeed 0
x["2016-06-11"]
####

####Find the first or last period of time####
#fromvid
first(edhec[, "Funds of Funds"], "4 months")
last(edhec[, "Funds of Funds"], "1 year")
#
# Create lastweek using the last 1 week of temps
lastweek <- last(temps, "1 week")
# Print the last 2 observations in lastweek
last(lastweek, n=2)
# Extract all but the first two days of lastweek
first(lastweek, "-2 days")
####

####Combining first and last####
#fromvid
first(last(edhec[, "Merger Arbitrage"], "2 year"), "5 months")
#
temps
# Last 3 days of first week
last(first(temps, "1 week"), "3 days") 
# Extract the first three days of the second week of temps
first(last(first(temps, "2 weeks"), "1 week"), "3 days")
####

####Matrix arithmetic - add, subtract, multiply, and divide in time!####
# Add a and b
a + b
# Add a with the numeric value of b
as.numeric(b) + a 
####

####Math with non-overlapping indexes####
# Add a to b, and fill all missing rows of b with 0
a + merge(b, index(a), fill = 0)
# Add a to b and fill NAs with the last observation
a + merge(b, index(a), fill = na.locf)
####