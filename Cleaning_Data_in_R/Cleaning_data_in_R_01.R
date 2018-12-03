
library(tidyr)
library(dplyr) #pipeline
library(ggplot2)

#load the lunch data
setwd("C:/Users/Ghozy Haqqoni/Documents/!Bahan Data Science Ghozy/DS Courses/DATACAMP/Cleaning_Data_in_R/datasets_cleaning_data/")
lunch <- read.csv("lunch.csv")
#for_challenge
weather <- readRDS("weather.rds")


###########EXPLORING RAW DATA#########
#view its class
class(lunch)
#view its dimensions
dim(lunch)
#look at column names
names(lunch)

#Understanding the structure of the data
str(lunch) #preview of data
glimpse(lunch) #similar to str, but better
summary(lunch) #summary of data

#looking at the data
head(lunch) #default: first 6 rows
tail(lunch, n=15) #15 last rows

#visualize the data
hist(lunch$perc_free_red)
plot(lunch$year, lunch$perc_free_red)
######################################

#############TIDY DATA 1################
#gather columns into key-value pairs (wide->long)
  # gather(data, key, value, ...)
  # data : a data frame
  # key: bare name of new key column
  # value: bare name of new value column
  # ...: bare name of col to gather or not, ex: -col

#spread key-value pairs into colums (long->wide)
  # spread(data, key, value)
  # data : a data frame
  # key: bare name of column containing keys
  # value: bare name of col containing values

# Apply the gather() function to bmi, saving the result to bmi_long. 
# This will create two new columns:
#   year, containing as values what are currently column headers
#   bmi_val, the actual BMI values
bmi_long <- gather(bmi, year, bmi_val, -Country)
# Apply spread() to bmi_long
bmi_wide <- spread(bmi_long, year, bmi_val)
######################################


#############TIDY DATA 2################
#Separate Colums
separate(lunch, year, c("a","b"))

#unite colums
unite(lunch, year, a, b, sep="-") #sep bisa -,/,;,.,,.

########################################

#############TYPE_CONVERSIONS################
#types of var
class(lunch$year)


#overview of lubridate package
#tidying Date
library(lubridate)

#experiment basic function of lubridate
#ymd
ymd("2018-11-30") #y-m-d
ymd("2018 November 30")
#mdy
mdy("November 30 2018")
#hms
hms("13:33:23")
#combine
ymd_hms("2018/11/29 00.01.02")
########################################

#############Key functions in stringr################
library(stringr)

#Trim leading and trailing white space
str_trim("     a   ") #no white space
  
#Pad string with zeros
str_pad("24493", width = 7, side = "left", pad="0") #7 digits

#Create character vector of names
friends <- c("Aldo", "Fenty", "Intan")

# Search for string in vector
str_detect(friends, "Intan") # FALSE FALSE TRUE

#Replacing string in vector
str_replace(friends, "Aldo", "Nandar") #"Nandar" "Fenty" "Intan"

#uplowCases
tolower("GAJAH") #gajah
toupper("gaJe") #GAJE
#####################################################

#############Missing and Special values################
#Finding missing values
#create a mini dataset
mv <- data.frame(A = c(1, NA, 8, NA),
                 B = c(3, NA, 88, 23),
                 C = c(2, 45, 3, 1))
mv #4*3

#Check for NAs
is.na(mv) #buat db kecil aja

#Are there any NAs?
any(is.na(mv))

#how many NAs?
sum(is.na(mv)) #ada 3 mv

#summary
summary(mv) #hitung manual

#Dealing with mv
#find rows with no mv
complete.cases(mv)

#subset data, keeping only complete cases
mv[complete.cases(mv),]

#another way to remove rows with NAs
na.omit(mv)
#####################################################

#############outliers n obvious error################
#Simulate some data
set.seed(10)
ages <- c(rnorm(30, mean = 15, sd = 5), -1, 68, 90)
ages
#view a boxplot
boxplot(ages, horizontal = TRUE)

#create another small dataset
mv2 <- data.frame(A = rnorm(100,50,10),
                  B = c(rnorm(99,50,10), 100),
                  C = c(rnorm(99,50,10), -1))
summary(mv2)
#check outliers
boxplot(mv2, horizontal = TRUE)

#####################################################

#############CHALLENGE################
#Steps
 # Understanding the structure of the raw data
 #    class(), dim(), names(), str(), glimpse(), summary()
 # Look at the data
 #    head(), tail(), print()
 # Visualizing the data
 #    hist(), plot()


# Understanding the structure of the raw data
class(weather)
dim(weather)
names(weather)
glimpse(weather)
summary(weather)

# Look at the data
head(weather)
head(weather, n=15)
tail(weather)
tail(weather, n=10)

#[MAIN] TIDY THE DATA
###
# Gather the columns
?gather
#weather2 <- gather(weather, day, value, ___, na.rm = TRUE)
weather2 <- gather(weather, day, value, X1:X31, na.rm = TRUE)

# View the head
head(weather2)
###
###
# First remove column of row names
without_x <- weather2[, -1]
head(without_x)
# Spread the data
?spread
#weather3 <- spread(___, ___, ___)
weather3 <- spread(without_x,measure,value)
# View the head
head(weather3)
summary(weather3)
###
###
#Prepare the data for analysis

# Remove X's from day column
#weather3$day <- 
weather3$day <- str_replace(weather3$day,"X","")

# Unite the year, month, and day columns
#weather4 <- unite(weather3, date, ___, ___, ___, sep = "-")
weather4 <- unite(weather3, date, year, month, day, sep="-")

# Convert date column to proper date format using lubridates's ymd()
#weather4$date <- 
weather4$date <- ymd(weather4$date)

# Rearrange columns using dplyr's select()
#weather5 <- select(weather4, date, Events, CloudCover:WindDirDegrees)
weather5 <- select(weather4, date, Events, CloudCover:WindDirDegrees)

# View the head of weather5
head(weather5)

# View the structure of weather5
glimpse(weather5)

# Examine the first 20 rows of weather5. Are most of the characters numeric?
head(weather5,20)

# See what happens if we try to convert PrecipitationIn to numeric
as.numeric(weather5$PrecipitationIn)
  # notice the warning message. 
  # Go back to the results of the head command if need be. 
  # What values in PrecipitationIn would become NA if coerced to numbers? 
  # Why would they be in the dataset to begin with?

# Replace "T" with "0" (T = trace)
#weather5$PrecipitationIn <- 
weather5$PrecipitationIn <- str_replace(weather5$PrecipitationIn, "T", "0")  

# Convert characters to numerics
# weather6 <- mutate_at(weather5, vars(CloudCover:WindDirDegrees), funs(as.numeric))
weather6 <- mutate_at(weather5, vars(CloudCover:WindDirDegrees), funs(as.numeric))

# Look at result
str(weather6)

###
###
#Missing, extreme, and unexpected values
# Count missing values
sum(is.na(weather6))

# Find missing values
summary(weather6)

# Find indices (row numbers) of NAs in Max.Gust.SpeedMPH
# ind <- 
?which #row mana yang ada na ->which(is.na())
ind <- which(is.na(weather6$Max.Gust.SpeedMPH))

# Look at the full rows for records missing Max.Gust.SpeedMPH
# weather6[___, ]
weather6[ind, ]

# Review distributions for all variables
summary(weather6)

# Find row with Max.Humidity of 1000
# ind <-
ind <- which(weather6$Max.Humidity == 1000)

# Look at the data for that day
# weather6[___, ]
weather6[ind, ]

# Change 1000 to 100
# weather6$Max.Humidity[ind] <-
weather6$Max.Humidity[ind] <- 100

# Look at summary of Mean.VisibilityMiles
summary(weather6$Mean.VisibilityMiles)

# Get index of row with -1 value
#ind <-
ind <- which(weather6$Mean.VisibilityMiles == -1)

# Look at full row
weather6[ind, ]

# Set Mean.VisibilityMiles to the appropriate value
weather6$Mean.VisibilityMiles[ind] <- 10
summary(weather6$Mean.VisibilityMiles)

# Review summary of full data once more
summary(weather6)

# Look at histogram for MeanDew.PointF
hist(weather6$MeanDew.PointF)

# Look at histogram for Min.TemperatureF
hist(weather6$Min.TemperatureF)

# Compare to histogram for Mean.TemperatureF
hist(weather6$Mean.TemperatureF)

names(weather6)
# Replace empty cells in events column
weather6$Events[weather6$Events == ""] <- "None"

# Print the first 6 rows of weather6
head(weather6,6)
###
######################################
