#Data Cleaning
references: https://www.datacamp.com/community/blog/an-introduction-to-cleaning-data-in-r

Tidy Data: It is often said that data scientists 
spend only 20% of their time analyzing their data, and 80% of time cleaning it. 
Indeed, maintaining a tidy, easy-to-use dataset is crucial in our age of big data. 
In the paper Tidy Data, veteran statistician Hadley Wickham 
gives definitions of tidy and messy data so that all data scientists 
can keep their work organized. 
In this mini-course, you'll learn to transform messy datasets into 
tidy datasets using the pandas package in python. Let's get started!

Exploring Raw Data
 1. Understand the structure of the data
 2. Look at the data
 3. VIsualize the data

Principles of Tidy Data
 1. Observations as rows
 2. Variables as colums
 3. One type of observational unit per table

Dirty/Messy data diagnosis
 - either Wide (<->) or long(^)
     bisa aja lebih banyak nama columns dari rows
 - variables stored in both rows and columns
 - A single observational unit is stored in multiple tables
 - multiple types of obs units are stored in the same table

Type of variables in R
 - character : "a"
 - numeric : 1
 - integer : 1L
 - factor : factor()
 - logical : TRUE
 - function : factor
 Checking type of var -> class()
 Coerce/change var -> as.typeOfVar()
 
Date
 - ymd
 - mdy
 - ymd_hm
 - ymd_hms
 - mdy_hm
 - mdy_hms
 
 Key functions in stringr
 #Trim leading and trailing white space
   str_strim("     a   ")
   > a #no white space
 #Pad string with zeros
   str_pad("24493", width = 7, side = "left", pad="0")
   > "0024493"  #7 digits
 #Create character vector of names
   friends <- c("Aldo", "Fenty", "Intan")
 # Search for string in vector
   str_detect(friends, "Intan")
   > FALSE FALSE TRUE
 #Replacing string in vector
   str_replace(friends, "Aldo", "Nandar")
   > "Nandar" "Fenty" "Intan"
 #uplowCases
   tolower("GAJAH")
   toupper("gaJe")
 #lain2
   str_detect()
   str_squish
   ;etc.

Missing Values
 May be random, but dangerous to assume
 Sometimes associated with variable/outcome of interest
 Di R, represented as NA
 
Special Values
 Inf - "infinite value"
 NaN - "not a number"

 Challenge
  
   
   
 
 