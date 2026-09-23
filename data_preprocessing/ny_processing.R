###############################################################################################################
# This is basic preprocessing of NY data to do the following:
#   * Reformat date to be a "date" data type
#   * Add in day of week
#   * Create race variables
#   * Grab age
#
# I have renamed variables to follow the naming conventions of my other scripts [consistent with CT names]
###############################################################################################################

# CHANGE PATH AS NEEDED -- if I don't set working directory it gives me path issues
setwd("C:\\Users\\ar2529\\Desktop\\overdose_age\\data_preprocessing")

library(stringr)
library(dplyr)
library(lubridate)
library(tidyverse)

# CHANGE PATH/READ FUNCTION AS NEEDED -- import data
ny <- read.csv("ny_2018_2023.csv")

# create DOD variable from D36A_MM, D36A_DD, D36A formatted as "MM/DD/YYYY"
names(ny)[names(ny) == 'D36A_DD'] <- 'Day'
names(ny)[names(ny) == 'D36A_MM'] <- 'Month'
names(ny)[names(ny) == 'D36A'] <- 'Year'

ny$DOD <- paste(ny$Month, ny$Day, ny$Year, sep="/")
# Other states have some edge cases with impossible date/month numbers, if this is the case use something like below
# ny$DOD <- ifelse(ny$Day =="99", "Unknown", paste(ny$Month, ny$Day, ny$Year, sep="/"))

# convert to date type -- if some are NA that's fine
ny$DOD = as.Date(mdy(ny$DOD), "%m/%d/%y")

# add in day of week and convert NAs to 0
ny$Weekday <- weekdays(ny$DOD)
ny$Weekday[is.na(ny$Weekday)] <- "Unknown"

# Reformat race variable -- please note that I can only use these 4 race/ethnicity categories because that's all CT has 
# and we need to facilitate a comparison
ny$Race <- ifelse(ny$D23A2 == 1, "Black", ifelse(ny$D22A1 == 1, "Hispanic", ifelse(ny$D23A1 == 1, "White", "Other")))

# In case there are NAs, we'll switch those to other
ny$Race[is.na(ny$Race)] <- "Other"

# Make the sex variable more descriptive
ny$Sex_revised <- ifelse(ny$D16A == 1, "Male", ifelse(ny$D16A == 2, "Female", "Unknown"))

# concatenate the race and sex variables, so "Black" and "female" becomes "Black female"
ny$Race_sex <- paste(ny$Race, ny$Sex_revised, sep = " ")

# Rename age variable and set ages between 18-80, remove NAs as needed
names(ny)[names(ny) == 'Age_years_calc'] <- 'age'
ny <- subset(ny, age > 17 & age < 81)
ny <- ny[!is.na(ny$age),]


# create a new df and only grab the columns from "ny" that are going to be useful for analysis
ny_processed <- select(ny, "Year", "Month", "DOD", "Weekday", "age", "Sex_revised", "Race", "Race_sex") 

# CHANGE PATH AS NEEDED -- export processed dataset
write.csv(ny_processed, file="processed_data/ny_processed.csv")
