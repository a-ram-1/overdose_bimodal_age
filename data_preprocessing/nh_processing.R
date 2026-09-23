####################################################################################
# This is basic preprocessing of nh data to do the following:
#   * Reformat date to be a "date" data type
#   * Add in day of week
#   * Collapse race data
#   * Extract substances of interest from ME notes, make into their own tox fields
####################################################################################

# if I don't set working directory it gives me path issues
setwd("C:\\Users\\ar2529\\Desktop\\overdose_age\\data_preprocessing")

library(stringr)
library(dplyr)
library(lubridate)
library(patchwork)
library(tidyverse) #to replace NA with something else
library(sf)

# import data
nh <- read.csv("nh_2009_2023.csv")

# create DOD variable from DOD, MOD and YOD
# we don't need the current DOD variable so let's just repurpose it
names(nh)[names(nh) == 'DECD_DTH_DT'] <- 'DOD'

# convert to date type -- putting %m/%d/%y causes issues
nh$DOD = as.Date(nh$DOD)

# get the race categories to match CT/MA--they did have a couple different asian categories but there are only 11 for 14 years...not worth making them their own category
nh$Race <- ifelse(nh$DECD_RACE_DSC_1 == "WHITE", "White", ifelse(nh$DECD_RACE_DSC_1 == "BLACK OR AFRICAN AMERICAN", "Black",  ifelse(nh$DECD_HISPANIC_DSC_1 == "YES, OTHER SPANISH/HISPANIC/LATINO", "Hispanic", "Other")))

# make the sex variable more descriptive
nh$Sex_revised <- ifelse(nh$DECD_SEX == "M", "Male", "Female")

# No need to remove records that aren't accidental/undetermined--there's nothing else in this dataset!

# make a series of tox fields -- we'll only focus on the fields that I've used in CT. In addition to being easier to parse the text fields, unlike ICD codes, will tell me exact substances

nh$etoh <- ifelse(grepl("T510", nh$Req_Mult_Cause_1, fixed = TRUE), 1, ifelse(grepl("T510", nh$Req_Mult_Cause_2, fixed = TRUE), 1, 0))
nh$pharma <- ifelse(grepl("T402", nh$Req_Mult_Cause_1, fixed = TRUE), 1, ifelse(grepl("T402", nh$Req_Mult_Cause_2, fixed = TRUE), 1, 0))
nh$synthetic <- ifelse(grepl("T404", nh$Req_Mult_Cause_1, fixed = TRUE), 1, ifelse(grepl("T404", nh$Req_Mult_Cause_2, fixed = TRUE), 1, 0))
#nh$stimulant <- ifelse(grepl("T405", nh$Req_Mult_Cause_1, fixed = TRUE), 1, ifelse(grepl("T405", nh$Req_Mult_Cause_2, fixed = TRUE), 1, 0))
nh$methadone <- ifelse(grepl("T403", nh$Req_Mult_Cause_1, fixed = TRUE), 1, ifelse(grepl("T403", nh$Req_Mult_Cause_2, fixed = TRUE), 1, 0))
nh$stimulant <- ifelse(grepl("T405", nh$Req_Mult_Cause_1, fixed = TRUE), 1, ifelse(grepl("T405", nh$Req_Mult_Cause_2, fixed = TRUE), 1, ifelse(grepl("T436", nh$Req_Mult_Cause_1, fixed = TRUE), 1, ifelse(grepl("T436", nh$Req_Mult_Cause_2, fixed = TRUE), 1, 0))))

# subset for opioid-detected deaths
# this is easiest to do by creating an opioid-detected variable first

nh$opioid_detected <- ifelse(grepl("T400", nh$Req_Mult_Cause_1, fixed = TRUE) | grepl("T401", nh$Req_Mult_Cause_1, fixed = TRUE) | grepl("T402", nh$Req_Mult_Cause_1, fixed = TRUE) | grepl("T403", nh$Req_Mult_Cause_1, fixed = TRUE) | grepl("T404", nh$Req_Mult_Cause_1, fixed = TRUE) | grepl("T406", nh$Req_Mult_Cause_1, fixed = TRUE) | grepl("T400", nh$Req_Mult_Cause_2, fixed = TRUE) | grepl("T401", nh$Req_Mult_Cause_2, fixed = TRUE) | grepl("T402", nh$Req_Mult_Cause_2, fixed = TRUE) | grepl("T403", nh$Req_Mult_Cause_2, fixed = TRUE) | grepl("T404", nh$Req_Mult_Cause_2, fixed = TRUE) | grepl("T406", nh$Req_Mult_Cause_2, fixed = TRUE), 1, 0)
nh <- subset(nh, opioid_detected == 1)

################STOPPED HERE 10/31

# get month, year and weekday
nh$Month <- month(nh$DOD)
nh$Year <- year(nh$DOD)
nh$Weekday <- weekdays(nh$DOD)

names(nh)

# only grab the columns that are going to be useful
nh_processed <- select(nh, "Sex_revised", "DOD", "Decd_Age", "etoh", "pharma", "synthetic", "stimulant", "methadone", "Race", "Month", "Year", "Weekday") 

# rename variables to be similar to those from other states
names(nh_processed)[names(nh_processed) == 'Decd_Age'] <- 'age'

# set ages between 18-80
nh_processed <- subset(nh_processed, age > 17 & age < 81)

# remove NAs from age -- there's just one in 2022 -- from https://stackoverflow.com/questions/11254524/omit-rows-containing-specific-column-of-na
#nh_processed <- nh_processed[!is.na(nh_processed$age),]

# concatenate the race and sex variables, so "Black" and "female" becomes "Black female"
nh_processed$Race_sex <- paste(nh_processed$Race, nh_processed$Sex_revised, sep = " ")

# convert all NA values in weekday to 0
#nh_processed$Weekday[is.na(nh_processed$Weekday)] <- "Unknown"

# indicator variable to aggregate
nh_processed$fatal <- 1

# create variables for the different eras--NOT UPDATED, DON'T USE YET!!!!!!!!!!!

pharma_era <- subset(nh_processed, Year %in% c("2009", "2010", "2011", "2012", "2013"))
pharma_era$Era <- rep("Pharma", times = nrow(pharma_era))

heroin_era <- subset(nh_processed, Year %in% c("2014", "2015", "2016", "2017"))
heroin_era$Era <- rep("Heroin", times = nrow(heroin_era))

fent_era <- subset(nh_processed, Year %in% c("2018", "2019", "2020"))
fent_era$Era <- rep("Fentanyl", times = nrow(fent_era))

polysubstance_era <- subset(nh_processed, Year %in% c("2021", "2022", "2023"))
polysubstance_era$Era <- rep("Polysubstance", times = nrow(polysubstance_era))

# create a dataset with all eras
all_eras <- rbind(pharma_era, heroin_era, fent_era, polysubstance_era)

# export files
write.csv(nh_processed, file="processed_data/nh_processed.csv")

#write.csv(pharma_era, file="processed_data/nh_pharma_era_processed.csv")
#write.csv(heroin_era, file="processed_data/nh_heroin_era_processed.csv")
#write.csv(fent_era, file="processed_data/nh_fent_era_processed.csv")
#write.csv(polysubstance_era, file="processed_data/nh_polysubstance_era_processed.csv")
#write.csv(all_eras, file="processed_data/nh_all_eras_processed.csv")
