####################################################################################
# This is basic preprocessing of OCME data to do the following:
#   * Reformat date to be a "date" data type
#   * Add in day of week
#   * Remove non-CT ODs
#   * Extract xylazine and gabapentin from ME notes, make into their own tox fields
####################################################################################

# set working directory -- if you don't do this the relative paths won't work
setwd("C:\\Users\\ar2529\\Desktop\\overdose_age\\data_preprocessing")

library(stringr)
library(dplyr)
library(lubridate)
library(patchwork)
library(tidyverse) #to replace NA with something else
library(sf)


# import OCME data and convert the date into a "date" data type so we can work with it
# this is 2009-2019 data, but I'm only going to use 2009-2017
ocme_2009_2019 <- read.csv("ocme_2009_2019.csv")
# get rid of 2018 and 2019
ocme_old <- subset(ocme_2009_2019, !(year %in% c(2018, 2019)))
# rename date of death variable to DOD
names(ocme_old)[names(ocme_old) == 'dod'] <- 'DOD'
# make DOD variable into a "date" data type
ocme_old$DOD = as.Date(ocme_old$DOD, "%m/%d/%Y")
# the 2014 ages aren't coded properly -- fix
# from https://stackoverflow.com/questions/15569333/get-date-difference-in-years-floating-point
ocme_old$age <- ifelse(ocme_old$year == 2014, floor(time_length(difftime(ocme_old$DOD, as.Date(ocme_old$dob, "%m/%d/%Y")),"years")), ocme_old$age)
# set ages between 18-80
ocme_old <- subset(ocme_old, age > 17 & age < 81)
# rename "year" to "Year"
names(ocme_old)[names(ocme_old) == 'year'] <- 'Year'


# add in day of week -- from https://stackoverflow.com/questions/9216138/find-the-day-of-a-week
ocme_old$Weekday <- weekdays(ocme_old$DOD)
# remove anyone with a non-ct residence
ocme_old <- subset(ocme_old, injury_state_updated == "CT")
# convert all NA values to 0
ocme_old[is.na(ocme_old)] <- 0
# ID xylazine/gabapentin in tox -- only look at words before the first semicolon because that's what's in the tox
# extract everything before a character: https://www.statology.org/r-extract-string-before-space/
# check if a word is in a string: https://www.statology.org/r-check-if-column-contains-string/
# i don't know a better way to do a case-insensitive search
ocme_old$gabapentin <- ifelse(str_detect(gsub( ";.*$", "", ocme_old$my_notes), 'Gabapentin')==TRUE, 1, ifelse(str_detect(gsub( ";.*$", "", ocme_old$my_notes), 'gabapentin')==TRUE, 1, ifelse(str_detect(gsub( ";.*$", "", ocme_old$my_notes), 'Pregabalin')==TRUE, 1, ifelse(str_detect(gsub( ";.*$", "", ocme_old$my_notes), 'pregabalin')==TRUE, 1,0))))
#ocme_old$gabapentin <- ifelse(str_detect(gsub( ";.*$", "", ocme_old$my_notes), 'Gabapentin')==TRUE, 1, ifelse(str_detect(gsub( ";.*$", "", ocme_old$my_notes), 'gabapentin')==TRUE, 1, 0))
ocme_old$xylazine <- ifelse(str_detect(gsub( ";.*$", "", ocme_old$my_notes), 'Xylazine')==TRUE, 1, ifelse(str_detect(gsub( ";.*$", "", ocme_old$my_notes), 'xylazine')==TRUE, 1, 0))
ocme_old$weed <- ifelse(str_detect(gsub( ";.*$", "", ocme_old$my_notes), 'OH-THC')==TRUE, 1, ifelse(str_detect(gsub( ";.*$", "", ocme_old$my_notes), 'oh-thc')==TRUE, 1, 0))
# create a stimulant variable with cocaine + amphetamines
ocme_old$stimulant <- ifelse(ocme_old$cocaine ==1, 1, ifelse(ocme_old$amphetamine == 1, 1, 0))

# this will be used for town calculations -- a better injury city indicator, renames blanks and gets rid of the Willimantic/Windham confusion
ocme_old$injury_city_revised <- ifelse(ocme_old$injury_city_updated == "", "Unknown", ifelse(ocme_old$injury_city_updated == "Willimantic", "Windham", ocme_old$injury_city_updated))
# collapse injury location
ocme_old$injury_location <- ifelse(ocme_old$injury_location_revised %in% c("unclear", "unknown", "missing"), "unknown",ifelse(ocme_old$injury_location_revised == "other residence", "residence", ifelse(ocme_old$injury_location_revised == "other, outdoors", "outdoors", ifelse(ocme_old$injury_location_revised == "other, outdoors", "outdoors", ocme_old$injury_location_revised))))
# make race_numeric into words
ocme_old$race_numeric_words <- ifelse(ocme_old$race_numeric == "1", "White", ifelse(ocme_old$race_numeric == "2", "Black", ifelse(ocme_old$race_numeric == "3", "Hispanic", ifelse(ocme_old$race_numeric == "4", "Other", ifelse(ocme_old$race_numeric == "7", "Other", "Other")))))
#concatenate the race and sex variables, so "Black" and "female" becomes "Black female"
ocme_old$race_sex <- paste(ocme_old$race_numeric_words, ocme_old$sex, sep = " ")
# rename "sex" to "Sex_revised"
names(ocme_old)[names(ocme_old) == 'sex'] <- 'Sex_revised'
# indicator variable to aggregate
ocme_old$fatal <- 1


# import OCME data and convert the date into a "date" data type so we can work with it
ocme_2018 <- read.csv("ocme_2018.csv")
ocme_2018$DOD = as.Date(ocme_2018$DOD, "%m/%d/%Y")
# add in day of week -- from https://stackoverflow.com/questions/9216138/find-the-day-of-a-week
ocme_2018$Weekday <- weekdays(ocme_2018$DOD)
# remove anyone with a non-ct residence
ocme_2018 <- subset(ocme_2018, injury_state_updated == "CT")
# rename "year" to "Year"
names(ocme_2018)[names(ocme_2018) == 'year'] <- 'Year'
# convert all NA values to 0
ocme_2018[is.na(ocme_2018)] <- 0
# ID xylazine/gabapentin in tox -- only look at words before the first semicolon because that's what's in the tox
# extract everything before a character: https://www.statology.org/r-extract-string-before-space/
# check if a word is in a string: https://www.statology.org/r-check-if-column-contains-string/
# i don't know a better way to do a case-insensitive search
ocme_2018$gabapentin <- ifelse(str_detect(gsub( ";.*$", "", ocme_2018$MY.NOTES), 'Gabapentin')==TRUE, 1, ifelse(str_detect(gsub( ";.*$", "", ocme_2018$MY.NOTES), 'gabapentin')==TRUE, 1, ifelse(str_detect(gsub( ";.*$", "", ocme_2018$MY.NOTES), 'Pregabalin')==TRUE, 1, ifelse(str_detect(gsub( ";.*$", "", ocme_2018$MY.NOTES), 'pregabalin')==TRUE, 1,0))))
#ocme_2018$gabapentin <- ifelse(str_detect(gsub( ";.*$", "", ocme_2018$MY.NOTES), 'Gabapentin')==TRUE, 1, ifelse(str_detect(gsub( ";.*$", "", ocme_2018$MY.NOTES), 'gabapentin')==TRUE, 1, 0))
ocme_2018$xylazine <- ifelse(str_detect(gsub( ";.*$", "", ocme_2018$MY.NOTES), 'Xylazine')==TRUE, 1, ifelse(str_detect(gsub( ";.*$", "", ocme_2018$MY.NOTES), 'xylazine')==TRUE, 1, 0))
ocme_2018$weed <- ifelse(str_detect(gsub( ";.*$", "", ocme_2018$MY.NOTES), 'OH-THC')==TRUE, 1, ifelse(str_detect(gsub( ";.*$", "", ocme_2018$MY.NOTES), 'oh-thc')==TRUE, 1, 0))
# create a stimulant variable with cocaine + amphetamines
ocme_2018$stimulant <- ifelse(ocme_2018$cocaine ==1, 1, ifelse(ocme_2018$amphetamine == 1, 1, 0))

# this will be used for town calculations -- a better injury city indicator, renames blanks and gets rid of the Willimantic/Windham confusion
ocme_2018$injury_city_revised <- ifelse(ocme_2018$injury_city_updated == "", "Unknown", ifelse(ocme_2018$injury_city_updated == "Willimantic", "Windham", ocme_2018$injury_city_updated))
# collapse injury location
ocme_2018$injury_location <- ifelse(ocme_2018$injury.location_revised %in% c("", "Unknown", "unclear"), "unknown",ifelse(ocme_2018$injury.location_revised == "other,indoors", "other, indoors", ifelse(ocme_2018$injury.location_revised %in% c("Other, Residential Institution", "other residence"), "residence", ifelse(ocme_2018$injury.location_revised == "other, outdoors", "outdoors", ocme_2018$injury.location_revised))))
# make race_numeric into words
ocme_2018$race_numeric_words <- ifelse(ocme_2018$race_.numeric == "1", "White", ifelse(ocme_2018$race_.numeric == "2", "Black", ifelse(ocme_2018$race_.numeric == "3", "Hispanic", ifelse(ocme_2018$race_.numeric == "4", "Other", ifelse(ocme_2018$race_.numeric == "7", "Other", "Other")))))
#concatenate the race and sex variables, so "Black" and "female" becomes "Black female"
ocme_2018$race_sex <- paste(ocme_2018$race_numeric_words, ocme_2018$sex, sep = " ")
# rename "Sex" to "Sex_revised"
names(ocme_2018)[names(ocme_2018) == 'sex'] <- 'Sex_revised'
# indicator variable to aggregate
ocme_2018$fatal <- 1
# set ages between 18-80
ocme_2018 <- subset(ocme_2018, age > 17 & age < 81)

ocme_2019 <- read.csv("ocme_2019.csv")
ocme_2019$DOD = as.Date(ocme_2019$DOD, "%m/%d/%Y")
#add in day of week
ocme_2019$Weekday <- weekdays(ocme_2019$DOD)
# remove anyone with a non-ct residence
ocme_2019 <- subset(ocme_2019, Injury.State == "CT")
# convert all NA values to 0
ocme_2019[is.na(ocme_2019)] <- 0
# ID xylazine/gabapentin in tox -- only look at words before the first semicolon because that's what's in the tox
# extract everything before a character: https://www.statology.org/r-extract-string-before-space/
# check if a word is in a string: https://www.statology.org/r-check-if-column-contains-string/
ocme_2019$gabapentin <- ifelse(str_detect(gsub( ";.*$", "", ocme_2019$MY.NOTES), 'Gabapentin')==TRUE, 1, ifelse(str_detect(gsub( ";.*$", "", ocme_2019$MY.NOTES), 'gabapentin')==TRUE, 1, ifelse(str_detect(gsub( ";.*$", "", ocme_2019$MY.NOTES), 'Pregabalin')==TRUE, 1, ifelse(str_detect(gsub( ";.*$", "", ocme_2019$MY.NOTES), 'pregabalin')==TRUE, 1, 0))))
#ocme_2019$gabapentin <- ifelse(str_detect(gsub( ";.*$", "", ocme_2019$MY.NOTES), 'Gabapentin')==TRUE, 1, ifelse(str_detect(gsub( ";.*$", "", ocme_2019$MY.NOTES), 'gabapentin')==TRUE, 1, 0))
ocme_2019$xylazine <- ifelse(str_detect(gsub( ";.*$", "", ocme_2019$MY.NOTES), 'Xylazine')==TRUE, 1, ifelse(str_detect(gsub( ";.*$", "", ocme_2019$MY.NOTES), 'xylazine')==TRUE, 1, 0))
ocme_2019$weed <- ifelse(str_detect(gsub( ";.*$", "", ocme_2019$MY.NOTES), 'OH-THC')==TRUE, 1, ifelse(str_detect(gsub( ";.*$", "", ocme_2019$MY.NOTES), 'oh-thc')==TRUE, 1, 0))
# create a stimulant variable with cocaine + amphetamines
ocme_2019$stimulant <- ifelse(ocme_2019$cocaine ==1, 1, ifelse(ocme_2019$amphetamine == 1, 1, 0))

# this will be used for town calculations -- a better injury city indicator, renames blanks and gets rid of the Willimantic/Windham confusion
ocme_2019$injury_city_revised <- ifelse(ocme_2019$Injury.City == "", "Unknown", ifelse(ocme_2019$Injury.City == "Willimantic", "Windham", ocme_2019$Injury.City))
# collapse injury location 
ocme_2019$injury_collapsed <- ifelse(ocme_2019$injury.locn_revised %in% c("other residence", "residence?"), "residence", ifelse(ocme_2019$injury.locn_revised == "other, outdoors", "outdoors", ifelse(ocme_2019$injury.locn_revised=="store", "store/shopping area", ifelse(ocme_2019$injury.locn_revised == "work site", "workplace", ifelse(ocme_2019$injury.locn_revised %in% c("", "unclear", "unspecified", "missing", "unidentified"), "unknown", ifelse(ocme_2019$injury.locn_revised == "other, indoors", "other indoors", ocme_2019$injury.locn_revised))))))
# make race_numeric into words
ocme_2019$race_numeric_words <- ifelse(ocme_2019$race_numeric...1.White..........2.Black....3.Hispanic...4.Other....7.UNK == "1", "White", ifelse(ocme_2019$race_numeric...1.White..........2.Black....3.Hispanic...4.Other....7.UNK == "2", "Black", ifelse(ocme_2019$race_numeric...1.White..........2.Black....3.Hispanic...4.Other....7.UNK == "3", "Hispanic", ifelse(ocme_2019$race_numeric...1.White..........2.Black....3.Hispanic...4.Other....7.UNK == "4", "Other", ifelse(ocme_2019$race_numeric...1.White..........2.Black....3.Hispanic...4.Other....7.UNK == "7", "Other", "Other")))))
# remove blanks in sex
ocme_2019$Sex_revised <- ifelse(ocme_2019$sex == "", "Unknown", ocme_2019$sex)
#concatenate the race and sex variables, so "Black" and "female" becomes "Black female"
ocme_2019$race_sex <- paste(ocme_2019$race_numeric_words, ocme_2019$Sex_revised, sep = " ")
# indicator variable to aggregate
ocme_2019$fatal <- 1
# set ages between 18-80
ocme_2019 <- subset(ocme_2019, age > 17 & age < 81)

ocme_2020 <- read.csv("ocme_2020.csv")
ocme_2020$DOD = as.Date(ocme_2020$DOD, "%m/%d/%Y")
#add in day of week
ocme_2020$Weekday <- weekdays(ocme_2020$DOD)
# remove anyone with a non-ct residence
ocme_2020 <- subset(ocme_2020, Injury.State == "CT")
# convert all NA values to 0
ocme_2020[is.na(ocme_2020)] <- 0
# ID xylazine/gabapentin in tox -- only look at words before the first semicolon because that's what's in the tox
# extract everything before a character: https://www.statology.org/r-extract-string-before-space/
# check if a word is in a string: https://www.statology.org/r-check-if-column-contains-string/
# Lauretta said to include pregabalin with gabapentin, so I'm adding it in
ocme_2020$gabapentin <- ifelse(str_detect(gsub( ";.*$", "", ocme_2020$MY.NOTES), 'Gabapentin')==TRUE, 1, ifelse(str_detect(gsub( ";.*$", "", ocme_2020$MY.NOTES), 'gabapentin')==TRUE, 1, ifelse(str_detect(gsub( ";.*$", "", ocme_2020$MY.NOTES), 'Pregabalin')==TRUE, 1, ifelse(str_detect(gsub( ";.*$", "", ocme_2020$MY.NOTES), 'pregabalin')==TRUE, 1, 0))))
#ocme_2020$gabapentin <- ifelse(str_detect(gsub( ";.*$", "", ocme_2020$MY.NOTES), 'Gabapentin')==TRUE, 1, ifelse(str_detect(gsub( ";.*$", "", ocme_2020$MY.NOTES), 'gabapentin')==TRUE, 1, 0))
ocme_2020$xylazine <- ifelse(str_detect(gsub( ";.*$", "", ocme_2020$MY.NOTES), 'Xylazine')==TRUE, 1, ifelse(str_detect(gsub( ";.*$", "", ocme_2020$MY.NOTES), 'xylazine')==TRUE, 1, 0))
ocme_2020$weed <- ifelse(str_detect(gsub( ";.*$", "", ocme_2020$MY.NOTES), 'OH-THC')==TRUE, 1, ifelse(str_detect(gsub( ";.*$", "", ocme_2020$MY.NOTES), 'oh-thc')==TRUE, 1, 0))
# create a stimulant variable with cocaine + amphetamines
ocme_2020$stimulant <- ifelse(ocme_2020$cocaine ==1, 1, ifelse(ocme_2020$amphetamine == 1, 1, 0))

#this will be used for town calculations -- a better injury city indicator, renames blanks and gets rid of the Willimantic/Windham confusion
ocme_2020$injury_city_revised <- ifelse(ocme_2020$Injury.City == "", "Unknown", ifelse(ocme_2020$Injury.City == "Willimantic", "Windham", ocme_2020$Injury.City))
# collapse injury location 
ocme_2020$injury_location <- ifelse(ocme_2020$injury.locn_revised == " residence ", "residence", ocme_2020$injury.locn_revised)
ocme_2020$injury_collapsed <- ifelse(ocme_2020$injury_location %in% c("apartment complex", "former residence", "other residence", "residence or drug spot"), "residence", ifelse(ocme_2020$injury_location %in% c("hospital ER", "hospital "), "hospital", ifelse(ocme_2020$injury_location == "other, indoors", "other indoors", ifelse(ocme_2020$injury_location == "unspecified", "unknown", ocme_2020$injury_location))))
# make race_numeric into words
ocme_2020$race_numeric_words <- ifelse(ocme_2020$race_numeric...1.White..........2.Black....3.Hispanic...4.Other....7.UNK == "1", "White", ifelse(ocme_2020$race_numeric...1.White..........2.Black....3.Hispanic...4.Other....7.UNK == "2", "Black", ifelse(ocme_2020$race_numeric...1.White..........2.Black....3.Hispanic...4.Other....7.UNK == "3", "Hispanic", ifelse(ocme_2020$race_numeric...1.White..........2.Black....3.Hispanic...4.Other....7.UNK == "4", "Other", ifelse(ocme_2020$race_numeric...1.White..........2.Black....3.Hispanic...4.Other....7.UNK == "7", "Other", "Other")))))
# There was one person whose race was just listed as blank, so I making it "Unknown"
ocme_2020$Sex_revised <- ifelse(ocme_2020$Sex == "", "Unknown", ifelse(ocme_2020$Sex == " Male ", "Male", ocme_2020$Sex))
# There was one person whose race was just listed as blank, so I making it "Unknown"
ocme_2020$race_sex <- paste(ocme_2020$race_numeric_words, ocme_2020$Sex_revised, sep = " ")
# indicator variable to aggregate
ocme_2020$fatal <- 1
# rename age variable to "age"
names(ocme_2020)[names(ocme_2020) == 'Age'] <- 'age'
# set ages between 18-80
ocme_2020 <- subset(ocme_2020, age > 17 & age < 81)

ocme_2021 <- read.csv("ocme_2021.csv")
ocme_2021$DateofDeath = as.Date(ocme_2021$DateofDeath, "%m/%d/%Y")
#add in day of week
ocme_2021$Weekday <- weekdays(ocme_2021$DateofDeath)
# remove anyone with a non-ct residence
ocme_2021 <- subset(ocme_2021, Injury.State == "CT")
# convert all NA values to 0
ocme_2021[is.na(ocme_2021)] <- 0
# ID xylazine/gabapentin in tox -- only look at words before the first semicolon because that's what's in the tox
# extract everything before a character: https://www.statology.org/r-extract-string-before-space/
# check if a word is in a string: https://www.statology.org/r-check-if-column-contains-string/
# I have to do MY.NOTES.1 because there is apparently a duplicate my notes column [the first one only contains a few entries]
ocme_2021$gabapentin <- ifelse(str_detect(gsub( ";.*$", "", ocme_2021$MY.NOTES.1), 'Gabapentin')==TRUE, 1, ifelse(str_detect(gsub( ";.*$", "", ocme_2021$MY.NOTES.1), 'gabapentin')==TRUE, 1, ifelse(str_detect(gsub( ";.*$", "", ocme_2021$MY.NOTES.1), 'Pregabalin')==TRUE, 1, ifelse(str_detect(gsub( ";.*$", "", ocme_2021$MY.NOTES.1), 'pregabalin')==TRUE, 1, 0))))
#ocme_2021$gabapentin <- ifelse(str_detect(gsub( ";.*$", "", ocme_2021$MY.NOTES.1), 'Gabapentin')==TRUE, 1, ifelse(str_detect(gsub( ";.*$", "", ocme_2021$MY.NOTES.1), 'gabapentin')==TRUE, 1, 0))
ocme_2021$xylazine <- ifelse(str_detect(gsub( ";.*$", "", ocme_2021$MY.NOTES.1), 'Xylazine')==TRUE, 1, ifelse(str_detect(gsub( ";.*$", "", ocme_2021$MY.NOTES.1), 'xylazine')==TRUE, 1, 0))
ocme_2021$weed <- ifelse(str_detect(gsub( ";.*$", "", ocme_2021$MY.NOTES.1), 'OH-THC')==TRUE, 1, ifelse(str_detect(gsub( ";.*$", "", ocme_2021$MY.NOTES.1), 'oh-thc')==TRUE, 1, 0))
# create a stimulant variable with cocaine + amphetamines
ocme_2021$stimulant <- ifelse(ocme_2021$cocaine ==1, 1, ifelse(ocme_2021$amphetamine..including.eutylone. == 1, 1, 0))

#this will be used for town calculations -- a better injury city indicator, renames blanks and gets rid of the Willimantic/Windham confusion
ocme_2021$injury_city_revised <- ifelse(ocme_2021$Injury.City == "", "Unknown", ifelse(ocme_2021$Injury.City == "Willimantic", "Windham", ocme_2021$Injury.City))
# collapse injury location
ocme_2021$injury_location <- ifelse(ocme_2021$injury.locn_revised %in% c("residence ", "resdience"), "residence", ifelse(ocme_2021$injury.locn_revised == "other,indoors", "other, indoors", ifelse(ocme_2021$injury.locn_revised == "outdoors ", "outdoors", ifelse(ocme_2021$injury.locn_revised == "store/shoppping area", "store/shopping area", ifelse(ocme_2021$injury.locn_revised == "vehicle ", "vehicle", ocme_2021$injury.locn_revised)))))
ocme_2021$injury_collapsed <- ifelse(ocme_2021$injury_location %in% c("residence for p4m)", "house", "unclear residence", "other residence"), "residence", ifelse(ocme_2021$injury_location == "DOC transitional site", "halfway house", ifelse(ocme_2021$injury_location %in% c("outdoors?", "street"), "outdoors", ifelse(ocme_2021$injury_location == "vehicle", "vehicle/parking lot", ifelse(ocme_2021$injury_location %in% c("unclear", "unspecified"), "unknown", ifelse(ocme_2021$injury_location %in% c("indoors", "elementary school", "truck cabin"), "other, indoors", ocme_2021$injury_location))))))
# make race_numeric into words
ocme_2021$race_numeric_words <- ifelse(ocme_2021$race_numeric...1.White..........2.Black....3.Hispanic...4.Other....7.UNK == "1", "White", ifelse(ocme_2021$race_numeric...1.White..........2.Black....3.Hispanic...4.Other....7.UNK == "2", "Black", ifelse(ocme_2021$race_numeric...1.White..........2.Black....3.Hispanic...4.Other....7.UNK == "3", "Hispanic", ifelse(ocme_2021$race_numeric...1.White..........2.Black....3.Hispanic...4.Other....7.UNK == "4", "Other", ifelse(ocme_2021$race_numeric...1.White..........2.Black....3.Hispanic...4.Other....7.UNK == "7", "Other", "Other")))))
# There was one person whose race was just listed as blank, so I making it "Unknown"
ocme_2021$Sex_revised <- ifelse(ocme_2021$Sex == "", "Unknown", ocme_2021$Sex)
# create a race sex variable
ocme_2021$race_sex <- paste(ocme_2021$race_numeric_words, ocme_2021$Sex_revised, sep = " ")
# indicator variable to aggregate
ocme_2021$fatal <- 1
# rename age variable to "age"
names(ocme_2021)[names(ocme_2021) == 'Age'] <- 'age'
# set ages between 18-80
ocme_2021 <- subset(ocme_2021, age > 17 & age < 81)

ocme_2022 <- read.csv("ocme_2022.csv")
ocme_2022$Date.of.Death = as.Date(ocme_2022$Date.of.Death, "%m/%d/%Y")
#add in day of week
ocme_2022$Weekday <- weekdays(ocme_2022$Date.of.Death)
# remove anyone with a non-ct residence
ocme_2022 <- subset(ocme_2022, Injury.State == "CT")
# convert all NA values to 0
ocme_2022[is.na(ocme_2022)] <- 0
# ID xylazine/gabapentin in tox -- only look at words before the first semicolon because that's what's in the tox
# extract everything before a character: https://www.statology.org/r-extract-string-before-space/
# check if a word is in a string: https://www.statology.org/r-check-if-column-contains-string/
# I have to do MY.NOTES.1 because there is apparently a duplicate my notes column [the first one only contains a few entries]
ocme_2022$gabapentin <- ifelse(str_detect(gsub( ";.*$", "", ocme_2022$MY.NOTES), 'Gabapentin')==TRUE, 1, ifelse(str_detect(gsub( ";.*$", "", ocme_2022$MY.NOTES), 'gabapentin')==TRUE, 1, ifelse(str_detect(gsub( ";.*$", "", ocme_2022$MY.NOTES), 'Pregbalin')==TRUE, 1, ifelse(str_detect(gsub( ";.*$", "", ocme_2022$MY.NOTES), 'pregabalin')==TRUE, 1,0))))
#ocme_2022$gabapentin <- ifelse(str_detect(gsub( ";.*$", "", ocme_2022$MY.NOTES), 'Gabapentin')==TRUE, 1, ifelse(str_detect(gsub( ";.*$", "", ocme_2022$MY.NOTES), 'gabapentin')==TRUE, 1, 0))
ocme_2022$xylazine <- ifelse(str_detect(gsub( ";.*$", "", ocme_2022$MY.NOTES), 'Xylazine')==TRUE, 1, ifelse(str_detect(gsub( ";.*$", "", ocme_2022$MY.NOTES), 'xylazine')==TRUE, 1, 0))
ocme_2022$weed <- ifelse(str_detect(gsub( ";.*$", "", ocme_2022$MY.NOTES), 'OH-THC')==TRUE, 1, ifelse(str_detect(gsub( ";.*$", "", ocme_2022$MY.NOTES), 'oh-thc')==TRUE, 1, 0))
# create a stimulant variable with cocaine + amphetamines
ocme_2022$stimulant <- ifelse(ocme_2022$cocaine ==1, 1, ifelse(ocme_2022$amphetamine.other.stimulants..including.eutylone..MDMA..N.N.diMe.pentylone. == 1, 1, 0))

#this will be used for town calculations -- a better injury city indicator, renames blanks and gets rid of the Willimantic/Windham confusion
ocme_2022$injury_city_revised <- ifelse(ocme_2022$Injury.City == "", "Unknown", ifelse(ocme_2022$Injury.City == "Willimantic", "Windham", ocme_2022$Injury.City))
# collapse injury location
ocme_2022$injury_collapsed <- ifelse(ocme_2022$injury.locn_revised %in% c("other residence", "residence?", "residence ", "residential building", "unknown residence", "home"), "residence", ifelse(ocme_2022$injury.locn_revised %in% c("other, indoors", "other, indoors ", "other building"), "other indoors", ifelse(ocme_2022$injury.locn_revised %in% c("boat", "camper IFO address"), "vehicle", ifelse(ocme_2022$injury.locn_revised %in% c("", "unknown", "unspecified", "unspecified place"), "unknown", ifelse(ocme_2022$injury.locn_revised == "workpkace", "workplace", ifelse(ocme_2022$injury.locn_revised =="other ", "other", ifelse(ocme_2022$injury.locn_revised %in% c("outdoors ", "street"), "outdoors", ifelse(ocme_2022$injury.locn_revised == "parking garage", "parking lot", ocme_2022$injury.locn_revised))))))))
# make race_numeric into words
ocme_2022$race_numeric_words <- ifelse(ocme_2022$race_.numeric...1.White..........2.Black....3.Hispanic...4.Other....7.UNK == "1", "White", ifelse(ocme_2022$race_.numeric...1.White..........2.Black....3.Hispanic...4.Other....7.UNK == "2", "Black", ifelse(ocme_2022$race_.numeric...1.White..........2.Black....3.Hispanic...4.Other....7.UNK == "3", "Hispanic", ifelse(ocme_2022$race_.numeric...1.White..........2.Black....3.Hispanic...4.Other....7.UNK == "4", "Other", ifelse(ocme_2022$race_.numeric...1.White..........2.Black....3.Hispanic...4.Other....7.UNK == "7", "Other", "Other")))))
# there's a NA in here and for some reason replacing it using the line above didn't work, so we'll try this
# from https://stackoverflow.com/questions/55910824/replace-na-with-unknown
ocme_2022 <- ocme_2022 %>% replace_na(list(race_numeric_words = "Other"))
# create a race sex variable
ocme_2022$race_sex <- paste(na.omit(ocme_2022$race_numeric_words), ocme_2022$Sex, sep = " ")
# rename "Sex" to "Sex_revised"
names(ocme_2022)[names(ocme_2022) == 'Sex'] <- 'Sex_revised'
# indicator variable to aggregate
ocme_2022$fatal <- 1
# rename age variable to "age"
names(ocme_2022)[names(ocme_2022) == 'Age'] <- 'age'
# set ages between 18-80
ocme_2022 <- subset(ocme_2022, age > 17 & age < 81)

ocme_2023 <- read.csv("ocme_2023.csv")
ocme_2023$Dateof.Death = as.Date(ocme_2023$Dateof.Death, "%m/%d/%Y")
# add in month, because for some reason this dataset doesn't have it
ocme_2023$Month <- month(ocme_2023$Dateof.Death)
# add in day of week
ocme_2023$Weekday <- weekdays(ocme_2023$Dateof.Death)
# add in year
ocme_2023$Year <- year(ocme_2023$Dateof.Death)
# remove anyone with a non-ct residence
ocme_2023 <- subset(ocme_2023, Injury.State == "CT")
# convert all NA values to 0
ocme_2023[is.na(ocme_2023)] <- 0
# ID xylazine/gabapentin in tox -- only look at words before the first semicolon because that's what's in the tox
# extract everything before a character: https://www.statology.org/r-extract-string-before-space/
# check if a word is in a string: https://www.statology.org/r-check-if-column-contains-string/
# I have to do MY.NOTES.1 because there is apparently a duplicate my notes column [the first one only contains a few entries]
ocme_2023$gabapentin <- ifelse(str_detect(gsub( ";.*$", "", ocme_2023$MY.NOTES), 'Gabapentin')==TRUE, 1, ifelse(str_detect(gsub( ";.*$", "", ocme_2023$MY.NOTES), 'gabapentin')==TRUE, 1, ifelse(str_detect(gsub( ";.*$", "", ocme_2023$MY.NOTES), 'Pregabalin')==TRUE, 1, ifelse(str_detect(gsub( ";.*$", "", ocme_2023$MY.NOTES), 'pregabalin')==TRUE, 1,0))))
#ocme_2023$gabapentin <- ifelse(str_detect(gsub( ";.*$", "", ocme_2023$MY.NOTES), 'Gabapentin')==TRUE, 1, ifelse(str_detect(gsub( ";.*$", "", ocme_2023$MY.NOTES), 'gabapentin')==TRUE, 1, 0))
ocme_2023$xylazine <- ifelse(str_detect(gsub( ";.*$", "", ocme_2023$MY.NOTES), 'Xylazine')==TRUE, 1, ifelse(str_detect(gsub( ";.*$", "", ocme_2023$MY.NOTES), 'xylazine')==TRUE, 1, 0))
ocme_2023$weed <- ifelse(str_detect(gsub( ";.*$", "", ocme_2023$MY.NOTES), 'OH-THC')==TRUE, 1, ifelse(str_detect(gsub( ";.*$", "", ocme_2023$MY.NOTES), 'oh-thc')==TRUE, 1, 0))
# create a stimulant variable with cocaine + amphetamines
ocme_2023$stimulant <- ifelse(ocme_2023$cocaine ==1, 1, ifelse(ocme_2023$amphetamine.other.stimulants..including.eutylone..MDMA..N.N.diMe.pentylone..MDA. == 1, 1, 0))

#this will be used for town calculations -- a better injury city indicator, renames blanks and gets rid of the Willimantic/Windham confusion 
ocme_2023$injury_city_revised <- ifelse(ocme_2023$Injury.City == "", "Unknown", ifelse(ocme_2023$Injury.City == "Willimantic", "Windham", ocme_2023$Injury.City))
# collapse injury location
table(ocme_2023$injury_collapsed)
ocme_2023$injury_collapsed <- ifelse(ocme_2023$injury.locn_revised %in% c("home", "home (unclear)", "other residence"), "residence", ifelse(ocme_2023$injury.locn_revised %in% c("other, indoors", "other, indoors ", "restaurant", "store", "store/shopping area", "penal institution"), "other indoors", ifelse(ocme_2023$injury.locn_revised %in% c("boat", "camper IFO address"), "vehicle", ifelse(ocme_2023$injury.locn_revised %in% c("", "unknown", "unspecified", "unclear", "other "), "unknown", ifelse(ocme_2023$injury.locn_revised == "shelter ", "shelter", ifelse(ocme_2023$injury.locn_revised =="other ", "other", ifelse(ocme_2023$injury.locn_revised %in% c("outdoors ", "outdoor", "sidewalk", "outside", "wooded area"), "outdoors", ifelse(ocme_2023$injury.locn_revised == "parking garage", "parking lot", ocme_2023$injury.locn_revised))))))))
# make race_numeric into words
ocme_2023$race_numeric_words <- ifelse(ocme_2023$race_.numeric...1.White..........2.Black....3.Hispanic...4.Other....7.UNK == "1", "White", ifelse(ocme_2023$race_.numeric...1.White..........2.Black....3.Hispanic...4.Other....7.UNK == "2", "Black", ifelse(ocme_2023$race_.numeric...1.White..........2.Black....3.Hispanic...4.Other....7.UNK == "3", "Hispanic", ifelse(ocme_2023$race_.numeric...1.White..........2.Black....3.Hispanic...4.Other....7.UNK == "4", "Other", ifelse(ocme_2023$race_.numeric...1.White..........2.Black....3.Hispanic...4.Other....7.UNK %in% c("0", "7"), "Other", "Other")))))
# there's a NA in here and for some reason replacing it using the line above didn't work, so we'll try this
# from https://stackoverflow.com/questions/55910824/replace-na-with-unknown
ocme_2023 <- ocme_2023 %>% replace_na(list(race_numeric_words = "Other"))
# create a race sex variable
ocme_2023$race_sex <- paste(na.omit(ocme_2023$race_numeric_words), ocme_2023$Sex, sep = " ")
# rename "Sex" to "Sex_revised"
names(ocme_2023)[names(ocme_2023) == 'Sex'] <- 'Sex_revised'
# indicator variable to aggregate
ocme_2023$fatal <- 1
# rename age variable to "age"
names(ocme_2023)[names(ocme_2023) == 'Age'] <- 'age'
# set ages between 18-80
ocme_2023 <- subset(ocme_2023, age > 17 & age < 81)

# export files
write.csv(ocme_old, file="processed_data/ocme_2009_2017_processed.csv")
write.csv(ocme_2018, file="processed_data/ocme_2018_processed.csv")
write.csv(ocme_2019, file="processed_data/ocme_2019_processed.csv")
write.csv(ocme_2020, file="processed_data/ocme_2020_processed.csv")
write.csv(ocme_2021, file="processed_data/ocme_2021_processed.csv")
write.csv(ocme_2022, file="processed_data/ocme_2022_processed.csv")
write.csv(ocme_2023, file="processed_data/ocme_2023_processed.csv")
