####################################################################################
# This is basic preprocessing of NC data to do the following:
#   * Reformat date to be a "date" data type
#   * Add in day of week
#   * Collapse race data
#   * Extract substances of interest from ICD codes, make into their own tox fields
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
nc_1 <- read.csv("nc_2009_2013.csv")
nc_2 <- read.csv("nc_2014_2019.csv")
nc_3 <- read.csv("nc_2020_2023.csv")

### start with nc_1

# add 0 to any of the date of death entries with a single digit month, so that way I can turn this into a date
nc_1$DOD <- ifelse(nchar(nc_1$Date.of.Death) == 7, paste("0", nc_1$Date.of.Death, sep=""), nc_1$Date.of.Death)
nc_1$DOD <- as.Date(nc_1$DOD, format="%m%d%Y")

# make a race field that's analogous to the other states' race fields
# first add in Latine people -- I don't distinguish between white Latine/Afrolatine etc
nc_1$race_numeric_words <- ifelse(nc_1$Ethnicity == "Unknown", nc_1$Race, ifelse(nc_1$Ethnicity == "Non-Hispanic", nc_1$Race, "Hispanic"))

# then collapse the other race categories to match the other states
nc_1$race_collapsed <- ifelse(nc_1$race_numeric_words %in% c("White", "Hispanic", "Black"), nc_1$race_numeric_words, "Other")

# make the sex variable more descriptive
nc_1$Sex_revised <- ifelse(nc_1$Sex == "M", "Male", "Female")

# concatenate the race and sex variables, so "Black" and "female" becomes "Black female"
nc_1$Race_sex <- paste(nc_1$race_collapsed, nc_1$Sex_revised, sep = " ")

# add in day of week -- from https://stackoverflow.com/questions/9216138/find-the-day-of-a-week
nc_1$Weekday <- weekdays(nc_1$DOD)
# add in month -- from https://stackoverflow.com/questions/22603847/how-to-extract-month-from-date-in-r
nc_1$Month <- month(nc_1$DOD)
# add in year -- we will use this to split into old/young. Not the most elegant solution but whatever
nc_1$Year <- year(nc_1$DOD)

# set ages between 18-80
nc_1 <- subset(nc_1, Age > 17 & Age < 81)


# get rid of anyone who died for a non-accidental/undetermined reason--cod1 has this info
nc_1 <- subset(nc_1, cod1 %in% c("X40", "X41", "X42", "X43", "X44", "Y10", "Y11", "Y12", "Y13", "Y14"))

# get one COD field so I can work with it more easily -- from https://stackoverflow.com/questions/18115550/combine-two-or-more-columns-in-a-dataframe-into-a-new-column-with-a-new-name

nc_1$cod <- paste(nc_1$cod1, nc_1$cod2, nc_1$cod3, nc_1$cod4, nc_1$cod5, nc_1$cod6, nc_1$cod7, nc_1$cod8, nc_1$cod9, nc_1$cod10, nc_1$cod11, nc_1$cod12, nc_1$cod13, nc_1$cod14, nc_1$cod15, nc_1$cod16, nc_1$cod17, nc_1$cod18, nc_1$cod19, nc_1$cod20)

# ID different substance categories -- from https://stackoverflow.com/questions/10128617/test-if-characters-are-in-a-string
nc_1$etoh <- ifelse(grepl("T510", nc_1$cod, fixed = TRUE), 1, 0)
nc_1$pharma <- ifelse(grepl("T402", nc_1$cod, fixed = TRUE), 1, 0)
nc_1$synthetic <- ifelse(grepl("T404", nc_1$cod, fixed = TRUE), 1, 0)
#nc_1$stimulant <- ifelse(grepl("T405", nc_1$cod, fixed = TRUE), 1, 0)
nc_1$methadone <- ifelse(grepl("T403", nc_1$cod, fixed = TRUE), 1, 0)
nc_1$stimulant <- ifelse(grepl("T405", nc_1$cod, fixed = TRUE), 1, ifelse(grepl("T436", nc_1$cod, fixed = TRUE), 1, 0))

# subset for opioid-detected deaths
# this is easiest to do by creating an opioid-detected variable first

nc_1$opioid_detected <- ifelse(grepl("T400", nc_1$cod, fixed = TRUE) | grepl("T401", nc_1$cod, fixed = TRUE) | grepl("T402", nc_1$cod, fixed = TRUE) | grepl("T403", nc_1$cod, fixed = TRUE) | grepl("T404", nc_1$cod, fixed = TRUE) | grepl("T406", nc_1$cod, fixed = TRUE), 1, 0)
nc_1 <- subset(nc_1, opioid_detected == 1)


# get a subset of columns to later merge with the other datafiles for other years
nc_1_merge <- select(nc_1, "Age", "DOD", "Weekday", "Month", "Year", "race_collapsed", "Sex_revised", "Race_sex", "etoh", "pharma", "synthetic", "stimulant", "methadone")

### let's work on nc_2 now

# add 0 to any of the date of death entries with a single digit month, so that way I can turn this into a date
nc_2$DOD <- ifelse(nchar(nc_2$Date.of.Death) == 7, paste("0", nc_2$Date.of.Death, sep=""), nc_2$Date.of.Death)
nc_2$DOD <- as.Date(nc_2$DOD, format="%m%d%Y")

# make the sex variable more descriptive
nc_2$Sex_revised <- ifelse(nc_2$Sex == "M", "Male", "Female")

# make a race field that's analogous to the other states' race fields
nc_2$race_collapsed <- ifelse(grepl("White", nc_2$Race.Ethnicity, fixed = TRUE), "White", ifelse(grepl("Black", nc_2$Race.Ethnicity, fixed = TRUE), "Black", ifelse(grepl("Non-Hispanic", nc_2$Race.Ethnicity, fixed = TRUE), "Other", ifelse(grepl("Unknown", nc_2$Race.Ethnicity, fixed = TRUE), "Other", nc_2$Race.Ethnicity))))

# concatenate the race and sex variables, so "Black" and "female" becomes "Black female"
nc_2$Race_sex <- paste(nc_2$race_collapsed, nc_2$Sex_revised, sep = " ")

# add in day of week -- from https://stackoverflow.com/questions/9216138/find-the-day-of-a-week
nc_2$Weekday <- weekdays(nc_2$DOD)
# add in month -- from https://stackoverflow.com/questions/22603847/how-to-extract-month-from-date-in-r
nc_2$Month <- month(nc_2$DOD)
# add in year -- we will use this to split into old/young. Not the most elegant solution but whatever
nc_2$Year <- year(nc_2$DOD)

# set ages between 18-80
nc_2 <- subset(nc_2, Age > 17 & Age < 81)

# get rid of anyone who died for a non-accidental/undetermined reason--cod1 has this info
nc_2 <- subset(nc_2, Underlying.Cause.of.Death %in% c("X40", "X41", "X42", "X43", "X44", "Y10", "Y11", "Y12", "Y13", "Y14"))

# get one COD field so I can work with it more easily -- from https://stackoverflow.com/questions/18115550/combine-two-or-more-columns-in-a-dataframe-into-a-new-column-with-a-new-name

nc_2$cod <- paste(nc_2$cod1, nc_2$cod2, nc_2$cod3, nc_2$cod4, nc_2$cod5, nc_2$cod6, nc_2$cod7, nc_2$cod8, nc_2$cod9, nc_2$cod10, nc_2$cod11, nc_2$cod12, nc_2$cod13, nc_2$cod14, nc_2$cod15, nc_2$cod16, nc_2$cod17, nc_2$cod18, nc_2$cod19, nc_2$cod20)

# ID different substance categories -- from https://stackoverflow.com/questions/10128617/test-if-characters-are-in-a-string
nc_2$etoh <- ifelse(grepl("T510", nc_2$cod, fixed = TRUE), 1, 0)
nc_2$pharma <- ifelse(grepl("T402", nc_2$cod, fixed = TRUE), 1, 0)
nc_2$synthetic <- ifelse(grepl("T404", nc_2$cod, fixed = TRUE), 1, 0)
nc_2$methadone <- ifelse(grepl("T403", nc_2$cod, fixed = TRUE), 1, 0)
#nc_2$stimulant <- ifelse(grepl("T405", nc_2$cod, fixed = TRUE), 1, 0)
nc_2$stimulant <- ifelse(grepl("T405", nc_2$cod, fixed = TRUE), 1, ifelse(grepl("T436", nc_2$cod, fixed = TRUE), 1, 0))

# subset for opioid-detected deaths
# this is easiest to do by creating an opioid-detected variable first

nc_2$opioid_detected <- ifelse(grepl("T400", nc_2$cod, fixed = TRUE) | grepl("T401", nc_2$cod, fixed = TRUE) | grepl("T402", nc_2$cod, fixed = TRUE) | grepl("T403", nc_2$cod, fixed = TRUE) | grepl("T404", nc_2$cod, fixed = TRUE) | grepl("T406", nc_2$cod, fixed = TRUE), 1, 0)
nc_2 <- subset(nc_2, opioid_detected == 1)

# get a subset of columns to later merge with the other datafiles for other years
nc_2_merge <- select(nc_2, "Age", "DOD", "Weekday", "Month", "Year", "race_collapsed", "Sex_revised", "Race_sex", "etoh", "pharma", "synthetic", "stimulant", "methadone")


### let's work on nc_3

# add 0 to any of the date of death entries with a single digit month, so that way I can turn this into a date
nc_3$DOD <- ifelse(nchar(nc_3$Date.of.Death) == 7, paste("0", nc_3$Date.of.Death, sep=""), nc_3$Date.of.Death)
nc_3$DOD <- as.Date(nc_3$DOD, format="%m%d%Y")

# make the sex variable more descriptive
nc_3$Sex_revised <- ifelse(nc_3$Sex == "M", "Male", "Female")

# make a race field that's analogous to the other states' race fields
nc_3$race_collapsed <- ifelse(grepl("White", nc_3$Race.Ethnicity, fixed = TRUE), "White", ifelse(grepl("Black", nc_3$Race.Ethnicity, fixed = TRUE), "Black", ifelse(grepl("Non-Hispanic", nc_3$Race.Ethnicity, fixed = TRUE), "Other", ifelse(grepl("Unknown", nc_3$Race.Ethnicity, fixed = TRUE), "Other", nc_3$Race.Ethnicity))))

# concatenate the race and sex variables, so "Black" and "female" becomes "Black female"
nc_3$Race_sex <- paste(nc_3$race_collapsed, nc_3$Sex_revised, sep = " ")

# add in day of week -- from https://stackoverflow.com/questions/9216138/find-the-day-of-a-week
nc_3$Weekday <- weekdays(nc_3$DOD)
# add in month -- from https://stackoverflow.com/questions/22603847/how-to-extract-month-from-date-in-r
nc_3$Month <- month(nc_3$DOD)
# add in year -- we will use this to split into old/young. Not the most elegant solution but whatever
nc_3$Year <- year(nc_3$DOD)

# set ages between 18-80
nc_3 <- subset(nc_3, Age > 17 & Age < 81)

# get rid of anyone who died for a non-accidental/undetermined reason--cod1 has this info
nc_3 <- subset(nc_3, Underlying.Cause.of.Death %in% c("X40", "X41", "X42", "X43", "X44", "Y10", "Y11", "Y12", "Y13", "Y14"))

# get one COD field so I can work with it more easily -- from https://stackoverflow.com/questions/18115550/combine-two-or-more-columns-in-a-dataframe-into-a-new-column-with-a-new-name

nc_3$cod <- paste(nc_3$cod1, nc_3$cod2, nc_3$cod3, nc_3$cod4, nc_3$cod5, nc_3$cod6, nc_3$cod7, nc_3$cod8, nc_3$cod9, nc_3$cod10, nc_3$cod11, nc_3$cod12, nc_3$cod13, nc_3$cod14, nc_3$cod15, nc_3$cod16, nc_3$cod17, nc_3$cod18, nc_3$cod19, nc_3$cod20)

# ID different substance categories -- from https://stackoverflow.com/questions/10128617/test-if-characters-are-in-a-string
nc_3$etoh <- ifelse(grepl("T510", nc_3$cod, fixed = TRUE), 1, 0)
nc_3$pharma <- ifelse(grepl("T402", nc_3$cod, fixed = TRUE), 1, 0)
nc_3$synthetic <- ifelse(grepl("T404", nc_3$cod, fixed = TRUE), 1, 0)
nc_3$methadone <- ifelse(grepl("T403", nc_3$cod, fixed = TRUE), 1, 0)
#nc_3$stimulant <- ifelse(grepl("T405", nc_3$cod, fixed = TRUE), 1, 0)
nc_3$stimulant <- ifelse(grepl("T405", nc_3$cod, fixed = TRUE), 1, ifelse(grepl("T436", nc_3$cod, fixed = TRUE), 1, 0))

# subset for opioid-detected deaths
# this is easiest to do by creating an opioid-detected variable first

nc_3$opioid_detected <- ifelse(grepl("T400", nc_3$cod, fixed = TRUE) | grepl("T401", nc_3$cod, fixed = TRUE) | grepl("T402", nc_3$cod, fixed = TRUE) | grepl("T403", nc_3$cod, fixed = TRUE) | grepl("T404", nc_3$cod, fixed = TRUE) | grepl("T406", nc_3$cod, fixed = TRUE), 1, 0)
nc_3 <- subset(nc_3, opioid_detected == 1)

# get a subset of columns to later merge with the other datafiles for other years
nc_3_merge <- select(nc_3, "Age", "DOD", "Weekday", "Month", "Year", "race_collapsed", "Sex_revised", "Race_sex", "etoh", "pharma", "synthetic", "stimulant", "methadone")

# bind everything together so we have one dataframe

nc <- rbind(nc_1_merge, nc_2_merge, nc_3_merge)

# indicator variable for aggregation in analysis
nc$fatal <- 1

# export files
write.csv(nc, file="processed_data/nc_processed.csv")