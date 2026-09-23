####################################################################################
# This is basic preprocessing of mass data to do the following:
#   * Reformat date to be a "date" data type
#   * Add in day of week
#   * Remove non-MA ODs
####################################################################################

# set working directory -- if you don't do this the relative paths won't work
setwd("C:\\Users\\ar2529\\Desktop\\overdose_age\\data_preprocessing")
library(stringr)
library(dplyr)
library(lubridate)
library(patchwork)
library(tidyverse) #to replace NA with something else
library(sf)
### 2009-2017

# i want to merge everything into one dataset so it's easier to work with

mass_2009 <- read.csv("mass_2009.csv")
mass_2010 <- read.csv("mass_2010.csv")
mass_2011 <- read.csv("mass_2011.csv")
mass_2012 <- read.csv("mass_2012.csv")
mass_2013 <- read.csv("mass_2013.csv")
mass_2014 <- read.csv("mass_2014_with_tox.csv")
mass_2015 <- read.csv("mass_2015.csv")
mass_2016 <- read.csv("mass_2016.csv")
mass_2017 <- read.csv("mass_2017.csv")

mass_2009_merge <- select(mass_2009, "DOD", "AGE_AT_DEATH", "SEX", "RACE", "TRX_CAUSE_ACME", "TRX_REC_AXIS_CD")
# the DOD variable is listed as one string -- need to turn into an age
# from https://stackoverflow.com/questions/32854538/converting-a-character-string-into-a-date-in-r
mass_2009_merge$DOD = as.Date(ymd(mass_2009_merge$DOD), "%m/%d/%Y")
mass_2009_merge$Race_collapsed <- ifelse(mass_2009_merge$RACE == "1", "White", ifelse(mass_2009_merge$RACE == "2", "Black", ifelse(mass_2009_merge$RACE =="15", "Hispanic", ifelse(mass_2009_merge$RACE == "99", "Other", "Other"))))
mass_2009_merge$Sex_revised <- ifelse(mass_2009_merge$SEX == "1", "Male", ifelse(mass_2009_merge$SEX == "2", "Female", "Unknown"))
# rename everything to match with the other columns
# first get rid of the other race column since that'll get confusing
mass_2009_merge <- select(mass_2009_merge, "DOD", "AGE_AT_DEATH", "Sex_revised", "Race_collapsed", "TRX_CAUSE_ACME", "TRX_REC_AXIS_CD") 
# rename age variable to age
names(mass_2009_merge)[names(mass_2009_merge) == 'AGE_AT_DEATH'] <- 'age'
# rename race variable to race
names(mass_2009_merge)[names(mass_2009_merge) == 'Race_collapsed'] <- 'Race'


mass_2010_merge <- select(mass_2010, "DOD", "AGE_AT_DEATH", "SEX", "RACE", "TRX_CAUSE_ACME", "TRX_REC_AXIS_CD")
mass_2010_merge$Race_collapsed <- ifelse(mass_2010_merge$RACE == "1", "White", ifelse(mass_2010_merge$RACE == "2", "Black", ifelse(mass_2010_merge$RACE =="15", "Hispanic", ifelse(mass_2010_merge$RACE == "99", "Other", "Other"))))
mass_2010_merge$DOD = as.Date(ymd(mass_2010_merge$DOD), "%m/%d/%Y")
mass_2010_merge$Sex_revised <- ifelse(mass_2010_merge$SEX == "1", "Male", ifelse(mass_2010_merge$SEX == "2", "Female", "Unknown"))
# rename everything to match with the other columns
# first get rid of the other race column since that'll get confusing
mass_2010_merge <- select(mass_2010_merge, "DOD", "AGE_AT_DEATH", "Sex_revised", "Race_collapsed", "TRX_CAUSE_ACME", "TRX_REC_AXIS_CD") 
# rename age variable to age
names(mass_2010_merge)[names(mass_2010_merge) == 'AGE_AT_DEATH'] <- 'age'
# rename race variable to race
names(mass_2010_merge)[names(mass_2010_merge) == 'Race_collapsed'] <- 'Race'


mass_2011_merge <- select(mass_2011, "DOD", "AGE_AT_DEATH", "SEX", "RACE", "TRX_CAUSE_ACME", "TRX_REC_AXIS_CD")
mass_2011_merge$DOD = as.Date(ymd(mass_2011_merge$DOD), "%m/%d/%Y")
mass_2011_merge$Race_collapsed <- ifelse(mass_2011_merge$RACE == "1", "White", ifelse(mass_2011_merge$RACE == "2", "Black", ifelse(mass_2011_merge$RACE =="15", "Hispanic", ifelse(mass_2011_merge$RACE == "99", "Other", "Other"))))
mass_2011_merge$Sex_revised <- ifelse(mass_2011_merge$SEX == "1", "Male", ifelse(mass_2011_merge$SEX == "2", "Female", "Unknown"))
# rename everything to match with the other columns
# first get rid of the other race column since that'll get confusing
mass_2011_merge <- select(mass_2011_merge, "DOD", "AGE_AT_DEATH", "Sex_revised", "Race_collapsed", "TRX_CAUSE_ACME", "TRX_REC_AXIS_CD") 
# rename age variable to age
names(mass_2011_merge)[names(mass_2011_merge) == 'AGE_AT_DEATH'] <- 'age'
# rename race variable to race
names(mass_2011_merge)[names(mass_2011_merge) == 'Race_collapsed'] <- 'Race'

mass_2012_merge <- select(mass_2012, "DOD", "AGE_AT_DEATH", "SEX", "RACE", "TRX_CAUSE_ACME", "TRX_REC_AXIS_CD")
mass_2012_merge$DOD = as.Date(ymd(mass_2012_merge$DOD), "%m/%d/%Y")
mass_2012_merge$Race_collapsed <- ifelse(mass_2012_merge$RACE == "1", "White", ifelse(mass_2012_merge$RACE == "2", "Black", ifelse(mass_2012_merge$RACE =="15", "Hispanic", ifelse(mass_2012_merge$RACE == "99", "Other", "Other"))))
mass_2012_merge$Sex_revised <- ifelse(mass_2012_merge$SEX == "1", "Male", ifelse(mass_2012_merge$SEX == "2", "Female", "Unknown"))
# rename everything to match with the other columns
# first get rid of the other race column since that'll get confusing
mass_2012_merge <- select(mass_2012_merge, "DOD", "AGE_AT_DEATH", "Sex_revised", "Race_collapsed", "TRX_CAUSE_ACME", "TRX_REC_AXIS_CD") 
# rename age variable to age
names(mass_2012_merge)[names(mass_2012_merge) == 'AGE_AT_DEATH'] <- 'age'
# rename race variable to race
names(mass_2012_merge)[names(mass_2012_merge) == 'Race_collapsed'] <- 'Race'

mass_2013_merge <- select(mass_2013, "DOD", "AGE_AT_DEATH", "SEX", "RACE", "TRX_CAUSE_ACME", "TRX_REC_AXIS_CD")
mass_2013_merge$DOD = as.Date(ymd(mass_2013_merge$DOD), "%m/%d/%Y")
mass_2013_merge$Race_collapsed <- ifelse(mass_2013_merge$RACE == "1", "White", ifelse(mass_2013_merge$RACE == "2", "Black", ifelse(mass_2013_merge$RACE =="15", "Hispanic", ifelse(mass_2013_merge$RACE == "99", "Other", "Other"))))
mass_2013_merge$Sex_revised <- ifelse(mass_2013_merge$SEX == "1", "Male", ifelse(mass_2013_merge$SEX == "2", "Female", "Unknown"))
# rename everything to match with the other columns
# first get rid of the other race column since that'll get confusing
mass_2013_merge <- select(mass_2013_merge, "DOD", "AGE_AT_DEATH", "Sex_revised", "Race_collapsed", "TRX_CAUSE_ACME", "TRX_REC_AXIS_CD") 
# rename age variable to age
names(mass_2013_merge)[names(mass_2013_merge) == 'AGE_AT_DEATH'] <- 'age'
# rename race variable to race
names(mass_2013_merge)[names(mass_2013_merge) == 'Race_collapsed'] <- 'Race'

# create a new categorical race variable
mass_2014$Race_collapsed <- ifelse(mass_2014$RACE1 == "Y", "White", ifelse(mass_2014$RACE_BLACK == "Y", "Black", ifelse((mass_2014$RACE_HISP_LAT_BLACK == "Y" | mass_2014$RACE_HISP_LAT_WHITE == "Y" | mass_2014$DETHNIC4 == "Y"), "Hispanic", ifelse((mass_2014$DETHNIC_REFUSED == "Y" | mass_2014$RACE_NOT_OBT == "Y" | mass_2014$RACE_UNK == "Y"), "Other", "Other"))))
mass_2014_merge <- select(mass_2014, "DOD_4_FD", "AGE1", "Race_collapsed", "INJRY_STATE", "SEX", "TRX_CAUSE_ACME", "TRX_REC_AXIS_CD")
# get rid of any non-MA injuries
mass_2014_merge <- subset(mass_2014_merge, INJRY_STATE == "MASSACHUSETTS")
mass_2014_merge <- select(mass_2014_merge, "DOD_4_FD", "AGE1", "Race_collapsed", "SEX", "TRX_CAUSE_ACME", "TRX_REC_AXIS_CD")
mass_2014_merge$DOD = as.Date(mass_2014_merge$DOD_4_FD, "%m/%d/%Y")
mass_2014_merge$Sex_revised <- ifelse(mass_2014_merge$SEX == "M", "Male", "Female")
# rename age variable to age
names(mass_2014_merge)[names(mass_2014_merge) == 'AGE1'] <- 'age'
# rename race variable to race
names(mass_2014_merge)[names(mass_2014_merge) == 'Race_collapsed'] <- 'Race'
mass_2014_merge <- select(mass_2014_merge, "DOD", "age", "Sex_revised", "Race", "TRX_CAUSE_ACME", "TRX_REC_AXIS_CD")

# this is for mass 2014 without tox
# create a date of death variable
# apparently paste0 pastes without spaces whereas paste pastes with spaces
# mass_2014$DOD <- paste0(mass_2014$death_month, "/", mass_2014$death_day, "/", mass_2014$death_year)
# # there is a "race" variable but it has a lot of NAs, best not to use
# mass_2014$Race_collapsed <- ifelse(mass_2014$racewhite == "1", "White", ifelse(mass_2014$raceblack == "1", "Black", ifelse((mass_2014$racehispbl == "1" | mass_2014$racehispwh == "1" | mass_2014$racehispot == "1"), "Hispanic", "Other")))
# mass_2014_merge <- select(mass_2014, "DOD", "ageyear", "male", "Race_collapsed")
# # make DOD into date type
# mass_2014_merge$DOD = as.Date(mass_2014_merge$DOD, "%m/%d/%Y")
# # create new sex variable with more descriptive categories
# mass_2014_merge$Sex_revised <- ifelse(mass_2014_merge$male == 1, "Male", "Female")
# # rename age variable to age
# names(mass_2014_merge)[names(mass_2014_merge) == 'ageyear'] <- 'age'
# # rename race variable to race
# names(mass_2014_merge)[names(mass_2014_merge) == 'Race_collapsed'] <- 'Race'
# mass_2014_merge <- select(mass_2014_merge, "DOD", "age", "Sex_revised", "Race")
# # need to add in tox variables just so I can merge this with the other data--setting this to NA causes issues so it's 0
# mass_2014_merge$TRX_CAUSE_ACME <- replicate(nrow(mass_2014_merge), 0)
# mass_2014_merge$TRX_REC_AXIS_CD <- replicate(nrow(mass_2014_merge), 0)


# create a new categorical race variable
mass_2015$Race_collapsed <- ifelse(mass_2015$RACE1 == "Y", "White", ifelse(mass_2015$RACE_BLACK == "2", "Black", ifelse((mass_2015$RACE_HISP_LAT_BLACK == "Y" | mass_2015$RACE_HISP_LAT_WHITE == "Y" | mass_2015$DETHNIC4 == "Y"), "Hispanic", ifelse((mass_2015$DETHNIC_REFUSED == "Y" | mass_2015$RACE_NOT_OBT == "Y" | mass_2015$RACE_UNK == "Y"), "Other", "Other"))))
mass_2015_merge <- select(mass_2015, "DOD_4_FD", "AGE1", "Race_collapsed", "INJRY_STATE", "SEX", "TRX_CAUSE_ACME", "TRX_REC_AXIS_CD")
# get rid of any non-MA injuries
mass_2015_merge <- subset(mass_2015_merge, INJRY_STATE == "MASSACHUSETTS")
mass_2015_merge <- select(mass_2015_merge, "DOD_4_FD", "AGE1", "Race_collapsed", "SEX", "TRX_CAUSE_ACME", "TRX_REC_AXIS_CD")
mass_2015_merge$DOD = as.Date(mass_2015_merge$DOD_4_FD, "%m/%d/%Y")
mass_2015_merge$Sex_revised <- ifelse(mass_2015_merge$SEX == "M", "Male", "Female")
# rename age variable to age
names(mass_2015_merge)[names(mass_2015_merge) == 'AGE1'] <- 'age'
# rename race variable to race
names(mass_2015_merge)[names(mass_2015_merge) == 'Race_collapsed'] <- 'Race'
mass_2015_merge <- select(mass_2015_merge, "DOD", "age", "Sex_revised", "Race", "TRX_CAUSE_ACME", "TRX_REC_AXIS_CD")


mass_2016$Race_collapsed <- ifelse(mass_2016$RACE1 == "Y", "White", ifelse(mass_2016$RACE_BLACK == "2", "Black", ifelse((mass_2016$RACE_HISP_LAT_BLACK == "Y" | mass_2016$RACE_HISP_LAT_WHITE == "Y" | mass_2016$DETHNIC4 == "Y"), "Hispanic", ifelse((mass_2016$DETHNIC_REFUSED == "Y" | mass_2016$RACE_NOT_OBT == "Y" | mass_2016$RACE_UNK == "Y"), "Other", "Other"))))
mass_2016_merge <- select(mass_2016, "DOD_4_FD", "AGE1", "Race_collapsed", "INJRY_STATE", "SEX", "TRX_CAUSE_ACME", "TRX_REC_AXIS_CD")
# get rid of any non-MA injuries
mass_2016_merge <- subset(mass_2016_merge, INJRY_STATE == "MASSACHUSETTS")
mass_2016_merge <- select(mass_2016_merge, "DOD_4_FD", "AGE1", "Race_collapsed", "SEX", "TRX_CAUSE_ACME", "TRX_REC_AXIS_CD")
mass_2016_merge$DOD = as.Date(mass_2016_merge$DOD_4_FD, "%m/%d/%Y")
mass_2016_merge$Sex_revised <- ifelse(mass_2016_merge$SEX == "M", "Male", "Female")
# rename age variable to age
names(mass_2016_merge)[names(mass_2016_merge) == 'AGE1'] <- 'age'
# rename race variable to race
names(mass_2016_merge)[names(mass_2016_merge) == 'Race_collapsed'] <- 'Race'
mass_2016_merge <- select(mass_2016_merge, "DOD", "age", "Sex_revised", "Race", "TRX_CAUSE_ACME", "TRX_REC_AXIS_CD")


# for some reason the 2017 data only has CERT_RACE_SPECIFY so I guess I have to go through those... also there's no unknown here
mass_2017$Race_collapsed <- ifelse(mass_2017$CERT_RACE_SPECIFY %in% c("AMERICAN WHITE", "CAUCASIAN", "WHITE", "WHTIE"), "White", ifelse(mass_2017$CERT_RACE_SPECIFY %in% c("AFRICAN ACMERICAN", "AFRICAN AMERICAN", "BLACK", "BLACK-WHITE", "BLACK / WHITE", "HAITIAN/BLACK"), "Black", ifelse(mass_2017$CERT_RACE_SPECIFY %in% c("HISPANIC", "HISPANIC / LATINO / BLACK", "HISPANIC LATINO", "HISPANIC LATINO BLACK", "HISPANIC LATINO WHITE", "HISPANIC/CUBAN", "HISPANIC/GUATEMALAN", "HISPANIC/LATINO WHITE", "HISPANIC/LATINO/WHITE", "HISPANICE", "LATINO", "WHITE HISPANIC", "WHITE/BLACK/HISPANIC"), "Hispanic", "Other")))
mass_2017_merge <- select(mass_2017, "DOD_4_FD", "AGE1", "Race_collapsed", "INJRY_STATE", "SEX", "TRX_CAUSE_ACME", "TRX_REC_AXIS_CD")
# get rid of any non-MA injuries
mass_2017_merge <- subset(mass_2017_merge, INJRY_STATE == "MASSACHUSETTS")
mass_2017_merge$DOD = as.Date(mass_2017_merge$DOD_4_FD, "%m/%d/%Y")
mass_2017_merge <- select(mass_2017_merge, "DOD", "AGE1", "Race_collapsed", "SEX", "TRX_CAUSE_ACME", "TRX_REC_AXIS_CD")
mass_2017_merge$Sex_revised <- ifelse(mass_2017_merge$SEX == "M", "Male", "Female")
# rename age variable to age
names(mass_2017_merge)[names(mass_2017_merge) == 'AGE1'] <- 'age'
# rename race variable to race
names(mass_2017_merge)[names(mass_2017_merge) == 'Race_collapsed'] <- 'Race'
mass_2017_merge <- select(mass_2017_merge, "DOD", "age", "Sex_revised", "Race", "TRX_CAUSE_ACME", "TRX_REC_AXIS_CD")


# rbind when the number of columns match
mass_old <- rbind(mass_2009_merge, mass_2010_merge, mass_2011_merge, mass_2012_merge, mass_2013_merge, mass_2014_merge, mass_2015_merge, mass_2016_merge, mass_2017_merge)
# remove anyone whose listed age is unrealistically high, for some reason this happens
mass_old <- subset(mass_old, age <= 150)
# add in day of week -- from https://stackoverflow.com/questions/9216138/find-the-day-of-a-week
mass_old$Weekday <- weekdays(mass_old$DOD)
# add in month -- from https://stackoverflow.com/questions/22603847/how-to-extract-month-from-date-in-r
mass_old$Month <- month(mass_old$DOD)
# add in year -- we will use this to split into old/young. Not the most elegant solution but whatever
mass_old$Year <- year(mass_old$DOD)
# make ages between 18-80
mass_old <- subset(mass_old, age > 17 & age < 81)
# convert all NA values to 0
mass_old[is.na(mass_old)] <- 0
# make first letter of race capitalized
mass_old$Race <- str_to_title(mass_old$Race)
# concatenate the race and sex variables, so "Black" and "female" becomes "Black female"
mass_old$Race_sex <- paste(mass_old$Race, mass_old$Sex_revised, sep = " ")
# indicator variable to aggregate
mass_old$fatal <- 1
# get rid of anyone who died for a non-accidental/undetermined reason, but ignore 2014 since it has no tox data
# for this the easiest thing to do is to just separate the 2014 data, subset, and add it back in
#mass_old_14 <- subset(mass_old, Year == "2014")
mass_old <- subset(mass_old, TRX_CAUSE_ACME %in% c("X40", "X41", "X42", "X43", "X44", "Y10", "Y11", "Y12", "Y13", "Y14"))
#mass_old <- rbind(mass_old, mass_old_14)
# ID different substance categories -- from https://stackoverflow.com/questions/10128617/test-if-characters-are-in-a-string
mass_old$etoh <- ifelse(grepl("T510", mass_old$TRX_REC_AXIS_CD, fixed = TRUE), 1, 0)
mass_old$pharma <- ifelse(grepl("T402", mass_old$TRX_REC_AXIS_CD, fixed = TRUE), 1, 0)
mass_old$synthetic <- ifelse(grepl("T404", mass_old$TRX_REC_AXIS_CD, fixed = TRUE), 1, 0)
mass_old$stimulant <- ifelse(grepl("T405", mass_old$TRX_REC_AXIS_CD, fixed = TRUE), 1, ifelse(grepl("T436", mass_old$TRX_REC_AXIS_CD, fixed = TRUE), 1, 0))
#mass_old$stimulant <- ifelse(grepl("T405", mass_old$TRX_REC_AXIS_CD, fixed = TRUE), 1, 0)
mass_old$methadone <- ifelse(grepl("T403", mass_old$TRX_REC_AXIS_CD, fixed = TRUE), 1, 0)

# subset for opioid-detected deaths
# this is easiest to do by creating an opioid-detected variable first

mass_old$opioid_detected <- ifelse(grepl("T400", mass_old$TRX_REC_AXIS_CD, fixed = TRUE) | grepl("T401", mass_old$TRX_REC_AXIS_CD, fixed = TRUE) | grepl("T402", mass_old$TRX_REC_AXIS_CD, fixed = TRUE) | grepl("T403", mass_old$TRX_REC_AXIS_CD, fixed = TRUE) | grepl("T404", mass_old$TRX_REC_AXIS_CD, fixed = TRUE) | grepl("T406", mass_old$TRX_REC_AXIS_CD, fixed = TRUE), 1, 0)
mass_old <- subset(mass_old, opioid_detected == 1)

# prep this dataset for a merge so we can construct the fent era
mass_old_merge <- select(mass_old, "DOD", "age", "Sex_revised", "Race", "TRX_CAUSE_ACME", "TRX_REC_AXIS_CD", "Weekday", "Month", "Year", "Race_sex", "fatal", "etoh", "pharma", "synthetic", "stimulant", "methadone")



### create pharma and heroin eras

# pharma: 2009, heroin: 2010-2015
pharma_era <- subset(mass_old_merge, Year == "2009")
heroin_era <- subset(mass_old_merge, Year %in% c("2010", "2011", "2012", "2013", "2014", "2015"))
# create a variable that tells us which era we're in, so we can use it for plotting
pharma_era$Era <- rep("Pharma", times = nrow(pharma_era))
heroin_era$Era <- rep("Heroin", times = nrow(heroin_era))
# set ages between 18-80
pharma_era <- subset(pharma_era, age > 17 & age < 81)
heroin_era <- subset(heroin_era, age > 17 & age < 81)

### 2018

# import mass data
mass_2018 <- read.csv("mass_2018.csv")
# rename date of death variable to DOD
names(mass_2018)[names(mass_2018) == 'DOD_4_FD'] <- 'DOD'
# rename age variable to age
names(mass_2018)[names(mass_2018) == 'AGE1'] <- 'age'
# rename race variable to race
names(mass_2018)[names(mass_2018) == 'RACEGROUP'] <- 'Race'


# convert the DOD into a "date" data type so we can work with it
mass_2018$DOD = as.Date(mass_2018$DOD, "%m/%d/%Y")
# add in day of week -- from https://stackoverflow.com/questions/9216138/find-the-day-of-a-week
mass_2018$Weekday <- weekdays(mass_2018$DOD)
# add in month -- from https://stackoverflow.com/questions/22603847/how-to-extract-month-from-date-in-r
mass_2018$Month <- month(mass_2018$DOD)
# add in year -- we will use this to split into old/young. Not the most elegant solution but whatever
mass_2018$Year <- year(mass_2018$DOD)
# remove anyone with a non-ma injury state
mass_2018 <- subset(mass_2018, INJRY_STATE == "MASSACHUSETTS")
# create new sex variable with more descriptive categories
mass_2018$Sex_revised <- ifelse(mass_2018$SEX == "M", "Male", "Female")
# convert all NA values to 0
mass_2018[is.na(mass_2018)] <- 0
# make first letter of race capitalized
mass_2018$Race <- str_to_title(mass_2018$Race)
# collapse the race categories and standardize them
mass_2018$Race <- ifelse(mass_2018$Race == " White Nh", "White", ifelse(mass_2018$Race == " Black Nh", "Black", ifelse(mass_2018$Race == " Hispanic", "Hispanic", "Other")))
# concatenate the race and sex variables, so "Black" and "female" becomes "Black female"
mass_2018$Race_sex <- paste(mass_2018$Race, mass_2018$Sex_revised, sep = " ")
# indicator variable to aggregate
mass_2018$fatal <- 1
# get rid of anyone who died for a non-accidental/undetermined reason
mass_2018 <- subset(mass_2018, TRX_CAUSE_ACME %in% c("X40", "X41", "X42", "X43", "X44", "Y10", "Y11", "Y12", "Y13", "Y14"))
# ID different substance categories -- from https://stackoverflow.com/questions/10128617/test-if-characters-are-in-a-string
mass_2018$etoh <- ifelse(grepl("T510", mass_2018$TRX_REC_AXIS_CD, fixed = TRUE), 1, 0)
mass_2018$pharma <- ifelse(grepl("T402", mass_2018$TRX_REC_AXIS_CD, fixed = TRUE), 1, 0)
mass_2018$synthetic <- ifelse(grepl("T404", mass_2018$TRX_REC_AXIS_CD, fixed = TRUE), 1, 0)
#mass_2018$stimulant <- ifelse(grepl("T405", mass_2018$TRX_REC_AXIS_CD, fixed = TRUE), 1, 0)
mass_2018$stimulant <- ifelse(grepl("T405", mass_2018$TRX_REC_AXIS_CD, fixed = TRUE), 1, ifelse(grepl("T436", mass_2018$TRX_REC_AXIS_CD, fixed = TRUE), 1, 0))
mass_2018$methadone <- ifelse(grepl("T403", mass_2018$TRX_REC_AXIS_CD, fixed = TRUE), 1, 0)
# get rid of non-MA ODs
mass_2018 <- subset(mass_2018, INJRY_STATE == "MASSACHUSETTS")
# set ages between 18-80
mass_2018 <- subset(mass_2018, age > 17 & age < 81)
# subset for opioid-detected deaths
# this is easiest to do by creating an opioid-detected variable first
mass_2018$opioid_detected <- ifelse(grepl("T400", mass_2018$TRX_REC_AXIS_CD, fixed = TRUE) | grepl("T401", mass_2018$TRX_REC_AXIS_CD, fixed = TRUE) | grepl("T402", mass_2018$TRX_REC_AXIS_CD, fixed = TRUE) | grepl("T403", mass_2018$TRX_REC_AXIS_CD, fixed = TRUE) | grepl("T404", mass_2018$TRX_REC_AXIS_CD, fixed = TRUE) | grepl("T406", mass_2018$TRX_REC_AXIS_CD, fixed = TRUE), 1, 0)
mass_2018 <- subset(mass_2018, opioid_detected == 1)

# prep this dataset for a merge so we can construct the fent era
mass_2018_merge <- select(mass_2018, "DOD", "age", "Sex_revised", "Race", "TRX_CAUSE_ACME", "TRX_REC_AXIS_CD", "Weekday", "Month", "Year", "Race_sex", "fatal", "etoh", "pharma", "synthetic", "stimulant", "methadone")



### 2019

# import mass data
mass_2019 <- read.csv("mass_2019.csv")
# rename date of death variable to DOD
names(mass_2019)[names(mass_2019) == 'DOD_4_FD'] <- 'DOD'
# rename age variable to age
names(mass_2019)[names(mass_2019) == 'AGE1'] <- 'age'
# rename race variable to race
names(mass_2019)[names(mass_2019) == 'RACEGROUP'] <- 'Race'

# convert the DOD into a "date" data type so we can work with it
mass_2019$DOD = as.Date(mass_2019$DOD, "%m/%d/%Y")
# add in day of week -- from https://stackoverflow.com/questions/9216138/find-the-day-of-a-week
mass_2019$Weekday <- weekdays(mass_2019$DOD)
# add in month -- from https://stackoverflow.com/questions/22603847/how-to-extract-month-from-date-in-r
mass_2019$Month <- month(mass_2019$DOD)
# add in year -- we will use this to split into old/young. Not the most elegant solution but whatever
mass_2019$Year <- year(mass_2019$DOD)
# create new sex variable with more descriptive categories
mass_2019$Sex_revised <- ifelse(mass_2019$SEX == "M", "Male", "Female")
# remove anyone with a non-ma injury state
mass_2019 <- subset(mass_2019, INJRY_STATE == "MASSACHUSETTS")
# convert all NA values to 0
mass_2019[is.na(mass_2019)] <- 0
# make first letter of race capitalized
mass_2019$Race <- str_to_title(mass_2019$Race)
# collapse the race categories and standardize them
mass_2019$Race <- ifelse(mass_2019$Race == " White Nh", "White", ifelse(mass_2019$Race == " Black Nh", "Black", ifelse(mass_2019$Race == " Hispanic", "Hispanic", "Other")))
# concatenate the race and sex variables, so "Black" and "female" becomes "Black female"
mass_2019$Race_sex <- paste(mass_2019$Race, mass_2019$Sex_revised, sep = " ")
# indicator variable to aggregate
mass_2019$fatal <- 1
# subset out non-accidental/undetermined deaths
mass_2019 <- subset(mass_2019, TRX_CAUSE_ACME %in% c("X40", "X41", "X42", "X43", "X44", "Y10", "Y11", "Y12", "Y13", "Y14"))
# ID different substance categories -- from https://stackoverflow.com/questions/10128617/test-if-characters-are-in-a-string
mass_2019$etoh <- ifelse(grepl("T510", mass_2019$TRX_REC_AXIS_CD, fixed = TRUE), 1, 0)
mass_2019$pharma <- ifelse(grepl("T402", mass_2019$TRX_REC_AXIS_CD, fixed = TRUE), 1, 0)
mass_2019$synthetic <- ifelse(grepl("T404", mass_2019$TRX_REC_AXIS_CD, fixed = TRUE), 1, 0)
#mass_2019$stimulant <- ifelse(grepl("T405", mass_2019$TRX_REC_AXIS_CD, fixed = TRUE), 1, 0)
mass_2019$stimulant <- ifelse(grepl("T405", mass_2019$TRX_REC_AXIS_CD, fixed = TRUE), 1, ifelse(grepl("T436", mass_2019$TRX_REC_AXIS_CD, fixed = TRUE), 1, 0))
mass_2019$methadone <- ifelse(grepl("T403", mass_2019$TRX_REC_AXIS_CD, fixed = TRUE), 1, 0)
# get rid of non-MA ODs
mass_2019 <- subset(mass_2019, INJRY_STATE == "MASSACHUSETTS")
# set ages between 18-80
mass_2019 <- subset(mass_2019, age > 17 & age < 81)
# subset for opioid-detected deaths
# this is easiest to do by creating an opioid-detected variable first
mass_2019$opioid_detected <- ifelse(grepl("T400", mass_2019$TRX_REC_AXIS_CD, fixed = TRUE) | grepl("T401", mass_2019$TRX_REC_AXIS_CD, fixed = TRUE) | grepl("T402", mass_2019$TRX_REC_AXIS_CD, fixed = TRUE) | grepl("T403", mass_2019$TRX_REC_AXIS_CD, fixed = TRUE) | grepl("T404", mass_2019$TRX_REC_AXIS_CD, fixed = TRUE) | grepl("T406", mass_2019$TRX_REC_AXIS_CD, fixed = TRUE), 1, 0)
mass_2019 <- subset(mass_2019, opioid_detected == 1)

# prep this dataset for a merge so we can construct the fent era
mass_2019_merge <- select(mass_2019, "DOD", "age", "Sex_revised", "Race", "TRX_CAUSE_ACME", "TRX_REC_AXIS_CD", "Weekday", "Month", "Year", "Race_sex", "fatal", "etoh", "pharma", "synthetic", "stimulant", "methadone")

### fent era

# I processed the pre-2018 data a little differently sadly so I'll have to create a separate df for 2016/2017 and then merge
fent_old <- subset(mass_old_merge, Year %in% c("2016", "2017"))
fent_era <- rbind(fent_old, mass_2018_merge, mass_2019_merge)
# create a variable that tells us which era we're in, so we can use it for plotting
fent_era$Era <- rep("Fentanyl", times = nrow(fent_era))
# set ages between 18-80
fent_era <- subset(fent_era, age > 17 & age < 81)


### 2020

# import mass data
mass_2020 <- read.csv("mass_2020.csv")
# rename date of death variable to DOD
names(mass_2020)[names(mass_2020) == 'DOD_4_FD'] <- 'DOD'
# rename age variable to age
names(mass_2020)[names(mass_2020) == 'AGE1'] <- 'age'
# rename race variable to race
names(mass_2020)[names(mass_2020) == 'RACEGROUP'] <- 'Race'

# convert the DOD into a "date" data type so we can work with it
mass_2020$DOD = as.Date(mass_2020$DOD, "%m/%d/%Y")
# add in day of week -- from https://stackoverflow.com/questions/9216138/find-the-day-of-a-week
mass_2020$Weekday <- weekdays(mass_2020$DOD)
# add in month -- from https://stackoverflow.com/questions/22603847/how-to-extract-month-from-date-in-r
mass_2020$Month <- month(mass_2020$DOD)
# add in year -- we will use this to split into old/young. Not the most elegant solution but whatever
mass_2020$Year <- year(mass_2020$DOD)
# remove anyone with a non-ma injury state
mass_2020 <- subset(mass_2020, INJRY_STATE == "MASSACHUSETTS")
# create new sex variable with more descriptive categories
mass_2020$Sex_revised <- ifelse(mass_2020$SEX == "M", "Male", "Female")
# convert all NA values to 0
mass_2020[is.na(mass_2020)] <- 0
# make first letter of race capitalized
mass_2020$Race <- str_to_title(mass_2020$Race)
# collapse the race categories and standardize them
mass_2020$Race <- ifelse(mass_2020$Race == " White Nh", "White", ifelse(mass_2020$Race == " Black Nh", "Black", ifelse(mass_2020$Race == " Hispanic", "Hispanic", "Other")))
# concatenate the race and sex variables, so "Black" and "female" becomes "Black female"
mass_2020$Race_sex <- paste(mass_2020$Race, mass_2020$Sex_revised, sep = " ")
# indicator variable to aggregate
mass_2020$fatal <- 1
# subset out non-accidental/undetermined deaths
mass_2020 <- subset(mass_2020, TRX_CAUSE_ACME %in% c("X40", "X41", "X42", "X43", "X44", "Y10", "Y11", "Y12", "Y13", "Y14"))
# ID different substance categories -- from https://stackoverflow.com/questions/10128617/test-if-characters-are-in-a-string
mass_2020$etoh <- ifelse(grepl("T510", mass_2020$TRX_REC_AXIS_CD, fixed = TRUE), 1, 0)
mass_2020$pharma <- ifelse(grepl("T402", mass_2020$TRX_REC_AXIS_CD, fixed = TRUE), 1, 0)
mass_2020$synthetic <- ifelse(grepl("T404", mass_2020$TRX_REC_AXIS_CD, fixed = TRUE), 1, 0)
#mass_2020$stimulant <- ifelse(grepl("T405", mass_2020$TRX_REC_AXIS_CD, fixed = TRUE), 1, 0)
mass_2020$methadone <- ifelse(grepl("T403", mass_2020$TRX_REC_AXIS_CD, fixed = TRUE), 1, 0)
mass_2020$stimulant <- ifelse(grepl("T405", mass_2020$TRX_REC_AXIS_CD, fixed = TRUE), 1, ifelse(grepl("T436", mass_2020$TRX_REC_AXIS_CD, fixed = TRUE), 1, 0))
# get rid of non-MA ODs
mass_2020 <- subset(mass_2020, INJRY_STATE == "MASSACHUSETTS")
# set ages between 18-80
mass_2020 <- subset(mass_2020, age > 17 & age < 81)
# subset for opioid-detected deaths
# this is easiest to do by creating an opioid-detected variable first
mass_2020$opioid_detected <- ifelse(grepl("T400", mass_2020$TRX_REC_AXIS_CD, fixed = TRUE) | grepl("T401", mass_2020$TRX_REC_AXIS_CD, fixed = TRUE) | grepl("T402", mass_2020$TRX_REC_AXIS_CD, fixed = TRUE) | grepl("T403", mass_2020$TRX_REC_AXIS_CD, fixed = TRUE) | grepl("T404", mass_2020$TRX_REC_AXIS_CD, fixed = TRUE) | grepl("T406", mass_2020$TRX_REC_AXIS_CD, fixed = TRUE), 1, 0)
mass_2020 <- subset(mass_2020, opioid_detected == 1)

# prep this dataset for a merge
mass_2020_merge <- select(mass_2020, "DOD", "age", "Sex_revised", "Race", "TRX_CAUSE_ACME", "TRX_REC_AXIS_CD", "Weekday", "Month", "Year", "Race_sex", "fatal", "etoh", "pharma", "synthetic", "stimulant", "methadone")


### 2021

# import mass data
mass_2021 <- read.csv("mass_2021.csv")
# rename date of death variable to DOD
names(mass_2021)[names(mass_2021) == 'DOD_4_FD'] <- 'DOD'
# rename age variable to age
names(mass_2021)[names(mass_2021) == 'AGE1'] <- 'age'
# rename race variable to race
names(mass_2021)[names(mass_2021) == 'RACEGROUP'] <- 'Race'

# convert the DOD into a "date" data type so we can work with it
mass_2021$DOD = as.Date(mass_2021$DOD, "%m/%d/%Y")
# add in day of week -- from https://stackoverflow.com/questions/9216138/find-the-day-of-a-week
mass_2021$Weekday <- weekdays(mass_2021$DOD)
# add in month -- from https://stackoverflow.com/questions/22603847/how-to-extract-month-from-date-in-r
mass_2021$Month <- month(mass_2021$DOD)
# add in year -- we will use this to split into old/young. Not the most elegant solution but whatever
mass_2021$Year <- year(mass_2021$DOD)
# remove anyone with a non-ma injury state
mass_2021 <- subset(mass_2021, INJRY_STATE == "MASSACHUSETTS")
# create new sex variable with more descriptive catergories
mass_2021$Sex_revised <- ifelse(mass_2021$SEX == "M", "Male", "Female")
# convert all NA values to 0
mass_2021[is.na(mass_2021)] <- 0
# make first letter of race capitalized
mass_2021$Race <- str_to_title(mass_2021$Race)
# collapse the race categories and standardize them
mass_2021$Race <- ifelse(mass_2021$Race == " White Nh", "White", ifelse(mass_2021$Race == " Black Nh", "Black", ifelse(mass_2021$Race == " Hispanic", "Hispanic", "Other")))
# concatenate the race and sex variables, so "Black" and "female" becomes "Black female"
mass_2021$Race_sex <- paste(mass_2021$Race, mass_2021$Sex_revised, sep = " ")
# indicator variable to aggregate
mass_2021$fatal <- 1
# subset out non-accidental/undetermined deaths
mass_2021 <- subset(mass_2021, TRX_CAUSE_ACME %in% c("X40", "X41", "X42", "X43", "X44", "Y10", "Y11", "Y12", "Y13", "Y14"))
# ID different substance categories -- from https://stackoverflow.com/questions/10128617/test-if-characters-are-in-a-string
mass_2021$etoh <- ifelse(grepl("T510", mass_2021$TRX_REC_AXIS_CD, fixed = TRUE), 1, 0)
mass_2021$pharma <- ifelse(grepl("T402", mass_2021$TRX_REC_AXIS_CD, fixed = TRUE), 1, 0)
mass_2021$synthetic <- ifelse(grepl("T404", mass_2021$TRX_REC_AXIS_CD, fixed = TRUE), 1, 0)
#mass_2021$stimulant <- ifelse(grepl("T405", mass_2021$TRX_REC_AXIS_CD, fixed = TRUE), 1, 0)
mass_2021$methadone <- ifelse(grepl("T403", mass_2021$TRX_REC_AXIS_CD, fixed = TRUE), 1, 0)
mass_2021$stimulant <- ifelse(grepl("T405", mass_2021$TRX_REC_AXIS_CD, fixed = TRUE), 1, ifelse(grepl("T436", mass_2021$TRX_REC_AXIS_CD, fixed = TRUE), 1, 0))
# get rid of non-MA ODs
mass_2021 <- subset(mass_2021, INJRY_STATE == "MASSACHUSETTS")
# set ages between 18-80
mass_2021 <- subset(mass_2021, age > 17 & age < 81)
# subset for opioid-detected deaths
# this is easiest to do by creating an opioid-detected variable first
mass_2021$opioid_detected <- ifelse(grepl("T400", mass_2021$TRX_REC_AXIS_CD, fixed = TRUE) | grepl("T401", mass_2021$TRX_REC_AXIS_CD, fixed = TRUE) | grepl("T402", mass_2021$TRX_REC_AXIS_CD, fixed = TRUE) | grepl("T403", mass_2021$TRX_REC_AXIS_CD, fixed = TRUE) | grepl("T404", mass_2021$TRX_REC_AXIS_CD, fixed = TRUE) | grepl("T406", mass_2021$TRX_REC_AXIS_CD, fixed = TRUE), 1, 0)
mass_2021 <- subset(mass_2021, opioid_detected == 1)

# prep this dataset for a merge so we can construct the polysubstance era
mass_2021_merge <- select(mass_2021, "DOD", "age", "Sex_revised", "Race", "TRX_CAUSE_ACME", "TRX_REC_AXIS_CD", "Weekday", "Month", "Year", "Race_sex", "fatal", "etoh", "pharma", "synthetic", "stimulant", "methadone")


### 2022

# import mass data
mass_2022 <- read.csv("mass_2022.csv")
# rename date of death variable to DOD
names(mass_2022)[names(mass_2022) == 'DOD_4_FD'] <- 'DOD'
# rename age variable to age
names(mass_2022)[names(mass_2022) == 'AGE1'] <- 'age'
# rename race variable to race
names(mass_2022)[names(mass_2022) == 'RACEGROUP'] <- 'Race'

# convert the DOD into a "date" data type so we can work with it
mass_2022$DOD = as.Date(mass_2022$DOD, "%m/%d/%Y")
# add in day of week -- from https://stackoverflow.com/questions/9216138/find-the-day-of-a-week
mass_2022$Weekday <- weekdays(mass_2022$DOD)
# add in month -- from https://stackoverflow.com/questions/22603847/how-to-extract-month-from-date-in-r
mass_2022$Month <- month(mass_2022$DOD)
# add in year -- we will use this to split into old/young. Not the most elegant solution but whatever
mass_2022$Year <- year(mass_2022$DOD)
# remove anyone with a non-ma injury state
mass_2022 <- subset(mass_2022, INJRY_STATE == "MASSACHUSETTS")
# create new sex variable with more descriptive catergories
mass_2022$Sex_revised <- ifelse(mass_2022$SEX == "M", "Male", "Female")
# convert all NA values to 0
mass_2022[is.na(mass_2022)] <- 0
# make first letter of race capitalized
mass_2022$Race <- str_to_title(mass_2022$Race)
# collapse the race categories and standardize them
mass_2022$Race <- ifelse(mass_2022$Race == " White Nh", "White", ifelse(mass_2022$Race == " Black Nh", "Black", ifelse(mass_2022$Race == " Hispanic", "Hispanic", "Other")))
# concatenate the race and sex variables, so "Black" and "female" becomes "Black female"
mass_2022$Race_sex <- paste(mass_2022$Race, mass_2022$Sex_revised, sep = " ")
# indicator variable to aggregate
mass_2022$fatal <- 1
# subset out non-accidental/undetermined deaths
mass_2022 <- subset(mass_2022, TRX_CAUSE_ACME %in% c("X40", "X41", "X42", "X43", "X44", "Y10", "Y11", "Y12", "Y13", "Y14"))
# ID different substance categories -- from https://stackoverflow.com/questions/10128617/test-if-characters-are-in-a-string
mass_2022$etoh <- ifelse(grepl("T510", mass_2022$TRX_REC_AXIS_CD, fixed = TRUE), 1, 0)
mass_2022$pharma <- ifelse(grepl("T402", mass_2022$TRX_REC_AXIS_CD, fixed = TRUE), 1, 0)
mass_2022$synthetic <- ifelse(grepl("T404", mass_2022$TRX_REC_AXIS_CD, fixed = TRUE), 1, 0)
#mass_2022$stimulant <- ifelse(grepl("T405", mass_2022$TRX_REC_AXIS_CD, fixed = TRUE), 1, 0)
mass_2022$methadone <- ifelse(grepl("T403", mass_2022$TRX_REC_AXIS_CD, fixed = TRUE), 1, 0)
mass_2022$stimulant <- ifelse(grepl("T405", mass_2022$TRX_REC_AXIS_CD, fixed = TRUE), 1, ifelse(grepl("T436", mass_2022$TRX_REC_AXIS_CD, fixed = TRUE), 1, 0))
# get rid of non-MA ODs
mass_2022 <- subset(mass_2022, INJRY_STATE == "MASSACHUSETTS")
# set ages between 18-80
mass_2022 <- subset(mass_2022, age > 17 & age < 81)
# subset for opioid-detected deaths
# this is easiest to do by creating an opioid-detected variable first
mass_2022$opioid_detected <- ifelse(grepl("T400", mass_2022$TRX_REC_AXIS_CD, fixed = TRUE) | grepl("T401", mass_2022$TRX_REC_AXIS_CD, fixed = TRUE) | grepl("T402", mass_2022$TRX_REC_AXIS_CD, fixed = TRUE) | grepl("T403", mass_2022$TRX_REC_AXIS_CD, fixed = TRUE) | grepl("T404", mass_2022$TRX_REC_AXIS_CD, fixed = TRUE) | grepl("T406", mass_2022$TRX_REC_AXIS_CD, fixed = TRUE), 1, 0)
mass_2022 <- subset(mass_2022, opioid_detected == 1)

# prep this dataset for a merge so we can construct the polysubstance era
mass_2022_merge <- select(mass_2022, "DOD", "age", "Sex_revised", "Race", "TRX_CAUSE_ACME", "TRX_REC_AXIS_CD", "Weekday", "Month", "Year", "Race_sex", "fatal", "etoh", "pharma", "synthetic", "stimulant", "methadone")


### 2023

# import mass data
mass_2023 <- read.csv("mass_2023.csv")
# rename date of death variable to DOD
names(mass_2023)[names(mass_2023) == 'DOD_4_FD'] <- 'DOD'
# rename age variable to age
names(mass_2023)[names(mass_2023) == 'AGE1'] <- 'age'
# rename race variable to race
names(mass_2023)[names(mass_2023) == 'RACEGROUP'] <- 'Race'

# convert the DOD into a "date" data type so we can work with it
mass_2023$DOD = as.Date(mass_2023$DOD, "%m/%d/%Y")
# add in day of week -- from https://stackoverflow.com/questions/9216138/find-the-day-of-a-week
mass_2023$Weekday <- weekdays(mass_2023$DOD)
# add in month -- from https://stackoverflow.com/questions/22603847/how-to-extract-month-from-date-in-r
mass_2023$Month <- month(mass_2023$DOD)
# add in year -- we will use this to split into old/young. Not the most elegant solution but whatever
mass_2023$Year <- year(mass_2023$DOD)
# remove anyone with a non-ma injury state
mass_2023 <- subset(mass_2023, INJRY_STATE == "MASSACHUSETTS")
# create new sex variable with more descriptive catergories
mass_2023$Sex_revised <- ifelse(mass_2023$SEX == "M", "Male", "Female")
# convert all NA values to 0
mass_2023[is.na(mass_2023)] <- 0
# make first letter of race capitalized
mass_2023$Race <- str_to_title(mass_2023$Race)
# collapse the race categories and standardize them
mass_2023$Race <- ifelse(mass_2023$Race == " White Nh", "White", ifelse(mass_2023$Race == " Black Nh", "Black", ifelse(mass_2023$Race == " Hispanic", "Hispanic", "Other")))
# concatenate the race and sex variables, so "Black" and "female" becomes "Black female"
mass_2023$Race_sex <- paste(mass_2023$Race, mass_2023$Sex_revised, sep = " ")
# indicator variable to aggregate
mass_2023$fatal <- 1
# subset out non-accidental/undetermined deaths
mass_2023 <- subset(mass_2023, TRX_CAUSE_ACME %in% c("X40", "X41", "X42", "X43", "X44", "Y10", "Y11", "Y12", "Y13", "Y14"))
# ID different substance categories -- from https://stackoverflow.com/questions/10128617/test-if-characters-are-in-a-string
mass_2023$etoh <- ifelse(grepl("T510", mass_2023$TRX_REC_AXIS_CD, fixed = TRUE), 1, 0)
mass_2023$pharma <- ifelse(grepl("T402", mass_2023$TRX_REC_AXIS_CD, fixed = TRUE), 1, 0)
mass_2023$synthetic <- ifelse(grepl("T404", mass_2023$TRX_REC_AXIS_CD, fixed = TRUE), 1, 0)
#mass_2023$stimulant <- ifelse(grepl("T405", mass_2023$TRX_REC_AXIS_CD, fixed = TRUE), 1, 0)
mass_2023$methadone <- ifelse(grepl("T403", mass_2023$TRX_REC_AXIS_CD, fixed = TRUE), 1, 0)
mass_2023$stimulant <- ifelse(grepl("T405", mass_2023$TRX_REC_AXIS_CD, fixed = TRUE), 1, ifelse(grepl("T436", mass_2023$TRX_REC_AXIS_CD, fixed = TRUE), 1, 0))
# get rid of non-MA ODs
mass_2023 <- subset(mass_2023, INJRY_STATE == "MASSACHUSETTS")
# set ages between 18-80
mass_2023 <- subset(mass_2023, age > 17 & age < 81)
# subset for opioid-detected deaths
# this is easiest to do by creating an opioid-detected variable first
mass_2023$opioid_detected <- ifelse(grepl("T400", mass_2023$TRX_REC_AXIS_CD, fixed = TRUE) | grepl("T401", mass_2023$TRX_REC_AXIS_CD, fixed = TRUE) | grepl("T402", mass_2023$TRX_REC_AXIS_CD, fixed = TRUE) | grepl("T403", mass_2023$TRX_REC_AXIS_CD, fixed = TRUE) | grepl("T404", mass_2023$TRX_REC_AXIS_CD, fixed = TRUE) | grepl("T406", mass_2023$TRX_REC_AXIS_CD, fixed = TRUE), 1, 0)
mass_2023 <- subset(mass_2023, opioid_detected == 1)

# prep this dataset for a merge so we can construct the polysubstance era
mass_2023_merge <- select(mass_2023, "DOD", "age", "Sex_revised", "Race", "TRX_CAUSE_ACME", "TRX_REC_AXIS_CD", "Weekday", "Month", "Year", "Race_sex", "fatal", "etoh", "pharma", "synthetic", "stimulant", "methadone")

# polysubstance era
polysubstance_era <-rbind(mass_2020_merge, mass_2021_merge, mass_2022_merge, mass_2023_merge)
# create a variable that tells us which era we're in, so we can use it for plotting
polysubstance_era$Era <- rep("Polysubstance", times = nrow(polysubstance_era))

# create a dataset with all eras
all_eras <- rbind(pharma_era, heroin_era, fent_era, polysubstance_era)

# export files
write.csv(mass_old_merge, file="processed_data/mass_old_processed.csv")
write.csv(mass_2018, file="processed_data/mass_2018_processed.csv")
write.csv(mass_2019, file="processed_data/mass_2019_processed.csv")
write.csv(mass_2020, file="processed_data/mass_2020_processed.csv")
write.csv(mass_2021, file="processed_data/mass_2021_processed.csv")
write.csv(mass_2022, file="processed_data/mass_2022_processed.csv")
write.csv(mass_2023, file="processed_data/mass_2023_processed.csv")
write.csv(pharma_era, file="processed_data/mass_pharma_era_processed.csv")
write.csv(heroin_era, file="processed_data/mass_heroin_era_processed.csv")
write.csv(fent_era, file="processed_data/mass_fent_era_processed.csv")
write.csv(polysubstance_era, file="processed_data/mass_polysubstance_era_processed.csv")
write.csv(all_eras, file="processed_data/mass_all_eras_processed.csv")