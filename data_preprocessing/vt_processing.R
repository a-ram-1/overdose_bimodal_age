####################################################################################
# This is basic preprocessing of VT data to do the following:
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
vt <- read.csv("vt_2009_2023.csv")

# create DOD variable from DOD, MOD and YOD
# we don't need the current DOD variable so let's just repurpose it
names(vt)[names(vt) == 'DOD'] <- 'Day'
vt$DOD <- ifelse(vt$Day =="99", "Unknown", paste(vt$MOD, vt$Day, vt$YOD, sep="/"))

# convert to date type -- some are NA which is fine
vt$DOD = as.Date(mdy(vt$DOD), "%m/%d/%y")
# that seems to solve the problem of unknown dates for now, all the functions will work with a NA and I can still produce all the graphs I want

# get the race categories to match CT/MA
vt$Race <- ifelse(vt$RACE01 == 1, "White", ifelse(vt$RACE02 == 1, "Black", ifelse((vt$ETHNIC2 == 1 | vt$ETHNIC3 == 1 | vt$ETHNIC4 == 1 | vt$ETHNIC5 == 1 | vt$ETHNIC6 == 1 ), "Hispanic", "Other")))

# this leaves us with some NAs, so we'll switch those to other
vt$Race[is.na(vt$Race)] <- "Other"

# make the sex variable more descriptive
vt$Sex_revised <- ifelse(vt$SEX == 1, "Male", "Female")

# make a series of tox fields -- we'll only focus on the fields that I've used in CT. In addition to being easier to parse the text fields, unlike ICD codes, will tell me exact substances

# fent 
# from https://stackoverflow.com/questions/10128617/test-if-characters-are-in-a-string
vt$fentonly <- ifelse((grepl("fentanyl", vt$CAUSEA, fixed = TRUE) | grepl("Fentanyl", vt$CAUSEA, fixed = TRUE) | grepl("fentanyl", vt$CAUSEB, fixed = TRUE) | grepl("Fentanyl", vt$CAUSEB, fixed = TRUE) | grepl("fentanyl", vt$CAUSEC, fixed = TRUE) | grepl("Fentanyl", vt$CAUSEC, fixed = TRUE) | grepl("fentanyl", vt$CAUSED, fixed = TRUE) | grepl("Fentanyl", vt$CAUSED, fixed = TRUE)), 1, 0)

# cocaine
vt$cocaine <- ifelse((grepl("cocaine", vt$CAUSEA, fixed = TRUE) | grepl("Cocaine", vt$CAUSEA, fixed = TRUE) | grepl("cocaine", vt$CAUSEB, fixed = TRUE) | grepl("Cocaine", vt$CAUSEB, fixed = TRUE) | grepl("cocaine", vt$CAUSEC, fixed = TRUE) | grepl("Cocaine", vt$CAUSEC, fixed = TRUE) | grepl("cocaine", vt$CAUSED, fixed = TRUE) | grepl("Cocaine", vt$CAUSED, fixed = TRUE) | grepl("methamphetamine", vt$CAUSEA, fixed = TRUE) | grepl("Methamphetamine", vt$CAUSEA, fixed = TRUE) | grepl("methamphetamine", vt$CAUSEB, fixed = TRUE) | grepl("Methamphetamine", vt$CAUSEB, fixed = TRUE) | grepl("methamphetamine", vt$CAUSEC, fixed = TRUE) | grepl("Methamphetamine", vt$CAUSEC, fixed = TRUE) | grepl("methamphetamine", vt$CAUSED, fixed = TRUE) | grepl("Methamphetamine", vt$CAUSED, fixed = TRUE) | grepl("amphetamine", vt$CAUSEA, fixed = TRUE) | grepl("Amphetamine", vt$CAUSEA, fixed = TRUE) | grepl("amphetamine", vt$CAUSEB, fixed = TRUE) | grepl("Amphetamine", vt$CAUSEB, fixed = TRUE) | grepl("amphetamine", vt$CAUSEC, fixed = TRUE) | grepl("Amphetamine", vt$CAUSEC, fixed = TRUE) | grepl("amphetamine", vt$CAUSED, fixed = TRUE) | grepl("Amphetamine", vt$CAUSED, fixed = TRUE)), 1, 0)

# gabapentin
vt$gabapentin <- ifelse((grepl("gabapentin", vt$CAUSEA, fixed = TRUE) | grepl("Gabapentin", vt$CAUSEA, fixed = TRUE) | grepl("gabapentin", vt$CAUSEB, fixed = TRUE) | grepl("Gabapentin", vt$CAUSEB, fixed = TRUE) | grepl("gabapentin", vt$CAUSEC, fixed = TRUE) | grepl("Gabapentin", vt$CAUSEC, fixed = TRUE) | grepl("gabapentin", vt$CAUSED, fixed = TRUE) | grepl("Gabapentin", vt$CAUSED, fixed = TRUE)), 1, 0)

# xylazine
vt$xylazine <- ifelse((grepl("xylazine", vt$CAUSEA, fixed = TRUE) | grepl("Xylazine", vt$CAUSEA, fixed = TRUE) | grepl("xylazine", vt$CAUSEB, fixed = TRUE) | grepl("Xylazine", vt$CAUSEB, fixed = TRUE) | grepl("xylazine", vt$CAUSEC, fixed = TRUE) | grepl("Xylazine", vt$CAUSEC, fixed = TRUE) | grepl("xylazine", vt$CAUSED, fixed = TRUE) | grepl("Xylazine", vt$CAUSED, fixed = TRUE)), 1, 0)

# alcohol -- seems to be denoted by alcohol and ethanol
vt$etoh <- ifelse((grepl("ethanol", vt$CAUSEA, fixed = TRUE) | grepl("Ethanol", vt$CAUSEA, fixed = TRUE) | grepl("ethanol", vt$CAUSEB, fixed = TRUE) | grepl("Ethanol", vt$CAUSEB, fixed = TRUE) | grepl("ethanol", vt$CAUSEC, fixed = TRUE) | grepl("Ethanol", vt$CAUSEC, fixed = TRUE) | grepl("ethanol", vt$CAUSED, fixed = TRUE) | grepl("Ethanol", vt$CAUSED, fixed = TRUE) | grepl("alcohol", vt$CAUSEA, fixed = TRUE) | grepl("Alcohol", vt$CAUSEA, fixed = TRUE) | grepl("alcohol", vt$CAUSEB, fixed = TRUE) | grepl("Alcohol", vt$CAUSEB, fixed = TRUE) | grepl("alcohol", vt$CAUSEC, fixed = TRUE) | grepl("Alcohol", vt$CAUSEC, fixed = TRUE) | grepl("alcohol", vt$CAUSED, fixed = TRUE) | grepl("Alcohol", vt$CAUSED, fixed = TRUE)), 1, 0)

# bupe
vt$bupe <- ifelse((grepl("buprenorphine", vt$CAUSEA, fixed = TRUE) | grepl("Buprenorphine", vt$CAUSEA, fixed = TRUE) | grepl("buprenorphine", vt$CAUSEB, fixed = TRUE) | grepl("Buprenorphine", vt$CAUSEB, fixed = TRUE) | grepl("buprenorphine", vt$CAUSEC, fixed = TRUE) | grepl("Buprenorphine", vt$CAUSEC, fixed = TRUE) | grepl("buprenorphine", vt$CAUSED, fixed = TRUE) | grepl("Buprenorphine", vt$CAUSED, fixed = TRUE)), 1, 0)

# methadone
vt$methadone <- ifelse((grepl("methadone", vt$CAUSEA, fixed = TRUE) | grepl("Methadone", vt$CAUSEA, fixed = TRUE) | grepl("methadone", vt$CAUSEB, fixed = TRUE) | grepl("Methadone", vt$CAUSEB, fixed = TRUE) | grepl("methadone", vt$CAUSEC, fixed = TRUE) | grepl("Methadone", vt$CAUSEC, fixed = TRUE) | grepl("methadone", vt$CAUSED, fixed = TRUE) | grepl("Methadone", vt$CAUSED, fixed = TRUE)), 1, 0)

# pharma -- I need to use ICD for this because Lauretta uses her pharma category as a catch-all for any pharma
# get one COD field so I can work with it more easily -- from https://stackoverflow.com/questions/18115550/combine-two-or-more-columns-in-a-dataframe-into-a-new-column-with-a-new-name

vt$cod <- paste(vt$CAUSE01, vt$CAUSE02, vt$CAUSE03, vt$CAUSE04, vt$CAUSE05, vt$CAUSE06, vt$CAUSE07, vt$CAUSE08, vt$CAUSE09, vt$CAUSE10, vt$CAUSE11, vt$CAUSE12, vt$CAUSE13, vt$CAUSE14, vt$CAUSE15, vt$CAUSE16, vt$CAUSE17, vt$CAUSE18, vt$CAUSE19, vt$CAUSE20)
vt$pharma <- ifelse(grepl("T402", vt$cod, fixed = TRUE), 1, 0)

# get rid of anyone who died for a non-accidental/undetermined reason--CAUSE01 has the underlying COD
vt <- subset(vt, CAUSE01 %in% c("X40", "X41", "X42", "X43", "X44", "Y10", "Y11", "Y12", "Y13", "Y14"))

# subset for opioid-detected deaths
# this is easiest to do by creating an opioid-detected variable first

vt$opioid_detected <- ifelse(grepl("T400", vt$cod, fixed = TRUE) | grepl("T401", vt$cod, fixed = TRUE) | grepl("T402", vt$cod, fixed = TRUE) | grepl("T403", vt$cod, fixed = TRUE) | grepl("T404", vt$cod, fixed = TRUE) | grepl("T406", vt$cod, fixed = TRUE), 1, 0)
vt <- subset(vt, opioid_detected == 1)

# only grab the columns that are going to be useful
vt_processed <- select(vt, "YOD", "MOD", "DOD", "AGE_YR", "Sex_revised", "Race", "fentonly", "cocaine", "gabapentin", "xylazine", "etoh", "bupe", "methadone", "pharma") 

# rename variables to be similar to those from other states
names(vt_processed)[names(vt_processed) == 'AGE_YR'] <- 'age'
names(vt_processed)[names(vt_processed) == 'MOD'] <- 'Month'
names(vt_processed)[names(vt_processed) == 'YOD'] <- 'Year'

# set ages between 18-80
vt_processed <- subset(vt_processed, age > 17 & age < 81)

# remove NAs from age -- there's just one in 2022 -- from https://stackoverflow.com/questions/11254524/omit-rows-containing-specific-column-of-na
vt_processed <- vt_processed[!is.na(vt_processed$age),]

# concatenate the race and sex variables, so "Black" and "female" becomes "Black female"
vt_processed$Race_sex <- paste(vt_processed$Race, vt_processed$Sex_revised, sep = " ")

# add in day of week -- from https://stackoverflow.com/questions/9216138/find-the-day-of-a-week
vt_processed$Weekday <- weekdays(vt_processed$DOD)

# convert all NA values in weekday to 0
vt_processed$Weekday[is.na(vt_processed$Weekday)] <- "Unknown"

# indicator variable to aggregate
vt_processed$fatal <- 1

# create subsets of the dataset for each year

vt_2009 <- subset(vt_processed, Year == "2009")
vt_2010 <- subset(vt_processed, Year == "2010")
vt_2011 <- subset(vt_processed, Year == "2011")
vt_2012 <- subset(vt_processed, Year == "2012")
vt_2013 <- subset(vt_processed, Year == "2013")
vt_2014 <- subset(vt_processed, Year == "2014")
vt_2015 <- subset(vt_processed, Year == "2015")
vt_2016 <- subset(vt_processed, Year == "2016")
vt_2017 <- subset(vt_processed, Year == "2017")
vt_2018 <- subset(vt_processed, Year == "2018")
vt_2019 <- subset(vt_processed, Year == "2019")
vt_2020 <- subset(vt_processed, Year == "2020")
vt_2021 <- subset(vt_processed, Year == "2021")
vt_2022 <- subset(vt_processed, Year == "2022")
vt_2023 <- subset(vt_processed, Year == "2023")

# create variables for the different eras
pharma_era <- subset(vt_processed, Year %in% c("2009", "2010", "2011", "2012", "2013"))
pharma_era$Era <- rep("Pharma", times = nrow(pharma_era))

heroin_era <- subset(vt_processed, Year %in% c("2014", "2015", "2016", "2017"))
heroin_era$Era <- rep("Heroin", times = nrow(heroin_era))

fent_era <- subset(vt_processed, Year %in% c("2018", "2019", "2020"))
fent_era$Era <- rep("Fentanyl", times = nrow(fent_era))

polysubstance_era <- subset(vt_processed, Year %in% c("2021", "2022", "2023"))
polysubstance_era$Era <- rep("Polysubstance", times = nrow(polysubstance_era))

# create a dataset with all eras
all_eras <- rbind(pharma_era, heroin_era, fent_era, polysubstance_era)

# export files
write.csv(vt_processed, file="processed_data/vt_processed.csv")
write.csv(pharma_era, file="processed_data/vt_pharma_era_processed.csv")
write.csv(heroin_era, file="processed_data/vt_heroin_era_processed.csv")
write.csv(fent_era, file="processed_data/vt_fent_era_processed.csv")
write.csv(polysubstance_era, file="processed_data/vt_polysubstance_era_processed.csv")
write.csv(all_eras, file="processed_data/vt_all_eras_processed.csv")
