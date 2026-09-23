# I am preprocessing NEMSIS data to do the following: 
#   * Remove duplicates
#   * Remove fatalities
#   * Format the date of injury and extract the year
#   * Add a 0 to the beginning of the zip codes [06511 v. 6511]
#   * Removing any entries where injury does not occur in CT
###################################################################

# set working directory -- if you don't do this the relative paths won't work
setwd("C:\\Users\\ar2529\\Desktop\\overdose_age\\data_preprocessing")

library(tidyverse) # to replace NA with something else
library(lubridate) # to fix the date formatting
library(dplyr)
library(tidygeocoder)
library(zipcodeR)

# import SWORD file
sword <- read.csv("sword.csv")

# convert all NA values to 0
sword[is.na(sword)] <- 0

# rename the columns because the original names had weird characters
colnames(sword) <- c("Date", "Fatal", "Gender", "Age", "Case_Number", "Lat", "Long", "State", "County", "Zip", "Nlx_Admin", "Hospital_Transport")

# remove any entries where the OD did not occur in CT
sword <- subset(sword, State=="CT")

# subset out ages less than 18 and greater than 80 [also removes any NA]
sword <- subset(sword, Age > 17 & Age < 81)

# only keep unique rows [remove duplicates], from https://www.datanovia.com/en/lessons/identify-and-remove-duplicate-data-in-r/
sword <- unique(sword)

# subset out fatals, from https://stackoverflow.com/questions/57422048/subselect-only-those-rows-that-contain-a-specific-word-on-specific-column
sword <- sword[grepl("Non-Fatal", sword$Fatal),]

# extract year and make it into its own column

# get rid of the time, from https://stackoverflow.com/questions/57422048/subselect-only-those-rows-that-contain-a-specific-word-on-specific-column
sword$Day <- sub(" .*", "", sword$Date)

# convert this string into a date data type so we can extract the year from it
sword$Day <- as.Date(sword$Day, "%m/%d/%Y")

# extract year from this, from https://stackoverflow.com/questions/36568070/extract-year-from-date
sword$Year <- format(sword$Day,'%Y')

# add in month, because for some reason this dataset doesn't have it
sword$Month <- month(sword$Day)

# add in day of week
sword$Weekday <- weekdays(sword$Day)

########################################################
# getting cities from geocoordinates
########################################################

# add a 0 in front of the zip codes so I can use them to get city locations
sword$Zip <- paste0("0", sword$Zip)

# reverse geocode any addresses that have a NA for the zip
nas <- subset(sword, Zip == "0NA")

# only perform this if there are NAs [from https://stackoverflow.com/questions/35366187/how-to-write-if-else-statements-if-dataframe-is-empty-in-r]: 

if (dim(nas)[1] != 0) {
  # from https://stackoverflow.com/questions/42319993/listing-cities-from-coordinates
  reverse <- nas %>%
    reverse_geocode(lat = Lat, long = Long, method = 'osm',
                    address = Address, full_results = TRUE) 
  
  # replace the zip code with whatever the reverse geocoding came up with so it's not NA
  sword$Zip <- ifelse(sword$Zip == "0NA", reverse$postcode, sword$Zip)  
}


# do a reverse zip lookup for towns 
reversezip <- reverse_zipcode(sword$Zip)

# rename the zipcode column so we can do a merge
names(reversezip)[names(reversezip) == 'zipcode'] <- 'Zip'

# merge with sword, from https://stackoverflow.com/questions/51268772/combine-dataframe-based-on-value-in-row-in-r
sword <- merge(sword, reversezip, by = "Zip", all.x=TRUE, all.y=TRUE)

# rename the major_city field to City, from https://stackoverflow.com/questions/7531868/how-to-rename-a-single-column-in-a-data-frame
names(sword)[names(sword) == 'major_city'] <- 'City'

# order the df in ascending order of date, because the reversezip function orders it based on city
# from https://stackoverflow.com/questions/6246159/how-to-sort-a-data-frame-by-date

sword <- sword[order(as.Date(sword$Day, format="%m/%d/%Y")),]
####################################################################################

# make an aggregator variable, for the city graphs
sword$Nonfatal <- 1

# make a new sex variable that compresses blanks into unknown
sword$Sex_revised <- ifelse(sword$Gender == "", "Unknown", sword$Gender)

# create a new file that only has the columns we need -- day, year, gender [maybe?], age, lat, long, county, zip
sword_processed <- sword %>% select(Gender, Sex_revised, Age, Lat, Long, City, County, Zip, Day, Year, Month, Weekday, Nonfatal)

# export file
write.csv(sword_processed, file="processed_data/sword_processed.csv")