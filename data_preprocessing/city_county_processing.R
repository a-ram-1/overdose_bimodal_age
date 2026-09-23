######################################################################
# This is basic preprocessing of city/county data to do the following:
#   * Convert population numbers to numeric data type
#   * Reformat data to more easily manipulate
######################################################################

# set working directory -- if you don't do this the relative paths won't work
setwd("C:\\Users\\ar2529\\Desktop\\overdose_age\\data_preprocessing")


library(tigris)
library(ggplot2)
library(stringr)
library(dplyr)
library(lubridate)
library(patchwork)
library(tidyverse) #to replace NA with something else
library(sf)

# adding in county/city population data from https://portal.ct.gov/DPH/Health-Information-Systems--Reporting/Population/Annual-Town-and-County-Population-for-Connecticut -- I converted to csv and isolated county/city in separate files

county_pop_2018 <- read.csv("pop_files/county_pop_2018.csv")
# I think the population numbers will be read as strings, so convert them to numbers so we can manipulate
# the commas were causing NAs when converting to numeric, solution to remove from https://stackoverflow.com/questions/57656023/as-numeric-returns-na-for-no-apparent-reason-for-some-of-the-values-in-a-column
county_pop_2018$Pop <- as.numeric(gsub(",","",county_pop_2018$Pop))

county_pop_2019 <- read.csv("pop_files/county_pop_2019.csv")
# I think the population numbers will be read as strings, so convert them to numbers so we can manipulate
county_pop_2019$Pop <- as.numeric(gsub(",","",county_pop_2019$Pop)) 

county_pop_2020 <- read.csv("pop_files/county_pop_2020.csv")
# I think the population numbers will be read as strings, so convert them to numbers so we can manipulate
county_pop_2020$Pop <- as.numeric(gsub(",","",county_pop_2020$Pop))

county_pop_2021 <- read.csv("pop_files/county_pop_2021.csv")
# I think the population numbers will be read as strings, so convert them to numbers so we can manipulate
county_pop_2021$Pop <- as.numeric(gsub(",","",county_pop_2021$Pop))

# for 2022 I had to manually calculate the county-level population estimates using [this](https://www1.ctdol.state.ct.us/lmi/misc/counties.asp) since CT had switched to reporting "planning region" estimates instead of county. I will continue using county for the sake of consistency
county_pop_2022 <- read.csv("pop_files/county_pop_2022.csv")
# I think the population numbers will be read as strings, so convert them to numbers so we can manipulate
county_pop_2022$Pop <- as.numeric(gsub(",","",county_pop_2022$Pop))

# for 2023 I had to manually calculate the county-level population estimates using [this](https://www1.ctdol.state.ct.us/lmi/misc/counties.asp) since CT had switched to reporting "planning region" estimates instead of county. I will continue using county for the sake of consistency
county_pop_2023 <- read.csv("pop_files/county_pop_2023.csv")
# I think the population numbers will be read as strings, so convert them to numbers so we can manipulate
county_pop_2023$Pop <- as.numeric(gsub(",","",county_pop_2023$Pop))


city_pop_2018 <- read.csv("pop_files/city_pop_2018.csv")
# I think the population numbers will be read as strings, so convert them to numbers so we can manipulate
city_pop_2018$Pop <- as.numeric(gsub(",","",city_pop_2018$Pop))

city_pop_2019 <- read.csv("pop_files/city_pop_2019.csv")
# I think the population numbers will be read as strings, so convert them to numbers so we can manipulate
city_pop_2019$Pop <- as.numeric(gsub(",","",city_pop_2019$Pop))

city_pop_2020 <- read.csv("pop_files/city_pop_2020.csv")
# I think the population numbers will be read as strings, so convert them to numbers so we can manipulate
city_pop_2020$Pop <- as.numeric(gsub(",","",city_pop_2020$Pop))

city_pop_2021 <- read.csv("pop_files/city_pop_2021.csv")
# I think the population numbers will be read as strings, so convert them to numbers so we can manipulate
city_pop_2021$Pop <- as.numeric(gsub(",","",city_pop_2021$Pop))

city_pop_2022 <- read.csv("pop_files/city_pop_2022.csv")
# I think the population numbers will be read as strings, so convert them to numbers so we can manipulate
city_pop_2022$Pop <- as.numeric(gsub(",","",city_pop_2022$Pop))

city_pop_2023 <- read.csv("pop_files/city_pop_2023.csv")
# I think the population numbers will be read as strings, so convert them to numbers so we can manipulate
city_pop_2023$Pop <- as.numeric(gsub(",","",city_pop_2023$Pop))

# export files
write.csv(county_pop_2018, file="processed_data/county_pop_2018_processed.csv")
write.csv(county_pop_2019, file="processed_data/county_pop_2019_processed.csv")
write.csv(county_pop_2020, file="processed_data/county_pop_2020_processed.csv")
write.csv(county_pop_2021, file="processed_data/county_pop_2021_processed.csv")
write.csv(county_pop_2022, file="processed_data/county_pop_2022_processed.csv")
write.csv(county_pop_2023, file="processed_data/county_pop_2023_processed.csv")

write.csv(city_pop_2018, file="processed_data/city_pop_2018_processed.csv")
write.csv(city_pop_2019, file="processed_data/city_pop_2019_processed.csv")
write.csv(city_pop_2020, file="processed_data/city_pop_2020_processed.csv")
write.csv(city_pop_2021, file="processed_data/city_pop_2021_processed.csv")
write.csv(city_pop_2022, file="processed_data/city_pop_2022_processed.csv")
write.csv(city_pop_2023, file="processed_data/city_pop_2023_processed.csv")
