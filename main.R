# Midterm - Bay Area Bike Rental Operation Research Project
# Bilal Mohideen - BTC1855H
# RStudio Version 2024.04.1+748 (2024.04.1+748)

#### Libraries/Datasets ####

# loading the required packages for the project
library(funModeling)
library(Hmisc)
library(tidyverse)
library(lubridate)
library(corrplot)

# importing the datasets in as data frames
station <- read.csv("station.csv")
trip <- read.csv("trip.csv")
weather <- read.csv("weather.csv")

#### EDA - Overview ####

# performing exploratory data analysis (EDA) for each dataset separately
# following template outlined here: 
# https://blog.datascienceheroes.com/exploratory-data-analysis-in-r-intro/
# data will be transformed during data cleaning/preparation stage
# this will be an informative analysis; therefore, the original datasets
# can be used (duplicating to create a copy is not necessary)

#### EDA - Station ####
# viewing overview and structure of "station" dataframe
# installation_date is in character form, can be converted to POSIX
# in next stage
glimpse(station)

# summary of the dataframe (quantity of zeros/NAs/infinite values, data 
# types, and unique values)
print(status(station))

# city is the only categorical variable that makes sense to analyze
# the others are all unique (names) or irrelevant to the project (installation
# date)
freq(station)
freq(station$city)

# counts are irrelevant for ID and longitude/latitude
# dock_count is the most relevant integer variable
plot_num(station)
print(profiling_num(station))

# descriptive statistics for the entire dataset
# provides info on missing and distinct values for each column
describe(station)

#### EDA - Trip ####
# viewing overview and structure of "trip" dataframe
# date variables (start_date and end_date) are in character form
glimpse(trip)

# summary of the dataframe (quantity of zeros/NAs/infinite values, data 
# types, and unique values)
# shows a number of zeros for the zip_code column
# IDs are all unique
print(status(trip))

# analysis for all categorical (character type) variables
# dates should not be included (not character type) - will be converted
# to POSIX in the next stage
# subscription_type frequency is clearly shown for subscribers and customers
# analysis for other variables is useful; however, there are too many unique
# values to consider, should only focus on top 10 highest frequencies for
# station names and zip codes
freq(trip)

# ID variables are classified as integer variables, but are not relevant for
# numerical variable analysis
# data profile is shown for duration (relevant numerical variable)
plot_num(trip)
print(profiling_num(trip))

# descriptive statistics for the entire dataset
# provides info on missing and distinct values for each column
# 1493 missing values for zip_code
describe(trip)

#### EDA - Weather ####
# viewing overview and structure of "weather" dataframe
# date variable is in character form
# precipitation_inches is in character form
glimpse(weather)

# summary of the dataframe (quantity of zeros/NAs/infinite values, data 
# types, and unique values)
# zeros in min_visibility_miles, mean_wind_speed_mph, precipitation_inches, 
# and cloud_cover 
# NAs in all the visibility variables and max_gust_speed_mph
print(status(weather))

# analysis for all categorical (character type) variables
# dates should not be included (not character type) - will be converted
# to POSIX in the next stage
# precipitation_inches should not be included as well
# analysis for events and city variables is useful
freq(weather)

# analysis for all numerical values in the dataset (temperature, visibility,
# wind speed, cloud cover)
# zip code included, but the analysis is irrelevant
plot_num(weather)
print(profiling_num(weather))

# descriptive statistics for the entire dataset
# provides info on missing and distinct values for each column
# 1473 missing values in events
# 451 missing values in max_gust_speed_mph
# 9 missing values in each of the visibility variables
describe(weather)

#### Data Cleaning - Overview ####
# will replace blank values with NA
# ensure that variable names/syntax are consistent across dataframes
# find the number of cancelled trips (duration less than 3 mins), record trip
# IDs, and remove from dataset
# establish outlier thresholds, find outlier values, record the associated
# trip IDs and remove from dataset
# copies of data created for data cleaning stage
station_v1 <- station
trip_v1 <- trip
weather_v1 <- weather

#### Data Cleaning - Station ####
# convert installation_date from character to POSIX format
station_v1$installation_date <- mdy(station_v1$installation_date, 
                                    tz = "UTC")

# EDA confirmed that there are not any NAs or blanks in the station dataset
# check for the count of duplicate rows
# all rows are distinct (returns FALSE)
any(duplicated(station_v1))

#### Data Cleaning - Trip ####
# replace blank values with NA in the zip_code column of dataset
# zip_code will not be used for downstream analysis, so rows with NA 
# do not need to be removed
trip_v1 <- trip_v1 %>%
  mutate(zip_code = na_if(zip_code, ""))

# check for the count of duplicate rows
# all rows are distinct (returns FALSE)
any(duplicated(trip_v1))

# convert start_date and end_date to POSIX format from character type
trip_v1$start_date <- mdy_hm(trip_v1$start_date, 
                             tz = "UTC")
trip_v1$end_date <- mdy_hm(trip_v1$end_date, 
                             tz = "UTC")

# recording trip IDs for cancelled trips (duration less than 3 mins)
# these trips would also have the exact same starting/ending station
# duration column is in seconds
# 3 minutes is equal to 3 x 60 = 180 seconds
cancelled_trips <- trip_v1$id[trip_v1$start_station_id 
                              == trip_v1$end_station_id
                              & trip_v1$duration < 180]

# filtering out rows with cancelled trips from the dataset
trip_v1 <- trip_v1 %>%
  filter(!(start_station_id == end_station_id 
           & duration < 180))

# trip IDs saved under cancelled_trips data frame
# save as a csv file for inclusion in the report
write.csv(cancelled_trips, "cancelled_trips.csv", row.names = F)

# from the EDA, the p_01 value is 128.0 and the p_99 value is 13311.62
# for the duration column (in seconds)
# however, trips shorter than 128 seconds (approx. 2 mins) or longer than 
# 13311 seconds (approx 3.7 hours) could still be realistic bike rental
# timeframes
# therefore, unrealistic trip length can be considered any trip under 
# 1 minute (60 seconds) in length, or over 6 hours in length (21600 seconds)
# values outside of this range (below 60 and above 21600) will be treated 
# as outliers, and the row will be removed from the dataset
# recording trip IDs for outlier trips
outlier_trips <- trip_v1$id[trip_v1$duration < 60 | trip_v1$duration > 21600]

# filtering out rows with outliers from the dataset
trip_v1 <- trip_v1 %>%
  filter(!(duration < 60 | duration > 21600))

# trip IDs saved under outlier_trips data frame
# save as a csv file for inclusion in the report
write.csv(outlier_trips, "outlier_trips.csv", row.names = F)
