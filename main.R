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
