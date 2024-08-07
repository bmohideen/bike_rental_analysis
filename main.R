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

#### Data Cleaning - Weather ####
# fix inconsistencies in column name (Speed vs. speed)
weather_v1 <- weather_v1 %>%
  rename(max_wind_speed_mph = max_wind_Speed_mph)

# convert date to POSIX format from character type
weather_v1$date <- mdy(weather_v1$date)

# check for the count of duplicate rows
# all rows are distinct (returns FALSE)
any(duplicated(weather_v1))

# replace blank values with NA in the events column of dataset
# rows with NA do not need to be removed
weather_v1 <- weather_v1 %>%
  mutate(events = na_if(events, ""))

# trace precipitation denoted by T, prevents column from being numeric
# will be changed to the midpoint between 0 and the lowest value (0.01), 
# which is 0.005
weather_v1 <- weather_v1 %>%
  mutate(precipitation_inches =
           case_when(precipitation_inches == 'T' ~ "0.005",
                     .default = precipitation_inches))
weather_v1$precipitation_inches <- as.numeric(weather_v1$precipitation_inches)

#### Rush Hours ####
# create new column with midpoint of each trip, so that the entire time spent
# riding the bike is considered for each separate ride
# origin required to ensure that there is a reference point when converting
# the numeric values back to POSIX format
trip_v1$midpoint_trip <- as.POSIXct(
  (as.numeric(trip_v1$start_date) + as.numeric(trip_v1$end_date)) / 2, 
  tz = "UTC", origin = "1970-01-01")

# create new column with day of week for midpoint date
trip_v1$trip_day_of_week <- weekdays(trip_v1$midpoint_trip)

# only concerned about times on each day of the week
# not interested in dates themselves
# can convert midpoint_trip from ymd_hms format to only include hour time
# this will ensure that midpoint_trip values can be compared across
# different dates
# this extracts only the hour from each time measurement
trip_hour <- hour(trip_v1$midpoint_trip)
trip_v1$trip_hour <- trip_hour

# create a new dataframe that only includes Mon-Fri (weekdays)
weekday_trips <- trip_v1 %>%
  filter(trip_day_of_week %in% 
           c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))

# create another new dataframe that only includes Sat-Sun (weekends)
weekend_trips <- trip_v1 %>%
  filter(trip_day_of_week %in% 
           c("Saturday", "Sunday"))

# creating a vector of hour labels
hour_labels <- c("12AM-1AM", "1AM-2AM", "2AM-3AM", "3AM-4AM", 
                 "4AM-5AM", "5AM-6AM","6AM-7AM", "7AM-8AM", 
                 "8AM-9AM", "9AM-10AM", "10AM-11AM", "11AM-12PM",
                 "12PM-1PM", "1PM-2PM", "2PM-3PM", "3PM-4PM", 
                 "4PM-5PM", "5PM-6PM","6PM-7PM", "7PM-8PM", 
                 "8PM-9PM", "9PM-10PM", "10PM-11PM", "11PM-12AM")

# converting hour labels to factor form, so that order is maintained
# in the histogram
hour_labels <- factor(hour_labels, levels = 
                        c("12AM-1AM", "1AM-2AM", "2AM-3AM", "3AM-4AM", 
                          "4AM-5AM", "5AM-6AM","6AM-7AM", "7AM-8AM", 
                          "8AM-9AM", "9AM-10AM", "10AM-11AM", "11AM-12PM",
                          "12PM-1PM", "1PM-2PM", "2PM-3PM", "3PM-4PM", 
                          "4PM-5PM", "5PM-6PM","6PM-7PM", "7PM-8PM", 
                          "8PM-9PM", "9PM-10PM", "10PM-11PM", "11PM-12AM"))

# creating a count of trips during each hour period, for all weekdays
# saving as a dataframe with frequency (y-axis) and the trip_hour values 
# (x-axis) ranging from 0-23
weekday_hour_count <- weekday_trips %>%
  count(trip_hour, name = "frequency")

# replacing the trip_hour values with their corresponding hour_labels
weekday_hour_count$trip_hour <- hour_labels

# creating a histogram that shows the volume of trips for each hour of the day
# specifically for weekdays
ggplot(weekday_hour_count, aes(x = hour_labels, y = frequency)) +
  geom_bar(stat = "identity", fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Frequency of Trips for 
       Bike Rentals in the Bay Area on Weekdays", 
       x = "Hour for the Midpoint of the Trip", 
       y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_text(hjust = 0.5))

# through visual inspection of the histogram, the hours of weekdays with the 
# highest trip volume are 7-10AM and 4-7PM (rush hours)
# in 24h time: rush hours are 7:00 - 10:00 and 16:00 - 19:00
# to determine the most frequent stations during rush hours, a separate 
# dataframe that only includes trips with a midpoint within rush hour 
# will be created
# in trip_hour, the hours are defined by starting hour (so 7-9 and 16-18)
rush_hour_trips <- weekday_trips %>%
  filter(trip_hour %in% c(7, 8, 9, 16, 17, 18))

#### Rush Hour Trips - Frequent Stations ####
# now that a dataframe with only rush hour trips has been created, need to 
# determine the most frequent starting and ending stations within the dataframe
rush_hour_start_stations <- rush_hour_trips %>%
# counts occurrences for each station name
  count(start_station_name) %>%
# orders by frequency in descending order
  arrange(desc(n)) %>%
# selects top 10 rows in terms of frequency
  top_n(10, n) %>%
# renames columns in table
  rename(station_name = start_station_name, frequency = n)
print(rush_hour_start_stations)

# for the most frequent ending stations during rush hours:
rush_hour_end_stations <- rush_hour_trips %>%
  # counts occurrences for each station name
  count(end_station_name) %>%
  # orders by frequency in descending order
  arrange(desc(n)) %>%
  # selects top 10 rows in terms of frequency
  top_n(10, n) %>%
  # renames columns in table
  rename(station_name = end_station_name, frequency = n)
print(rush_hour_end_stations)

#### Weekend Trips - Frequent Stations ####
# for the most frequent starting stations during the weekends:
weekend_start_stations <- weekend_trips %>%
  # counts occurrences for each station name
  count(start_station_name) %>%
  # orders by frequency in descending order
  arrange(desc(n)) %>%
  # selects top 10 rows in terms of frequency
  top_n(10, n) %>%
  # renames columns in table
  rename(station_name = start_station_name, frequency = n)
print(weekend_start_stations)

# for the most frequent ending stations during the weekends:
weekend_end_stations <- weekend_trips %>%
  # counts occurrences for each station name
  count(end_station_name) %>%
  # orders by frequency in descending order
  arrange(desc(n)) %>%
  # selects top 10 rows in terms of frequency
  top_n(10, n) %>%
  # renames columns in table
  rename(station_name = end_station_name, frequency = n)
print(weekend_end_stations)

#### Bike Utilization ####
# rather than calculating the average utilization for each individual bike
# determined by bike_id, an average value will be calculated across all bikes
# used within a single month
# this will determine the months in which bikes are used more often, so an 
# average value calculated across all bikes is sufficient
# average for individual bikes is not necessary, and applicability would be 
# limited as variations between different bikes are not known (all bikes are
# looked at as the same in the dataset)
# if there were different types of bikes mapped out in the dataset, then it 
# would make more sense to calculate the average for individual bikes

# create new column for months of the midpoint for each trip
# each trip should only be associated with a single month
# which is why midpoint for trip is used
trip_v1$trip_month <- month(trip_v1$midpoint_trip)

# create vector with number of days in each month (Jan - Dec, in order)
# dates are all in 2014, which is not a leap year - Feb had 28 days
month_days <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

# calculating total duration of bike use for each month
# 1 day = 24 x 60 x 60 = 86400 seconds
# duration is in seconds, divide by 86400 (number of seconds in 1 day)
# to get duration in days
monthly_duration <- trip_v1 %>%
  group_by(trip_month) %>%
  summarise(total_duration = sum(duration) / 86400,
# from EDA: there are 687 unique bike IDs, but all may not be in use for each
# given month
# need to multiply the denominator (the total time in the month) by the number
# of unique bikes used during each month
            unique_bikes = length(unique(bike_id)))

# calculating average utilization
# total time: number of days in month x number of unique bikes for the month
# total duration: duration of bike rides converted to days (not seconds)
monthly_util <- monthly_duration %>%
  mutate(total_time = month_days[trip_month] * unique_bikes[trip_month],
         average_utilization = total_duration / total_time)
print(monthly_util)

#### Weather Condition Analysis ####
# need to create new dataset that combines trip data with weather data
# common columns are date and zip code
# weather date format is year, month, day
# trip date format includes time as well
# time needs to be removed from date 
# midpoint for each trip will be used for time to get a single date for 
# each trip
trip_v1$midpoint_trip <- as.Date(trip_v1$midpoint_trip)

# can combine trip and station data together through the station name 
# and name columns; city is included with station data as well
# this will allow for trip data to be analyzed by city
# (city also present in weather data)
trip_station <- left_join(trip_v1, station_v1, 
                          by = c("start_station_name" = "name"))

# need to rename midpoint_trip column as date (to be consistent with weather)
trip_station <- trip_station %>%
  rename(date = midpoint_trip)

# combining weather together with combined station and trip dataframe
trip_station_weather <- left_join(trip_station, weather_v1, 
                                  by = c("date", "city"))

# for correlation matrix - all variables must be numeric
# only need to include variables that are relevant to the analysis


