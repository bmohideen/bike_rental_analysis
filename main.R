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
glimpse(station)

# summary of the dataframe (quantity of zeros/NAs/infinite values, data 
# types, and unique values)
print(status(station))

# city is the only categorical variable that makes sense to analyze
# the others are all unique (names) or irrelevant to the project (installation
# date)
freq(station)
freq(station$city)

# frequencies are irrelevant for ID and longitude/latitude
# dock_count is the most relevant integer variable
plot_num(station)
print(profiling_num(station))

# descriptive statistics for the entire dataset
# provides info on missing and distinct values for each column
describe(station)
