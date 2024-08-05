# Midterm - Bay Area Bike Rental Operation Research Project
# Bilal Mohideen - BTC1855H
# RStudio Version 2024.04.1+748 (2024.04.1+748)

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

#### EDA ####

# performing exploratory data analysis (EDA) for each dataset separately
# following template outlined here: 
#https://blog.datascienceheroes.com/exploratory-data-analysis-in-r-intro/
