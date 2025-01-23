# Bay Area Bike Rental Operation Research Project

Project plan:

a) Exploratory Data Analysis (EDA) - Analyze each of the datasets (station.csv, trips.csv, and weather.csv) to summarize and highlight key characteristics. Plan to use "funModeling" package.

b) Data Cleaning - Remove any inaccurate/inconsistent data. Plan to use "tidyverse" and "dplyr" packages. Trips starting and ending at same station with duration < 3 mins will be removed from dataset. Outlier thresholds will be determined, and these values in the dataset will be removed.

c) Rush Hours - Establish highest volume hours on weekdays (rush hours). Determine the most frequent starting/ending stations during rush hours and weekends.

d) Bike Utilization - Calculate the average utilization of bikes for each month.

e) Weather Condition Analysis - Combine trip data together with weather data in a new dataset. Create correlation matrix for the dataset. 
