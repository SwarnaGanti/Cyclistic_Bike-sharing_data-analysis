
# Explore,Clean(duplicates, missing data, recode), Manipulate, Describe & Summarise, Visualise, Analyse
# Install and load necessary packages for analysis
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("lubridate")
install.packages("skimr")
install.packages("janitor")
install.packages("plyr")
install.packages("dplyr")
library(tidyverse)
library(ggplot2)
library(lubridate)
library(skimr)
library(janitor)
library(plyr)
library(dplyr)
# Reading and merging all the 12 csv files into one single dataframe
cyclistic_data <- list.files(path="~/Desktop/Google Analytics-Capstone Project/cyclistic_trip_data",
                             pattern = "*.csv", full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows

# Viewing the first few rows of the dataframe
head(cyclistic_data)

# retireve the number of rows and columns in the dataframe
dim(cyclistic_data)

# Getting to know the data
str(cyclistic_data)
glimpse(cyclistic_data)

#view the dataframe
#View(cyclistic_data)

#returns just the names of the columns
names(cyclistic_data)

# Find the columns with missing values
colSums(is.na(cyclistic_data))

# Dropping all the rows with missing values
data_withoutNA <- cyclistic_data %>% 
  drop_na()

# Number of rows and columns after removing the missing values
dim(data_withoutNA)
colSums(is.na(data_withoutNA))

# Renaming the columns
data_renamedcolumns <- rename(data_withoutNA, c("ride_id" = "trip_id"
                              ,"rideable_type" = "bike_id"
                              ,"start_station_name" = "from_station_name"
                              ,"start_station_id" = "from_station_id"
                              ,"end_station_name" = "to_station_name"
                              ,"end_station_id" = "to_station_id"
                              ,"member_casual" = "usertype"))
names(data_renamedcolumns)

# Remove the columns -  "start_lat","start_lng","end_lat","end_lng"
finaldata <- data_renamedcolumns %>% 
select(-c( start_lat,start_lng,end_lat,end_lng))
##str(finaldata)

#Adding columns that list the date, month, day, and year of each ride
finaldata$date <- as.Date(finaldata$started_at)
finaldata$month <- format(as.Date(finaldata$started_at),"%b")
finaldata$day <- format(as.Date(finaldata$started_at),"%d")
finaldata$Year <- format(as.Date(finaldata$started_at),"%Y")
finaldata$day_of_week <- format(as.Date(finaldata$started_at),"%A")

# Inspecting structure of the data
str(finaldata)


# Finding ride length
finaldata <- finaldata %>% 
  mutate(finaldata, ride_length =
           difftime(ended_at,started_at,units='mins'))
finaldata$ride_length <- formatC(finaldata$ride_length, digits = 2, format = "f")
finaldata$ride_length <- as.numeric(as.character(finaldata$ride_length))
is.numeric(finaldata$ride_length)

# adding  season column
finaldata <- finaldata %>% 
  mutate(season = case_when(month(finaldata$started_at) %in% c(12, 1 ,2) ~ 'Winter',
                            month(finaldata$started_at) %in% c(3,4, 5) ~ 'Spring',
                            month(finaldata$started_at) %in% c(6,7,8) ~ 'Summer',
                            month(finaldata$started_at) %in% c(9, 10, 11) ~ 'Fall' ))


##str(finaldata)
#View(finaldata)

# Find mean, max, min of ride length
summary(finaldata$ride_length)

# remove rows with negative ride length
finaldata <- finaldata[finaldata$ride_length >0,]
finaldata
mean(finaldata$ride_length)
median(finaldata$ride_length)
max(finaldata$ride_length)
min(finaldata$ride_length)

# compare ride length statistics between casual and member riders
aggregate(finaldata$ride_length~finaldata$usertype, FUN = mean)
aggregate(finaldata$ride_length~finaldata$usertype, FUN = median)
aggregate(finaldata$ride_length~finaldata$usertype, FUN = max)
aggregate(finaldata$ride_length~finaldata$usertype, FUN = min)

# average ride length er day for casual riders and members
aggregate(finaldata$ride_length~finaldata$usertype+finaldata$day_of_week, FUN = mean)

# sort the day of week in order
finaldata$day_of_week <- ordered(finaldata$day_of_week,
                                 levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# average ride length per day for casual riders and members
aggregate(finaldata$ride_length~finaldata$usertype+finaldata$day_of_week, FUN = mean)

str(finaldata)
View(finaldata)


write.csv(finaldata, file = '~/Desktop/Google Analytics-Capstone Project/cyclistic_data.csv', row.names = FALSE)


  
