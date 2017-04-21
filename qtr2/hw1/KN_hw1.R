##--------------------------------------------
##
## R Review Homework Headstart
##
## Class: PCE Data Science Methods Class
##
##--------------------------------------------

##-----Set working directory-----
setwd('C:/Users/Kevin/Google Drive/datasci')


##-----Load Libraries-----
#install.packages("data.table")
library(dplyr)
library(data.table)

source('weather_retrieval.R')

# Load jittered Data
headcount = read.csv('JitteredHeadCount.csv', stringsAsFactors = FALSE)
headcount$DateFormat = as.Date(headcount$DateFormat, format="%m/%d/%Y")

weather_file_name = 'las_vegas_hourly_weather.csv'

# Let's test if the file is in the directry using list.files()
#        If it is, load that file instead of running webscraper

if (weather_file_name %in% list.files()){
  weather_data = read.csv(weather_file_name, stringsAsFactors = FALSE)
  names(weather_data) = c('time','temp','dew_pt','humidity','pressure',
                          'visibility','wind_dir','wind_speed','gust_speed',
                          'precipitation','events','conditions',
                          'wind_dir_deg','date')
} else {
  range(headcount$DateFormat)
  
  airport = 'KLAS'
  dates = seq(from=min(headcount$DateFormat),
              to=max(headcount$DateFormat),
              by=1)
  weather_data = get_weather_data(airport, dates)
  names(weather_data) = c('time','temp','dew_pt','humidity','pressure',
                          'visibility','wind_dir','wind_speed','gust_speed',
                          'precipitation','events','conditions',
                          'wind_dir_deg','date')
}

# Let's create a datetime in the weather data
weather_data$datetime = paste(weather_data$date,weather_data$time)
weather_data$datetime = strptime(weather_data$datetime, format="%Y-%m-%d %I:%M %p")
weather_data$Hour = as.numeric(format(round(weather_data$datetime, units="hours"), format="%H"))


# Let's merge with different methods.
#   - If we were truly merging to analyze casino data, we don't want to lose
#      headcount data if weather doesn't exist, so we want to do a
#      left merge (keeping all the headcount data)
#
#   - Remember, we want to merge on date AND hour.
#   - Note: the headcount data has multiple rows for these (more than 1 game type)

# Check for duplicates!
anyDuplicated(headcount[c("DateFormat", "Hour","GameCode")])

anyDuplicated(weather_data[c("date", 'Hour')]) # Oh no!  How could this happen?

# Drop for now:
weather_data = weather_data[!duplicated(weather_data[c("date", 'Hour')]),]

# Rename some columns:
intersect(names(headcount), names(weather_data))
weather_data$DateFormat = weather_data$date
weather_data$date = NULL
weather_data$DateFormat = as.Date(weather_data$DateFormat, format="%Y-%m-%d")


# Pick one of the below merges, and comment out the other two.
# <<<CHANGE BELOW MERGING CODE>>>

# Merge (base)
headcount_base_all = merge(headcount, weather_data, all.x=TRUE, by=c("DateFormat","Hour"))


# Merge(data.table)
# Note that data.table has a problem.  It canNOT take POSIX values. So we drop it (we are done with that column anyways)
weather_data$datetime = NULL
library(data.table)
headcount = as.data.table(headcount)
weather_data = as.data.table(weather_data)

# Set keys for faster merges
setkeyv(headcount, c("DateFormat", "Hour"))
setkeyv(weather_data, c("DateFormat", "Hour"))

headcount_dt_all = merge(headcount, weather_data, all.x=TRUE, by=c("DateFormat", "Hour"))

# Merge(dplyr)
library(dplyr)
headcount_dplyr_all = left_join(headcount, weather_data, by=c("DateFormat", "Hour"))


##----Find another insight involving weather------

# For now, drop all NA rows:
#     use the command 'complete.cases':
# Use 'complete.cases()' as a row filter on your data frame.
headcount_base_all$datetime = NULL
headcount_base_all = headcount_base_all[complete.cases(headcount_base_all),]

# can also use na.omit()

headcount_dplyr_all = na.omit(headcount_dplyr_all)
headcount_dt_all = na.omit(headcount_dt_all)

#observation 1 - Temperature and humidity

library(ggplot2)

avg_hum = aggregate(HeadCount ~ humidity, data = headcount_base_all, FUN = mean)
avg_temp = aggregate(HeadCount ~ temp, data = headcount_base_all, FUN = mean)

qplot(avg_hum$humidity,avg_hum$HeadCount, geom=c("point", "smooth"), 
      method="lm", formula=y~x, xlab = "Humidity %", ylab = "Headcount")

qplot(avg_temp$temp,avg_temp$HeadCount, geom=c("point", "smooth"), 
      method="lm", formula=y~x, xlab = "Temperature (F)", ylab = "Headcount")


#observation 2 - headcount by closing price gain/loss S&P500 index (from Yahoo finance csv export)

avg_by_day = aggregate(HeadCount ~ DayOfWeek, data = headcount_base_all, FUN = mean)

stock = read.csv('spclose.csv', stringsAsFactors = FALSE)
stock$DateFormat = as.Date(stock$DateFormat, format="%m/%d/%Y")

#select days 1-5 (since market is only open then)
weekDayHC <- subset(headcount_base_all, DayOfWeek <= 5)

avg_by_day = aggregate(HeadCount ~ DateFormat, data = weekDayHC, FUN = mean)

stockHCtest = merge(stock, weekDayHC, by = "DateFormat")

diffHC = merge(stock, avg_by_day, by = "DateFormat")

#nothing too interesting
qplot(diffHC$Diff,diffHC$HeadCount, geom=c("point", "smooth"), 
      method="lm", formula=y~x, xlab = "Index gain/loss", ylab = "Headcount")

#over time

qplot(diffHC$DateFormat,diffHC$HeadCount, geom=c("point", "smooth"), 
      method="lm", formula=y~x, xlab = "Date", ylab = "Headcount")
qplot(diffHC$Close,diffHC$HeadCount, geom=c("point", "smooth"), 
      method="lm", formula=y~x, xlab = "Closing price", ylab = "Headcount")
diffHC = merge(stock, avg_by_day, by = "DateFormat")



#observation 3 compare headcount to google searches for las vegas (via google trends csv export). Unfortunately weekly

google = read.csv('google.csv', stringsAsFactors = FALSE,strip.white=TRUE)
#google$rawDate <- strsplit(google$Week, "-")
google$year <- sapply(strsplit(google$Week, "-"), '[',1)
google$month <- sapply(strsplit(google$Week, "-"), '[',2)
google$day <- trimws(sapply(strsplit(google$Week, "-"), '[',3))
google$DateFormat <- paste(google$month, google$day, google$year, sep = "/")
google$DateFormat = as.Date(google$DateFormat, format="%m/%d/%Y")
require(zoo)
googleHC <- merge(headcount_base_all, google, by = "DateFormat", all = TRUE)
googleHC<- na.locf(googleHC, na.rm = TRUE)

avg_by_day = aggregate(HeadCount ~ DateFormat, data = headcount_base_all, FUN = mean)
searchHC <- merge(google, avg_by_day,by = "DateFormat", all = TRUE)
searchHC <- na.locf(searchHC, na.rm = TRUE)
searchHC$HeadCount <- as.numeric(as.character(searchHC$HeadCount))
searchHC$Index <- as.numeric(as.character(searchHC$Index))
plot(searchHC$Index,searchHC$HeadCount)
ggplot(searchHC, aes(Index, HeadCount)) + geom_jitter() + geom_smooth(method=lm)
