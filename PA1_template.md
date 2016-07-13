---
title: "Peer Assessment Project 1"
---

#Loading and preprocessing the data
```{r}
getwd()
setwd("G:/Data Science Course Materials/Reproducible Research/wk2")
data <- read.csv("activity.csv")
str(data)
summary(data)
echo = TRUE
```

# What is mean total number of steps taken per day?
```{r}
steps.date <- aggregate(steps ~ date, data = data, FUN = sum)
barplot(steps.date$steps, names.arg = steps.date$date, xlab = "Date", ylab = "Total steps each day", col= "red", ylim =c(0, 25000))
# Calculate and report the mean and median total number of steps taken per day
mean(steps.date$steps)
median(steps.date$steps)
echo = TRUE
```
#Average daily activity pattern- a time series plot (type = "l") of the 5-minute interval (x-axis) and the average number of steps taken averaged across all days (y-axis).
```{r}
steps.interval <- aggregate(steps ~ interval, data = data, FUN = mean)
plot(steps.interval, type = "l")
#Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
steps.interval[which.max(steps.interval$steps), ]
echo = TRUE
```
#Imputing missing values
```{r}
sum(is.na(data))
# Strategy to impute missing values - all missing values are filled in with mean value for that 5-minute interval
# Replace each missing value with the mean value of its 5-minute interval
fill.value <- function(steps, interval) {
  filled <- NA
  if (!is.na(steps)) 
    filled <- c(steps) else filled <- (steps.interval[steps.interval$interval == interval, "steps"])
    return(filled)
}
filled.data <- data
filled.data$steps <- mapply(fill.value, filled.data$steps, filled.data$interval)
echo = TRUE
```
# Preparing plot after computing missing data
```{r}
steps.date <- aggregate(steps ~ date, data = filled.data, FUN = sum)
barplot(steps.date$steps, names.arg = steps.date$date, xlab = "Date", ylab = "Steps per day with no NAs", col = "red", ylim = c(0, 25000))
#mean and median after computing missing values
mean(steps.date$steps)
median(steps.date$steps)
echo = TRUE
```
#Are there differences in activity patterns between weekdays and weekends?
#Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
```{r}
DayGrp <- function(date) {
  if (weekdays(as.Date(date)) %in% c("Saturday", "Sunday")) {
    "Weekend"
  } else {
    "Weekday"
  }
}
filled.data$DayGrp <- as.factor(sapply(filled.data$date, DayGrp))


library(lattice)

steps.type <- aggregate(steps ~ interval + DayGrp, filled.data, mean)
#Plotting the patterns between weekdays and weekend
xyplot(steps ~ interval | DayGrp, data=steps.type, layout=c(2,1), type='l')
echo = TRUE
```
