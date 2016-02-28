---
title: "Assignment1 Reproducible Research"
author: "Sarah Bullough"
date: "February 28, 2016"
output: html_document
---

Set working directory
```{r}
setwd("C:/Users/Calli6/Desktop/Reproducible Research")
```

Call libraries
```{r}
library(dplyr)
library(lattice)
```

Download Dataset and Unzip
```{r}
dataset_url <-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(dataset_url, "activity.zip")
unzip("activity.zip", exdir = "Activity")
```

Read in Dataset Activity
```{r}
activity <- read.csv("C:/Users/Calli6/Desktop/Reproducible Research/Activity/activity.csv", stringsAsFactors=FALSE)
```

##What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

Remove missing values from the dataset.
```{r}
activity_nona <- na.omit(activity)
```

1.    Calculate the total number of steps taken per day.  
```{r}
stepsperday <- group_by(activity_nona, date) %>% summarize(Totals=sum(steps))
knitr::kable(stepsperday)
```

2.   Make a histogram of the total number of steps taken each day.
```{r}
hist(stepsperday$Totals, breaks = 10, main = "Total Steps per Day", xlab = "Totals")
```

3.    Calculate and report the mean and median of the total number of steps taken per day.
```{r}
meanstepsperday <- mean(stepsperday$Totals)
medianstepsperday <- median(stepsperday$Totals)
```

```{r}
meanstepsperday

medianstepsperday
```

##What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
and the average number of steps taken, averaged across all days (y-axis)
```{r}
avgstepsperinterval <- group_by(activity_nona, interval) %>% summarize(Mean = mean(steps))

plot(avgstepsperinterval$interval, avgstepsperinterval$Mean
     , type = "l"
     , ylab = "Average Steps per Interval"
     , xlab = "Interval")
```

2. Which 5-minute interval, on average across all the days in the dataset, 
contains the maximum number of steps?
```{r}
maxstepsperinterval <- group_by(activity_nona, interval) %>% summarize(Max = max(steps))
arrange_by_steps <- arrange(maxstepsperinterval, -Max)
Interval_with_maxsteps <- arrange_by_steps[1,]
knitr::kable(Interval_with_maxsteps)
```

##Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). 
The presence of missing days may introduce bias into some calculations or summaries of the data.

1.  Calculate and report the total number of missing values in the dataset 
(i.e. the total number of rows with NAs)

Number of dates missing
```{r}
sum(is.na(activity$date))
```

Number of intervals missing
```{r}
sum(is.na(activity$interval)) #0
```

Number of steps missing
```{r}
sum(is.na(activity$steps)) 
```

2. Devise a strategy for filling in all of the missing values in the dataset. 
Use the mean of the time interval to replace the missing values.
```{r}
avgstepsperinterval <- group_by(activity_nona, interval) %>% summarize(Mean = mean(steps))
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
```{r}
activity_means <- left_join(activity, avgstepsperinterval, by = "interval")

activity_means$steps[is.na(activity_means$steps) == TRUE] <- activity_means$Mean
knitr::kable(head(activity_means))
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 
```{r}
stepsperday_new <- group_by(activity_means, date) %>% summarize(Totals=sum(steps))
hist(stepsperday_new$Totals, breaks = 10, main= "Total Steps per Day with NA replacements", xlab = "Totals")
mean(stepsperday_new$Totals)
median(stepsperday_new$Totals)
```

Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

With NAs:
```{r}
mean(stepsperday$Totals)
median(stepsperday$Totals)
```

Without NAs:
```{r}
mean(stepsperday_new$Totals)
median(stepsperday_new$Totals)
```

The mean is exactly the same and the median is slightly higher with the mean for the interval in place of the missing values.

##Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

Change date to a correct date format
```{r}
date_format <- as.data.frame(strptime(activity_means$date, "%Y-%m-%d"))
```

Put new date column back on dataset and change the name
```{r}
activity_date <- cbind(activity_means, date_format)
colnames(activity_date)[5] <- "date_formatted"
```

Create day variable indicating whether day is a weekday or a weekend
```{r}
activity_date$weekday <- weekdays(activity_date$date_formatted, abbreviate = TRUE)
weekdays <- c("Mon","Tue","Wed","Thu","Fri")
activity_date$day <- ifelse(activity_date$weekday %in% weekdays, c("weekday"), c("weekend"))
activity_date$day <- factor(activity_date$day)
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.
```{r}
avgstepsperintervalperweekday <- group_by(activity_date, day,interval) %>% summarize(Mean = mean(steps))
attach(avgstepsperintervalperweekday)
xyplot(Mean~interval | factor(day),layout=c(1,2), type = "l")

```
