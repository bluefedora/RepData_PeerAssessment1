# Reproducible Research: Peer Assessment 1
## Introduction 
 
It is now possible to collect a large amount of data about personal movement 
using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone 
Up. These type of devices are part of the “quantified self” movement – a group 
of enthusiasts who take measurements about themselves regularly to improve their
health, to find patterns in their behavior, or because they are tech geeks. But 
these data remain under-utilized both because the raw data are hard to obtain 
and there is a lack of statistical methods and software for processing and 
interpreting the data.

This analysis makes use of data from a personal activity monitoring device. This
device collects data at 5 minute intervals through out the day. The data 
consists of two months of data from an anonymous individual collected during the
months of October and November, 2012 and include the number of steps taken in 5 
minute intervals each day. 
 
The data were provided by R.D. Peng, PhD at
http://github.com/rdpeng/RepData_PeerAssessment1. 

The variables in the data set are:
* interval: Identifier for the 5-minute interval in which the measurement was 
taken in the day.  Presumably starting at midnight, 12:05 would end the first 
interval.  There are 288 5-minute intervals in the day. The interval numbers are
not sequential; e.g., the first hour is numbered 0 to 55 (12 five-minute 
intervals), the next hour is numbered 100 to 155, followed by 200 through 255 
and so on.
* date:  The date on which the measurement was taken in YYYY-MM-DD character 
string format
* steps: Number of steps taken in the 5-minute interval (missing values are 
coded as NA)

## Loading and preprocessing the data

Goals:

1. Load the data
1. Process/transform the data as needed

Process points:
* Load libraries
* Unzip the data file
* Read the .csv data into "activity" data.frame
* Convert date strings to an additional date-type variable
* Calculate missing step values vector
* Setup vectors to use to compare means and medians later
* Instantiate functions


```r
library(utils)
library(stats)
library(plyr)
library(lattice)
unzip("activity.zip")
activity <- read.csv("activity.csv")
activity$dated <- as.Date(activity$date, "%Y-%m-%d")
missing_vector <- is.na(activity$steps)
# setup vectors to store means and medians for comparisons
means <- c(0, 0, 0)
medians <- c(0, 0, 0)
```

Instantiate histogram, mean and median reporting function.


```r
# Function to:
#  Aggregate steps, calculate total number of steps each day
#  Calculate the mean and median total number of steps per day
#  produce histogram
# Takes as input:
#  x the dataframe containing the activity data
#  sub the subtitle to the histogram to identify it
#  calc the calculation index of where to store the resultant mean and medians
# Output:
#  stores the mean and median of the total number of steps taken each day for 
#    use later
#  plots the histogram of total steps per day

histogram <- function(x, sub, calc=1) {
    #find the total number of steps per day, store in sums
    sums <- with(x, aggregate(steps, list(date), sum, na.rm=TRUE))
    names(sums) <- c("Date", "TotalSteps")
    means[calc] <<- mean(sums$TotalSteps, na.rm=TRUE)
    medians[calc] <<- median(sums$TotalSteps, na.rm=TRUE)
    barplot(sums$TotalSteps, main="Total Number of Steps per Day", 
            ylab="Steps", xlab="Date", sub=sub)
    abline(a=means[calc], col="green", b=0)
    abline(a=medians[calc], col="red", b=0)
    legend("topleft", legend=c("mean", "median"), 
           col=c("green", "red"), lty=1)
}
```

Instantiate imputing functions, based on the same intervals over the time 
period, it will use the function specified, median is default.


```r
# Functions used to impute values for steps
# Take as input:
#  x which is the variable to impute
#  returns the calculated value to use as imputed value
# used in conjunction with ddply which does the split, calc, and combine
imputeMean <- function(x) replace(x, is.na(x), mean(x, na.rm=TRUE))
imputeMedian <- function(x) replace(x, is.na(x), median(x, na.rm=TRUE))
#
# Function to impute missing step data by using specified function on values
#  from the same interval
#
# The strategy is to use the median or mean from all the other observations in
# the same time interval as an estimate of what the missing value might be
#
# Takes as input:
#  x which is the data frame containg the activity data
#  func which specifies the aggregation function
# Returns:
#  data frame like original but with imputed values for steps where 
#    steps were NA
imputeByInterval <- function(x, func="median") {
    if (func=="mean")
        y <- ddply(x, ~ interval, transform, steps = imputeMean(steps))
    else 
        y <- ddply(x, ~ interval, transform, steps = imputeMedian(steps))  
    
    return(y)
}
```

## What is mean total number of steps taken per day?

Goals:

1. Make a histogram of the total number of steps taken each day
1. Calculate and report the mean and median total number of steps taken each
day

Process points:

(all while ignoring missing data)
* Using the original activity data, plot the total steps per day 
* Calculate and display mean and medians


```r
histogram(activity, sub="Original Activity Data", 1)
```

![plot of chunk totalStepsPerDayOriginalData](figure/totalStepsPerDayOriginalData.png) 

The mean of the total number of steps taken each day is: 9354.2295

The median of the total number of steps taken each day is:1.0395 &times; 10<sup>4</sup>

## What is the average daily activity pattern?

Goals:

1. Make a time series plot of the 5-minute interval (x-axis) and the average
number of steps taken, averaged across all days (y-axis)
1. Which 5-minute interval, on average across all the days in the data set, 
contains the maximum number of steps?

Process points:
* Calculate the average number of steps per interval and store in data set
'intervals'
* Line plot the result x, y (intervals, steps).
* Determine which interval has the maximum average steps
* Report both the interval index and value


```r
intervals <- ddply(activity, ~ interval, summarize, 
                   AverageSteps=mean(steps, na.rm=TRUE))  
plot(intervals$interval, intervals$AverageSteps, 
     main="Average Daily Activity Pattern", type="l")
```

![plot of chunk avgDailyPattern](figure/avgDailyPattern.png) 

Find the interval containing, on the average, the maximum number of steps:


```r
max_interval_index <- which(intervals$AverageSteps==max(intervals$AverageSteps))
interval <- intervals$interval[max_interval_index]
```

The 5 minute interval, which on average across all the days in the data set, 
contains the maximum number of steps is interval #: 835.

## Imputing missing values
I wanted to compare using two methods of imputing the missing step numbers, 
mean/interval and median/interval.  Both are listed here.

Goals:

1. Calculate and report the total number of missing values in the data set
1. Devise a strategy for filling in all the missing values in the data set
1. Create a new data set that is equal to the original data set but with missing
data filled in
1. Make a histogram of the total number of steps taken each day and calculate 
and report the mean and median total number of steps taken per day.  Do these 
values differ than the estimates from above? What is the impact of imputing 
missing data on the estimates of total daily number of steps.

*Note: the imputing functions are listed at the top of this file.  Look there 
for the functions and the strategy of imputing values.*

Processing points:
* Calculate the number of missing values. 
* We will calculate the mean and median values across all the days for the 5 
minute intervals and will use those values in place of the missing step values; 
the new data set is 'activity_imputed'. 
* Compare overall mean and median values of the resulting imputed data sets.
* We'll then compare the results of imputing both ways by comparing the 
plotting of the histogram (compare step totals between the two imputing models
and the original histogram)


```r
missing <- sum(missing_vector)
```

There are 2304 missing step values in the original data set.

**_First_** impute using the mean per interval:


```r
activity_imputed <- imputeByInterval (activity, func="mean")
missing_vector2 <- is.na(activity_imputed$steps)
missing2 <- sum(missing_vector2)
```

After imputing using means per interval, there are 0 missing step 
values in the imputed data set.

Here is the histogram of the data after the missing values were imputed using
the interval means as described above.


```r
histogram(activity_imputed, "Interval Steps Imputed with Means", 2)
```

![plot of chunk meanPerDayImputedMeans](figure/meanPerDayImputedMeans.png) 

The mean of the total number of steps taken each day is: 1.0766 &times; 10<sup>4</sup>

The median of the total number of steps taken each day is:1.0766 &times; 10<sup>4</sup>

**_Second_** impute using the median per interval:


```r
activity_imputed <- imputeByInterval (activity, func="median")
missing_vector2 <- is.na(activity_imputed$steps)
missing2 <- sum(missing_vector2)
```

After imputing using medians per interval, there are 0 missing step 
values in the imputed data set.

Here is the histogram of the data after the missing values were imputed using
the medians as described above.


```r
histogram(activity_imputed, "Interval Steps Imputed with Medians", 3)
```

![plot of chunk meanPerDayImputedMedians](figure/meanPerDayImputedMedians.png) 

The mean of the total number of steps taken each day is: 9503.8689

The median of the total number of steps taken each day is:1.0395 &times; 10<sup>4</sup>

Looking at the results of both imputing functions it looks like:

* Using the mean steps per interval to impute gave us a change in mean from 
9354.2295 to 1.0766 &times; 10<sup>4</sup> and a change of median of from 1.0395 &times; 10<sup>4</sup> to 
1.0766 &times; 10<sup>4</sup>
* Using the median steps per interval to impute gave us a change in mean from 
9354.2295 to 9503.8689 and a change of median of from 1.0395 &times; 10<sup>4</sup> to 
1.0395 &times; 10<sup>4</sup>
* The histogram of the total number of steps changed the most with imputing the 
missing values using the interval means v the interval medians

## Are there differences in activity patterns between weekdays and weekends?

Will use the last imputed data (using the interval medians to impute)

Goals:

1. Create a new factor variable in the data set with two levels -- "weekday" 
and "weekend" indicating whether a given date is a weekday or weekend day.
1. Make a panel plot containing time series plots of the 5-minute interval
(x-axis) and the average number of steps taken, averaged across all weekday days
or weekend days (y-axis).

Processing points:
* Using the transform function introduce a factor variable, weekend, that takes
on the values of "weekday" or "weekend" based upon the date of the observation.
* Aggregate the steps based on interval and weekday/weekend
* Produce a panel plot of steps ~ interval; one panel for weekend and one
for weekday.
* We can then compare the plots


```r
xx <- transform(activity_imputed, 
                weekend = ifelse(
                weekdays(activity_imputed$dated) %in% c("Sunday", "Saturday"),
                     "weekend", "weekday"))
interval_weekend <- ddply(xx, .(interval, weekend), summarize, steps=mean(steps))
xyplot(steps ~ interval | weekend, data=interval_weekend, layout=c(1,2),
       type="l")
```

![plot of chunk patternDifferences](figure/patternDifferences.png) 

Examining the patterns for weekend v weekday, we see:
* About the same pattern of inactivity periods at the beginning of the day and 
at the end of day
* However, in the morning, the daily weekend activity is delayed
* The weekend activity is greater during the day than it is during the day on
weekdays
* Both patterns exhibit a higher level of activity in the morning at about the
same time


