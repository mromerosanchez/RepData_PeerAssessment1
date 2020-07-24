---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data


```r
#loading libraries

library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library (ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 4.0.2
```

```r
# Unzip and pre-procesing the data

filename <- "activity.zip"

unzip(filename)

activity<-read.csv("activity.csv", header=TRUE)

head(activity)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```




## What is mean total number of steps taken per day?

*For this part of the assignment, you can ignore the missing values in the dataset.*

*1. Calculate the total number of steps taken per day*

*2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day*

*3. Calculate and report the mean and median of the total number of steps taken per day*

**Total Steps per day**


```r
# create and print number of steps per day

StepsByDay<-aggregate(steps~date, activity, sum)

StepsByDay
```

```
##          date steps
## 1  2012-10-02   126
## 2  2012-10-03 11352
## 3  2012-10-04 12116
## 4  2012-10-05 13294
## 5  2012-10-06 15420
## 6  2012-10-07 11015
## 7  2012-10-09 12811
## 8  2012-10-10  9900
## 9  2012-10-11 10304
## 10 2012-10-12 17382
## 11 2012-10-13 12426
## 12 2012-10-14 15098
## 13 2012-10-15 10139
## 14 2012-10-16 15084
## 15 2012-10-17 13452
## 16 2012-10-18 10056
## 17 2012-10-19 11829
## 18 2012-10-20 10395
## 19 2012-10-21  8821
## 20 2012-10-22 13460
## 21 2012-10-23  8918
## 22 2012-10-24  8355
## 23 2012-10-25  2492
## 24 2012-10-26  6778
## 25 2012-10-27 10119
## 26 2012-10-28 11458
## 27 2012-10-29  5018
## 28 2012-10-30  9819
## 29 2012-10-31 15414
## 30 2012-11-02 10600
## 31 2012-11-03 10571
## 32 2012-11-05 10439
## 33 2012-11-06  8334
## 34 2012-11-07 12883
## 35 2012-11-08  3219
## 36 2012-11-11 12608
## 37 2012-11-12 10765
## 38 2012-11-13  7336
## 39 2012-11-15    41
## 40 2012-11-16  5441
## 41 2012-11-17 14339
## 42 2012-11-18 15110
## 43 2012-11-19  8841
## 44 2012-11-20  4472
## 45 2012-11-21 12787
## 46 2012-11-22 20427
## 47 2012-11-23 21194
## 48 2012-11-24 14478
## 49 2012-11-25 11834
## 50 2012-11-26 11162
## 51 2012-11-27 13646
## 52 2012-11-28 10183
## 53 2012-11-29  7047
```

**Histogram of the total number of steps taken each day**


```r
hist(StepsByDay$steps, xlab="Number of Steps", ylab="Number of Days", main="Total Number of Steps by day")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

**3. Calculate and report the mean and median of the total number of steps taken per day**

```r
# Mean
meansteps<-mean(StepsByDay$steps)
meansteps
```

```
## [1] 10766.19
```

```r
#Median
medianSteps<-median(StepsByDay$steps)
medianSteps
```

```
## [1] 10765
```



## What is the average daily activity pattern?

**1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)**   



```r
steps_by_Interval<-aggregate(steps~interval, data=activity, mean, na.rm=TRUE)

g<-ggplot(steps_by_Interval, aes(interval, steps))

g+geom_line(col="blue")+ggtitle("Average steps per time interval")+xlab("Time")+ylab("Steps")+theme(plot.title = element_text(face="bold", size=18))
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

**2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?**


```r
five_int_by_max_Steps <- steps_by_Interval[which.max(steps_by_Interval$steps),]$interval
five_int_by_max_Steps
```

```
## [1] 835
```



## Imputing missing values
**1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)**  

```r
number_missing_data <- sum(is.na(activity$steps))
number_missing_data
```

```
## [1] 2304
```

**2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.** 

```r
fill_value <- function(steps, interval) {
    filled1 <- NA
    if (!is.na(steps))
        filled1<- c(steps)
    else
        filled1 <- (steps_by_Interval[steps_by_Interval$interval==interval, "steps"])
    return(filled1)
}
```

**3. New dataset that is equal to the original dataset but with the missing data filled in**   


```r
filled_activity <- activity
filled_activity$steps <- mapply(fill_value, filled_activity$steps, filled_activity$interval)

head(filled_activity, n=10)
```

```
##        steps       date interval
## 1  1.7169811 2012-10-01        0
## 2  0.3396226 2012-10-01        5
## 3  0.1320755 2012-10-01       10
## 4  0.1509434 2012-10-01       15
## 5  0.0754717 2012-10-01       20
## 6  2.0943396 2012-10-01       25
## 7  0.5283019 2012-10-01       30
## 8  0.8679245 2012-10-01       35
## 9  0.0000000 2012-10-01       40
## 10 1.4716981 2012-10-01       45
```
**4. Make a histogram of the total number of steps taken each day** 



```r
total_steps <- tapply(filled_activity$steps, filled_activity$date, FUN=sum)

hist(total_steps, xlab="Number of Steps", ylab="Frecuency", main="Total Number of Steps by day")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->


**4b. Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?**  


```r
# Mean
mean(total_steps)
```

```
## [1] 10766.19
```

```r
#Median
median(total_steps)
```

```
## [1] 10766.19
```

```r
#The values are different. Missing data had an  influence in the median value. 
```




## Are there differences in activity patterns between weekdays and weekends?

*For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.*  
**1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.**


```r
weekday_or_weekend <- function(date) {
    day <- weekdays(date)
    if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
        return("weekday")
    else if (day %in% c("Saturday", "Sunday"))
        return("weekend")
    else
        stop("invalid date")
}
filled_activity$date <- as.Date(filled_activity$date)
filled_activity$day <- sapply(filled_activity$date, FUN=weekday_or_weekend)
```

**2. Two time series plot of the 5-minute interval (x) and the average number of steps taken averaged across weekday days or weekend days (y).**  


```r
averages <- aggregate(steps ~ interval + day, data=filled_activity, mean)

g<-ggplot(averages, aes(interval, steps)) 
g + geom_line(col="orange") + ggtitle("Average steps per Weekend or Weekday")+facet_grid(day ~ .) +
    xlab("5-minute interval") + ylab("Number of steps") + theme(plot.title = element_text(face="bold", size=18))
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->





