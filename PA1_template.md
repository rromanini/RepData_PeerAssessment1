Activity Monitoring
================

Loading and preprocessing the data
----------------------------------

``` r
library(ggplot2)
library(plyr)
```

    ## Warning: package 'plyr' was built under R version 3.2.5

1-Load the data (i.e. read.csv())

``` r
# set work directory
setwd("c:/rui/data science/")

#Loading and preprocessing the data
activityMonitor = read.csv("activity.csv")
```

2-Process/transform the data (if necessary) into a format suitable for your analysis In this case, I choose to create another dataset, without NA values

``` r
actMonitorWithoutNA = activityMonitor[which(!is.na(activityMonitor$steps)),]
```

What is mean total number of steps taken per day?
-------------------------------------------------

1-Calculate the total number of steps taken per day

``` r
totalByDay = aggregate(actMonitorWithoutNA$steps, by=list(date=actMonitorWithoutNA$date), FUN=sum)
```

2-If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day

``` r
hist(
  totalByDay$x,
  col = "blue",
  main = "Histogram of the Total Number of Steps Taken Each Day",
  xlab = "Total Number of Steps Taken Each Day",
  breaks = 20
)
```

![](figure/unnamed-chunk-5-1.png)

3-Calculate and report the mean and median of the total number of steps taken per day

``` r
mean(totalByDay$x)
```

    ## [1] 10766.19

``` r
median(totalByDay$x)
```

    ## [1] 10765

What is the average daily activity pattern?
-------------------------------------------

1-Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

``` r
averageDaily <- aggregate(x=list(steps=actMonitorWithoutNA$steps), 
                      by=list(interval=actMonitorWithoutNA$interval),FUN=mean)
ggplot(data=averageDaily, aes(x=interval, y=steps)) +
  geom_line() +
  xlab("5-minute interval") +
  ylab("average number of steps taken")
```

![](figure/unnamed-chunk-7-1.png)

2-Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

``` r
averageDaily[which.max(averageDaily$steps),]
```

    ##     interval    steps
    ## 104      835 206.1698

Imputing missing values
-----------------------

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1-Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

``` r
nrow(activityMonitor[which(is.na(activityMonitor$steps)),])
```

    ## [1] 2304

2-Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

In this case, I used the mean of the non NA values and created a new dataset called newActivityMonitor

3-Create a new dataset that is equal to the original dataset but with the missing data filled in.

``` r
getMean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
newActivityMonitor = ddply(activityMonitor, ~ interval, transform, steps = getMean(steps))
```

4-Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

``` r
newTotalByDay = aggregate(newActivityMonitor$steps, by=list(date=newActivityMonitor$date), FUN=sum)
hist(
  newTotalByDay$x,
  col = "blue",
  main = "Histogram of the Total Number of Steps Taken Each Day",
  xlab = "Total Number of Steps Taken Each Day",
  breaks = 20
)
```

![](figure/unnamed-chunk-11-1.png)

``` r
mean(newTotalByDay$x)
```

    ## [1] 10766.19

``` r
median(newTotalByDay$x)
```

    ## [1] 10766.19

There is no significant impact.

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1-Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

``` r
Sys.setlocale("LC_TIME", "English")
```

    ## [1] "English_United States.1252"

``` r
isWeekend = weekdays(as.Date(newActivityMonitor$date)) %in% c("Saturday", "Sunday")
newActivityMonitor$daytype = factor(isWeekend, levels = c(FALSE, TRUE), labels = c("weekday", "weekend"))
```

2-Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

``` r
par(mfrow = c(2, 1))
# Weekdays
weekdays = newActivityMonitor[newActivityMonitor$daytype == "weekday", ]

averageWeekDay <- aggregate(x=list(steps=weekdays$steps), 
                          by=list(interval=weekdays$interval),FUN=mean)
ggplot(data=averageWeekDay, aes(x=interval, y=steps)) +
  geom_line() +
  xlab("5-minute interval") +
  ylab("average number of steps taken") +
  ggtitle("Average Week Day")
```

![](figure/unnamed-chunk-13-1.png)

``` r
# Weekends
weekends = newActivityMonitor[newActivityMonitor$daytype == "weekend", ]

averageWeekend <- aggregate(x=list(steps=weekends$steps), 
                            by=list(interval=weekends$interval),FUN=mean)
ggplot(data=averageWeekend, aes(x=interval, y=steps)) +
  geom_line() +
  xlab("5-minute interval") +
  ylab("average number of steps taken") +
  ggtitle("Average Weekend")
```

![](figure/unnamed-chunk-13-2.png)
