---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

## Loading and preprocessing the data

Read the data and process it: 

```r
all_activity <- read.csv("activity.csv")
activity <- all_activity[complete.cases(all_activity), ]
head (activity)
```

```
##     steps       date interval
## 289     0 2012-10-02        0
## 290     0 2012-10-02        5
## 291     0 2012-10-02       10
## 292     0 2012-10-02       15
## 293     0 2012-10-02       20
## 294     0 2012-10-02       25
```

## What is mean total number of steps taken per day?

 1. Calculate the total number of steps taken per day

  Calculate the steps taken each day:

```r
# Calculate the steps per day
steps_eachday <- aggregate(steps ~ date, activity, sum)
head (steps_eachday)
```

```
##         date steps
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```

2. Make a histogram of the total number of steps taken each day

Import ggplot 2 and plot the histogram:


```r
# Load ggplot2
library(ggplot2)

## Plot the histogram
hist(steps_eachday$steps, breaks = 20, main="Number of Steps", 
     xlab="Total number of steps taken each day", ylab = "Number of Days",
     col="purple")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

3. Calculate and report the mean and median of the total number of steps taken per day

Calulating the mean number of steps each day:

```r
mean(steps_eachday$steps)
```

```
## [1] 10766.19
```

Calculating the median number of steps each day:

```r
median(steps_eachday$steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

1. Make a time series plot (i.e. type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
# Calculate average steps per interval for all days 
Ag_steps_interval <- aggregate(steps ~ interval, activity, mean)

# Plot the time series with appropriate labels and heading
plot(Ag_steps_interval$interval, Ag_steps_interval$steps, type='l', main="Average number of steps per interval", xlab="Time Intervals", ylab="Average # of steps", col="green")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

The following interval has the highest number of steps:

```r
# Find interval index with the highest average steps
interval_higheststeps <- which.max(Ag_steps_interval$steps)
Ag_steps_interval[interval_higheststeps, ]$interval
```

```
## [1] 835
```

## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

Used complete cases to find the number of NA values (i.e. Missing values)

```r
missing_value <- all_activity[!complete.cases(all_activity), ]
nrow(missing_value)
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

For this problem, the strategy I used is that I used the mean for the 5 minute interval to replace the NA values in the data set:


```r
# For all activity rows, find the one with N/A for steps and find the interval for that row. Find the average steps for that row (Ag_steps_interval) and then replace the N/A with the average steps value for that interval. 

for (i in 1:nrow(all_activity)) 
  {
    if(is.na(all_activity$steps[i])) 
      {
        new_replace <-   Ag_steps_interval$steps[which(Ag_steps_interval$interval == all_activity$interval[i])]
        all_activity$steps[i] <- new_replace 
    }
}
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
replaced_steps <- aggregate(steps ~ date, all_activity, sum)
```

4. Make a histogram of the total number of steps taken each day

Made a new histogram with replaced steps:

```r
# Make a histogram

hist (replaced_steps$steps, breaks = 20, main = "Histogram of total # of steps per day", xlab = "Steps per day", ylab = "Number of Days", col = "red")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

Mean calculation with replaced NA values

```r
mean(replaced_steps$steps)
```

```
## [1] 10766.19
```

Median calculation with replaced NA values

```r
median(replaced_steps$steps)
```

```
## [1] 10766.19
```

As noticed by comparing the median and mean of the new dataset with the older one, one can notice that replacing the NA with the average # of steps for that interval has not had a huge impact on the data set and the distribution is relatively the same. The means of the two data sets match and median are only slightly different (but are in close proximity to eachother).

## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
# Method to differentiate between weekday and weekend by looking through the dates:

weekday <- function(date) {
    wd <- weekdays(as.Date(date, '%Y-%m-%d'))
    if  (!(wd == 'Sunday' || wd == 'Saturday')) {
        d <- 'weekday'
    } else {
        d <- 'weekend'
    }
    d
}

# Apply the weekday function to the processed data set
activity$day_type <- as.factor(sapply(activity$date, weekday))
```

2. Make a panel plot containing a time series plot (i.e.type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

Using ggplot to make the histogram:

```r
# Average number of steps taken averaged across weekday and weekends
steps_by_day <- aggregate(steps ~ interval+day_type, activity, mean)

# Make panel plot:
plt <- ggplot(steps_by_day, type='l', aes(interval, steps)) +
    geom_line (stat = "identity", aes(color = day_type)) +
    facet_grid (day_type ~ ., scales="fixed", space="fixed") +
    labs (x = "Interval", y = expression ("# of steps")) +
    ggtitle ("# of steps/interval on weekday vs. weekend")
print(plt)
```

![](PA1_template_files/figure-html/unnamed-chunk-15-1.png)<!-- -->

It appears that fewer steps were taken on weekend mornings relative to weekdays but more steps were taken on weekend evenings relative to weekdays. This makes sense in terms of people's activity on the weekdays versus on the weekends.
