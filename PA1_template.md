---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
#### 1. Load the data (i.e. read.csv())

```r
MyActivity <- read.csv("activity.csv")
```
#### 2. Process/transform the data (if necessary) into a format suitable for your analysis

```r
head(MyActivity)
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

```r
str(MyActivity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

## What is mean total number of steps taken per day?
#### 1. Calculate the total number of steps taken per day

```r
Tot_Steps_day <- tapply(MyActivity$steps, MyActivity$date, sum, na.rm = TRUE)
```
* Total number of steps taken per day: 0, 126, 11352, 12116, 13294, 15420, 11015, 0, 12811, 9900, 10304, 17382, 12426, 15098, 10139, 15084, 13452, 10056, 11829, 10395, 8821, 13460, 8918, 8355, 2492, 6778, 10119, 11458, 5018, 9819, 15414, 0, 10600, 10571, 0, 10439, 8334, 12883, 3219, 0, 0, 12608, 10765, 7336, 0, 41, 5441, 14339, 15110, 8841, 4472, 12787, 20427, 21194, 14478, 11834, 11162, 13646, 10183, 7047, 0

#### 2. Make a histogram of the total number of steps taken each day

```r
hist(Tot_Steps_day, 
      xlab = "Steps per Day", 
      main = "Total number of steps taken per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

#### 3. Calculate and report the mean and median total number of steps taken per day

```r
mean_Steps   <- mean(Tot_Steps_day)
median_Steps <- median(Tot_Steps_day)
```
* Mean: 9354.2295082
* Median:  10395

## What is the average daily activity pattern?

#### 1. Make a time series plot (i.e. \color{red}{\verb|type = "l"|}type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
Ave_Steps_Interval <- tapply(MyActivity$steps, MyActivity$interval, mean, na.rm = TRUE)
plot(unique(MyActivity$interval),Ave_Steps_Interval, 
      xlab = "Intervals", 
      ylab = "Total steps per interval", 
      main = "Average of Number of steps per interval",
      pch = 20)
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

#### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
max_Steps <- max(Ave_Steps_Interval)
max_interval <- unique(MyActivity$interval)[which(Ave_Steps_Interval == max(Ave_Steps_Interval))]
```
* Interval with maximum number of steps: 835

## Imputing missing values
#### 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with \color{red}{\verb|NA|}NAs)

```r
TotNas <- sum(is.na(MyActivity))
```

* Number of missing values: 2304

#### 2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
#### 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
UpdatedMyActivity <- MyActivity


MeanSteps <- tapply(MyActivity$steps, MyActivity$interval, mean, na.rm = TRUE)
MyActivity_NAs    <- MyActivity[ is.na(MyActivity$steps),]
MyActivity_nonNAs <- MyActivity[!is.na(MyActivity$steps),]
MyActivity_NAs$steps <- as.factor(MyActivity_NAs$interval)
levels(MyActivity_NAs$steps) <- MeanSteps
levels(MyActivity_NAs$steps) <- as.numeric(levels(MyActivity_NAs$steps))
MyActivity_NAs$steps <- as.integer(as.vector(MyActivity_NAs$steps))
UpdatedMyActivity <- rbind(MyActivity_NAs, MyActivity_nonNAs)
```

#### 4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
Tot_Steps_day2 <- tapply(UpdatedMyActivity$steps, UpdatedMyActivity$date, sum)
hist(Tot_Steps_day2, 
      xlab = "Steps per Day", 
      main = "Total number of steps taken per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

```r
mean_Steps2   <- mean(Tot_Steps_day2)
median_Steps2 <- median(Tot_Steps_day2)
```

* Mean (Imputed): 1.074977\times 10^{4}
* Median (Imputed):  10641


## Are there differences in activity patterns between weekdays and weekends?
#### 1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
UpdatedMyActivity$Weektype <- ifelse(weekdays(as.Date(UpdatedMyActivity$date)) == "Saturday" | weekdays(as.Date(UpdatedMyActivity$date)) == "Sunday", "weekend", "weekday" )
```

#### 2. Make a panel plot containing a time series plot (i.e. \color{red}{\verb|type = "l"|}type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
StepsIntervalWeekday <- aggregate(steps ~ interval + Weektype, data = UpdatedMyActivity, FUN = mean)
names(StepsIntervalWeekday) <- c("interval", "Weektype", "meansteps")

library(ggplot2)
pp <- ggplot(StepsIntervalWeekday, aes(interval, meansteps))
pp + geom_point() + facet_grid(Weektype~.) + labs(x = "Intervals", y = "Average Steps", title = "Activity Patterns")
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->
