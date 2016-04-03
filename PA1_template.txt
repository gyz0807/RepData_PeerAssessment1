# Reproducible Research: Peer Assessment 1



## Loading and preprocessing the data

```r
library(dplyr); library(lubridate)
activity <- read.csv("activity.csv") %>%
        tbl_df()
```

## What is mean total number of steps taken per day?
For this part of the assignment, you can ignore the missing values in the dataset.

1. Calculate the total number of steps taken per day

```r
total_steps <- sum(activity$steps, na.rm = TRUE)
total_steps
```

```
## [1] 570608
```

2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day

```r
group_day <- activity %>%
        mutate(date = ymd(date)) %>%
        group_by(date) %>%
        filter(!is.na(steps)) %>%
        summarise(steps = sum(steps))

hist(group_day$steps, main = "Total # of Steps Take Each Day",
     xlab = "Total # of Steps Each Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)

3. Calculate and report the mean and median of the total number of steps taken per day

```r
summary(group_day$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    8841   10760   10770   13290   21190
```

## What is the average daily activity pattern?
1. Make a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
summarize_interval <- activity %>%
        filter(!is.na(steps)) %>%
        group_by(interval) %>%
        summarize(mean = mean(steps))
with(summarize_interval, plot(interval, mean, type="l", xlab="Interval",
                              ylab = "Average Number of Steps"))
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
max_row <- which.max(summarize_interval$mean)
summarize_interval[max_row, "interval"]
```

```
## Source: local data frame [1 x 1]
## 
##   interval
##      (int)
## 1      835
```

## Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as 𝙽𝙰). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with 𝙽𝙰s)

```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

```r
# Use the mean for that 5-minute interval to fill out NAs
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
na_row <- which(is.na(activity$steps))
interval_na_row <- activity$interval[na_row]
mean_na_row <- data.frame(interval=interval_na_row)
mean_na_row$order <- 1:nrow(mean_na_row)
mean_na_row <- merge(mean_na_row, summarize_interval, by.x = "interval", 
                     by.y = "interval")
mean_na_row <- mean_na_row[order(mean_na_row$order),]
activity[na_row, "steps"] <- mean_na_row$mean
head(activity)
```

```
## Source: local data frame [6 x 3]
## 
##       steps       date interval
##       (dbl)     (fctr)    (int)
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
group_day <- activity %>%
        mutate(date = ymd(date)) %>%
        group_by(date) %>%
        filter(!is.na(steps)) %>%
        summarise(steps = sum(steps))

hist(group_day$steps, main = "Total # of Steps Take Each Day",
     xlab = "Total # of Steps Each Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)

```r
summary(group_day$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9819   10770   10770   12810   21190
```
The values differ from the estimates from the first part of the assignment.  
Imputing missing values enlarges 1st Qu. and Median, and diminishes 3rd Qu.

## Are there differences in activity patterns between weekdays and weekends?
For this part the 𝚠𝚎𝚎𝚔𝚍𝚊𝚢𝚜() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
library(chron)
new_activity <- activity %>%
        mutate(date = ymd(date), weekend = is.weekend(date))
new_activity$weekend[new_activity$weekend==FALSE] <- "weekday"
new_activity$weekend[new_activity$weekend=="TRUE"] <- "weekend"
new_activity <- new_activity %>%
        mutate(weekend = as.factor(weekend))
new_activity
```

```
## Source: local data frame [17,568 x 4]
## 
##        steps       date interval weekend
##        (dbl)     (time)    (int)  (fctr)
## 1  1.7169811 2012-10-01        0 weekday
## 2  0.3396226 2012-10-01        5 weekday
## 3  0.1320755 2012-10-01       10 weekday
## 4  0.1509434 2012-10-01       15 weekday
## 5  0.0754717 2012-10-01       20 weekday
## 6  2.0943396 2012-10-01       25 weekday
## 7  0.5283019 2012-10-01       30 weekday
## 8  0.8679245 2012-10-01       35 weekday
## 9  0.0000000 2012-10-01       40 weekday
## 10 1.4716981 2012-10-01       45 weekday
## ..       ...        ...      ...     ...
```

2. Make a panel plot containing a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
group_weekday <- new_activity %>%
        group_by(weekend, interval) %>%
        summarize(mean = mean(steps))

library(ggplot2)
ggplot(group_weekday, aes(interval, mean)) + 
        facet_grid(weekend~.) + 
        geom_line() +
        xlab("Interval") + ylab("Number of Steps") +
        ggtitle("Average Number of Steps (weekday vs weekend)")
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)
