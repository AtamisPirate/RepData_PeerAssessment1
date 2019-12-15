---
title: 'Reproducible Research: Peer Assessment 1'
output:
  html_document:
    keep_md: yes
  pdf_document: default
  word_document: default
---



```r
library(ggplot2)
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
## Loading and preprocessing
path <- getwd()
activity_Wmissing <- read.csv("activity.csv")
activity <-na.omit(activity_Wmissing)
##What is the mean total number of steps taken per day?
perdayactivity <- aggregate(activity$steps, by=list(activity$date), FUN =sum)
colnames(perdayactivity) <- c("date","steps")
hist(perdayactivity$steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

```r
mean(perdayactivity$steps)
```

```
## [1] 10766.19
```

```r
median(perdayactivity$steps)
```

```
## [1] 10765
```

```r
##What is the daily activity patern?
average_interval <- aggregate(activity$steps~interval,activity, mean)
colnames(average_interval) <- c("interval","steps")
plot(average_interval$interval, average_interval$steps, type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-1-2.png)<!-- -->

```r
interval_row<-which.max(average_interval$steps)
max(average_interval$steps[[interval_row]])
```

```
## [1] 206.1698
```

```r
max(average_interval$interval[[interval_row]])
```

```
## [1] 835
```

```r
##Missing value handling by replacing the missing value with the average
fixedmissing <- activity
for (i in 1:nrow(activity_Wmissing)) {
        if(is.na(activity_Wmissing$steps[i]) == TRUE) {
           #temp <- join(average_interval,temptable[1,], by = "interval", type = "right")
           temp1 <- merge(average_interval, activity_Wmissing[i,], by = "interval")
           temp2 <- select(temp1,-"steps.y" )
           colnames(temp2) <- c("interval","steps","date")
           fixedmissing <- rbind(fixedmissing,temp2)
           
        }
}




activity_replaced_daily <- aggregate(fixedmissing$steps, by=list(fixedmissing$date), FUN =sum)
colnames(activity_replaced_daily) <- c("date","steps")

hist(activity_replaced_daily$steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-1-3.png)<!-- -->

```r
mean(activity_replaced_daily$steps)
```

```
## [1] 10766.19
```

```r
median(activity_replaced_daily$steps)                
```

```
## [1] 10766.19
```

```r
##Are there differences in activity patterns between weekdays and weekends?


fixedmissing$ActualDate <- as.Date(fixedmissing$date, format = "%Y-%m-%d")
fixedmissing$weekday <- weekdays(fixedmissing$ActualDate)
fixedmissing$Weekdayorweekend <- ifelse(fixedmissing$weekday=='Saturday' | fixedmissing$weekday=='Sunday', 'weekend','weekday')


weekday_interval <- ggplot(fixedmissing, aes(x=interval, y=steps)) + 
        facet_grid(Weekdayorweekend~.) +
        geom_line() 
        
weekday_interval
```

![](PA1_template_files/figure-html/unnamed-chunk-1-4.png)<!-- -->
                
