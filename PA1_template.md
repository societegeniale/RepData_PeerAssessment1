---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
1. Load the data (i.e. read.csv())

```r
library(data.table)

if( !file.exists("activity.csv")) unzip("activity.zip")
stepsData <- fread("activity.csv")
```

2. Process/transform the data (if necessary) into a format suitable for your analysis

```r
stepsData$date <- as.Date(stepsData$date,"%Y-%m-%d")
```

## What is mean total number of steps taken per day?
1. Make a histogram of the total number of steps taken each day

```r
library(ggplot2)

stepsSum <- stepsData[,.(steps=sum(steps)),by=date]

ggplot(data=stepsSum, mapping = aes(x=steps) ) + geom_histogram()
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

2. Calculate and report the mean and median total number of steps taken per day

```r
avgStepsSum <- stepsSum[,.(average=mean(steps,na.rm = TRUE),median=median(steps,na.rm = TRUE))]
print(avgStepsSum)
```

```
##     average median
## 1: 10766.19  10765
```

## What is the average daily activity pattern?
1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
intervalMean <- stepsData[,.(steps=mean(steps, na.rm=TRUE)),by=interval]

ggplot(data=intervalMean, mapping = aes(x=interval,y=steps) ) + geom_line()
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
intervalMean$interval[which.max(intervalMean$steps)]
```

```
## [1] 835
```

## Imputing missing values
1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
nbNA <- sum(is.na(stepsData$steps))
paste0("Number of missing values: ", nbNA, " (", round(nbNA/nrow(stepsData)*100,digits=2), "%)")
```

```
## [1] "Number of missing values: 2304 (13.11%)"
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

We can see that a few full days are missing (288 is the numbers of interval in a day): 

```r
stepsData[,sum(is.na(steps)),by=date][V1!=0]
```

```
##          date  V1
## 1: 2012-10-01 288
## 2: 2012-10-08 288
## 3: 2012-11-01 288
## 4: 2012-11-04 288
## 5: 2012-11-09 288
## 6: 2012-11-10 288
## 7: 2012-11-14 288
## 8: 2012-11-30 288
```
So lets replace those missings days with a "typical day" where each interval is the mean interval accross all days

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
NAsIdx <- which(is.na(stepsData$steps))
stepsDataFilled <- stepsData
for(i in NAsIdx) {
  stepsDataFilled[i, steps := as.integer(round(intervalMean[interval == stepsDataFilled[i,interval], steps]))]
}
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
stepsFilledSum <- stepsDataFilled[,.(steps=sum(steps)),by=date]

ggplot(data=stepsFilledSum, mapping = aes(x=steps) ) + geom_histogram()
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png)

The histogram looks the same as the last one.

The mean and median are:

```r
avgStepsFilledSum <- stepsFilledSum[,.(average=mean(steps,na.rm = TRUE),median=median(steps,na.rm = TRUE))]
print(avgStepsFilledSum)
```

```
##     average median
## 1: 10765.64  10762
```
They do not differ very much from the values of the begining. The difference in percentage is:

```r
(avgStepsFilledSum-avgStepsSum)/avgStepsSum*100 
```

```
##         average      median
## 1: -0.005102409 -0.02786809
```


## Are there differences in activity patterns between weekdays and weekends?
1.Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
stepsDataFilled[,weekend:=factor(sapply(stepsData$date, 
                          function(d) {if(weekdays(d)  %in% c("samedi","dimanche")) "weekend" else "weekday" } ))]
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

```r
weeklyMean <- stepsDataFilled[,.(steps=mean(steps, na.rm=TRUE)),by=.(interval,weekend)]
ggplot(data=weeklyMean, mapping = aes(x=interval,y=steps) ) + facet_grid(weekend~.) + geom_line()
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14-1.png)
