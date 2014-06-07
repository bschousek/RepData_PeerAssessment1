# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
This code assumes that the file *activity.zip* is in the current working directory. 
First we unzip it, then load the generated *activity.csv* into the workspace.
At this time we also convert the date variable into POSIXct format using the
lubridate mdy() function.  
Also create an aggregated day by day total step count


```r
library(lubridate)
unzip("activity.zip")
raw_activity<-read.csv("activity.csv",header=T,sep=",")
activity<-data.frame(date=ymd(raw_activity$date))
interval<-sprintf("%04d",raw_activity$interval)
activity$interval<-as.POSIXct(strptime(interval,"%H%M"))
activity$steps=raw_activity$steps
```


## What is mean total number of steps taken per day?
Next we aggregate across all days and plot a histogram  
* add breaks=10 to give a little finer granularity on the histogram  
* add annotation with mean and median

```r
daybyday<-aggregate(steps~date,data=activity,sum)

maintitle<-paste("Histogram of steps per day\n",
                 "n=",nrow(daybyday)," days",
                 "; mean=",round(mean(daybyday$steps)),
                 "; median=",round(median(daybyday$steps)),
                 sep="")
hist(daybyday$steps,breaks=10,main=maintitle,xlab="Number of steps")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 
## What is the average daily activity pattern?

```r
averageday<-aggregate(steps~interval,data=activity,mean)
with(averageday,plot(steps~interval,type="l"))
max_steps<-averageday[which.max(averageday$steps),]
points(max_steps$interval,max_steps$steps)
text(max_steps$interval,max_steps$steps,pos=4,
     paste("Maximum",round(max_steps$steps),"Steps at",
           strftime(max_steps$interval, format="%H:%M")))
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 


## Imputing missing values
How many NAs are there total?

```r
narows<-is.na(activity$steps)
sum(narows)
```

```
## [1] 2304
```
Now for each of those create data equal to the mean for the interval as calculated in averageday

```r
imputers<-lapply(which(narows),function(x) {
        averageday$steps[which(averageday$interval==activity$interval[x])]})
imputed.activity<-activity
imputed.activity$steps[narows]<-simplify2array(imputers)
```

```r
daybyday<-aggregate(steps~date,data=imputed.activity,sum)

maintitle<-paste("Histogram of steps per day\n",
                 "n=",nrow(daybyday)," days",
                 "; mean=",round(mean(daybyday$steps)),
                 "; median=",round(median(daybyday$steps)),
                 sep="")
hist(daybyday$steps,breaks=10,main=maintitle,xlab="Number of steps")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 
## Are there differences in activity patterns between weekdays and weekends?
