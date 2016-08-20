---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```r
library(ggplot2)
library(plyr)
```

```r
data.all <- read.csv(file = 'activity.csv', header = TRUE, colClasses = c('numeric', 'Date', 'numeric'))

totalSteps<-aggregate(steps~date,data=data.all,sum,na.rm=FALSE)
```

## What is mean total number of steps taken per day?

```r
meanSteps.byday<-aggregate(steps~date,data=totalSteps,mean,na.rm=FALSE)
meanSteps.byday
meanSteps.total <- mean(totalSteps$steps, na.rm = FALSE)
meanSteps.total
```

## What is the average daily activity pattern?

```r
hist(totalSteps$steps, breaks = 10, xlab = 'Steps by Day', ylab = 'Freq')
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

```r
ggplot(data = meanSteps.byday, aes(date, steps)) + 
  geom_line() +
  scale_x_date(date_breaks = "1 day") + 
  ggtitle("Mean Steps by Day")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-2.png)

## Imputing missing values

```r
replace.with.mean <- function(x, fun) { 
  missing <- is.na(x) 
  replace(x, missing, fun(x[!missing])) 
} 


data.transformed <- ddply(data.all, ~ date, transform, steps = replace.with.mean(steps, mean))

data.transformed[is.na(data.transformed)] <- 0
```

## Are there differences in activity patterns between weekdays and weekends?

```r
data.transformed.weekday <- subset(data.transformed,  weekdays(date) == 'Saturday' | weekdays(date) == 'Sunday')
data.transformed.weekend <- subset(data.transformed, weekdays(date) != 'Saturday' & weekdays(date) != 'Sunday')


meanSteps.byweekend.transform<-aggregate(steps~interval,data=data.transformed.weekend,mean)
meanSteps.byweekday.transform<-aggregate(steps~interval,data=data.transformed.weekday,mean)

weekend <- ggplot(data = meanSteps.byweekend.transform, aes(interval, steps)) + 
  geom_line() +
  ggtitle("Mean Steps by Interval for Weekend")

weekday <- ggplot(data = meanSteps.byweekday.transform, aes(interval, steps)) + 
  geom_line() +
  ggtitle("Mean Steps by Interval for Week Days")


multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
```