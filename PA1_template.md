---
title: "Course Project 1 - Reproducible Research"
author: "José Roberto S. Lima"
date: "May 22, 2016"
output: html_fragment
---
***
#Reporting the analysis of activity monitoring data  
***


```r
knitr::opts_chunk$set(autodep = TRUE, cache = TRUE, results = "hide")
```
This project consists in analysing an activity monitoring data in order to
proceed with the following tasks: 

##Loading and pre-processing the data  


```r
#Defining the non default packages needed to run entire code
library(lattice)

#Downloading the dataset, if necessary
zipLocal <- "repdata-data-activity.zip"
if (!file.exists(zipLocal)){
        zipUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
        download.file(zipUrl, zipLocal)
        unzip(zipLocal)
} else {unzip(zipLocal)}

#Reading the .csv file and preprocessing the data frame to ensure that it is
#------rightly ordered
actmd <- read.csv("activity.csv", stringsAsFactors = FALSE)
actmd$date <- as.Date(actmd$date, "%Y-%m-%d")
actmd <- actmd[order(actmd$date, actmd$interval), ]
```

The abreviation _**actmd**_ means from both bold and italic letters of _**act**_ivity _**m**_onitoring _**d**_ata. The data frame _**actmd**_ has the variables _steps_, _interval_ and _date_, as we can see from  


```r
str(actmd)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

##What is mean total number of steps taken per day?  

First, the number of _steps_ taken peer day is stored in the variable _**eachDay**_. Subsequently, in each day, a number of calculations gives us sum, mean and median of the _steps_ grouped. It is possible to see the sum calculations above in a pleasant way by a histogram plot.  



```r
#Separating and grouping $steps by $date
#Calculating using sapply() function: sum, mean and median of each group above
#Plotting the histogram of the total (sum) of each group above
eachDay <- split(actmd$steps, actmd$date)
total <- sapply(eachDay, sum, na.rm = TRUE)
average <- sapply(eachDay, mean, na.rm = TRUE)
middle <- sapply(eachDay, median, na.rm = TRUE)
hist(total, col = "green", main = "Total Steps in each Day", ylim = c(0, 10),
     breaks = 60)
rug(total)
```

![plot of chunk histogram](figure/histogram-1.png)

##What is the average daily activity pattern?  

By now, the steps are grouped by each 5-minute interval, considering all days.
The _**eachInterval**_ organize this data. Besides, a time series plot shows the mean of these groups -- the _**average2**_ vareable -- over each interval.  


```r
#Separating and grouping $steps by $interval
#Calculating using sapply function: mean of each group above
#Ploting the time series of the average (mean) of each group above
eachInterval <- split(actmd$steps, actmd$interval)
average2 <- sapply(eachInterval,mean, na.rm = TRUE)
plot(1:length(unique(actmd$interval)), average2,type = "l", main = "", xlab = "",
     ylab = "")
title(main = "Mean of Steps in each 5-minute Interval", ylab = "Average",
      xlab = "Interval (5-minute)")
```

![plot of chunk timeSplot](figure/timeSplot-1.png)


```r
#The 5-minute interval in which the average (defined above) is maximum
maxAverage <- max(average2)
maxInterval <- unique(actmd$interval)[maxAverage == average2]
```
Also, we got the maximum average value _**maxAverage**_ = 206.169811320755 and the repective 5-minute interval _**maxInterval**_ = 835, corresponding to interval 13h50-55min.  

***

##Inputing missing values

As we saw previously using _**str(actmd)**_, there are a number of NAs values in _**actmd$steps**_. Precisely, we have  


```r
with(actmd, table(validySteps = !is.na(steps)))
```

```
## validySteps
## FALSE  TRUE 
##  2304 15264
```

Clearly, considering the code above, the logical value _**FALSE**_ counts the NAs
that belongs to _**actmd$steps**_. In this way, we can confirm this information as follows


```r
numNA <- dim(actmd[!complete.cases(actmd), ])[1]
numNA
```

```
## [1] 2304
```

Now, our strategy to overcome this problem with missing values is filling in all of the NAs by the mean for each 5-minute interval, in all days. The new dataset _**actmdnoNA**_ was obtained after this filling in operation.  


```r
#Filling in the NAs with the mean for each 5-minute interval within actmdnoNA
#Como otimizar? utilizando _apply() functions?
actmdnoNA <- actmd
replaceNA <- rep(average2, length(actmd$interval)/length(average2))
for(i in 1:length(actmd$steps)){
        if(is.na(actmd$steps[i])){
                actmdnoNA$steps[i] <- replaceNA[i]
        }
}
```

The operations within this new dataset will be represented by the sufix _noNA_ added to the original variable names. Another time, the number of _steps_ taken peer day is stored. And again, the sum, mean and median of the _steps_ grouped are calculated.


```r
#Separating and grouping $steps by $date of "actmdnoNA" (actmd with NAs filled 
#------in by the mean for each 5-minute interval)
#Calculating using sapply() function: sum, mean and median of each group above
#Plotting the histogram of the total (sum) of each group above
eachDaynoNA <- split(actmdnoNA$steps, actmdnoNA$date)
totalnoNA <- sapply(eachDaynoNA, sum)
averagenoNA <- sapply(eachDaynoNA, mean)
middlenoNA <- sapply(eachDaynoNA, median)
hist(totalnoNA, col = "green", main = "Total Steps in each Day (without NAs)",
     breaks = 60)
rug(totalnoNA)
```

![plot of chunk histogram2](figure/histogram2-1.png)

The comparisson between the two histogram plots shows that the filling in operation made the median terms became more frequent -- median terms of the data as the histogram organizes.

##Are there differences in activity patterns between weekdays and weekends?

Here, we must to solve the problem of creating a new factor variable, which indicates whether the current day is in _weekday_ or in _weekend_.  
This task presents some complications, the main is that we have to sort the initial dates to ensure that the first one is a known day (as **monday**, for instance). In this way, we easily identify the days on weekends.  


```r
#Sorting appropriately days in the week (monday as first day, tuesday as second 
#------day and so on)
#Creating the new factor variable "week" with levels "weekday" and "weekend"
auxorder <- 0
auxMonday <- ((actmdnoNA$date[1] + auxorder) - as.Date("2016-05-16",
                                                       "%Y-%m-%d"))/7 
#May 16th 2016 - easily verified as Monday
auxMonday <- auxMonday - floor(auxMonday)
while (auxMonday != 0) {
        auxorder <- auxorder + 1
        auxMonday <- ((actmdnoNA$date[1] + auxorder) - as.Date("2016-05-02",
                                                               "%Y-%m-%d"))/7 
        #May 16th 2016 - easily verified as Monday
        auxMonday <- auxMonday - floor(auxMonday)
}
uweekdates <- unique(weekdays(actmdnoNA$date + auxorder))
weekdates <- weekdays(actmdnoNA$date)
actmdnoNA$week <- rep("weekday", dim(actmdnoNA)[1])
for (j in 1:dim(actmdnoNA)[1]) {
        if(weekdates[j] == uweekdates[6] | weekdates[j] == uweekdates[7]) {
                actmdnoNA$week[j] <- "weekend"
        }
}
actmdnoNA$week <- factor(actmdnoNA$week)
```

After, in order to make panel plot using the _**lattice**_ package, we must to create a new data frame _**newdata**_. The graph contains a time series plot of the 5-minute _interval_ (x-axis) and the average number of _steps_ taken, averaged across all weekday days or weekend days (y-axis).  


```r
data1 <- subset(actmdnoNA, week == "weekday")
data2 <- subset(actmdnoNA, week == "weekend")
#Com certeza há uma função _apply() para fazer isso!

eachInt1 <- split(data1$steps, data1$interval)
eachInt2 <- split(data2$steps, data2$interval)
avr1 <- sapply(eachInt1, mean)
avr2 <- sapply(eachInt2, mean)

data1 <- data.frame(avr = avr1, interval = unique(actmdnoNA$interval),
                    week = rep("weekday", length(avr1)))
data2 <- data.frame(avr = avr2, interval = unique(actmdnoNA$interval),
                    week = rep("weekend", length(avr2)))
newdata <- rbind(data1, data2)

xyplot(avr ~ interval | week, data = newdata, type = "l", layout = c(1,2))
```

![plot of chunk timeSplot2](figure/timeSplot2-1.png)
