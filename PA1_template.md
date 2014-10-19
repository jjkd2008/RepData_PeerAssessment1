# Reproducible Research: Peer Assessment 1
This document describes the details of analysis of a study tracking the number
of steps taken by the study subject over the course of 2 months.  The data is
located in a *.csv file entitled activity.csv.  It was downloaded on Oct 13, 2014
from the Coursera course website for the course Reproducible Research.

The variables included in this dataset are:

steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)

date: The date on which the measurement was taken in YYYY-MM-DD format

interval: Identifier for the 5-minute interval in which measurement was taken

There are a total of 17,568 observations in this dataset.

## Loading and preprocessing the data
The data was loaded into R and then processed into two processed datasets:
t - setting up individual columns for each date and rows for the time intervals
t2 - setting up individual columns for each time interval and rows for the dates
The two data sets will be used separately in the below analysis. 
This analysis requires the <i>reshape</i> and <i>plyr</i> packages

```r
require(reshape)
```

```
## Loading required package: reshape
```

```r
require(plyr)
```

```
## Loading required package: plyr
## 
## Attaching package: 'plyr'
## 
## The following objects are masked from 'package:reshape':
## 
##     rename, round_any
```

```r
require(xtable)
```

```
## Loading required package: xtable
```


```r
## Load the data & preprocess as necessary
dat <-read.csv(file='activity.csv')
#set up columns for each date with values of steps for each interval
t<-cast(dat,interval~date,value="steps")
# set up columns for each interval with values for steps for each date
t2<-cast(dat,date~interval,value="steps")
```

## What is mean total number of steps taken per day?
To calculate the mean number of steps per day, we first calculate the sum for
each day and then examine a histogram of the number of steps per day:

```r
t_sum<-colSums(t,na.rm=FALSE)
hist(t_sum)
```

![plot of chunk calc_sum](./PA1_template_files/figure-html/calc_sum.png) 

After this examination, we calculate the <strong>mean</strong> and <strong>median</strong>
of the number of steps per day.  In these calculations, the days with missing values
were ignored.

```r
t_mean <- mean(t_sum,na.rm=TRUE)
t_median <-median(t_sum,na.rm=TRUE)
```
The analysis determined that mean number of steps is 1.0766 &times; 10<sup>4</sup> and the median number
of steps is 1.0765 &times; 10<sup>4</sup>.


## What is the average daily activity pattern?
To find the average daily activity pattern, use the <strong>t2</strong> cast data set
which collected the time intervals in columns and the daily data as rows.  
Calculate a vector of column means (t2_mean) and then set up the plot characteristics
to generate the time series of means across each time interval.

```r
## find colmeans
t2_mean <-colMeans(t2,na.rm=TRUE)
## set value for x axis and the plot characteristics
x_int <-as.integer(names(t2)[-1])
title<-c("Mean number of steps per time interval \n measured across days")
yl<-c("Mean # of steps")
xl<-c("Time Interval (HHMM)")
plot(x_int,t2_mean,type="l",xlab=xl,ylab=yl,main=title)
```

![plot of chunk aver_daily_pattern](./PA1_template_files/figure-html/aver_daily_pattern.png) 

Within the set of means, the maximum value is determined by pulling the maximum
value from the <em>t2_mean</em> vector.

```r
## Which 5-min interval, on average across all days, contains max # of steps?
max_steps_interval<-names(which(t2_mean == max(t2_mean)))
```
The early morning interval beginning at 835 was found to have the largest
average steps per interval.

## Data processing to handle missing values
The next stage of the analysis was to determine a method for handling missing values within
the dataset.  As a first step, determine the number (and location) of rows with
missing values. 

```r
## Calc & report total # of missing values in dataset (ie total rows with NAs)
rows_na <- apply(t2, 1, function(x){any(is.na(x))})
num_rows_na <- sum(rows_na) ## report this in text
```
The vector <em>rows_na</em> is a logical vector storing T/F for each row with T
meaning the row contains missing values.  The total number of rows found is 8.

To deal with the missing data, replace the row entries with missing values with the
integer mean value of the steps for that time interval across all remaining, non-missing
days.

```r
## impute the missing data (describe the strategy)
## use the integer mean value of the steps for each time interval to replace NAs
## create new dataset with data imputed
tImpute<-t2
tImpute[rows_na,2:289]<-as.integer(t2_mean)
```

A question arises as to how this treatment of the missing values effects the overall
analysis.  To determine the answer for this question, re-create the analysis of the
dataset with ignored missing values on the imputed dataset.  First find totals for
each day (here stored in rows) and plot a histogram:

```r
## make hist of total # steps taken each day
tI_sum<-rowSums(tImpute)
hist(tI_sum)
```

![plot of chunk create_hist](./PA1_template_files/figure-html/create_hist.png) 

Next, compute the <strong>mean</strong> and <strong>median</strong> for the imputed
dataset and compare to results from original analysis:

```r
## Do these values differ from the estimates from the first part of the assignment?
## What is the impact of the imputation?
tI_mean <- mean(tI_sum)
tI_median <-median(tI_sum)
```
The analysis determined that mean number of steps is 1.075 &times; 10<sup>4</sup> and the median number
of steps is 1.0632 &times; 10<sup>4</sup> for the imputed data set.  Comparing with the original 
analysis shows the following:


```r
## create data for table export
tI_m<-c(tI_mean,tI_median)
t_m<-c(t_mean,t_median)
xtab<-data.frame(rbind(tI_m,t_m),row.names=c("Imputed Data","Drop Missing Data"))
names(xtab)<-c("Mean","Median")
xt<-xtable(xtab)
print(xt, type="html")
```

<!-- html table generated in R 3.1.1 by xtable 1.7-4 package -->
<!-- Sun Oct 19 15:54:05 2014 -->
<table border=1>
<tr> <th>  </th> <th> Mean </th> <th> Median </th>  </tr>
  <tr> <td align="right"> Imputed Data </td> <td align="right"> 10749.77 </td> <td align="right"> 10632.00 </td> </tr>
  <tr> <td align="right"> Drop Missing Data </td> <td align="right"> 10766.19 </td> <td align="right"> 10765.00 </td> </tr>
   </table>
The inclusion of the missing data rows imputed with the mean value for each interval
makes minor changes to the Mean (as expected using the mean for the imputation value)
and an approximately 1.2% change in the Median values which is again not signficant.

## Are there differences in activity patterns between weekdays and weekends?
To investigate any differences in activity pattern across weekdays versus weekends,
the imputed data set was used to create individual time series plots for the
collection of weekdays (i.e. Monday - Friday) and weekends (i.e. Saturday and Sunday).
To separate the data, create a factor variable with two values: <em>weekday</em>
and <em>weekend</em>.

```r
## using imputed data set
## create factor variable with two levels: weekday, weekend
tImpute_days <-weekdays(as.Date(tImpute$date))
tImpute_days <-revalue(tImpute_days,c("Monday"="weekday","Tuesday"="weekday",
                           "Wednesday"="weekday","Thursday"="weekday",
                           "Friday"="weekday","Saturday"="weekend","Sunday"="weekday"))
tImpute<-cbind(tImpute,tImpute_days)
```
The plot will use the newly created variable and calculated means for each
time interval across all days in the Imputed data set (tImpute).

```r
## make a panel plot containing a time series of 5-min interval v aver # of
## steps taken, averaged across all weekdays and weekend days
tI_mday<-colMeans(tImpute[tImpute$tImpute_days=="weekday",2:289])
tI_mend<-colMeans(tImpute[tImpute$tImpute_days=="weekend",2:289])
```
The plot below shows larger peaks on the weekend and more sustained activity in 
the afternoon and evening time intervals on the weekend.

```r
par(mfrow=c(2,1),mai=c(1,1,0.35,1))
title<-c("Mean number of steps per time interval")
yl<-c("Mean # of steps")
xl<-c("Time Interval (HHMM)")
plot(x_int,tI_mday,type="l",xlab="",ylab="Weekdays",main=title)
plot(x_int,tI_mend,type="l",xlab=xl,ylab="Weekends")
```

![plot of chunk timeseries](./PA1_template_files/figure-html/timeseries.png) 

