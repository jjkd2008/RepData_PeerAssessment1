## working script for the Reproducible Research Project 1
## assumptions
## file activity.csv is located in the working directory

## required packages (if any)
require(reshape)
require(plyr)

## main steps

## Load the data & preprocess as necessary
dat <-read.csv(file='activity.csv')
#set up columns for each date with values of steps for each interval
t<-cast(dat,interval~date,value="steps")
# set up columns for each interval with values for steps for each date
t2<-cast(dat,date~interval,value="steps")

## What is the mean total # of steps taken per day (ignoring missing data)?

## create histogram
## calculate total steps taken per day
t_sum<-colSums(t,na.rm=FALSE)
hist(t_sum)
## calc & report mean/median total # steps taken per day
t_mean <- mean(t_sum,na.rm=TRUE)
t_median <-median(t_sum,na.rm=TRUE)
## report in the text using variable references



## What is the average daily activity pattern?

## time series plot of 5-min interval v. average # steps taken (averaged across all days)
## use the t2 cast data set
## find colmeans
t2_mean <-colMeans(t2,na.rm=TRUE)
## set value for x axis and the plot characteristics
x_int <-as.integer(names(t2)[-1])
title<-c("Mean number of steps per time interval \n measured across days")
yl<-c("Mean # of steps")
xl<-c("Time Interval (HHMM)")
plot(x_int,t2_mean,type="l",xlab=xl,ylab=yl,main=title)
## Which 5-min interval, on average across all days, contains max # of steps?
max_steps_interval<-names(which(t2_mean == max(t2_mean)))
## report in text


## Impute missing values

## Calc & report total # of missing values in dataset (ie total rows with NAs)
rows_na <- apply(t2, 1, function(x){any(is.na(x))})
num_rows_na <- sum(rows_na) ## report this in text

## impute the missing data (describe the strategy)
## use the integer mean value of the steps for each time interval to replace NAs
## create new dataset with data imputed
tImpute<-t2
tImpute[rows_na,2:289]<-as.integer(t2_mean)

## make hist of total # steps taken each day
tI_sum<-rowSums(tImpute)
hist(tI_sum)

## calc & report mean/median total number of steps taken per day
## Do these values differ from the estimates from the first part of the assignment?
## What is the impact of the imputation?
tI_mean <- mean(tI_sum)
tI_median <-median(tI_sum)

## create data for table export
tI_m<-c(tI_mean,tI_median)
t_m<-c(t_mean,t_median)
xtab<-data.frame(rbind(tI_m,t_m),row.names=c("Imputed Data","Drop Missing Data"))
names(xtab)<-c("Mean","Median")
xt<-xtable(xtab)
print(xt, type="html")
## Are there differences in activity patterns between weekdays and weekends?
## using imputed data set
## create factor variable with two levels: weekday, weekend
tImpute_days <-weekdays(as.Date(tImpute$date))
tImpute_days <-revalue(tImpute_days,c("Monday"="weekday","Tuesday"="weekday",
                           "Wednesday"="weekday","Thursday"="weekday",
                           "Friday"="weekday","Saturday"="weekend","Sunday"="weekday"))
tImpute<-cbind(tImpute,tImpute_days)

## make a panel plot containing a time series of 5-min interval v aver # of
## steps taken, averaged across all weekdays and weekend days
tI_mday<-colMeans(tImpute[tImpute$tImpute_days=="weekday",2:289])
tI_mend<-colMeans(tImpute[tImpute$tImpute_days=="weekend",2:289])

par(mfrow=c(2,1),mai=c(1,1,0.35,1))
title<-c("Mean number of steps per time interval")
yl<-c("Mean # of steps")
xl<-c("Time Interval (HHMM)")
plot(x_int,tI_mday,type="l",xlab="",ylab="Weekdays",main=title)
plot(x_int,tI_mend,type="l",xlab=xl,ylab="Weekends")
