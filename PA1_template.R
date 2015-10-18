#
# R file corresponding to Rmd
#

activity <- read.csv("activity.csv")


# Transform the data
library(lubridate)
activity$date     <- ymd(activity$date)

# Calulate the total, mean and median steps for each day
library(plyr)
totalDailySteps <- ddply(activity, c("date"), summarize, steps = sum(steps))

hist(totalDailySteps$steps, 
     main="Histogram Of Total Steps Per Day", 
     xlab="Steps")
print(mean  (totalDailySteps$steps, na.rm=TRUE))
print(median(totalDailySteps$steps, na.rm=TRUE))

# Calulate the mean ACROSS days for each interval
stepsPerInterval <- ddply(activity, c("interval"), 
                          summarize, steps = mean(steps, na.rm=TRUE))
plot(x=stepsPerInterval$interval, 
     y=stepsPerInterval$steps, 
     type="l", 
     main="Mean Steps Per 5 Minute Interval",
     xlab="Interval",
     ylab="Steps")

# Calculate the most active interval
print(mostActiveInterval <- stepsPerInterval[stepsPerInterval$steps==
                                       max(stepsPerInterval$steps), ]$interval)
print(mostActiveInterval)


#
# Imputing missing values
totalMissingSteps <- sum(is.na(activity$steps))
totalMissing      <- sum(is.na(activity))
print(totalMissingSteps)
print(totalMissing)


imputation <- rep(stepsPerInterval$steps,61)
iactivity  <- activity
iactivity$steps <- ifelse(is.na(iactivity$steps), imputation, iactivity$steps)


totalDailySteps <- ddply(iactivity, c("date"), summarize, steps = sum(steps))

hist(totalDailySteps$steps, 
     main="Histogram Of Total Steps Per Day", 
     xlab="Steps")

print(mean  (totalDailySteps$steps, na.rm=TRUE))
print(median(totalDailySteps$steps, na.rm=TRUE))


#
#
# weekdays
library(ggplot2)
iactivity$dayType <- factor(ifelse((weekdays(iactivity$date)=="Saturday" |
                                    weekdays(iactivity$date)=="Sunday"     ), 
                                   "weekend", "weekday"))

# Calulate the mean ACROSS days for each interval
stepsPerInterval <- ddply(iactivity, c("interval", "dayType"), 
                          summarize, steps = mean(steps, na.rm=TRUE))

print(ggplot(data=stepsPerInterval, aes(interval, steps)) +
       geom_line() +
       facet_grid(dayType ~ .))  



