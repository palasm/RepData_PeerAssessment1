## Loading and preprocessing the data
## 1. Show any code that is needed to Load the data (i.e. read.csv())
activity <- read.csv("activity.csv")

## 2. Process/transform the data (if necessary) into a format suitable for your analysis 
## Check Data set 
head(activity)

##Check the summary of the data set just to get bit more idea on source data set 
summary(activity)


## Ignore NA data set 
activityNotNA <- na.omit(activity)

## Check if the NA data is there 
summary(activityNotNA) 
## Check the class of the date column
class(activityNotNA$date)

## Convert factor to Date 
activityNotNA$date <- as.Date(activityNotNA$date)
## Check if the class of the date has been changed to Date
class(activityNotNA$date)

##  Convert factor to Date & assign the date value to original data frame
activity$date <- as.Date(activity$date)

## What is mean total number of steps taken per day?
## 1. Calculate the total number of steps taken per day
totalStepsbyDay <- aggregate(activityNotNA$steps, by=list(activityNotNA$date), FUN=sum)
head(totalStepsbyDay)

## Change the column name to more readable 
names(totalStepsbyDay) <- c("Date", "TotalSteps")

## 2. Make a histogram of the total number of steps taken each day 
hist(totalStepsbyDay$TotalSteps, main = "Histogram of Total Steps by Day", xlab = "Total Steps per day" , col ='Light Green')


## 3. Calculate and report the mean and median total number of steps taken per day

mean(totalStepsbyDay$TotalSteps) 
median(totalStepsbyDay$TotalSteps) 

## What is the average daily activity pattern?

## 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

totalStepsbyInterval <- aggregate(activityNotNA$steps, by=list(activityNotNA$interval), FUN=mean)
names(totalStepsbyInterval) <- c("Interval", "AverageStepsAllDays")


plot(totalStepsbyInterval, xlab ="5 Min Interval", ylab = " Average Steps", main="Average Daily Steps taken ", type = "l",lwd=2, col ='Green')

## Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

max(totalStepsbyInterval$AverageStepsAllDays)

intervalwithMaxStep <- totalStepsbyInterval[(totalStepsbyInterval$AverageStepsAllDays == max(totalStepsbyInterval$AverageStepsAllDays)), ]

intervalwithMaxStep


## Imputing missing values

## 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
totalNA <- sum(is.na(activity$steps))
totalNA

## 2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
## Strategy - Fill NA with Mean of that day 5 mins interval

##3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
averageSteps <- aggregate(steps ~ interval, data = activity, FUN = mean)
stepsfilledNA <- numeric()
for (i in 1 :nrow(activity)) {
    data <- activity[i, ]
    if (is.na(data$steps)) {
        steps <- subset(averageSteps, interval == data$interval)$steps
    } else {
          steps <-data$steps
    }
    stepsfilledNA <- c(stepsfilledNA, steps)
}

filledinActivity <- activity
filledinActivity$steps <- stepsfilledNA
summary(filledinActivity)

totalNA <- sum(is.na(filledinActivity$steps))
totalNA

## 4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

totalStepsbyDayFilled <- aggregate(filledinActivity$steps, by=list(filledinActivity$date), FUN=sum)
names(totalStepsbyDayFilled) <- c("Date", "TotalStepsFilled")

hist(totalStepsbyDayFilled$TotalStepsFilled, main = "Histogram of Total Steps by Day", xlab = "Total Steps per day" , col ='Green')

##Before Imputing
mean(totalStepsbyDay$TotalSteps) 
median(totalStepsbyDay$TotalSteps)
##After Imputing
mean(totalStepsbyDayFilled$TotalStepsFilled)
median(totalStepsbyDayFilled$TotalStepsFilled)
## observation - Mean is same for before imputing but median is little bit different


## Are there differences in activity patterns between weekdays and weekends?
##  For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

## 1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

activityDay <- weekdays(activity$date)
activityDayType <- vector()

for (i in 1 :nrow(activity)) {
    if (activityDay[i] == "Sunday") {
        activityDayType[i] = "weekend"
    } else if (activityDay[i] == "Saturday") {
      activityDayType[i] = "weekend"
    } else {
      activityDayType[i] = "weekday"
    }
}


activity$activityDayType <- activityDayType
activity$activityDayType <- factor(activity$activityDayType)

## 2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

totalStepsbyDaybyDayType <- aggregate(steps ~ interval + activityDayType, data = activity, FUN=mean)

library(lattice)
xyplot(steps ~ interval | activityDayType,totalStepsbyDaybyDayType, layout= c(1, 2) , type ='l', main = "Weekdays & Weekend Steps Pattern Analysis", 
       ylab = "Average Steps Per Day" , xlab = "Frequency in Interval", col ="Magenta")
