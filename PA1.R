####		Replicating Data -- Peer Assessment 1		####
##			     Glenn Kerbein			  ##
##			     March 4, 2017		    	  ##
####################################################################

## STEP 0: init environment
#clean the working environment
rm(list=ls(all=TRUE)) 


## STEP 1: init data for reading
# Get required files
datafile <- "activity.zip"
if(!file.exists(datafile)){
	url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
	download.file(
		url,
		destfile = datafile,
		method = "curl",
		quiet = TRUE
	)
	unzip(
		zipfile = datafile,
	)
}

if(!file.exists("activity.csv")){
  unzip(
    zipfile = datafile
  )
}
activity<-read.csv(
  "activity.csv",
  header = TRUE
)
totalSteps <- aggregate(
	steps ~ date,
	data = activity,
	sum,
	na.rm = TRUE
)

## STEP 2: Histogram, mean, and median

# Make a histogram of the total number of steps taken daily.
hist(
  totalSteps$steps,
  main = "Steps per day",
  xlab = "Steps",
  col = "green",
  breaks = 8
)

#get the mean and median of these steps
meansteps <- mean(totalSteps$steps)
mediansteps <- median(totalSteps$steps)

## STEP 3: Compute the average daily activity pattern.

# Make a time series plot of the five minute interval along the x-axis and the average number of steps taken, averaged across all days along the y-axis.
stepsInterval <- aggregate(
	steps ~ interval,
	data = activity,
	mean,
	na.rm = TRUE
)
plot(
	steps ~ interval,
	data = stepsInterval,
	type = "l"
)

# Which 5-minute interval, average across all days in the dataset, contains the maximum number of steps?

maxsteps <- stepsInterval[which.max(stepsInterval$steps),]$interval

## STEP 4: Imputing missing values

# Calculate and report the total number of missing values in the dataset.
missingsteps <- sum(is.na(activity$steps))

# Devise a strategy for filling in all of the missing values in the dataset

meanstepsforinterval<-function(ival){
    stepsInterval[stepsInterval$ival==ival,]$steps
}

# Create a new dataset that is equal to the original dataset but with the missing data filled in.
completeactivity<-activity
numfilledvals=0
for(i in 1:nrow(completeactivity)){
    if(is.na(completeactivity[i,]$steps)){
        completeactivity[i,]$steps<-meanstepsforinterval(completeactivity[i,]$interval)
        numfilledvals=numfilledvals+1
    }
}
cat("Total ",numfilledvals, "NA values were filled.\n\r")

# Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.

stepsdaily<-aggregate(steps~date,data=activityFilled,sum)
hist(stepsdaily$steps)
mean(stepsdaily$steps)
median(stepsdaily$steps)

## STEP 5: Differences in activity patterns between weekdays and weekends?

# sort by weekend/weekday
completeactivity$week <- ifelse(
	weekdays(completeactivity$date) == "Saturday" | weekdays(completeactivity$date) == "Sunday",
	"weekend",
	"weekday"
)
# add factors for sorting
completeactivity$day <- factor(
	completeactivity$day,
	levels=c(
		"weekday",
		"weekend"
	)
)

# Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

stepsivalweek <- aggregate(
	steps ~ interval + day,
	completeactivity,
	mean
)
library(lattice)
xyplot(
	steps ~ interval | factor(day),
	data = stepsInterval2,
	aspect = 1/2,
	type = "l"
)

