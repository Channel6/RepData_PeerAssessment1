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
		method = "wget",
		quiet = TRUE
	)
	unzip(
		zipfile = datafile,
	)
}


activity<-read.csv("activity.csv", header = TRUE)
totalSteps <- aggregate(
	steps ~ date,
	data = activity,
	sum,
	na.rm = TRUE
)

## STEP 2: Histogram, mean, and median

# Make a histogram of the total number of steps taken daily.
hist(totalSteps$steps)

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
        completeactivity[i,]$steps<-interval2steps(completeactivity[i,]$interval)
        numfilledvals=numfilledvals+1
    }
}
cat("Total ",numfilledvals, "NA values were filled.\n\r")

# Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.

stepsdaily<-aggregate(steps~date,data=activityFilled,sum)
hist(stepsdaily$steps)
mean(stepsdaily$steps)
median(stepsdaily$steps)


