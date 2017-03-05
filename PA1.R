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


