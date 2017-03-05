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


## STEP 2: Histogram, mean, and median


