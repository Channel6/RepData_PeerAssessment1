####		Replicating Data -- Peer Assessment 1		####
##			     Glenn Kerbein			  ##
##			     March 4, 2017		    	  ##
####################################################################

#clean the working environment
rm(list=ls(all=TRUE)) 

# Get required files
datafile <- "activity_monitoring.zip"
if(!file.exists(datafile)){
	url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
	download.file(
		url,
		destfile = "activity_monitoring.zip",
		method = "wget",
		quiet = TRUE
	)
	unzip(
		zipfile = datafile,
	)
}



activity<-read.csv("activity.csv")
tail(activity)
head(activity)


