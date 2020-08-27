#course Project 1

#What is the total number of steps taken per day? 
#loading the data 
data <- read.csv("activity.csv", header = TRUE, sep =",")
dim(data)
table(is.na(data))
table(is.na(data$steps))
table(is.na(data$date))
table(is.na(data$interval))

##Mean total number of steps taken per day
library(dplyr)
#Compute total number of steps
total_steps <- sum(data$steps, na.rm = TRUE)
total_steps

#Plot the histogram
steps_perday <- aggregate(steps ~ date, data, FUN = sum, na.rm = TRUE)
steps_perday

png("plot.png", width = 480, height = 480, unit = "px") #Set up the png plot
hist(steps_perday$steps, main = "Total number of steps taken each day", 
     xlab = "Number of steps", ylab = "Frequency")

dev.off()

#Mean and median of the total numer of steps taken per day
summary(steps_perday$steps, na.rm = TRUE)
##The __mean__ of the total number of steps taken per day = 10766
##The __median__ of the total number of steps taken per day = 10765



