#What is the daily activity pattern? 

#loading the data 
data <- read.csv("activity.csv", header = TRUE, sep =",")
dim(data)
head(data)

#Plot the activity graph using line graph (type = "l")
avg_pattern <- aggregate(steps~interval, data, mean, na.rm = TRUE)

png("plo2.png", width = 480, height = 480, unit = "px") #Set up the png plot
plot(avg_pattern, type = "l", col = "red", 
     main = "The daily activity pattern", 
     xlab = "5-minute interval", ylab = "Average number of steps")

dev.off()

#Which 5-minute interval, on average across all the dataset, contains the 
#maximum number of steps? 

max_interval <- avg_pattern$interval[which.max(avg_pattern$steps)]
max_interval  #The 835th interval contains the maximum number of steps 

