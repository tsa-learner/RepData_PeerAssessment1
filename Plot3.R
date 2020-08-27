#Imputing missing values

#loading the data 
data <- read.csv("activity.csv", header = TRUE, sep =",")
dim(data)
head(data)

#1. To calculate and report the total number of missing values 
total_NA <- sum(is.na(data))
total_NA #Total missing data
sum(is.na(data$steps))#missing data in 'steps' column = 2304
sum(is.na(data$date)) #missing data in "date' column = 0
table(is.na(data$interval)) #missing in 'interval' = 0

#The missing values will be imputed based on mean and since only one column
#contains the missing data, it is pretty straightforward 
head(data) #The top 6 columns of 'steps' contain NAs

#Impute NA in steps and generate a new dataframe 'clean_data
clean_data$steps[is.na(clean_data$steps)] <- mean(data$steps, na.rm = TRUE)
head(data)  #For comparison
tail(data)

head(clean_data) #The NAs are replaced by the mean = 37.3826
tail(clean_data)

#to plot a histogram of the total number of steps taken each day
no_of_steps <- aggregate(steps~date, clean_data, FUN = sum)
png("Plot3.png")
par(mfrow = c(2,1))
hist(no_of_steps$steps, col = "blue", ylim = c(0,35),
     main = "Total number of steps taken each day (after imupting NAs)",
     xlab = "Total number of steps", ylab = "Frequency")

#To plot original data on the righ side
org_data <- aggregate(steps ~ date, data, FUN = sum, na.rm = TRUE)
hist(org_data$steps, col = "green", ylim = c(0,35),
     main = "Total number of steps taken each day (Original data)",
     xlab = "Total number of steps", ylab = "Frequency")
dev.off()
# From the figures, there is a difference after imputing the NAs. 

#To check the actual difference, we can do the following calculation
org_mean <- mean(org_data$steps, na.rm = TRUE)
org_median <- median(org_data$steps, na.rm = TRUE)
org_totalSteps <- sum(org_data$steps)

new_mean <- mean(no_of_steps$steps, na.rm = TRUE)
new_median <- median(no_of_steps$steps, na.rm = TRUE)
new_totalSteps <- sum(no_of_steps$steps)

#Differences 
diff_mean <- org_mean - new_mean
diff_median <- org_median - new_median
diff_totalSteps <- org_totalSteps - new_totalSteps
cat("The difference in mean = ", diff_mean, "\nThe difference in median is = ", diff_median, 
    "\n The total difference in number of steps = ", diff_totalSteps)
