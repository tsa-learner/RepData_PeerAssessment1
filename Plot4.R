#Are there differences in activity patterns between weekdays and weekends?

#Set a function to segregate the weekends and weekdays and then add as variable

#Using the weekdays() create weekend and weekday
data <- read.csv("activity.csv", header = TRUE, sep =",")
dim(data)
head(data)

clean_data <- data
clean_data$steps[is.na(clean_data$steps)] <- mean(data$steps, na.rm = TRUE)
head(data)  #For comparison
tail(data)


daysofweek <- function(date){
  day <- weekdays(date)
  if(day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
    return("Weekday")
  else if (day %in% c("Saturday", "Sunday"))
    return ("Weekend")
  else
    stop("Invalid Date")
}

clean_data$steps[is.na(data$steps)] <- mean(data$steps, na.rm = TRUE)
final_data <- aggregate(steps ~ interval + date, clean_data, FUN = sum)
final_data$date <- as.Date(final_data$date)
final_data$day <- sapply(final_data$date, FUN = daysofweek)
head(final_data)
tail(final_data) #data looks correct

mean_stepsperDay <- aggregate(steps ~ interval+day, final_data, mean)

png("Plot4.png")

library(ggplot2)
g <- ggplot(mean_stepsperDay, aes(x = interval, y = steps))
g + geom_line(size = 1) + facet_grid(day ~.) +
  ggtitle("Average daily activity time series") +          
  xlab("Interval") + 
  ylab("Number of steps")
dev.off()

