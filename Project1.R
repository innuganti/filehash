## Load the data 

if(!file.exists("getdata-projectfiles-UCI HAR Dataset.zip")) {
  temp <- tempfile()
  download.file("http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",temp)
  unzip(temp)
  unlink(temp)
}

data <- read.csv("activity.csv")

## Total number of steps taken per day

steps_by_day <- aggregate(steps ~ date, data, sum)
hist(steps_by_day$steps, main = paste("Total Steps Each Day"), col="orange", xlab="Number of Steps")

rmean <- mean(steps_by_day$steps)
rmedian <- median(steps_by_day$steps)

# The mean is 1.0766 × 104 and the median is 10765.

## What is the average daily activity pattern


steps_by_interval <- aggregate(steps ~ interval, data, mean)
par(bg = 'lightblue')
plot(steps_by_interval$interval,steps_by_interval$steps, type="l", xlab="Interval", ylab="Number of Steps",main="Average Number of Steps per Day by Interval")

## Max average steps
max_interval <- steps_by_interval[which.max(steps_by_interval$steps),1]
max_interval

## The 5-minute interval, on average across all the days in the data set, containing the maximum number of steps is 835.

## Impute missing values. Compare imputed to non-imputed data

incomplete <- sum(!complete.cases(data))
imputed_data <- transform(data, steps = ifelse(is.na(data$steps), steps_by_interval$steps[match(data$interval, steps_by_interval$interval)], data$steps))

imputed_data[as.character(imputed_data$date) == "2012-10-01", 1] <- 0

## 

steps_by_day_i <- aggregate(steps ~ date, imputed_data, sum)
hist(steps_by_day_i$steps, main = paste("Total Steps Each Day"), col="Green", xlab="Number of Steps")

# Create Histogram to show difference

hist(steps_by_day$steps, main = paste("Total Steps Each Day"), col="magenta", xlab="Number of Steps", add=T)
legend("topright", c("Imputed", "Non-imputed"), col=c("green", "magenta"), lwd=10)

## Calculate new mean and median for imputed data

rmean.i <- mean(steps_by_day_i$steps)
rmedian.i <- median(steps_by_day_i$steps)

## Calculate difference between imputed and non-imputed data

mean_diff <- rmean.i - rmean
med_diff <- rmedian.i - rmedian

## Total difference
total_diff <- sum(steps_by_day_i$steps) - sum(steps_by_day$steps)

## The imputed data mean is 1.059 × 104
# The imputed data median is 1.0766 × 104
# The difference between the non-imputed mean and imputed mean is -176.4949
# The difference between the non-imputed mean and imputed mean is 1.1887
# The difference between total number of steps between imputed and non-imputed data is 7.5363 × 104. Thus, there were 7.5363 × 104 more steps in the imputed data

## Are there differences in activity patterns between weekdays and weekends


weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", 
              "Friday")
imputed_data$dow = as.factor(ifelse(is.element(weekdays(as.Date(imputed_data$date)),weekdays), "Weekday", "Weekend"))

steps_by_interval_i <- aggregate(steps ~ interval + dow, imputed_data, mean)

library(lattice)

xyplot(steps_by_interval_i$steps ~ steps_by_interval_i$interval|steps_by_interval_i$dow, main="Average Steps per Day by Interval",xlab="Interval", ylab="Steps",layout=c(1,2), type="l")