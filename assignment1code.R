activity <- read.csv("activity.csv")
daily.steps <- aggregate(FUN = sum, x = activity$steps, by = list(activity$date), na.rm = TRUE)
interval.steps <- aggregate(FUN = sum, x = activity$steps, by = list(activity$interval), na.rm = TRUE)
daily.steps <- setNames(daily.steps, c("Date", "Total.Steps"))
interval.steps <- setNames(interval.steps, c("Interval", "Total.Steps"))
mean.interval.steps <- aggregate(FUN = mean, x = activity$steps, by = list(activity$interval), na.rm = TRUE)
mean.interval.steps <- setNames(mean.interval.steps, c("Interval", "Mean.Steps"))
median.daily.steps <- median(daily.steps$Total.Steps)
mean.daily.steps <- mean(daily.steps$Total.Steps)
interval.mean <- aggregate(FUN = mean, activity$steps,  by = list(activity$interval), na.rm = TRUE)
max.mean.interval.step <- interval.mean[which.max(x = interval.mean$x),]
plot(mean.interval.steps, type = 'l', col = "Blue", xlab = "Time Interval", ylab = "Mean Steps")
sum(is.na(activity$steps))




#IMPUTE SHIT

#library(mice)
#imputed.data <- mice(data = activity, method = "norm.nob", m=1)

#get.mean <- function(interval) {
#  mean.interval.steps$Mean.Steps[mean.interval.steps$Interval == interval]
#}
#imputed.activity <- activity
#imputed.activity$steps[is.na(imputed.activity$steps)] <- get.mean(imputed.activity$interval)



get.mean <- function(interval) {
  mean.interval.steps$Mean.Steps[mean.interval.steps$Interval == interval]
}


impute.steps <- function(act) {

  steps <- act$steps
  intervals <- act$interval
  imputed.steps <- c(1:length(steps))
  for (i in 1:length(steps)) {
    ifelse(is.na(steps[i]), imputed.steps[i] <- get.mean(intervals[i]), imputed.steps[i] <- steps[i])
  }  
  imputed.steps
}

activity$imputed.steps <- impute.steps(activity)

weekends <- imp.activity$day == "Weekend"
weekdays <- imp.activity$day == "Weekday"

weekend.means <- aggregate(FUN = mean, x = imp.activity$steps[weekends], by = list(imp.activity$interval[weekends]))
weekday.means <- aggregate(FUN = mean, x = imp.activity$steps[weekdays], by = list(imp.activity$interval[weekdays]))

