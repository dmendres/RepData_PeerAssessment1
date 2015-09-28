# Working script for loading and processing Activity data for Reproducible Research Peer project # 1
# Course repdata-032.
oldWd <- getwd()
setwd("Coursera/RDevelopmentWork/RepData_PeerAssessment1")
unzip("activity.zip", overwrite = T)
dfActivity <- read.csv("activity.csv", header = T, sep = ",", na.strings = c("NA"))

#cleanup: convert to POSIXct date type
dfActivity$date <- as.POSIXct(strptime(dfActivity$date, format = "%Y-%m-%d"))
str(dfActivity)
summary(dfActivity)


# TODO: INTERVAL IS IN 24 HR CLOCK, E.G. 0,...,855,900,...,2355, TOTAL OF 288. SHOULD CONVERT TO MORE USEFUL TIME FOR PLOTTING
#DONE: CONVERT TO NUMERIC "HOUR"

#part 1: examine totals by day
#part 1 explicitly states to ignore NAs
#plot the histogram of total steps by day
dfActivityStepsByDay <- aggregate(dfActivity$steps, by = list(dfActivity$date), sum)
colnames(dfActivityStepsByDay) <- c("date", "steps")

hist(dfActivityStepsByDay$steps, breaks = 20, col = "lightBlue", plot = T)
meanStepsByDay = mean(dfActivityStepsByDay$steps, na.rm = T)
medianStepsByDay = median(dfActivityStepsByDay$steps, na.rm = T)
#plot the mean, median is very close, won't show up distinctly
abline(v = meanStepsByDay, col = "darkRed")
print(paste("Mean daily steps", meanStepsByDay, "Median daily steps", medianStepsByDay))

#part 2: average daily activity pattern
dfActivityStepsByInterval <- aggregate(dfActivity$steps, by = list(dfActivity$interval), mean, na.rm = T)
colnames(dfActivityStepsByInterval) <- c("interval", "steps")
dfActivityStepsByInterval$hour <-  floor(dfActivityStepsByInterval$interval/100) + dfActivityStepsByInterval$interval%%100/60
plot(dfActivityStepsByInterval$steps ~ dfActivityStepsByInterval$hour, type = "l", col = "darkBlue")
print(paste("Interval of maximum activity", dfActivityStepsByInterval[which.max(dfActivityStepsByInterval$steps),]))

#part 3: impute missing values
#I think NA steps is logically equivalent to 0-steps, but let's count them
naIndicator = is.na(dfActivity$steps)
mean(naIndicator) # 13% are NA
print(paste(mean(naIndicator), "fraction missing values"))
print(paste(sum(naIndicator), "total missing values"))

dfActivityMergeIntervalAvgs <- merge(dfActivity, dfActivityStepsByInterval,by = "interval")
dfActivityMergeIntervalAvgs$steps <- 
  ifelse(is.na(dfActivityMergeIntervalAvgs$steps.x), 
         dfActivityMergeIntervalAvgs$steps.y, #average for the same interval
         dfActivityMergeIntervalAvgs$steps.x)
dfActivityMergeIntervalAvgs$steps0 <- 
  ifelse(is.na(dfActivityMergeIntervalAvgs$steps.x), 
         0,
         dfActivityMergeIntervalAvgs$steps.x)
dfCleanActivity <- data.frame(dfActivityMergeIntervalAvgs$interval, 
                              dfActivityMergeIntervalAvgs$steps, dfActivityMergeIntervalAvgs$steps0,
                              dfActivityMergeIntervalAvgs$date, dfActivityMergeIntervalAvgs$hour)
colnames(dfCleanActivity) <- c("interval", "steps", "steps0", "date", "hour")
#rm("dfActivityMergeIntervalAvgs")
summary(dfCleanActivity)

dfCleanActivityStepsByDay <- aggregate(dfCleanActivity$steps, by = list(dfCleanActivity$date), sum)
colnames(dfCleanActivityStepsByDay) <- c("date", "steps")
summary(dfCleanActivityStepsByDay)

dfCleanActivitySteps0ByDay <- aggregate(dfCleanActivity$steps0, by = list(dfCleanActivity$date), sum)
colnames(dfCleanActivitySteps0ByDay) <- c("date", "steps0")
summary(dfCleanActivitySteps0ByDay)


cleanMeanStepsByDay = mean(dfCleanActivityStepsByDay$steps, na.rm = T)
cleanMeanSteps0ByDay = mean(dfCleanActivitySteps0ByDay$steps0, na.rm = T)
cleanMedianStepsByDay = median(dfCleanActivityStepsByDay$steps, na.rm = T)
cleanMedianSteps0ByDay = median(dfCleanActivitySteps0ByDay$steps0, na.rm = T)

print(paste("With interval average imputed for NAs, Mean daily steps", cleanMeanStepsByDay, "Median daily steps", cleanMedianStepsByDay))
print(paste("With 0 imputed for NAs, mean daily steps", cleanMeanSteps0ByDay, "Median daily steps", cleanMedianSteps0ByDay))
#side-by-side
par("mfrow" = c(1,3))
hist(dfActivityStepsByDay$steps, breaks = 20, col = "lightBlue", plot = T, ylim = c(0,20))
abline(v = meanStepsByDay, col = "darkRed")
hist(dfCleanActivityStepsByDay$steps, breaks = 20, col = "lightBlue", plot = T, ylim = c(0,20))
abline(v = cleanMeanStepsByDay, col = "darkRed")
hist(dfCleanActivitySteps0ByDay$steps0, breaks = 20, col = "lightBlue", plot = T, ylim = c(0,20))
abline(v = cleanMeanSteps0ByDay, col = "darkRed")

#part 4: compare weekday and weekend activity
dfCleanActivity$weekdays <- ifelse(weekdays(dfCleanActivity$date) %in% c("Saturday", "Sunday"), "weekend", "weekday")
dfCleanActivity$weekdays <- factor(dfCleanActivity$weekdays)
library(ggplot2)
q <- qplot(interval, steps, data = dfCleanActivity, stat = "summary", geom = "line",
           fun.y = mean, facets = weekdays ~ .)
print(q)

dfWeekActivity = split(dfCleanActivity, dfCleanActivity$weekdays)
dfWeekdayActivityByInterval = aggregate(dfWeekActivity[[1]]$steps, by = list(dfWeekActivity[[1]]$interval), mean)
colnames(dfWeekdayActivityByInterval) = c("interval", "steps")
dfWeekendActivityByInterval = aggregate(dfWeekActivity[[2]]$steps, by = list(dfWeekActivity[[2]]$interval), mean)
colnames(dfWeekendActivityByInterval) = c("interval", "steps")

maxWeekdayActivityInterval  = dfWeekdayActivityByInterval[which.max(dfWeekdayActivityByInterval$steps), "interval"]
maxWeekdayActivityIntervalTime = paste(floor(maxWeekdayActivityInterval/100), maxWeekdayActivityInterval%%100, sep = ":")
maxWeekdayActivitySteps = signif(dfWeekdayActivityByInterval[which.max(dfWeekdayActivityByInterval$steps), "steps"],4)
meanWeekdayPmSteps = signif(mean(
  dfWeekdayActivityByInterval[dfWeekdayActivityByInterval$interval >= 1200. & dfWeekdayActivityByInterval$interval <= 1700.,
                              "steps"]),4)
maxWeekendActivityInterval = dfWeekendActivityByInterval[which.max(dfWeekendActivityByInterval$steps), "interval"]
maxWeekendActivityIntervalTime = paste(floor(maxWeekendActivityInterval/100), maxWeekendActivityInterval%%100, sep = ":")
maxWeekendActivitySteps = signif(dfWeekendActivityByInterval[which.max(dfWeekendActivityByInterval$steps), "steps"], 4)
meanWeekendPmSteps = signif(mean(
  dfWeekendActivityByInterval[dfWeekendActivityByInterval$interval >= 1200 & dfWeekendActivityByInterval$interval <= 1700,
                              "steps"]),4)