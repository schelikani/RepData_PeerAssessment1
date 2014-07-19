# PA 1 Assignment

# Set the working directory

# Load the Data
a1 = read.csv("activity.csv", sep =",", header= T)

# Clean the data
# 1 conver the date to data type date
a1$date = as.Date(a1$date)
library(plyr)
tsteps= ddply(a1, c("date"), summarise, dsteps = sum(steps, na.rm= T))
smean=mean(tsteps$dsteps)
smedian =median(tsteps$dsteps)
barplot(tsteps$dsteps, col ="red", names.arg=tsteps$date, main="Daily Total Steps Vs Date", xlab="Date", ylab="TotalDailySteps")
abline(smean, 0, col="blue", lwd=5)
abline(smedian, 0, col="yellow", lwd=5)
legend("top",c("TotalDailySteps", "StepsMean", "StepsMedian"), fill=c("red","blue","yellow"))

# Next Question  5 Minute interval data
iTsteps= ddply(a1, c("interval"), summarise, steps = mean(steps, na.rm= T))
plot(iTsteps$interval, iTsteps$steps, type="l",col ="red", main="Interval Mean Steps Vs 5Minute Interval", xlab="5 MinuteInterval#(Day24HrMin)", ylab="Intervel Mean Steps")
head(iTsteps[order(iTsteps$steps, na.last=T, decreasing=T),])

# Missing data questions
nadays = sum(is.na(a1$steps))
nadates=levels(as.factor(a1[is.na(a1$steps),]$date))

# Remove the data for the dates indentified as NA
a3 = a1[!(as.character(a1$date) %in% nadates),]
# Create the data frame that has data for each date that is removed above
mdata =data.frame(steps=numeric(), date=character(), interval=integer())

iTsteps= data.frame(iTsteps, date =NA)
iTsteps$date =as.Date(iTsteps$date)

for(i in 1:length(nadates))
{
  iTsteps$date = nadates[i]
  mdata= rbind(mdata,iTsteps)
}

a3 = rbind(a3, mdata)
#Weekday and Weekend Activity Analysis
# as Day column that indicates the date is Weekday or Weekend
a3 = data.frame(a3, day = NA)
a3$day =as.character(a3$day)

a3[weekdays(a3$date) %in% c("Monday","Tuesday","Wednesday","Thursday","Friday"),]$day ="weekday"
a3[weekdays(a3$date) %in% c("Saturday","Sunday"),]$day ="weekend"

iTstepsnew = ddply(a3, c("day", "interval"), summarise, avgsteps = mean(steps, na.rm= T))
qplot(interval, avgsteps, data =iTstepsnew, col= day)

# restore the working directory
setwd(cdir)
