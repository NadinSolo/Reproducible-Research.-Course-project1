
#The data downloaded to the computer, unzipped.
# Read data into object variable dataFile
dataFile <- read.csv("./data/activity.csv")
# Observe data using summary()
sum(dataFile)
str(dataFile)

# Observe data again

str(dataFile)
# a set of missing values using the mice package and the md.pattern function
#Total number of steps taken per day
steps_by_day <- aggregate(steps ~ date, data=dataFile, sum)

#histogram of the total number  of steps taken each day
hist(steps_by_day$steps, main = paste("Total Steps Each Day"), col="blue",
     xlab="Number of Steps")

#Copy histogram to PNG file (plot1.png)
dev.copy(png, file ='./plot1.png')
dev.off()

#mean and  median number of steps  taken each day
rmean <- mean(steps_by_day$steps)
rmean
rmedian <- median(steps_by_day$steps)
rmedian
## The mean is 1.0766 × 104 and the median is 10765.
#Time series plot  of the average number of steps taken
aver_numb_steps<-aggregate(steps ~ interval, data=dataFile, mean)

library(ggplot2)
ggplot(aver_numb_steps, aes(interval,steps))+
  geom_line() +
  geom_point() +
  ggtitle(expression("Average Number of Steps per Day by Interval"))+
  labs(x = "Interval", y ="Number of Steps")


#Copy plot to PNG file (plot2.png)
dev.copy(png, file ='./plot2.png')
dev.off()

#The 5-minute interval that, on average, contains the maximum number of steps is 835:
max_interval <- aver_numb_steps[which.max(aver_numb_steps$steps),1]
max_interval

# a set of missing values using the mice package and the md.pattern function
library(mice)
md.pattern(dataFile, rotate.names = TRUE)

#2304 values belong to a variable steps
sum(is.na(dataFile$steps))


# Create a new dataset that is equal to the original dataset
impute.dataFile <- dataFile


#filling in missing values in the dataset
#use the mean

impute.dataFile$steps[is.na(impute.dataFile$steps)] <- mean(impute.dataFile$steps,na.rm=TRUE)
impute.dataFile$steps <- as.numeric(impute.dataFile$steps)
impute.dataFile$interval <- as.numeric(impute.dataFile$interval)
colSums(is.na(impute.dataFile))

#Create a new dataset that is equal to the original dataset but with
# the missing data filled in
summary(impute.dataFile)

#Make a histogram of the total number of steps taken each day
steps_by_day_i <- aggregate(steps ~ date, impute.dataFile, sum)
hist(steps_by_day_i$steps, main = paste("Total Steps Each Day"), col="Green", 
     xlab="Number of Steps")

#Copy plot to PNG file (plot3.png)
dev.copy(png, file ='./plot3.png')
dev.off()

#Calculate and report the mean and median total number of steps taken per day

rmean.i <-  mean(steps_by_day_i$steps)
rmean.i
rmedian.i <- median(steps_by_day_i$steps)
rmedian.i
## Calculate difference between imputed and non-imputed data

mean_diff <- rmean.i - rmean
print(mean_diff)
med_diff <- rmedian.i - rmedian
print(med_diff)

## Total difference
total_diff <- sum(steps_by_day_i$steps) - sum(steps_by_day$steps)
total_diff
## Create a new factor variable in the dataset with two levels – “weekday” 
#and “weekend” indicating whether a given date is a weekday or weekend day.
library(timeData)
library(dplyr)
str(weekdays)
str(weekend)

library(chron)

table(is.weekend(impute.dataFile$date))
#Adding new factor variable "dayofweek" indicating whether a given date is 
#a weekday or weekend day
impute.dataFile$dayofweek <- ifelse(is.weekend(impute.dataFile$date), "weekend",
                          "weekday")
table(impute.dataFile$dayofweek)
head(impute.dataFile)

#Make a panel plot containing a time series plot (i.e. type = “l”) of
# the 5-minute interval (x-axis) and the average number of steps taken, 
#averaged across all weekday days or weekend days (y-axis). The plot should 

#look something like the following, which was created using simulated data:
#Aggregating(mean) steps over interval and day of week

meanintervalnew<- aggregate(steps ~ interval + dayofweek, impute.dataFile, FUN=mean)

#Aggregated Data
head(meanintervalnew)

#Time Series plot using ggplot
ggplot(meanintervalnew, aes(x=interval, y=steps)) + 
  geom_line(color="red", size=1) + 
  facet_wrap(~dayofweek, nrow=2) +
  labs(x="\nInterval", y="\nNumber of steps")

#Copy plot to PNG file (plot4.png)
dev.copy(png, file ='./plot4.png')
dev.off()
