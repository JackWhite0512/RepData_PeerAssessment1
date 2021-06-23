PA1_template
Gianluca Kikidis
23/6/2021
Introduction
It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the “quantified self” movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The data for this assignment can be downloaded from the course web site:

Dataset: Activity monitoring data
The variables included in this dataset are:

steps: Number of steps taking in a 5-minute interval (missing values are coded as 𝙽𝙰)
date: The date on which the measurement was taken in YYYY-MM-DD format
interval: Identifier for the 5-minute interval in which measurement was taken
The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

Loading and preprocessing the data
read the downloaded csv file (alternatively, copy URL, download and unzip).

df <- read.csv("activity.csv")
df$date = as.Date(df$date)
What is mean total number of steps taken per day?
total number of steps taken per day
steps_tot <- aggregate(df$steps,by=list(df$date),FUN=sum,na.rm=TRUE) 
colnames(steps_tot) <- c("Day", "Steps")
histogram of the total number of steps taken each day
hist(steps_tot$Steps,
     breaks = 25,
     col="cyan", 
     xlab="Steps (total)", 
     ylim=c(0, 10), 
     main="Daily Steps Histogram")


mean and median of the total number of steps taken per day
mean(steps_tot$Steps)
## [1] 9354.23
median(steps_tot$Steps)
## [1] 10395
What is the average daily activity pattern?
time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
pattern_avrg <- aggregate(df$steps, 
                          by = list(Interval = df$interval), na.rm=T, 
                          FUN = "mean")
colnames(pattern_avrg) <- c("Interval", "Avrg_Steps")
plot(pattern_avrg$Interval, pattern_avrg$Avrg_Steps, type = "l", col="purple",
     ylab = "Average steps", 
     xlab = "Intervals (5-min)",
     main = "Average daily activity pattern")


5-minute interval containing the maximum number of steps
Steps_max <- pattern_avrg$Interval[which(pattern_avrg$Avrg_Steps==max(pattern_avrg$Avrg_Steps))]

Steps_max
## [1] 835
Imputing missing values
Calculate and report the total number of missing values in the dataset
sum(is.na(df$steps))
## [1] 2304
filling in all of the missing values in the dataset
df$steps[is.na(df$steps)]<-mean(df$steps,na.rm=TRUE)
head(df)
##     steps       date interval
## 1 37.3826 2012-10-01        0
## 2 37.3826 2012-10-01        5
## 3 37.3826 2012-10-01       10
## 4 37.3826 2012-10-01       15
## 5 37.3826 2012-10-01       20
## 6 37.3826 2012-10-01       25
stopifnot(sum(is.na(df)) == 0)
Create a new dataset with the missing data filled in
df.new = df
df$steps[is.na(df$steps)]<-mean(df$steps,na.rm=TRUE)
stopifnot(sum(is.na(df.new)) == 0)
Histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day
steps_tot_new <- aggregate(df.new$steps,by=list(df.new$date),FUN=sum,na.rm=TRUE) 
colnames(steps_tot_new) <- c("Day", "Steps")

hist(steps_tot_new$Steps,
     breaks = 25,
     col="cyan", 
     xlab="Steps (total)", 
     ylim=c(0, 20), 
     main="Daily Steps Histogram (filled NA)")


mean(steps_tot_new$Steps)                       ## The mean has changed and is
## [1] 10766.19
median(steps_tot_new$Steps)                     ## now equal to the median.
## [1] 10766.19
Are there differences in activity patterns between weekdays and weekends?
New factor variable in the dataset with two levels – “weekday” and “weekend”
df.new$date <- as.Date(df.new$date)
df.new$day <- weekdays(df.new$date)
df.new$week <- ifelse(df.new$day=="sabato" | df.new$day=="domenica",
                          "Weekend","Weekday")
df.new$week <- factor(df.new$week)
Time series plot averaged across all weekday or weekend days
plot_weekday <- aggregate(steps ~ interval + week, data=df.new, mean)

library(ggplot2)
ggplot(plot_weekday, aes(interval, steps)) + 
  geom_line(col="blue") + 
  facet_grid(week ~ .) +
  xlab("Interval") + 
  ylab("Number of steps") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ggtitle("Weekdays and weekends activity differences")
