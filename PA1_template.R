library(ggplot2)

###############################################################################
##Loading and preprocessing the data
###############################################################################

df <- read.csv("activity.csv")
df$date = as.Date(df$date)
 
## What is mean total number of steps taken per day?

## total number of steps taken per day
steps_tot <- aggregate(df$steps,by=list(df$date),FUN=sum,na.rm=TRUE) 
colnames(steps_tot) <- c("Day", "Steps")

## histogram of the total number of steps taken each day
hist(steps_tot$Steps,
     breaks = 25,
     col="cyan", 
     xlab="Steps (total)", 
     ylim=c(0, 10), 
     main="Daily Steps Histogram")
## mean and median of the total number of steps taken per day
mean(steps_tot$Steps)
median(steps_tot$Steps)



###############################################################################
## What is the average daily activity pattern?
###############################################################################

## time series plot of the 5-minute interval (x-axis) and the average number
## of steps taken, averaged across all days (y-axis)
pattern_avrg <- aggregate(df$steps, 
                          by = list(Interval = df$interval), na.rm=T, 
                          FUN = "mean")
colnames(pattern_avrg) <- c("Interval", "Avrg_Steps")
plot(pattern_avrg$Interval, pattern_avrg$Avrg_Steps, type = "l", col="purple",
     ylab = "Average steps", 
     xlab = "Intervals (5-min)",
     main = "Average daily activity pattern")

## 5-minute interval containing the maximum number of steps
Steps_max <- pattern_avrg$Interval[which(pattern_avrg$Avrg_Steps==max(pattern_avrg$Avrg_Steps))]

Steps_max

###############################################################################
## Imputing missing values
###############################################################################

## Calculate and report the total number of missing values in the dataset
sum(is.na(df$steps))

## filling in all of the missing values in the dataset
df$steps[is.na(df$steps)]<-mean(df$steps,na.rm=TRUE)
head(df)
stopifnot(sum(is.na(df)) == 0)

## Create a new dataset with the missing data filled in
df.new = df
df$steps[is.na(df$steps)]<-mean(df$steps,na.rm=TRUE)
stopifnot(sum(is.na(df.new)) == 0)

## Histogram of the total number of steps taken each day and 
## Calculate and report the mean and median total number of steps taken per day
steps_tot_new <- aggregate(df.new$steps,by=list(df.new$date),FUN=sum,na.rm=TRUE) 
colnames(steps_tot_new) <- c("Day", "Steps")

hist(steps_tot_new$Steps,
     breaks = 25,
     col="cyan", 
     xlab="Steps (total)", 
     ylim=c(0, 20), 
     main="Daily Steps Histogram (filled NA)")

mean(steps_tot_new$Steps)                       ## The mean has changed and is
median(steps_tot_new$Steps)                     ## now equal to the median.

###############################################################################
## Are there differences in activity patterns between weekdays and weekends?
###############################################################################

## New factor variable in the dataset with two levels - "weekday" and "weekend"
df.new$date <- as.Date(df.new$date)
df.new$day <- weekdays(df.new$date)
df.new$week <- ifelse(df.new$day=="sabato" | df.new$day=="domenica",
                          "Weekend","Weekday")
df.new$week <- factor(df.new$week)

## Time series plot averaged across all weekday or weekend days
plot_weekday <- aggregate(steps ~ interval + week, data=df.new, mean)

ggplot(plot_weekday, aes(interval, steps)) + 
  geom_line(col="blue") + 
  facet_grid(week ~ .) +
  xlab("Interval") + 
  ylab("Number of steps") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ggtitle("Weekdays and weekends activity differences")
  