JHDS - Reproducible Research - Poject 1

rm(list = ls()) #clear all variables in the workspace

#install.packages("tidyverse")
library(tidyverse) 
install.packages("lubridate")
library(lubridate)

setwd("~/Documents/1_KM/1- EdX_Coursera_Udemy_Moocs/JHDS - Reproducible Research/Projects")

getwd()

fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileUrl, destfile = paste0(getwd(), '/repdata%2Fdata%2Factivity.zip'), method = "curl")
unzip("repdata%2Fdata%2Factivity.zip",exdir = "data")

activity <- read_csv("./data/activity.csv")
activity_daily <- activity %>% na.omit() %>% group_by(date) %>% summarize(steps = sum(steps)) 

ggplot(activity_daily, aes(x = steps)) +  
  geom_histogram(breaks=seq(0, 23000, by=2000), col="red", fill="green", 
               alpha = .2) + 
  labs(title="Histogram for Steps per Day", x="Steps", y="Count") 

# + xlim(c(18,52)) + 
#   ylim(c(0,30))
max(activity_daily$steps, na.rm = T)

avg_daily_steps <- round(mean(activity_daily$steps), 2)
median_daily_steps <- median(activity_daily$steps)

min(activity_daily$date)
max(activity_daily$date)

#time <- c("2012-10-02","2012-11-29")
#lubridate::ymd(time)

p <- ggplot(activity_daily, type = l, aes(x=activity_daily$date, y = activity_daily$steps)) +
  geom_line() +
  xlab("")
p

# Which 5 minute interval contained the most steps on average

#%>% na.omit()
activity_int_sum <- activity %>% na.omit() %>% group_by(interval) %>% summarize(steps = sum(steps)) 
activity_int_sum

activity_int_mean <- activity %>% na.omit() %>% group_by(interval) %>% summarize(steps = mean(steps)) 
activity_int_mean
#the above is effectively the same as using the tapply, ie as follows
#activity_int_avg<- tapply(activity$steps, activity$interval, mean, na.rm = TRUE)

max_steps_int <- activity_int_mean[which.max(activity_int_mean$steps),]
max_steps_intm


#days <- max(activity_daily$date) - min(activity_daily$date)
#activity_int_avg <- activity_int_sum %>% steps = mutate(activity_int$steps/58)
#rm(activity_int_avg)


#Question 6 - Code to describe and show strategy for imputing missing data

## Imputing Missing Data
impute <- split(activity, activity$interval)
impute <- lapply(impute, function(x) {
    x$steps[which(is.na(x$steps))] <- mean(x$steps, na.rm = T)
    return(x)
})

impute <- do.call("rbind", impute)
row.names(impute) <- NULL

impute2 <- split(impute, impute$date)
impute2 <- lapply(impute2, function(x) {
  x$steps[which(is.na(x$steps))] <- mean(x$steps, na.rm = TRUE)
  return(x)
})

impute2 <- do.call("rbind", impute2)
row.names(impute2) <- NULL

head(impute2)


## the above is too involved... trying a different approach

#Step1 - determine how many values are missing

values_missing <- tbl_df(activity)
str(values_missing)

values_missing %>% filter(is.na(steps)) %>% summarize(missing_values = n())

#Step2 - impute the missing values in a new column



activity$imputed_steps <- ifelse(is.na(activity$steps), round(activity_int_mean$steps[match(activity$interval, activity_int_mean$interval)],0), activity$steps)

activity2 <- data.frame(steps=activity$imputed_steps, interval=activity$interval, date=activity$date)
#calculate how many steps we imputed
sum(activity2$steps)-sum(activity$steps, na.rm = T)


#Calculate a histogram of this new imputed data
activity2_daily <- activity2 %>% na.omit() %>% group_by(date) %>% summarize(steps = sum(steps)) 

ggplot(activity2_daily, aes(x = steps)) +  
  geom_histogram(breaks=seq(0, 23000, by=2000), col="red", fill="green", 
                 alpha = .2) + 
  labs(title="Histogram for Steps per Day", x="Steps", y="Count") 

# Calculate the new mean and median values
avg2_daily_steps <- round(mean(activity2_daily$steps), 2)
median2_daily_steps <- median(activity2_daily$steps)

# Are there differences between the weekends and weekdays?

# Correct date formats (if needed????)
#activity2_daily$newDate <- as.Date(activity2_daily$date, format = "%Y-%m-%d")

# Create a weekday variable
activity2_daily$weekday <- weekdays(activity2_daily$date)

# create a new variable indicating weekday or weekend
activity2_daily$day_type <- ifelse(activity2_daily$weekday=='Saturday' | activity2_daily$weekday=='Sunday', 'Weekend','Weekday')

######## Fix the above
# Create variable with date in correct format
activityFull$RealDate <- as.Date(activityFull$date, format = "%Y-%m-%d")
# create a variable with weekdays name
activityFull$weekday <- weekdays(activityFull$RealDate)
# create a new variable indicating weekday or weekend
activityFull$DayType <- ifelse(activityFull$weekday=='Saturday' | activityFull$weekday=='Sunday', 'weekend','weekday')
# see first 10 values
head(activityFull, n=10)

activity3 <- activity2
activity3$weekday <- weekdays(activity3$date)

# create a new variable indicating weekday or weekend
activity3$day_type <- ifelse(activity3$weekday=='Saturday' | activity3$weekday=='Sunday', 'Weekend','Weekday')


# Create a plot to compare the Weekends and Weekdays
#format a table that has meaningful 
activity4 <- activity3 %>% mutate(interval = interval/100)
activity5 <- aggregate(steps~interval+day_type, data= activity4, FUN = mean, na.action=na.omit)

p1 <- ggplot(activity5, aes(interval, steps))
p1+geom_line(col="darkred")+ggtitle("Average steps per time interval: weekdays vs. weekends")+xlab("Time in Hours")+ylab("Steps")+theme(plot.title = element_text(face="bold", size=12))+facet_grid(day_type ~ .)

