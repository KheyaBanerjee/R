#
# Project for CACDS- "Intro to R"
# Kheya Banerjee
# June 2018
#

# 
# Loading packages
library(dplyr)


###########################################################
#......LOADING AND PREPROCESSING THE DATA.................#
###########################################################


# 1. Loading data

data <- read.csv("activity.csv", header=T, sep=",")


# 2. Process data by coverting the date string to a date object and Converting data to a tibble

data$date <- as.Date(data$date)
dataframe <- as_tibble(data)



###########################################################
#...WHAT IS MEAN TOTAL NUMBER OF STEPS TAKEN PER DAY?.....#
###########################################################


# 1. Calculating total number of steps TAKEN per day

outdt <- dataframe %>% group_by(date) %>% summarise_all(funs(sum))
total.steps.per.day <- outdt$steps


# 2. Plotting the histogram of the total number of steps taken each day

brkstps <- seq(0,25000,2500)
hist(total.steps.per.day, breaks = brkstps, xlab = "Total steps", ylab = "Frequency",
     main = "Histogram of the total number of steps\n taken each day (without imputing data)")


# 3. Calculate and print mean and median numer of steps taken per day

meansteps <- mean(total.steps.per.day, na.rm = T)
mediansteps <- median(total.steps.per.day, na.rm = T)
print(paste('Mean number of steps taken per day: ', meansteps))
print(paste('Median number of steps taken per day: ', mediansteps))




###########################################################
#.....WHAT IS THE AVERAGE DAILY ACTIVITY PATTERN?.........#
###########################################################


# 1. Plotting the average number of steps per time interval

outint <- dataframe %>% group_by(interval) %>% summarise(steps=mean(steps, na.rm = T))
mean.steps.per.interval <- outint$steps
plot(mean.steps.per.interval, type="l",  main="Time Series plot of 5 minute\n Interval (without imputing data)", 
     ylab="Average number of Steps", xlab="Interval INDEX") 


# 2. Calculating the Maximum value of average steps on interval

max.steps <- outint$interval[which.max(outint$steps)]
print(paste('Interval value for maximum average steps: ', max.steps))





###########################################################
#.............IMPUTING MISSING VALUES.....................#
###########################################################


# 1. Calculating and reporting the number of rows with NA

comdata <- complete.cases(dataframe)  
narows <- length(which(comdata == "FALSE"))
print(paste('Number of missing value in the dataset: ', narows))


# 2. Filling in the missing values with mean value of that interval and
# 3. Create a new dataset with missing values filled in

dataframe.fill <- cbind(dataframe,comdata)                         
incomplete.cases <- which(dataframe.fill$comdata == FALSE)
for (r in 1:length(incomplete.cases)){
  dataframe.fill$steps[incomplete.cases[r]] <- outint$steps[which(outint$interval == 
                                                                    dataframe.fill$interval[incomplete.cases[r]])]
}


# 4. Calculating the histogram of the total number of steps taken each day

outdt.fill <- dataframe.fill %>% group_by(date) %>% summarise_all(funs(sum))
total.steps.per.day.fill <- outdt.fill$steps
brkstps <- seq(0,25000,2500)
hist(total.steps.per.day.fill, breaks = brkstps, xlab = "Total steps", ylab = "Frequency", 
     main = "Histogram of the total number of steps\n taken each day (with imputing data)")


# 4. Calculating the mean and median total number of steps taken per day

meansteps.fill <- mean(total.steps.per.day.fill, na.rm = T)
mediansteps.fill <- median(total.steps.per.day.fill, na.rm = T)
print(paste('Mean number of steps taken per day: ', meansteps.fill))
print(paste('Median number of steps taken per day: ', mediansteps.fill))





######################################################################
#...DIFFERENCES IN ACTIVITY PATTERNS BETWEEN WEEKDAYS AND WEEKENDS...#
######################################################################



# 1. Creating new factor variable for weekends and weekdays

dataframe.fill$day <- weekdays(dataframe.fill$date)   
for (i in 1:nrow(dataframe.fill)) {                      
  if (dataframe.fill[i,]$day %in% c("Saturday","Sunday")) { 
    dataframe.fill[i,]$day <- "weekend"        
  }
  else{
    dataframe.fill[i,]$day <- "weekday"          
  }
}


# 2. Creating a panel plot of the interval and the average number of steps taken
#      averaged across all weekday or weekend days

data.weekend <- dataframe.fill[which(dataframe.fill$day == "weekend"),]
data.weekday <- dataframe.fill[which(dataframe.fill$day == "weekday"),]
outint.fill.weekend <- data.weekend %>% group_by(interval) %>% summarise(steps=mean(steps, na.rm = T))
outint.fill.weekday <- data.weekday %>% group_by(interval) %>% summarise(steps=mean(steps, na.rm = T))

par(mfrow=c(2,1))
#owner <- factor(outint.fill.weekday$interval)
plot(outint.fill.weekend$steps ~ outint.fill.weekend$interval , type="l",  
       main="Time Series plot of 5 minute Interval\n (Weekend)", ylab="Average number of Steps", xlab="Interval INDEX")
plot(outint.fill.weekday$steps ~ outint.fill.weekday$interval, type="l",  
       main="Time Series plot of 5 minute Interval\n (Weekdays)", ylab="Average number of Steps", xlab="Interval INDEX") 


#  http://redheadedstepdata.io/a-few-simple-plots-in-r/
# http://rstudio-pubs-static.s3.amazonaws.com/2056_f7e31337ebe64911b83eb0017a9d1626.html