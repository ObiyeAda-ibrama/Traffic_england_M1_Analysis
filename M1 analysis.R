library(readxl)
trafficData <- read_excel("NO 10 data.xlsx",
col_types = c("date", "numeric", "text",
"numeric", "text", "text", "numeric",
"text", "text"))
View(trafficData)

dim(trafficData)#dimension
str(trafficData)#structure
summary(trafficData)#summary of data

#DATA CLEANING
#checking for missingness
missing_data <- colSums(is.na(trafficData)) #shows sum of missing data for all features
missing_data
install.packages('dyplr')
library(dplyr)
trafficData$NB_traffic_conditions <- ifelse(is.na(trafficData$NB_traffic_conditions) == TRUE, 'None', trafficData$NB_traffic_conditions)#replacing missing value with phrase 'None'
trafficData$SB_traffic_conditions <- ifelse(is.na(trafficData$SB_traffic_conditions) == TRUE, 'None', trafficData$SB_traffic_conditions)#replacing missing value with phrase 'None'

#making traffic conditions factors
trafficData$NB_traffic_conditions <- as.factor(trafficData$NB_traffic_conditions)
trafficData$SB_traffic_conditions <- as.factor(trafficData$SB_traffic_conditions)

#visualizing missing data
install.packages('naniar')
library(naniar)
vis_miss(trafficData)#to view missing data
mcar_test(trafficData) #checking missingness

#distribution for missing features
hist(trafficData$NB_speed)#poisson
hist(trafficData$SB_speed)#poisson

#finding median 
summary(trafficData$NB_speed)
summary(trafficData$SB_speed)

#MoCT
trafficData$NB_speed <- ifelse(is.na(trafficData$NB_speed) == TRUE, 66, trafficData$NB_speed)#replacing missing value with median
trafficData$SB_speed <- ifelse(is.na(trafficData$SB_speed) == TRUE, 65, trafficData$SB_speed)#replacing missing value with median

#DATA PROCESSING
#splitting data
#into days
monday_data <- subset(trafficData, DAYS == "Monday")
tuesday_data <- subset(trafficData, DAYS == "Tuesday")
Wednesday_data <- subset(trafficData, DAYS == "Wednesday")
thursday_data <- subset(trafficData, DAYS == "Thursday")
friday_data <- subset(trafficData, DAYS == "Friday")
saturday_data <- subset(trafficData, DAYS == "Saturday")
sunday_data <- subset(trafficData, DAYS == "Sunday")
#into times
twelveam <- subset(trafficData, TIME == 0)
threeam <- subset(trafficData, TIME == 3)
sixam <- subset(trafficData, TIME == 6)
nineam <- subset(trafficData, TIME == 9) 
twelvePM <- subset(trafficData, TIME == 12)
threePM <- subset(trafficData, TIME == 15)
sixPM <- subset(trafficData, TIME == 18)
ninePM <- subset(trafficData, TIME == 21)
#into dates
data_22 <- subset(trafficData, DATE == "2022-02-22")
data_23 <- subset(trafficData, DATE == "2022-02-23")
data_24 <- subset(trafficData, DATE == "2022-02-24")
data_25 <- subset(trafficData, DATE == "2022-02-25")
data_26 <- subset(trafficData, DATE == "2022-02-26")
data_27 <- subset(trafficData, DATE == "2022-02-27")
data_28 <- subset(trafficData, DATE == "2022-02-28")
data_1 <- subset(trafficData, DATE == "2022-03-01")
data_2 <- subset(trafficData, DATE == "2022-03-02")
data_3 <- subset(trafficData, DATE == "2022-03-03")
data_4 <- subset(trafficData, DATE == "2022-03-04")
data_5 <- subset(trafficData, DATE == "2022-03-05")
data_6 <- subset(trafficData, DATE == "2022-03-06")
data_7 <- subset(trafficData, DATE == "2022-03-07")

#to get mean speed for all days
summary(monday_data)
summary(tuesday_data)
summary(Wednesday_data)
summary(thursday_data)
summary(friday_data)
summary(saturday_data)
summary(sunday_data)

#create dataframe for days and average speed
days <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday')
mean_NBspeeds <- c(64.19, 64.02, 63.87, 64.03, 64.01, 64.78, 65.12)
mean_sBspeeds <- c(62.94, 62.44, 62.15, 63.18, 62.52, 63.85, 63.33)
days_meanspeed <- data.frame(days, mean_NBspeeds, mean_sBspeeds)

#mean speed for all times
summary(twelveam)
summary(threeam)
summary(sixam)
summary(nineam)
summary(twelvePM)
summary(threePM)
summary(sixPM)
summary(ninePM)

#create dataframe for times and average speed
times <- c(0, 3, 6, 9, 12, 15, 18, 21)
meanNBspeeds <- c(63.84, 63.31, 64.67, 65.05, 64.42, 63.44, 64.99, 64.68)
meanSBspeeds <- c(62.06, 62.35, 61.51, 64.05, 63.77, 63.50, 63.51, 62.83)
times_meanspeed <- data.frame(times, meanNBspeeds, meanSBspeeds)

#mean speed for all dates
summary(data_22)
summary(data_23)
summary(data_24)
summary(data_25)
summary(data_26)
summary(data_27)
summary(data_28)
summary(data_1)
summary(data_2)
summary(data_3)
summary(data_4)
summary(data_5)
summary(data_6)
summary(data_7)

#create dataframe for dates and average speed
dates <- c(22, 23, 24, 25, 26, 27, 28, 1 , 2, 3, 4, 5, 6, 7)
meaNBspeeds <- c(63.54, 64.31, 64.17, 63.63, 64.05, 64.95, 63.48, 64.5, 63.42, 63.9, 64.39, 65.51, 65.3, 64.90)
meanSBpeeds <- c(62.26, 61.27, 62.93, 62.43, 63.89, 62.41, 62.46, 62.62, 63.03, 63.43, 62.61, 63.81, 64.24, 63.42)
dates_meanspeed <- data.frame(dates, meaNBspeeds, meanSBpeeds)




#EXPLORATION
#TRAFFIC CONDITIONS
ggplot(aes(x = SB_traffic_conditions), data = trafficData) +geom_bar(stat = "count") + ggtitle('SB TRAFFIC CONDITIONS')
ggplot(aes(x = NB_traffic_conditions), data = trafficData) +geom_bar(stat = "count") + ggtitle('NB TRAFFIC CONDITIONS')

#TIME, SPEED AND TRAFFIC CONDITIONS
ggplot(trafficData,aes(x=TIME,y=NB_speed,col=NB_traffic_conditions))+geom_point() + ylab('SPEED') + ggtitle('NORTHBOUND TRAFFIC')
ggplot(trafficData,aes(x=TIME,y=SB_speed,col=SB_traffic_conditions))+geom_point() + ylab('SPEED') + ggtitle('SOUTHBOUND TRAFFIC')


install.packages('gridExtra')
library(gridExtra)
#MONDAYS
boxplot(monday_data$NB_speed~monday_data$TIME,xlab= 'Time', ylab= 'Speed', main ='Time vs Northbound Speed (Mondays)')
boxplot(monday_data$SB_speed~monday_data$TIME,xlab= 'Time', ylab= 'Speed', main ='Time vs Southbound Speed (Mondays)')

#TUESDAYS
boxplot(tuesday_data$NB_speed~tuesday_data$TIME,xlab= 'Time', ylab= 'Speed', main ='Time vs Northbound Speed (Tuesdays)')
boxplot(tuesday_data$SB_speed~tuesday_data$TIME,xlab= 'Time', ylab= 'Speed', main ='Time vs Southbound Speed (Tuesdays)')

#WEDNESDAYS
boxplot(Wednesday_data$NB_speed~Wednesday_data$TIME,xlab= 'Time', ylab= 'Speed', main ='Time vs Northbound Speed (Wednesdays)')
boxplot(Wednesday_data$SB_speed~Wednesday_data$TIME,xlab= 'Time', ylab= 'Speed', main ='Time vs Southbound Speed (Wednesdays)')

#THURSDAYS
boxplot(thursday_data$NB_speed~thursday_data$TIME,xlab= 'Time', ylab= 'Speed', main ='Time vs Northbound Speed (Thursdays)')
boxplot(thursday_data$SB_speed~thursday_data$TIME,xlab= 'Time', ylab= 'Speed', main ='Time vs Southbound Speed (Thursdays)')

#FRIDAYS
boxplot(friday_data$NB_speed~friday_data$TIME,xlab= 'Time', ylab= 'Speed', main ='Time vs Northbound Speed (Fridays)')
boxplot(friday_data$SB_speed~friday_data$TIME,xlab= 'Time', ylab= 'Speed', main ='Time vs Southbound Speed (Fridays)')

#SATURDAYS
boxplot(saturday_data$NB_speed~saturday_data$TIME,xlab= 'Time', ylab= 'Speed', main ='Time vs Northbound Speed (Saturdays)')
boxplot(saturday_data$SB_speed~saturday_data$TIME,xlab= 'Time', ylab= 'Speed', main ='Time vs Southbound Speed (Saturdays)')

#SUNDAYS
boxplot(sunday_data$NB_speed~sunday_data$TIME,xlab= 'Time', ylab= 'Speed', main ='Time vs Northbound Speed (Sundays)')
boxplot(sunday_data$SB_speed~sunday_data$TIME,xlab= 'Time', ylab= 'Speed', main ='Time vs Southbound Speed (Sundays)')



#days vs mean speed
barplot(days_meanspeed$mean_NBspeeds~days_meanspeed$days, xlab = 'DAYS',ylab = 'Mean Speed', ylim = c(60,67), main = 'Daily Average Northbound speed')
barplot(days_meanspeed$mean_sBspeeds~days_meanspeed$days, xlab = 'DAYS',ylab = 'Mean Speed', ylim = c(60,67), main = 'Daily Average Southbound speed')

#time vs mean speed
#NORTHBOUND
plot(times_meanspeed$times,times_meanspeed$meanNBspeeds , xlab = 'TIMES',ylab = 'Mean Speed', main = 'Average Northbound speed vs TIME')
times_meanNBspeed_model <- lm(meanNBspeeds ~ times, data = times_meanspeed)
abline(times_meanNBspeed_model, col ='blue') #linear regression line
times_meanNBspeed_model

#SOUTHBOUND
plot(times_meanspeed$times,times_meanspeed$meanSBspeeds , xlab = 'TIMES',ylab = 'Mean Speed', main = 'Average Southbound speed vs TIME')
times_meanSBspeed_model <- lm(meanSBspeeds ~ times, data = times_meanspeed)
abline(times_meanSBspeed_model, col ='blue') #linear regression line
times_meanSBspeed_model

#dates vs mean speed
#NORTHBOUND
plot(dates_meanspeed$dates,dates_meanspeed$meaNBspeeds, xlab = 'DATES',ylab = 'Mean Speed', main = 'Average Northbound speed per DAY')
dates_meanNBspeed_model <- lm(meaNBspeeds ~ dates, data = dates_meanspeed)
abline(dates_meanNBspeed_model, col ='blue') #linear regression line
dates_meanNBspeed_model
#SOUTHBOUND
plot(dates_meanspeed$dates,dates_meanspeed$meanSBpeeds, xlab = 'DATES',ylab = 'Mean Speed', main = 'Average Southbound speed per DAY')
dates_meansBspeed_model <- lm(meanSBspeeds ~ dates, data = dates_meanspeed)
abline(dates_meansBspeed_model, col ='blue') #linear regression line
dates_meansBspeed_model

#time vs speed
boxplot(trafficData$NB_speed~trafficData$TIME,xlab= 'Time', ylab= 'Speed', main ='Time vs Northbound Speed')
boxplot(trafficData$SB_speed~trafficData$TIME,xlab= 'Time', ylab= 'Speed', main ='Time vs Southbound Speed')

ggplot(trafficData,aes(x=`NB_ JUNCTIONS`, y=NB_speed, col=NB_traffic_conditions))+geom_point() + ylab('SPEED') + ggtitle('NORTHBOUND JUNCTIONS TRAFFIC')


traffic2 <- trafficData
plot(trafficData$NB_speed ~ trafficData$DATE, xaxt = "n", type = "l")
axis(1, trafficData$DATE, format(trafficData$DATE, "%b %d"), cex.axis = .7)

ggplot(data = dates_meanspeed, aes(x = dates, y = meaNBspeeds,)) +
  geom_point() + labs(x = "Date", y = "Northbound speed", title = "Northbound speed")


  

#PLOTTING TIME VS MEAN SPEED FOR BOTH DIRECTIONS
plot(times, meanNBspeeds, xlab = 'Time', ylab = 'Mean Speed', main = 'Northbound Time vs Mean Speed', type = 'l', col = 'red')
axis(side = 1, at=c(0:21))

plot(times, meanSBspeeds, xlab = 'Time', ylab = 'Mean Speed', main = 'Southbound Time vs Mean Speed', type = 'l', col = 'red')
axis(side = 1, at=c(0:21))





