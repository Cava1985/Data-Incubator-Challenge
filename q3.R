setwd('/Users/Daniele_Cavaglieri/Desktop/Data Incubator Challenge')
library(sqldf)
library(splitstackshape)
library(plyr)
library(ggmap)
library(zoo)
library(grid)
library(gridExtra)

database <- read.csv(file="SFPD_Incidents_-_from_1_January_2003.csv", header=TRUE, sep=",")

mydata <- database[c("Category", "Date", "Time", "DayOfWeek", "PdDistrict", "X", "Y")] 
names(mydata) <- tolower(names(mydata))

mydata$date <- as.Date(mydata$date,"%m/%d/%Y")
mydata$year <- as.numeric(format(mydata$date, "%Y")) 
mydata$month <- as.numeric(format(mydata$date, "%m"))
mydata$day <- as.numeric(format(mydata$date, "%d"))
mydata$year_month <- as.yearmon(mydata$date)

mydata$time2 <- mydata$time
mydata <- cSplit(mydata, c("time2"), c(":"))
mydata <- rename(mydata, c("time2_1"="hour","time2_2"="minute"))

assaults <- sqldf('select * from mydata where year < 2015 and category="ASSAULT"')
p1 <- ggplot(data=assaults, aes(assaults$x, assaults$y)) + geom_point(aes(color=assaults$pddistrict), size=1.5) + 
  xlim(-122.5,-122.35) + ylim(37.7,37.85) + xlab("x") + ylab("y") + labs(colour = "PD District")

p2 <- ggplot(data.frame(assaults$pddistrict), aes(x=assaults$pddistrict)) + geom_bar() +
  xlab("PD District") + ylab("Number of assaults")

grid.arrange(p1, p2, nrow = 2, top = "Assaults in SF for each district")

count_mon <- sqldf('select month, count(*) from assaults group by month')
p3 <- ggplot(data.frame(count_mon), aes(count_mon[,1], count_mon[,2])) + 
  geom_line(color='blue') + geom_point(color='blue') + 
  scale_x_continuous(breaks = c(seq(1,12)), labels = c(seq(1,12))) +
  xlab("Month") + ylab("Number of assaults") + ggtitle("Assaults in SF for each month")

count_hour <- sqldf('select hour, count(*) from mydata where year < 2015 and category="ASSAULT" group by hour')
p4 <- ggplot(data.frame(count_hour), aes(count_hour[,1], count_hour[,2])) + 
  geom_line(color='red') + geom_point(color='red') +
  scale_x_continuous(breaks = c(seq(0,23)), labels = c(seq(0,23))) +
  xlab("Hour") + ylab("Number of assaults") + ggtitle("Assaults in SF for each hour")

grid.arrange(p3, p4, nrow = 2, top = "Assaults in SF for each time of the day and year")

