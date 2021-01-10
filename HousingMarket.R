# Ankit Rustagi
# Project 12

# Question 1
library(lubridate)
states <- read.csv("/class/datamine/data/zillow/State_time_series.csv")
class(states$Date)
typeof(states$Date)

# Question 2
# ymd, mdy, dmy
head(states)
states$Date <- ymd(states$Date)
class(states$Date)

# Question 3
# I prefer using these functions instead of what was used in the previous project for (3) as these have proven to be a faster and more efficient solution for solving the problem.
states$year <- year(states$Date)
states$month <- month(states$Date)
states$day_of_week <- wday(states$Date, label = TRUE)

table(states$year, useNA = "no")
table(states$month, useNA = "no") 
table(states$day_of_week, useNA = "no")

# No, we do not have the same amount of data for all years, all months, and all days of the week. 

# Question 4
barplot(tapply(states$DaysOnZillow_AllHomes, states$month, mean, na.rm = TRUE),
        main="Average Days on Zillow by Month",
        xlab="Month",
        ylab="Average Days on Zillow",
        names.arg = month.abb)

# Question 5
# Although the graph is continuously fluctuating, the overall trend of the graph is that the as the year progress, the number of days on Zillow for the homes decreases.
states2010plus <- subset(states, states$year >= 2010)
states2010plusHomes <- tapply(states2010plus$DaysOnZillow_AllHomes, states2010plus$Date, mean, na.rm = TRUE)

plot(states2010plusHomes, type = "l",
     main = "Average Days on Zillow by Date",
     xlab="Date",
     ylab="Days on Zillow")

# Question 6
# After analyzing the graphs, it is clear that homes do sell faster in some states in comparison to other states.
cal <- subset(states2010plus, subset = states2010plus$RegionName %in% c("California") & !is.na(states2010plus$DaysOnZillow_AllHomes),
                        select = c("Date", "RegionName", "DaysOnZillow_AllHomes"))
ind <- subset(states2010plus, subset = states2010plus$RegionName %in% c("Indiana") & !is.na(states2010plus$DaysOnZillow_AllHomes),
              select = c("Date", "RegionName", "DaysOnZillow_AllHomes"))
ny <- subset(states2010plus, subset = states2010plus$RegionName %in% c("NewYork") & !is.na(states2010plus$DaysOnZillow_AllHomes),
              select = c("Date", "RegionName", "DaysOnZillow_AllHomes"))
fl <- subset(states2010plus, subset = states2010plus$RegionName %in% c("Florida") & !is.na(states2010plus$DaysOnZillow_AllHomes),
             select = c("Date", "RegionName", "DaysOnZillow_AllHomes"))

colors <- rainbow(4)
plot(cal$Date, cal$DaysOnZillow_AllHomes, ylim=c(1,250), main = "Days on Zillow for different States by Dates", 
     xlab="Date", ylab="Days on Zillow", type = 'l', col = colors[1])
lines(ind$Date, ind$DaysOnZillow_AllHomes, type = 'l', col = colors[2])
lines(ny$Date, ny$DaysOnZillow_AllHomes, type = 'l', col = colors[3])
lines(fl$Date, fl$DaysOnZillow_AllHomes, type = 'l', col = colors[4])

legend("topright", legend=c("California","Indiana","NewYork","Florida"), col=colors, pc=15)
