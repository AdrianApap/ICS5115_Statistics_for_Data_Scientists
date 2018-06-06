#Clear workspace
rm(list = ls())

#install.packages('nortest')
#install.packages('moments')
#install.packages('ggplot2')
#install.packages('tidyr')

#Load libraries
library(nortest)
library(moments) 
library(ggplot2)
library(tidyr)

#P value constant
P_VALUE <- 0.05

#Helper functions
check_hyp <- function(hyp, p) {
  if(p < P_VALUE) {
    print(paste('Reject H0 p-value:',p,hyp[2]))
    return(TRUE)
  }else{
    print(paste('Not enough evidence to reject H0 p-value:',p,hyp[1]))
    return(FALSE)
  }
}

#Load flight data
flight_data <- read.csv("flight_data.csv", header=TRUE, stringsAsFactors=FALSE)

#Split departure date and time
flight_data <- flight_data %>% separate(departure_time, c('departure_date','departure_time'), sep=' ', remove=TRUE)

#Change type of date columns
flight_data$date <- as.Date(flight_data$date)
flight_data$departure_date <- as.Date(flight_data$departure_date)

flight_data <- flight_data[colnames(flight_data)[colnames(flight_data) != 'id']]
flight_data <- flight_data[colnames(flight_data)[colnames(flight_data) != 'airlines']]
flight_data <- flight_data[colnames(flight_data)[colnames(flight_data) != 'fly_duration']]
flight_data <- flight_data[colnames(flight_data)[colnames(flight_data) != 'distance']]
flight_data <- flight_data[colnames(flight_data)[colnames(flight_data) != 'departure_time']]
flight_data <- flight_data[colnames(flight_data)[colnames(flight_data) != 'arrival_time']]

#Calculate number of days before departure date
flight_data$booking_days <- flight_data$departure_date - flight_data$date
#Convert to integer
flight_data$booking_days <- as.integer(flight_data$booking_days)

#Set time column to factor
flight_data$time <- as.factor(flight_data$time)

#Histogram of prices
hist.price <- ggplot(flight_data, aes(x = price)) +
  geom_histogram(bins=100,aes(y=..count../1000),color='black',fill = 'darkgrey',alpha = 0.7) +
  labs(x='Price in \u20ac', y='Count (in thousands)') +
  ggtitle('Histogram of Flight Prices in \u20ac') +
  theme(plot.title=element_text(hjust = 0.5))
plot(hist.price)

#Test for normality

#QQ plot of price
qqnorm(flight_data$price)
qqline(flight_data$price)

#Shapiro-Wilk test
#Not possible sample size must be between 3 and 5000
#normality.sw <- shapiro.test(flight_data$price)

#Anderson-Darling normality test
hypothesis <- c('H0: Flight price is normally distributed.','H1: Flight price is not normally distributed')
normality.ad <- ad.test(flight_data$price)
check_hyp(hypothesis,normality.ad$p.value)

#Kolmogorov-Smirnov test
normality.ks1 <- ks.test(flight_data$price,rnorm(10000))
check_hyp(hypothesis,normality.ks1$p.value)

normality.ks2 <- ks.test(flight_data$price,rnorm(10000,mean=mean(flight_data$price),sd=sd(flight_data$price)))
check_hyp(hypothesis,normality.ks2$p.value)

#Box plot of prices against time
box.time <- ggplot(flight_data, aes(x=time, y=price)) +
  geom_boxplot() +
  coord_flip() +
  labs(y='Price in \u20ac', x='Time of booking') +
  ggtitle('Box Plot of Time of booking against Flight Price') +
  theme(plot.title=element_text(hjust = 0.5))
plot(box.time)

#Summary of price data grouped by time
result.time_summary_AM <- summary(flight_data[flight_data$time == '09:00',]$price)
result.time_summary_PM <- summary(flight_data[flight_data$time == '21:00',]$price)

#Check if there is a difference between 9:00 and 21:00
hypothesis <- c('H0: Time does not affect flight prices.','H1: Time affects flight prices.')
result.kruskal <- kruskal.test(price ~ time, data=flight_data) 
check_hyp(hypothesis, result.kruskal$p.value)

#Remove time column from data and keep only unique rows
flight_data <- unique(flight_data[colnames(flight_data)[(colnames(flight_data) != 'time')]])
#Remove country of origin column (Always Malta)
flight_data <- unique(flight_data[colnames(flight_data)[(colnames(flight_data) != 'fly_from')]])

#For each unique destination and booking days combination
#Merge the prices by taking the median
new_flight_data <- flight_data[0,]
for(dest in unique(flight_data$fly_to)){
  tmp_data <- flight_data[flight_data$fly_to == dest,]
  for(booking_day in unique(tmp_data$booking_days)){
    tmp_booking_data <- tmp_data[tmp_data$booking_days == booking_day,]
    tmp_row <- tmp_booking_data[1,]
    tmp_row$price <- median(tmp_booking_data$price)
    
    new_flight_data <- rbind(new_flight_data, tmp_row)
  }
}

#Set flight data to new flight data
flight_data <- new_flight_data

#Histogram of prices
hist.price <- ggplot(flight_data, aes(x = price)) +
  geom_histogram(bins=100,aes(y=..count../1000),color='black',fill = 'darkgrey',alpha = 0.7) +
  labs(x='Price in \u20ac', y='Count (in thousands)') +
  ggtitle('Histogram of Flight Prices in \u20ac') +
  theme(plot.title=element_text(hjust = 0.5))
plot(hist.price)

#Test for normality again with new data

#QQ plot of price
qqnorm(flight_data$price)
qqline(flight_data$price)

#Anderson-Darling normality test
hypothesis <- c('H0: Flight price is normally distributed.','H1: Flight price is not normally distributed')
normality.ad <- ad.test(flight_data$price)
check_hyp(hypothesis,normality.ad$p.value)

#Kolmogorov-Smirnov test
normality.ks1 <- ks.test(flight_data$price,rnorm(10000))
check_hyp(hypothesis,normality.ks1$p.value)

normality.ks2 <- ks.test(flight_data$price,rnorm(10000,mean=mean(flight_data$price),sd=sd(flight_data$price)))
check_hyp(hypothesis,normality.ks2$p.value)

#Save new data to csv
write.csv(flight_data, file='new_flight_data.csv', row.names=FALSE)