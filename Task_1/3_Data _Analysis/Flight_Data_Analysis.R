#Clear workspace
rm(list = ls())

#install.packages('ggplot2')
#install.packages('magrittr')
#install.packages('tidyverse')

#Load libraries
library(ggplot2)
library(mgcv)
library(magrittr)
library(tidyverse)

#Load flight data
flight_data <- read.csv('new_flight_data.csv', header=TRUE, stringsAsFactors=FALSE)
flight_data$fly_to <- as.factor(flight_data$fly_to)

m = gam(price~s(booking_days)+fly_to,data=flight_data)
print(summary(m))
plot(m,main='Plot of smoothed variable booking_days')

flight_pred <- data.frame(booking_days = flight_data$booking_days,
                          fly_to = flight_data$fly_to,
                          price = flight_data$price,
                          predicted_values  = predict(m, newdata = flight_data))

tmpplot<-ggplot(flight_pred, aes(x = booking_days)) +
  geom_point(aes(y = price), size = 1, alpha = 0.5) + geom_line(aes(y = predicted_values), colour = "red")
plot(tmpplot,sub='Plot of smoothed variable booking_days')

result <- flight_pred[0,]
for(dest in unique(flight_pred$fly_to)){
  tmp_data <- flight_pred[flight_pred$fly_to == dest,]
  min_idx <- which(tmp_data$predicted_values == min(tmp_data$predicted_values))
  result <- rbind(result, tmp_data[min_idx,])
}

min_booking_days.mean <- mean(result$booking_days)
min_booking_days.median <- median(result$booking_days)