#ID = 43, 
43%%6
#Data: Students with mod( ID_last_two_digits, 6) = 1 use AirPassengers
43%%3
#Title for Assignment 2:mod( ID_last_two_digits, 3) = 1 use title 2
##"Evaluating Forecast on AirPassengers Data: An Evidence of ARIMA vs. ETS"##

library(ggfortify)
library(tseries)
library(forecast)


data(AirPassengers)
AP <- AirPassengers
class(AP)

#Part 1

AP #PERFORM EXPLORATORY DATA ANALYSIS


sum(is.na(AP)) #missing values


frequency(AP) # frequency


cycle(AP) #TS cycle

summary(AP) # summary

# Plot the raw data

plot(AP,xlab="Date", ylab = "Passenger numbers (1000's)",main="Air Passenger numbers from 1949 to 1961")


#As an alternative 
autoplot(AP) + labs(x ="Date", y = "Passenger numbers (1000's)", title="Air Passengers from 1949 to 1961") 

#boxplot for seasonal effects check.
boxplot(AP~cycle(AP),xlab="Date", ylab = "Passenger Numbers (1000's)" ,main ="Monthly Air Passengers Boxplot from 1949 to 1961")


#Part 2: 

decomposeAP <- decompose(AP,"multiplicative")  #TIME SERIES DECOMPOSITION
autoplot(decomposeAP)

#Part 3: 

##1. adf

adf.test(AP) 

plot(AirPassengers)+abline(reg=lm(AirPassengers~time(AirPassengers)))  #TEST STATIONARITY OF THE TIME SERIES

##2. Test stationarity

autoplot(acf(AP,plot=FALSE))+ labs(title="Correlogram of Air Passengers from 1949 to 1961") 

acf(diff(log(AirPassengers)))

##It determine value of q(value 1)

pacf(diff(log(AirPassengers))) 

#It determine value of p (value 0)

#d is number of time you do the differentiations to make the mean
#We do diff only one time so value of d is 1
plot(diff(log(AirPassengers)))

#Step 3: 
plot(log(AirPassengers))  

plot(diff(log(AirPassengers))) 

plot(aggregate(AirPassengers,FUN = mean))

#got upword trend

# any missing values
decomposeAP$random 


#  7:138 which exclude the NA values
autoplot(acf(decomposeAP$random[7:138],plot=FALSE))+ labs(title="Correlogram of Air Passengers Random Component from 1949 to 1961") 

#Part 4: FIT A TIME SERIES MODEL

##1. Linear


autoplot(AP) + geom_smooth(method="lm") + labs(x ="Date", y = "Passenger numbers (1000's)", title="Air Passengers from 1949 to 1961")

##2. ARIMA

arimaAP <- auto.arima(AP)
arimaAP

ggtsdiag(arimaAP)

#Part 5: FORECASTing

forecastAP <- forecast(arimaAP, level = c(95), h = 36)
autoplot(forecastAP)




