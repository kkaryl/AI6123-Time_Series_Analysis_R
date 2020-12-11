# Title     : NTU Time Series R Lecture 3 (20200129)
# Objective : Try BoxCox and Regression model on passenger data
# Created by: Karyl
# Created on: 29/1/2020

# Create a vector of airline
Airline=c(878, 1005, 1173, 883, 972, 1125, 1336, 988, 1020, 1146, 1400, 1006, 1108, 1288, 1570, 1174, 1227, 1468, 1736, 1283) #x

logAirline = log(Airline) #y

plot.ts(Airline)
plot.ts(logAirline)

# In order to demonstrate the Box-Cox transformation,
# I’ll introduce Rob Hyndman’s `forecast` package:
# install.packages('forecast')
library(forecast)

# It has two functions that are of use here.
# The primary function is BoxCox(),
# which will return a transformed time series given
# a time series and a value for the parameter lambda:
?BoxCox
?BoxCox.lambda
plot.ts(BoxCox(logAirline, lambda = 0.5))

# We can use the BoxCox.lambda() function,
# which will provide us an optimal value for parameter lambda:
lambda <- BoxCox.lambda(Airline)
print(lambda)

# Import the data "passenger"
passenger=read.table("data/passenger.txt")

# Build a regression model
?lm # Used to fit linear models
mlr <- lm(y_t~t+D_1+D_2+D_3, data=passenger)
summary(mlr)

# Produce the actual data and predictions in one figure
?plot
y <- logAirline #y = log(x)
plot(1:20, y, xlim = c(0, 26), ylim=c(6.6, 7.6))
lines(1:20, y, type="l" )
lines(1:20, y-mlr$residuals, type="l", col="red")

# Add predictions
?predict
?forecast

predict(mlr, n.ahead=4)




