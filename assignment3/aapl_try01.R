# This project is to analyze financial data.
# The data are from the daily historical Apple stock prices
# (open, high, low, close and adjusted prices)
# from February 1, 2002 to January 31, 2017
# extracted from the Yahoo Finance website.
# The data has logged the prices of the Apple stock everyday
# and comprises of the open, close, low, high and
# the adjusted close prices of the stock for the span of 15 years.
# The goal of the project is to discover an interesting trend
# in the apple stock prices over the past 15 years (3775 attributes)
# and to design and develop the best model for forecasting.
# https://learn.datacamp.com/courses/garch-models-in-r
library(ggplot2)
library(gridExtra)
library(zoo)
library(xts)
source('ggplothelpers.R')

# Load data and preprocess
# aapl.df<-read.csv('AAPL.csv')
# aapl.df$Date <- as.Date(aapl.df$Date, format = '%Y-%m-%d')
# aapl.df <- data.frame(aapl.df[,c(1,5)])
# lastdate <- nrow(aapl.df)
#
# autoplot(aapl.df)
# plot(aapl.df$Date, aapl.df$Close,
#      main =  'Apple Stock Prices',
#      xlab = 'Date',
#      ylab = 'Closing Price(USD)',
#      type = 'l',
#      col = 'blue')
global.xlab <- 'Year'
global.ylab <- 'Closing Price (USD)'

aapl.p<-read.csv('AAPL.csv')
aapl.p$Date <- as.Date(aapl.p$Date, format = '%Y-%m-%d')
aapl.p <- data.frame(aapl.p[,c(1,5)])
aapl.p <- zoo(aapl.p[,2], as.Date(aapl.p[,1],format="%m/%d/%Y"))
aapl.p <- xts(aapl.p)
names(aapl.p) <- 'Apple Stock Prices'
plotts(aapl.p)
plotAcf2(aapl.p)
adf.test(aapl.p)

aapl.r <- diff(log(aapl.p))
aapl.r <- aapl.r[2:length(aapl.r)]
names(aapl.r) <- 'Apple Daily Return'
plotts(aapl.r)
plotAcf2(aapl.r)
plotAcf2(abs(aapl.r))
plotAcf2(aapl.r^2)

## ----- Plot QQ Plot ----- ###
dev.off()
qqnorm(aapl.r)
qqline(aapl.r)
library(tseries)
skewness(aapl.r); kurtosis(aapl.r) 
eacf(abs(aapl.r))
eacf(aapl.r^2)


library(tseries)
garch.11=garch(aapl.r, order=c(1,1))
summary(garch.11)
AIC(garch.11) # -18554.31

plot(residuals(garch.11),type='h',ylab='Standardized Residuals')
#win.graph(width=2.5,height=2.5,pointsize=8)
qqnorm(residuals(garch.11)); qqline(residuals(garch.11))
plotAcf2(abs(residuals(garch.11)))
plotAcf2(residuals(garch.11)^2)
gBox(garch.11,method='squared')

garch.22=garch(aapl.r, order=c(2,2))
summary(garch.22)
AIC(garch.22) #-18446.87
plot(residuals(garch.22),type='h',ylab='Standardized Residuals')
plotAcf2(abs(residuals(garch.22)))
plotAcf2(residuals(garch.22)^2)
gBox(garch.22,method='squared') #nope


