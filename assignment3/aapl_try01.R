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
library(quantmod)
library(ggplot2)
library(gridExtra)
library(zoo)
library(xts)
library(tseries) # adf.test
library(FinTS)   # ArchTest
library(rugarch)
source('ggplothelpers.R')

global.xlab <- 'Year'
global.ylab <- 'Closing Price (USD)'

## ----- Get data from Yahoo ----- ###
# Fetch apple stock prices from 2002-02-01 to 2017-01-31
getSymbols('AAPL', from='2002-02-01', to='2017-02-01') 
class(AAPL) # xts object
aapl.c <- AAPL[,4] # extract Close price
names(aapl.c) <- 'Apple Stock Prices (2002-2017)'
head(aapl.c)

## ----- Examine data ----- ###
# Step1: Plot timeplot to view volaility clustering
plotts(aapl.c)
# Step2: Plot ACF and PACF
plotAcf2(aapl.c)
# Step3: Augmented Dickey-Fuller Test
adf.test(aapl.c) # p-value = 0.4114
# kpss.test(aapl.c) # p-value = 0.01

## ----- Load data from CSV ----- ###
# aapl.c<-read.csv('AAPL.csv')
# aapl.c$Date <- as.Date(aapl.c$Date, format = '%Y-%m-%d')
# aapl.c <- data.frame(aapl.c[,c(1,5)])
# aapl.c <- zoo(aapl.c[,2], as.Date(aapl.c[,1],format="%m/%d/%Y"))
# aapl.c <- xts(aapl.c)
# names(aapl.c) <- 'Apple Stock Prices'
# plotts(aapl.c)
# plotAcf2(aapl.c)
# adf.test(aapl.c) # p-value = 0.4114

## ----- Try with auto.arima ----- ###
# auto.arima(aapl.c, seasonal=FALSE) #ARIMA(0,1,0) with drift AIC=10321.05 
# aafit010 <- auto.arima(aapl.c, seasonal=FALSE)
# tsdisplay(residuals(aafit010), lag.max = 40, main="AUTO ARIMA(0,1,0)")
# 
# arfit1134 <- arima(aapl.c, order=c(1,1,34))
# tsdisplay(residuals(arfit1134), lag.max = 40, main="ARIMA(1,1,34)")
# AIC(arfit1134)

## ----- Normalize data ----- ###
aapl.r <- diff(log(aapl.c))
aapl.r <- aapl.r[2:length(aapl.r)]
names(aapl.r) <- 'Apple Daily Return (2002-2017)'
plotts(aapl.r)
plotAcf2(aapl.r)
plotAcf2(abs(aapl.r))
plotAcf2(aapl.r^2)

## ----- Plot QQ Plot ----- ###
dev.off()
qqnorm(aapl.r)
qqline(aapl.r, col = 2)
skewness(aapl.r); kurtosis(aapl.r) 
eacf(abs(aapl.r))
eacf(aapl.c^2)

## ----- Split train/test set ----- ##
length(aapl.c)
num_of_train <- round(0.8 * length(aapl.c)) %>% print()
aapl.train <- head(aapl.c, num_of_train)
aapl.test <- tail(aapl.c, round(length(aapl.c) - num_of_train))
length(aapl.test)


garch.11=garch(aapl.r, order=c(1,1))
summary(garch.11)
AIC(garch.11) # -18554.31

plot(residuals(garch.11),type='h',ylab='Standardized Residuals')
#win.graph(width=2.5,height=2.5,pointsize=8)
qqnorm(residuals(garch.11)); qqline(residuals(garch.11))
plotAcf2(abs(residuals(garch.11)))
plotAcf2(residuals(garch.11)^2)
gBox(garch.11,method='squared')

garch.22=garch(aapl.r, order=c(2,1))
summary(garch.22)
AIC(garch.22) #-18549.33
plot(residuals(garch.22),type='h',ylab='Standardized Residuals')
plotAcf2(abs(residuals(garch.22)))
plotAcf2(residuals(garch.22)^2)
gBox(garch.22,method='squared')

#https://github.com/letianzj/QuantTrading/blob/e0bf14db3daaf021893d991a27baba4b72dd158c/Strategies/R/Trading.R
#https://github.com/jinlin82/time_series/blob/5974e335133c284fd5afc8ad03789fa259859df2/Exercise/whh.rmd
#https://github.com/Pariyat/wqu-econometrics-group-6-A-w5/blob/a896b9ee44e3668346bca444154202bd256ed68e/GroupworkAssignmentSubmission_2_M5.Rmd
# https://www.youtube.com/watch?v=JjrrwEn-2uI


