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

# library(quantmod)
# library(ggplot2)
# library(gridExtra)
library(zoo)
library(xts)
library(tseries) # adf.test
library(TSA) # kurtosis, skewness
library(fGarch)
library(rugarch)
library(forecast)

# my ggplot helper class
source('ggplothelpers.R') 

global.xlab <- 'Year'
global.ylab <- 'Closing Price (USD)'

## ----- Get data from Yahoo ----- ###
# Fetch apple stock prices from 2002-02-01 to 2017-01-31
stock.symbols <- c('AAPL') 
stock.all <- getSymbols(stock.symbols, from='2002-02-01', to='2017-02-01') 
stock.data <- get(stock.all[1])
chartSeries(stock.data, theme = "white", name = stock.symbols[1])
class(AAPL) # xts object
aapl.c <- stock.data[,4] # extract Close price
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
# Step 4: STL Decomposition
aapl.c.monthly <- to.monthly(stock.data)
adj <- Ad(aapl.c.monthly)
freq <- 12
adj.ts <- ts(adj, frequency = freq)
fit.stl <- stl(adj.ts[,1], s.window = "period")
autoplot(fit.stl, main="STL Decomposition")

## ----- Data transformation ----- ##
lambda = BoxCox.lambda(aapl.c) %>% print() #0.1146397
lambda = 0 # log transform
percentage = 1 # change to 1 to remove multiplication
aapl.r <- diff(BoxCox(aapl.c, lambda))*percentage
aapl.r <- aapl.r[2:length(aapl.r)]
names(aapl.r) <- 'Apple Daily Return (2002-2017) * 100'
plotts(aapl.r)

plotAcf2(aapl.r)
plotAcf2(abs(aapl.r))
plotAcf2(aapl.r^2)

## ----- Split train/test set ----- ##
# num_of_train <- round(0.95 * length(aapl.c)) %>% print()
num_of_train <- (length(aapl.r) - 120) %>% print()
aapl.train <- head(aapl.r, num_of_train)
aapl.test <- tail(aapl.c, round(length(aapl.r) - num_of_train))
length(aapl.r); length(aapl.train); length(aapl.test)
# plotts(aapl.c)
plotts(aapl.train)

## ----- Plot QQ Plot ----- ##
dev.off()
qqnorm(aapl.r)
qqline(aapl.r, col = 2)
skewness(aapl.r); kurtosis(aapl.r) 

## ----- EACF ----- ##
eacf(aapl.r)
eacf(abs(aapl.r))
eacf(aapl.r^2)

## ----- GARCH ----- ##
garch.40 <- garch(aapl.r, order=c(4,0), trace=F)
# summary(garch.40)
AIC(garch.40) # 16806.18; -17926.01
garch.11 <- garch(aapl.r, order=c(1,1), trace=F)
# summary(garch.11)
AIC(garch.11) # 16205.51; -18554.31
garch.22 <- garch(aapl.r, order=c(2,2), trace=F) 
# summary(garch.22)
AIC(garch.22) # 16577.53; -18446.87
garch.33 <- garch(aapl.r, order=c(3,3), trace=F)
# summary(garch.33)
AIC(garch.33) # 16507.91; -18472.86
garch.21 <- garch(aapl.r, order=c(2,1), trace=F)
# summary(garch.21)
AIC(garch.21) # 16201.29; -18549.33
garch.31 <- garch(aapl.r, order=c(3,1), trace=F)
# summary(garch.31)
AIC(garch.31) # 16195; -18512.5

## ----- GARCH Diagnostic Checking ----- ##
plot(residuals(garch.11),type='h',ylab='Standardized Residuals', main='GARCH(1,1)')
#win.graph(width=2.5,height=2.5,pointsize=8)
qqnorm(residuals(garch.11)); qqline(residuals(garch.11), col = 2)
plotAcf2(abs(residuals(garch.11)))
plotAcf2(residuals(garch.11)^2)
gBox(garch.11,method='squared')

plot(residuals(garch.31),type='h',ylab='Standardized Residuals', main='GARCH(3,1)')
qqnorm(residuals(garch.31)); qqline(residuals(garch.31), col = 2)
plotAcf2(abs(residuals(garch.31)))
plotAcf2(residuals(garch.31)^2)
gBox(garch.31,method='squared')
tsdisplay(residuals(garch.31), lag.max = 40, main="GARCH(3,1)")

## 
autoarima = auto.arima(aapl.train)
tsdiag(autoarima)
pred.autoarima <- forecast(autoarima, h = length(aapl.test)+20)$mean
accuracy(pred.autoarima, aapl.r)
fgarch11 <- garchFit(formula=arma(2,2)~garch(1,1),data=aapl.train,trace=F,cond.dist="std")
garch11_predict = predict(fgarch11, n.ahead = 120)
prediction <- predict(model.fit, n.ahead = 20,plot=TRUE,conf=.95,nx=100) 
prediction

## ----- Model GARCH using rugarch package ----- ##
# ARMA(0,0)-GARCH(1,1) model with t-distribution
garch11.t.spec <- ugarchspec(variance.model=list(garchOrder=c(1,1)), 
                             mean.model=list(armaOrder=c(0,0)), distribution.model = "std")
garch11.t.fit <- ugarchfit(spec=garch11.t.spec, data=aapl.r)
garch11.t.fit # -5.0171
plot(garch11.t.fit, which = "all")
# ARMA(0,0)-GARCH(1,1) model with skewed t-distribution
garch11.skt.spec <- ugarchspec(variance.model=list(garchOrder=c(1,1)), 
                               mean.model=list(armaOrder=c(0,0)), distribution.model = "sstd")
garch11.skt.fit <- ugarchfit(spec=garch11.skt.spec, data=aapl.r)
garch11.skt.fit # -5.0155
plot(garch11.skt.fit, which = "all")
# ARMA(0,0)-eGARCH(1,1) model with t-distribution
egarch11.t.spec <- ugarchspec(variance.model=list(model = "eGARCH", garchOrder=c(1,1)), 
                              mean.model=list(armaOrder=c(0,0)), distribution.model = "std")
egarch11.t.fit <-  ugarchfit(spec=egarch11.t.spec, data=aapl.r)
egarch11.t.fit # -5.0279
plot(egarch11.t.fit, which = "all")
# ARMA(0,0)-fGARCH(1,1) model with t-distribution
fgarch11.t.spec<-ugarchspec(variance.model=list(model = "fGARCH", garchOrder=c(1,1), submodel = "APARCH"), 
                           mean.model=list(armaOrder=c(0,0)), distribution.model = "std")
fgarch11.t.fit<-ugarchfit(spec=fgarch11.t.spec, data=aapl.r)
fgarch11.t.fit #-5.0264 
plot(fgarch11.t.fit, which = "all")
# ARMA(0,0)-Igarch model
igarch11.t.spec<-ugarchspec(variance.model=list(model = "iGARCH", garchOrder=c(1,1)), 
                           mean.model=list(armaOrder=c(0,0)), distribution.model = "std")
igarch11.t.fit<-ugarchfit(spec=igarch11.t.spec, data=aapl.r)
igarch11.t.fit #-5.0162
plot(igarch11.t.fit, which = "all")

rff=ugarchfit(spec=egarch11.t.spec, data=aapl.r, out.sample=500)
rf=ugarchforecast(rff, n.ahead=20, n.roll=450)
rf
plot(rf, which="all")

#https://github.com/letianzj/QuantTrading/blob/e0bf14db3daaf021893d991a27baba4b72dd158c/Strategies/R/Trading.R
#https://github.com/jinlin82/time_series/blob/5974e335133c284fd5afc8ad03789fa259859df2/Exercise/whh.rmd
#https://github.com/Pariyat/wqu-econometrics-group-6-A-w5/blob/a896b9ee44e3668346bca444154202bd256ed68e/GroupworkAssignmentSubmission_2_M5.Rmd
# https://www.youtube.com/watch?v=JjrrwEn-2uI


