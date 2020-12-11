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
if(!require(quantmod)) install.packages("quantmod")
if(!require(fGarch)) install.packages("fGarch")
if(!require(rugarch)) install.packages("rugarch")

library(quantmod) #getSymbols
library(zoo)
library(xts)
library(tseries) # adf.test
library(TSA) # kurtosis, skewness
library(fGarch)
library(rugarch)
library(forecast)

# my ggplot helper class
source('ggplothelpers.R') 

# my revert data transform class
revertTransform = function(prev_value, transformed_data) {
  if (!is.na(transformed_data[1])) {
    transformed_data = rbind(prev_value, transformed_data) 
    transformed_data[1] <- NA
  }
  transformed_data[is.na(transformed_data)] <- 0
  transformed_data <- cumsum(transformed_data)
  transformed_data <- exp(transformed_data)
  transformed_data <- coredata(prev_value)[1] * transformed_data
  return(transformed_data)
}

global.xlab <<- 'Year'
global.ylab <<- 'Closing Price (USD)'

## ----- Get data from Yahoo ----- ###
# Fetch apple stock prices from 2002-02-01 to 2017-01-31
stockname <- 'AAPL'
stock.data <- getSymbols(stockname, from='2002-02-01', to='2017-02-01', src='yahoo', auto.assign = F) 
stock.data <- na.omit(stock.data)
chartSeries(stock.data, theme = "white", name = stockname)
class(stock.data) # xts object
aapl.c <- stock.data[,4] # extract Close price
names(aapl.c) <- 'Apple Stock Prices (2002-2017)'
head(aapl.c)

## ----- Examine data ----- ###
# Step1: Plot timeplot to view volaility clustering
plotts(aapl.c)
# Step2: Plot ACF and PACF
plotAcf2(aapl.c)
# Step3: Augmented Dickey-Fuller Test
adf.test(aapl.c) # p-value = 0.4114 (non-stationary)
# Step 4: STL Decomposition
aapl.c.monthly <- to.monthly(stock.data)
adj <- Ad(aapl.c.monthly)
freq <- 12
adj.ts <- ts(adj, frequency = freq)
fit.stl <- stl(adj.ts[,1], s.window = "period")
autoplot(fit.stl, main="STL Decomposition")

## ----- Split train/test set ----- ##
# num_of_train <- round(0.95 * length(aapl.c)) %>% print()
num_of_train <- (length(aapl.c) - 30) %>% print()
aapl.c.train <- head(aapl.c, num_of_train)
aapl.c.test <- tail(aapl.c, round(length(aapl.c) - num_of_train))
length(aapl.c); length(aapl.c.train); length(aapl.c.test)
plotts(aapl.c.train)

## ----- Data transformation ----- ##
# lambda = BoxCox.lambda(aapl.c) %>% print() #0.1146397
lambda = 0 # log transform
percentage = 1 # change to 1 to remove multiplication
aapl.r <- diff(BoxCox(aapl.c, lambda))*percentage # returns
aapl.r <- aapl.r[!is.na(aapl.r)] # aapl.r <- aapl.r[2:length(aapl.r)]
length(aapl.c); length(aapl.r)
names(aapl.r) <- 'Apple Daily Return (2002-2017)'
# Plot timeplot and ACF PACF Plots of Abs and Sq
plotts(aapl.r)
plotAcf2(aapl.r) # acf cut off lag 4, pacf also lag 4
plotAcf2(abs(aapl.r))
plotAcf2(aapl.r^2)
# ADF test to check stationarity
adf.test(aapl.r) # p-value = 0.01

## ----- Plot QQ Plot ----- ##
dev.off()
qqnorm(aapl.r)
qqline(aapl.r, col = 2)
skewness(aapl.r); kurtosis(aapl.r) 

## ----- EACF ----- ##
eacf(aapl.r)
eacf(abs(aapl.r))
eacf(aapl.r^2)

## ----- Split train/test set ----- ##
# num_of_train <- round(0.95 * length(aapl.r)) %>% print()
num_of_train <- (length(aapl.r) - 30) %>% print()
aapl.r.train <- head(aapl.r, num_of_train)
aapl.r.test <- tail(aapl.r, round(length(aapl.r) - num_of_train))
length(aapl.r); length(aapl.r.train); length(aapl.r.test)
plotts(aapl.r.train)

## ----- GARCH ----- ##
garch.40=garch(aapl.r, order=c(4,0))
# summary(garch.40)
AIC(garch.40) # 16806.18; -17926.01
garch.11=garch(aapl.r, order=c(1,1))
# summary(garch.11)
AIC(garch.11) # 16205.51; -18554.31
garch.12=garch(aapl.r, order=c(1,2))
# summary(garch.12)
AIC(garch.12) # 16593.74; -18502.63
garch.22=garch(aapl.r, order=c(2,2))
# summary(garch.22)
AIC(garch.22) # 16577.53; -18446.87
garch.33=garch(aapl.r, order=c(3,3))
# summary(garch.33)
AIC(garch.33) # 16507.91; -18472.86
garch.21=garch(aapl.r, order=c(2,1))
# summary(garch.21)
AIC(garch.21) # 16201.29; -18549.33
garch.31=garch(aapl.r, order=c(3,1))
# summary(garch.31)
AIC(garch.31) # 16195; -18512.5

## ----- GARCH Diagnostic Checking ----- ##
plot(residuals(garch.11),type='h',ylab='Standardized Residuals', main='GARCH(1,1)')
qqnorm(residuals(garch.11)); qqline(residuals(garch.11), col = 2)
plotAcf2(abs(residuals(garch.11)))
plotAcf2(residuals(garch.11)^2)
gBox(garch.11,method='squared') # above p-value
tsdisplay(residuals(garch.11), lag.max = 40, main="GARCH(1,1)")

plot(residuals(garch.21),type='h',ylab='Standardized Residuals', main='GARCH(2,1)')
qqnorm(residuals(garch.21)); qqline(residuals(garch.21), col = 2)
plotAcf2(abs(residuals(garch.21)))
plotAcf2(residuals(garch.21)^2)
gBox(garch.21,method='squared') # above p-value
tsdisplay(residuals(garch.21), lag.max = 40, main="GARCH(2,1)")

plot(residuals(garch.31),type='h',ylab='Standardized Residuals', main='GARCH(3,1)')
qqnorm(residuals(garch.31)); qqline(residuals(garch.31), col = 2)
plotAcf2(abs(residuals(garch.31)))
plotAcf2(residuals(garch.31)^2)
gBox(garch.31,method='squared') # below p-value
tsdisplay(residuals(garch.31), lag.max = 40, main="GARCH(3,1)")

## ----- AUTO.ARIMA Forecast ----- ##
fit.aa313 <- auto.arima(aapl.c.train, lambda=0, d=1) %>% print() #AIC = -17792.94
tsdiag(fit.aa313)
Box.test(fit.aa313$residuals, type="Ljung-Box")
pred.aa313 <- forecast(fit.aa313, h=length(aapl.c.test))
pred.aa313$mean
accuracy(pred.aa313, aapl.c.test)
autoplot(pred.aa313)

## ----- FGARCH Fitting ----- ##
# std sstd snorm norm
fit.garch11 <- garchFit(formula = ~garch(1, 1), 
                     data = aapl.r.train, trace = F, cond.dist = "std")
Box.test(residuals(fit.garch11), type="Ljung-Box") #0.7899
plot(fit.garch11, which=3)  #Series with 2 Conditional SD Superimposed
plot(fit.garch11, which=13)  #QQ Plot
pred.garch11 <- predict(fit.garch11, n.ahead = length(aapl.r.test), plot=TRUE)
accuracy(pred.garch11$meanForecast, aapl.r.test)  #0.00504715

fit.garch21 <- garchFit(formula = ~garch(2, 1), 
                        data = aapl.r.train, trace = F, cond.dist = "std") 
Box.test(residuals(fit.garch21), type="Ljung-Box") #0.7899
plot(fit.garch21, which=3)  #Series with 2 Conditional SD Superimposed
plot(fit.garch21, which=13)  #QQ Plot
pred.garch21 <- predict(fit.garch21, n.ahead = length(aapl.r.test), plot=TRUE)
accuracy(pred.garch21$meanForecast, aapl.r.test)  #0.005047115


## ----- RUGARCH Fitting ----- ##
library(PerformanceAnalytics)
chart.Histogram(aapl.r, methods = c("add.normal", "add.density"),
                colorset=c("gray","red","blue"))

# Standard GARCH model with std
spec.sGarch11 <- ugarchspec(mean.model=list(armaOrder=c(0,0)),
                            variance.model=list(model="sGARCH", garchOrder=c(1,1)),
                            distribution.model = "std")
fit.sGarch11 <- ugarchfit(spec=spec.sGarch11, data=aapl.r)
fit.sGarch11
stdmsftret <- residuals(fit.sGarch11, standardize = TRUE)
Box.test(abs(stdmsftret), 22, type = "Ljung-Box") #0.173
length(coef(fit.sGarch11)) #6
likelihood(fit.sGarch11)   #9472.777
infocriteria(fit.sGarch11) #-5.015795

# Standard GARCH model with std and variance targeting
spec.sGarch11.vt <- ugarchspec(mean.model=list(armaOrder=c(0,0)),
                               variance.model=list(model="sGARCH", garchOrder=c(1,1), 
                                                   variance.targeting = TRUE),
                               distribution.model = "std")
fit.sGarch11.vt <- ugarchfit(spec=spec.sGarch11.vt, data=aapl.r)
fit.sGarch11.vt
stdmsftret <- residuals(fit.sGarch11.vt, standardize = TRUE)
Box.test(abs(stdmsftret), 22, type = "Ljung-Box") #0.1375
length(coef(fit.sGarch11.vt)) #5
likelihood(fit.sGarch11.vt)   #9471.963
infocriteria(fit.sGarch11.vt) #-5.016139
plot(fit.sGarch11.vt, which="all")

# GJR GARCH model with std
spec.gjrGarch11 <- ugarchspec(mean.model=list(armaOrder=c(0,0)),
                              variance.model=list(model="gjrGARCH", garchOrder=c(1,1)),
                              distribution.model = "std")
fit.gjrGarch11 <- ugarchfit(spec=spec.gjrGarch11, data=aapl.r)
fit.gjrGarch11
stdmsftret <- residuals(fit.gjrGarch11, standardize = TRUE)
Box.test(abs(stdmsftret), 22, type = "Ljung-Box") #0.6314
length(coef(fit.gjrGarch11)) #7
likelihood(fit.gjrGarch11)   #9482.752 (higher better)
infocriteria(fit.gjrGarch11) #-5.020266
plot(fit.gjrGarch11, which="all")

# Family GARCH model with TGARCH submodel with std 
spec.fGarch11 <- ugarchspec(mean.model=list(armaOrder=c(0,0)),
                            variance.model=list(model = "fGARCH", garchOrder=c(1,1),
                                                submodel="TGARCH", variance.targeting=FALSE), 
                            distribution.model = "std")
fit.fGarch11 <- ugarchfit(spec=spec.fGarch11, data=aapl.r)
fit.fGarch11
stdmsftret <- residuals(fit.fGarch11, standardize = TRUE)
Box.test(abs(stdmsftret), 22, type = "Ljung-Box") #0.6208
length(coef(fit.fGarch11)) #7
likelihood(fit.fGarch11)   #9494.399 (higher better)
infocriteria(fit.fGarch11) #-5.026967
plot(fit.fGarch11, which="all")

# Exponential GARCH model with std
spec.eGarch11 <- ugarchspec(mean.model=list(armaOrder=c(0,0)),
                            variance.model=list(model = "eGARCH", garchOrder=c(1,1),
                                                variance.targeting = FALSE), 
                            distribution.model = "std")
fit.eGarch11 <- ugarchfit(spec=spec.eGarch11, data=aapl.r)
fit.eGarch11
stdmsftret <- residuals(fit.eGarch11, standardize = TRUE)
Box.test(abs(stdmsftret), 22, type = "Ljung-Box") #0.644
length(coef(fit.eGarch11)) #6
likelihood(fit.eGarch11)   #9496.096 (higher better)
infocriteria(fit.eGarch11) #-5.027865
plot(fit.eGarch11, which="all")

## ----- RUGARCH Forecast ----- ##
# Best rugarch package GARCH model: Exponential GARCH model with std 
garchspec <- ugarchspec(mean.model=list(armaOrder=c(0,0)),
                        variance.model=list(model = "eGARCH", garchOrder=c(1,1)), 
                        distribution.model = "std")
# start.window = window(aapl.r, start = as.Date("2013-01-31"), end = as.Date("2016-01-31"))

# estimate the model
garchfit <- ugarchfit(data = aapl.r.train, spec = garchspec)
garchfit
likelihood(garchfit)
infocriteria(garchfit)
setfixed(garchspec) <- as.list(coef(garchfit))

# garchfilter <- ugarchfilter(data = aapl.r, spec = garchspec)
# plot(sigma(garchfilter))
# Make the predictions for the mean and vol for the next 30 days
garchforecast <- ugarchforecast(data = aapl.r.train,
                                fitORspec = garchspec,
                                n.ahead = length(aapl.r.test))
garchforecast
plot(garchforecast, which=1)
plot(garchforecast, which=3)
cbind(fitted(garchforecast), sigma(garchforecast))

# N-roll
garchspec <- ugarchspec(mean.model=list(armaOrder=c(0,0)),
                        variance.model=list(model = "eGARCH", garchOrder=c(1,1)), 
                        distribution.model = "std")
fta <- ugarchfit(garchspec, aapl.r, out.sample=length(aapl.r.test))
fwdCast = ugarchforecast(fta, n.ahead=length(aapl.r.test), n.roll=length(aapl.r.test))
fwdCast

plot(fwdCast, which="all")

## ----- RUGARCHROLL PREDICTIONS ----- ##
garchspec <- ugarchspec(mean.model=list(armaOrder=c(0,0)),
                        variance.model=list(model = "eGARCH", garchOrder=c(1,1)), 
                        distribution.model = "std")


garchroll <- ugarchroll(garchspec, data = aapl.r, n.start = num_of_train,
                        refit.window = "moving", refit.every = 2)
plot(garchroll, which="all")
preds <- as.data.frame(garchroll)
length(preds)
preds
e <- preds$Realized - preds$Mu
mean(e^2) 


## ----- RUGARCH Simulations ----- ##
garchspec <- ugarchspec(mean.model=list(armaOrder=c(0,0)),
                        variance.model=list(model = "eGARCH", garchOrder=c(1,1),
                                            variance.targeting = FALSE), 
                        distribution.model = "std")
garchfit <- ugarchfit(data = aapl.r.train, spec = garchspec)
simgarchspec <- garchspec
# Simulate 3 different simulations for future 8 years 
setfixed(simgarchspec) <- as.list(coef(garchfit))
simgarch <- ugarchpath(spec = simgarchspec, m.sim = 3,
                       n.sim = 8 * 252, rseed = 123) 
simret <- fitted(simgarch)
plot.zoo(simret)
plot.zoo(sigma(simgarch))
simprices <- exp(apply(simret, 2, "cumsum"))
matplot(simprices, type = "l", lwd = 3)

