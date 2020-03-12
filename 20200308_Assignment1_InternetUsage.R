# The wwwusage  time series data consist of  the number of users connected to the internet through a server.
# 
# The data are collected at a time interval of one minute and there are 100 observations.
# 
# Please fit an appropriate ARIMA model for it and submit a short report 
# including R codes, the fitted model, the diagnostic checking, AIC, etc.

# Load data 
y = scan("./data/wwwusage.txt", skip = 1)
min(y) # find min y
max(y) # find max y
mean(y) 
plot(1:100, y, xlim=c(0,100),ylim=c(50,250), 
     main="Number of users connected to internet over Time", 
     xlab="Time (minute)", ylab="Number of users connected")
lines(1:100,y,type="l") # plot line
acf(y, lag.max = 40)  # does not cut off until lag 32 (dies down quickly)
pacf(y) # cut off after lag 2

# Apply one time differencing (z1)
z1=diff(y)
length(z1)
min(z1)
max(z1)
plot(1:99,z1,xlim=c(0,100),ylim=c(-15,15), main="Time Plot after one-time differencing") # Plot time plot after differencing
lines(1:99,z1,type="l")
acf(z1, lag.max = 40)  # does not cut off until lag  24 (dies down quickly)
pacf(z1) # cut off after lag 3

# Yule Walker to estimate AR coefficient (z1)
ts.yw <- ar.yw(z1, order.max = 5)
ts.yw
summary(ts.yw) # also suggested 3

# Try arima(3, 1, 0) from yule walker est on differenced data
fit310 = arima(x = y, order=c(3,1,0)) 
fit310
tsdiag(fit310)
AIC(fit310) # 511.994
BIC(fit310) # 522.3745

# Forecast for arima(3, 1, 0)
plot(1:100, y, xlim=c(0,120), ylim=c(-100,500), main="Fitted and Forecast Values for ARIMA(3, 1, 0)",
     xlab="Time (minute)", ylab="Number of users connected")
lines(1:100,y,type="l")
lines(fitted(fit310),col="blue")
forecast310 = predict(fit310, n.ahead=20)
lines(101:120, forecast310$pred, type="o", col="red")
lines(101:120, forecast310$pred-1.96*forecast310$se, col="blue") #lower limit of confidence interval
lines(101:120, forecast310$pred+1.96*forecast310$se, col="blue") #upper limit of confidence interval

# Try arima(1, 1, 1) suggested by auto.arima
fitauto <- auto.arima(y,max.p = 5,max.q = 5,max.P = 5,max.Q = 5,max.d = 3,seasonal = FALSE,ic = 'aicc')
fitauto
fit111 = arima(x = y, order=c(1,1,1)) 
fit111
tsdiag(fit111)
AIC(fit111) # 514.2995
BIC(fit111) # 522.0848 (lower BIC than arima(3,1,0))

plot(1:100, y, xlim=c(0,120), ylim=c(-100,500), main="Fitted and Forecast Values for ARIMA(1, 1, 1)",
     xlab="Time (minute)", ylab="Number of users connected")
lines(1:100,y,type="l")
lines(fitted(fit111),col="blue")
forecast111 = predict(fit111, n.ahead=20)
lines(101:120, forecast111$pred, type="o", col="red")
lines(101:120, forecast111$pred-1.96*forecast111$se, col="blue") #lower limit of confidence interval
lines(101:120, forecast111$pred+1.96*forecast111$se, col="blue") #upper limit of confidence interval

# Apply two time differencing
z2=diff(z1)
length(z2)
min(z2)
max(z2)
plot(1:98,z2,xlim=c(0,100),ylim=c(-10,10), main="Time Plot after two-time differencing") # Plot time plot after differencing
lines(1:98,z2,type="l")
acf(z2, lag.max = 30)  # does not cut off until lag  27 
pacf(z2) # cut off after lag 2

# Yule Walker to estimate AR coefficient (z2)
ts.yw <- ar.yw(z2, order.max = 5)
ts.yw
summary(ts.yw) # also suggested 2

# Try arima(2, 2, 0) based on Yule Walker est on z2
fit220 = arima(x = y, order=c(2,2,0)) 
fit220
tsdiag(fit220)
AIC(fit220) # 511.4645 (lower than arima(1,1,1))
BIC(fit220) # 519.2194 (lower than arima(1,1,1))

plot(1:100, y, xlim=c(0,120), ylim=c(-100,500), main="Fitted and Forecast Values for ARIMA(2, 2, 0)",
     xlab="Time (minute)", ylab="Number of users connected")
lines(1:100,y,type="l")
lines(fitted(fit220),col="blue")
forecast220 = predict(fit220, n.ahead=20)
lines(101:120, forecast220$pred, type="o", col="red")
lines(101:120, forecast220$pred-1.96*forecast220$se, col="blue") #lower limit of confidence interval
lines(101:120, forecast220$pred+1.96*forecast220$se, col="blue") #upper limit of confidence interval

# Try arima(5, 2, 5) with lowest AIC via brute force testing
fit525 = arima(x = y, order=c(5,2,5)) 
fit525
tsdiag(fit525)
AIC(fit525) # 509.8135 (lowest AIC)
BIC(fit525) # 538.2481

plot(1:100, y, xlim=c(0,120), ylim=c(-100,500), main="Fitted and Forecast Values for ARIMA(5, 2, 5)",
     xlab="Time (minute)", ylab="Number of users connected")
lines(1:100,y,type="l")
lines(fitted(fit525),col="blue")
forecast525 = predict(fit525, n.ahead=20)
lines(101:120, forecast525$pred, type="o", col="red")
lines(101:120, forecast525$pred-1.96*forecast525$se, col="blue") #lower limit of confidence interval
lines(101:120, forecast525$pred+1.96*forecast525$se, col="blue") #upper limit of confidence interval

plot(forecast(fit310,h=20), ylim=c(-100,500))
plot(forecast(fit111,h=20), ylim=c(-100,500))
plot(forecast(fit220,h=20), ylim=c(-100,500))
plot(forecast(fit525,h=20), ylim=c(-100,500))

acctest <- window(y, start=91, end=100)
accuracy(forecast(fit310), acctest)
accuracy(forecast(fit111), acctest)
accuracy(forecast(fit220), acctest)
accuracy(forecast(fit525), acctest)

library(forecast)
checkresiduals(fit310)
checkresiduals(fit111)
checkresiduals(fit220)
checkresiduals(fit525)

library(sarima)
sfit310 <- sarima(y, p = 3, d = 1, q = 0) #4.676866
sfit310$ttable
sfit111 <- sarima(y, p = 1, d = 1, q = 1) #4.664191
sfit111$ttable
sfit220 <- sarima(y, p = 2, d = 2, q = 0) #4.844786
sfit220$ttable
sfit525 <- sarima(y, p = 5, d = 2, q = 5) #4.628987
sfit525$ttable

