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
acf(y)  # does not cut off (dies down quickly)
pacf(y) # cut off after lag 2

# Apply one time differencing
z=diff(y) 
length(z)
min(z) # find min z
max(z) # find min z
plot(1:99,z,xlim=c(0,100),ylim=c(-15,15)) # Plot time plot after differencing
lines(1:99,z,type="l")
acf(z)  # does not cut off (dies down quickly)
pacf(z) # cut off after lag 3

ts.yw <- ar.yw(z, order.max = 5)
ts.yw
summary(ts.yw)

# Try AR(3, 1) - best
fit = arima(x = y, order=c(3,1,0)) 
fit
tsdiag(fit)
AIC(fit)
BIC(fit)

plot(1:100, y, xlim=c(0,100), ylim=c(50,250), main="Number of users connected to internet over Time",
     xlab="Time (minute)", ylab="Number of users connected")
lines(1:100,y,type="l")
lines(fitted(fit),col="blue")

fit3 = arima(x = y, order=c(1,1,1)) 
fit3
tsdiag(fit3)
AIC(fit3)
BIC(fit3)

fit2 = arima(x = z, order=c(3,0,0)) 
fit2
tsdiag(fit2)
AIC(fit2)
BIC(fit2)

lines(fitted(fit),col="blue")

plot (1:100, y, xlim=c(0,100), ylim=c(50,250))
lines(1:100, y, type="l")
lines(fitted(fit),col="red")
#lines(1:100, y - fit$residuals, type="l", col="red") #yhat = y-fit$residuals (e)

# Test two times differencing
h=diff(z)
length(h)
min(h) # find min h
max(h) # find min h
plot(1:98,h,xlim=c(0,100),ylim=c(-10,10)) # Plot time plot after differencing
lines(1:98,h,type="l")
acf(h)
pacf(h)
autofit = auto.arima(h, max.p = 5,max.q = 5,max.P = 5,max.Q = 5,max.d = 3,seasonal = FALSE,ic = 'aicc')
autofit
fit4 = arima(x = y, order=c(5,2,5)) 
fit4
tsdiag(fit4)


