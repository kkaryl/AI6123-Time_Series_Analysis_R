x = c(344.39, 246.63, 131.53, 288.87, 313.45, 189.76, 179.1, 221.1, 246.84, 209, 51.21, 133.89,
      277.01, 197.98, 50.68, 218.08, 365.1, 207.51, 54.63, 214.09, 267, 230.28, 230.32, 426.41,
      467.06, 306.03, 253.23, 279.46, 336.56, 196.67, 152.15, 319.67, 440, 315.04, 216.42,
      339.78, 434.66, 399.66, 330.8, 539.78)
plot.ts(x)
length(x)
lines(1:40, x, xlim=(0,40), ylim=(50,550))
min(x)
max(x)

plot(1:40, x, xlim=c(0,40), ylim=c(50,550))
lines(1:40, x, type="l")
acf(x)

dx = diff(x, lag=4)
dx
length(dx) #36 observations left
min(dx)
max(dx)
plot(1:36, dx, xlim=c(0,36), ylim=c(-145,230))
lines(1:36, dx, type="l")

## Stationary test
library(tseries)
adf.test(dx)  #pvalue = 0.01 < 0.05 (reject null hypo) = stationary
adf.test(x)   #nonstationary

# D = 1, d = 0
# s = 4
acf(dx) # only look at ACF 4, 8, 12, 16: \
# spike at lag 4 suggest Q likely=1, P = 0, ignore spike at lag 5 because it mimics lag 1
pacf(dx) # spike at lag 4, suggest P = 1, Q = 0

# for small p and q, only look at first four 
# ACF: cut off at lag 1: q = 1, p = 0 # either AR or MA
# PACF: cut off at lag 1: p = 1, q = 0

# possible models:
# p = 0, q = 1, D = 1, Q = 1, s = 4
fita1 = arima(x, order=c(0,0,1), 
              seasonal=list(order=c(0,1,1),period=4))
tsdiag(fita1)

# p = 1, q = 0, D = 1, P = 1, s = 4
fita3 = arima(x, order=c(1,0,0), 
              seasonal=list(order=c(1,1,0),period=4))
tsdiag(fita3) # pvalue too low (not adequate)

fita4 = arima(x, order=c(1,0,0), 
              seasonal=list(order=c(0,1,1),period=4))
tsdiag(fita4) # pvalue too low (not adequate)

fita5 = arima(x, order=c(0,0,1), 
              seasonal=list(order=c(1,1,0),period=4))
tsdiag(fita5) 
aic(fita5)
aic(fita1)

auto.arima(x, seasonal = T)

# p = 0, q = 2, D = 1, Q = 1, s= 4
fita2 = arima(x, order=c(0,0,2), 
              seasonal=list(order=c(0,1,1),period=4))
tsdiag(fita2)



fita6 = arima(x, order=c(0,0,5),seasonal=list(order=c(0,1,0), period=4))
tsdiag(fita6)

              