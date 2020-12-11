
library(tseries)

x = rnorm(1000)

plot.ts(x)
acf(x) #look like white noise
adf.test(x) # 


y = diffinv(x)
plot(y, type="l") # original timeplot is non-stationary

adf.test(y) # p = 0.9 > 0.05 fail to reject null hypothesis (stationary)

x = rnorm(1000)
kpss.test(x) # p = 0.1 > 0.05 fail to reject null hypothesis (level stationary data)
kpss.test(x) # p = 0.1 > 0.05 fail to reject null hypothesis (truly stationary data) # both trend and level stationary
adf.test(x)   

z <- cumsum(x)
#?cumsum
plot(z, type="l")

kpss.test(z) # p = 0.01 < 0.05 reject null hypothesis (not stationary)
kpss.test(z, "Trend")

a <- 0.3 * (1:1000)+rnorm(1000)
plot(a, type="l")
kpss.test(a, null="Trend") # p > 0.05 fail to reject null hypothesis (trend stationary)
