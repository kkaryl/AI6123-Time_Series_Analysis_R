set.seed(1235678); 
library(TSA)

garch01.sim=garch.sim(alpha=c(.01,.9),n=500)
plot(garch01.sim,type='l',ylab=expression(r[t]), xlab='t')
# Volatility clustering is evident in
# the data as larger fluctuations cluster together, although the series is able to
# recover from large fluctuations quickly because of the very short memory in the
# conditional variance process.

set.seed(1234567);
library(TSA) #need to reimport for seed to take effect
garch11.sim=garch.sim(alpha=c(0.02,0.05),beta=.9,n=500)
plot(garch11.sim,type='l',ylab=expression(r[t]), xlab='t')

par(mfrow=c(1,2))
acf(garch11.sim)
pacf(garch11.sim)
acf(garch11.sim^2)
pacf(garch11.sim^2)

eacf(garch11.sim^2)
eacf(abs(garch11.sim))

library(tseries)
g2=garch(garch11.sim,order=c(1,1))
summary(g2)
AIC(g2)



