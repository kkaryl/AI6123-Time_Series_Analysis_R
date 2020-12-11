setwd("~/Git/AI6123-Time-Series-Analysis-R")
library(TSA)

load("data/CREF.rda")
plot.ts(CREF)
# plot shows that the returns were more volatile over some time periods and became very volatile
# toward the end of the study period

r.cref=diff(log(CREF))*100 # *100 to reduce numerical errors and show as percentage changes
plot(r.cref); abline(h=0)

par(mfrow=c(1,2))
acf(r.cref) #little serial correlation
pacf(r.cref)

## abs and square acf and pacf shows
## significant autocorrelations
## and hence provide some evidence that the daily CREF returns are
## not independently and identically distributed
par(mfrow=c(1,2))
acf(abs(r.cref)) 
pacf(abs(r.cref))
acf(r.cref^2)
pacf(r.cref^2)

## Plot QQ plot: to see the distribution scores
# The QQ plot suggests that the distribution
# of returns may have a tail thicker than that of a normal distribution and may be
# somewhat skewed to the right. This is referred to be heavy-tailed distribution.
dev.off()
qqnorm(r.cref)
qqline(r.cref)

# The thickness of the tail of a distribution relative to that of a normal distribution
# measured by the (excess) kurtosis
# positive kurtosis is called a heavy-tailed distribution, whereas it
# is called light-tailed if its kurtosis is negative
kurtosis(r.cref)

# In summary, the CREF return data are found to be serially uncorrelated but
# admit a higher-order dependence structure, namely volatility clustering, and a
# heavy-tailed distribution. It is commonly observed that such characteristics are
# rather prevalent among financial time series data.
eacf(abs(r.cref))

library(tseries)
g11=garch(r.cref,order=c(1,1))
summary(g11)
AIC(g11) #better

g22=garch(r.cref,order=c(2,2))
summary(g22)
AIC(g22)  

par(mfrow=c(1,1))
plot(residuals(g11),type='h',ylab='Standardized Residuals')
#win.graph(width=2.5,height=2.5,pointsize=8)
qqnorm(residuals(g11)); qqline(residuals(g11))
acf(residuals(g11)^2,na.action=na.omit)
gBox(g11,method='squared')

# loglik03=logLik(arch03)
# summary(arch03)