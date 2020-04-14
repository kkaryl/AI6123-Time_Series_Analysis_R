library(zoo)
library(xts)

aapl.p<-read.csv('AAPL.csv')
aapl.p$Date <- as.Date(aapl.p$Date, format = '%Y-%m-%d')
aapl.p <- data.frame(aapl.p[,c(1,5)])
aapl.p <- zoo(aapl.p[,2], as.Date(aapl.p[,1],format="%m/%d/%Y"))
aapl.p <- xts(aapl.p)

aapl.r <- diff(log(aapl.p))
aapl.r <- aapl.r[2:length(aapl.r)]
names(aapl.r) <- 'Apple Daily Return'

pArr <- seq(0 , 15, by = 1)
qArr <- seq(0 , 15, by = 1)
corder <- NA
aic <- NA
# bic <- NA
i = 1

for (p in pArr) {
  # for (d in dArr){
    for (q in qArr) {
      aicc <- NA
      # bicc <- NA
      message(sprintf("Trying order: c(%s)", paste(p,q, sep=",")))
      tryCatch({
        fit <- garch(aapl.r, order=c(p,q))
        aicc <- AIC(fit)
        # bicc <- BIC(fit)
      },
      error=function(cond) {
        #message(sprintf("Error Order: c(%s,%s,%s) - %s", p, d, q, cond))
      },
      warning=function(cond) {
        #message(sprintf("Warning Order: c(%s,%s,%s) - %s", p, d, q, cond))
      },
      finally={
        corder[i] <- sprintf("c(%d,%d)", p,q)
        aic[i] <- aicc
        # bic[i] <- bicc
        i = i + 1
      })
    }
  # }
}
aicInd <- which(aic == min(aic, na.rm = TRUE))
# bicInd <- which(bic == min(bic, na.rm = TRUE))
message(sprintf("Best AIC: %s Order: %s", aic[aicInd], corder[aicInd]))
# message(sprintf("Best BIC: %s Order: %s", bic[bicInd], corder[bicInd]))

# garch.22=garch(aapl.r, order=c(2,2))
# summary(garch.22)
# AIC(garch.22) #-18446.87
# plot(residuals(garch.22),type='h',ylab='Standardized Residuals')
# plotAcf2(abs(residuals(garch.22)))
# plotAcf2(residuals(garch.22)^2)
# gBox(garch.22,method='squared') #nope