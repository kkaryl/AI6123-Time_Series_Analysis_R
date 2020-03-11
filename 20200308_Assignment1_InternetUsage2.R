y = scan("./data/wwwusage.txt", skip = 1)

# Use auto.arima to find a suitable model
library(forecast)
fit <- auto.arima(y,max.p = 5,max.q = 5,max.P = 5,max.Q = 5,max.d = 3,seasonal = FALSE,ic = 'aicc')
plot(forecast(fit,h=20))
AIC(fit)
fit

# Use brute force to obtain min AIC and BIC models
pArr <- seq(0 , 9, by = 1)
dArr <- seq(0 , 3, by = 1) # at most 3 times differencing
qArr <- seq(0 , 9, by = 1)
corder <- NA
aic <- NA
bic <- NA
i = 1

for (p in pArr) {
  for (d in dArr){
    for (q in qArr) {
      aicc <- NA
      bicc <- NA
      message(sprintf("Trying order: c(%s)", paste(p,d,q, sep=",")))
      tryCatch({
        fit <-arima(x = y, order=c(p,d,q))
        aicc <- AIC(fit)
        bicc <- BIC(fit)
      },
      error=function(cond) {
        #message(sprintf("Error Order: c(%s,%s,%s) - %s", p, d, q, cond))
      },
      warning=function(cond) {
        #message(sprintf("Warning Order: c(%s,%s,%s) - %s", p, d, q, cond))
      },
      finally={
        corder[i] <- sprintf("c(%d,%d,%d)", p,d,q)
        aic[i] <- aicc
        bic[i] <- bicc
        i = i + 1
      })
    }
  }
}
aicInd <- which(aic == min(aic, na.rm = TRUE))
bicInd <- which(bic == min(bic, na.rm = TRUE))
message(sprintf("Best AIC: %s Order: %s", aic[aicInd], corder[aicInd]))
message(sprintf("Best BIC: %s Order: %s", bic[bicInd], corder[bicInd]))