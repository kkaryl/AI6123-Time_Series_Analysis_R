y = scan("./data/wwwusage.txt", skip = 1)


library(forecast)
fit <- auto.arima(y,max.p = 5,max.q = 5,max.P = 5,max.Q = 5,max.d = 3,seasonal = FALSE,ic = 'aicc')
plot(forecast(fit,h=20))
AIC(fit)
fit

pArr <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
dArr <- c(0, 1, 2, 3)
qArr <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)

fit = arima(x = y, order=c(5,2,5)) 
tsdiag(fit)
BIC(fit)
bestAIC <- AIC(fit)
bestOrder <- ""

for (p in pArr) {
  for (d in dArr){
    for (q in qArr) {
      message(sprintf("Running Order: c(%s,%s,%s)", p, d, q))
      tryCatch({
        fit <-arima(x = y, order=c(p,d,q))
        newAIC = AIC(fit)
        if (newAIC < bestAIC) {
          bestAIC = AIC(fit)
          bestOrder <- sprintf("c(%s,%s,%s)", p, d, q)
          message(sprintf("Best AIC: %s Order: c(%s,%s,%s)", bestAIC, p, d, q))
        }
      },
      error=function(cond) {
        #message(sprintf("Error Order: c(%s,%s,%s) - %s", p, d, q, cond))
      },
      warning=function(cond) {
        #message(sprintf("Warning Order: c(%s,%s,%s) - %s", p, d, q, cond))
      })
    }
  }
}

(sprintf("Best AIC: %s Order: %s", bestAIC, bestOrder))