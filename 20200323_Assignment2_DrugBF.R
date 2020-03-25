df_data = read.delim("./data/drug.txt", header=TRUE, sep = ",")
global.freq <<- 12
ts_all <- ts(df_data$value, start=c(1991, 7), end=c(2008, 6), frequency=global.freq)
num_of_train <- round(0.7 * length(ts_all))
ts_train <- head(ts_all, num_of_train)
ts_val <- tail(ts_all, round(length(ts_all) - num_of_train))

# Use auto.arima to find a suitable model
library(forecast)
# fit <- auto.arima(ts_train, max.p=15, max.q=1,
#                   max.P=15, max.Q=15, max.d=15, max.D=3, allowdrift = TRUE,
#                   stepwise=FALSE, approximation=FALSE, seasonal = TRUE, ic = 'aicc')
# # fit <- auto.arima(y,max.p = 5,max.q = 5,max.P = 5,max.Q = 5,max.d = 3,seasonal = FALSE,ic = 'aicc')
# plot(forecast(fit,h=20))
# AIC(fit)
# fit

# Use brute force to obtain min AIC and BIC models
pArr <- seq(0 , 5, by = 1)
dArr <- seq(0 , 2, by = 1) # at most 3 times differencing
qArr <- seq(0 , 5, by = 1)
PArr <- seq(0 , 5, by = 1)
DArr <- seq(0 , 2, by = 1) # at most 3 times differencing
QArr <- seq(0 , 5, by = 1)
corder <- NA
Corder <- NA
aic <- NA
bic <- NA
rmse <- NA
i = 1

# library(foreach)
# library(doParallel)
# 
# #setup parallel backend to use many processors
# cores=detectCores()
# cl <- makeCluster(cores[1]-1) #not to overload your computer
# registerDoParallel(cl)

#start time
strt<-Sys.time()

for (p in pArr) {
  for (d in dArr) {
    for (q in qArr) {
      for (P in PArr) {
        for (D in DArr) {
          for (Q in QArr) {
            message(sprintf("Trying order: c(%s), C(%s)", paste(p,d,q, sep=","), paste(P,D,Q, sep=",")))
            aicc <- NA
            bicc <- NA
            rmsee <- NA
            tryCatch({
              fit <-arima(ts_train, order=c(p,d,q), seasonal=list(order=c(P,D,Q),period=12))
              pred <- exp(forecast(fit, h = length(ts_val)+20)$mean)
              aicc <- AIC(fit)
              bicc <- BIC(fit)
              rmsee <- accuracy(pred, ts_all)[2]
            },
            error=function(cond) {
              #message(sprintf("Error Order: c(%s,%s,%s) - %s", p, d, q, cond))
            },
            warning=function(cond) {
              #message(sprintf("Warning Order: c(%s,%s,%s) - %s", p, d, q, cond))
            },
            finally={
              corder[i] <- sprintf("c(%d,%d,%d) C(%d,%d,%d)", p,d,q, P,D,Q)
              aic[i] <- aicc
              bic[i] <- bicc
              rmse[i] <- rmsee
              i = i + 1
            })
          }
        }
      }
    }
  }
}
aicInd <- which(aic == min(aic, na.rm = TRUE))
bicInd <- which(bic == min(bic, na.rm = TRUE))
rsmeInd <- which(rsmeInd == min(rsmeInd, na.rm = TRUE))
message(sprintf("Best AIC: %s Order: %s", aic[aicInd], corder[aicInd]))
message(sprintf("Best BIC: %s Order: %s", bic[bicInd], corder[bicInd]))
message(sprintf("Best BIC: %s Order: %s", rsmeInd[bicInd], corder[bicInd]))

#stop cluster
print(Sys.time()-strt)
# stopCluster(cl)
