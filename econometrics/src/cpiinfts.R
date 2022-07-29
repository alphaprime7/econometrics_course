u3 = read.csv ("pu3.csv", head=TRUE)
cpi = read.csv ("pcpi.csv", head = TRUE)

mets <- 4*ts(cpi, start=c(1950,1), frequency=4)
## mets stands for me time series
mets2 <- ts(u3, start=c(1950,1), frequency=4)
## mets2 stands for me time series 2

philips.fit <- dynlm((mets) ~ L(mets, c(1,2,3,4)) + L(mets2, c(1,2,3,4)), start=c(1950,1), end=c(2016,3))
summary(philips.fit)

forecast <- forecast(philips.fit$fitted,5)  
## 5 PERIOD AHEAD FORECAST

plot(forecast,xlim=c(2013,2018),ylim=c(-4,12), main="Mets", xlab="time", ylab="cpi", col="red")
## GRAPH STARTS 2013 TO 2018
lines(mets)
## create a line of original mets time series to compare time series to prediction

plot(mets, xlim=c(2013,2018), ylim=c(-4,12), main="Philips Curve Estimate", xlab="Time", ylab="Inflation, Actual and Predicted")
## LIMITS THE X AXIS TO BE BETWEEN 2013 AND 2018
lines(philips.fit$fitted, col="red")
## create a line of the philips.fit to compare actual vs predicted inflation

plot(jitter(mets), jitter(philips.fit$fitted), main="Actual vs. Predicted Values", xlab="Actual Inflation", ylab="Predicted Inflation", pch=19)
## Scatter plot of predicted vs actual inflation values

plot(mets2,diff(mets,lag=1), main="The Philips Curve and NAIRU", xlab="Unemployment U3", ylab="Inflation, CPI", pch=19)


#EXTENSION CODE
philips1.fit <- dynlm((mets) ~ L(mets, c(1,2)) + L(mets2, c(1)), start=c(1950,1), end=c(2016,3))
summary(philips1.fit)

forecast2 <- forecast(philips1.fit$fitted,5)  
## 5 PERIOD AHEAD FORECAST

plot(forecast2,xlim=c(2013,2018),ylim=c(-4,12), main="Mets", xlab="time", ylab="cpi", col="red")
## GRAPH STARTS 2013 TO 2018
lines(mets)
## create a line of original mets time series to compare time series to prediction

plot(mets, xlim=c(2013,2018), ylim=c(-4,12), main="Philips Curve Estimate", xlab="Time", ylab="Inflation, Actual and Predicted")
## LIMITS THE X AXIS TO BE BETWEEN 2013 AND 2018
lines(philips1.fit$fitted, col="red")
## create a line of the philips.fit to compare actual vs predicted inflation

plot(jitter(mets), jitter(philips1.fit$fitted), main="Actual vs. Predicted Values", xlab="Actual Inflation", ylab="Predicted Inflation", pch=19)
## Scatter plot of predicted vs actual inflation values

plot(mets2,diff(mets,lag=1), main="The Philips Curve and NAIRU", xlab="Unemployment U3", ylab="Inflation, CPI", pch=19)

