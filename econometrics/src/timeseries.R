## Install necessary libraries and packages here. Get "dynlm" and "forecast".

#INFLATION DATA# <- read.csv(file=#YOUR FILE#)
#UNEMPLOYMENT DATA# <- read.csv(file=#YOUR FILE#)

#YOUR TIME SERIES# <- 4*ts(#INFLATION DATA#, start=c(#START#), frequency=#QUARTERLY#)
#YOUR TIME SERIES# <- ts(#UNEMPLOYMENT DATA#, start=c(#START#), frequency=#QUARTERLY#)

philips.fit <- dynlm(#DEPENDENT VARIABLE~ #INDEPENDENT VARIABLES, LAGS#)
summary(philips.fit)
  
forecast <- forecast(philips.fit$fitted,5)  ## 5 PERIOD AHEAD FORECAST
plot(forecast,xlim=c(2013,2018),	    ## GRAPH STARTS 2013 TO 2018
       ylim=c(-4,12),
       main="#TITLE#", 
       xlab="#X LABEL#",
       ylab="#Y LABEL#",
       col="red")
lines(#YOUR TIME SERIES#)

plot(#YOUR TIME SERIES#, xlim=c(2013,2018),		## LIMITS THE X AXIS TO BE BETWEEN 2013 AND 2018
      ylim=c(-4,12),					## LIMITS THE Y AXIS TO BE BETWEEN -4 AND 12
      main="Philips Curve Estimate",
      xlab="Time",
      ylab="Inflation, Actual and Predicted")
    lines(philips.fit$fitted, col="red")
    
    
plot(cpi.ts, philips.fit$fitted, main="Actual vs. Predicted Values",
         xlab="Actual Inflation",
         ylab="Predicted Inflation",
         pch=19)
    
plot(#YOUR UNEMPLOYMENT TIME SERIES#,diff(#YOUR INFLATION TIME SERIES#,lag=1), main="The Philips Curve and NAIRU",
      xlab="Unemployment U3",
      ylab="Inflation, CPI",
      pch=19)
    