#reading files with file.choose
library(readr)
read_csv(file.choose())

#invoke packages
library(TSA)
library(tseries)
library(forecast)

# We are using beer for time series analysis.
#beer is an inbuilt R dataset
# another data set data(austres)
beer <- data(beersales)
print(beersales)
plot(beersales)

library(dplyr)
library(lubridate)

# does not work in chr data as seen with deersales
beer %>% group_by(quarter = paste(quarters(date), 
  lubridate::year(date))) %>% summarise(x = mean(x))

floor_date()

# dividing our data into quaters          
beer2 <- window(ausbeer,start=1971,end=2006-.1)

#print out our data from above
print(beer2)

fit <- tslm(beer2~trend+season)
summary(fit)

plot(beer2,xlab="Year",ylab="Megaliters",main="Quaterly Beer Production Actual vs Predicted")

# fit our time series linear predicted model to the actual model or data
lines(fitted(fit),col=2)

# create legends for our figures indicating actual vs predicted
legend("topright",lty=1,col=c(1,2),legend=c("Actual","Predicted"))


## Import a dataset to learn weekly to quaterly time series conversion
library(dplyr)
library(lubridate)
library(tidyverse)
library(zoo)
testdate <- read.csv(file.choose())
testdate$Date <- as.numeric(as.character(testdate$Date))

# Using the tidyverse method

## as.Date is a function and will be used to transform date into
# r friendly format
testdate$Date1 <- as.Date(testdate$Date, format = "%/m%/d/%y")
testdate <- arrange(testdate, Date)
testdate$qdate <- as.yearqtr(testdate$Date)
testdate_qtrly <- testdate %>%
  group_by(qdate) %>%
  summarise_all(mean)

## Using zoo
#round dates down to week
testdate$qdate <- as.yearqtr(testdate$Date,           
                                 format = "%Y-%m-%d")
testdate_qtrly <- testdate %>%
  group_by(qdate) %>%
  summarise_all(mean)


