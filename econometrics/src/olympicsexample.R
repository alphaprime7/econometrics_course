#Read the data from the downloaded file into the R program as a set 
#named "olympics".

olympics = read.csv(file="U:\\ECON 6320 Fall 2016\\olympics_HW.csv, head=TRUE)

#Check the data for GDP and medals to see if it passes the "smell test".

summary(olympics_HW$GDP)
summary(olympics_HW$Medals)

#Check all the data.

summary(olympics_HW)

#Make a scatter plot with Medals on the Y axis and GDP on the X axis.

plot(olympics_HW$GDP, olympics_HW$medals, main="Medals and GDP", xlab="GDP", ylab="Medals")

