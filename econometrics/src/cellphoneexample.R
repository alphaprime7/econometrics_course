#Read the data from the downloaded file into the R program as a set 
#named "celldeath".

celldeath = read.csv(file="U:\\ECON 6320 Fall 2016\\Cellphone_2012_homework.csv", head=TRUE)

#Summarize the data for cell phone subscriptions.

summary(celldeath$numberofdeaths)
summary(celldeath$cell_subscription)

#Check all the data.

summary(celldeath)

#Make a scatter plot with deaths on the Y axis and subscriptions on the X axis.

plot(celldeath$cell_subscription, celldeath$numberofdeaths, main="Cell Phones Kill", xlab="Murder Weapon", ylab="Bodies")

#Run a robust regression on cell phones and traffic fatalities.

library(AER)
ols=lm(celldeath$numberofdeaths ~ celldeath$cell_subscription)
summary(ols)
coeftest(ols, vcov=vcovHC(ols, type="HC1"))

#Add in population data.

ols2=lm(celldeath$numberofdeaths ~ celldeath$cell_subscription + celldeath$population)
summary(ols2)
coeftest(ols2, vcov=vcovHC(ols2, type="HC1"))

#Add in total miles driven.

ols3=lm(celldeath$numberofdeaths ~ celldeath$cell_subscription + celldeath$population + celldeath$total_miles_driven)
summary(ols3)
coeftest(ols3, vcov=vcovHC(ols3, type="HC1"))

#Assess the multicollinearity.

AuxReg1 = lm (celldeath$total_miles_driven ~ celldeath$population + celldeath$cell_subscription)
AuxReg2 = lm (celldeath$population ~ celldeath$cell_subscription + celldeath$total_miles_driven)
