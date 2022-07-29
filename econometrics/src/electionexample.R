 ## Install the "AER" package; one of the money open source resources
## to have a "robust" estimation for heterscedasticity.

install.packages("AER")

## Read the CSV file downloaded from the Bailey Real Econometrics Student
## Companion Site. 

pres <- read.csv(file="U:\\ECON 6320 Fall 2016\\PresVote.csv")

## Summary of the data, just to check.

summary(pres)

## Graph the data, just to check.

plot(pres$vote, pres$rdi4, xlab="Change in Income", ylab="Voter Share for Incumbent Party")

## Load the AER package files into your workstation.

library(AER)

## Run the standard OLS linear regression for 
## Vote_i = \beta_0 + \beta_1 * rdi4 + \epsilon_i 
## Store results into object named "ols".

ols = lm(pres$vote ~ pres$rdi4)

## Check the results using the summary command.

summary(ols)

## Run the robust regression using the "coeftest" command in the AER package.

coeftest(ols, vcov=vcovHC(ols, type="HC1"))

## How are the t-statistics and p-values different? 