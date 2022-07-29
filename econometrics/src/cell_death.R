cell = read.csv("cell.csv", head=TRUE)
#This is to name the file cell and read the file cell

summary(cell)
#summarize the data cell in order to check integrity

POLS = lm(DeathsPerBillionMiles ~ cell_ban + text_ban, data=cell)
#run a pooled OLS model of DeathsPerBillion vs cell_ban & text_ban
summary(POLS)
#summarize POLS

library(AER)
#we call for the package AER

coeftest(POLS, vcov=vcovHC(POLS, type="HC1"))
#We run a robust regression to find heteroscedastic errors

pan1 = plm(DeathsPerBillionMiles ~ cell_ban + text_ban, data=cell, index=c("state_numeric","year"), model="within")
#We run a one-way fixed model with the state_numeric and year

summary(pan1)
#we summarize the panel model 2 and 253 DF, p-value: 8.0611e-09

pan2 = plm(DeathsPerBillionMiles ~ cell_ban + text_ban, data=cell, index=c("state_numeric","year"), model="random")
#we run a panel random effects model

summary(pan2)
#we summarize the random effects model

phtest(pan1,pan2)
#We run this test to determine which model (fixed or random) is bias

pandm = plm(DeathsPerBillionMiles ~ cell_ban + text_ban, data=cell, index=c("state_numeric","year"), model="within", effect="twoways")
summary(pandm)


panf = plm(DeathsPerBillionMiles ~ cell_ban + text_ban + cell_per10thous_pop + urban_percent, data=cell, 
index=c("state_numeric","year"), model="within", effect="twoways")
##Estimation of a two-way fixed effects model while controlling for cell_per10thous_pop and urban_percent

summary(panf)
##summarize estimated model

pang = plm(DeathsPerBillionMiles ~ cell_ban + text_ban + cell_per10thous_pop + urban_percent
 + factor(state_numeric), data=cell, index=c("year"), model="within")
##I ran an LSDV two-way model with dummy variable "state_numeric" as a function of time
summary(pang)
##summary of LSDV model

OLSE = lm(FEDFUNDS ~ inflation + lag_FEDFUNDS + elec_dem + elec_rep + lag_DM4, data=fed)
##We run a multi-variate OLS model controlling for 5 variables including lag_DM4
summary(OLSE)
##We summarize the model

coeftest(OLSE, vcov=vcovHC(OLSE, type="HC1"))
##we run a robust regression on OLSE to look for HC1 type errors

OLS = lm(FEDFUNDS ~ inflation + lag_FEDFUNDS + elec_dem + elec_rep, data=fed)
##We run a multi-variate OLS model controlling for 4 variables 
summary(OLS)
##We summarize the model





