OLS = lm(fed$FEDFUNDS ~ fed$elec_dem)
## run a univariate OLS model of FEDFUNDS vs elec_dem
summary(OLS)
## summarize the OLS model ran above

OLS1 = lm(fed$FEDFUNDS ~ fed$elec_rep)
## run a bivariate OLS model of FEDFUNDS vs elec_rep
summary(OLS1)

library(AER)
##we load the AER package

coeftest(OLS, vcov=vcovHC (OLSMV, type="HC1"))
##we run a robust regression to test for heteroscedastic errors

OLSE = lm(FEDFUNDS ~ inflation + lag_FEDFUNDS + elec_dem + DM4, data=fed)
##we run a multivariate OLS model controlling for 4 variables
summary(OLSE)
##we summarize the OLSE model

coeftest(OLSE, vcov=vcovHC (OLSE, type="HC1"))
##we run a robust regression to find heteroscedastic errors

OLSE1 = lm(FEDFUNDS ~ inflation + lag_FEDFUNDS + elec_dem + lag_DM4, data=fed)
##we run a model controlling for lag_DM4 alongside 3 other variables
summary(OLSE1)
##we summarize our OLSE1 model




