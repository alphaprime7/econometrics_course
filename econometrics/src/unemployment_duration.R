# Data in this analysis is obtained from the FRED addin in microsoft excel
# Normally we will import using the read.csv

ud <- read.csv (file = 'fredud.csv', header = T)

ols = lm(ud$pud ~ ud$pu3c + ud$bur + ud$wur + ud$pgdp + ud$plpr + ud$pcpi)
## We run a multi-variate regression model with 6 independent variables

summary(ols)
## We summarize the ols model

coeftest(ols, vcov=vcovHC (ols, type="HC1"))
## Robust regression looking for heteroscedastic errors





