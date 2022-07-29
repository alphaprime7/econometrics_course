fed = read.csv("fed.csv", head=TRUE)

#To name the file "fed.csv" as fed in order to facilitate typing code
summary(fed)

# To ensure the integrity of data by summarizing all the data
plot(fed$elec_rep, fed$FEDFUNDS, main="FEDFUNDS and elec_rep", xlab="elec_rep", ylab="FEDFUNDS")

#scatterplot of FEDFUNDS on y-axis versus elec_dem on x-axis
OLS = lm(fed$FEDFUNDS ~ fed$elec_dem + fed$elec_rep)

#Run a linear model of FEDFUNDS to elec_dem and elec_rep
summary(OLS)

OLS = lm(fed$FEDFUNDS ~ fed$elec_dem)
# Create a linear model of FEDFUNDS versus elec_dem

summary(OLS)
#summarize OLS results

abline(OLS)
#Create a fitted line for elec_dem OLS results 

plot(fed$elec_rep, fed$FEDFUNDS, main="FEDFUNDS and elec_rep", xlab="elec_rep", ylab="FEDFUNDS")
#Scatterplot of FEDFUNDS vs elec_rep

OLS = lm(fed$FEDFUNDS ~ fed$elec_rep)
# Create a linear model of FEDFUNDS versus elec_dem

summary(OLS)
#summarize OLS results

abline(OLS)
#Create a fitted line for elec_rep OLS results 

OLSMV = lm(fed$FEDFUNDS ~ fed$inflation + fed$lag_FEDFUNDS + fed$elec_dem + fed$elec_rep)
#We run a multivariate OLS model with 4 independent variables indicated above vs FEDFUNDS

summary(OLSMV)
#summarize OLSMV 

library (AER)
#We call for AER library in order to run robust regression


coeftest(OLSMV, vcov=vcovHC(OLSMV, type="HC1"))
#We run robust regression to find Hetertoscedastic errors


