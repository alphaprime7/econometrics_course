#this is to read the fed.csv file
fed = read.csv("fed.csv", head=TRUE)

#this is to name the file fed so that we do not need to use the csv extension when giving commands
plot(fed$elec_dem, fed$FEDFUNDS, main="FEDFUNDS vs Elec_dem", 
     xlab="elec_dem", ylab="FEDFUNDS")

#this is plot of FEDFUNDS(Y-axis) to Elec_dem(X-axis) as demanded in question 1
OLS = lm(fed$FEDFUNDS ~ fed$elec_dem)

#this is to run a linear model
summary(OLS)
abline(OLS)

#this is to draw a fitted line for the data plot or OLS
plot(fed$elec_rep, fed$FEDFUNDS, main="FEDFUNDS vs Elec_rep", xlab="elec_rep", ylab="FEDFUNDS")
OLS = lm(fed$FEDFUNDS ~ fed$elec_rep)
summary(OLS)
abline(OLS)

# biregression (2 x vars) binary model
OLS = lm(fed$FEDFUNDS ~ fed$elec_dem + fed$elec_rep)
summary(OLS)

# a multiregression model (ran this in excel as well)
AuxReg2 = lm(fed$FEDFUNDS ~ fed$inflation + fed$lag_FEDFUNDS + fed$elec_dem + fed$elec_rep)
summary(AuxReg2)

# a multiregression model (ran this in excel as well) 
AuxReg3 = lm(fed$inflation  ~ fed$lag_FEDFUNDS + fed$elec_dem + fed$elec_rep)
summary(AuxReg3)

# a multiregression model (ran this in excel as well) 
AuxReg4 = lm(fed$lag_FEDFUNDS   ~ fed$inflation + fed$elec_dem + fed$elec_rep)
summary(AuxReg4)

# Our results indicated that inflation and lag-fedfunds were statitically
# significant meaning not by chance
# Indicates that lag_fedfunds(trailing behind value) and inflation
# have an effect on current fed funds
  
    
    
    
    