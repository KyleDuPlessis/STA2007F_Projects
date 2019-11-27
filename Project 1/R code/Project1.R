# STA2007F Project 1: Regression
# 21/03/2019
# Group 6:
# - dplkyl002
# - hrdmal001
# - bcktan001

setwd("C:/Users/999999/Desktop/Stats Project")

install.packages("visreg", dep=T)
install.packages ("car", dep=T)
require(visreg)
require(car)

gpp<-read.csv("GPP.csv", header = T) # read in data

# EXPLORATORY DATA ANALYSIS

names(gpp)
head(gpp)
dim(gpp)
str(gpp)
summary(gpp)

# the gross primary productivity (GPP) [grams of carbon / m^2 / year] is the reponse variable we are interested in
# looking at its distribution, and checking for errors or outliers
# it is nearly a symmetrical distribution already, so the response does not require a log transformation
summary(gpp$GPP)
density.hist <- hist(gpp$GPP, xlab="Gross primary productivity (GPP) [grams of carbon / m^2 / year]", ylab="Frequency", main="Frequency distribution of gross primary productivity (GPP)")

# create a pairs plot
pairs(~GPP + rainfall + summer.temp + livestock.units + parcel.size + soil.nutrients + seasonality + land.use, data=gpp)

# FITTING THE MODELS

# MULTICOLLINEARITY
# t1 <- lm(gpp$GPP ~ rainfall + summer.temp + livestock.units + parcel.size + soil.nutrients + seasonality + land.use, data=gpp)
# vif(t1)
# livestock.units 11.090290
# land.use        11.417444
# livestock.units and land.use are correlated (variance inflation factors (vif) above 4 or 5)

# NO MULTICOLLINEARITY
# t2 <- lm(GPP ~ soil.nutrients + rainfall + seasonality + livestock.units, data=gpp)
# vif(t2)
# soil.nutrients  rainfall    seasonality   livestock.units 
# 1.411833        1.411885    1.047019      1.041133

# FOR MODELS m1 AND m2 BELOW:
# - summer.temp and parcel.size not included since there's no clear pattern in the data and this
#   cannot be captured appropriately by neither a linear nor non-linear model
# - land.use not included since it is correlated with livestock.units
# - only livestock.units is included since it is more directly related and meaningful for modelling the response
# - there is no multicollinearity (no inflated standard errors and consequently no large p-values)
#   even though variance inflation factors (vif) for the explanatory variables are now larger than 4 or 5
# - high variance inflation factors (vif) doesn't matter in this case, since in the termites.R example code
#   after adding the interaction term the vif is also well above 4 or 5
#   termite.lm4 <- lm(density ~ height*map + soil, data=termites)
#   vif(termite.lm4)
#                 GVIF Df GVIF^(1/(2*Df))
#height     486.228578  1       22.050591
#map         42.165773  1        6.493518
#soil         3.494321  4        1.169285
#height:map 402.633740  1       20.065735

m1 <- lm(GPP ~ soil.nutrients + rainfall + livestock.units + I(livestock.units^2) + seasonality, data=gpp)
vif(m1)
summary(m1)

m2 <- lm(GPP ~ soil.nutrients * rainfall + livestock.units + I(livestock.units^2) + seasonality, data=gpp)
vif(m2)
summary(m2)

confint(m2)

###########################
#### MODEL CHECKING (m1)
########################### 

###########################Linearity
# Partial relationships ("holding everything else constant") between the response and the explanatory variables
par(mfrow=c(1,2))
cex.lab <- 2.5; cex.axis <- 1.8
par(mar=c(5,8,1,0))

visreg(m1, "soil.nutrients", line=list(col="blue"), points=list(cex=1, pch=16, col=1), ylab="", cex.lab=cex.lab, cex.axis=cex.axis)
mtext("GPP", side=2, line=6, cex=cex.lab)
par(mar=c(5,3,1,5))
visreg(m1, "rainfall", line=list(col="blue"), points=list(cex=1, pch=16, col=1), yaxt="n", ylab="", cex.lab=cex.lab, cex.axis=cex.axis)

par(mfrow=c(1,2))
cex.lab <- 2.5; cex.axis <- 1.8
par(mar=c(5,8,1,0))

visreg(m1, "livestock.units", line=list(col="blue"), points=list(cex=1, pch=16, col=1), ylab="", cex.lab=cex.lab, cex.axis=cex.axis)
mtext("GPP", side=2, line=6, cex=cex.lab)
par(mar=c(5,3,1,5))
visreg(m1, "seasonality", line=list(col="blue"), points=list(cex=1, pch=16, col=1), yaxt="n", ylab="", cex.lab=cex.lab, cex.axis=cex.axis)

########################### Constant error variance
# Residual plot: fitted vs residuals
par(mfrow=c(1,1))
cex.lab <- 2.5; cex.axis <- 1.8
par(mar=c(5,5,1,2))
plot(fitted(m1), rstandard(m1), ylab="Standardised residuals", xlab="Fitted values", cex.lab=cex.lab, cex.axis=cex.axis)
abline(h=0, col=2)

########################### Independence
# Residuals plotted in order of observation
par(mfrow=c(1,2))
cex.lab <- 1.8; cex.axis <- 1.5; pch <- 16; lwd <- 2
par(mar=c(5,5,1,2))
plot(rstandard(m1), type='b', xlab="Order of observations", ylab="Standardised residuals", cex.lab=cex.lab, cex.axis=cex.axis, lwd=lwd)
abline(h=0, col=2, lwd=lwd)

acf(rstandard(m1))

require(lmtest)
dwtest(m1, alternative="two.sided")
# The null hypothesis is that autocorrelation is 0

########################### Normality
par(mfrow=c(1,2))
cex.lab <- 2; cex.axis <- 1.5
par(mar=c(5,5,1,2))
hist(rstandard(m1), main='', col="grey70", xlab="Standardised residuals", cex.lab=cex.lab, cex.axis=cex.axis)

require(car)
cex.lab <- 2; cex.axis <- 1.5
par(mar=c(5,5,1,2), cex.lab=cex.lab, cex.axis=cex.axis)
qqPlot(m1, dist="t", ylab="Model residuals", xlab="Standard Normal residuals", pch=16)

shapiro.test(rstandard(m1))
# Shapiro-Wilk normality test
# The null hypothesis is that the sample distribution is normal
# No sample will be perfectly normal
# One does not have to prove that residuals are normal, only check that they do not
# seriously violate this assumption
# This assumption is approximatey met

par(mfrow=c(2,2))
plot(m1)



###########################
#### MODEL CHECKING (m2)
###########################

########################### Linearity
# Partial relationships ("holding everything else constant") between the response and the explanatory variables
par(mfrow=c(1,1))
visreg(m2, "soil.nutrients", by = "rainfall", breaks=3, overlay=TRUE)

par(mfrow=c(1,2))
cex.lab <- 2.5; cex.axis <- 1.8
par(mar=c(5,8,1,0))

visreg(m2, "livestock.units", line=list(col="blue"), points=list(cex=1, pch=16, col=1), ylab="", cex.lab=cex.lab, cex.axis=cex.axis)
mtext("GPP", side=2, line=6, cex=cex.lab)
par(mar=c(5,3,1,5))
visreg(m2, "seasonality", line=list(col="blue"), points=list(cex=1, pch=16, col=1), yaxt="n", ylab="", cex.lab=cex.lab, cex.axis=cex.axis)

########################### Constant error variance
# Residual plot: fitted vs residuals
par(mfrow=c(1,1))
cex.lab <- 2.5; cex.axis <- 1.8
par(mar=c(5,5,1,2))
plot(fitted(m2), rstandard(m2), ylab="Standardised residuals", xlab="Fitted values", cex.lab=cex.lab, cex.axis=cex.axis)
abline(h=0, col=2)

########################### Independence
# Residuals plotted in order of observation
par(mfrow=c(1,2))
cex.lab <- 1.8; cex.axis <- 1.5; pch <- 16; lwd <- 2
par(mar=c(5,5,1,2))
plot(rstandard(m2), type='b', xlab="Order of observations", ylab="Standardised residuals", cex.lab=cex.lab, cex.axis=cex.axis, lwd=lwd)
abline(h=0, col=2, lwd=lwd)

acf(rstandard(m2))

require(lmtest)
dwtest(m2, alternative="two.sided")
# The null hypothesis is that autocorrelation is 0

########################### Normality
par(mfrow=c(1,2))
cex.lab <- 2; cex.axis <- 1.5
par(mar=c(5,5,1,2))
hist(rstandard(m2), main='', col="grey70", xlab="Standardised residuals", cex.lab=cex.lab, cex.axis=cex.axis)

require(car)
cex.lab <- 2; cex.axis <- 1.5
par(mar=c(5,5,1,2), cex.lab=cex.lab, cex.axis=cex.axis)
qqPlot(m2, dist="t", ylab="Model residuals", xlab="Standard Normal residuals", pch=16)

shapiro.test(rstandard(m2))
# Shapiro-Wilk normality test
# The null hypothesis is that the sample distribution is normal
# No sample will be perfectly normal
# One does not have to prove that residuals are normal, only check that they do not
# seriously violate this assumption
# This assumption is approximatey met

par(mfrow=c(2,2))
plot(m2)


######################
#### MODEL SELECTION
######################

# AIC
aics <- AIC(m1,m2) # AIC
delta.aics <- aics$AIC - min(aics$AIC) # Delta AIC
wi <- exp(-0.5*delta.aics)/sum(exp(-0.5*delta.aics)) # Akaike weights

logliks <- c(logLik(m1),logLik(m2)) # log Likelihoods
models <- c("m1", "m2") # model names

(modtable <- data.frame(models, -2*logliks, numpar=aics$df, AIC=aics$AIC, delta.aics,wi))
write.csv(modtable, "modtable.csv")

# anova

#t <- lm(GPP ~ soil.nutrients * rainfall + livestock.units + I(livestock.units^2) + seasonality, data=gpp)
t0 <- lm(GPP ~ 1, data=gpp)
t1 <- lm(GPP ~ soil.nutrients * rainfall, data=gpp)
t2 <- lm(GPP ~ soil.nutrients * rainfall + livestock.units, data=gpp)
t3 <- lm(GPP ~ soil.nutrients * rainfall + livestock.units + I(livestock.units^2), data=gpp)
t4 <- lm(GPP ~ soil.nutrients * rainfall + livestock.units + I(livestock.units^2) + seasonality, data=gpp)

anova(t0, t1, t2, t3, t4)
