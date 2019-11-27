# STA2007F Project 2: ED
# 30/04/2019
# Group 6:
# - dplkyl002
# - hrdmal001
# - bcktan001

setwd("C:/Users/999999/Desktop/ED Project")

#install.packages("visreg", dep=T)
#install.packages ("car", dep=T)
#require(visreg)
#require(car)

# randomisation in R
all.treats <- rep(c("0","2","8","16","24"), 12)

rand <- sample(all.treats)

rand

# actual code
leaf <- read.csv("LeafData.csv")

head(leaf); tail(leaf)
summary(leaf)
names(leaf)

boxplot(SLA..cm2.g. ~ Treatment..mM.NO3., data=leaf, ylim=c(100,500), range=0,axes=F,ylab="SLA [cm2/g]")
axis(1,at=1:5,labels=sort(unique(leaf$Treatment..mM.NO3.)))
axis(2,at=seq(100,500,50), las=1)
stripchart(SLA..cm2.g. ~ Treatment..mM.NO3., data=leaf, add=TRUE, vertical=TRUE, method="jitter", jitter=.1)

sort(tapply(leaf$SLA..cm2.g., leaf$Treatment..mM.NO3., sd))
43.02643/18.66019

dotchart(leaf$SLA..cm2.g.,ylab="Order of observation", xlab="SLA [cm2/g]")

# analysis of variance
m1<-aov(SLA..cm2.g. ~ Treatment..mM.NO3.,data=leaf)
summary(m1)

hist(resid(m1))

# plotting the marginal means
lsmeans<-predict(m1, newdata=data.frame(Treatment..mM.NO3. = c(0, 2, 8, 16, 24)), se=TRUE)
lsmeans$fit  # means
lsmeans$se.fit  # standard errors

plot(1:5, lsmeans$fit, pch=19, ylim=c(150,400), xlim=c(0,24), axes=F, xlab="nitrogen-adjusted nutrient solution [mM NO3]", ylab="SLA [cm^2/g]")
for (i in 1:5) segments(i,lsmeans$fit[i]-lsmeans$se[i],i,lsmeans$fit[i]+lsmeans$se[i])
axis(1, at=c(1:5), labels=c("0", "2", "8", "16", "24"))
axis(2, las=1)

# fitting the ANOVA model with 'treatment contrast' parameterisation

# had to modify the leaf data file by appending "mM NO3" to each of the treatment names
# this is done so that R reads it in as a string and not a numeric value 
# so that the code below works as expected
# the data in "LeafDataTreatsModify.csv" is still identical to "LeafData.csv"
leafNew <- read.csv("LeafDataTreatsModify.csv")

head(leafNew); tail(leafNew)
summary(leafNew)
names(leafNew)

# 'treatment contrast' parameterisation
m1.tc <- lm(SLA..cm2.g. ~ Treatment..mM.NO3., data=leafNew)
summary(m1.tc)

# Tukey's method
m1<-aov(SLA..cm2.g. ~ Treatment..mM.NO3., data = leafNew)
pht <- TukeyHSD(m1, ordered = T)
pht
plot(pht, las=1)

# power analysis
m1<-lm(SLA..cm2.g. ~ Treatment..mM.NO3., data = leafNew)
summary(m1)$sigma

power.t.test(delta=25, sd=32, power=0.8, sig.level=0.05)
power.t.test(n=60, delta=25, sd=32, sig.level=0.05)
