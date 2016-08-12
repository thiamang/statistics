# Install Applied Linear Regresssion Package
install.packages("alr3", dependencies = TRUE)
install.packages("ggplot2", dependencies = TRUE)
install.packages("pastecs", dependencies = TRUE)
install.packages("psych", dependencies = TRUE)
library (alr3)
library(pastecs)
data() # See datasets contained in package

# CHAPTER 3. Multiple Regression
head(UN2)  #Take a peak at data
pairs(~UN2$logPPgdp + UN2$logFertility + UN2$Purban, data=UN2, main="UN2 Scatter plot")  # multiple scatter plot

cor(UN2) #Correlations to give some perspective to the picture above

summary(lm(UN2$logFertility ~ UN2$Purban))

data()
head(fuel2001)
par(mfrow=c(1,2))
hist(fuel2001$Drivers);qqnorm(fuel2001$Drivers);qqline(fuel2001$Drivers) #Not normal
hist(fuel2001$FuelC);qqnorm(fuel2001$FuelC);qqline(fuel2001$FuelC) # Not Normal
hist(fuel2001$Income);qqnorm(fuel2001$Income);qqline(fuel2001$Income) # Closer to Normal
hist(log(fuel2001$Miles));qqnorm(log(fuel2001$Miles));qqline(log(fuel2001$Miles))
hist(fuel2001$MPC);qqnorm(fuel2001$MPC);qqline(fuel2001$MPC)
hist(fuel2001$Pop);qqnorm(fuel2001$Pop);qqline(fuel2001$Pop)
hist(fuel2001$Tax);qqnorm(fuel2001$Tax);qqline(fuel2001$Tax)
summary(fuel2001)
sapply(fuel2001, mean, na.rm=TRUE)
stat.desc(fuel2001)
cor(fuel2001) # Correlation
pairs (~fuel2001$Drivers+fuel2001$FuelC+fuel2001$Income+fuel2001$Miles+fuel2001$MPC+fuel2001$Tax, data = fuel2001)
