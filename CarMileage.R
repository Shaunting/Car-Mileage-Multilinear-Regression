
library("readxl")
data<- read_excel("C:/Users/shaun/OneDrive/Documents/Toledo/Classes/Stats 2/carmileage.xlsx")
attach(data)

# Remove frist column
cardata=data[,-1]
dim(cardata)

#Preliminary analysis 
#1.Examine each of the variables

# Entire Dataset
summary(cardata)
sapply(cardata,sd)
sapply(cardata,hist)
# MPG
hist(MPG)
boxplot(MPG)
# HP
hist(HP)
boxplot(HP)
# VOL
hist(VOL)
boxplot(VOL)
#SP
hist(SP)
boxplot(SP)
#WT
hist(WT)
boxplot(WT)

#Examine the relationships between pairs of variables
pairs(cardata)
cor(cardata)
cor.test(VOL,MPG)

# Finding the best model 
model1=lm(MPG~HP+VOL+SP+WT,data=cardata)
summary(model1) # VOL is not significant 

model2=lm(MPG~HP+SP+WT,data=cardata)
summary(model2) # Increased Adjusted R-Squared

#Examine Residuals:
#Plot the residuals vs. the predicted values y_hat to see if there is evidence of a curved (rather than linear)relation
plot(fitted(model2),residuals(model2))
abline(0,0)

#Plot the residuals vs. each of the explanatory variables to check if the variances of the residuals are constant

plot(HP,residuals(model2),xlab="HP",ylab="Residuals")
abline(0,0)
plot(SP,residuals(model2),xlab="SP",ylab="Residuals")
abline(0,0)
plot(WT,residuals(model2),xlab="WT",ylab="Residuals")
abline(0,0)

#nomal quantile plot for residuals to check normality of residuals
qqnorm(residuals(model2),xlab="Normal Score", ylab="Residuals")
qqline(residuals(model2))


anova(model2)

model2=lm(MPG~HP+SP+WT,data=cardata)
summary(model2) # Increased Adjusted R-Squared

beta1 <- coef(model2)[2]
se_beta1 <- summary(model2)$coef[2, 2]

beta2 <- coef(model2)[3]
se_beta2 <- summary(model2)$coef[3, 2]

