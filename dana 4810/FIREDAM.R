
attach(FIREDAM)
View(FIREDAM)
head(FIREDAM)
plot(x=DISTANCE, y=DAMAGE, ylab = "Sales Revenue ($1000s)", xlab = "Advertising Expenditure ($100s)", 
     main = "Scatterplot of Sales Revenue vs. Advertising Expenditure",
     col="blue", pch=19, , ylim = c(2,55), xlim = c(0,8))

abline(lm(SALES_Y~ADVEXP_X), col="Red", lty=1, lwd=2)


#Regression
model=lm(SALES_Y~ADVEXP_X)
summary(model)
#adding Predicted points
points(x=ADVEXP_X, y= predict(model), col="red",pch=19)
legend("topleft", c("Predicted Sales Revenue","Observed Sales Revenue"), 
       cex = 0.7, fill = c("Red","Blue"))
#Prediction
New=data.frame(ADVEXP_X=c(4,5))
predict(model,New)

##ANOVA Tabel
anova(model)
coef(model)
#CV
s=sqrt(sum(model$residuals^2)/(length(SALES_Y) -2))
cv=(s/mean(SALES_Y))*100

#Rejection Region for the slope
library(mosaic)
xct(0.95,3)

#t-value
qt(0.025,3, lower.tail = FALSE)

#95% confidence intervals for the slope and intercept
round(confint(model,level=0.95),3)

##Cor
cor(x=DISTANCE, y=DAMAGE)
cor.test(x=DISTANCE, y=DAMAGE)
#Rho test
attach(CASINO)
head(CASINO)
cor(x=EMPLOYEES, y=CRIMERAT)
plot(x=EMPLOYEES, y=CRIMERAT, ylab = "Yearly Crime Rate",
     xlab = "Number of Casino Employees (thousands)", 
     main = "Scatterplot of Crime Rate vs. Number of Casino Employees",
     col="blue", ylim = c(1,4.5), xlim = c(15,40), pch=19)


detach(CASINO)

attach(TIRES)
head(TIRES)
plot(x=PRESS_X, y=MILEAGE_Y, ylab = "Mileage (thousands)",
     xlab = "Pressure (pounds per sq. inch)", 
     main = "Scatterplot of Mileage vs. Pressure",
     col="blue", ylim = c(24,40), xlim = c(29,38), pch=19)

cor.test(x=PRESS_X, y=MILEAGE_Y)
detach(TIRES)
##95% CI and PI
model=lm(SALES_Y~ADVEXP_X)
predict(model,newdata=data.frame(ADVEXP_X=4),
        interval="confidence",level=0.95)

predict(model,newdata=data.frame(ADVEXP_X=4),
        interval="prediction",level=0.95)

#95% CI and PI Plot
plot(x=ADVEXP_X, y=SALES_Y, ylab = "Sales Revenue ($1000s)", xlab = "Advertising Expenditure ($100s)",
     main="Regression",col="blue", ylim = c(-2,6), xlim = c(-2,7), pch=19)
model=lm(SALES_Y~ADVEXP_X)
abline(lm(SALES_Y~ADVEXP_X), col="Red", lty=1, lwd=2)

newx <- seq(1, 5, by=0.005)
CI=predict(model,newdata=data.frame(ADVEXP_X=newx),
           interval="confidence",level=0.95)

PI=predict(model,newdata=data.frame(ADVEXP_X=newx),
           interval="prediction",level=0.95)

lines(newx, CI[ ,2], col="green", lty=2)
lines(newx, CI[,3], col="green", lty=2)
lines(newx, PI[,2], col="orange", lty=2)
lines(newx, PI[,3], col="orange", lty=2)
legend("topleft", c("95% confidence interval", "95% prediction interval"), 
       cex = 0.7, fill = c("green", "orange"))

##or using ggplot
library("ggplot2")
pred=predict(model, interval = "prediction")
new_df = cbind(ADSALES, pred)

ggplot(new_df, aes(ADVEXP_X, SALES_Y))+
  geom_point()+
  geom_line(aes(y=lwr), color = "red", linetype = "dashed")+
  geom_line(aes(y=upr), color = "red", linetype = "dashed")+
  geom_smooth(method=lm, se=TRUE)