
GASTURBINE <- read.delim("~/Fall 2024/DANA4810/Chapter 4/Data/GASTURBINE.txt")
View(GASTURBINE)
attach(GASTURBINE)
head(GASTURBINE)
pairs(~HEATRATE+RPM+CPRATIO+INLET.TEMP+EXH.TEMP+AIRFLOW,data = GASTURBINE, 
      main="Scatterplot Matrix of Gas Turbines")
model41=lm(HEATRATE~RPM+CPRATIO+INLET.TEMP+EXH.TEMP+AIRFLOW,data = GASTURBINE)
summary(model41)
anova(model41)


#Prediction
New=data.frame(RPM=7500, CPRATIO=13.5, INLET.TEMP=1000, EXH.TEMP=525,AIRFLOW=10)
predict(model41,New)

New=data.frame(RPM=c(7500,27245), CPRATIO=c(13.5,9.2), INLET.TEMP=c(1000,1134), EXH.TEMP=c(525,602),AIRFLOW=c(10,7))
predict(model41,New)


##95% CI and PI
predict(model41,New, interval="confidence",level=0.95)

predict(model41,New, interval="prediction",level=0.95)



EXPRESS <- read.delim("~/Fall 2024/DANA4810/Chapter 4/Data/EXPRESS.txt")
View(EXPRESS)
attach(EXPRESS)
head(EXPRESS)
pairs(~Cost+Weight+Distance,data = EXPRESS, 
      main=" Matrix Scatterplot")
model=lm(Cost~Weight+Distance+ I(Weight^2)+I(Distance^2)+Distance*Weight, data = EXPRESS)
summary(model)
#anova(model)

#Prediction
New=data.frame(Weight=c(5), Distance=(100))
predict(model,New)

##95% CI and PI
predict(model,New, interval="confidence",level=0.95)

predict(model,New, interval="prediction",level=0.95)







# Activity
GFCLOCKS <- read.delim("~/Fall 2024/DANA4810/Chapter 4/Data/GFCLOCKS.txt")
View(GFCLOCKS)
attach(GFCLOCKS)
head(GFCLOCKS)
pairs(~PRICE+AGE+NUMBIDS,data = GFCLOCKS, 
      main="Scatterplot Matrix of grandfather clocks")
# stronger linear relationship with age

model41a=lm(PRICE~AGE+NUMBIDS,data = GFCLOCKS)
summary(model41a)

#mean auction price of an antique clock increase $12.74 for every 1-year increase in age when the number of bidders is held fixed.
#an age of 0 years and 0 bidders on the clock—is not practical

anova(model41a)

#c#SEE: 516727
#d#Root MSE: standard error: sqrt(516727/29)

#we expect the model to provide predictions of auction price to within about ±2s = ±2(133.5) = ±267 dollars.

#e#F-statistic: 120.2 on 2 and 29 DF,  p-value: 9.216e-15
#at least one of the model coefficients is nonzero. The overall model appears to be statistically useful for predicting auction prices.

#f#Ha: β2 > 0


#85.9530/8.7285
# p-vale/2 for one side


#g
confint(model41a,level=0.95)


New=data.frame(AGE=150, NUMBIDS=10)
predict(model41a,New)

predict(model41a,New, interval="confidence",level=0.95)
# we are 95% confident that the mean auction price for all 150-year-old clocks sold at an auction with 10 bidders lies between $1,381.40 and $1,481.90.

predict(model41a,New, interval="prediction",level=0.95)

#We say, with 95% confidence, that the auction price for a single 150-year-old clock sold at an auction with 10 bidders falls between $1,154.10 and $1,709.30.

summary(GFCLOCKS)
#both selected values fall well outside their respective ranges


head(GFCLOCKS)
model41a2=lm(PRICE~AGE+NUMBIDS+AGE.BID,data = GFCLOCKS)
summary(model41a2)

#F-statistic:   193 on 3 and 28 DF,  p-value: < 2.2e-16

#Ha: β3 > 0
#p-value/2

#(150*1.2978)-93.2648
#we estimate that the auction price of a 150-year-old clock will increase by about $101.74 for every additional bidder.
#Once interaction is detected, the higher order is important (but keep the terms)


Complete sample

FLAG <- read.delim("~/Spring 2025/DANA4810_230_A322_TU/Chapter 4/Data/FLAG.txt")
View(FLAG)

#Complete Example
attach(FLAG)
head(FLAG)
model=lm(COST~DOTEST+I(DOTEST^2)+STATUS+STATUS*DOTEST+STATUS*I(DOTEST^2))
summary(model)
#CV
s=sqrt(sum(model$residuals^2)/((length(COST) - (dim(model.matrix(model))[2]))))
s
cv=(s/mean(COST))*100
cv
#Reduced Model
modelReduced=lm(COST~DOTEST+STATUS+STATUS*DOTEST)
summary(modelReduced)
anova(modelReduced, model)
##Plot
plot(x=DOTEST, y=COST, ylab = "Contract Cost ($1000s)", xlab = "Estimate of the cost of DOT ($1000s)", 
     main = "Scatterplot of Contract Cost vs. DOT engineer’s estimate of the cost"
     ,ylim = c(0,15000), xlim = c(0,12000), pch=19, col=ifelse(FLAG$STATUS=="1", "black", "pink"))
legend("topleft", legend = c("Status: Fixed","Status: Competitive"), 
       col = c("black", "pink"), pch =19:19, cex = 0.8)

Competitive=subset(FLAG, STATUS=="0")
Fixed=subset(FLAG, STATUS=="1")

#abline(lm(Fixed$COST~Fixed$DOTEST), col="Red", lty=1, lwd=2) 
#abline(lm(Competitive$COST~Competitive$DOTEST), col="Green", lty=1, lwd=2) 




plot(x=DOTEST, y=COST, ylab = "Contract Cost ($1000s)", xlab = "Estimate of the cost of DOT ($1000s)", 
     main = "Scatterplot of Contract Cost vs. DOT engineer’s estimate of the cost"
     ,ylim = c(0,15000), xlim = c(0,12000), pch=19, col="blue")

abline(lm(Fixed$COST~Fixed$DOTEST), col="Red", lty=1, lwd=2) 
abline(lm(Competitive$COST~Competitive$DOTEST), col="Green", lty=1, lwd=2) 
legend("topleft", legend = c("Least Squares Line for Fixed","Least Squares Line for Competitive"), 
       col = c("red", "Green"), lty=1:1, cex = 0.8)
M1=lm(Fixed$COST~Fixed$DOTEST)
summary(M1)
M2=lm(Competitive$COST~Competitive$DOTEST)
summary(M2)

###s For Reduced model
s1=sqrt(sum(modelReduced$residuals^2)/((length(COST) - (dim(model.matrix(modelReduced))[2]))))
s1
cv1=(s1/mean(COST))*100
cv1
##95% PI
New=data.frame(DOTEST=c(1386.29), STATUS=c(1))
predict(modelReduced, newdata=New, interval="prediction", level=0.95)






















