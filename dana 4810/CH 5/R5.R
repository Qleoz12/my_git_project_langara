######Parabola

par(mfrow=c(3,2))
#beta1
eq=function(x){1-5*x+x^2}
curve(eq,from=-20, to=20, xlab="x", ylab="y", ylim=c(-5,100), xlim = c(-20,10))
title(main="1-5*x+x^2")
eq=function(x){1+5*x+x^2}
curve(eq,from=-20, to=10, xlab="x", ylab="y", ylim=c(-5,100), xlim = c(-20,10))
title(main="1+5*x+x^2")
#beta2
eq=function(x){x+10*x^2}
curve(eq,from=-10, to=10, xlab="x", ylab="y", ylim=c(0,100), xlim = c(-10,10))
title(main="x+10*x^2")
eq=function(x){x+x^2}
curve(eq,from=-10, to=10, xlab="x", ylab="y", ylim=c(0,100), xlim = c(-10,10))
title(main="x+x^2")
#-Beta2
eq=function(x){x+10*x^2}
curve(eq,from=-10, to=10, xlab="x", ylab="y", ylim=c(0,100), xlim = c(-10,10))
title(main="x+10*x^2")
eq=function(x){x-10*x^2}
curve(eq,from=-10, to=10, xlab="x", ylab="y", ylim=c(-100,10), xlim = c(-10,10))
title(main="x-10*x^2")
#######third order
par(mfrow=c(1,2))
#beta3
eq=function(x){1+x+x^2+x^3}
curve(eq, xlab="x", ylab="y", ylim=c(-5,5), xlim = c(-5,5))
title(main="1+x+x^2+x^3")
eq=function(x){1+x+x^2-x^3}
curve(eq, xlab="x", ylab="y", ylim=c(-5,5), xlim = c(-5,5))
title(main="1+x+x^2-x^3")

####
GASTURBINE <- read.delim("GASTURBINE.txt")
View(GASTURBINE)

attach(GASTURBINE)
head(GASTURBINE)
plot( x= AIRFLOW,y= HEATRATE, main="HEAT RATE vs. AIR FLOW", pch=19, col="blue",
      ylim = c(8000, 20000), xlim =c(0, 800), xlab = "Air flow (kilograms per second)", 
      ylab = " Heat rate (kilojoules per kilowatt per hour)" )

model=lm(HEATRATE~AIRFLOW+I(AIRFLOW^2)+I(AIRFLOW^3),data = GASTURBINE)
summary(model)

#####5.19
model519=lm(HEATRATE~RPM+CPRATIO+RPM*CPRATIO+I(RPM^2)+I(CPRATIO^2))
summary(model519)

###RPM is Constant

new1=data.frame(RPM=c(5000), CPRATIO=GASTURBINE$CPRATIO)
y=predict(model519, newdata=new1)
plot(x=CPRATIO,y=y, main = "Scatterplot of predicted Heat Rate and CPRATIO, RPM=5000", 
     ylab = "Predicted Heat Rate", xlab ="CPRATIO", 
     ylim = c(8000, 15000), xlim=c(0,40),pch=19, col="blue")
new2=data.frame(RPM=c(15000), CPRATIO=GASTURBINE$CPRATIO)
y=predict(model519, newdata=new2)
plot(x=CPRATIO,y=y, main = "Scatterplot of predicted Heat Rate and CPRATIO, RPM=15000", 
     ylab = "Predicted Heat Rate", xlab ="CPRATIO", 
     ylim = c(8000, 15000), xlim=c(0,40),pch=19, col="blue")


modelni=lm(HEATRATE~RPM+CPRATIO+I(RPM^2)+I(CPRATIO^2))
summary(modelni)

anova(model519,modelni)


DIESEL <- read.delim("~/Spring 2025/DANA4810_230_A322_TU/Ch5/Data/DIESEL.txt")
View(DIESEL)

##DIESEL
attach(DIESEL)
head(DIESEL)
table(FUEL)
table(BRAND)
x1=ifelse(FUEL=="F2", 1, 0)
x2=ifelse(FUEL=="F3", 1, 0)
x3=ifelse(BRAND=="B2", 1, 0)
##creating a data frame
DIESELDUMMY=data.frame(DIESEL,PERFORM=DIESEL$PERFORM, x1, x2,x3)
modelDUMMY1=lm(PERFORM~x1+x2+x3)
summary(modelDUMMY1)
predict(modelDUMMY1,DIESELDUMMY, interval="confidence",level=0.95)




predict(modelDUMMY1,DIESELDUMMY, interval="prediction",level=0.95)
#%95CI for F3 B2 for Main Model
New=data.frame(x1=c(0), x2=c(1), x3=c(1))
predict(modelDUMMY1,New, interval="confidence",level=0.95)


######Interaction
modelDUMMY2=lm(PERFORM~x1+x2+x3+x1*x3+x2*x3)
summary(modelDUMMY2)

######Interaction no x1x3
modelDUMMY3=lm(PERFORM~x1+x2+x3+x2*x3)
summary(modelDUMMY3)

#%95CI for F3 B2 for Complete Model
New=data.frame(x1=c(0), x2=c(1), x3=c(1))
predict(modelDUMMY2,New, interval="confidence",level=0.95)


##Table for a subset of the data set
Table_F3_B2=subset(DIESEL, FUEL=="F3"&BRAND=="B2")
Table_F3_B2
ybar32=mean(Table_F3_B2$PERFORM)
ybar32


###Graph of sample means for engine performance
library(ggplot2)

ggplot(DIESEL,aes(x=factor(FUEL),y=PERFORM,colour=BRAND,group=BRAND))+
   geom_point()+
   stat_summary(aes(y=PERFORM,group=BRAND),fun= mean,geom = "line")+
   ggtitle("RStudio graph of sample means for engine performance")


##Partial F Test

modelDUMMY1=lm(PERFORM~x1+x2+x3)

modelDUMMY2=lm(PERFORM~x1+x2+x3+x1*x3+x2*x3)
anova(modelDUMMY1, modelDUMMY2)