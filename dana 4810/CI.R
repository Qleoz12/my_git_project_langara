 ATTENTIMES <- read.csv("H:/DANA/Ch1/ATTENTIMES.csv", sep="") 
View(ATTENTIMES)
attach(ATTENTIMES)
head(ATTENTIMES)
summary(ATTENTIMES)

t.test(ï..AttentionTime, conf.level = 0.99)


norm.interval = function(ï..AttentionTime, variance = var(ï..AttentionTime), conf.level = 0.99)
  {z = qnorm((1 - conf.level)/2, lower.tail = FALSE)
   xbar = mean(ï..AttentionTime)
   sdx = sqrt(variance/length(ï..AttentionTime))
   c(xbar - z * sdx, xbar + z * sdx)}

norm.interval(ï..AttentionTime)

######
SILICA<- read.csv("H:/DANA/Ch1/SILICA.csv", sep="") 
View(SILICA)
attach(SILICA)
head(SILICA)
summary(SILICA)
t.test(SiliconDioxide, conf.level = .95)
sd(SiliconDioxide)