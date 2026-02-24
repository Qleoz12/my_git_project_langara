DIETS <- read.csv("D:/Nooshin's/Langara/DANA 4810/Chapter 1/Data/DIETS.csv")
View(DIETS)
attach(DIETS)
head(DIETS)
summary(DIETS)

#CI_E1.16
t.test(WTLOSS~DIET,conf.level=0.95)



#Summary Statistics
library(mosaic)
favstats(WTLOSS~DIET)

#Test_Independent_Samples

t.test(WTLOSS~DIET,conf.level=0.95,alternative = c("greater"))
#Pooled_Test

attach(READING)
head(READING)
summary(READING)
t.test(SCORE~METHOD, conf.level=0.95, alternative = c("two.sided"), var.equal=TRUE)
boxplot(SCORE~METHOD)
favstats(SCORE~METHOD) 

#Paired_Test
attach(PAIRED)
summary(PAIRED)
head(PAIRED)
t.test(NEW, STANDARD, conf.level=0.95, alternative = c("greater"), paired=TRUE)
#F_Test
favstats(SCORE~METHOD) 
var.test(SCORE[METHOD=="STD"],SCORE[METHOD=="NEW"], alternative = "two.sided")
