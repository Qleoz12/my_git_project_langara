
#1. Emprical rules does not apply


mean=30
sd=4
# 2a. Calculate the probability that a randomly chosen product weights less than 27.6g
pnorm(27.6, mean, sd)

prob_less_than_27.6 <- pnorm(27.6, mean, sd)
prob_less_than_27.6
cat("Probability that a randomly chosen product weights less than 27.6g:", prob_less_than_27.6, "\n")


# 2b. Calculate the probability that a randomly chosen product has a weight that exceeds 26.6g
prob_exceeds_26.6 <- 1 - pnorm(26.6, mean, sd)
cat("Probability that a randomly chosen product has a weight that exceeds 26.6g:", prob_exceeds_26.6, "\n")


# 2c. Calculate the probability that a randomly chosen product is between 26.9 and 29.6g
prob_between_26.9_and_29.6 <- pnorm(29.6, mean, sd) - pnorm(26.9, mean, sd)
cat("Probability that a randomly chosen product is between 26.9 and 29.6g:", prob_between_26.9_and_29.6, "\n")



# 3a. Find the maximum weight of the lightest 10% of products
lightest_10_percent <- qnorm(0.10, mean, sd)
cat("Maximum weight of the lightest 10% of products:", lightest_10_percent, "\n")


# 3b. Find the minimum weight of the heaviest 5% of products
heaviest_5_percent <- qnorm(0.95, mean, sd)
cat("Minimum weight of the heaviest 5% of products:", heaviest_5_percent, "\n")

#Check type 2 using type 1
#lightest_27.42531_percent <- qnorm(0.2742531, mean, sd)
#cat("Maximum weight of the lightest 10% of products:", lightest_10_percent, "\n")


# 4a. Calculate the z-score for the lightest 10%
z_lightest_10_percent <- qnorm(0.10)
cat("Z-score for the maximum weight of the lightest 10% of products:", z_lightest_10_percent, "\n")

# 4b. Calculate the z-score for the heaviest 5%
z_heaviest_5_percent <- qnorm(0.95)
cat("Z-score for the minimum weight of the heaviest 5% of products:", z_heaviest_5_percent, "\n")

#5. 
qnorm(0.975)
qnorm(0.025)



# Given parameters
mean_population <- 30      # Mean of the population
sd_population <- 4         # Standard deviation of the population
n <- 49 # Sample size
mean_sample <- 30.6 

# Standard error of the sample mean
sd_error <- sd_population / sqrt(n)

#6a.
pnorm(30.6, mean_population, sd_error)

#6b. 
1-pnorm(30.6, mean_population, sd_error)

#6c
pnorm(29.6, mean_population, sd_error)- pnorm(28.9, mean_population, sd_error)

#6d
qnorm(.9,mean_population, sd_error)

#6e
qnorm(.1,mean_population, sd_error)



#CI z-test
#install.packages("BSDA")
library(BSDA)


ATTENTIMES <- read.csv("ATTENTIMES.csv", sep="")
View(ATTENTIMES)
attach(ATTENTIMES)
head(ATTENTIMES)
mean(Attention)

z.test(Attention, alternative = 'two.sided', mu=mean(Attention), sigma.x=sd(Attention), conf.level=.99)

t.test(Attention,conf.level=.99 )

#CI t-test
SILICA <- read.csv("SILICA.csv", sep="")
View(SILICA)
head(SILICA)
attach(SILICA)
t.test(SiliconDioxide, conf.level=.95)

# HT z-test

#a. Define mu as the average wait time among all customers inside the bank during lunch hour (12-1pm), after the drive-thru ATM installation.
#b. Ha: mu<15,   h0: mu>= 15
#c
mean_population <- 15       # Historical average waiting time
sd_population <- 9          # Known standard deviation
n <- 20                     # Sample size
mean_sample <- 12.3         # Sample mean

# Calculate the standard error
sd_error <- sd_population / sqrt(n)
#p-value
pnorm(12.3, mean_population, sd_error)

# We do not have enough statistical evidence to reject the null hypothesis and conclude
#that the average wait time among all customers inside the bank during lunch hour (12-1pm),
#after the drive-thru ATM installation, is not significantly less than 15 minutes.
#In other words, the installation of the drive-thru ATM does not significantly help alleviate the long wait time inside the bank during lunch.
#d Calculate p-value*2, then make your conclusion



#Hypothesis test
BONES <- read.csv("BONES.csv", sep="")
View(BONES)
attach(BONES)
t.test(BONES,mu=8.5,alternative="two.sided")
#At 5% level of significance, we have sufficient evidence to conclude that the true mean length-to-width ratio of
#all humerus bones of this species differs from 8.5.

t.test(BONES,mu=8.5,alternative="greater")

t.test(BONES,mu=8.5,alternative="less")


# Two sample t-test

DIETS <- read.csv("DIETS.csv")
View(DIETS)
attach(DIETS)
head(DIETS)
t.test(WTLOSS~DIET, conf.level=0.95)

library(mosaic)
favstats(WTLOSS~DIET)
tapply(WTLOSS,DIET,sd)

#We are we are 95% confident that the mean weight loss for the low-fat diet is between .69 and 3.13 pounds more than the
#mean weight loss for the other diet.



#Pooled t-test (if the variances are equal)

READING <- read.csv("~/Fall 2024/DANA4810/Chapter1/DATA/READING.csv")
View(READING)
head(READING)
attach(READING)
t.test(SCORE~METHOD, conf.level=.95, alternative="two.sided", var.equal=TRUE)
# t.test is more robust, with the equal sizes of sample the equal variances can be relaxed



#Paired t-test
PAIRED <- read.csv("~/Fall 2024/DANA4810/Chapter1/DATA/PAIRED.csv")
View(PAIRED)
attach(PAIRED)
t.test(NEW,STANDARD, conf.level=0.95, alternative=c("greater"),Paired=TRUE)


# test for equal variances
var.test(SCORE[METHOD=="STD"],SCORE[METHOD=="NEW"], alternative = "two.sided")

