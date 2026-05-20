##reference - this work is referenced from Missouri State University


##import the file
master = read.csv("~/Desktop/14 EFA data.csv")

##accuracy
summary(master)

##recode
table(master$q6)
master[ , c(6,9,17,27)] = 8 - master[ , c(6,9,17,27)]
table(master$q6)

##missing
percentmissing = function (x){ sum(is.na(x))/length(x) * 100}
missing = apply(master, 1, percentmissing)#return a list of missing values row-wise
table(missing)#one participant got 28.125% missing values
              #8 participants with 3.125%

##exclude the participant missing too much data
replacepeople = subset(master, missing <= 5)
                #Return subsets data frames which meet conditions.

##make sure the columns aren't missing too much
apply(replacepeople, 2, percentmissing)


##impute missing data using mice package
library(mice)
tempnomiss = mice(replacepeople)
nomiss = complete(tempnomiss, 1)
summary(nomiss)

##outliers using Malahanobis distance
cutoff = qchisq(1-.001, ncol(nomiss))
mahal = mahalanobis(nomiss,
                    colMeans(nomiss),
                    cov(nomiss))
cutoff ##cutoff score
ncol(nomiss) ##df
summary(mahal < cutoff)

##exclude outliers
noout = subset(nomiss, mahal < cutoff)

##additivity - option 1
correl = cor(noout, use = "pairwise.complete.obs")
symnum(correl)
correl

##assumption set up - we check to see if assumptions for regressions are met
#https://journals.sagepub.com/doi/full/10.1177/0095798418771807
#regression
random = rchisq(nrow(noout), 7)
fake = lm(random~., data = noout)
standardized = rstudent(fake)
fitted = scale(fake$fitted.values)

##normality
hist(standardized)

##linearity
qqnorm(standardized)
abline(0,1)

##homogeneity -
plot(fitted,standardized)
abline(0,0)
abline(v = 0)

##running the efa analysis
library(psych)
library(GPArotation)

##correlation adequacy Bartlett's test
cortest.bartlett(correl, n = nrow(noout))

##sampling adequacy KMO test
KMO(correl)

##how many factors?
nofactors = fa.parallel(noout, fm="ml", fa="fa")
sum(nofactors$fa.values > 1.0) ##old kaiser criterion
sum(nofactors$fa.values > .7) ##new kaiser criterion

##simple structure with a three factor model
round1 = fa(noout, nfactors=3, rotate = "oblimin", fm = "ml")
round1 #save the loading data into an excel file and check high correlation and similar correlation
       #drop items that have similar correlations

round2 = fa(noout[ , -c(4,15)], nfactors=3, rotate = "oblimin", fm = "ml")
round2
print(round2$loadings,cutoff = 0.3)
#RMSA or RMSR or the root means the square of residuals is 0.05 -> is acceptable as this value should be closer to 0
#RMSEA (root mean square error of approximation) index is 0.09 -> above 0.08 of acceptable level
#TFI is 0.823 is close to the acceptable level

##get cfi
finalmodel = fa(noout[ , -c(4,15)], nfactors=3, rotate = "oblimin", fm = "ml")
1 - ((finalmodel$STATISTIC-finalmodel$dof)/
       (finalmodel$null.chisq-finalmodel$null.dof))
#link for the CFI formula: http://davidakenny.net/cm/fit.htm

##reliability
factor1 = c(1, 3, 7, 8, 10:12, 14, 16, 18, 20:25, 29, 31, 32)
factor2 = c(2, 5, 13, 19, 26, 28, 30)
factor3 = c(6, 9, 17, 27)
psych::alpha(noout[ , factor1])
psych::alpha(noout[ , factor2])
psych::alpha(noout[ , factor3])
#r.drop: item-total correlation without that item itself. 
#low item-total correlations indicate that that item doesn’t correlate well with the scale overall


##create new factor scores
noout$f1 = apply(noout[ , factor1], 1, mean) ##creates average scores
noout$f2 = apply(noout[ , factor2], 1, mean) ##creates average scores
noout$f3 = apply(noout[ , factor3], 1, mean) ##creates average scores

summary(noout)
sd(noout$f1)
sd(noout$f2)
sd(noout$f3)
