rm(list=ls())

#load data
cereals <- read.csv("~/Desktop/Langara/DANA4830/Topics/Section-3-PCA/Activities/Cereals/cereals.csv")
names(cereals)
View(cereals)

#explore some correlation
round(cor(cereals$Calories,cereals$Rating),2)
#intepretation
#69% of the total variation in both variables is actually a covariation,
#or variation in one variable that is duplicated by similar variation in
#the other variable.

plot(cereals$Calories,cereals$Rating)
plot(x = cereals[, c(4, 16)], pch = 16)

#################################################
#select some columns and the scatterplot matrix
#the function for the scatterplot matrix is pairs()
#The required input is x, which asks for the columns to include in the scatterplot matrix

#optional agurment is pch - change points from open circles to closed circles 
pairs(x = cereals[, c(10, 8, 11)], pch = 16)

######Multicollinearity
#variance inflation factors - vifs
#vifs >= 5 meaning moderate multicollinearity
#vifs >= 10 meaning severve multicollinearity
#calculate VIFs, we first need to install and open the car package
library(car)
#run regression
model.three <- lm(Rating ~ Fiber + Potass + Sugars, data = cereals)
vif(model.three)
#The VIF for fiber is 6.85 
#The VIF for potassium is 6.69, with both values indicating moderate‐to‐strong multicollinearity


#####Principal components analysis with varimax rotation
###psych package
library(psych)


#The rotate = a varimax perform varimax rotation on the components before presenting the results
#nfactors = 5 input states that we want five components
pcaModelone <- principal(cereals[,-c(1:3)], rotate = "varimax", nfactors = 5, n.obs=NA)
names(pcaModelone)
pcaModelone$loadings
print(pcaModelone$loadings, cutoff = 0.49)
pcaModelone

#Principal components analysis no rotation
pcaModeltwo <- principal(cereals[,-c(1:3)],  nfactors = 5, n.obs=NA)
pcaModeltwo
pcaModeltwo$loadings
print(pcaModeltwo$loadings, cutoff = 0.49)

#Principal components analysis obtaining component scores from raw data
pcaModelthree <- principal(cereals[,-c(1:3)], rotate = "varimax", nfactors = 5, n.obs=NA, score=TRUE)
pcaModelthree

#first component: calories, sugars, rating, and fat
#second component: Potassium, Fiber, and protein #health conciousness
#third component: carbo, qaker
#fourth component: cold and AHFP
#fifth component:Kellogss
#12 variables | 20 variables
#65%          | 100% of total variance

#use cutoff = 0.49 to suppress small PCA weights
print(pcaModelone$loadings, cutoff = 0.49)

#compare with no rotation
pca.norot <- principal(cereals[,-c(1:3)], rotate = "none",
                         nfactors = 5)
print(pca.norot$loadings, cutoff = 0.49)


#####Principal components analysis with varimax rotation
###stats package
library(stats)
#without telling prcomp that the data needs to scale 
#prcomp will not normalize the data
model.stat.one <- prcomp(na.omit(cereals[,-c(1:3)]))
names(model.stat.one)
summary(model.stat.one)
model.stat.one$rotation

model.stat.two <- prcomp(na.omit(cereals[,-c(1:3)]), scale. = T)   
summary(model.stat.two)
model.stat.two$rotation[,1:5]

#principal function
pca.t <- principal(na.omit(cereals[,-c(1:3)]), rotate = "none",
                       nfactors = 20)
summary(pca.t)
pca.t$loadings

model.stat.three <- princomp(na.omit(cereals[,-c(1:3)]), cor = TRUE)   
model.stat.three$loadings
