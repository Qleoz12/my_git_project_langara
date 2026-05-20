#large dataset demonstration

rm(list=ls())

#load data
cities <- read.csv("~/Desktop/Langara/DANA4830/Topics/Section-3-PCA/Note-PCA-Practice/Biology-Data-2/cities.csv")

#Create a data matrix that omits the city names and look at the data and the correlation matrix
cities_matrix <- data.matrix(cities[,2:12])
rownames(cities_matrix) <- cities[,1] # add city names as row labels


#Examining the correlation matrix

plot(cities[,2:12], pch=16, cex=0.6)
cor(cities[,2:12])
#result of correlation: the three population variables are postively correlated with one another
#and are inversely correlated with Growth, Food and PersRoom


######Multicollinearity
#variance inflation factors - vifs
#vifs >= 5 meaning moderate multicollinearity
#vifs >= 10 meaning severse multicollinearity
#calculate VIFs, we first need to install and open the car package
library(car)
#run regression
model.one <- lm(Pop.1990 ~ Area + Food + PersRoom + Water + Growth +Elec + Phones + Vehicles, data = cities[,2:12])
vif(model.one)

model.two <- lm(Pop.2000 ~ Area + Food + PersRoom + Water + Growth +Elec + Phones + Vehicles, data = cities[,2:12])
vif(model.two)

model.three <- lm(Pop.1980 ~ Area + Food + PersRoom + Water + Growth +Elec + Phones + Vehicles, data = cities[,2:12])
vif(model.three)

#running PCA
cities_pca <- princomp(cities_matrix, cor=T)
cities_pca

#understand the outputs of PCA
names(cities_pca)

summary(cities_pca)

print(cities_pca$loadings, cutoff = 0.3)

#comp2: years - time period
#comp1: room, water, and elec - facility- amenity
#comp3: food, areas, phones - convenience or sustainability or service delivery
#comp4: vehicles 

cities_pca$scores
#plot on 2 dimensions
biplot(cities_pca, col=c("black","red"), cex=c(0.7,0.8))

#how many PCs are selected - eigenvalues >= 1
the.eigen <- eigen(cor(cities_matrix))
the.eigen
# Rename matrix rows and columns for easier interpretation
rownames(the.eigen$vectors) <- c("Area", "Pop80", "Pop90", "Pop20", "Growth", "Food", "PersRoom", "Water", "Elec", "Phones", "Vehicles")
colnames(the.eigen$vectors) <- c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8", "PC9", "PC10", "PC11")

print(the.eigen)

#criterio to select PCs
screeplot(cities_pca)
screeplot(cities_pca, npcs=7, type="lines")


######

model.pca.two <- prcomp(cities_matrix, center= TRUE, scale=TRUE)
summary(model.pca.two)

library(psych)
model.pca.three <- principal(cities_matrix, nfactors=3, rotate="varimax")
summary(model.pca.three)
model.pca.three

