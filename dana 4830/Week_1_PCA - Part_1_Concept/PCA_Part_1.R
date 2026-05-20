rm(list=ls())
m1 <- c(4, 2, 3, 5, 5)
m2 <- c(5, 3, 4, 4, 5)
m3 <- c(1, 1, 3, 4, 2)
m4 <- c(5, 4, 2, 1, 3)
movie <- data.frame(m1, m2, m3, m4)
View(movie)

movie$m3<- NULL
movie$m4 <- NULL
m.pca <- prcomp(movie)
m.pca$rotation 

######
#Option 2 is run PCA but with standardize data
library(factoextra)
Spcdf <- scale(movie,center = T, scale=T)
res.pca <- prcomp(Spcdf)
summary(res.pca)
res.pca$rotation #this output is the weights

#the weight in this R version a1 = -0.7071068 and a2= 0.7071068
#(-0.7071068)^2 + 0.7071068^2 = 1

res.pca$x
res.pca$loadings
res.pca$scores

######
#Option 3

pcdf <- read.csv("~/Desktop/MovieSample.csv", header=T)
View(pcdf)
pcdf$User<- NULL
pcdf$Movie3_Saw<- NULL
pcdf$Movie4_Ring <- NULL

library(factoextra)
Spcdf <- scale(pcdf,center = T, scale=T)
sd(pcdf$`Movie1_Toy-Story`)
res.pca <- prcomp(Spcdf )
summary(res.pca)
res.pca$rotation
res.pca$x
res.pca$loadings
res.pca$scores




