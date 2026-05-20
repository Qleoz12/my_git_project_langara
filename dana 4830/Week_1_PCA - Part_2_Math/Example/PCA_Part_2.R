rm(list=ls())

#load data
pcdf <- read.csv("~/Desktop/MovieSample.csv", header=T)
View(pcdf)

#processing data
pcdf <- Movie
View(Movie)
pcdf$User<- NULL
pcdf$Movie3_Saw<- NULL
pcdf$Movie4_Ring <- NULL

#performing PCA
library(factoextra)
Spcdf <- scale(pcdf,center = T, scale=T)
sd(pcdf$`Movie1_Toy-Story`)#confirming standard deviation of that column
res.pca <- prcomp(Spcdf )
summary(res.pca)
res.pca$rotation
res.pca$x
res.pca$loadings
res.pca$scores