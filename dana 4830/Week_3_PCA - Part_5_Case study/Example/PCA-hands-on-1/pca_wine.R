rm(list=ls())

#load data
mydata<-read.csv("C:/Users/qnguyen/Documents/Langara/DANA4830/Topics/Section-3-PCA/PCA-hands-on-1/wine.csv", header = TRUE)
summary(mydata)
View(mydata)
attach(mydata)
names(mydata)


######################
pcal<-princomp(mydata[,-1], scores=TRUE, cor=TRUE) #principal component analysis on given data matrix
summary(pcal)

names(pcal)

#have a scree plot on
plot(pcal)
screeplot(pcal,type="line",main="Screen Plot")

#check factor loadings
pcal$loadings #(component coefficients) correlation coefficients between variables(rows) and factors(columns)
loadings(pcal)

pcal$sdev#The scaled standard deviation of the (first 4) principal components

#sdev can be calculated through eigen values
sqrt(eigen(var(scale(X, center=TRUE, scale=TRUE)))$values)

#We can extract the variances of the components with
(pcal.var <- pcal$sdev^2)#the results are the same as eigen values

#now you can compare with the eigenvalues using eigen function
ei <- eigen(cor(X))
print(ei)

biplot(pcal)
pcal$scores[1:10,]



###############
#correlation
cor(mydata[,-1])
#another function to calculate PCA
pcal1<-prcomp(mydata[,-1], scale = TRUE)
names(pcal1)#properties of principal component results

print(pcal1)
summary(pcal1)
screeplot(pcal1, main ="Scree Plot", xlab="Components")
screeplot(pcal1, main="Scree Plot", type="line")

#intepretation of PCA
biplot(pcal1, scale=0)

#extract component and attach to the new data frame
pcal1$x

#we try to plot our original dataset onto 2 PCs
wine_new <- cbind(X, pcal1$x[,1:2])
head(wine_new)
class(wine_new)

wine_new1 <- as.data.frame(wine_new)
as.character(wine_new1$Wine)#convert numerical values to character to have elipse graphs

str(wine_new1)
View(wine_new1)
#create a plot
library(ggplot2)

ggplot(wine_new1, aes(PC1, PC2, col = Wine, fill = Wine)) +
  stat_ellipse(geom = "polygon", col = "black", alpha = 0.5) +
  geom_point(shape = 21, col = "black")


#correlation of variables and PCs to see the changes of variabels against the 2 PCs
cor(X, wine_new[,6:7])

par(mfrow = c(2, 1))
plot(pcal1$rotation[, 1], ylim = c(-1, 1))
plot(pcal1$rotation[, 2], ylim = c(-1, 1))

#load lattice to run the dotplot graph
library(lattice)
# DotPlot PC1
load    <- pcal1$rotation
sorted.loadings <- load[order(load[, 1]), 1]
myTitle <- "Loadings Plot for PC1" 
myXlab  <- "Variable Loadings"
dotplot(sorted.loadings, main=myTitle, xlab=myXlab, cex=1.5, col="red")

#DotPlot PC2

sorted.loadings <- load[order(load[, 2]), 2]
myTitle <- "Loadings Plot for PC2"
myXlab  <- "Variable Loadings"
dotplot(sorted.loadings, main=myTitle, xlab=myXlab, cex=1.5, col="red")

biplot(pcal1, cex=c(1, 0.7))

# Now draw the BiPlot and clear out the observation
biplot(pcal1, col=c("white","red"), cex=c(1, 0.7))

# Apply the Varimax Rotation
(my.var <- varimax(pcal1$rotation))

#we get the result of rotation on top of rotation
#empty space suggests that it is more or less equal to zero
#the results give us the dominant variables for each component

#oblique rotation
(my.var1 <-promax(pcal1$rotation))
#This case, both rotation methods give the results. This means 2 components are not correlated